;;;; General

;; load paths
(defvar my-libraries nil "My library paths to add to `load-path'")
(progn
  (setq
    my-libraries
    (list
      (concat my-libraries "~/.emacs.d/elisp/slime")
      (concat my-libraries "~/.emacs.d/elisp/xterm-frobs")
      (concat my-libraries "~/.emacs.d/elisp/xterm-title")
      (concat my-libraries "~/.emacs.d/elisp/color-theme-tangotango")
      (concat my-libraries "~/.emacs.d/elisp/misc-cmds")
      (concat my-libraries "~/.emacs.d/elisp/centered-cursor-mode")))
  (dolist (library my-libraries)
    (add-to-list 'load-path library)))

;; prefer UTF-8 encoding
(set-language-environment "UTF-8")

;; keep backup files in ~/.emacs.d/tmp
(setq backup-directory-alist
      (list
        (cons ".*" (expand-file-name "~/.emacs.d/tmp/"))))

;; same with autosave files
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/tmp/") t)))

;;;; UI gripes

;; don't show the welcome message
(setq inhibit-startup-screen t)

;; don't show the *scratch* buffer message
(setq initial-scratch-message nil)

;; start in text-mode by default
(setq initial-major-mode 'text-mode)

;; use text-mode for unknown file types
(setq default-major-mode 'text-mode)

;; change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; no bells, ever
(setq ring-bell-function 'ignore)

;; do mouse scrolling one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))

;; no menubar
(menu-bar-mode -1)

;; give all windows equal space
;; TODO: In emacs24 this causes "Lisp nesting exceeds `max-lisp-eval-depth'
;(add-hook 'window-configuration-change-hook 'balance-windows)

;; no fringes in the gui
(when window-system
  (set-fringe-mode 0))

;; set frame title
;; TODO: make it say e.g. "fstab * (/etc) - emacs"
(setq frame-title-format "%f %& emacs")

;; show column number
(setq column-number-mode t)

;; show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Draw tabs with the same color as trailing whitespace
(add-hook 'font-lock-mode-hook
          (lambda ()
            (font-lock-add-keywords
                nil
                '(("\t" 0 'trailing-whitespace prepend)))))

; Make home key go to beginning of indentation when possible
(require 'misc-cmds)
(global-set-key [home] 'beginning-or-indentation)
(substitute-key-definition 'beginning-of-line 'beginning-or-indentation global-map)

;; automatically format paragraphs in text mode
(setq-default fill-column 75)
(global-set-key (kbd "C-c q") 'auto-fill-mode)
(add-hook 'text-mode-hook (lambda () (auto-fill-mode 1)))

;; use syntax highlighting everywhere
(global-font-lock-mode t)
(add-hook
   'shell-mode-hook
   'ansi-color-for-comint-mode-on)

;; highlight matching parentheses
(show-paren-mode t)

;;;; Indentation

;; use spaces, not tabs
(setq-default indent-tabs-mode nil)
;; 4 spaces per indent
(setq default-tab-width 4)

;; these are annoying
(setq tab-always-indent nil)
(setq backward-delete-char-untabify-method nil)

;; auto-indent the next line
(define-key global-map (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "DEL") 'backward-delete-whitespace-to-column)
(add-hook 'cperl-mode-hook (lambda () (define-key cperl-mode-map (kbd "DEL") 'backward-delete-whitespace-to-column)))

;; http://stackoverflow.com/questions/1450169/how-do-i-emulate-vims-softtabstop-in-emacs
(defun backward-delete-whitespace-to-column ()
  "delete back to the previous column of whitespace, or as much whitespace as possible,
or just one char if that's not possible"
  (interactive)
  (if indent-tabs-mode
      (call-interactively 'backward-delete-char-untabify)
    (let ((movement (% (current-column) tab-width))
          (p (point)))
      (when (= movement 0) (setq movement tab-width))
      (save-match-data
        (if (string-match "\\w*\\(\\s-+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char-untabify (- (match-end 1) (match-beginning 1)))
        (call-interactively 'backward-delete-char-untabify))))))

;;;; Languages

;;;;; Perl

;; use cperl-mode instead of perl-mode
(defalias 'perl-mode 'cperl-mode)

;; indenting
(setq cperl-indent-level 4                  ; 4-space indents
      cperl-tab-always-indent nil           ; always let me indent further
      cperl-continued-statement-offset 0)   ; don't reindent multiline statements

;; automatically format comment paragraphs
(add-hook 'c-mode-common-hook
          (lambda ()
            (auto-fill-mode 1)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face))))))
;;;; Other packages

;;;;; linum.el

(require 'linum)

;; show line numbers
(global-linum-mode 1)

;; add space between the line numbers and the text, and right-justify
(setq linum-format (lambda (line) (propertize (format (let ((w (length (number-to-string (count-lines (point-min) (point-max)))))) (concat "%" (number-to-string w) "d ")) line) 'face 'linum)))

;;;;; centered-cursor-mode.el

;; center the cursor when scrolling
(and
  (require 'centered-cursor-mode)
  (global-centered-cursor-mode +1))

;;;;; xterm-title.el

;; set terminal title
(when (and (not window-system)
           (string-match "^xterm" (getenv "TERM")))
  (require 'xterm-title)
  (xterm-title-mode 1))

;;;;; slime.el

(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-net-coding-system 'utf-8-unix)
(require 'slime)
(slime-setup '(slime-fancy))

;;;;; color-theme.el

(require 'color-theme)
(require 'color-theme-tangotango)
(color-theme-tangotango)

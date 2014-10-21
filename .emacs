;;;; General

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

;; Manage elisp packages with el-get
(setq el-get-user-package-directory "~/.emacs.d/el-get-init/")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(require 'el-get-elpa)
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))
(unless (file-directory-p el-get-recipe-path-emacswiki)
  (el-get-emacswiki-build-local-recipes))

(setq el-get-sources
  '((:name xterm-frobs
        :type http
        :url "http://www.splode.com/~friedman/software/emacs-lisp/src/xterm-frobs.el")
    (:name xterm-title
        :type http
        :url "http://www.splode.com/~friedman/software/emacs-lisp/src/xterm-title.el")))

(setq
  my-el-get-packages
  '(el-get
    centered-cursor-mode
    misc-cmds
    slime
    tango-2-theme
    xterm-frobs
    xterm-title))

(el-get 'sync my-el-get-packages)

;; prefer UTF-8 encoding
(set-language-environment "UTF-8")

;; keep backup files in ~/.emacs.d/tmp
(setq backup-directory-alist
      (list
        (cons ".*" (expand-file-name "~/.emacs.d/tmp/"))))

;; same with autosave files
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/tmp/") t)))

;; save cursor position in files
(require 'saveplace)
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)

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

;; use i-beam as a cursor
(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))

;; do mouse scrolling one line at a time
(when window-system
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control)))))

;; no menubar
(menu-bar-mode -1)

;; no fringes in the gui
(when window-system
  (set-fringe-mode 0))

;; set frame title
(setq-default frame-title-format
  '(:eval
     (format "%s@%s: %s %s"
             (or (file-remote-p default-directory 'user)
                 user-real-login-name)
             (or (file-remote-p default-directory 'host)
                 system-name)
             (buffer-name)
             (cond
                 (buffer-file-truename
                 (concat "(" buffer-file-truename ")"))
                 (dired-directory
                 (concat "{" dired-directory "}"))
                 (t
                 "[no file]")))))

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

;; show line numbers
(require 'linum)
(global-linum-mode 1)

;; add space between the line numbers and the text, and right-justify
(setq linum-format (lambda (line) (propertize (format (let ((w (length (number-to-string (count-lines (point-min) (point-max)))))) (concat "%" (number-to-string w) "d ")) line) 'face 'linum)))

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

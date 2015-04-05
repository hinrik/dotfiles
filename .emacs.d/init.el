;;;; Packaging

;; make my elisp code loadable
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/"))

;; load installed packages now rather than after init file processing
(setq package-enable-at-startup nil)

;; the repositories
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; load installed packages
(package-initialize)

(defvar my-packages
  '(centered-cursor-mode
    helm
    helm-projectile
    hide-comnt
    highlight-numbers
    magit
    projectile
    slime
    xterm-frobs
    xterm-title)
  "The ELPA packages I want to install")

;; install the packages I use
(let (refreshed)
  (mapc
    (lambda (package)
      (unless (package-installed-p package)
        (unless refreshed
          (package-refresh-contents)
          (setq refreshed t))
        (package-install package)))
    my-packages))

;;;; Core stuff

;; prefer UTF-8 encoding
(set-language-environment "UTF-8")

;; use a simple pager so emacs can easily consume paged output
(setenv "PAGER" "cat")

;; keep backup files in ~/.emacs.d/state/tmp
(setq backup-directory-alist
      (list
        (cons ".*" (expand-file-name "~/.emacs.d/state/tmp/"))))

;; same with autosave files
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/state/tmp/") t)))
(setq auto-save-list-file-prefix "~/.emacs.d/state/auto-save-list/")

;; ditto for tramp
(setq tramp-auto-save-directory "~/.emacs.d/state/tramp-autosave")
(setq tramp-persistency-file-name "~/.emacs.d/state/tramp")

;; save cursor position in files
(require 'saveplace)
(setq save-place-file "~/.emacs.d/state/saveplace")
(setq-default save-place t)

;; save command, search, and kill ring history
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-additional-variables
  '(kill-ring search-ring regexp-search-ring)
  savehist-file "~/.emacs.d/state/savehist")
(savehist-mode t)

;; faster than the scp method
(setq tramp-default-method "ssh")

;; allow sudo editing with C-x C-f /sudo:host:/path
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;; I like my ls output sorted by filetype
(setq dired-listing-switches "-lhX")

;; more comprehensive help when using C-a
(setq apropos-do-all t)

;; case-insensitive incremental search
(setq case-fold-search t)

;; make sure buffer names are unique
(require 'uniquify)

;; when typing a filename to visit, // will mean / and
;; ~ will mean $HOME regardless of preceding text.
(setq file-name-shadow-tty-properties '(invisible t))
(file-name-shadow-mode 1)

;; C-d should logout/kill a shell buffer
(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  "Send delchar/eof or kill the buffer"
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

;; use helm for completion/narrowing in minibuffer, C-x C-f, etc
(setq helm-ff-transformer-show-only-basename nil  ; show full-path
      helm-move-to-line-cycle-in-source t         ; allow cycling top<->bottom
      helm-display-header-line nil                ; disable the header
      helm-M-x-fuzzy-match t                      ; mmm, fuzzy
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)
(require 'helm-config)
(helm-mode t)

;; use C-c h instead, as C-x c is a bit too close to C-x C-c
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

; Projectile allows for fast find-file and easy grepping in a git repo
(setq projectile-enable-caching t
      projectile-cache-file "~/.emacs.d/state/projectile.cache"
      projectile-known-projects-file "~/.emacs.d/state/projectile-bookmarks.eld"
      projectile-use-git-grep t
      projectile-completion-system 'helm
      projectile-switch-project-action 'helm-projectile)
(projectile-global-mode)
(helm-projectile-on)

;;;; Appearance

;; change default colors to fit a dark backround
(set-variable 'frame-background-mode 'dark)

;; customize colors further with my color theme
(require 'literal-tango-theme)
(load-theme 'literal-tango t)

;; don't show the welcome message
(setq inhibit-startup-screen t)

;; don't show the *scratch* buffer message
(setq initial-scratch-message nil)

;; don't show the echo area startup message
(defun display-startup-echo-area-message ())

;; change "yes or no" prompts to "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; no bells, ever
(setq ring-bell-function 'ignore)

;; use i-beam as a cursor
(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))

;; no menubar
(menu-bar-mode -1)

;; no fringes in the gui
(when window-system (set-fringe-mode 0))

;; show column number
(setq column-number-mode t)

(when (boundp 'mouse-wheel-mode)    ; dependency of centered-cursor-mode
  (require 'centered-cursor-mode)
  (global-centered-cursor-mode +1))

;; Don't show the \ character when lines wrap because it sucks
;; copy/pasting from Emacs in a terminal. See
;; http://emacswiki.org/emacs/LineWrap
(set-display-table-slot standard-display-table 'wrap ?\ )

;; set terminal title
(when (and (not window-system)
           (string-match "^xterm" (getenv "TERM")))
             (require 'xterm-title)
               (xterm-title-mode 1))

;; show line numbers in programming modes
(require 'linum)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'cperl-mode-hook 'linum-mode)

;; dynamic-length line number, right-justified, with a space before the text
;; http://stackoverflow.com/questions/3626632/right-align-line-numbers-with-linum-mode/8470136#8470136
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))

;; toggle between vertical and horizontal window split
;; http://www.emacswiki.org/emacs/ToggleWindowSplit
(define-key ctl-x-4-map "t" 'toggle-window-split)
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
       (next-win-buffer (window-buffer (next-window)))
       (this-win-edges (window-edges (selected-window)))
       (next-win-edges (window-edges (next-window)))
       (this-win-2nd (not (and (<= (car this-win-edges)
           (car next-win-edges))
             (<= (cadr this-win-edges)
           (cadr next-win-edges)))))
       (splitter
        (if (= (car this-win-edges)
         (car (window-edges (next-window))))
      'split-window-horizontally
    'split-window-vertically)))
  (delete-other-windows)
  (let ((first-win (selected-window)))
    (funcall splitter)
    (if this-win-2nd (other-window 1))
    (set-window-buffer (selected-window) this-win-buffer)
    (set-window-buffer (next-window) next-win-buffer)
    (select-window first-win)
    (if this-win-2nd (other-window 1))))))

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

;;; Modeline

;; in the modeline, show which function the cursor is in
(which-func-mode 1)

;; http://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs
(defun get-faces (pos)
  "Get the font faces at POS."
  (delq nil
        (delete-dups (list
          (get-char-property pos 'read-face-name)
          (get-char-property pos 'face)
          (plist-get (text-properties-at pos) 'face)))))

;; my nice modeline
(setq-default mode-line-format (list
  "%e"
  '(:propertize (:eval mode-line-remote) face '(:foreground "magenta" :weight bold))
  " "
  mode-line-buffer-identification
  " "
  '(:propertize "(" face '(:weight bold))
  '(:propertize (:eval mode-line-mule-info) face '(:foreground "green" :weight bold))
  '(:propertize ")" face '(:weight bold))
  " "
  '(:propertize "[" face '(:weight bold))
  '(:propertize (:eval mode-line-modified) face '(:foreground "yellow"))
  '(:propertize "]" face '(:weight bold))
  " "
  mode-line-position
  '(:propertize mode-name face '(:foreground "magenta" :weight bold))
  `(vc-mode vc-mode)
  '(:eval
    (if (projectile-project-root)
        (format " Proj[%s]"
                (projectile-project-name))
      ""))
  " "
  mode-line-misc-info
  '(:propertize (:eval (mapconcat 'symbol-name (get-faces (point)) ",")) face '(:foreground "cyan"))
  " "))

;;;; Editing

;; start in text-mode by default
(setq initial-major-mode 'text-mode)

;; use text-mode for unknown file types
(setq default-major-mode 'text-mode)

;; my sentences do not end with double spaces
(setq sentence-end-double-space nil)

;; C-c/C-v/C-x/C-z for copy/paste/cut/undo is ingrained in my muscle memory
(cua-mode t)

;; toggle comment visibility
(require 'hide-comnt)
(global-set-key (kbd "C-c k") 'hide/show-comments-toggle)

;; automatically format paragraphs in text mode, as well as code comments
;; in programming modes
(setq-default fill-column 73)
(setq comment-auto-fill-only-comments t)
(add-hook 'text-mode-hook (lambda () (auto-fill-mode 1)))
(add-hook 'prog-mode-hook (lambda () (auto-fill-mode 1)))

;; insert closing parenthesis/bracket/etc automatically
(electric-pair-mode 1)

;; I often want to remove a whole line, like Vim's "d d"
(global-set-key (kbd "C-k") 'kill-whole-line)

;; magit bindings
(global-set-key (kbd "C-c b") 'magit-blame-mode)
(global-set-key (kbd "C-c g") 'magit-status-mode)

;; show trailing whitespace in programming modes
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; when I comment blocks of code, I don't want padding at the beginning
(setq comment-padding 0)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq comment-start
                  (replace-regexp-in-string "\\s-+$" "" comment-start))))

;;;; Highlighting

;; highlight matching parentheses
(show-paren-mode t)

;; use syntax highlighting everywhere
(global-font-lock-mode t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; highlight numbers in code
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'cperl-mode-hook 'highlight-numbers-mode)

;; center the cursor vertically when scrolling
;; highlight FIXME/TODO/BUG/XXX
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
              '(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\):" 1 font-lock-warning-face t)))))

;;;; Indentation

;; make C-a/Home go to beginning of indentation when possible
;; http://www.emacswiki.org/emacs/BackToIndentationOrBeginning
(substitute-key-definition 'move-beginning-of-line 'back-to-indentation-or-beginning global-map)
(defun back-to-indentation-or-beginning () (interactive)
       (if (= (point) (progn (back-to-indentation) (point)))
           (beginning-of-line)))

;; make C-e/End go to end of code or end of line
(substitute-key-definition 'move-end-of-line 'end-of-code-or-line global-map)
(defun point-in-comment ()
  "Determine if the point is inside a comment"
  (interactive)
  (let ((syn (syntax-ppss)))
    (and (nth 8 syn)
         (not (nth 3 syn)))))

(defun end-of-code-or-line (arg)
    "Move to the end of code.c If already there, move to the end of line,
that is after the possible comment. If at the end of line, move to the
end of code.

Comments are recognized in any mode that sets syntax-ppss properly."
    (interactive "P")
    (let ((eoc (save-excursion
                 (move-end-of-line arg)
                 (while (point-in-comment)
                   (backward-char))
                 (skip-chars-backward " \t")
                 (point))))
      (cond ((= (point) eoc)
             (move-end-of-line arg))
            (t
             (move-end-of-line arg)
             (while (point-in-comment)
               (backward-char))
                        (skip-chars-backward " \t")))))

;; make cua-mode aware of the above so region selection is not degraded
(put 'back-to-indentation-or-beginning 'CUA 'move)
(put 'end-of-code-or-line 'CUA 'move)

;; 4-column indentation
(setq default-tab-width 4)

;; indentation should consist of spaces, not a real tab
(setq-default indent-tabs-mode nil)

;; I want Tab to always insert indentation at point
(define-key prog-mode-map (kbd "TAB") 'tab-to-tab-stop)

;; backspace should erase whitespace in indentation-sized chunks when possible
;; http://stackoverflow.com/questions/1450169/how-do-i-emulate-vims-softtabstop-in-emacs
(define-key prog-mode-map (kbd "DEL") 'backward-delete-whitespace-to-column)

(defun backward-delete-whitespace-to-column ()
  "Delete back to the previous column of whitespace, or as much whitespace as possible.
This emulates the 'softtabstop' feature in Vim."
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

;;; cperl-mode

;; cperl-mode still doesn't inherit from prog-mode-macp, so we need these too
(add-hook 'cperl-mode-hook (lambda () (define-key cperl-mode-map (kbd "TAB") 'tab-to-tab-stop)))
(add-hook 'cperl-mode-hook (lambda () (define-key cperl-mode-map (kbd "DEL") 'backward-delete-whitespace-to-column)))

;; use cperl-mode instead of perl-mode
(defalias 'perl-mode 'cperl-mode)

;; more comprehensive syntax highlighting
(setq cperl-highlight-variables-indiscriminately t)

;; indenting
(setq cperl-indent-level 4                  ; 4-space indents
      cperl-tab-always-indent nil           ; always let me indent further
      cperl-continued-statement-offset 0    ; don't reindent multiline statements
      cperl-indent-parens-as-block t        ; indent multiline () blocks correctly
      cperl-electric-keywords t             ; Expand "if ", "for ", and more
      cperl-label-offset 0)                 ; No special indenting of labels

;;; slime-mode

;; we're livin' in the future
(setq slime-net-coding-system 'utf-8-unix)

;; use SBCL runtime
(setq inferior-lisp-program "/usr/bin/sbcl")

;; load popular extensions
(slime-setup '(slime-fancy))

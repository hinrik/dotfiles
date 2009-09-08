;; load paths
(setq load-path (cons "~/.emacs.d" load-path))

;; don't show the welcome message
(setq inhibit-startup-screen t)

;; don't show the *scratch* buffer message
(setq initial-scratch-message nil)

;; change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; no bells, ever
(setq ring-bell-function 'ignore)

;; center the cursor when scrolling
(and
  (require 'centered-cursor-mode)
  (global-centered-cursor-mode +1))

;; do mouse scrolling one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))

;; keep backup files in ~/.tmp/emacs
(setq backup-directory-alist
      (list
        (cons ".*" (expand-file-name "~/.tmp/emacs/"))))

;; same with autosave files
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.tmp/emacs/") t)))

;; show matching parens
(require 'paren)
(show-paren-mode t)

;; use cperl-mode instead of perl-mode
(defalias 'perl-mode 'cperl-mode)

;; no menu bar or tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; show line numbers
(require 'linum)
(global-linum-mode 1)

;; add space between the line numbers and the text, and right-justify
(setq linum-format (lambda (line) (propertize (format (let ((w (length (number-to-string (count-lines (point-min) (point-max)))))) (concat "%" (number-to-string w) "d ")) line) 'face 'linum)))

;; default gui font
(add-to-list 'default-frame-alist '(font . "Mono-7"))

;; color theme
(require 'color-theme)

;; this one is alright
(color-theme-salmon-font-lock)

;; these are sort of ok
;;(color-theme-gtk-ide)
;;(color-theme-jedit-grey)
;;(color-theme-rotor)
;;(color-theme-sitaramv-nt)
;;(color-theme-emacs-21)

;; colors in the shell
(add-hook
   'shell-mode-hook
   'ansi-color-for-comint-mode-on)

;;;; General

;; load paths
(setq load-path (cons "~/.emacs.d" load-path))

;; prefer UTF-8 encoding
(set-language-environment "UTF-8")

;; keep backup files in ~/.tmp/emacs
(setq backup-directory-alist
      (list
        (cons ".*" (expand-file-name "~/.tmp/emacs/"))))

;; same with autosave files
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.tmp/emacs/") t)))

;;;; UI gripes

;; don't show the welcome message
(setq inhibit-startup-screen t)

;; don't show the *scratch* buffer message
(setq initial-scratch-message nil)

;; change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; no bells, ever
(setq ring-bell-function 'ignore)

;; do mouse scrolling one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))

;; no menu/tool/scroll bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; set frame title
;; TODO: make it say e.g. "fstab * (/etc) - emacs"
(setq frame-title-format "%f %& emacs")

;; default gui font
(add-to-list 'default-frame-alist '(font . "Mono-7"))

;; use syntax highlighting everywhere
(global-font-lock-mode t)
(add-hook
   'shell-mode-hook
   'ansi-color-for-comint-mode-on)

;; highlight matching parentheses
(show-paren-mode t)

;;;; Indentation

;; use spaces, not tabs
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; 4 spaces per indent
(setq default-tab-width 4)
(setq tab-width 4)

;; auto-indent the next line
(define-key global-map (kbd "RET") 'newline-and-indent)

;;;; Languages

;;;;; Perl

;; use cperl-mode instead of perl-mode
(defalias 'perl-mode 'cperl-mode)

;; Indent style
(setq cperl-tab-always-indent t ; TAB anywhere on the line indents
      cperl-indent-left-aligned-comments t ; The above goes for C<#> comments too
      cperl-auto-newline nil ; Automatically insert newline
      cperl-close-paren-offset -4
      cperl-indent-level 4 ; one tab?!
      cperl-indent-parens-as-block t ; "Foo->new( key => q[val],", not lined up with the (
      cperl-continued-statement-offset 0) ; C<sub foo\n {\n ...> sucks

;;;; Packages

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
(add-to-list 'load-path "~/src/slime/")
(require 'slime)
(slime-setup '(slime-fancy))

;;;;; color-theme.el

(require 'color-theme)

;; this theme looks alright
(color-theme-salmon-font-lock)

;; these are sort of ok
;;(color-theme-gtk-ide)
;;(color-theme-jedit-grey)
;;(color-theme-rotor)
;;(color-theme-sitaramv-nt)
;;(color-theme-emacs-21)


;; Increase GC threshold to speed up startup, then bring it back down to
;; a reasonable but higher-than-default value afterward
(setq gc-cons-threshold 100000000)
(run-with-idle-timer
  5 nil
  (lambda ()
    (setq gc-cons-threshold 1000000)))

;;; Packaging

(eval-when-compile
  ;; Add MELPA repository and initialize installed packages
  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (package-initialize)

  ;; Install use-package macro to install/load other packages
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

;;; Basic configuration

;; this prevents Emacs from dumping this into init.el
(setq custom-file "~/.emacs.d/state/selected-packages.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; don't load default.el
(setq inhibit-default-init t)

;; prefer UTF-8 encoding
(set-language-environment "UTF-8")

;; easily consume paged output
(setenv "PAGER" "cat")

;; keep various state/backup/tmp files in ~/.emacs.d/state/
(setq backup-directory-alist         '((".*" . "~/.emacs.d/state/tmp/"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/state/tmp/" t))
      auto-save-list-file-prefix     "~/.emacs.d/state/auto-save-list/"
      shared-game-score-directory    "~/.emacs.d/state/games/")

;; keep a long minibuffer history, without duplicates
(setq history-length t
      history-delete-duplicates t)

;; when typing a filename to visit, // will mean / and
;; ~ will mean $HOME regardless of preceding text.
(setq file-name-shadow-tty-properties '(invisible t))
(file-name-shadow-mode 1)

;; use text-mode on startup, and for unknown filetypes
(setq initial-major-mode 'text-mode)
(setq-default major-mode 'text-mode)

;;; Clean up the UI

;; no bells, ever
(setq ring-bell-function 'ignore)

;; no startup message
(fset 'display-startup-echo-area-message 'ignore)

;; no scroll bar, spaceline currently doesn't play well with it
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; no menu bar
(menu-bar-mode -1)

;; no toolbar
(when (display-graphic-p)
  (tool-bar-mode -1))

;; don't show the welcome message
(setq inhibit-startup-screen t)

;; no initial message in the *scratch* buffer
(setq initial-scratch-message nil)

;; no fringes in the gui
(when window-system (set-fringe-mode 0))

;; I prefer "y or n" to "yes or no"
(defalias 'yes-or-no-p 'y-or-n-p)

;; quicker feedback on incomplete key macros
(setq echo-keystrokes 0.1)

;;; Appearance

;; Fix oversized GUI windows on HiDPI displays
(when (and (string= system-type "gnu/linux") window-system)
  (defun my-gsettings-get-number (schema key)
    (let* ((command (concat "gsettings get " schema " " key))
           (output (shell-command-to-string command)))
      (and
       (not (string-match "^No such key" output))
       (string-match "\\(?:\\w+ \\)?\\([0-9]+\\(?:\\.[0-9]+\\)?\\)" output)
       (string-to-number (match-string 1 output)))))

  (setq my-frame-width  120
        my-frame-height 32)

  (defun my-window-setup-hook ()
    (let ((scale-factor (my-gsettings-get-number "org.gnome.desktop.interface" "scaling-factor")))
      (when (and scale-factor (> scale-factor 1))
        (let* ((frame-width (truncate (/ my-frame-width scale-factor)))
               (frame-height (truncate (/ my-frame-height scale-factor)))
               (orig-frame-pos (frame-position))
               (pos-x (car orig-frame-pos))
               (pos-y (cdr orig-frame-pos)))
          (when (eq (frame-parameter nil 'fullscreen) 'maximized)
            (toggle-frame-maximized))
          (set-frame-size (selected-frame) frame-width frame-height)
          (set-frame-position (selected-frame) pos-x pos-y)))))
  (add-hook 'window-setup-hook 'my-window-setup-hook))

;; GUI font size
(when (display-graphic-p)
  (let* ((scale-factor (my-gsettings-get-number "org.gnome-desktop.interface" "text-scaling-factor"))
         (font-size (if (and scale-factor (>= scale-factor 2)) 14 12))
         (font (concat "Mono-" (number-to-string font-size))))
    (set-frame-font font)
    (add-to-list 'default-frame-alist `(font . ,font))))

;; Monokai looks nice
(use-package monokai-theme
  :ensure t
  :config
  (progn
    (set-frame-parameter nil 'background-mode 'dark)
    (set-terminal-parameter nil 'background-mode 'dark)
    (load-theme 'monokai t)))

(custom-set-faces
 ;; These cperl faces have terrible fg/bg colors by default and are rarely
 ;; affected by color themes. So let's make them inherit from some standard
 ;; faces instead.
 '(cperl-hash-face ((t (:inherit font-lock-variable-name-face :weight bold :slant italic))))
 '(cperl-array-face ((t (:inherit font-lock-variable-name-face :weight bold))))
 '(cperl-nonoverridable-face ((t (:inherit font-lock-function-name-face))))

 ;; ditto
 '(php-function-name ((t (:inherit default))))
 '(php-property-name ((t :inherit default)))
 '(php-variable-sigil ((t (:inherit php-variable-name))))
 '(php-$this-sigil ((t (:inherit php-variable-name))))
 '(php-$this ((t (:inherit php-variable-name)))))

;; don't add \ when line wraps
(set-display-table-slot standard-display-table 'wrap ?\ )

;; show column number
(setq column-number-mode t)

;; wrap long lines visually
(use-package emacs
  :config (global-visual-line-mode t))

(use-package total-lines
  :load-path "~/src/total-lines"
  :config (global-total-lines-mode))

;; show line numbers on the side in programming modes,
(if (version< emacs-version "26.1")
    (use-package nlinum
      :ensure t
      :bind ("C-c l" . nlinum-mode)
      :hook (prog-mode . nlinum-mode)
      :config (setq nlinum-format "%d "))
  (use-package display-line-numbers
    :bind ("C-c l" . display-line-numbers-mode)
    :hook (prog-mode . display-line-numbers-mode)
    :config (setq display-line-numbers-width-start t)))

;; set frame title
(setq-default frame-title-format
  '(:eval
     (format "%s %s - emacs"
             (concat
               (buffer-name)
               (cond (buffer-read-only " =")
                 ((buffer-modified-p) " +")))
             (cond
               (buffer-file-truename
                  (concat "(" (file-name-directory buffer-file-truename) ")"))
               (dired-directory
                 (concat "{" dired-directory"}"))
               (t "[no file]")))))
(setq-default icon-title-format frame-title-format)

;; use frame title as terminal title
(if (version< emacs-version "25.0")
    (progn
      (use-package xterm-frobs
        :ensure t)
      (use-package xterm-title
        :if (and (not window-system)
                 (string-match "^xterm" (getenv "TERM")))
        :config (xterm-title-mode 1)))
  (setq xterm-set-window-title t))

;;; Modeline

(use-package spaceline-config
  :ensure spaceline
  :config
  (progn
    (spaceline-define-segment faces-at-point
      "Displays the font-lock face(s) at point"
      (let ((face (face-at-point)))
        (when face
          (symbol-name face)))
      :enabled nil)

    (spaceline-define-segment line-total-column
      "Line/Total,Column"
      (format
       "%s/%d,%s"
       (format-mode-line "%l")
       total-lines
       (format "%-2d" (1+ (current-column)))))

    (defun spaceline-my-theme ()
      "My modeline"
      (spaceline-compile
        `(anzu
          selection-info
          (remote-host buffer-id buffer-modified)
          (version-control :when active)
          ((flycheck-error flycheck-warning flycheck-info) :when active))
        `(faces-at-point
          major-mode
          line-total-column
          buffer-position))
      (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

    (setq powerline-default-separator 'utf-8) ; so it works in a terminal
    (spaceline-my-theme)
    (spaceline-helm-mode)))

;;; Editing

;; my sentences don't end with double spaces
(setq sentence-end-double-space nil)

;; automatically format text paragraphs and code comments
(use-package auto-fill
  :bind ("C-c q" . auto-fill-mode)
  :hook ((text-mode prog-mode) . auto-fill-mode)
  :config
  (progn
    (setq-default fill-column 73)
    (setq comment-auto-fill-only-comments t)
    (define-key text-mode-map (kbd "C-c q") 'auto-fill-mode)
    (define-key prog-mode-map (kbd "C-c q") 'auto-fill-mode)))

;; insert closing parenthesis/bracket/etc automatically
(use-package elec-pair
  :config (electric-pair-mode 1))

;; I often want to remove a whole line, like Vim's "d d"
(global-set-key (kbd "C-k") 'kill-whole-line)

;; more convenient mappings for working with paragraphs
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-m") 'mark-paragraph)

;; a better tool than mark-word, and more
(use-package expand-region
  :ensure t
  :bind ("M-," . er/expand-region))

;; ditto for scrolling the other window
(defun my-scroll-other-window-up () (interactive)
  (scroll-other-window -1))
(defun my-scroll-other-window-down () (interactive)
  (scroll-other-window 1))
(global-set-key (kbd "M-t") 'my-scroll-other-window-up)
(global-set-key (kbd "C-t") 'my-scroll-other-window-down)

;; show trailing whitespace in programming modes
(use-package whitespace
  :hook ((prog-mode text-mode) . whitespace-mode)
  :config (setq whitespace-style '(face trailing)))

;; when I comment blocks of code, I don't want padding at the beginning
(setq comment-padding 0)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq comment-start
                  (replace-regexp-in-string "\\s-+$" "" comment-start))))

;;; Highlighting

;; highlight matching parentheses
(use-package paren
  :config (show-paren-mode t))

;; use syntax highlighting everywhere
(use-package font-lock
  :hook (shell-mode . ansi-color-for-comint-mode-on)
  :config (global-font-lock-mode t))

;; highlight quoted elisp symbols
(use-package highlight-quoted
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . highlight-quoted-mode))

;; also highlight bound variables
(use-package lisp-extra-font-lock
  :ensure t
  :config (lisp-extra-font-lock-global-mode 1))

;; highlight numbers in code
(use-package highlight-numbers
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook
              (lambda ()
                (unless (eq major-mode 'perl6-mode)
                  (highlight-numbers-mode))))))

;; temporarily highlight changes from pasting, etc
(use-package volatile-highlights
  :defer t
  :config (volatile-highlights-mode t))

;; highlight FIXME/TODO/BUG/XXX
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
              '(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\):" 1 font-lock-warning-face t)))))

;;; Indentation

;; make Home go to beginning of indentation when possible
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
    "Move to the end of code. If already there, move to the end of line,
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

;; 4-column indentation
(setq-default tab-width 4)

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

;;; Window and frame management

;; always scroll one line at a time
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; center the cursor vertically when scrolling
(use-package centered-cursor-mode
  :ensure t
  :preface
  (progn
    (defun my-toggle-centered-cursor-mode ()
      (interactive)
      (if (and (boundp 'centered-cursor-mode)
               centered-cursor-mode)
          (centered-cursor-mode 0)
        (progn
          (scroll-lock-mode 0)
          (centered-cursor-mode 1))))
    (global-set-key (kbd "C-z") 'my-toggle-centered-cursor-mode))
  :config (global-centered-cursor-mode +1))

;; sometimes I want more context above/below the cursor
(use-package scroll-lock-mode
  :bind ("M-z" . my-toggle-scroll-lock-mode)
  :preface
  (progn
    (defun my-toggle-scroll-lock-mode ()
      (interactive)
      (if (and (boundp 'scroll-lock-mode)
               scroll-lock-mode)
          (scroll-lock-mode 0)
        (progn
          (centered-cursor-mode 0)
          (scroll-lock-mode 1))))))

;; I don't use the mouse
(use-package disable-mouse
  :ensure t
  :config (global-disable-mouse-mode))

(defun next-important-buffer ()
  (interactive)
  (let ((bread-crumb (buffer-name)))
    (next-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not (equal bread-crumb (buffer-name))))
      (next-buffer))))

(defun previous-important-buffer ()
  (interactive)
  (let ((bread-crumb (buffer-name)))
    (previous-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not (equal bread-crumb (buffer-name))))
      (previous-buffer))))

;; quick cycling through important buffers
(global-set-key (kbd "M-o") 'next-important-buffer)
(global-set-key (kbd "M-k") 'previous-important-buffer)

;; don't ask, just kill the buffer when I hit C-x k
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; handle tmux's xterm-keys
;; http://unix.stackexchange.com/questions/24414/shift-arrow-not-working-in-emacs-within-tmux
(if (getenv "TMUX")
    (progn
      (let ((x 2) (tkey ""))
        (while (<= x 8)
          (if (= x 2)
              (setq tkey "S-"))
            (if (= x 3)
                (setq tkey "M-"))
            (if (= x 4)
                (setq tkey "M-S-"))
            (if (= x 5)
                (setq tkey "C-"))
            (if (= x 6)
                (setq tkey "C-S-"))
            (if (= x 7)
                (setq tkey "C-M-"))
            (if (= x 8)
                (setq tkey "C-M-S-"))
            (define-key key-translation-map (kbd (format "M-[ 1 ; %d A" x)) (kbd (format "%s<up>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 1 ; %d B" x)) (kbd (format "%s<down>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 1 ; %d C" x)) (kbd (format "%s<right>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 1 ; %d D" x)) (kbd (format "%s<left>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 1 ; %d H" x)) (kbd (format "%s<home>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 1 ; %d F" x)) (kbd (format "%s<end>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 5 ; %d ~" x)) (kbd (format "%s<prior>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 6 ; %d ~" x)) (kbd (format "%s<next>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 2 ; %d ~" x)) (kbd (format "%s<delete>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 3 ; %d ~" x)) (kbd (format "%s<delete>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 1 ; %d P" x)) (kbd (format "%s<f1>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 1 ; %d Q" x)) (kbd (format "%s<f2>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 1 ; %d R" x)) (kbd (format "%s<f3>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 1 ; %d S" x)) (kbd (format "%s<f4>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 15 ; %d ~" x)) (kbd (format "%s<f5>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 17 ; %d ~" x)) (kbd (format "%s<f6>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 18 ; %d ~" x)) (kbd (format "%s<f7>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 19 ; %d ~" x)) (kbd (format "%s<f8>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 20 ; %d ~" x)) (kbd (format "%s<f9>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 21 ; %d ~" x)) (kbd (format "%s<f10>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 23 ; %d ~" x)) (kbd (format "%s<f11>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 24 ; %d ~" x)) (kbd (format "%s<f12>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 25 ; %d ~" x)) (kbd (format "%s<f13>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 26 ; %d ~" x)) (kbd (format "%s<f14>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 28 ; %d ~" x)) (kbd (format "%s<f15>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 29 ; %d ~" x)) (kbd (format "%s<f16>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 31 ; %d ~" x)) (kbd (format "%s<f17>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 32 ; %d ~" x)) (kbd (format "%s<f18>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 33 ; %d ~" x)) (kbd (format "%s<f19>" tkey)))
            (define-key key-translation-map (kbd (format "M-[ 34 ; %d ~" x)) (kbd (format "%s<f20>" tkey)))
            (setq x (+ x 1))))))

;; rebalance split windows on close
(defadvice delete-window (after auto-balance activate)
  (balance-windows))

;; http://www.emacswiki.org/emacs/ToggleWindowSplit
(global-set-key (kbd "C-c r") 'window-toggle-split-direction)
(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

;;; Misc

;; force me to use proper emacs keybindings
(use-package guru-mode
  :ensure t
  :config (guru-global-mode))

(use-package dired
  ;; sort ls output by filetype
  :config (setq dired-listing-switches "-lhX"))

;; search with regexp support by default
(use-package isearch
  :bind (("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp)))

;; shows current/total matches for isearch
(use-package anzu
  :ensure t
  :config
  (progn
    (setq anzu-cons-mode-line-p nil)
    (global-anzu-mode +1)))

(use-package shell
  :preface
  (progn
    (defun comint-delchar-or-eof-or-kill-buffer (arg)
      "Send delchar/eof or kill the buffer"
      (interactive "p")
      (if (null (get-buffer-process (current-buffer)))
          (kill-buffer)
        (comint-delchar-or-maybe-eof arg))))
  :config
  (progn
    ;; C-d should logout/kill a shell buffer
    (add-hook 'shell-mode-hook
              (lambda ()
                (define-key shell-mode-map
                  (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))))

;; editing files over ssh
(use-package tramp
  :config
  (progn
    (setq tramp-auto-save-directory   "~/.emacs.d/state/tramp-autosave"
          tramp-persistency-file-name "~/.emacs.d/state/tramp")))

;; store list of recently opened files on disk
(use-package recentf
  :config
  (progn
    (setq recentf-save-file "~/.emacs.d/state/recentf")
    (recentf-mode 1)))

;; save minibuffer, search, and kill ring history
(use-package savehist
  :init (savehist-mode t)
  :config
  (progn
    (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
          savehist-file "~/.emacs.d/state/savehist")))

;; save my position in visited files
(setq save-place-file "~/.emacs.d/state/saveplace")
(if (version< emacs-version "25.0")
    (use-package saveplace
      :config (setq-default save-place t))
  (save-place-mode 1))

(use-package eshell
  :config
  (progn
    (setq eshell-history-file-name "~/.emacs.d/state/eshell-history")
    ;; case-insensitive completion
    (setq eshell-cmpl-ignore-case t)))

;; code/word completion
(use-package company
  :ensure t
  :config
  (progn
    ;; don't autocomplete
    (setq company-idle-delay 1)
    (define-key company-mode-map (kbd "C-l") 'company-complete-common)
    (global-company-mode t)))

;; projectile offers fast find-file for project files, git-grep, etc
(use-package projectile
  :ensure t
  :config
  (progn
    (setq projectile-enable-caching t
          projectile-cache-file "~/.emacs.d/state/projectile.cache"
          projectile-known-projects-file "~/.emacs.d/state/projectile-bookmarks.eld"
          projectile-use-git-grep t)
    (projectile-mode)))

;; use helm for completion/narrowing in minibuffer, C-x C-f, etc
(use-package helm
  :ensure t
  :init
  (setq helm-adaptive-history-file "~/.emacs.d/state/helm-history"
        helm-move-to-line-cycle-in-source t         ; allow cycling top<->bottom
        helm-display-header-line nil                ; disable the header
        helm-completion-mode-start-message nil      ; be quiet
        helm-grep-file-path-style 'relative         ; more useful than basename
        helm-ff-file-name-history-use-recentf t     ; remember more files
        helm-M-x-fuzzy-match t                      ; mmm, fuzzy
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)
  :config
  (progn
    ; use helm for eshell completion and history
    (add-hook 'eshell-mode-hook
              #'(lambda ()
                  (eshell-cmpl-initialize)
                  (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
                  (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))
    ;; use helm for any kind of generic completion in emacs
    (helm-mode t)
    ;; sort candidates based on my previous selections
    (helm-adaptive-mode t)))

(use-package helm-config
  :bind (("M-x" . helm-M-x)
         ("M-s o" . helm-occur)
         ("C-x b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x r l" . helm-filtered-bookmarks)
         ("C-h i" . helm-info-emacs)
         ("C-h a" . helm-apropos)))

(use-package helm-descbinds
  :ensure t
  :bind ("C-h b" . helm-descbinds))

(use-package helm-themes
  :ensure t)

(use-package helm-projectile
  :ensure t
  :config
  (progn
    (setq projectile-completion-system 'helm
          projectile-switch-project-action 'helm-projectile)
    (helm-projectile-toggle 1)))

;; shared imenu between all buffers of the same major mode
(use-package imenu-anywhere
  :ensure t
  :bind ("C-c i" . helm-imenu-anywhere))

;; display keybinding overview 1 sec after hitting prefix keys
(use-package guide-key
  :ensure t
  :config
  (progn
    (guide-key-mode)
    (setq guide-key/guide-key-sequence t)))

;; code folding
(use-package hideshow
  :bind (("C-c s" . hs-toggle-hiding)
         ("C-c C-s" . my-toggle-hideshow-all))
  :hook (prog-mode . hs-minor-mode)
  :preface
  (progn
    (defvar my-hs-hide nil
      "Current state of hideshow for toggling all.")
    (defun my-toggle-hideshow-all ()
      "Toggle hideshow all."
      (interactive)
      (setq my-hs-hide (not my-hs-hide))
      (if my-hs-hide
          (hs-hide-all)
        (hs-show-all)))))

;; toggle comment visibility
(use-package hide-comnt
  :ensure t
  :bind ("C-c k" . hide/show-comments-toggle))

; git magic
(use-package magit
  :ensure t
  :bind (("C-c b" . magit-blame)
         ("C-x g" . magit-status)))

;; automatic syntax checking
(use-package flycheck
  :ensure t
  :defer t
  :hook (prog-mode . flycheck-mode)
  :config
  (progn
    ;; wait a bit longer before checking
    (setq flycheck-idle-change-delay 2)
    ;; I usually don't want to hear from perlcritic
    (setq-default flycheck-disabled-checkers
      '(perl-perlcritic))
    ;; don't warn about my custom packages not being loadable
    (setq flycheck-emacs-lisp-load-path '("~/.emacs.d/elisp/"))
    ;; use "C-c f" as the flycheck prefix key
    (define-key flycheck-mode-map flycheck-keymap-prefix nil)
    (setq flycheck-keymap-prefix (kbd "C-c f"))
    (define-key flycheck-mode-map flycheck-keymap-prefix
      flycheck-command-map)))

;; check Cask files
(use-package flycheck-cask
  :ensure t
  :config (flycheck-cask-setup))

(use-package flycheck-crystal
  :load-path "~/src/emacs-crystal-mode")

(use-package flycheck-lilypond
  :load-path "~/src/flycheck-lilypond")

;; check package conventions
(use-package flycheck-package
  :ensure t
  :config (flycheck-package-setup))

;; Show Flycheck messages in popups
(use-package flycheck-pos-tip
  :ensure t
  :config (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages))

;; more useful sizing of multiple windows
(use-package golden-ratio
  :ensure t
  :config
  (progn
    (golden-ratio-mode 1)
    (add-to-list 'window-size-change-functions 'golden-ratio)))

;;; Major modes

(use-package lisp-mode
  :defer t
  :mode ("/Cask\\'" . emacs-lisp-mode)
  :config
  (progn
    ;; use imenu to browse use-package blocks
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (add-to-list 'imenu-generic-expression
                             '("Used Packages"
                               "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2))
                ;; don't be anal about my init.el
                (when (string-match "\\.emacs\\.d/init\\.el$" (buffer-file-name))
                  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))))))

(use-package cperl-mode
  :defer t
  :init (defalias 'perl-mode 'cperl-mode)
  :config
  (progn
    ;; more comprehensive syntax highlighting
    (setq cperl-highlight-variables-indiscriminately t)

    ;; indenting
    (setq cperl-indent-level 4                  ; 4-space indents
          cperl-tab-always-indent nil           ; always let me indent further
          cperl-continued-statement-offset 0    ; don't reindent multiline statements
          cperl-indent-parens-as-block t        ; indent multiline () blocks correctly
          cperl-close-paren-offset -4           ; back-indent closing parens, K&R style
          cperl-electric-keywords t             ; Expand "if ", "for ", and more
          cperl-label-offset 0)                 ; No special indenting of labels

    ;; cperl-mode overrides things from prog-mode-map
    (add-hook 'cperl-mode-hook
              (lambda ()
                (progn
                  (define-key cperl-mode-map (kbd "TAB") 'tab-to-tab-stop)
                  (define-key cperl-mode-map (kbd "DEL") 'backward-delete-whitespace-to-column)
                  (when (projectile-project-root)
                    (setq flycheck-perl-include-path (list
                                                      (concat
                                                       (projectile-project-root)
                                                       "lib")))))))))

(use-package slime
  :ensure t
  :defer t
  :config
  (progn
    (setq slime-net-coding-system 'utf-8-unix)
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (slime-setup '(slime-fancy))))

(use-package ace-jump-mode
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-c w") 'ace-jump-word-mode)
    (global-set-key (kbd "C-c j") 'ace-jump-line-mode)
    (global-set-key (kbd "C-c c") 'ace-jump-char-mode)))

(use-package org
  :ensure t
  :defer t)

(use-package crystal-mode
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t)

;; this catches files Emacs gets wrong, e.g. *.config.php
(defun my-looks-like-php ()
  (or
   (looking-at-p "^<\\?php")
   (string-match-p "\\.php$" (buffer-file-name))))

(use-package php-mode
  :ensure t
  :defer t
  :init (add-to-list 'magic-mode-alist '(my-looks-like-php . php-mode))
  :hook php-enable-symfony2-coding-style
  :config
  (progn
    (setq php-template-compatibility nil)
    (setq php-lineup-cascaded-calls t)))

(use-package company-php
  :ensure t
  :config
  (progn
    (ac-php-core-eldoc-setup)
    (add-to-list 'company-backends 'company-ac-php-backend)))

(use-package geben
  :ensure t
  :config
  (progn
    (setq geben-path-mappings '(("~/work" "/vagrant"))
          geben-temporary-file-directory "~/.emacs.d/state/geben")))

(use-package thrift-mode
  :ensure thrift
  :defer t)

(use-package rust-mode
  :ensure t
  :defer t)

(use-package toml-mode
  :ensure t
  :defer t)

(use-package dumb-jump
  :ensure t
  :load-path "/home/hinrik/src/dumb-jump"
  :bind (("C-c C-d" . dumb-jump-go-current-window)
         ("C-c C-r" . dumb-jump-back)
         ("C-c C-s" . dumb-jump-go-other-window))
  :hook (prog-mode dumb-jump-mode)
  :config (setq dumb-jump-selector 'helm))

;;(use-package godot-gdscript
;;  :load-path "elisp/"
;;  :mode "\\.gd\\'"
;;  :defer t)

;;(use-package highlight-refontification
;;  :ensure t
;;  :defer t)
(use-package lilypond-mode
  :load-path "~/.emacs.d/elisp"
  :config
  (progn
    (require 'lilypond-init)
    (add-hook 'LilyPond-mode-hook (lambda () (run-hooks 'prog-mode-hook)))))

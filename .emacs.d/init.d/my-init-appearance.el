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
(add-hook 'after-make-frame-functions
  (lambda ()
    (when (display-graphic-p)
      (tool-bar-mode -1))))

;; don't show the welcome message
(setq inhibit-startup-screen t)

;; don't show a message every time a file is auto-saved
(unless (version< emacs-version "27.0")
  (setq auto-save-no-message t))

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
(add-hook 'after-make-frame-functions
  (lambda ()
    (when (display-graphic-p)
      (let* ((scale-factor (my-gsettings-get-number "org.gnome-desktop.interface" "text-scaling-factor"))
             (font-size (if (and scale-factor (>= scale-factor 2)) 14 14))
             (font (concat "Mono-" (number-to-string font-size))))
       (set-frame-font font)
       (add-to-list 'default-frame-alist `(font . ,font))))))

;; Monokai looks nice
(use-package monokai-theme
  :ensure t
  :config
  (progn
    (set-frame-parameter nil 'background-mode 'dark)
    (set-terminal-parameter nil 'background-mode 'dark)
    (unless window-system
      (set-face-background 'default "color-16")
      (set-face-background 'line-number "color-16"))
    (load-theme 'monokai t)))

;; don't add \ when line wraps
(set-display-table-slot standard-display-table 'wrap ?\ )

;; show column number
(setq column-number-mode t)

;; wrap long lines visually
(use-package emacs
  :config (global-visual-line-mode t))

(use-package total-lines
  :ensure t
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

(use-package powerline
  :ensure t)

(use-package spaceline-config
  :load-path "~/.emacs.d/elisp"
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
          lines-position))
      (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

    (setq powerline-default-separator 'utf-8) ; so it works in a terminal
    (spaceline-my-theme)
    (spaceline-helm-mode)))

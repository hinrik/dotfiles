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
(use-package tramp)

;; store list of recently opened files on disk
(use-package recentf
  :config (recentf-mode 1))

;; save minibuffer, search, and kill ring history
(use-package savehist
  :init (savehist-mode t)
  :config
  (progn
    (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))))

;; save my position in visited files
(if (version< emacs-version "25.0")
    (use-package saveplace
      :config (setq-default save-place t))
  (save-place-mode 1))

(use-package eshell
  :config
  (progn
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
          projectile-use-git-grep t)
    (projectile-mode)))

;; use helm for completion/narrowing in minibuffer, C-x C-f, etc
(use-package helm
  :ensure t
  :init
  (setq helm-move-to-line-cycle-in-source t         ; allow cycling top<->bottom
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

(use-package avy
  :ensure t
  :bind ("C-c c" . avy-goto-word-1))

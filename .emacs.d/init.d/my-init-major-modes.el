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
    (setq geben-path-mappings '(("~/work" "/vagrant")))))

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
  :load-path "~/src/dumb-jump"
  :bind (("C-c C-d" . dumb-jump-go-current-window)
         ("C-c C-r" . dumb-jump-back)
         ("C-c C-s" . dumb-jump-go-other-window))
  :hook (prog-mode . dumb-jump-mode)
  :config (setq dumb-jump-selector 'helm))

(use-package lilypond-mode
  :load-path "elisp"
  :config
  (progn
    (require 'lilypond-init)
    (add-hook 'LilyPond-mode-hook (lambda () (run-hooks 'prog-mode-hook)))))

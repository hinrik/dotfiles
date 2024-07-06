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
                ;; don't be anal about my init.el & init.d/*
                (when (string-match "\\.emacs\\.d/init\\." (buffer-file-name))
                  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
                  (setq-local flycheck-emacs-lisp-load-path '("~/.emacs.d/elisp/")))))))


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

    ;; I usually don't want to hear from perlcritic
    (setq-default flycheck-disabled-checkers
      '(perl-perlcritic))

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

(use-package org
  :ensure t
  :defer t
  :config (setq org-catch-invisible-edits 'show-and-error))

(use-package crystal-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package php-mode
  :ensure t
  :defer t
  :preface
  (progn
    ;; this catches files Emacs gets wrong, e.g. *.config.php
    (defun my-looks-like-php ()
      (or
       (looking-at-p "^<\\?php")
       (string-match-p "\\.php$" (buffer-file-name)))))
  :init (add-to-list 'magic-mode-alist '(my-looks-like-php . php-mode))
  :hook php-enable-symfony2-coding-style
  :config
  (progn
    (setq php-template-compatibility nil
          php-lineup-cascaded-calls t)))

(use-package rust-mode
  :ensure t
  :defer t)

(use-package toml-mode
  :ensure t
  :defer t)

(use-package dumb-jump
  :ensure t
  :bind (("C-c C-d" . dumb-jump-go-current-window)
         ("C-c C-r" . dumb-jump-back)
         ("C-c C-s" . dumb-jump-go-other-window))
  :init (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config (setq dumb-jump-selector 'helm))

(use-package lilypond-mode
  :load-path "~/.emacs.d/elisp"
  :hook (LilyPond-mode . (lambda () (run-hooks 'prog-mode-hook)))
  :config (require 'lilypond-init))

(use-package cmake-mode
  :ensure t
  :defer t)

(c-add-style "my/k&r"
             '("k&r"
               (c-offsets-alist
                (case-label . 2)
                (statement-cont . 4)
                (cpp-macro-cont . 4)
                (cpp-define-intro . 4)
                (arglist-cont-nonempty . 4))))
(setq c-basic-offset 2
      c-default-style "my/k&r")

(defun love-or-fennel-repl (command &optional buffer)
  (interactive
   (list (if current-prefix-arg
             (read-string "Fennel command: " fennel-program)
           fennel-program)))
  (let ((root (projectile-project-root))
        (prev-buffer (current-buffer))
        (prev-directory default-directory))
    (if (and root (file-exists-p (concat root "main.lua")))
        (progn
          (setq default-directory root)
          (fennel-repl "love .")
          (with-current-buffer prev-buffer
            (setq default-directory prev-directory)))
      (fennel-repl command buffer))))


(use-package fennel-mode
  :load-path "~/.emacs.d/fennel-mode"
  :defer t
  :mode "\\.fnl\\'"
  :config
  (progn
    (define-key fennel-mode-map (kbd "C-c C-z") 'love-or-fennel-repl)
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs '(fennel-mode . ("fennel-ls"))))))

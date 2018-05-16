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
 '(php-$this ((t (:inherit php-variable-name))))

 '(avy-lead-face-0 ((t (:inherit default :underline t))))
 '(avy-lead-face-1 ((t (:inherit default :underline t))))
 '(avy-lead-face-2 ((t (:inherit default :underline t))))
 '(avy-lead-face ((t (:inherit default :underline t)))))

;; for debugging font-lock
(use-package highlight-refontification
  :ensure t
  :defer t)

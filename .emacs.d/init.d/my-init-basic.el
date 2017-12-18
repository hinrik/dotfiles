;;; Basic configuration

;; keep all transient files in ~/.emacs.d/{etc,var}
(use-package no-littering
  :ensure t
  :config
  (progn
    (setq auto-save-file-name-transforms
          `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
          custom-file (no-littering-expand-etc-file-name "emacs-custom.el"))))

;; don't load default.el
(setq inhibit-default-init t)

;; prefer UTF-8 encoding
(set-language-environment "UTF-8")

;; easily consume paged output
(setenv "PAGER" "cat")

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

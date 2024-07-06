;;; Basic configuration

(defun suppress-messages (func &rest args)
  "Suppress message output from FUNC."
  ;; Some packages are too noisy.
  ;; https://superuser.com/questions/669701/emacs-disable-some-minibuffer-messages
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
        (apply func args)
      (advice-remove 'message #'silence))))

(use-package no-littering
  :ensure t
  :config
  (progn
    (setq auto-save-file-name-transforms
          `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
          custom-file
          (no-littering-expand-etc-file-name
            (if (version< emacs-version "27.0")
                "custom.el"
              "emacs-custom.el")))
    ;; load the customizations file so emacs won't litter init.el
    (load custom-file 'noerror 'nomessage)))

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

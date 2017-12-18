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

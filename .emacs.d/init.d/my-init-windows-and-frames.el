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
(if (version< emacs-version "26.3")
    (global-set-key (kbd "C-x k") 'kill-this-buffer)
  ;; more effective than kill-this-buffer
  (global-set-key (kbd "C-x k") 'kill-current-buffer))


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

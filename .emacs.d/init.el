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

;; suppress `defadvice' warnings that might come from 3rd-party packages
(setq ad-redefinition-action 'accept)

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
      tramp-auto-save-directory      "~/.emacs.d/state/tramp-autosave"
      tramp-persistency-file-name    "~/.emacs.d/state/tramp"
      eshell-history-file-name       "~/.emacs.d/state/eshell-history"
      recentf-save-file              "~/.emacs.d/state/recentf"
      shared-game-score-directory    "~/.emacs.d/state/games/")

;; store list of recently opened files on disk
(recentf-mode 1)

;;; Clean up the UI

;; no bells, ever
(setq ring-bell-function 'ignore)

;; no startup message
(fset 'display-startup-echo-area-message 'ignore)

;; no menu bar
(menu-bar-mode -1)

;; no toolbar
(tool-bar-mode -1)

;; don't show the welcome message
(setq inhibit-startup-screen t)

;; no initial message in the *scratch* buffer
(setq initial-scratch-message nil)

;; no fringes in the gui
(when window-system (set-fringe-mode 0))

;; I prefer "y or n" to "yes or no"
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Appearance

;; theming
(if window-system
    (progn
      (set-frame-font "Mono-8.5")
      ;; Monokai looks nice
      (use-package monokai-theme
        :ensure t
        :config
        (load-theme 'monokai t)
        ;; override Monokai's very dim comment color
        (set-face-foreground 'font-lock-comment-face "#729FCF")
        (set-face-foreground 'font-lock-comment-delimiter-face "#729FCF")))
  (progn
    (use-package literal-tango-theme
      :load-path "elisp/"
      :config (load-theme 'literal-tango t))))

;; bar/i-beam cursor
(modify-all-frames-parameters '((cursor-type . bar)))

;; don't add \ when line wraps
(set-display-table-slot standard-display-table 'wrap ?\ )

;; show column number
(setq column-number-mode t)

(add-hook 'tty-setup-hook (lambda () (send-string-to-terminal "\033[?0;0;0c") ))

;; show line numbers on the side in programming modes
(use-package nlinum
  :ensure t
  :bind ("C-c l" . nlinum-mode)
  :init
  (add-hook 'prog-mode-hook 'nlinum-mode)
  ;; precalculate max line number width
  (add-hook 'nlinum-mode-hook
            (lambda ()
              (setq nlinum--width
                    (1+ (length (number-to-string
                                 (count-lines (point-min) (point-max))))))))
  :config (setq nlinum-format "%d "))

;; make buffer names unique
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

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
(use-package xterm-title
  :ensure t
  :if (and (not window-system)
           (string-match "^xterm" (getenv "TERM")))
  :diminish xterm-title-mode
  :init (use-package xterm-frobs :ensure t)
  :config (xterm-title-mode 1))

;;; Modeline

;; in the modeline, show which function the cursor is in
(use-package which-func
  :preface
  ;; strip the package name from the function name, it's usually superfluous
  (defun shorten-function-name (name)
    (last (split-string name "::")))
  (defun my-current-function ()
    (gethash (selected-window) which-func-table which-func-unknown))
  (defconst my-which-func-current
    `(:eval (shorten-function-name (replace-regexp-in-string
             "%" "%%"
             (my-current-function)))))
  :init (which-function-mode)
  :config
  (setq which-func-current 'my-which-func-current
        which-func-format (list
          '(:propertize (:eval (if (my-current-function) "➤" "")) face which-func)
          '(:propertize which-func-current local-map which-func-keymap face which-func))))

;; http://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs
(defun get-faces (pos)
  "Get the font faces at POS."
  (delq nil
        (delete-dups (list
          (get-char-property pos 'read-face-name)
          (get-char-property pos 'face)
          (plist-get (text-properties-at pos) 'face)))))

;; my nice modeline
(setq-default mode-line-format (list
  "%e "
  mode-line-remote
  " "
  '(:propertize (:eval mode-line-buffer-identification) face (:weight bold))
  " "
  '(:propertize "[" face (:weight bold))
  '(:propertize (:eval mode-line-modified) face (:foreground "yellow"))
  '(:propertize "]" face (:weight bold))
  " "
  mode-line-position
  mode-line-modes
  mode-line-misc-info
  '(:propertize (:eval (mapconcat 'symbol-name (get-faces (point)) ",")) face (:foreground "cyan"))
  " "))

;;; Editing

;; join the following line onto the current one
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

; drag lines/words/regions vertically/horizontally with M-<direction>
(use-package drag-stuff
  :ensure t
  :diminish drag-stuff-mode
  :config
  (add-hook 'find-file-hook (lambda () (drag-stuff-mode t))))

;; my sentences don't end with double spaces
(setq sentence-end-double-space nil)

;; C-n at end-of-buffer should insert a newline
(setq next-line-add-newlines t)

;; I'm used to C-c/C-v/C-x/C-z for copy/paste/cut/undo
(cua-mode t)

;; make undo behave like tree, which is what I'm used to
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(use-package hideshow
  :diminish hs-minor-mode
  :bind ("C-c s" . hs-toggle-hiding)
  :init (add-hook 'prog-mode-hook 'hs-minor-mode))

(defvar my-hs-hide nil
  "Current state of hideshow for toggling all.")
(defun my-toggle-hideshow-all ()
  "Toggle hideshow all."
  (interactive)
  (setq my-hs-hide (not my-hs-hide))
  (if my-hs-hide
      (hs-hide-all)
    (hs-show-all)))
(global-set-key (kbd "C-c C-s") 'my-toggle-hideshow-all)

; Expand region by semantic units
(use-package expand-region
  :ensure t
  :bind (("C-a" . er/expand-region)))

;; toggle comment visibility
(use-package hide-comnt
  :ensure t
  :bind ("C-c k" . hide/show-comments-toggle))

;; automatically format text paragraphs and code comments
(setq-default fill-column 73)
(setq comment-auto-fill-only-comments t)
(add-hook 'text-mode-hook (lambda () (progn (auto-fill-mode 1) (diminish 'auto-fill-function))))
(add-hook 'prog-mode-hook (lambda () (progn (auto-fill-mode 1) (diminish 'auto-fill-function))))

;; insert closing parenthesis/bracket/etc automatically
(electric-pair-mode 1)

;; I often want to remove a whole line, like Vim's "d d"
(global-set-key (kbd "C-k") 'kill-whole-line)

; git magic
(use-package magit
  :ensure t
  :bind (("C-c b" . magit-blame)
         ("C-c g" . magit-status)))

;; show trailing whitespace in programming modes
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; when I comment blocks of code, I don't want padding at the beginning
(setq comment-padding 0)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq comment-start
                  (replace-regexp-in-string "\\s-+$" "" comment-start))))

;; automatic syntax checking
(use-package flycheck
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  ;; I usually don't want to hear from perlcritic
  (setq-default flycheck-disabled-checkers
    '(perl-perlcritic))
  ;; don't warn about my custom packages not being loadable
  (setq flycheck-emacs-lisp-load-path '("~/.emacs.d/elisp/"))
  ;; use "C-c f" as the flycheck prefix key
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c f"))
  (define-key flycheck-mode-map flycheck-keymap-prefix
    flycheck-command-map)
  ;; check Cask files
  (use-package flycheck-cask
    :ensure t
    :config (flycheck-cask-setup))
  ;; check package conventions
  (use-package flycheck-package
    :ensure t
    :config (flycheck-package-setup))
  ;; Show Flycheck messages in popups
  (use-package flycheck-pos-tip
    :ensure t
    :config
    (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages)))

;;; Highlighting

;; highlight matching parentheses
(show-paren-mode t)

;; use syntax highlighting everywhere
(global-font-lock-mode t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; highlight quoted elisp symbols
(use-package highlight-quoted
  :ensure t
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

;; highlight numbers in code
(use-package highlight-numbers
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; highlight FIXME/TODO/BUG/XXX
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
              '(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\):" 1 font-lock-warning-face t)))))

;;; Indentation

;; manually indent/deindent regions with C-c i
(global-set-key (kbd "C-c i") 'indent-rigidly)

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

;; smooth resizing of GUI frames
(setq frame-resize-pixelwise t)

;; don't ask, just kill the buffer when I hit C-x k
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Move between windows with Shift+Arrow
(use-package windmove
  :bind (("S-<left>" . windmove-left)
         ("S-<right>" . windmove-right)
         ("S-<up>" . windmove-up)
         ("S-<down>" . windmove-down)))

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

; Undo and redo window configurations with C-c left/right
(use-package winner
  :init (winner-mode))

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

;; http://emacswiki.org/emacs/TransposeWindows
(global-set-key (kbd "C-c C-r") 'rotate-windows)
(defun rotate-windows (arg)
  "Rotate your windows; use the prefix argument to rotate the other direction"
  (interactive "P")
  (if (not (> (count-windows) 1))
      (message "You can't rotate a single window!")
    (let* ((rotate-times (if (and (numberp arg) (not (= arg 0))) arg 1))
           (direction (if (or (< rotate-times 0) (equal arg '(4)))
                          'reverse
                        (lambda (x) x)))
           (i 0))
      (while (not (= rotate-times 0))
        (while  (< i (- (count-windows) 1))
          (let* ((w1 (elt (funcall direction (window-list)) i))
                 (w2 (elt (funcall direction (window-list)) (+ i 1)))
                 (b1 (window-buffer w1))
                 (b2 (window-buffer w2))
                 (s1 (window-start w1))
                 (s2 (window-start w2))
                 (p1 (window-point w1))
                 (p2 (window-point w2)))
            (set-window-buffer-start-and-point w1 b2 s2 p2)
            (set-window-buffer-start-and-point w2 b1 s1 p1)
            (setq i (1+ i))))

        (setq i 0
              rotate-times
              (if (< rotate-times 0) (1+ rotate-times) (1- rotate-times)))))))

;;; Misc

;; use text-mode on startup, and for unknown filetypes
(setq initial-major-mode 'text-mode)
(setq-default major-mode 'text-mode)

;; more comprehensive help when using C-a
(setq apropos-do-all t)

;; sort ls output by filetype
(setq dired-listing-switches "-lhX")

;; keep a long minibuffer history, without duplicates
(setq history-length t
      history-delete-duplicates t)

;; when typing a filename to visit, // will mean / and
;; ~ will mean $HOME regardless of preceding text.
(setq file-name-shadow-tty-properties '(invisible t))
(file-name-shadow-mode 1)

;; quick back/forward/replace for symbol under point with M-p/M-n/M-'
(use-package smartscan
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'smartscan-mode)
  (add-hook 'text-mode-hook 'smartscan-mode))

;; nice alternative to isearch
(use-package phi-search
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'phi-search)
  (global-set-key (kbd "C-r") 'phi-search-backward)
  (setq phi-search-case-sensitive 'guess
        phi-search-limit 5000))

;; center the cursor vertically when scrolling
(use-package centered-cursor-mode
  :ensure t
  :diminish centered-cursor-mode
  :config (global-centered-cursor-mode +1))

;; C-d should logout/kill a shell buffer
(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  "Send delchar/eof or kill the buffer"
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

;; save minibuffer, search, and kill ring history
(use-package savehist
  :init (savehist-mode t)
  :config
  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
        savehist-file "~/.emacs.d/state/savehist"))

;; save my position in visited files
(use-package saveplace
  :config
  (setq save-place-file "~/.emacs.d/state/saveplace")
  (setq-default save-place t))

;; use helm for completion/narrowing in minibuffer, C-x C-f, etc
(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (setq helm-move-to-line-cycle-in-source t         ; allow cycling top<->bottom
        helm-display-header-line nil                ; disable the header
        helm-completion-mode-start-message nil      ; be quiet
        helm-split-window-in-side-p t               ; split from current window
        helm-ff-file-name-history-use-recentf t     ; remember more files
        helm-M-x-fuzzy-match t                      ; mmm, fuzzy
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)
  :config
  (use-package helm-config
    :bind ("M-x" . helm-M-x))
  ; descend into directories with Tab
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
  (helm-mode t))

;; shared imenu between all buffers of the same major mode
(use-package imenu-anywhere
  :ensure t
  :bind ("C-c i" . helm-imenu-anywhere))

;; projectile offers fast find-file for project files, git-grep, etc
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching t
        projectile-cache-file "~/.emacs.d/state/projectile.cache"
        projectile-known-projects-file "~/.emacs.d/state/projectile-bookmarks.eld"
        projectile-use-git-grep t)
  (projectile-global-mode)
  (with-eval-after-load 'helm
    (use-package helm-projectile
      :ensure t)
    (setq projectile-completion-system 'helm
          projectile-switch-project-action 'helm-projectile)
    (helm-projectile-toggle 1)))

;; display keybinding overview 1 sec after hitting prefix keys
(use-package guide-key
  :ensure t
  :diminish guide-key-mode
  :config
  (guide-key-mode)
  (setq guide-key/guide-key-sequence t))

;;; Major modes

(use-package lisp-mode
  :defer t
  :mode ("/Cask\\'" . emacs-lisp-mode)
  :config
  ;; use imenu to browse use-package blocks
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-to-list 'imenu-generic-expression
                           '("Used Packages"
                             "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))))

;; These cperl faces have terrible fg/bg colors by default and are rarely
;; affected by color themes. So let's make them inherit from some standard
;; faces instead.
(defun fix-cperl-faces ()
  (face-remap-set-base 'cperl-hash-face
    '((:inherit font-lock-variable-name-face :weight bold :slant italic)))
  (face-remap-set-base 'cperl-array-face
    '((:inherit font-lock-variable-name-face :weight bold)))
  (face-remap-set-base 'cperl-nonoverridable-face
    '((:inherit font-lock-function-name-face))))

(use-package cperl-mode
  :defer t
  :diminish abbrev-mode
  :init (defalias 'perl-mode 'cperl-mode)
  :config
  ;; more comprehensive syntax highlighting
  (setq cperl-highlight-variables-indiscriminately t)

  ;; indenting
  (setq cperl-indent-level 4                  ; 4-space indents
        cperl-tab-always-indent nil           ; always let me indent further
        cperl-continued-statement-offset 0    ; don't reindent multiline statements
        cperl-indent-parens-as-block t        ; indent multiline () blocks correctly
        cperl-electric-keywords t             ; Expand "if ", "for ", and more
        cperl-label-offset 0)                 ; No special indenting of labels

  (add-hook 'cperl-mode-hook 'fix-cperl-faces)
  ;; cperl-mode-map still doesn't inherit from prog-mode-map, so we need these
  (add-hook 'cperl-mode-hook
            (lambda ()
              (progn
                (define-key cperl-mode-map (kbd "TAB") 'tab-to-tab-stop)
                (define-key cperl-mode-map (kbd "DEL") 'backward-delete-whitespace-to-column))))
  ;; cperl-mode doesn't derive from prog-mode, so make sure prog-mode
  ;; hooks get run
  (add-hook 'cperl-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

(use-package slime
  :ensure t
  :defer t
  :config
  (setq slime-net-coding-system 'utf-8-unix)
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (slime-setup '(slime-fancy)))

(use-package org
  :ensure t
  :config
  ;; preserve windmove keybindings
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftright-final-hook 'windmove-right))

(use-package markdown-mode
  :ensure t
  :defer t)

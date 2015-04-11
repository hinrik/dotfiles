;;;; Packaging

;; Add MELPA repository and initialize installed packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Install use-package which will be used to install other packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;;; Basic configuration

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

;;;; Clean up the UI

;; no bells, ever
(setq ring-bell-function 'ignore)

;; no startup message
(fset 'display-startup-echo-area-message 'ignore)

;; no menu bar
(menu-bar-mode -1)

;; don't show the welcome message
(setq inhibit-startup-screen t)

;; no initial message in the *scratch* buffer
(setq initial-scratch-message nil)

;; no fringes in the gui
(when window-system (set-fringe-mode 0))

;; I prefer "y or n" to "yes or no"
(defalias 'yes-or-no-p 'y-or-n-p)

;;;; Appearance

;; theming
(if window-system
  (progn
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

;; smooth resizing of GUI frames
(setq frame-resize-pixelwise t)

;; show column number
(setq column-number-mode t)

;; show line numbers on the side in programming modes
(use-package nlinum
  :ensure t
  :init (add-hook 'prog-mode-hook 'nlinum-mode)
  :config (setq nlinum-format "%d "))

;; make buffer names unique
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

; Move between windows with Shift+Arrow
(use-package windmove
  :bind (("S-<left>" . windmove-left)
         ("S-<right>" . windmove-right)
         ("S-<up>" . windmove-up)
         ("S-<down>" . windmove-down)))

; Undo and redo window configurations
(use-package winner
  :init (winner-mode))

;; center the cursor vertically when scrolling
(use-package centered-cursor-mode
  :ensure t
  :diminish centered-cursor-mode
  :config (global-centered-cursor-mode +1))

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

;; strip the package name from the function name, it's usually superfluous
(defun shorten-function-name (name)
  (last (split-string name "::")))

(defconst my-which-func-current
  `(:eval (shorten-function-name (replace-regexp-in-string
           "%" "%%"
           (gethash (selected-window) which-func-table which-func-unknown)))))

;; in the modeline, show which function the cursor is in
(use-package which-func
  :init (which-function-mode)
  :config
  (setq which-func-current 'my-which-func-current
        which-func-format  `((:propertize ("➤ " which-func-current)
                             local-map ,which-func-keymap
                             face which-func))))

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
  '(:propertize (:eval mode-line-buffer-identification) face '(:weight bold))
  " "
  '(:propertize "[" face '(:weight bold))
  '(:propertize (:eval mode-line-modified) face '(:foreground "yellow"))
  '(:propertize "]" face '(:weight bold))
  " "
  mode-line-position
  mode-line-modes
  mode-line-misc-info
  '(:propertize (:eval (mapconcat 'symbol-name (get-faces (point)) ",")) face '(:foreground "cyan"))
  " "))

;;;; Editing

;; my sentences don't end with double spaces
(setq sentence-end-double-space nil)

;; I'm used to C-c/C-v/C-x/C-z for copy/paste/cut/undo
(cua-mode t)

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
  :bind (("C-c b" . magit-blame-mode)
         ("C-c g" . magit-status-mode)))

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
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  ;; disable perlcritic in the syntax check
  (setq-default flycheck-disabled-checkers
    (append flycheck-disabled-checkers
      '(perl-perlcritic)))
  ;; use "C-c f" as the flycheck prefix key
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c f"))
  (define-key flycheck-mode-map flycheck-keymap-prefix
              flycheck-command-map)
  ;; Show Flycheck messages in popups
  (use-package flycheck-pos-tip
    :ensure t
    :config
    (with-eval-after-load 'flycheck
      (setq flycheck-display-errors-function
            'flycheck-pos-tip-error-messages))))

;;;; Highlighting

;; highlight matching parentheses
(show-paren-mode t)

;; use syntax highlighting everywhere
(global-font-lock-mode t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; highlight numbers in code
(use-package highlight-numbers
  :ensure t
  :config (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; highlight FIXME/TODO/BUG/XXX
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
              '(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\):" 1 font-lock-warning-face t)))))

;;;; Indentation

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
    "Move to the end of code.c If already there, move to the end of line,
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

;; make cua-mode aware of the above so region selection is not degraded
(put 'back-to-indentation-or-beginning 'CUA 'move)
(put 'end-of-code-or-line 'CUA 'move)

;; 4-column indentation
(setq default-tab-width 4)

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

;;;; Misc

;; use text-mode on startup, and for unknown filetypes
(setq initial-major-mode 'text-mode
      default-major-mode 'text-mode)

;; case-insensitive incremental search
(setq case-fold-search t)

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

;; toggle between vertical and horizontal window split
;; http://www.emacswiki.org/emacs/ToggleWindowSplit
(define-key ctl-x-4-map "t" 'toggle-window-split)
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
       (next-win-buffer (window-buffer (next-window)))
       (this-win-edges (window-edges (selected-window)))
       (next-win-edges (window-edges (next-window)))
       (this-win-2nd (not (and (<= (car this-win-edges)
           (car next-win-edges))
             (<= (cadr this-win-edges)
           (cadr next-win-edges)))))
       (splitter
        (if (= (car this-win-edges)
         (car (window-edges (next-window))))
      'split-window-horizontally
    'split-window-vertically)))
  (delete-other-windows)
  (let ((first-win (selected-window)))
    (funcall splitter)
    (if this-win-2nd (other-window 1))
    (set-window-buffer (selected-window) this-win-buffer)
    (set-window-buffer (next-window) next-win-buffer)
    (select-window first-win)
    (if this-win-2nd (other-window 1))))))

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
  :bind ("C-c h" . helm-command-prefix)
  :init
  (setq helm-ff-transformer-show-only-basename nil  ; show full-path in find-file
        helm-move-to-line-cycle-in-source t         ; allow cycling top<->bottom
        helm-display-header-line nil                ; disable the header
        helm-completion-mode-start-message nil      ; be quiet
        helm-M-x-fuzzy-match t                      ; mmm, fuzzy
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)
  :config
  (use-package helm-config)
  (helm-mode t))

;; projectile offers fast find-file for project files, git-grep, etc
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (use-package helm-projectile :ensure t)
  (setq projectile-enable-caching t
        projectile-cache-file "~/.emacs.d/state/projectile.cache"
        projectile-known-projects-file "~/.emacs.d/state/projectile-bookmarks.eld"
        projectile-use-git-grep t
        projectile-completion-system 'helm
        projectile-switch-project-action 'helm-projectile)
  (projectile-global-mode)
  (helm-projectile-toggle 1))

;;;; Major modes

(use-package cperl-mode
  :diminish abbrev-mode
  :config
  ;; use cperl-mode instead of perl-mode
  (defalias 'perl-mode 'cperl-mode)

  ;; cperl-mode-map still doesn't inherit from prog-mode-map, so we need these
  (add-hook 'cperl-mode-hook
            (lambda ()
              (progn
                (define-key cperl-mode-map (kbd "TAB") 'tab-to-tab-stop)
                (define-key cperl-mode-map (kbd "DEL") 'backward-delete-whitespace-to-column))))

  ;; These cperl faces have terrible fg/bg colors by default and are rarely
  ;; affected by color themes. So let's make them inherit from some standard
  ;; faces instead.
  (set-face-attribute 'cperl-hash-face nil
    :background nil
    :foreground nil
    :inherit 'font-lock-variable-name-face)
  (set-face-attribute 'cperl-array-face nil
    :background nil
    :foreground nil
    :inherit 'font-lock-variable-name-face)
  (set-face-attribute 'cperl-nonoverridable-face nil
    :background nil
    :foreground nil
    :inherit 'font-lock-function-name-face)

  ;; more comprehensive syntax highlighting
  (setq cperl-highlight-variables-indiscriminately t)

  ;; indenting
  (setq cperl-indent-level 4                  ; 4-space indents
        cperl-tab-always-indent nil           ; always let me indent further
        cperl-continued-statement-offset 0    ; don't reindent multiline statements
        cperl-indent-parens-as-block t        ; indent multiline () blocks correctly
        cperl-electric-keywords t             ; Expand "if ", "for ", and more
        cperl-label-offset 0)                 ; No special indenting of labels

  ;; cperl-mode doesn't derive from prog-mode, so make sure prog-mode
  ;; hooks get run
  (add-hook 'cperl-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

(use-package slime
  :ensure t
  :config
  (setq slime-net-coding-system 'utf-8-unix)
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (slime-setup '(slime-fancy)))

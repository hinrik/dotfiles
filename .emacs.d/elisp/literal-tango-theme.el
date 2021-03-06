;;; literal-tango-theme.el --- Tango-inspired 16-color theme for Emacs

;; Copyright (C) 2014-2015
;;
;; Author: Hinrik Örn Sigurðsson <hinrik.sig@gmail.com>
;; Version: 1.0.0
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program. If not, see
;; <http://www.gnu.org/licenses/>.

;; This theme is for 16-color terminals. It's recommended that you set
;; your terminal to the Tango color palette with a black background.

(unless (>= emacs-major-version 24)
  (error "The literal-tango theme requires Emacs 24 or later!"))

(deftheme literal-tango "literal tango color theme")

(custom-theme-set-faces 'literal-tango
  '(mode-line ((t :foreground "white" :background "black")))
  '(mode-line-inactive ((t :foreground "black" :background "white")))
  '(region ((t :background "black")))
  '(show-paren-match ((t :background "black" :bold t)))
  '(minibuffer-prompt ((t :foreground "yellow")))
  '(button ((t :foreground "blue" :bold nil)))
  '(info-xref ((t :foreground "blue")))
  '(info-xref-visited ((t :foreground "magenta")))
  '(info-title-1 ((t :foreground "green" :bold t)))
  '(highlight ((t :foreground "yellow" :background nil)))
  '(linum ((t :inherit default :foreground "black" :bold t)))
  '(line-number ((t :foreground "black" :bold t)))
  '(line-number-current-line ((t :foreground "white" :bold t)))
  '(which-func ((t :foreground "yellow")))
  '(isearch ((t :foreground "black" :background "yellow")))
  '(lazy-highlight ((t :foreground "black" :background "white")))
  '(hl-line ((t :background "black")))
  '(phi-search-selection-face ((t :foreground "black" :background "yellow")))
  '(phi-search-match-face ((t :foreground "black" :background "white")))

  '(font-lock-comment-face ((t :foreground "blue")))
  '(font-lock-comment-delimiter-face ((t :foreground "blue")))
  '(font-lock-function-name-face ((t :foreground "yellow")))
  '(font-lock-keyword-face ((t :foreground "yellow")))
  '(font-lock-type-face ((t :foreground "yellow")))
  '(font-lock-variable-name-face ((t :foreground "green")))
  '(font-lock-string-face ((t :foreground "red")))
  '(font-lock-constant-face ((t :foreground "magenta")))
  '(font-lock-warning-face ((t :foreground "red" :bold t :underline t)))
  '(font-lock-preprocessor-face ((t :foreground "magenta")))
  '(font-lock-builtin-face ((t :foreground "cyan")))
  '(font-lock-doc-face ((t :foreground "cyan")))
  '(font-lock-negation-char-face ((t :foreground "yellow")))
  '(font-lock-regexp-grouping-construct ((t :foreground "yellow")))
  '(font-lock-regexp-grouping-backslash ((t :foreground "cyan")))

  '(font-lock-color-constant-face ((t :foreground "green")))
  '(font-lock-reference-face ((t :foreground "green")))
  '(font-lock-other-type-face ((t :foreground "magenta")))
  '(font-lock-special-keyword-face ((t :foreground "cyan")))
  '(font-lock-exit-face ((t :foreground "cyan")))
  '(font-lock-other-emphasized-face ((t :foreground "yellow")))

  '(magit-tag ((t :foreground "yellow" :bold t)))
  '(magit-branch-local ((t :foreground "green")))
  '(magit-section-title ((t :foreground "yellow")))
  '(magit-item-highlight ((t :background "black")))
  '(magit-key-mode-switch-face ((t :foreground "green")))
  '(magit-hash ((t :foreground "cyan")))
  '(magit-blame-date ((t :background "black" :bold t)))
  '(magit-blame-hash ((t :background "black" :bold t)))
  '(magit-blame-heading ((t :background "black" :bold t)))
  '(magit-blame-name ((t :background "black" :bold t)))
  '(magit-blame-summary ((t :background "black" :bold t)))
  '(magit-log-sha1 ((t :foreground "cyan")))
  '(magit-log-author ((t :foreground "green")))
  '(magit-log-date ((t :foreground "yellow")))
  '(magit-log-head-label-tags ((t :foreground "yellow" :bold t)))
  '(magit-log-head-label-head ((t :foreground "cyan" :bold t)))
  '(magit-log-head-label-remote ((t :foreground "red" :bold t)))
  '(magit-log-head-label-local ((t :foreground "green" :bold t)))
  '(magit-log-reflog-label-reset ((t :foreground "red")))
  '(magit-log-reflog-label-rebase ((t :foreground "red")))
  '(magit-log-reflog-label-other ((t :foreground "red")))
  '(magit-log-reflog-label-commit ((t :foreground "yellow")))
  '(magit-log-reflog-label-remote ((t :foreground "green")))
  '(magit-log-reflog-label-amend ((t :foreground "magenta")))
  '(magit-log-reflog-label-checkout ((t :foreground "blue")))
  '(magit-diff-hunk-heading ((t :foreground "cyan")))
  '(magit-diff-hunk-heading-highlight ((t :background "black" :foreground "cyan")))
  '(magit-diff-context ((t :foreground nil)))
  '(magit-diff-context-highlight ((t :background "black")))
  '(magit-diff-added ((t :background nil :foreground "green")))
  '(magit-diff-added-highlight ((t :background "black" :foreground "green")))
  '(magit-diff-removed ((t :background nil :foreground "red")))
  '(magit-diff-removed-highlight ((t :background "black" :foreground "red")))
  '(magit-diff-base ((t :background nil :foreground "yellow")))
  '(magit-diff-base-highlight ((t :background "black" :foreground "yellow")))
  '(magit-section-highlight ((t :background "black")))
  '(magit-section-heading ((t :foreground "yellow")))
  '(dired-symlink ((t :foreground "cyan")))
  '(dired-directory ((t :foreground "blue" :bold t)))
  '(dired-perm-write ((t :foreground "red" :bold t)))
  '(helm-match ((t :foreground "yellow")))
  '(helm-selection ((t :background "black" :bold t)))
  '(helm-selection-line ((t :background "black")))
  '(helm-candidate-number ((t :foreground "green" :bold t)))
  '(helm-header ((t :background "black" :foreground "blue" :bold t :underline nil)))
  '(helm-source-header ((t :bold t :underline t)))
  '(helm-bookmark-directory ((t :foreground "blue" :bold t)))
  '(helm-buffer-directory ((t :foreground "blue" :bold t)))
  '(helm-ff-directory ((t :foreground "blue" :bold t)))
  '(helm-ff-dotted-directory ((t :background nil :foreground "white")))
  '(helm-ff-symlink ((t :foreground "cyan")))
  '(helm-ff-file ((t :foreground nil)))
  '(helm-bookmark-file ((t :foreground nil)))
  '(helm-grep-lineno ((t :foreground nil)))
  '(helm-grep-match ((t :background "yellow")))
  '(helm-buffer-file ((t :foreground nil)))
  '(helm-lisp-completion-info ((t :foreground "green" :bold t)))
  '(company-tooltip ((t :background "black")))
  '(company-tooltip-selection ((t :background "black" :underline t)))
  '(company-tooltip-common ((t :background "black" :bold t)))
  '(company-tooltip-common-selection ((t :background "black" :bold t :underline t)))
  '(company-scrollbar-bg ((t :background "black")))
  '(company-scrollbar-fg ((t :background "white")))
  '(company-preview ((t :background "black")))
  '(company-preview ((t :background "black")))
  '(company-preview-common ((t :background "black" :bold t)))

  '(mu4e-header-highlight-face ((t :background "black" :bold t)))
  '(mu4e-unread-face ((t :foreground "green")))
  '(mu4e-header-key-face ((t :foreground "yellow")))
  '(mu4e-cited-1-face ((t :foreground "yellow")))
  '(mu4e-cited-2-face ((t :foreground "green")))
  '(mu4e-cited-3-face ((t :foreground "cyan")))
  '(mu4e-cited-4-face ((t :foreground "magenta")))
  '(mu4e-cited-5-face ((t :foreground "blue")))
  '(mu4e-cited-6-face ((t :foreground "red")))
  '(mu4e-cited-7-face ((t :foreground "yellow")))
  '(mu4e-title-face ((t :foreground "green" :bold t)))
)

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'literal-tango)

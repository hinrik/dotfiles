;;; literal-tango-theme.el --- Tango-inspired color theme for GNU Emacs 24
;; Author: Hinrik Örn Sigurðsson
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
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(deftheme literal-tango
  "literal tango color theme")

(custom-theme-set-faces
 'literal-tango
 '(mode-line ((t (:foreground "white" :background "black"))))
 '(mode-line-inactive ((t (:foreground "black" :background "white"))))
 '(region ((t (:background "black"))))
 '(show-paren-match ((t (:background "black" :bold t))))
 '(minibuffer-prompt ((t (:foreground "yellow"))))
 '(button ((t (:foreground "blue" :bold nil))))
 '(isearch ((t (:foreground "black" :background "white"))))
 '(lazy-highlight ((t (:foreground "black" :background "yellow"))))
 '(info-xref ((t (:foreground "blue"))))
 '(info-xref-visited ((t (:foreground "magenta"))))
 '(info-title-1 ((t (:foreground "green" :bold t))))
 '(linum ((t (:foreground "black" :bold t))))
 '(which-func ((t (:foreground "yellow"))))
 '(font-lock-comment-face ((t (:foreground "blue"))))
 '(font-lock-function-name-face ((t (:foreground nil))))
 '(font-lock-keyword-face ((t (:foreground "yellow"))))
 '(font-lock-type-face ((t (:foreground "yellow"))))
 '(font-lock-variable-name-face ((t (:foreground "green"))))
 '(font-lock-string-face ((t (:foreground "red"))))
 '(font-lock-constant-face ((t (:foreground "red"))))
 '(font-lock-warning-face ((t (:foreground "red" :bold t :underline t))))
 '(font-lock-preprocessor-face ((t (:foreground "magenta"))))
 '(font-lock-builtin-face ((t (:foreground "cyan"))))
 '(cperl-array-face ((t :inherit font-lock-variable-name-face)))
 '(cperl-hash-face ((t :inherit font-lock-variable-name-face)))
 '(cperl-nonoverridable-face ((t :inherit font-lock-keyword-face)))
 '(magit-log-sha1 ((t :foreground "cyan")))
 '(magit-tag ((t :foreground "yellow" :bold t)))
 '(magit-branch ((t :foreground "green" :bold t)))
 '(magit-section-title ((t :foreground "yellow")))
 '(magit-item-highlight ((t :background "black")))
 '(magit-log-author ((t :foreground "green")))
 '(magit-log-date ((t :foreground "yellow")))
 '(magit-key-mode-switch-face ((t :foreground "green")))
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
 '(dired-symlink ((t :foreground "cyan")))
 '(dired-directory ((t :foreground "blue" :bold t)))
 '(dired-perm-write ((t :foreground "red" :bold t)))
)

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'literal-tango)

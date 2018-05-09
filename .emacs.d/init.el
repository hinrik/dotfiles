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
  (if (version< emacs-version "27.0")
    (package-initialize))

  ;; Install use-package macro to install/load other packages
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

(add-to-list 'load-path "~/.emacs.d/init.d/")
(load-library "my-init-basic")
(load-library "my-init-appearance")
(load-library "my-init-editing")
(load-library "my-init-highlighting")
(load-library "my-init-indentation")
(load-library "my-init-windows-and-frames")
(load-library "my-init-misc")
(load-library "my-init-major-modes")

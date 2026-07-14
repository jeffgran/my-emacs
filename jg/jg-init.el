;;; local.el --- Machine-specific overrides, NOT committed to git.
;;
;; This file is gitignored and loaded FIRST in jg-init.el, before any
;; other config.  Override any variable here and the downstream use-package
;; blocks / load-library calls will see your value.
;;
;; SETUP:
;;   1. Copy this template to jg/local.el on each machine.
;;   2. Uncomment / edit the variables you need to override.
;;   3. See .gitignore — jg/local.el is excluded from version control.
;;
;; COMMON OVERRIDES:
;;   - projectile-project-search-path   (different project dirs per machine)
;;   - shell-file-name / explicit-shell-file-name  (homebrew bash on mac, /usr/bin/bash on linux)
;;   - exec-path-from-shell-variables   (machine-specific env vars to import)
;;   - anything else that differs between your machines
(let ((local-file (concat user-emacs-directory "jg/local.el")))
  (when (file-exists-p local-file)
    (load local-file)))

(setq custom-file "~/.emacs.d/jg/custom.el")
(load custom-file)

;; color theme stuff.
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "jg/color-themes/jg-zenburn"))
(load-theme 'jg-zenburn t)


(load-library "jg-straight")        ; load packages via straight, and configuration per package with use-package
(load-library "jg-navigation-mode") ; navigation keys
(load-library "jg-minibuffer-mode") ; minimal navigation keys specifically for minibuffer
(load-library "jg-code-mode")       ; text/code editing keys
(load-library "jg-comint-mode")     ; keys for shell mode and other comint-derived repl modes
(load-library "jg-modes")           ; my various major/minor modes and their setups
(load-library "jg-functions")       ; custom functions I've written to make me faster :) also useful stuff I found on the internet
(load-library "jg-setup")           ; basic stock on/off switches and stuff.
(load-library "jg-keys")            ; rebinding standard-lib bindings

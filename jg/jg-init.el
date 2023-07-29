(setq custom-file "~/.emacs.d/jg/custom.el")
(load custom-file)

;; color theme stuff.
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "jg/color-themes/jg-zenburn"))
(load-theme 'jg-zenburn t)


(load-library "jg-straight")        ; load packages via straight.el
(load-library "jg-modes")           ; my various major/minor modes and their setups
(load-library "jg-functions")       ; custom functions I've written to make me faster :) also useful stuff I found on the internet
(load-library "jg-setup")           ; basic stock on/off switches and stuff.
(load-library "jg-mode")            ; my keys. they are sweeet.

(persp-state-load (concat user-emacs-directory ".persp"))

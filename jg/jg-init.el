;; color theme stuff. 
(add-to-list 'custom-theme-load-path (concat emacs-root "jg/color-themes/jg-zenburn"))
(load-theme 'jg-zenburn t)
(require 'unicode-fonts)
(unicode-fonts-setup)

(require 'tramp)

(add-to-list 'load-path (concat emacs-root "jg/pbcopy.el"))
(require 'pbcopy)

(add-to-list 'load-path (concat emacs-root "jg"))
(require 'wcy-swbuff)


(load-library "jg-modes")           ; my various major/minor modes and their setups
(load-library "jg-functions")       ; custom functions I've written to make me faster :) also useful stuff I found on the internet
(load-library "jg-setup")           ; basic stock on/off switches and stuff.
(load-library "jg-mode")            ; my keys. they are sweeet.

(server-start)
(add-hook 'after-init-hook '(lambda ()
                              (cd emacs-root)))


(require 'cl)
(defvar emacs-root 
  (if (eq system-type 'darwin)
      "/Users/jgran/my-emacs/"
    "C:/emacs/my-emacs/"))
(labels ((add-path (p) 
                   (add-to-list 'load-path 
                                (concat emacs-root p))))
  (add-path "") ; root
  (add-path "emacs-goodies-el")
  (add-path "ruby-mode")
  ;(add-path "Enhanced-Ruby-Mode")
  (add-path "rails-minor-mode")
  (add-path "php-mode")
  (add-path "rvm.el")
  (add-path "coffee-mode")
  (add-path "rspec-mode")
  (add-path "shoulda-mode")
  (add-path "nxhtml")
  (add-path "maxframe")
)

(load "mode-compile-ext.el")
(load "nxhtml/autostart.el")

(load-library "jg-colors")     ; my color scheme setup
(load-library "jg-modes")      ; my various major/minor modes and their setups
(load-library "jg-functions")  ; custom functions I've written to make me faster :)
(load-library "jg-setup")      ; basic stock on/off switches and stuff.
(load-library "jg-mode")       ; my keys. they are sweeet.

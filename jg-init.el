(require 'cl)
(defvar emacs-root 
  (if (eq system-type 'darwin)
      "/Users/jgran/"
    "C:/"))
(labels ((add-path (p) 
                   (add-to-list 'load-path 
                                (concat emacs-root p))))
  (add-path "my-emacs")
  (add-path "my-emacs/emacs-goodies-el")
  (add-path "my-emacs/ruby-mode")
  (add-path "my-emacs/rails-minor-mode")
  (add-path "my-emacs/php-mode")
  (add-path "my-emacs/rvm.el")
  (add-path "my-emacs/rspec-mode")
  (add-path "my-emacs/shoulda-mode")
  (add-path "my-emacs/nxhtml")
)

(load "mode-compile-ext.el")
(load "nxhtml/autostart.el")


(load-library "jg-colors")     ; my color scheme setup
(load-library "jg-modes")      ; my various major/minor modes and their setups
(load-library "jg-functions")  ; custom functions I've written to make me faster :)
(load-library "jg-setup")      ; basic stock on/off switches and stuff.
(load-library "jg-mode")       ; my keys. they are sweeet.

;; default project
(set-project "indra")

(prelude-require-packages
 '(
   s
   maxframe
   back-button
   modeline-posn
   ;; auto-complete
   ;; auto-complete-pcmp
   company
   ;;ac-js2
   coffee-mode
   ruby-mode
   mmm-mode ; multi major mode
   ;;enh-ruby-mode
                                        ;ruby-electric
   ;;rsense
   inf-ruby
   ;;ac-inf-ruby
   paredit
   rainbow-delimiters
   elscreen
   ;;readline-complete
   dired+
   ssh
   smex
   expand-region
   helm
   view
   php-mode
   js3-mode ;; javascript
   zoom-frm
   smartparens ; instead of paredit, gives "wrap" commands too
   robe ; smart ruby completion and definitions, etc?
   rspec-mode ; from peter, I think. looks newly updated.
   eww-lnum
                                        ;etags-select ; select from multiple of the same tag
   ggtags ; instead of or also with? ctags
   
   ;; ideas I wanna maybe try
                                        ; cycbuf ; instead of wcy-swbuff
                                        ; adaptive-wrap ; instead of srb-adaptive-wrap
                                        ; redo+ ; instead of redo.el
                                        ; do i really need mode-compile.el?
   ))



(require 'cl)                           ; common-lisp
(require 's)  ; string manip library

(defvar emacs-root 
  (if (eq system-type 'darwin)
      (concat (s-trim (shell-command-to-string "echo $HOME")) "/my-emacs/")
    "C:/emacs/my-emacs/"))
(cl-labels ((add-path (p) 
                   (add-to-list 'load-path 
                                (concat emacs-root p))))
  (add-path "") ; root
  ;;(add-path "nxhtml")
  (add-path "apel")
  (add-path "jg-quicknav")
  (add-path "emacs-goodies-el")
)


;; color theme stuff. 
(add-to-list 'custom-theme-load-path (concat emacs-root "color-themes"))
(add-to-list 'custom-theme-load-path (concat emacs-root "color-themes/emacs-color-theme-solarized"))
(add-to-list 'custom-theme-load-path (concat emacs-root "color-themes/jg-zenburn"))

(disable-theme 'zenburn) ;; this leaves traces behind unless you disable it first
(load-theme 'jg-zenburn)

(load "mode-compile-ext.el")



(load-library "jg-modes")           ; my various major/minor modes and their setups
(load-library "jg-functions")       ; custom functions I've written to make me faster :) also useful stuff I found on the internet
(load-library "jg-setup")           ; basic stock on/off switches and stuff.
(load-library "jg-mode")            ; my keys. they are sweeet.


(server-start)
(cd emacs-root)

(require 'cask)
(cask-initialize)
(require 'pallet)
(pallet-mode t)



(require 'cl)                           ; common-lisp
(require 's)                            ; string manip library

(defvar emacs-root 
  (if (eq system-type 'darwin)
      (concat (s-trim (shell-command-to-string "echo $HOME")) "/.emacs.d/")
    "C:/emacs/my-emacs/")) ; will need to be fixed if I ever go back to windoze

(add-to-list 'load-path (concat emacs-root "jg"))
(load-library "jg-init")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#000000" "#cd3278" "#94BC2D" "#FFEA77" "#0c5093" "#FA4785" "#3AaCbf" "#f0f0f0"])
 '(blink-cursor-mode t)
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("21d5f90e7565c1edc6db8cd4ae7adb7b81e0fa4f1be2726bfdbbd9ab7dbfbd6d" "e6d2471579829f38c348f5fb922e273eff858c7c7687bfa870f075552d9dbfa7" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "2a953a5d70fdfb1aeb07b81b3c3d703d5ed0359f932896a49b45784ef3b37dfa" default)))
 '(fci-rule-color "#102a33")
 '(js2-basic-offset 2)
 '(jsx-indent-level 2)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-push-arguments nil)
 '(markdown-command "maruku")
 '(server-kill-new-buffers t)
 '(speedbar-frame-parameters
   (quote
    ((minibuffer)
     (width . 20)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (set-background-color "black"))))
 '(tab-width 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

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
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   (quote
    ("2a953a5d70fdfb1aeb07b81b3c3d703d5ed0359f932896a49b45784ef3b37dfa" "7921ca5cec91d5a60ad5534f7d08bf9765c1ea11cc779768d5593734e310b98e" "321df602b729681183f5d6d087c8f1d03d03046b7f6dded4c90fcbc4fc483e14" "ef6e4ac99715cdd005dfd8e71bd720285d0989eddd4d4290ad8b33ceee503d8b" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "68da95b5bfb09845639a397493e32fbffa29520d9b52cab29c1e4bee31755a6e" default)))

 '(fci-rule-color "#102a33")
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
     (set-background-color "black")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

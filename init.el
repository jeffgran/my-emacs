
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)



(require 'cl)                           ; common-lisp
(require 's)                            ; string manip library

(defvar emacs-root 
  (if (or (eq system-type 'darwin) (eq system-type 'gnu/linux))
      (concat (s-trim (shell-command-to-string "echo $HOME")) "/.emacs.d/")
    "C:/emacs/my-emacs/")) ; will need to be fixed if I ever go back to windoze

(add-to-list 'load-path (concat emacs-root "jg"))
(load-library "jg-init")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments
   (quote
    ("--line-number" "--smart-case" "--nogroup" "--column" "--stats" "--ignore" "react_bundle.js.map" "--ignore" "react_bundle.js" "--")))
 '(ansi-color-names-vector
   ["#000000" "#cd3278" "#94BC2D" "#FFEA77" "#0c5093" "#FA4785" "#2aa198" "#f0f0f0"])
 '(blink-cursor-mode t)
 '(coffee-tab-width 2)
 '(css-indent-offset 2)
 '(custom-safe-themes
   (quote
    ("21d5f90e7565c1edc6db8cd4ae7adb7b81e0fa4f1be2726bfdbbd9ab7dbfbd6d" "e6d2471579829f38c348f5fb922e273eff858c7c7687bfa870f075552d9dbfa7" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "2a953a5d70fdfb1aeb07b81b3c3d703d5ed0359f932896a49b45784ef3b37dfa" default)))
 '(fci-rule-color "#102b53")
 '(flycheck-javascript-flow-args nil)
 '(js2-basic-offset 2 t)
 '(jsx-indent-level 2)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-push-arguments nil)
 '(markdown-command "maruku")
 '(package-selected-packages
   (quote
    (tide elixir-mode graphql-mode lsp-typescript company-lsp lsp-mode lsp-sourcekit company-terraform terraform-mode swift-mode web-mode fish-completion fish-mode helm-fuzzy-find enh-ruby-mode rubocop dired-subtree company-flow flycheck-flow flow-mode zoom-frm yaml-mode ws-butler vkill vimrc-mode use-package unicode-fonts swbuff ssh smooth-scroll smex smartparens smart-mode-line slim-mode scad-mode rust-mode ruby-additional rspec-mode redo+ readline-complete rbenv rake rainbow-mode rainbow-delimiters python-mode projectile prodigy processing-mode popwin ponylang-mode php-mode phi-rectangle pbcopy paredit pallet nyan-mode multiple-cursors modeline-posn maxframe markdown-mode magit lua-mode less-css-mode keyfreq jsx-mode json-mode js2-mode jg-quicknav ido-completing-read+ idle-highlight-mode htmlize haml-mode god-mode git-timemachine ggtags flycheck-package flycheck-cask flx-ido fiplr expand-region exec-path-from-shell eww-lnum ensime elscreen-buffer-group drag-stuff dired+ deferred csv-mode coffee-mode browse-kill-ring back-button ag adaptive-wrap)))
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
 '(swift-mode:basic-offset 4)
 '(swift-mode:multiline-statement-offset 4)
 '(swift-mode:parenthesized-expression-offset 4)
 '(tab-width 2)
 '(typescript-indent-level 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ensime-warnline-highlight ((t nil))))
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

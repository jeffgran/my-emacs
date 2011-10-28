; custom setup stuff.
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

(setq
   backup-by-copying t                             ; don't clobber symlinks
   backup-directory-alist '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t                               ; use versioned backups
   x-select-enable-clipboard t
   shift-select-mode nil
   cua-highlight-region-shift-only nil
)


(custom-set-variables
 '(blink-cursor-mode t)
 '(cua-mode t nil (cua-base))
 '(markdown-command "maruku")
 '(server-kill-new-buffers t)
 '(speedbar-frame-parameters 
   (quote ((minibuffer) 
           (width . 20) 
           (border-width . 0)
           (menu-bar-lines . 0) 
           (tool-bar-lines . 0)
           (unsplittable . t)
           (set-background-color "black"))))
 )

(setq-default cursor-type 'bar)
(global-auto-revert-mode t)
(global-font-lock-mode 1)
(tool-bar-mode 0)
(setq scroll-bar-mode-explicit t)
;(cua-mode t)
(global-hl-line-mode 1)
(icomplete-mode 1)



;; windows specific stuff
(if (eq system-type 'windows-nt)
    (progn
      (setq find-program "C:/GnuWin32/bin/find.exe")
      (setq grep-program "C:/GnuWin32/bin/grep.exe")
))

(if (eq system-type 'darwin)
    (progn
      (set-project "indra")
))

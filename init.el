(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(defvar emacs-root
  (if (eq system-type 'darwin)
      (concat (s-trim (shell-command-to-string "echo $HOME")) "/.emacs.d/")
    "C:/emacs/my-emacs/")) ; will need to be fixed if I ever go back to windoze

(add-to-list 'load-path (concat emacs-root "jg"))
(load-library "jg-init")

;; custom setup stuff.
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;; "speed up buffers with long lines" who knows why
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq split-height-threshold 9999
      split-width-threshold 160)

(require 'epg)
(setq epg-pinentry-mode 'loopback)

(setq
 backup-by-copying t                          ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.saves")) ; don't litter my fs tree
 tramp-backup-directory-alist backup-directory-alist
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t                      ; use versioned backups
 x-select-enable-clipboard t

 comint-scroll-to-bottom-on-output nil ; always add output at the bottom
 comint-scroll-to-bottom-on-input nil ; always put input at the bottom
 comint-scroll-show-maximum-output nil ; don't scroll to bottom maybe?
 comint-prompt-read-only t             ; what it says
 comint-buffer-maximum-size 10000 ; max scrollback size in comint/shell/etc

 enable-recursive-minibuffers t ; run a subcommand in a minibuffer to "pipe" the output from one to the other.

 ;; dired
 dired-listing-switches "-AlhopF"

 tags-add-tables t ; when loading up a second tags table, "add" it to the tags, instead of replacing (or asking which)
 )

(setq font-lock-global-modes t)

(setq debug-on-error nil)

;; default encoding for new buffers, among other default settings and fallbacks.
(prefer-coding-system 'utf-8) 



;; osx specific
(when (memq window-system '(mac ns))

  ;; load up a shell and snarf the env vars from there (like $PATH eg)
  ;; so that shell commands etc work inside emacs the same as they would from a shell.
  (require 'exec-path-from-shell)
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize)

  ;;osx keys
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'alt)

  (setq dired-use-ls-dired nil) ; osx `ls` does not support `--dired` flag
  )


(setq-default cursor-type 'bar)
(global-auto-revert-mode t)
(global-font-lock-mode 1)
(tool-bar-mode 0)
(setq scroll-bar-mode-explicit t)
                                        ;(cua-mode t)
(global-hl-line-mode 1)
(icomplete-mode 1)

(setq help-window-select t)



;; windows specific stuff
(if (eq system-type 'windows-nt)
    (progn
      (setq find-program "C:/GnuWin32/bin/find.exe")
      (setq grep-program "C:/GnuWin32/bin/grep.exe")
      ))

(setq custom-file "~/.emacs.d/jg/custom.el")
(load custom-file)

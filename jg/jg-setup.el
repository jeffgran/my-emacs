; custom setup stuff.
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; the above breaks saving remote tramp files, this fixes it, I have no idea how either mechanism works.
(setq tramp-backup-directory-alist backup-directory-alist)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq split-height-threshold 9999
      split-width-threshold 160)

(setq
   backup-by-copying t                             ; don't clobber symlinks
   backup-directory-alist '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t                               ; use versioned backups
   x-select-enable-clipboard t
   ;;shift-select-mode nil
   ;;cua-highlight-region-shift-only nil

   ;explicit-shell-file-name "/bin/bash"           ; for remote ssh sessions, for which shell to run remotely?
   comint-scroll-to-bottom-on-output nil           ; always add output at the bottom
   comint-scroll-to-bottom-on-input nil            ; always put input at the bottom 
   ;;shell-command-switch "-ic"                    ; for local bash, use -ic instead of -c so i can use my bash_aliases
   comint-scroll-show-maximum-output nil           ; don't scroll to bottom maybe?
   comint-prompt-read-only t                       ; what it says
   comint-buffer-maximum-size 10000                ; max scrollback size in comint/shell/etc


   enable-recursive-minibuffers t                  ; run a subcommand in a minibuffer to "pipe" the output from one to the other.

   ;; dired
   dired-listing-switches "-AlhopF"
   
   tags-add-tables t                               ; when loading up a second tags table, "add" it to the tags,
                                                   ; instead of replacing (or asking which)
)

(setq font-lock-global-modes t)

(setq helm-split-window-default-side 'right)
(setq debug-on-error t)
;; (setq max-specpdl-size 10000)
;; (setq max-lisp-eval-depth 10000)

;; default encoding for new buffers, among other default settings and fallbacks.
(prefer-coding-system 'utf-8) 

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(autoload 'vkill "vkill" nil t)

;; tramp, for sudo access
(require 'tramp)
(setq tramp-default-method "ssh") ; default is "scp"

(defun snippet-insert (foo &rest))

(custom-set-variables
 '(blink-cursor-mode t)
 ;;'(cua-mode t nil (cua-base))
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

(setq help-window-select t)

;;osx keys
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'alt)


;; windows specific stuff
(if (eq system-type 'windows-nt)
    (progn
      (setq find-program "C:/GnuWin32/bin/find.exe")
      (setq grep-program "C:/GnuWin32/bin/grep.exe")
))

;; save the "session" of windows/buffers/etc
;; um, this doesn't really seem to work very well.

;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
;; (setq desktop-save 'if-exists)
;; (desktop-save-mode 1)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
;;; (setq desktop-globals-to-save
;;;       (append '((extended-command-history . 30)
;;;                 (file-name-history        . 100)
;;;                 (grep-history             . 30)
;;;                 (compile-history          . 30)
;;;                 (minibuffer-history       . 50)
;;;                 (query-replace-history    . 60)
;;;                 (read-expression-history  . 60)
;;;                 (regexp-history           . 60)
;;;                 (regexp-search-ring       . 20)
;;;                 (search-ring              . 20)
;;;                 (shell-command-history    . 50)
;;;                 tags-file-name
;;;                 register-alist)))

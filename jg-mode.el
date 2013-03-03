(defvar jg-code-mode-map (make-sparse-keymap) "jg-code-mode-map.")
(defvar jg-navigation-mode-map (make-sparse-keymap) "jg-navigation-mode-map.")


;;*************************************************
;;               JG Navigation Mode
;;*************************************************

(define-key jg-navigation-mode-map (kbd "C-;") 'forward-char)
;;(define-key jg-navigation-mode-map (kbd "M-^") 'forward-char) ;; hack to use key translation in iterm to get this to work right in terminal
(define-key jg-navigation-mode-map (kbd "C-j") 'backward-char)
(define-key jg-navigation-mode-map (kbd "C-'") 'forward-word)
(define-key jg-navigation-mode-map (kbd "C-h") 'backward-word)
;;(define-key jg-navigation-mode-map (kbd "C-t") 'exchange-point-and-mark)



(define-key jg-navigation-mode-map (kbd "C-t") 'forward-to-char)
(define-key jg-navigation-mode-map (kbd "C-b") 'backward-to-char)


;; scrolling and line movement
(define-key jg-navigation-mode-map (kbd "M-C-n") 'View-scroll-half-page-forward)
(define-key jg-navigation-mode-map (kbd "M-C-p") 'View-scroll-half-page-backward)
(define-key jg-navigation-mode-map (kbd "C-M-.") 'end-of-buffer)
(define-key jg-navigation-mode-map (kbd "C-M-,") 'beginning-of-buffer)
(define-key jg-navigation-mode-map (kbd "C-l") 'goto-line)

(if window-system
    (progn
      ;; emacs is weird. have to have this to be able to bind C-i without also binding <tab>
      ;; doesn't work in terminal!
      (keyboard-translate ?\C-i ?\H-i)
      (define-key jg-navigation-mode-map (kbd "H-i") 'back-to-indentation)
      (define-key jg-navigation-mode-map (kbd "C-M-i") 'recenter-top-bottom)
      ))

(define-key jg-navigation-mode-map (kbd "M-i") 'indent-for-tab-command)

(if window-system
    (progn
      (keyboard-translate ?\C-m ?\H-m)
      (define-key jg-navigation-mode-map (kbd "C-m") nil)
      (define-key jg-navigation-mode-map (kbd "H-m") 'move-to-window-line-top-bottom)
      ))



;; "windows" (in emacs parlance)
(define-key jg-navigation-mode-map (kbd "M-o") 'other-window)
(define-key jg-navigation-mode-map (kbd "M-1") 'delete-other-windows)
(define-key jg-navigation-mode-map (kbd "M-2") 'split-window-below)
(define-key jg-navigation-mode-map (kbd "M-3") 'split-window-right)


;;screens (tabs)
(define-key jg-navigation-mode-map (kbd "M-t") 'elscreen-create)
(define-key jg-navigation-mode-map (kbd "M-w") 'elscreen-kill)
(define-key jg-navigation-mode-map (kbd "M-{") 'elscreen-previous)
(define-key jg-navigation-mode-map (kbd "M-}") 'elscreen-next)
(define-key jg-navigation-mode-map (kbd "A-0") '(lambda () (interactive) (elscreen-goto 0)))
(define-key jg-navigation-mode-map (kbd "A-1") '(lambda () (interactive) (elscreen-goto 1)))
(define-key jg-navigation-mode-map (kbd "A-2") '(lambda () (interactive) (elscreen-goto 2)))
(define-key jg-navigation-mode-map (kbd "A-3") '(lambda () (interactive) (elscreen-goto 3)))
(define-key jg-navigation-mode-map (kbd "A-4") '(lambda () (interactive) (elscreen-goto 4)))
(define-key jg-navigation-mode-map (kbd "A-5") '(lambda () (interactive) (elscreen-goto 5)))
(define-key jg-navigation-mode-map (kbd "A-6") '(lambda () (interactive) (elscreen-goto 6)))
(define-key jg-navigation-mode-map (kbd "A-7") '(lambda () (interactive) (elscreen-goto 7)))
(define-key jg-navigation-mode-map (kbd "A-8") '(lambda () (interactive) (elscreen-goto 8)))
(define-key jg-navigation-mode-map (kbd "A-9") '(lambda () (interactive) (elscreen-goto 9)))


;;buffers
(define-key jg-navigation-mode-map (kbd "C-w") 'kill-this-buffer)
(define-key jg-navigation-mode-map (kbd "C-q") 'keyboard-quit)
(define-key jg-navigation-mode-map (kbd "M-g") 'repeat)


;; select by semantic units. super helpful!
(define-key jg-navigation-mode-map (kbd "M-SPC") 'er/expand-region)
(define-key jg-navigation-mode-map (kbd "M-C-SPC") 'er/contract-region)


(define-key jg-navigation-mode-map (kbd "C-S-o") 'jg-fuzzy-find-in-project)
(define-key jg-navigation-mode-map (kbd "C-x p") 'ido-jg-set-project-root)


;;buffer switching. thanks to jg-elscreen-buffer-list, only switches buffer within the current tab.
(define-key jg-navigation-mode-map (kbd "<C-tab>") 'wcy-switch-buffer-forward)
(define-key jg-navigation-mode-map (kbd "<C-S-tab>") 'wcy-switch-buffer-backward)

(define-key jg-navigation-mode-map (kbd "M-b") 'helm-buffers-list)
;;(define-key jg-navigation-mode-map (kbd "M-b") 'electric-buffer-list)
(define-key jg-navigation-mode-map (kbd "C-M-o") 'helm-recentf)
;;(define-key jg-navigation-mode-map (kbd "C-o") 'ido-find-file)
(define-key jg-navigation-mode-map (kbd "C-o") 'jg-quicknav)
;;(define-key jg-navigation-mode-map (kbd "M-o") 'helm-find-files)



;; forward/back buttons like in a browser. go to the last place I was.
(define-key jg-navigation-mode-map (kbd "C-<") 'back-button-global-backward)
(define-key jg-navigation-mode-map (kbd "C->") 'back-button-global-forward)
;; same, but only within the current file
(define-key jg-navigation-mode-map (kbd "C-,") 'back-button-local-backward)
(define-key jg-navigation-mode-map (kbd "C-.") 'back-button-local-forward)

(define-key jg-navigation-mode-map (kbd "M-m") 'helm-imenu)

;; isearch
(define-key jg-navigation-mode-map (kbd "C-f") 'isearch-forward)
(define-key jg-navigation-mode-map (kbd "C-r") 'isearch-backward)
(define-key jg-navigation-mode-map (kbd "A-f") 'flex-isearch-forward)
(define-key jg-navigation-mode-map (kbd "A-r") 'flex-isearch-backward)



;; i guess exiting completely is navigation
(define-key jg-navigation-mode-map (kbd "M-q") 'save-buffers-kill-terminal)

;; smex is M-x but like ido-mode. sweet!
(define-key jg-navigation-mode-map (kbd "M-x") 'smex)

;; let's put the shell commands under the M-s prefix
(define-key jg-navigation-mode-map (kbd "M-S") 'jg-new-shell) ;; new shell in the current project root
(define-key jg-navigation-mode-map (kbd "M-s a") 'shell-command)
(define-key jg-navigation-mode-map (kbd "M-s o") 'async-shell-command) ;; Shell command, insert output Other buffer.
(define-key jg-navigation-mode-map (kbd "M-s R") 'shell-command-on-region-replace) ;; Shell command on Region, Replace region with output.
(define-key jg-navigation-mode-map (kbd "M-s r") 'shell-command-on-region) ;; Shell command on Region.
(define-key jg-navigation-mode-map (kbd "M-s h") 'shell-command-insert-output-here) ;; Shell command, insert output Here.

(define-key jg-navigation-mode-map (kbd "M-s s") 'jg-open-ssh) ;; Shell command, insert output Here.

(define-key isearch-mode-map(kbd "M-s h") 'shell-command-insert-output-here) ;; Shell command, insert output Here.

;;*************************************************
;;            End JG Navigation Mode
;;*************************************************






;;***************************
;; Text Surgery
;;***************************


(define-key jg-code-mode-map (kbd "M-P") 'duplicate-current-line-up)
(define-key jg-code-mode-map (kbd "M-N") 'duplicate-current-line-or-region)
(define-key jg-code-mode-map (kbd "RET") 'comment-indent-new-line)
(define-key jg-code-mode-map (kbd "M-RET") 'open-line-below)
(define-key jg-code-mode-map (kbd "<M-C-return>") 'open-line-above)

(define-key jg-code-mode-map (kbd "C-S-C") 'kill-ring-save)
(define-key jg-code-mode-map (kbd "C-S-K") 'kill-whole-line)
(define-key jg-code-mode-map (kbd "C-k") 'kill-line)



;; new and improved! move line OR region up and down!
(define-key jg-code-mode-map (kbd "M-p") 'move-text-up)
(define-key jg-code-mode-map (kbd "M-n") 'move-text-down)


;;(define-key jg-code-mode-map (kbd "C-\\") 'indent-for-tab-command)



;;***********************
;; Cut/Copy/Open/Save/Etc
;;***********************
(define-key jg-code-mode-map (kbd "C-v") 'clipboard-yank)
(define-key jg-code-mode-map (kbd "C-S-v") 'cua-paste-pop)
(define-key jg-code-mode-map (kbd "C-M-v") 'paste-unshift)


(define-key jg-code-mode-map (kbd "C-z") 'undo)
(define-key jg-code-mode-map (kbd "C-S-Z") 'redo)

(define-key jg-code-mode-map (kbd "C-s") 'save-buffer)
(define-key jg-code-mode-map (kbd "C-x o") 'helm-occur)
(define-key jg-code-mode-map (kbd "C-x n") 'elscreen-screen-nickname)


;;*******************
;; Search/Replace/Etc
;;*******************
(define-key jg-code-mode-map (kbd "C-S-f") 'rgrep)
(define-key jg-code-mode-map (kbd "C-M-f") 'grep-buffers)
(define-key jg-code-mode-map (kbd "C-S-r") 'query-replace)

(define-key jg-code-mode-map (kbd "C-c C-t") 'build-ctags)
(define-key jg-code-mode-map (kbd "C-8") 'find-tag)

;;***********************
;; Macros
;;***********************
(define-key jg-code-mode-map (kbd "C-5") 'view-percent)
(define-key jg-code-mode-map (kbd "C-%") 'view-percent-equal)
(define-key jg-code-mode-map (kbd "C-{") 'css-curlies)
(define-key jg-code-mode-map (kbd "C-#") 'comment-or-uncomment-region)
;;(define-key jg-code-mode-map (kbd "TAB") 'hippie-expand)
(define-key jg-code-mode-map (kbd "A-q") 'fill-paragraph)




;; not useful any more. was a trick to help convert hobo dryml templates
;;(define-key jg-code-mode-map (kbd "C-M-c") 'layout-content)

;;(define-key jg-code-mode-map (kbd "A-w") '(lambda () (interactive) (switch-to-buffer "*scratch*") (prelude-kill-other-buffers)) )



(define-key jg-code-mode-map (kbd "C-x r") 'rename-this-buffer-and-file)


;;**************
;; yasnippet
;;**************
;; take away tab because I want that to be tab-complete
;; (define-key yas-minor-mode-map (kbd "C-c ; u") 'yas-expand) ;;keybinding I'll never want
;; (setq yas-trigger-key nil)
;; ;; trigger yasnippet with helm interface
;; (define-key jg-code-mode-map (kbd "C-y") 'helm-c-yas-complete)





;; other things i rebind
(define-key jg-code-mode-map (kbd "M-C-x") 'eval-expression)
(define-key shell-mode-map (kbd "M-C-x") 'eval-expression)


;; set up a new help key prefix since I use C-h for movement
;; this breaks shit. apparently it has to be a single key, not a sequence.
;; (if window-system
;;     (progn
;;       (setq help-char ?\M-?)
;;       (global-set-key "\M-?" 'help-for-help)
;;       ))
(define-key help-mode-map (kbd "B") 'help-go-back)
(define-key help-mode-map (kbd "F") 'help-go-forward)


(define-minor-mode jg-code-mode "JG Code Editing Mode Keys" t " [JGc]" 'jg-code-mode-map)
(define-minor-mode jg-navigation-mode "JG Navigation Mode Keys" t " [JGn]" 'jg-navigation-mode-map)
(jg-code-mode 1)
(jg-navigation-mode 1)
;;(yas-minor-mode -1)




;; ==========================================================
;; Other modes and hooks I have to "fix" to work with jg-mode
;; ==========================================================



;; don't break my C-j
(require 'paredit)
(define-key paredit-mode-map (kbd "C-j") nil)



;; fix the ruby-tools keymap
(require 'ruby-tools)
(define-key ruby-tools-mode-map (kbd "C-'") nil)
(define-key ruby-tools-mode-map (kbd "C-\"") nil)
(define-key ruby-tools-mode-map (kbd "C-:") nil)
(define-key ruby-tools-mode-map (kbd "C-;") nil)
;;(define-key ruby-tools-mode-map (kbd "#") nil)

(define-key ruby-mode-map (kbd "TAB") nil)
(define-key ruby-mode-map (kbd "C-j") nil) ;; unbind the ruby C-j because it conflicts

(define-key ruby-tools-mode-map (kbd "M-'") 'ruby-tools-to-single-quote-string)
(define-key ruby-tools-mode-map (kbd "M-\"") 'ruby-tools-to-double-quote-string)
(define-key ruby-tools-mode-map (kbd "M-:") 'ruby-tools-to-symbol)


;; for some reason they don't take effect unless I bind them every time, in this hook.
(add-hook 'ido-setup-hook 'ido-jg-keys)
(defun ido-jg-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  )



(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-q") 'isearch-abort)
(define-key isearch-mode-map (kbd "TAB") 'isearch-complete)
(define-key isearch-mode-map (kbd "C-w") 'isearch-yank-symbol-string)
(define-key isearch-mode-map (kbd "A-z") 'isearch-toggle-regexp)
(define-key minibuffer-local-isearch-map (kbd "TAB") 'isearch-complete-edit)


;; turn jg-code-mode off for buffers where I'm not editing the text ... it messes stuff up.
(defun disable-jg-code-mode ()
  (jg-code-mode 0)
  )

(add-hook 'minibuffer-setup-hook 'disable-jg-code-mode)
(add-hook 'help-mode-hook 'disable-jg-code-mode)
(add-hook 'compilation-mode-hook 'disable-jg-code-mode)
(add-hook 'grep-mode-hook 'disable-jg-code-mode)
(add-hook 'erc-mode-hook 'disable-jg-code-mode)
(add-hook 'eshell-mode-hook 'disable-jg-code-mode)
(add-hook 'shell-mode-hook 'disable-jg-code-mode)


(defun c-k-clear-for-term-mode ()
  (define-key (current-local-map) (kbd "M-k") '(lambda () (interactive) (term-send-raw-string "clear\r") ))
  (define-key (current-local-map) (kbd "M-w") '(lambda () (interactive) (term-send-raw-string "exit\r") ))
  )


(defadvice term-mode (after term-mode-fixes ())
  (linum-mode 0)
  (disable-jg-code-mode)
  (c-k-clear-for-term-mode)
  )

(defun term-send-C-r ()
  (interactive)
  (term-send-raw-string "\C-r")
  )


;; enable cua and transient mark modes in term-line-mode
(defadvice term-line-mode (after term-line-mode-fixes ())
  (set (make-local-variable 'cua-mode) t)
  (set (make-local-variable 'transient-mark-mode) t)
  (define-key (current-local-map) (kbd "M-RET") 'term-char-mode)
  (define-key (current-local-map) (kbd "M-SPC") 'er/expand-region)
  (define-key (current-local-map) (kbd "M-C-SPC") 'er/contract-region)
  (c-k-clear-for-term-mode)
  ;;
  (define-key term-mode-map (kbd "C-j") nil)
  (define-key term-mode-map (kbd "C-;") nil)
  )
(ad-activate 'term-line-mode)

;; disable cua and transient mark modes in term-char-mode
(defadvice term-char-mode (after term-char-mode-fixes ())
  (set (make-local-variable 'cua-mode) nil)
  (set (make-local-variable 'transient-mark-mode) nil)
  (define-key (current-local-map) (kbd "M-RET") 'term-line-mode)
  (define-key (current-local-map) (kbd "M-r") 'term-send-C-r)
  (c-k-clear-for-term-mode)
  ;; (define-key (current-local-map) (kbd "C-h") '(lambda () (interactive) (term-send-raw-string "\M-b")))
  ;; (define-key (current-local-map) (kbd "C-d") '(lambda () (interactive) (term-send-raw-string "\C-d")))
  (define-key (current-local-map) (kbd "M-&") '(lambda () (interactive) (term-send-raw-string "\C-b")))
  (define-key (current-local-map) (kbd "M-^") '(lambda () (interactive) (term-send-raw-string "\C-f")))
  )
(ad-activate 'term-char-mode)

;; dired stuff
(define-key dired-mode-map "B" 'dired-up-directory)
(define-key dired-mode-map "f" 'dired-isearch-filenames)
(add-hook 'dired-mode-hook 'disable-jg-code-mode)

;; shell-mode stuff
(define-key shell-mode-map (kbd "M-k") 'clear-shell)
(define-key shell-mode-map (kbd "C-o") (kbd "C-x C-f RET"))
(define-key shell-mode-map (kbd "C-S-f") 'rgrep)
(define-key shell-mode-map (kbd "C-S-v") 'cua-paste-pop)
(define-key shell-mode-map (kbd "C-M-v") 'paste-unshift)
(define-key shell-mode-map (kbd "M-.") 'comint-restore-input)
(define-key shell-mode-map (kbd "TAB") nil)
;; ssh-mode
(require 'ssh)
(define-key ssh-mode-map (kbd "TAB") nil)

;; scratch buffer
;(define-key lisp-interaction-mode (kbd "C-c C-j") 'eval-print-last-sexp)


(load "eww")                            ; pre-load this so I can set up my customizations
(load "shell")                          ; same


(global-set-key [remap move-beginning-of-line] 'move-beginning-of-line)
(global-set-key (kbd "M-=") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)

(require 'god-mode)
;;(global-set-key (kbd "<escape>") 'god-mode-all)
;;(global-set-key (kbd "C-<return>") 'god-mode-all)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
;;(define-key god-local-mode-map (kbd "c") 'kill-ring-save)


(defvar jg-code-mode-map (make-sparse-keymap) "jg-code-mode-map.")
(defvar jg-navigation-mode-map (make-sparse-keymap) "jg-navigation-mode-map.")


;;*************************************************
;;               JG Navigation Mode
;;*************************************************
(setq shift-select-mode nil)
(define-key jg-navigation-mode-map (kbd "C-l") 'forward-char)
(define-key jg-navigation-mode-map (kbd "C-j") 'backward-char)
(define-key jg-navigation-mode-map (kbd "C-;") 'forward-word)
(define-key jg-navigation-mode-map (kbd "C-\"") nil)
(define-key jg-navigation-mode-map (kbd "C-h") 'backward-word)

;; real toggle mark command. how does this not exist? but it doesn't
(define-key jg-navigation-mode-map (kbd "C-f") '(lambda ()
                                                  (interactive)
                                                  (if (region-active-p)
                                                      (deactivate-mark)
                                                    (call-interactively 'set-mark-command))))
;;(define-key jg-navigation-mode-map (kbd "C-t") 'exchange-point-and-mark)



(define-key jg-navigation-mode-map (kbd "C-t") 'forward-to-char)
(define-key jg-navigation-mode-map (kbd "C-b") 'backward-to-char)


;; scrolling and line movement
(define-key jg-navigation-mode-map (kbd "M-C-n") 'View-scroll-half-page-forward)
(define-key jg-navigation-mode-map (kbd "M-C-p") 'View-scroll-half-page-backward)
(define-key jg-navigation-mode-map (kbd "C-M-.") 'end-of-buffer)
(define-key jg-navigation-mode-map (kbd "C-M-,") 'beginning-of-buffer)
(define-key jg-navigation-mode-map (kbd "M-l") 'goto-line)
;; skipping to matching brackets
(define-key jg-navigation-mode-map (kbd "M-C-h") 'backward-list)
(define-key jg-navigation-mode-map (kbd "M-C-;") 'forward-list)

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
      (define-key jg-navigation-mode-map (kbd "H-m") 'set-mark-command)
      ))



;; "windows" (in emacs parlance)
(define-key jg-navigation-mode-map (kbd "M-0") 'other-window)
(define-key jg-navigation-mode-map (kbd "M-1") 'delete-other-windows)
(define-key jg-navigation-mode-map (kbd "M-2") 'split-window-below)
(define-key jg-navigation-mode-map (kbd "M-3") 'split-window-right)


;;screens (tabs)
(define-key jg-navigation-mode-map (kbd "M-t") 'elscreen-create)
(define-key jg-navigation-mode-map (kbd "M-w") 'elscreen-kill)
(define-key jg-navigation-mode-map (kbd "M-{") 'elscreen-previous)
(define-key jg-navigation-mode-map (kbd "<C-M-left>") 'elscreen-previous)
(define-key jg-navigation-mode-map (kbd "M-}") 'elscreen-next)
(define-key jg-navigation-mode-map (kbd "<C-M-right>") 'elscreen-next)
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
(define-key jg-navigation-mode-map (kbd "M-g") nil)


;; select by semantic units. super helpful!
(define-key jg-navigation-mode-map (kbd "M-SPC") 'er/expand-region)
(define-key jg-navigation-mode-map (kbd "M-C-SPC") 'er/contract-region)

;; my custom selection stuff
(define-key jg-navigation-mode-map (kbd "M-e") 'select-whole-line-or-lines)
(define-key jg-navigation-mode-map (kbd "M-a") 'select-whole-line-or-lines-backwards)
(define-key jg-navigation-mode-map (kbd "C-a") 'back-to-indentation-or-beginning)
(define-key jg-navigation-mode-map (kbd "<home>") 'back-to-indentation-or-beginning)
(define-key jg-navigation-mode-map (kbd "<end>") 'move-end-of-line)

;;(define-key jg-navigation-mode-map (kbd "C-=") 'cua-set-mark)

(define-key jg-navigation-mode-map (kbd "<help>") 'set-mark-command)
(define-key jg-navigation-mode-map (kbd "<escape>") 'keyboard-quit)

(define-key jg-navigation-mode-map (kbd "C-S-o") 'fiplr-find-file)
(define-key jg-navigation-mode-map (kbd "C-x p") 'ido-jg-set-project-root)

(define-key jg-navigation-mode-map (kbd "C-x k") 'rake)


(define-key jg-navigation-mode-map (kbd "C-x C-b") 'ibuffer)

;;buffer switching. thanks to jg-elscreen-buffer-list, only switches buffer within the current tab.
;; (define-key jg-navigation-mode-map (kbd "<C-tab>") 'wcy-switch-buffer-forward)
;; (define-key jg-navigation-mode-map (kbd "<C-S-tab>") 'wcy-switch-buffer-backward)
(define-key jg-navigation-mode-map (kbd "<C-tab>") 'jg-switch-buffer)
(define-key jg-navigation-mode-map (kbd "C-v") 'jg-switch-buffer)

(define-key jg-navigation-mode-map (kbd "M-b") 'helm-buffers-list)

;;(define-key jg-navigation-mode-map (kbd "M-b") 'electric-buffer-list)
(define-key jg-navigation-mode-map (kbd "C-M-o") 'helm-recentf)
;;(define-key jg-navigation-mode-map (kbd "C-o") 'ido-find-file)
(define-key jg-navigation-mode-map (kbd "C-o") 'jg-quicknav)
(define-key jg-navigation-mode-map (kbd "M-o") 'find-file-at-point-with-line)
(define-key jg-navigation-mode-map (kbd "C-/") '(lambda ()
                                                  (interactive)
                                                  (dired default-directory)))



;; forward/back buttons like in a browser. go to the last place I was.
(define-key jg-navigation-mode-map (kbd "C-<") 'back-button-global-backward)
(define-key jg-navigation-mode-map (kbd "C->") 'back-button-global-forward)
;; same, but only within the current file
(define-key jg-navigation-mode-map (kbd "C-,") 'back-button-local-backward)
(define-key jg-navigation-mode-map (kbd "C-.") 'back-button-local-forward)

;; (define-key jg-navigation-mode-map (kbd "M-m") 'helm-imenu)

;; isearch
(define-key jg-navigation-mode-map (kbd "M-f") 'isearch-forward)
(define-key jg-navigation-mode-map (kbd "M-r") 'isearch-backward)
;; (define-key jg-navigation-mode-map (kbd "A-f") 'flex-isearch-forward)
;; (define-key jg-navigation-mode-map (kbd "A-r") 'flex-isearch-backward)



;; i guess exiting completely is navigation
(define-key jg-navigation-mode-map (kbd "M-q") 'save-buffers-kill-terminal)

;; smex is M-x but like ido-mode. sweet!
(define-key jg-navigation-mode-map (kbd "A-x") 'smex)
(define-key jg-navigation-mode-map (kbd "s-x") 'smex)


;; let's put the shell commands under the M-s prefix
(define-key jg-navigation-mode-map (kbd "C-A-s") 'jg-new-shell) ;; new shell in the current project root
;; (define-key jg-navigation-mode-map (kbd "M-s a") 'shell-command)
;; (define-key jg-navigation-mode-map (kbd "M-s a") nil)
;; (define-key jg-navigation-mode-map (kbd "M-s o") 'async-shell-command) ;; Shell command, insert output Other buffer.
;; (define-key jg-navigation-mode-map (kbd "M-s o") nil) ;; Shell command, insert output Other buffer.
;; (define-key jg-navigation-mode-map (kbd "M-s R") 'shell-command-on-region-replace) ;; Shell command on Region, Replace region with output.
;; (define-key jg-navigation-mode-map (kbd "M-s R") nil) ;; Shell command on Region, Replace region with output.
;; (define-key jg-navigation-mode-map (kbd "M-s r") 'shell-command-on-region) ;; Shell command on Region.
;; (define-key jg-navigation-mode-map (kbd "M-s r") nil) ;; Shell command on Region.
;; (define-key jg-navigation-mode-map (kbd "M-s h") 'shell-command-insert-output-here) ;; Shell command, insert output Here.
;; (define-key jg-navigation-mode-map (kbd "M-s h") nil) ;; Shell command, insert output Here.

(define-key jg-navigation-mode-map (kbd "C-s s") 'jg-open-ssh)
(define-key jg-navigation-mode-map (kbd "C-s b") 'ensime-sbt)

(define-key jg-navigation-mode-map (kbd "M-R") 'jg-new-inf-ruby) ;; new irb in the current project root
(define-key jg-navigation-mode-map (kbd "C-c 0") 'copy-buffer-file-name-as-kill)

;; (define-key isearch-mode-map(kbd "M-s h") 'shell-command-insert-output-here) ;; Shell command, insert output Here.
;; (define-key isearch-mode-map(kbd "M-s h") nil) ;; Shell command, insert output Here.

;;*************************************************
;;            End JG Navigation Mode
;;*************************************************





;;***************************
;; Text Surgery
;;***************************


(define-key jg-code-mode-map (kbd "M-P") 'duplicate-current-line-or-region-up)
(define-key jg-code-mode-map (kbd "M-N") 'duplicate-current-line-or-region)

(define-key jg-code-mode-map (kbd "M-RET") 'open-line-below)

(define-key jg-code-mode-map (kbd "M-k") 'kill-region)
(define-key jg-code-mode-map (kbd "M-x") 'kill-region)
(define-key jg-navigation-mode-map (kbd "M-x") 'kill-region)
(define-key jg-code-mode-map (kbd "C-k") 'kill-line)




;; new and improved! move line OR region up and down!
(require 'drag-stuff)
(define-key jg-code-mode-map (kbd "M-p") 'drag-stuff-up)
(define-key jg-code-mode-map (kbd "M-n") 'drag-stuff-down)
(define-key jg-code-mode-map (kbd "C-M-\\") 'indent-region-or-buffer)

;;(define-key jg-code-mode-map (kbd "C-\\") 'indent-for-tab-command)


;;***********************
;; SmartParens
;;***********************
(define-key smartparens-mode-map (kbd "C-)") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-)") 'sp-slurp-hybrid-sexp)
(define-key smartparens-mode-map (kbd "C-(") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-}") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-{") 'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-\\") 'sp-rewrap-sexp)
(define-key smartparens-mode-map (kbd "C-|") 'sp-backward-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-r") nil)


;;***********************
;; Cut/Copy/Open/Save/Etc
;;***********************

(define-key jg-navigation-mode-map (kbd "M-c") 'kill-ring-save)
(define-key jg-navigation-mode-map (kbd "M-v") 'clipboard-yank)
(define-key jg-navigation-mode-map (kbd "M-V") 'yank-pop)
(define-key jg-navigation-mode-map (kbd "C-M-v") 'paste-unshift)


(define-key jg-code-mode-map (kbd "M-z") 'undo-tree-undo)
(define-key jg-code-mode-map (kbd "M-Z") 'undo-tree-redo)
(define-key jg-code-mode-map (kbd "C-M-z") 'undo-tree-visualize)



;;(define-key jg-code-mode-map (kbd "C-s") 'save-buffer)
(define-key jg-code-mode-map (kbd "M-s") nil)
(define-key jg-code-mode-map (kbd "M-s") 'save-buffer)
;;(define-key jg-code-mode-map (kbd "C-x o") 'helm-occur)
(define-key jg-code-mode-map (kbd "C-x n") 'elscreen-screen-nickname)
(define-key jg-code-mode-map (kbd "C-M-j") 'join-line)

;;*******************
;; Search/Replace/Etc
;;*******************
(define-key jg-code-mode-map (kbd "C-S-f") 'ag)
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

(define-key jg-navigation-mode-map (kbd "A-SPC") '(lambda () (interactive) (insert-char ?_ 1)))





(define-key jg-code-mode-map (kbd "C-x r") 'rename-this-buffer-and-file)



(define-key jg-code-mode-map (kbd "M-C-x") 'eval-expression)

;; other things i rebind
(define-key shell-mode-map (kbd "M-C-x") 'eval-expression)

(require 'magit)
(define-key jg-code-mode-map (kbd "M-g") 'magit-dispatch-popup)
(define-key shell-mode-map (kbd "M-g") 'magit-dispatch-popup)

(define-key magit-popup-mode-map (kbd "s") '(lambda ()
                                              (interactive)
                                              (magit-popup-quit)
                                              (magit-status)))
(define-key magit-popup-mode-map (kbd "g") '(lambda ()
                                              (interactive)
                                              (magit-popup-quit)
                                              (magit-refresh)))

;(define-key magit-popup-mode-map (kbd "h") 'magit refresh)

;(define-key jg-code-mode-map (kbd "M-g s") nil)
;(define-key jg-code-mode-map (kbd "M-g r") 'magit-refresh)
;;(define-key shell-mode-map (kbd "M-g s") 'magit-status)
;(define-key shell-mode-map (kbd "M-g p") 'magit-dispatch-popup)
;(define-key shell-mode-map (kbd "M-g r") 'magit-refresh)


;;(define-key jg-code-mode-map (kbd "M-g c") 'magit-checkout)
;;(define-key shell-mode-map (kbd "M-g c") 'magit-checkout)


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
;; or want to change their internal maps to my liking
;; ==========================================================



(require 'phi-rectangle)
(define-key phi-rectangle-mode-map (kbd "C-<return>") nil)
(define-key phi-rectangle-mode-map (kbd "C-M-<return>") 'phi-rectangle-set-mark-command)







;; company completion
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-f") 'company-filter-candidates)

;; don't break my C-j
(require 'paredit)
(define-key paredit-mode-map (kbd "C-j") nil)




;; some extra keys for ruby mode.
(define-key enh-ruby-mode-map (kbd "M-h") 'enh-ruby-backward-sexp)
(define-key enh-ruby-mode-map (kbd "M-'") 'enh-ruby-forward-sexp)
(define-key enh-ruby-mode-map (kbd "C-M-h") 'enh-ruby-beginning-of-block)
(define-key enh-ruby-mode-map (kbd "C-M-'") 'enh-ruby-end-of-block)






;; for some reason they don't take effect unless I bind them every time, in this hook.
(add-hook 'ido-setup-hook 'ido-jg-keys)
(defun ido-jg-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  )



(define-key isearch-mode-map (kbd "M-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-r") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-q") 'isearch-abort)
(define-key isearch-mode-map (kbd "TAB") 'isearch-complete)
(define-key isearch-mode-map (kbd "C-w") 'isearch-yank-symbol-string)
(define-key isearch-mode-map (kbd "M-i") 'isearch-yank-word-or-char)
(define-key isearch-mode-map (kbd "A-z") 'isearch-toggle-regexp)
(define-key minibuffer-local-isearch-map (kbd "TAB") 'isearch-complete-edit)


;; turn jg-code-mode off for buffers where I'm not editing the text ... it messes stuff up.
(defun disable-jg-code-mode ()
  (jg-code-mode 0))

(add-hook 'minibuffer-setup-hook 'disable-jg-code-mode)
(add-hook 'help-mode-hook 'disable-jg-code-mode)
(add-hook 'compilation-mode-hook 'disable-jg-code-mode)
(add-hook 'grep-mode-hook 'disable-jg-code-mode)
(add-hook 'erc-mode-hook 'disable-jg-code-mode)
(add-hook 'comint-mode-hook 'disable-jg-code-mode)
(add-hook 'custom-mode-hook 'disable-jg-code-mode)

(add-hook 'magit-mode-hook '(lambda ()
                              (jg-code-mode 0)
                              (jg-navigation-mode 0)))

(defun c-k-clear-for-term-mode ()
  (define-key (current-local-map) (kbd "M-k") '(lambda () (interactive) (term-send-raw-string "clear\r") )))


(defadvice term-mode (after term-mode-fixes ())
  (disable-jg-code-mode)
  (c-k-clear-for-term-mode))

(defun term-send-C-r ()
  (interactive)
  (term-send-raw-string "\C-r"))




;; enable cua and transient mark modes in term-line-mode
;; (defadvice term-line-mode (after term-line-mode-fixes ())
;;   (set (make-local-variable 'cua-mode) t)
;;   (set (make-local-variable 'transient-mark-mode) t)
;;   (define-key (current-local-map) (kbd "M-RET") 'term-char-mode)
;;   ;;(define-key (current-local-map) (kbd "M-SPC") 'er/expand-region)
;;   (define-key (current-local-map) (kbd "M-C-SPC") 'er/contract-region)
;;   (c-k-clear-for-term-mode)
;;   ;;
;;   (define-key term-mode-map (kbd "C-j") nil)
;;   (define-key term-mode-map (kbd "C-;") nil)
;;   )
;; (ad-activate 'term-line-mode)

;; disable cua and transient mark modes in term-char-mode
;; (defadvice term-char-mode (after term-char-mode-fixes ())
;;   (set (make-local-variable 'cua-mode) nil)
;;   (set (make-local-variable 'transient-mark-mode) nil)
;;   (define-key (current-local-map) (kbd "M-RET") 'term-line-mode)
;;   (define-key (current-local-map) (kbd "M-r") 'term-send-C-r)
;;   (c-k-clear-for-term-mode)
;;   ;; (define-key (current-local-map) (kbd "C-h") '(lambda () (interactive) (term-send-raw-string "\M-b")))
;;   ;; (define-key (current-local-map) (kbd "C-d") '(lambda () (interactive) (term-send-raw-string "\C-d")))
;;   (define-key (current-local-map) (kbd "M-&") '(lambda () (interactive) (term-send-raw-string "\C-b")))
;;   (define-key (current-local-map) (kbd "M-^") '(lambda () (interactive) (term-send-raw-string "\C-f")))
;;   )
;; (ad-activate 'term-char-mode)

;; dired stuff
(defun jg-dired-updir ()
  (interactive)
  (find-alternate-file ".."))
(define-key dired-mode-map "B" 'jg-dired-updir) ; "updir", but let's me reuse the directory
(define-key dired-mode-map (kbd "C-,") 'jg-dired-updir)
(define-key dired-mode-map "f" 'dired-isearch-filenames)
(define-key dired-mode-map "q" 'kill-this-buffer)
(define-key dired-mode-map (kbd "C-g") 'kill-this-buffer)
(define-key dired-mode-map (kbd "A-x") 'smex)
(define-key dired-mode-map (kbd "M-0") 'other-window)
(define-key dired-mode-map (kbd "M-1") 'delete-other-windows)

(define-key dired-mode-map (kbd "SPC") 'dired-subtree-toggle)
(define-key dired-mode-map (kbd "TAB") 'dired-subtree-toggle)


(define-prefix-command 'dired-jump-map)
(define-key dired-mode-map (kbd "j") 'dired-jump-map)

(define-key dired-jump-map (kbd "n") 'dired-subtree-next-sibling)
(define-key dired-jump-map (kbd "p") 'dired-subtree-previous-sibling)
(define-key dired-jump-map (kbd "e") 'dired-subtree-end)
(define-key dired-jump-map (kbd "s") 'dired-subtree-beginning)

(define-key dired-mode-map (kbd "C-o") 'jg-quicknav)
;; (define-key dired-mode-map (kbd "C-o") 'dired-display-file)



(add-hook 'dired-mode-hook 'disable-jg-code-mode)
(add-hook 'dired-mode-hook '(lambda () (jg-navigation-mode -1)))
;;(add-hook 'dired-mode-hook '(lambda () (interactive) (describe-function 'dired-mode)))

;; shell-mode stuff
(define-key shell-mode-map (kbd "M-k") 'clear-shell)
;;(define-key shell-mode-map (kbd "C-o") (kbd "C-x C-f RET"))
(define-key shell-mode-map (kbd "C-S-f") 'ag)
(define-key shell-mode-map (kbd "M-S-v") 'yank-pop)
(define-key shell-mode-map (kbd "C-M-v") 'paste-unshift)
(define-key shell-mode-map (kbd "M-z") 'undo-tree-undo)
(define-key shell-mode-map (kbd "M-Z") 'undo-tree-redo)
(define-key shell-mode-map (kbd "C-M-z") 'undo-tree-visualize)
(define-key shell-mode-map (kbd "M-.") 'comint-restore-input)
(define-key shell-mode-map (kbd "TAB") nil)

(define-key comint-mode-map (kbd "C-r") 'comint-history-isearch-backward)

;; ssh-mode
(require 'ssh)
(define-key ssh-mode-map (kbd "TAB") nil)

;; scratch buffer
;(define-key lisp-interaction-mode (kbd "C-c C-j") 'eval-print-last-sexp)




(define-key eww-mode-map (kbd "B") 'eww-back-url)
(define-key eww-mode-map (kbd "<") 'eww-back-url)
(define-key eww-mode-map (kbd ">") 'eww-forward-url)
(define-key eww-mode-map (kbd "C-c 0") 'eww-copy-page-url)
(define-key eww-mode-map (kbd "f") 'eww-lnum-follow)
(define-key eww-mode-map (kbd "F") 'eww-lnum-universal)

(add-hook 'eww-mode-hook
          'disable-jg-code-mode)



(add-hook 'mu4e-main-mode-hook 'disable-jg-code-mode)
(add-hook 'mu4e-headers-mode-hook 'disable-jg-code-mode)
(add-hook 'mu4e-view-mode-hook 'disable-jg-code-mode)


(define-key c++-mode-map (kbd "TAB") nil)

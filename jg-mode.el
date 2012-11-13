(defvar jg-code-mode-map (make-sparse-keymap) "jg-code-mode-map.")
(defvar jg-navigation-mode-map (make-sparse-keymap) "jg-navigation-mode-map.")


;;*************************************************
;;               JG Navigation Mode
;;*************************************************

(define-key jg-navigation-mode-map (kbd "C-;") 'forward-char)
(define-key jg-navigation-mode-map (kbd "C-j") 'backward-char)
(define-key jg-navigation-mode-map (kbd "C-'") 'forward-word)
(define-key jg-navigation-mode-map (kbd "C-h") 'backward-word)
(define-key jg-navigation-mode-map (kbd "C-t") 'exchange-point-and-mark)

(define-key jg-navigation-mode-map (kbd "C-t") 'forward-to-char)
(define-key jg-navigation-mode-map (kbd "C-b") 'backward-to-char)


;; scrolling and line movement
(define-key jg-navigation-mode-map (kbd "M-C-n") 'View-scroll-half-page-forward)
(define-key jg-navigation-mode-map (kbd "M-C-p") 'View-scroll-half-page-backward)
(define-key jg-navigation-mode-map (kbd "C-M-.") 'end-of-buffer)
(define-key jg-navigation-mode-map (kbd "C-M-,") 'beginning-of-buffer)

;; emacs is weird. have to have this to be able to bind C-i without also binding <tab>
(if window-system
    (progn
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

;;buffers
(define-key jg-navigation-mode-map (kbd "C-w") 'kill-this-buffer)
(define-key jg-navigation-mode-map (kbd "C-q") 'keyboard-quit)
(define-key jg-navigation-mode-map (kbd "M-g") 'repeat)



;;buffer switching. thanks to jg-elscreen-buffer-list, only switches buffer within the current tab.
(define-key jg-navigation-mode-map (kbd "<C-tab>") 'wcy-switch-buffer-forward)
(define-key jg-navigation-mode-map (kbd "<C-S-tab>") 'wcy-switch-buffer-backward)


;; forward/back buttons like in a browser. go to the last place I was.
(define-key jg-navigation-mode-map (kbd "C-<") 'back-button-global-backward)
(define-key jg-navigation-mode-map (kbd "C->") 'back-button-global-forward)
;; same, but only within the current buffer
(define-key jg-navigation-mode-map (kbd "C-,") 'back-button-local-backward)
(define-key jg-navigation-mode-map (kbd "C-.") 'back-button-local-forward)

;; isearch
(define-key jg-navigation-mode-map (kbd "C-f") 'isearch-forward)
(define-key jg-navigation-mode-map (kbd "C-r") 'isearch-backward)


;; i guess exiting completely is navigation
(define-key jg-navigation-mode-map (kbd "M-q") 'save-buffers-kill-terminal)

;; smex is M-x but like ido-mode. sweet!
(define-key jg-navigation-mode-map (kbd "M-x") 'smex)

;;*************************************************
;;            End JG Navigation Mode
;;*************************************************







;;***************************
;; Text Surgery
;;***************************


(define-key jg-code-mode-map (kbd "M-P") 'duplicate-current-line-up)
(define-key jg-code-mode-map (kbd "M-N") 'duplicate-current-line-or-region)
(define-key jg-code-mode-map (kbd "M-RET") 'open-line-below)
(define-key jg-code-mode-map (kbd "<M-C-return>") 'open-line-above)

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
(define-key jg-code-mode-map (kbd "C-S-o") 'fuzzy-find-in-project)
(define-key jg-code-mode-map (kbd "C-x p") 'ido-jg-set-project-root)
(define-key jg-code-mode-map (kbd "C-x o") 'helm-occur)
(define-key jg-code-mode-map (kbd "C-x n") 'elscreen-screen-nickname)


;;*******************
;; Search/Replace/Etc
;;*******************
(define-key jg-code-mode-map (kbd "C-S-f") 'rgrep)
(define-key jg-code-mode-map (kbd "C-M-f") 'grep-buffers)
(define-key jg-code-mode-map (kbd "C-S-r") 'query-replace)


;;***********************
;; Macros
;;***********************
(define-key jg-code-mode-map (kbd "C-5") 'view-percent)
(define-key jg-code-mode-map (kbd "C-%") 'view-percent-equal)
(define-key jg-code-mode-map (kbd "C-{") 'css-curlies)
(define-key jg-code-mode-map (kbd "C-#") 'comment-or-uncomment-region)
(define-key jg-code-mode-map (kbd "TAB") 'hippie-expand)



;; select by semantic units. super helpful!
(define-key jg-code-mode-map (kbd "M-SPC") 'er/expand-region)
(define-key jg-code-mode-map (kbd "M-C-SPC") 'er/contract-region)

;; not useful any more. was a trick to help convert hobo dryml templates
;;(define-key jg-code-mode-map (kbd "C-M-c") 'layout-content)

;;(define-key jg-code-mode-map (kbd "A-w") '(lambda () (interactive) (switch-to-buffer "*scratch*") (prelude-kill-other-buffers)) )



(define-key jg-code-mode-map (kbd "C-x r") 'rename-this-buffer-and-file)


;;**************
;; yasnippet
;;**************
;; take away tab because I want that to be tab-complete
(define-key yas-minor-mode-map (kbd "C-c ; u") 'yas-expand) ;;keybinding I'll never want
(setq yas-trigger-key nil)
;; trigger yasnippet with helm interface
(define-key jg-code-mode-map (kbd "C-y") 'helm-c-yas-complete)





;; other things i rebind
(define-key jg-code-mode-map (kbd "M-C-x") 'eval-expression)


;; set up a new help key prefix since I use C-h for movement
(if window-system
    (progn
      (setq help-char ?\M-?)
      (global-set-key "\M-?" 'help-for-help)
      ))
(define-key help-mode-map (kbd "B") 'help-go-back)
(define-key help-mode-map (kbd "F") 'help-go-forward)


(define-minor-mode jg-code-mode "JG Code Editing Mode Keys" t " [JGc]" 'jg-code-mode-map)
(define-minor-mode jg-navigation-mode "JG Navigation Mode Keys" t " [JGn]" 'jg-navigation-mode-map)
(jg-code-mode 1)
(jg-navigation-mode 1)




;; ==========================================================
;; Other modes and hooks I have to "fix" to work with jg-mode
;; ==========================================================



;; don't break my C-j
(require 'paredit)
(define-key ruby-mode-map (kbd "C-j") nil) ;; unbind the ruby C-j because it conflicts
(define-key paredit-mode-map (kbd "C-j") nil)



;; fix the ruby-tools keymap
(require 'ruby-tools)
(define-key ruby-tools-mode-map (kbd "C-'") nil)
(define-key ruby-tools-mode-map (kbd "C-\"") nil)
(define-key ruby-tools-mode-map (kbd "C-:") nil)
(define-key ruby-tools-mode-map (kbd "C-;") nil)
;;(define-key ruby-tools-mode-map (kbd "#") nil)

(define-key ruby-tools-mode-map (kbd "M-'") 'ruby-tools-to-single-quote-string)
(define-key ruby-tools-mode-map (kbd "M-\"") 'ruby-tools-to-double-quote-string)
(define-key ruby-tools-mode-map (kbd "M-:") 'ruby-tools-to-symbol)


;; "Add my keybindings for ido."
;; for some reason they don't take effect unless I bind them every time, in this hook.
(add-hook 'ido-setup-hook 'ido-jg-keys)
(defun ido-jg-keys ()
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  )



(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-q") 'isearch-abort)
(define-key isearch-mode-map (kbd "TAB") 'isearch-complete)
(define-key minibuffer-local-isearch-map (kbd "TAB") 'isearch-complete-edit)


;; turn jg-code-mode off for buffers where I'm not editing the text ... it messes stuff up.
(defun disable-jg-code-mode ()
  (jg-code-mode 0)
  (yas-minor-mode -1)
)

(add-hook 'minibuffer-setup-hook 'disable-jg-code-mode)
(add-hook 'help-mode-hook 'disable-jg-code-mode)
(add-hook 'compilation-mode-hook 'disable-jg-code-mode)
(add-hook 'grep-mode-hook 'disable-jg-code-mode)
(add-hook 'erc-mode-hook 'disable-jg-code-mode)
(add-hook 'eshell-mode-hook 'disable-jg-code-mode)
(add-hook 'shell-mode-hook 'disable-jg-code-mode)


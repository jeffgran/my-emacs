(defvar jg-mode-map (make-keymap) "jg-mode-map.")
;;***********
;; Navigation
;;***********
(define-key jg-mode-map (kbd "C-;") 'forward-char)
(define-key jg-mode-map (kbd "C-j") 'backward-char)
  (define-key ruby-mode-map (kbd "C-j") nil) ;; unbind the ruby C-j because it conflicts
(define-key jg-mode-map (kbd "C-'") 'forward-word)
(define-key jg-mode-map (kbd "C-h") 'backward-word)
(define-key jg-mode-map (kbd "C-t") 'exchange-point-and-mark)

(define-key jg-mode-map (kbd "C-g") 'forward-to-char)
(define-key jg-mode-map (kbd "C-b") 'backward-to-char)


; don't break my C-j
(require 'paredit)
(define-key paredit-mode-map (kbd "C-j") nil)


;; emacs is weird. have to have this to be able to bind C-i without also binding <tab>
(keyboard-translate ?\C-i ?\H-i)
(define-key jg-mode-map (kbd "H-i") 'back-to-indentation)
(define-key jg-mode-map (kbd "C-\\") 'indent-for-tab-command)
(define-key jg-mode-map (kbd "M-C-i") 'recenter-top-bottom)
(define-key jg-mode-map (kbd "M-i") 'indent-for-tab-command)

;; scrolling and line movement
(define-key jg-mode-map (kbd "A-n") 'scroll-up)
(define-key jg-mode-map (kbd "A-p") 'scroll-down)
(define-key jg-mode-map (kbd "C-l") 'goto-line)


(define-key jg-mode-map (kbd "M-P") 'duplicate-current-line-up)
(define-key jg-mode-map (kbd "M-N") 'duplicate-current-line-down)
(define-key jg-mode-map (kbd "M-RET") 'open-line-below)
(define-key jg-mode-map (kbd "<M-C-return>") 'open-line-above)


;; new and improved! move line OR region up and down!
(define-key jg-mode-map (kbd "M-p") 'move-text-up)
(define-key jg-mode-map (kbd "M-n") 'move-text-down)



;screens (tabs)
(define-key jg-mode-map (kbd "M-t") 'elscreen-create)
(define-key jg-mode-map (kbd "M-w") 'elscreen-kill)
(define-key jg-mode-map (kbd "M-{") 'elscreen-previous)
(define-key jg-mode-map (kbd "M-}") 'elscreen-next)

;buffer switching. thanks to jg-elscreen-buffer-list, only switches buffer within the current tab.
(define-key jg-mode-map (kbd "<C-tab>") 'wcy-switch-buffer-forward)
(define-key jg-mode-map (kbd "<C-S-tab>") 'wcy-switch-buffer-backward)


;; forward/back buttons like in a browser. go to the last place I was.
(define-key jg-mode-map (kbd "C-<") 'back-button-global-backward)
(define-key jg-mode-map (kbd "C->") 'back-button-global-forward)
;; same, but only within the current buffer
(define-key jg-mode-map (kbd "C-,") 'back-button-local-backward)
(define-key jg-mode-map (kbd "C-.") 'back-button-local-forward)


;;***********************
;; Cut/Copy/Open/Save/Etc
;;***********************
(define-key jg-mode-map (kbd "C-v") 'clipboard-yank)
(define-key jg-mode-map (kbd "C-S-v") 'cua-paste-pop)
(define-key jg-mode-map (kbd "C-M-v") 'paste-unshift)

(define-key jg-mode-map (kbd "C-z") 'undo)
(define-key jg-mode-map (kbd "C-S-Z") 'redo)

(define-key jg-mode-map (kbd "C-s") 'save-buffer)
(define-key jg-mode-map (kbd "C-S-o") 'fuzzy-find-in-project)
(define-key jg-mode-map (kbd "C-x p") 'fuzzy-find-project-root)


;;*******************
;; Search/Replace/Etc
;;*******************
(define-key jg-mode-map (kbd "C-f") 'isearch-forward)
(define-key jg-mode-map (kbd "C-r") 'isearch-backward)
(define-key jg-mode-map (kbd "C-S-f") 'rgrep)
(define-key jg-mode-map (kbd "C-M-f") 'grep-buffers)
(define-key jg-mode-map (kbd "C-S-r") 'query-replace)


;;***********************
;; Macros
;;***********************
(define-key jg-mode-map (kbd "C-5") 'view-percent)
(define-key jg-mode-map (kbd "C-%") 'view-percent-equal)
(define-key jg-mode-map (kbd "C-{") 'css-curlies)
(define-key jg-mode-map (kbd "C-#") 'comment-or-uncomment-region)
(define-key jg-mode-map (kbd "TAB") 'hippie-expand)



;; select by semantic units. super helpful!
(define-key jg-mode-map (kbd "M-SPC") 'er/expand-region)
(define-key jg-mode-map (kbd "M-C-SPC") 'er/contract-region)

;; not useful any more. was a trick to help convert hobo dryml templates
(define-key jg-mode-map (kbd "C-M-c") 'layout-content)

(define-key jg-mode-map (kbd "C-w") 'kill-this-buffer)
(define-key jg-mode-map (kbd "A-w") '(lambda () (interactive) (switch-to-buffer "*scratch*") (prelude-kill-other-buffers)) )
(define-key jg-mode-map (kbd "C-q") 'keyboard-quit)
(define-key jg-mode-map (kbd "M-g") 'repeat)

;; "windows" (in emacs parlance)
(define-key jg-mode-map (kbd "M-o") 'other-window)
(define-key jg-mode-map (kbd "M-1") 'delete-other-windows)
(define-key jg-mode-map (kbd "M-2") 'split-window-below)
(define-key jg-mode-map (kbd "M-3") 'split-window-right)


(define-key jg-mode-map (kbd "C-x r") 'rename-this-buffer-and-file)


;;**************
;; yasnippet
;;**************
;; take away tab because I want that to be tab-complete
(define-key yas-minor-mode-map (kbd "C-c ; u") 'yas-expand) ;;keybinding I'll never want
(setq yas-trigger-key nil)
;; trigger yasnippet
(define-key jg-mode-map (kbd "C-y") 'yas-insert-snippet)
(add-hook 'help-mode-hook (lambda () (yas-minor-mode -1)))





;; other things i rebind
(define-key jg-mode-map (kbd "M-C-x") 'eval-expression)
(define-key jg-mode-map (kbd "M-q") 'save-buffers-kill-terminal)


;; set up a new help key prefix since I use C-h for movement
(setq help-char ?\M-?)
(global-set-key "\M-?" 'help-for-help)
(define-key help-mode-map (kbd "B") 'help-go-back)
(define-key help-mode-map (kbd "F") 'help-go-forward)


(define-minor-mode jg-mode "JG Mode Keys" t " [JG]" 'jg-mode-map)
(jg-mode 1)




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
;;(define-key jg-mode-map (kbd "M-;") 'ruby-tools-clear-string)
;;(define-key jg-mode-map (kbd "#") 'ruby-tools-interpolate)


(add-hook 'ido-setup-hook 'ido-jg-keys)
(defun ido-jg-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  )



(add-hook 'isearch-mode-hook
 (lambda ()
  (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "C-q") 'isearch-abort)
  (define-key isearch-mode-map (kbd "TAB") 'isearch-complete)
  (define-key minibuffer-local-isearch-map (kbd "TAB") 'isearch-complete-edit)
 )
)

;; turn jg-mode off for the minibuffer... it messes stuff up.
;; note to self: I think what I would have to do to fix this properly
;; is make two separate jg-maps, one for just the movement and one for everything else
;; here, I would just have to turn off the "everything else" one, and leave the 
;; movement keys intact in the minibuffer. someday.
(add-hook 'minibuffer-setup-hook
 (lambda ()
  (jg-mode 0)
 )
)

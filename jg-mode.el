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


;; (define-key jg-mode-map (kbd "C-:") 'forward-char-select)
;; (define-key jg-mode-map (kbd "C-S-j") 'backward-char-select)
;; (define-key jg-mode-map (kbd "C-S-h") 'backward-word-select)

;(define-key jg-mode-map (kbd "M-j") 'backward-char-unselect)
;(define-key jg-mode-map (kbd "M-;") 'forward-char-unselect)
;(define-key jg-mode-map (kbd "M-h") 'backward-word-unselect)
;(define-key jg-mode-map (kbd "M-'") 'forward-word-unselect)

(define-key jg-mode-map (kbd "M-i") 'back-to-indentation)

(define-key jg-mode-map (kbd "C-S-n") 'scroll-up)
(define-key jg-mode-map (kbd "C-S-p") 'scroll-down)
(define-key jg-mode-map (kbd "C-l") 'goto-line)

(define-key jg-mode-map (kbd "M-P") 'duplicate-current-line-up)
(define-key jg-mode-map (kbd "M-N") 'duplicate-current-line-down)
(define-key jg-mode-map (kbd "M-p") 'move-line-up)
(define-key jg-mode-map (kbd "M-n") 'move-line-down)
(define-key jg-mode-map (kbd "M-RET") 'open-line-below)
(define-key jg-mode-map (kbd "<M-C-return>") 'open-line-above)
;(define-key jg-mode-map (kbd "RET") 'newline)


;screens (tabs)
(define-key jg-mode-map (kbd "s-t") 'elscreen-create)
(define-key jg-mode-map (kbd "s-w") 'elscreen-kill)
(define-key jg-mode-map (kbd "s-}") 'elscreen-previous)
(define-key jg-mode-map (kbd "s-{") 'elscreen-next)


(define-key jg-mode-map (kbd "<C-tab>") 'wcy-switch-buffer-forward)
(define-key jg-mode-map (kbd "<C-S-tab>") 'wcy-switch-buffer-backward)

(define-key jg-mode-map (kbd "C-.") 'marker-visit-next)
(define-key jg-mode-map (kbd "C-,") 'marker-visit-prev)

;(define-key jg-mode-map (kbd "C-S-b") 'bc-set)
(define-key jg-mode-map (kbd "C->") 'bc-next)
(define-key jg-mode-map (kbd "C-<") 'bc-previous)

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
;(define-key jg-mode-map (kbd "M-'") 'select-or-wrap-string)
(define-key jg-mode-map (kbd "M-SPC") 'extend-selection)
(define-key jg-mode-map (kbd "C-\"") 'string-wrap)

(define-key jg-mode-map (kbd "C-M-c") 'layout-content)

(define-key jg-mode-map (kbd "C-w") 'kill-this-buffer)
(define-key jg-mode-map (kbd "M-w") 'kill-all-buffers)
(define-key jg-mode-map (kbd "C-q") 'keyboard-quit)
;(define-key jg-mode-map (kbd "M-g") 'repeat)
(define-key jg-mode-map (kbd "M-o") 'other-window)
(define-key jg-mode-map (kbd "M-1") 'delete-other-windows)
(define-key jg-mode-map (kbd "C-x p") 'project-switch)
(define-key jg-mode-map (kbd "C-x r") 'rename-this-buffer-and-file)


(define-minor-mode jg-mode "JG Mode Keys" t " [JG]" 'jg-mode-map)
(jg-mode 1)



(add-hook 'isearch-mode-hook
 (lambda ()
  (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "C-q") 'isearch-abort)
  (define-key isearch-mode-map (kbd "TAB") 'isearch-complete)
  (define-key minibuffer-local-isearch-map (kbd "TAB") 'isearch-complete-edit)
 )
)

;; turn jg-mode off for the minibuffer
(add-hook 'minibuffer-setup-hook
 (lambda ()
  (jg-mode 0)
 )
)

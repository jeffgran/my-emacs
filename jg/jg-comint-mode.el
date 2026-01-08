(defvar-keymap jg-comint-mode-map
  "C-M-x" 'eval-expression
  "M-k" 'clear-shell
  "C-S-f" 'jg-dispatch-helm-ag
  "M-S-v" 'yank-pop
  "C-M-v" 'paste-unshift
  "M-z" 'undo-tree-undo
  "M-Z" 'undo-tree-redo
  "C-M-z" 'undo-tree-visualize
  "M-." 'comint-restore-input
  "C-r" 'comint-history-isearch-backward
  "M-g" 'magit-dispatch
  "TAB" nil)

(define-minor-mode jg-comint-mode
  "JG Comint Mode Keys"
  :init-value nil ;; default off
  :lighter " [JGs]"
  :keymap jg-comint-mode-map)

(add-hook 'comint-mode-hook 'jg-comint-mode)

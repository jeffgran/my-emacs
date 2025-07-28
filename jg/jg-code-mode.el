;;***************************
;; Code Editing Mode
;;***************************
(defvar-keymap jg-code-mode-map
  "M-P" 'duplicate-current-line-or-region-up
  "M-N" 'duplicate-current-line-or-region

  "M-RET" 'open-line-below

  "M-k" 'kill-region
  "M-x" 'kill-region
  "C-k" 'kill-line
  "C-x M-d" 'zap-to-char

  "C-c H-m" 'jg-dispatch-mc

  "C-c o" 'occur

  "C-c y" 'yas-describe-tables

  ;; "H-l" nil
  ;; "H-u" nil

  "C-H-n" 'mc/mark-next-lines
  "C-H-a" 'mc/mark-all-like-this-dwim

  ;; new and improved! move line OR region up and down!
  "M-p" 'drag-stuff-up
  "M-n" 'drag-stuff-down
  "C-M-\\" 'indent-region-or-buffer

  ;; "C-\\" 'indent-for-tab-command

  "M-z" 'undo-tree-undo
  "M-Z" 'undo-tree-redo
  "C-M-z" 'undo-tree-visualize

  "C-x s" 'shell
  "C-x C-s" 'switch-to-shell
  "M-s" 'save-buffer
  "C-M-j" 'join-line

  ;;*******************
  ;; Search/Replace/Etc
  ;;*******************
  "C-S-f" 'jg-dispatch-helm-ag
  "C-M-f" 'grep-buffers
  "C-S-r" 'query-replace

  "C-c C-t" 'build-ctags
  "C-8" 'find-tag

  ;;***********************
  ;; Macros
  ;;***********************
  "C-5" 'view-percent
  "C-%" 'view-percent-equal
  "C-{" 'css-curlies
  "C-#" 'comment-or-uncomment-region
  ;; "TAB" 'hippie-expand
  "H-q" 'fill-paragraph

  "C-x r" 'rename-this-buffer-and-file

  "C-M-x" 'eval-expression

  "M-g" 'magit-dispatch)

(define-minor-mode jg-code-mode
  "JG Code Editing Mode Keys"
  :init-value t
  :lighter " [JGc]"
  :keymap jg-code-mode-map)

(jg-code-mode 1)

;; turn jg-code-mode off for buffers where I'm not editing the text ... it messes stuff up.
(defun disable-jg-code-mode ()
  (jg-code-mode 0))

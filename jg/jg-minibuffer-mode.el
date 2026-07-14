(defvar-keymap jg-minibuffer-mode-map
  "C-l" 'forward-char
  "C-j" 'backward-char
  "C-;" '(lambda () (interactive) (forward-word-strictly))
  "C-h" '(lambda () (interactive) (backward-word-strictly))
  "C-f" #'(lambda ()
            (interactive)
            (if (region-active-p)
                (deactivate-mark)
              (call-interactively 'set-mark-command)))
  "M-j" 'jg-dispatch
  "C-a" 'back-to-indentation-or-beginning
  "M-x" 'kill-region
  "M-c" 'kill-ring-save
  "C-g" '(lambda () (interactive) (if mark-active (deactivate-mark) (abort-recursive-edit)))
  "M-v" 'clipboard-yank
  "M-V" 'yank-pop
  "C-M-v" 'paste-unshift
  )

(define-minor-mode jg-minibuffer-mode
  "JG Minibuffer Keys"
  :init-value t
  :lighter " [JGm]"
  :keymap jg-minibuffer-mode-map)

(defun disable-jg-minibuffer-mode ()
  (jg-minibuffer-mode 0))

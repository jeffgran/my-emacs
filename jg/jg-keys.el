(global-set-key [remap move-beginning-of-line] 'move-beginning-of-line)

(setq ns-alternate-modifier 'hyper)

(global-set-key (kbd "M-=") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "H-a") #'(lambda () (interactive) (insert-char ?á)))
(global-set-key (kbd "H-e") #'(lambda () (interactive) (insert-char ?é)))
(global-set-key (kbd "H-i") #'(lambda () (interactive) (insert-char ?í)))
(global-set-key (kbd "H-o") #'(lambda () (interactive) (insert-char ?ó)))
(global-set-key (kbd "H-u") #'(lambda () (interactive) (insert-char ?ú)))
(global-set-key (kbd "H-n") #'(lambda () (interactive) (insert-char ?ñ)))


(defun switch-to-shell ()
  (interactive)
  (let ((buf (seq-find '(lambda (b)
                          (with-current-buffer b
                            (and (eq major-mode 'shell-mode)
				                         (not (null (get-buffer-process
				                                     (current-buffer)))))))
                       (persp-buffer-list-filter (buffer-list)))))
    (if (bufferp buf)
        (switch-to-buffer buf)
      (shell))))


(define-key help-mode-map (kbd "B") 'help-go-back)
(define-key help-mode-map (kbd "M-,") 'help-go-back)
(define-key help-mode-map (kbd "F") 'help-go-forward)


(define-key isearch-mode-map (kbd "M-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-r") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-w") 'isearch-yank-symbol-string)
(define-key isearch-mode-map (kbd "M-i") 'isearch-yank-word-or-char)
(define-key isearch-mode-map (kbd "H-z") 'isearch-toggle-regexp)
(define-key minibuffer-local-isearch-map (kbd "TAB") 'isearch-complete-edit)


;;*************************************************
;;                 JG Comint Mode
;;*************************************************
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
  :init-value nil
  :lighter " [JGs]"
  :keymap jg-comint-mode-map)


(add-hook 'minibuffer-setup-hook 'disable-jg-code-mode)
(add-hook 'help-mode-hook 'disable-jg-code-mode)
(add-hook 'compilation-mode-hook 'disable-jg-code-mode)
(add-hook 'grep-mode-hook 'disable-jg-code-mode)
(add-hook 'erc-mode-hook 'disable-jg-code-mode)
(add-hook 'comint-mode-hook 'disable-jg-code-mode)
(add-hook 'comint-mode-hook 'jg-comint-mode)
(add-hook 'custom-mode-hook 'disable-jg-code-mode)


(defun c-k-clear-for-term-mode ()
  (define-key (current-local-map) (kbd "M-k") #'(lambda () (interactive) (term-send-raw-string "clear\r") )))

(defun jg-term-mode-after-hook ()
  "Custom setup for term-mode."
  (disable-jg-code-mode)
  (c-k-clear-for-term-mode))
(advice-add 'term-mode :after #'jg-term-mode-after-hook)

;; dired stuff
(defun jg-dired-updir ()
  (interactive)
  (find-alternate-file ".."))
(define-key dired-mode-map "B" 'jg-dired-updir) ; "updir", but let's me reuse the directory
(define-key dired-mode-map (kbd "C-,") 'jg-dired-updir)
(define-key dired-mode-map "f" 'dired-isearch-filenames)
(define-key dired-mode-map "q" #'(lambda () (interactive) (kill-buffer (current-buffer))))
(define-key dired-mode-map (kbd "C-g") #'(lambda () (interactive) (kill-buffer (current-buffer))))
(define-key dired-mode-map (kbd "H-x") 'amx)
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

(define-key dired-mode-map (kbd "C-o") 'helm-find-files)
;; (define-key dired-mode-map (kbd "C-o") 'dired-display-file)



(add-hook 'dired-mode-hook 'disable-jg-code-mode)
(add-hook 'dired-mode-hook #'(lambda () (jg-navigation-mode -1)))



;; original version: May 14, 2015. Doesn't really work but I can kinda cycle through buffers.
;;
;; May 17 - basics are working well. buffer ordering is right, switching forward and backward.
;;
;;

(defvar jg-switch-buffer-window nil)
(defvar jg-switch-buffer-index 0)
(defvar jg-switch-buffer-current nil)
(defvar jg-switch-buffer-buffer-list nil)

;; TODO fix
(defvar jg-buffer-switcher-buffer nil)

(defun jg-switch-buffer ()
  (interactive)
  
  (setq jg-buffer-switcher-buffer (get-buffer-create "*jg-switch-buffer*"))
  (setq jg-switch-buffer-buffer-list (buffer-list))

  (setq jg-switch-buffer-window (selected-window))
  (jg-switch-buffer-reset-index)
  (jg-switch-buffer-increase-index)
  (jg-switch-buffer-set-current)
  ;; (with-current-buffer jg-buffer-switcher
  ;;   (setq buffer-read-only t))

  (setq this-command 'jg-switch-buffer)
  
  (save-window-excursion
    (display-buffer jg-buffer-switcher-buffer)
    (with-current-buffer jg-buffer-switcher-buffer
      (erase-buffer)
      (mapcar
       (lambda (buf) (insert (concat (buffer-name buf) "\n")))
       jg-switch-buffer-buffer-list))
    (read-string "Switch Buffer:"))
  
  (when jg-switch-buffer-current
    (set-window-buffer (selected-window) jg-switch-buffer-current)
    (jg-switch-buffer-reorder-buffer-list (cl-remove-if '(lambda (buf) (eq buf jg-switch-buffer-current)) jg-switch-buffer-buffer-list)))
  
  (kill-buffer jg-buffer-switcher-buffer)
  (setq jg-buffer-switcher-buffer nil)
  (jg-switch-buffer-reset-index))

(defun jg-switch-buffer-reset-index ()
  (setq jg-switch-buffer-index (position (current-buffer) jg-switch-buffer-buffer-list)))

(defun jg-switch-buffer-increase-index ()
  (setq jg-switch-buffer-index (+ 1 jg-switch-buffer-index))
  (when (eq jg-switch-buffer-index (length jg-switch-buffer-buffer-list))
    (setq jg-switch-buffer-index 0))
  ;(message "%s" jg-switch-buffer-index)
  )

(defun jg-switch-buffer-decrease-index ()
  (setq jg-switch-buffer-index (- jg-switch-buffer-index 1))
  (when (eq jg-switch-buffer-index -1)
    (setq jg-switch-buffer-index (- (length jg-switch-buffer-buffer-list) 1)))
  ;(message "%s" jg-switch-buffer-index)
  )

(defvar jg-switch-buffer-mode-map (make-sparse-keymap))

(define-key jg-switch-buffer-mode-map (kbd "C-n") 'jg-switch-buffer-next)
(define-key jg-switch-buffer-mode-map (kbd "<C-tab>") 'jg-switch-buffer-next)
(define-key jg-switch-buffer-mode-map (kbd "C-v") 'jg-switch-buffer-next)

(define-key jg-switch-buffer-mode-map (kbd "C-p") 'jg-switch-buffer-prev)
(define-key jg-switch-buffer-mode-map (kbd "<C-S-tab>") 'jg-switch-buffer-prev)
(define-key jg-switch-buffer-mode-map (kbd "C-S-V") 'jg-switch-buffer-prev)

(define-key jg-switch-buffer-mode-map (kbd "RET") 'jg-switch-buffer-confirm)
(define-key jg-switch-buffer-mode-map (kbd "C-g") 'jg-switch-buffer-quit)


(define-key jg-navigation-mode-map (kbd "<C-tab>") 'jg-switch-buffer)
(define-key jg-navigation-mode-map (kbd "C-v") 'jg-switch-buffer)

(define-minor-mode jg-switch-buffer-mode
  "foo"
  :lighter " jgsb"
  :keymap jg-switch-buffer-mode-map
  :group 'jg-switch-buffer)


(add-hook 'minibuffer-setup-hook 'jg-switch-buffer-minibuffer-setup)

(defun jg-switch-buffer-minibuffer-setup ()
  (when (eq this-command 'jg-switch-buffer)
    (jg-switch-buffer-mode t)

    (setq overriding-local-map jg-switch-buffer-mode-map)
    ))



(defun jg-switch-buffer-next ()
  (interactive)
  (jg-switch-buffer-increase-index)
  (jg-switch-buffer-set-current))

(defun jg-switch-buffer-prev ()
  (interactive)
  (jg-switch-buffer-decrease-index)
  (jg-switch-buffer-set-current))


(defun jg-switch-buffer-set-current ()
  (setq jg-switch-buffer-current (nth-value jg-switch-buffer-index jg-switch-buffer-buffer-list))
  (set-window-buffer jg-switch-buffer-window jg-switch-buffer-current))

(defun jg-switch-buffer-confirm ()
  (interactive)
  (exit-minibuffer))

(defun jg-switch-buffer-quit ()
  (interactive)
  (setq jg-switch-buffer-current nil)
  (exit-minibuffer))


(defun jg-switch-buffer-minibuffer-setup ()
  "For assigning to the `minibuffer-setup-hook' to set up for a `jg-quicknav' session"
  (when (eq this-command 'jg-switch-buffers)
    (jg-switch-buffer-mode t)
    (setq overriding-local-map jg-switch-buffer-mode-map)))


;; (defun jg-switch-buffer-fix-buffer-list-order
;;     (let )
;;     )

;; from https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-List.html
(defun jg-switch-buffer-reorder-buffer-list (new-list)
  (while new-list
    (bury-buffer (car new-list))
    (setq new-list (cdr new-list))))

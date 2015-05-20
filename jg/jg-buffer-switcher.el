;; original version: May 14, 2015. Doesn't really work but I can kinda cycle through buffers.
;;
;; May 17 - basics are working well. buffer ordering is right, switching forward and backward.
;;
;; May 19 - got highlighting current selection and kill-buffer working
;;          - need bounds check if killing last buffer
;; 

(defvar jg-switch-buffer-window nil)
(defvar jg-switch-buffer-index 0)
(defvar jg-switch-buffer-current nil)
(defvar jg-switch-buffer-buffer-list nil)

;; TODO fix name
(defvar jg-switch-buffer-switcher-buffer nil)

(defface jg-switch-buffer-selected-buffer-face
  '((t (:foreground "#FFEA77" :background "#004083")))
  "Highlights the selected buffer in the buffer list."
  :group 'jg-switch-buffer)



(defun jg-switch-buffer ()
  (interactive)
  
  (setq jg-switch-buffer-switcher-buffer (get-buffer-create "*jg-switch-buffer*"))
  (setq jg-switch-buffer-buffer-list (buffer-list))

  (setq jg-switch-buffer-window (selected-window))
  (jg-switch-buffer-reset-index)
  (jg-switch-buffer-increase-index)
  (jg-switch-buffer-set-current)
  
  (with-current-buffer jg-switch-buffer-switcher-buffer
    (setq buffer-read-only t)
    (setq cursor-type nil))

  (setq this-command 'jg-switch-buffer)
  
  (save-window-excursion
    (display-buffer jg-switch-buffer-switcher-buffer)
    (jg-switch-buffer-update-display)
    (read-string "Switch Buffer:"))
  
  (when jg-switch-buffer-current
    (set-window-buffer (selected-window) jg-switch-buffer-current)
    (jg-switch-buffer-reorder-buffer-list (cl-remove-if '(lambda (buf) (eq buf jg-switch-buffer-current)) jg-switch-buffer-buffer-list)))
  
  (kill-buffer jg-switch-buffer-switcher-buffer)
  (setq jg-switch-buffer-switcher-buffer nil)
  (jg-switch-buffer-reset-index))

(defun jg-switch-buffer-update-display ()
  (with-current-buffer jg-switch-buffer-switcher-buffer
    (let ((buffer-read-only nil))
      (erase-buffer)
      (mapcar (lambda (buf) (insert (concat (buffer-name buf) "\n"))) jg-switch-buffer-buffer-list)
      (remove-list-of-text-properties (point-min) (point-max) '(face))
      (goto-char (point-min))
      (forward-line jg-switch-buffer-index)
      (add-text-properties
       (line-beginning-position)
       (+ 1 (line-end-position))
       '(face jg-switch-buffer-selected-buffer-face)))))

(defun jg-switch-buffer-reset-index ()
  (setq jg-switch-buffer-index (position (current-buffer) jg-switch-buffer-buffer-list)))

(defun jg-switch-buffer-increase-index ()
  (setq jg-switch-buffer-index (+ 1 jg-switch-buffer-index))
  (when (eq jg-switch-buffer-index (length jg-switch-buffer-buffer-list))
    (setq jg-switch-buffer-index 0)))

(defun jg-switch-buffer-decrease-index ()
  (setq jg-switch-buffer-index (- jg-switch-buffer-index 1))
  (when (eq jg-switch-buffer-index -1)
    (setq jg-switch-buffer-index (- (length jg-switch-buffer-buffer-list) 1))))

(defvar jg-switch-buffer-mode-map (make-sparse-keymap))

(define-key jg-switch-buffer-mode-map (kbd "C-n") 'jg-switch-buffer-next)
(define-key jg-switch-buffer-mode-map (kbd "<C-tab>") 'jg-switch-buffer-next)
(define-key jg-switch-buffer-mode-map (kbd "C-v") 'jg-switch-buffer-next)

(define-key jg-switch-buffer-mode-map (kbd "C-p") 'jg-switch-buffer-prev)
(define-key jg-switch-buffer-mode-map (kbd "<C-S-tab>") 'jg-switch-buffer-prev)
(define-key jg-switch-buffer-mode-map (kbd "C-S-V") 'jg-switch-buffer-prev)

(define-key jg-switch-buffer-mode-map (kbd "RET") 'jg-switch-buffer-confirm)
(define-key jg-switch-buffer-mode-map (kbd "C-g") 'jg-switch-buffer-quit)

(define-key jg-switch-buffer-mode-map (kbd "C-d") 'jg-switch-buffer-kill)
(define-key jg-switch-buffer-mode-map (kbd "C-k") 'jg-switch-buffer-kill)


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
  (set-window-buffer jg-switch-buffer-window jg-switch-buffer-current)
  (jg-switch-buffer-update-display))

(defun jg-switch-buffer-confirm ()
  (interactive)
  (exit-minibuffer))

(defun jg-switch-buffer-quit ()
  (interactive)
  (setq jg-switch-buffer-current nil)
  (exit-minibuffer))

(defun jg-switch-buffer-kill ()
  (interactive)
  (let ((buffer-to-kill jg-switch-buffer-current)
        (temp-buffer (get-buffer-create "*jg-switch-buffer-temp*")))
    (set-window-buffer jg-switch-buffer-window temp-buffer)
    (if (kill-buffer buffer-to-kill)
        (progn
          (jg-switch-buffer-remove-from-buffer-list jg-switch-buffer-current)
          (jg-switch-buffer-set-current))
      (set-window-buffer jg-switch-buffer-window buffer-to-kill))
    (kill-buffer temp-buffer)))

(defun jg-switch-buffer-remove-from-buffer-list (a-buffer)
  (setq jg-switch-buffer-buffer-list
        (cl-remove-if
         '(lambda (buf) (eq buf a-buffer))
         jg-switch-buffer-buffer-list)))

(defun jg-switch-buffer-minibuffer-setup ()
  "For assigning to the `minibuffer-setup-hook' to set up for a `jg-quicknav' session"
  (when (eq this-command 'jg-switch-buffers)
    (jg-switch-buffer-mode t)
    (setq overriding-local-map jg-switch-buffer-mode-map)))



;; from https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-List.html
(defun jg-switch-buffer-reorder-buffer-list (new-list)
  (while new-list
    (bury-buffer (car new-list))
    (setq new-list (cdr new-list))))

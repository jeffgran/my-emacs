;;*************************************************
;;                  JG Dired Mode
;;*************************************************

(defvar-keymap jg-dired-mode-map
  ;; file/buffer navigation (shadows native dired/global keys below)
  "C-o" 'helm-find-files
  "C-S-o" 'helm-projectile-find-file
  "C-v" 'helm-buffers-list

  ;; isearch
  "M-f" 'isearch-forward
  "M-r" 'isearch-backward

  "M-g" 'magit-dispatch

  ;; native commands shadowed above, kept reachable on free keys
  "C-S-d" 'dired-display-file     ; native dired C-o slot
  "C-S-v" 'scroll-up-command)     ; native global C-v slot

(define-minor-mode jg-dired-mode
  "JG Dired Mode Keys"
  :init-value nil ;; default off; enabled from dired-mode-hook
  :lighter " [JGd]"
  :keymap jg-dired-mode-map)

(defun disable-jg-dired-mode ()
  (jg-dired-mode 0))

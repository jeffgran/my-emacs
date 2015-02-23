;;; elscreen-bg.el --- elscreen buffer group

;; Copyright (C) 2015 Jeff Gran

;; Author: Jeff Gran <jeff@jeffgran.com>
;;	Author: Ryan C. Thompson
;; URL: https://github.com/jeffgran/elscreen-bg
;; Created: 7 Nov 2012
;; Keywords: buffer
;; Version: 1.0.0
;; Package-Requires: ((elscreen "20140421.414"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Enabling this package gives each elscreen its own buffer group.
;; When a buffer is first displayed, it automatically gets added to the
;; group of the current elscreen.

;;; Code:

(require 'elscreen)

(defvar elscreen-bg-skip-commands ())

(defun elscreen-bg-add-buffer-to-list (arg)
  "Add the buffer to the current screen's elscreen-bg-list elscreen property"
  (let* ((screen-properties (elscreen-get-screen-property (elscreen-get-current-screen)))
         (elscreen-bg-list (elscreen-bg-get-alist 'elscreen-bg-list screen-properties))
         (the-new-buffer (if (stringp arg)
                             (get-buffer arg)
                           arg)))
    ;;(message (buffer-name the-new-buffer))

    ;; add the new buffer to the list
    (if (null elscreen-bg-list)
        (push the-new-buffer elscreen-bg-list)
      (add-to-list 'elscreen-bg-list the-new-buffer))
    ;; TODO also remove it from any other lists

    ;; set the elscreen property to the new changed one.
    (elscreen--set-alist 'screen-properties 'elscreen-bg-list elscreen-bg-list)
    (elscreen-set-screen-property (elscreen-get-current-screen) screen-properties)
    
    ;; "refresh" the screen/tabs display in the top line
    (elscreen-run-screen-update-hook)
  ))
                           
(defun elscreen-bg-get-buffer-list ()
  "Return the saved list of buffers which have been accessed in this screen"
  (let ((screen-properties (elscreen-get-screen-property (elscreen-get-current-screen))))
    (elscreen-bg-reorder-buffer-list 
     (remove-if-not 'buffer-live-p 
                    (or (elscreen-bg-get-alist 'elscreen-bg-list screen-properties)
                        (list (get-buffer "*scratch*")))))))


;;make ido-switch-buffer (& friends) use my buffer list
(add-hook 'ido-make-buffer-list-hook 
          (lambda () (setq ido-temp-list (mapcar 'buffer-name (elscreen-bg-get-buffer-list)) )))



(defun elscreen-bg-reorder-buffer-list (the-list)
  "Set buffers in NEW-LIST to be the most recently used, in order."
    (ad-deactivate 'buffer-list)
    (setq real-buffer-list (buffer-list))
    (ad-activate 'buffer-list)

    (elscreen-bg-filter-buffer-list the-list real-buffer-list)
    
    )

(defun elscreen-bg-filter-buffer-list (the-list real-buffer-list)
  "Filter the 'real list' (the result of the original (buffer-list) call, which is a c function and returns them in order of 
most recently used) only keeping the ones that are in the-list. The purpose is to effectively sort the-list in order of most 
recently used."
  (if (member this-command elscreen-bg-skip-commands)
      real-buffer-list
    (delq nil
          (mapcar (lambda (x) 
                    (and 
                     (member (buffer-name x) (mapcar 'buffer-name the-list))
                     x ))
                  real-buffer-list))))


;; these two are to add any newly shown buffer to the buffer list of the current screen
(defadvice display-buffer (around elscreen-bg-display-buffer-advice activate)
  (setq ret-val ad-do-it)
  (setq the-buffer ret-val)
  (setq the-buffer (cond
                    ((bufferp ret-val)
                     ret-val)
                    ((windowp ret-val)
                     (window-buffer ret-val))
                    (t
                     (throw "wat did this return?"))))
  ;;(message (prin1-to-string the-buffer))
  (elscreen-bg-add-buffer-to-list the-buffer)
  (setq ad-return-value ret-val))

(defadvice switch-to-buffer (around elscreen-bg-switch-to-buffer-advice activate)
  (setq ret-val ad-do-it)
  (setq the-buffer ret-val)
  (setq the-buffer (cond
                    ((bufferp ret-val)
                     ret-val)
                    ((windowp ret-val)
                     (window-buffer ret-val))
                    (t
                     (throw "wat did this return?"))))
  ;;(message (prin1-to-string the-buffer))
  (elscreen-bg-add-buffer-to-list the-buffer)
  (setq ad-return-value ret-val))




(defadvice buffer-list (around elscreen-bg-buffer-list activate)
  "make the built-in function (buffer-list) return MY buffer list instead"
  (setq ad-return-value (elscreen-bg-get-buffer-list))
)


(defadvice elscreen-kill (before elscreen-bg-kill-buffers activate)
  "when you kill a screen, kill all the buffers in its list."
  (mapcar '(lambda (b) (kill-buffer b)) (elscreen-bg-get-buffer-list))
  )

(defadvice switch-to-prev-buffer (around elscreen-bg-switch-to-prev-buffer activate) 
"this is for when you kill a buffer. it looks for a buffer to show next. we
want to make sure it only shows one from the list of buffers in the current
screen"
  ;; nth 1 means the 'next' one (the 'first' one is the current one we're closing)
  (let* ((last-buffer (nth 1 (elscreen-bg-get-buffer-list)))
         (the-buffer (or (and (not (eq last-buffer (window-buffer (selected-window))))
                              last-buffer)
                         (get-buffer "*scratch*"))))
    (set-window-buffer (selected-window) the-buffer)
    ))


(defadvice kill-buffer (around elscreen-bg-dont-kill-scratch activate)
  (unless (string= (buffer-name (current-buffer)) "*scratch*")
      ad-do-it)
)


;; (defadvice ibuffer (around elscreen-bg-turn-off-buffer-list-advice activate)
;; "turn off the buffer-list for this, so I have means to look at them all if I want to."
;;   (ad-deactivate 'buffer-list)
;;   ad-do-it
;;   (ad-activate 'buffer-list)
;; )


(defun elscreen-bg-get-current-property (name)
  (let((properties (elscreen-get-screen-property (elscreen-get-current-screen)))
       (elscreen-bg-get-alist name properties))))

(defun elscreen-bg-get-alist (key alist)
  (cdr (assoc key alist)))


(provide 'elscreen-bg)

;;; elscreen-bg.el ends here

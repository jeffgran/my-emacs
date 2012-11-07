(provide 'jg-elscreen-buffer-list)
(require 'elscreen)



(defun jg-elscreen-add-buffer-to-list (arg)
  "Add the buffer to the current screen's jg-buffer-list elscreen property"
  (let* ((screen-properties (elscreen-get-screen-property (elscreen-get-current-screen)))
         (jg-buffer-list (get-alist 'jg-buffer-list screen-properties))
         (the-new-buffer (if (stringp arg)
                             (get-buffer arg)
                           arg)
                         ))
    ;;(message (buffer-name the-new-buffer))

    ;; add the new buffer to the list
    (if (null jg-buffer-list)
        (push the-new-buffer jg-buffer-list)
      (add-to-list 'jg-buffer-list the-new-buffer))

    ;; set the elscreen property to the new changed one.
    (set-alist 'screen-properties 'jg-buffer-list jg-buffer-list)
    (elscreen-set-screen-property (elscreen-get-current-screen) screen-properties)
    
    ;; "refresh" the screen/tabs display in the top line
    (elscreen-run-screen-update-hook)
  ))
                           
(defun jg-elscreen-get-buffer-list ()
  "Return the saved list of buffers which have been accessed in this screen"
  (let ((screen-properties (elscreen-get-screen-property (elscreen-get-current-screen))))
    (jg-elscreen-reorder-buffer-list 
     (remove-if-not 'buffer-live-p 
                    (or (get-alist 'jg-buffer-list screen-properties)
                        (list (get-buffer "*scratch*")))))))


;;make ido-switch-buffer (& friends) use my buffer list
(add-hook 'ido-make-buffer-list-hook 
          (lambda () (setq ido-temp-list (mapcar 'buffer-name (jg-elscreen-get-buffer-list)) )))



(defun jg-elscreen-reorder-buffer-list (the-list)
  "Set buffers in NEW-LIST to be the most recently used, in order."
    (ad-deactivate 'buffer-list)
    (setq real-buffer-list (buffer-list))
    (ad-activate 'buffer-list)

    (jg-elscreen-filter-buffer-list the-list real-buffer-list)
    
    )

(defun jg-elscreen-filter-buffer-list (the-list real-buffer-list)
  (delq nil
        (mapcar (lambda (x) 
                  (and 
                   (member (buffer-name x) (mapcar 'buffer-name the-list))
                   x ))
                real-buffer-list)))



(defadvice display-buffer (after jg-elscreen-display-buffer-advice activate)
  ;;(message (buffer-name (current-buffer)))
  (jg-elscreen-add-buffer-to-list (current-buffer)))

(defadvice switch-to-buffer (after jg-elscreen-switch-to-buffer-advice activate)
  ;;(message (buffer-name (current-buffer)))
  (jg-elscreen-add-buffer-to-list (current-buffer)))


(defadvice buffer-list (around jg-elscreen-buffer-list activate)
  (setq ad-return-value (jg-elscreen-get-buffer-list))
)


;; turn off the jg buffer-list for this, so I have means to look at them all if I want to.
(defadvice ibuffer (around jg-elscreen-turn-off-buffer-list-advice activate)
  (ad-deactivate 'buffer-list)
  ad-do-it
  (ad-activate 'buffer-list)
)


;; these are more low-level commands that didn't end up working out advising them.
;; they are called too many times, all the time, for the minibuffer and echo area buffers, etc.
;; even up to a dozen times or so in a singe keystroke. leaving here for a reminder that they
;; exist in case I ever come back here spelunking.
;; (defadvice set-buffer (after jg-elscreen-set-buffer-advice activate)
;;   (jg-elscreen-add-buffer-to-list (ad-get-arg 0)))

;; (defadvice set-window-buffer (after jg-elscreen-set-window-buffer-advice activate)
;;   (jg-elscreen-add-buffer-to-list (ad-get-arg 1)))

;; (defadvice pop-to-buffer (after jg-elscreen-pop-to-buffer-advice activate)
;;   (jg-elscreen-add-buffer-to-list (ad-get-arg 0)))

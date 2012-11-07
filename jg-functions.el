;; JG functions


;; duplicate a line
(defun duplicate-current-line (num)
  (interactive)
  (beginning-of-line nil)
  (let ((b (point)))
    (end-of-line nil)
    (copy-region-as-kill b (point)))
  (beginning-of-line num)
  (open-line 1)
  (yank)
  (back-to-indentation))

(defun duplicate-current-line-up ()
  (interactive)
  (duplicate-current-line 1)
)

(defun duplicate-current-line-down ()
  (interactive)
  (duplicate-current-line 2)
)

(defun kill-all-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
                (buffer-list)))


(defun view-percent ()
  (interactive)
  (snippet-insert "<% $. %>")
)

(defun view-percent-equal ()
  (interactive)
  (snippet-insert "<%= $. %>")
)

(defun layout-content ()
  (interactive)
  (snippet-insert "<%= content_for(:$${title}) { $. } %>")
)

(defun css-curlies ()
  (interactive)
  (snippet-insert " {\n$.\n}")
  (forward-line 1)
  (indent-for-tab-command)
  (forward-line -1)
  (indent-for-tab-command)
)

(defun open-line-above ()
  (interactive)
  (forward-line -1)
  (end-of-line nil)
  (open-line 1)
  (forward-line 1)
  (indent-for-tab-command)
)

(defun open-line-below ()
  (interactive)
  (end-of-line nil)
  (open-line 1)
  (forward-line 1)
  (indent-for-tab-command)
)


(defun project-switch ()
  (interactive)
  (let ((project (completing-read "Which project?: " '(("olex3")("indra")("discovery")))))
    (set-project project)
))

(defun set-project (project)
  (if (string= "indra" project)
      (progn (rvm-use "ruby-1.9.3-p286" "indra")
             (fuzzy-find-project-root "~/openlogic/indra")
             ))

  (if (string= "olex3" project)
      (progn (rvm-use "ruby-1.9.3-p286" "olex")
             (fuzzy-find-project-root "~/openlogic/olex")
             ))

  (if (string= "discovery" project)
      (progn (rvm-use "jruby-1.6.5.1" "")
             (fuzzy-find-project-root "~/openlogic/discovery")
             ))
)


;; yank from the kill ring in reverse
(defun paste-unshift ()
  (interactive)
  (yank-pop -1)
)





;; (defadvice ?-?-? (before open-new-tab activate)
;;   "Opens all buffers in a new tab."
;;   ;;(elscreen-create)
;; )













;;;;;;;;;;;;;;;;;;;;;;;;
;;  Point-to-char stuff
;;;;;;;;;;;;;;;;;;;;;;;;
(defvar point-to-char-last-char nil)
(defvar point-to-char-use-last-char nil)


(defun prompt-for-char ();;(char)
  "Get a character from the minibuffer prompt"
  (interactive)
  (setq char (read-key "Char:"))
  (message (char-to-string char))
  (if (or (equal ?\C-g char) (equal ?\C-b char))
      (setq point-to-char-use-last-char t))
  (if point-to-char-use-last-char
      (progn
        ;(message "use last char!")
        (setq char point-to-char-last-char)
        (setq point-to-char-use-last-char nil))
      (progn
        ;(message "don't use last char!")
        (if (char-table-p translation-table-for-input)
            (setq char (or (aref translation-table-for-input char) char)))
        ;(message (char-to-string char))
        (setq point-to-char-last-char char)
        )
      )
  (local-set-key (kbd "C-6") nil)
  ;(message "hi")
  char
)



(defun point-to-char (char arg)
  "Move to ARG'th occurrence of CHAR."
  (setq case-fold-search nil) ;;temporarily set case sensitivity true
  (search-forward
   (char-to-string char) nil nil arg)
  (setq case-fold-search t) ;; set it back
)

(defun forward-to-char ()
  (interactive)
  (point-to-char (call-interactively 'prompt-for-char) 1)
)
(defun backward-to-char ()
  (interactive)
  (point-to-char (call-interactively 'prompt-for-char) -1)
)

;;;;;;;;;;;;;;;;;;;;;;;;
;; / Point-to-char stuff
;;;;;;;;;;;;;;;;;;;;;;;;


;; I found this on the internet somewhere
(defun semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (decf arg))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (incf arg)))
  (up-list arg))



;; written by steve yegge, I believe
(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))


(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

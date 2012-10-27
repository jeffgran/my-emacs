;; JG functions

;; move lines up and down
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

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


(defun increase-select (n word)
  (let (p1 p2)
    (if mark-active
	(progn
	  (if (> 0 n)
	      (progn
		(setq p1 (region-beginning))
		(setq p2 (region-end)))
	    (progn
		(setq p2 (region-beginning))
		(setq p1 (region-end))))
	  )
      (progn 
	  (setq p1 (point))
	  (setq p2 (point))
	  )
      )
    (goto-char p1)
    (if word
        (forward-word n)
      (forward-char n))
    (push-mark p2)
    (setq mark-active t)
    )
  )

(defun decrease-select (n word)
  (let (p1 p2)
    (if mark-active
	(progn
	  (if (> 0 n)
	      (progn
		(setq p1 (region-beginning))
		(setq p2 (region-end)))
	    (progn
		(setq p2 (region-beginning))
		(setq p1 (region-end))))
	  )
      (progn 
	  (setq p1 (point))
	  (setq p2 (point))
	  )
      )
    (goto-char p2)
    (if word
        (backward-word (- 0 n))
      (backward-char (- 0 n)))
    (push-mark p1)
    (setq mark-active t)
    )
  )

(defun backward-word-select ()
  (interactive)
  (if mark-active
      (increase-select -1 t)
    (backward-word))
)

(defun forward-word-select ()
  (interactive)
  (if mark-active
      (increase-select 1 t)
    (forward-word))
)

(defun backward-char-select ()
  (interactive)
  (if mark-active
      (increase-select -1 nil)
    (backward-char))
)

(defun forward-char-select ()
  (interactive)
  (if mark-active
      (increase-select 1 nil)
    (forward-char))
)

(defun backward-word-unselect ()
  (interactive)
  (decrease-select -1 t)
)

(defun forward-word-unselect ()
  (interactive)
  (decrease-select 1 t)
)

(defun backward-char-unselect ()
  (interactive)
  (decrease-select -1 nil)
)

(defun forward-char-unselect ()
  (interactive)
  (decrease-select 1 nil)
)

(defun string-wrap ()
  (interactive)
  (if mark-active
      (progn
        (save-excursion
          (setq p1 (region-beginning))
          (setq p2 (region-end))
          (goto-char p2)
          (insert "\"")
          (goto-char p1)
          (insert "\"")
          ))))


(defun jg-rgrep (regexp)
   "Run `rgrep' with REGEXP, \"*.rb\" FILES"
   (interactive
    (progn
      (grep-compute-defaults)
      (list (grep-read-regexp))))
   (rgrep regexp "*.rb *.erb *.dryml *.rhtml *.rxml *.css *.js" "/users/jgran/openlogic/olex/")) 

(defun project-switch ()
  (interactive)
  (let ((project (completing-read "Which project?: " '(("olex3")("indra")("discovery")))))
    (set-project project)
))

(defun set-project (project)
  (if (string= "indra" project)
      (progn (rvm-use "ruby-1.9.2-p290" "indra")
             (fuzzy-find-project-root "~/openlogic/indra")
             ))

  (if (string= "olex3" project)
      (progn (rvm-use "ruby-1.9.2-p290" "olex3")
             (fuzzy-find-project-root "~/openlogic/olex")
             ))

  (if (string= "discovery" project)
      (progn (rvm-use "jruby-1.6.5.1" "")
             (fuzzy-find-project-root "~/openlogic/discovery")
             ))
)

(defun paste-unshift ()
  (interactive)
  (yank-pop -1)
)


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
  (search-forward
   (char-to-string char) nil nil arg)
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


;; I found this on the internet somewhere
(defun extend-selection (arg &optional incremental)
  "Mark the symbol surrounding point.
Subsequent calls mark higher levels of sexps."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (and transient-mark-mode mark-active)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1)))
  (setq mark-active t)
)

;; I found these two on the internet somewhere
;; copy/cut whole line when nothing is selected
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if (region-active-p) (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if (region-active-p) (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))


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


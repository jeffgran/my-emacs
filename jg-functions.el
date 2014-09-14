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
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command)
)

(defun open-line-below ()
  (interactive)
  (end-of-line nil)
  (open-line 1)
  (forward-line 1)
  (indent-for-tab-command)
)




;;*************************************************
;;*************************************************
(defvar jg-default-project-root "~/my-emacs/")

(defun jg-project-root () 
  (or (jg-elscreen-get-current-property 'jg-project-root)
      jg-default-project-root)
)

(defun jg-elscreen-get-current-property (name)
  (let* ((properties (elscreen-get-screen-property (elscreen-get-current-screen)))
         (property (get-alist name properties)))
    property
    )
)

(defun ido-jg-set-project-root ()
  (interactive)
  (let* ((dir-path (ido-read-directory-name "[JG] Select project root: "))
         (dir-name (nth 1 (reverse (split-string dir-path "/")))))
    ;;; (message dir-path)
    ;;; (message dir-name)

    (let ((screen-properties (elscreen-get-screen-property (elscreen-get-current-screen))))
      (set-alist 'screen-properties 'jg-project-root dir-path)
      (elscreen-set-screen-property (elscreen-get-current-screen) screen-properties))

    (setq fuzzy-find-project-root dir-path)
    (elscreen-screen-nickname dir-name)
    (rvm-elscreen-activate-corresponding-ruby dir-path)
    (cd dir-path)
    (if (file-exists-p (concat dir-path "TAGS"))
        (visit-project-tags)
      (build-ctags dir-path))
))


;; TODO add the ablitity to pass the path to the real rvm func, that
;; way we don't have to use a separate (almost identical) func
(defun rvm-elscreen-activate-corresponding-ruby (path)
    "activate the corresponding ruby version for the path passed in.
This function searches for an .rvmrc file and activates the configured ruby.
If no .rvmrc file is found, the default ruby is used insted."
    (interactive)
    (when (rvm-working-p)
      (let* ((rvmrc-path (rvm--rvmrc-locate path))
             (rvmrc-info (if rvmrc-path (rvm--rvmrc-read-version rvmrc-path) nil)))
        (if rvmrc-info (rvm-use (first rvmrc-info) (second rvmrc-info))
          (rvm-use-default)))))



(defadvice rvm-use (after rvm-elscreen-save-info activate)
"Save the rvm ruby and gemset we just switched to in the elscreen screen properties list"
  (let ((screen-properties (elscreen-get-screen-property (elscreen-get-current-screen))))
    (set-alist 'screen-properties 'rvm-ruby (ad-get-arg 0))
    (set-alist 'screen-properties 'rvm-gemset (ad-get-arg 1))
    (elscreen-set-screen-property (elscreen-get-current-screen) screen-properties)
))


(defun rvm-elscreen-recall ()
"Recall the saved rvm ruby and gemset from the screen properties and set them as the current."
  (let* ((screen-properties (elscreen-get-screen-property (elscreen-get-current-screen)))
         (rvm-ruby (get-alist 'rvm-ruby screen-properties))
         (rvm-gemset (get-alist 'rvm-gemset screen-properties)))
    (if (and rvm-ruby rvm-gemset)
        (rvm-use rvm-ruby rvm-gemset)
      (rvm-use-default))))

;; different approach: advise the fuzzy-find method to use whatever the current property is
;; more lazy/functional approach
(defun jg-fuzzy-find-in-project ()
  "Wrapper around `fuzzy-find-project-root' to use the current `jg-project-root'
as the fuzzy-find root"
  (interactive)
  (let ((fuzzy-find-project-root (jg-project-root)))
    (fuzzy-find-in-project)
    )
)

;; from the internet somewhere. stackoverflow I think
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))




;; Upon switching to a new screen, recall the rvm setup
(add-hook 'elscreen-goto-hook 'rvm-elscreen-recall)
(add-hook 'elscreen-kill-hook 'rvm-elscreen-recall)


;; advise the interactive forms of these functions (cut and copy) to
;; use the current line if nothing is selected (no region)
(put 'kill-ring-save 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

(put 'kill-region 'interactive-form      
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))








;; yank from the kill ring in reverse
(defun paste-unshift ()
  (interactive)
  (yank-pop -1)
)


;; ==========================================================================
;; *shell* mode stuff
;; -----------------
(defun clear-shell ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)
    (move-end-of-line 1)))


;; open a new shell with a better name,
;; in the root of the current project
(defun jg-new-shell ()
  (interactive)
  (let ((default-directory (jg-project-root)))
        (shell (generate-new-buffer-name "$shell")))
)

(defun jg-ansi-colorize-buffer ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max))
)


;; run a shell command and print the output at point
(defun shell-command-insert-output-here ()
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u / universal prefix arg
    (call-interactively 'shell-command)
    )
  )

;; run a shell command on the selected region, and replace it with the output
(defun shell-command-on-region-replace ()
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u / universal prefix arg
    (call-interactively 'shell-command-on-region)
    )
  )

(defun fix-stdin-buffer ()
  (cond
   ;; When opening a stdin temp file from a pipe in the shell, via ~/.bash/ebuffer
   ((string-match "___STDIN-.+" (buffer-name))
    (comint-mode)
    (ansi-color-for-comint-mode-on)
    (jg-ansi-colorize-buffer)
    )))
(add-hook 'server-visit-hook 'fix-stdin-buffer)



;; from http://stackoverflow.com/questions/7987494/emacs-shell-mode-display-is-too-wide-after-splitting-window
(defun comint-fix-window-size ()
  "Change process window size."
  (when (derived-mode-p 'comint-mode)
    (set-process-window-size (get-buffer-process (current-buffer))
                             (window-height)
                             (window-width))))
(defun my-shell-mode-hook ()
  ;; add this hook as buffer local, so it runs once per window.
  (add-hook 'window-configuration-change-hook 'comint-fix-window-size nil t)
  (setq comint-input-autoexpand nil))
(add-hook 'shell-mode-hook 'my-shell-mode-hook)
;; ==========================================================================


;; ssh-mode
;; open a new ssh shell with a better name,
(defun jg-open-ssh ()
  (interactive)
  
  ;; now set up some stuff in the new buffer
  (let ((ssh-buffer (call-interactively 'ssh)))
    (with-current-buffer ssh-buffer
      (rename-buffer (concat ssh-remote-user "@" ssh-host) t)
      ;; (make-local-variable 'explicit-shell-file-name)
      ;; (setq explicit-shell-file-name nil)
      (make-local-variable 'shell-file-name)
      (setq shell-file-name "/bin/bash")
      )))


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
  (if (or (equal ?\C-t char) (equal ?\C-b char))
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


;; ------------------------------------------------------------------------
;; from http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags/
(defun build-ctags (&optional passed-root)
  (interactive)
  (message "building project tags")
  (let ((root (or passed-root (jg-project-root))))
    (shell-command (concat "ctags -e -R --extra=+fq --exclude=db --exclude=test --exclude=jars --exclude=vendor --exclude=.git --exclude=public -f " root "TAGS " root))
    (visit-project-tags))
  (message "tags built successfully"))
(defun visit-project-tags ()
  (interactive)
  (let ((tags-file (concat (jg-project-root) "TAGS")))
    (visit-tags-table tags-file)
    (message (concat "Loaded " tags-file))))
;; ------------------------------------------------------------------------


;; from http://stackoverflow.com/questions/3139970/open-a-file-at-line-with-filenameline-syntax
;; (modified by jg)
(require 'ffap)
(defun find-file-at-point-with-line()
  "if file has an attached line num goto that line, ie boom.rb:12"
  (interactive)
  (setq line-num 0)
  (save-excursion
    (search-forward-regexp "[^ ]:" (point-max) t)
    (if (looking-at "[0-9]+")
         (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
  (if (ffap-file-at-point)
      (progn
        (find-file (ffap-file-at-point))
        (when (not (equal line-num 0))
          (goto-line line-num)
          (recenter-top-bottom)))
    (ding)))



(defun isearch-yank-symbol-string ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if sym
        (progn
          (setq isearch-regexp nil
                isearch-string (symbol-name sym)
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))




;; (defun le::save-buffer-force-backup (arg)
;;   "save buffer, always with a 2 \\[universal-argument]'s
;; see `save-buffer'
;; With ARG, don't force backup.
;; "
;;   (interactive "P")
;;   (if (consp arg)
;;       (save-buffer)
;;     (save-buffer 16)))
;; (global-set-key [remap save-buffer] 'le::save-buffer-force-backup)

(defadvice back-button-pop-local-mark (after center-after-back-button-local activate)
  "Center the view after moving it"
  (recenter))
(ad-disable-advice 'back-button-local 'after 'center-after-back-button-local)

(defadvice rvm-use (after rvm-elscreen-save-info activate)
  "Save the rvm ruby and gemset we just switched to in the elscreen screen properties list"
  (let ((screen-properties (elscreen-get-screen-property (elscreen-get-current-screen))))
    (set-alist 'screen-properties 'rvm-ruby (ad-get-arg 0))
    (set-alist 'screen-properties 'rvm-gemset (ad-get-arg 1))
    (elscreen-set-screen-property (elscreen-get-current-screen) screen-properties)
    ))

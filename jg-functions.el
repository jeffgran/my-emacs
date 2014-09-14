;; JG functions

(defun kill-all-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
                (buffer-list)))


;; (defun view-percent ()
;;   (interactive)
;;   (snippet-insert "<% $. %>")
;; )

;; (defun view-percent-equal ()
;;   (interactive)
;;   (snippet-insert "<%= $. %>")
;; )

;; (defun layout-content ()
;;   (interactive)
;;   (snippet-insert "<%= content_for(:$${title}) { $. } %>")
;; )

;; (defun css-curlies ()
;;   (interactive)
;;   (snippet-insert " {\n$.\n}")
;;   (forward-line 1)
;;   (indent-for-tab-command)
;;   (forward-line -1)
;;   (indent-for-tab-command)
;; )

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
    ;;(rvm-elscreen-activate-corresponding-ruby dir-path)
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
    (shell (generate-new-buffer-name "$shell"))))

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
    ))

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
  (add-hook 'window-configuration-change-hook 'comint-fix-window-size nil t))
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


(defun prompt-for-char ()
  "Get a character from the minibuffer prompt"
  (interactive)
  (setq char (read-key "Char:"))
  
  (if (characterp char)
      (progn
        (if (or (equal ?\C-t char) (equal ?\C-b char))
            (setq point-to-char-use-last-char t))
        (if point-to-char-use-last-char
            (progn
              (message (char-to-string point-to-char-last-char))
                                        ;(message "use last char!")
              (setq char point-to-char-last-char)
              (setq point-to-char-use-last-char nil))
          (progn
            (message (char-to-string char))
                                        ;(message "don't use last char!")
                                        ;(message (char-to-string char))
            (setq point-to-char-last-char char)
            )
          )
        (local-set-key (kbd "C-6") nil)
                                        ;(message "hi")
        char)
    ;; else
    (progn
      (message "not a charater")
      (print char t)
      nil)
    ))


(defun point-to-char (char arg)
  "Move to ARG'th occurrence of CHAR."
  (if (and char (characterp char))
      (progn
        (setq case-fold-search nil) ;;temporarily set case sensitivity true
        (if (and (> arg 0) (eq (char-after (point)) char))
            (setq arg (+ 1 arg)))
        (if (and (< arg 0) (eq (char-after (- (point) 1)) char))
            (setq arg (- arg 1)))
        (search-forward
         (char-to-string char) nil t arg)
        (setq case-fold-search t) ;; set it back
        char)
    char))

(defun forward-to-char (arg)
  (interactive "p")
  (let ((char (progn
                (if (or (eq last-command this-command) (eq last-command 'backward-to-char))
                    point-to-char-last-char
                  (call-interactively 'prompt-for-char)))
              )))
  (point-to-char char arg))

(defun backward-to-char (arg)
  (interactive "p")
  (let ((char (progn
                (if (or (eq last-command this-command) (eq last-command 'forward-to-char))
                    point-to-char-last-char
                  (call-interactively 'prompt-for-char)))
              )))
  (point-to-char char (- arg)))

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


;; dunno where I found this but then I modified it.
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (let* ((point-was-backwards (progn
                                  (if (> (point) (mark))
                                      (progn (exchange-point-and-mark) t)
                                    nil)
                                  ))
           (original-point-column (current-column))
           (original-mark-column (prog2
                                     (exchange-point-and-mark)
                                     (current-column)
                                   (exchange-point-and-mark))))
      (let* ((beg (line-beginning-position))
             (end (progn
                    (exchange-point-and-mark)
                    (if (and (eq (line-beginning-position) (point))
                             (not (eq beg (point))))
                        (line-beginning-position)
                      (+ 1 (line-end-position)))))
             (text   (delete-and-extract-region beg end)))
        (forward-line arg)
        (beginning-of-line)
        (insert text)
        (if point-was-backwards
            (progn
              (backward-char (length text))
              (move-to-column original-point-column)
              (set-mark (point))
              (beginning-of-line)
              (forward-char (length text))
              (unless (eq 0 original-mark-column)
                (progn (backward-char)
                       (move-to-column original-mark-column)))
              )
          (progn
            (unless (eq 0 original-mark-column)
              (backward-char))
            (move-to-column original-mark-column)
            (set-mark (point))
            (unless (eq 0 original-mark-column)
              (end-of-line))
            (backward-char (- (length text) 1))
            (move-to-column original-point-column)
            ))
        
        (setq deactivate-mark nil)
        )))
   (t
    (let ((original-column (current-column)))
      (beginning-of-line)
      
      (when (or (> arg 0) (not (eobp)))
        (forward-line)
        (when (or (< arg 0) (not (bobp)))
          (transpose-lines arg))
        (forward-line -1)
        (forward-char original-column)
        )))
   ))

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
  (let ((beg nil)
        (end nil)
        (original-point (point))
        (original-mark (mark))
        (mark-was-active mark-active)
        (point-was-backwards (> (point) (mark))))
    (if (and mark-active point-was-backwards)
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (setq end (progn
                (if mark-was-active
                    ;; now make the point the end, not the beginning
                    (exchange-point-and-mark))
                
                (if (and (eq (line-beginning-position) (point))
                         (not (eq beg (point))))
                    (point)
                  (+ 1 (line-end-position)))))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i (abs arg))
        (goto-char end)
        (insert region)
        (setq end (point)))
      (if (> arg 0)
          (progn
            (set-mark (+ original-mark (* arg (length region))))
            (goto-char (+ original-point (* arg (length region)))))
        (progn
          (set-mark original-mark)
          (goto-char original-point)))
      (setq deactivate-mark (not mark-was-active)))))

(defun duplicate-current-line-or-region-up (arg)
  (interactive "p")
  (duplicate-current-line-or-region (- (or arg 1))))


;; written by me
(defun select-whole-line-or-lines ()
  (interactive)
  (if (and mark-active (> (point) (mark)))
    (exchange-point-and-mark))
  (let ((end-pos (if mark-active (mark) (point))))
    (set-mark (line-beginning-position))
    (goto-char end-pos)
    (end-of-line)
    (forward-char)))


;; written by me
(defun select-whole-line-or-lines-backwards ()
  (interactive)
  (if (and mark-active (< (point) (mark)))
      (exchange-point-and-mark))
  (let ((beg-pos
         (if (and mark-active (eq (char-after (point)) (string-to-char "\n")) (eq (char-before (mark)) (string-to-char "\n")) )
             (and (message "here") (- (mark) 1)) ;; extend back another line if we're already at the beginning
           (if mark-active
               (mark)
             (point)))))
    (set-mark
     (if (and mark-active (> (point) (mark)) (eq (char-before (point)) (string-to-char "\n")) (not (eq (char-after (point)) (string-to-char "\n"))))
         (- (line-beginning-position) 1)
       (line-end-position)))
    (goto-char beg-pos)
    (beginning-of-line)
    ))

(defun kill-whole-line-or-lines ()
  (interactive)
  (call-interactively 'select-whole-line-or-lines)
  (kill-region (point) (mark)))

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

;; JG functions

(defun kill-all-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (buffer-list)))


(defun demi-brolin ()
  (interactive)
  (save-excursion
    (progn
      (end-of-line)
      (insert-char ?\;))))


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

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))


;;*************************************************
;;*************************************************
(defvar jg-default-project-root emacs-root)

(defun jg-project-root ()
  (or (cdr (assoc 'jg-project-root (elscreen-get-screen-property (elscreen-get-current-screen))))
      jg-default-project-root))

;; (defun jg-project-root ()
;;   (projectile-project-root))


(defun ido-jg-set-project-root ()
  (interactive)
  (let* ((dir-path (read-directory-name "[JG] Select project root: " (or default-directory emacs-root) default-directory))
         (dir-name (nth 1 (reverse (split-string dir-path "/")))))
    ;;; (message dir-path)
    ;;; (message dir-name)

    (let ((screen-properties (elscreen-get-screen-property (elscreen-get-current-screen))))
      (elscreen--set-alist 'screen-properties 'jg-project-root dir-path)
      (elscreen-set-screen-property (elscreen-get-current-screen) screen-properties))

    (elscreen-screen-nickname dir-name)
    (cd dir-path)
    (call-interactively 'jg-new-shell)
    ;; (if (file-exists-p (concat dir-path "TAGS"))
    ;;     (visit-project-tags)
    ;;   (build-ctags dir-path))
    ))


;; from the internet somewhere. stackoverflow I think
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))






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
  (let ((comint-buffer-maximum-size 1))
    (comint-truncate-buffer)
    (move-end-of-line 2)))


;; open a new shell with a better name,
;; in the root of the current project
(defun jg-new-shell ()
  (interactive)
  (let ((default-directory (jg-project-root)))
    (shell (generate-new-buffer-name "$shell"))))

(defun jg-new-inf-ruby ()
  (interactive)
  (let ((default-directory (jg-project-root)))
    (inf-ruby)
    (rename-buffer (generate-new-buffer-name "$ruby"))))

(defun jg-ansi-colorize-buffer ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max))
  )


;; run a shell command and print the output at point
(defun shell-command-insert-output-here ()
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u / universal prefix arg
    (call-interactively 'shell-command)))

;; run a shell command on the selected region, and replace it with the output
(defun shell-command-on-region-replace ()
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u / universal prefix arg
    (call-interactively 'shell-command-on-region)
    ))

(defun fix-stdin-buffer ()
  (cond
   ;; When opening a stdin temp file from a pipe in the shell, via ~/my-bash/bin/ebuffer
   ((string-match "___STDIN-.+" (buffer-name))
    (comint-mode)
    (ansi-color-for-comint-mode-on)
    (jg-ansi-colorize-buffer)
    )))
(add-hook 'server-visit-hook 'fix-stdin-buffer)


;;(defvar jg-shell-output-buffer "")
;; (defun jg-shell-filter-long-lines (string)
;;   (let* ((max-len (* 2 (window-width)))
;;          (lines (split-string string "\n")))
;;     (mapconcat '(lambda (s) (substring s 0 (min (length s) max-len))) lines "\n")
;;     ))

;; (add-hook 'comint-preoutput-filter-functions #'jg-shell-filter-long-lines)

;; from http://stackoverflow.com/questions/7987494/emacs-shell-mode-display-is-too-wide-after-splitting-window
;; (defun comint-fix-window-size ()
;;   "Change process window size."
;;   (when (derived-mode-p 'comint-mode)
;;     (set-process-window-size (get-buffer-process (current-buffer))
;;                              (window-height)
;;                              (window-width))))
;; (defun my-shell-mode-hook ()
;;   ;; add this hook as buffer local, so it runs once per window.
;;   (add-hook 'window-configuration-change-hook 'comint-fix-window-size nil t)
;;   (setq comint-input-autoexpand nil))
;; (add-hook 'shell-mode-hook 'my-shell-mode-hook)
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
      ;; hack to at least set the default-directory to the home dir of the remote box
      (setq default-directory (concat "/ssh:" (buffer-name (current-buffer)) ":~"))
      ))
  ;; (call-interactively (ssh-directory-tracking-mode)) ;; used to work at OL but causes a weird hang on LT EC2 boxes :(
  )

;; send C-a q to exit screen in lxc:
;; (process-send-string nil (kbd "C-a q"))



;; copied from tramp-sh.el to change the default login args
;; (add-to-list 'tramp-methods
;;              '("ssh-jg"
;;                (tramp-login-program        "ssh -t")
;;                (tramp-login-args           (("-l" "%u") ("-e" "none") ("%h")))
;;                (tramp-async-args           (("-q")))
;;                (tramp-remote-shell         "/bin/sh")
;;                (tramp-remote-shell-args    ("-c"))
;;                (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
;;                                             ("-o" "UserKnownHostsFile=/dev/null")
;;                                             ("-o" "StrictHostKeyChecking=no")))
;;                (tramp-default-port         22)))
;; (setq tramp-default-method "ssh")

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
          (cl-decf arg))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (cl-incf arg)))
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
;; (defun move-text-internal (arg)
;;   (cond
;;    ((and mark-active transient-mark-mode)
;;     (let* ((point-was-backwards (progn
;;                                   (if (> (point) (mark))
;;                                       (progn (exchange-point-and-mark) t)
;;                                     nil)
;;                                   ))
;;            (original-point-column (current-column))
;;            (original-mark-column (prog2
;;                                      (exchange-point-and-mark)
;;                                      (current-column)
;;                                    (exchange-point-and-mark))))
;;       (let* ((beg (line-beginning-position))
;;              (end (progn
;;                     (exchange-point-and-mark)
;;                     (if (and (eq (line-beginning-position) (point))
;;                              (not (eq beg (point))))
;;                         (line-beginning-position)
;;                       (+ 1 (line-end-position)))))
;;              (text   (delete-and-extract-region beg end)))
;;         (forward-line arg)
;;         (beginning-of-line)
;;         (insert text)
;;         (if point-was-backwards
;;             (progn
;;               (backward-char (length text))
;;               (move-to-column original-point-column)
;;               (set-mark (point))
;;               (beginning-of-line)
;;               (forward-char (length text))
;;               (unless (eq 0 original-mark-column)
;;                 (progn (backward-char)
;;                        (move-to-column original-mark-column)))
;;               )
;;           (progn
;;             (unless (eq 0 original-mark-column)
;;               (backward-char))
;;             (move-to-column original-mark-column)
;;             (set-mark (point))
;;             (unless (eq 0 original-mark-column)
;;               (end-of-line))
;;             (backward-char (- (length text) 1))
;;             (move-to-column original-point-column)
;;             ))

;;         (setq deactivate-mark nil)
;;         )))
;;    (t
;;     (let ((original-column (current-column)))
;;       (shut-up 
;;         (beginning-of-line)
;;         (kill-line)
;;         (when (not (eobp))
;;           (delete-forward-char 1))
;;         (forward-line arg)
;;         (yank)
;;         (newline)
;;         (forward-line (- 1))
;;         (beginning-of-line)
;;         (forward-char original-column))))
;;    ))



;; (defun move-text-down (arg)
;;    "Move region (transient-mark-mode active) or current line
;;   arg lines down."
;;    (interactive "*p")
;;    (move-text-internal arg))

;; (defun move-text-up (arg)
;;    "Move region (transient-mark-mode active) or current line
;;   arg lines up."
;;    (interactive "*p")
;;    (move-text-internal (- arg)))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")

  ;; make sure the mark exists, in case it's a new buffer
  (unless (mark)
    (progn
      (set-mark (point))
      (deactivate-mark)))
  
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
             (- (mark) 1) ;; extend back another line if we're already at the beginning
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


;; not currently in use... using default kill-region (and phi-kill-region-dwim)
(defun kill-whole-line-or-lines ()
  (interactive)
  (call-interactively 'select-whole-line-or-lines)
  (kill-region (point) (mark)))

;; from emacswiki
(defun back-to-indentation-or-beginning ()
  (interactive) 
  (if (bolp) (back-to-indentation) (beginning-of-line)))

;; ------------------------------------------------------------------------
;; from http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags/
(defun build-ctags (&optional passed-root)
  (interactive)
  (message "building project tags")
  (let ((root (or passed-root (jg-project-root))))
    (shell-command (concat "ctags -e -R --extra=+fq --exclude=db --exclude=jars --exclude=vendor --exclude=.git --exclude=public -f " root ".tags " root))
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




;; from http://hbin.me/blog/2013/02/24/the-ultimate-solution-for-emacs-find-tags/
(require 'thingatpt)

(defun thing-after-point ()
  "Things after point, including current symbol."
  (if (thing-at-point 'symbol)
      (save-excursion
        (let ((from (beginning-of-thing 'symbol))
              (to   (end-of-thing 'line)))
          (and (> to from)
               (buffer-substring-no-properties from to))))))

(defun ruby-thing-at-point ()
  "Get ruby thing at point.
   1. thing at 'current_user'   get current_user;
   2. thing at '!current_user'  get current_user;
   3. thing at 'current_user!'  get current_user!;
   4. thing at 'current_user='  get current_user=;
   5. thing at 'current_user =' get current_user=;
   6. thing at 'current_user ==' get current_user;
   7. thing at 'current_user ||=' get current_user=;
   Otherwise, get `find-tag-default symbol."
  (if (member (symbol-name major-mode)
              '("ruby-mode" "rhtml-mode" "haml-mode" "slim-mode"))
      (let ((symbol (thing-at-point 'symbol))
            (remain (thing-after-point)))
        (if (and symbol remain)
            (let ((sym (s-chop-prefixes '("!!" "!") symbol))
                  (rem (s-chop-prefixes '("!!" "!") remain)))
              (if (s-matches? (concat "^" sym "\\( *\\(||\\)?=[^=]\\)") rem)
                  (concat sym "=")
                sym))
          (find-tag-default)))
    (find-tag-default)))

(defun current-relative-filename ()
    (file-relative-name (buffer-file-name) (projectile-acquire-root)))

(defun copy-local-relative-filename ()
  (interactive)
  (kill-new (current-relative-filename))
  (message "Copied: %s" (current-kill 0 t)))

(defun copy-local-filename ()
  (interactive)
  (kill-new (file-name-nondirectory (buffer-file-name)))
  (message "Copied: %s" (current-kill 0 t)))

(defun copy-local-relative-filename-with-line ()
  (interactive)
  (kill-new
   (concat (current-relative-filename) ":" (number-to-string (line-number-at-pos))))
  (message "Copied: %s" (current-kill 0 t)))

(defun copy-local-relative-directoryname ()
  (interactive)
  (kill-new (file-name-directory (current-relative-filename)))
  (message "Copied: %s" (current-kill 0 t)))

(defun copy-local-absolute-filename ()
  (interactive)
  (kill-new (buffer-file-name))
  (message "Copied: %s" (current-kill 0 t)))

(defun copy-local-absolute-filename-with-line ()
  (interactive)
  (kill-new (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos))))
  (message "Copied: %s" (current-kill 0 t)))

(defun copy-local-absolute-directoryname ()
  (interactive)
  (kill-new (file-name-directory (buffer-file-name)))
  (message "Copied: %s" (current-kill 0 t)))


(defun git-link-with-prefix ()
  (interactive)
  (let ((current-prefix-arg '-))
    (call-interactively 'git-link)))

(defun git-link-visit-homepage ()
  (interactive)
  (let ((git-link-open-in-browser t))
    (call-interactively 'git-link-homepage)))

(defun git-link-visit ()
  (interactive)
  (let ((git-link-open-in-browser t))
    (call-interactively 'git-link)))

(defun git-link-visit-with-prefix ()
  (interactive)
  (let ((git-link-open-in-browser t)
        (current-prefix-arg '-))
    (call-interactively 'git-link)))


(transient-define-prefix git-link-dispatch ()
  ["Copy Path"
   [("l d" "Relative Directory" copy-local-relative-directoryname)
    ("l f" "Relative File" copy-local-relative-filename)
    ("l l" "Relative File:Line" copy-local-relative-filename-with-line)
    ("l n" "Filename" copy-local-filename)
    ("a d" "Absolute Directory" copy-local-absolute-directoryname)
    ("a f" "Absolute File" copy-local-absolute-filename)
    ("a l" "Absolute File:Line" copy-local-absolute-filename-with-line)]
   ]
  ["Copy Link"
   [("c r" "Repository" git-link-homepage)
    ("c f" "File" git-link-with-prefix)
    ("c l" "Line" git-link)]
   ]
  ["Open Link"
   ("o r" "Repository" git-link-visit-homepage)
   ("o f" "File" git-link-visit-with-prefix)
   ("o l" "Line" git-link-visit)])

(transient-insert-suffix 'magit-dispatch "l"
  '("k" "Link" git-link-dispatch))

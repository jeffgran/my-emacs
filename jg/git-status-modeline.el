;;; git-modeline.el --- Provide git repo summary in modeline
;; https://github.com/jkitchin/jmax/tree/master/techela
;; http://kitchingroup.cheme.cmu.edu/blog/category/git/


;;; Commentary:
;; 

;;; Code:

(defun git-status-modeline-in-git-p ()
  "Return if in a git repo or not."
  (not (string-match "^fatal" (shell-command-to-string "git rev-parse --git-dir"))))


(defun git-status-modeline-repo-name ()
  "TODO"
  (s-trim (shell-command-to-string "basename `git rev-parse --show-toplevel`")))


(defface git-status-modeline-repo-face
  '((t (:foreground "khaki")))
  "This face is used to color the name of the repo in the `git-status-modeline'"
  :group 'git-status-modeline)

(defface git-status-modeline-branch-face
  '((t (:foreground "LightSkyBlue1")))
  "This face is used to color the name of the current branch in the `git-status-modeline'"
  :group 'git-status-modeline)

(defface git-status-modeline-commits-ahead-face
  '((t (:foreground "burlywood1")))
  "TODO `git-status-modeline'"
  :group 'git-status-modeline)

(defface git-status-modeline-commits-behind-face
  '((t (:foreground "RosyBrown1")))
  "TODO `git-status-modeline'"
  :group 'git-status-modeline)

(defface git-status-modeline-dangerous-face
  '((t (:foreground "red")))
  "TODO `git-status-modeline'"
  :group 'git-status-modeline)

(defface git-status-modeline-safe-face
  '((t (:foreground "forest green")))
  "TODO `git-status-modeline'"
  :group 'git-status-modeline)



(defun git-status-modeline-parse-status ()
  "Get git status and return a propertized string."
  (interactive)
  (let ((U 0)   ; untracked files
	(M 0)   ; modified files
	(S 0)   ; staged files
	(O 0)   ; wat?
	(U-files "")
	(M-files "")
	(S-files "")
        (O-files ""))
    (dolist (line (split-string
		   (shell-command-to-string "git status --porcelain")
		   "\n"))

      ;; ignore empty line at end
      ;;((string= "" line) nil)

      (when (string-match "^\\?\\?" line)
       (setq U (+ 1 U))
       (setq U-files (concat U-files "\n" line)))

      (when (string-match "^.[MDA]" line)
       (setq M (+ 1 M))
       (setq M-files (concat M-files "\n" line))
       )

      (when (string-match "^[MDA]" line)
       (setq S (+ 1 S))
       (setq S-files (concat S-files "\n" line))
       ))
      
    ;; construct propertized string
    (concat
     "("
     (s-join "|"
             (remq nil
                   (list
                    (when (> S 0)
                      (propertize
                       (format "S:%d" S)
                       'face 'git-status-modeline-safe-face
                       'help-echo S-files))

                    (when (> M 0)
                      (propertize
                       (format "M:%d" M)
                       'face 'git-status-modeline-dangerous-face
                       'help-echo M-files))

                    (when (> U 0)
                      (propertize
                       (format "?:%d" U)
                       'face 'git-status-modeline-dangerous-face
                       'help-echo U-files))
                    
                    (when (> O 0)
                        (propertize
                         (format "|O:%d" O)
                         'face 'git-status-modeline-dangerous-face
                         'help-echo O-files))
                    
                    )))
     ") ")))


(defun git-status-modeline-remote-status ()
  "Get commits relative to remote, returns propertized string"
  (interactive)
  (let* (
         (repo (git-status-modeline-repo-name))
         ;; get the branch we are on.
         (branch (s-trim
		  (shell-command-to-string
		   "git rev-parse --abbrev-ref HEAD")))
	 ;; get the remote the branch points to.
	 (remote (s-trim
		  (shell-command-to-string
		   (format "git config branch.%s.remote" branch))))
         (remote-branch (s-trim
			 (shell-command-to-string
			  "git for-each-ref --format='%(upstream:short)' $(git symbolic-ref -q HEAD)")))
	 (commits (split-string
		   (s-trim
		    (shell-command-to-string
		     (format
		      "git rev-list --count --left-right HEAD...%s"
		      remote-branch)))))
	 (local (nth 0 commits))
	 (remotes (nth 1 commits)))
    (concat
     (propertize repo 'face 'git-status-modeline-repo-face)
     ":"
     (propertize
      (format "[%s" branch)
      'face 'git-status-modeline-branch-face)

     (when (> (string-to-number local) 0)
       (propertize
        (format "+%s" local)
        'face 'git-status-modeline-commits-ahead-face))

     (when (> (string-to-number remotes) 0)
       (propertize
        (format "-%s" local)
        'face 'git-status-modeline-commits-behind-face))
     (propertize
      "]"
      'face 'git-status-modeline-branch-face)
     )))


(defvar git-status-modeline-dir-backed-major-modes-list (list
                                                         'shell-mode
                                                         'dired-mode)
  "A list of major modes that have a meaningful `default-directory' set,
which should show the git status in the modeline for that directory")

(defun git-status-modeline-update (&optional a-buffer)
  (run-with-idle-timer 1 nil
                       '(lambda (buffer)
                          (setq
                           git-status-modeline-string
                           (if (and (or
                                     (member major-mode git-status-modeline-dir-backed-major-modes-list)
                                     (buffer-file-name (or buffer (current-buffer))))
                                    (git-status-modeline-in-git-p))
                               (concat
                                (git-status-modeline-remote-status)
                                (git-status-modeline-parse-status))
                             ""))
                          (force-mode-line-update))
                       a-buffer))


(defvar git-status-modeline-string ""
  "The string that gets displayed in the modeline.
Automatically gets updated by `git-status-modeline-update'.")
(put 'git-status-modeline-string 'risky-local-variable t)
(make-variable-buffer-local 'git-status-modeline-string)

(defvar git-status-modeline-display nil
  "Non-nil value means display the git-status-modeline, nil value
means do not. Automatically set when turning on/off the
`git-status-modeline-mode' or `git-status-modeline-global-mode'")

(defvar git-status-modeline-element
  '(git-status-modeline-display git-status-modeline-string)
  "The element that we push into `mode-line-format' to add our section
to the mode-line. If you have a custom mode-line-format you can add the
value of this element to it.")


(define-minor-mode git-status-modeline-mode
  "Minor mode to put git repo status in mode-line."
  :init-value nil
  :group 'git-status-modeline
  (if git-status-modeline-mode
      (progn (setq git-status-modeline-display t)
             (when (null (member git-status-modeline-element mode-line-format))
               (push git-status-modeline-element mode-line-format))
             (git-status-modeline-update (current-buffer)))
    (setq git-status-modeline-display nil)))

(define-globalized-minor-mode git-status-modeline-global-mode git-status-modeline-mode
  (lambda () (git-status-modeline-mode t)))





(defadvice display-buffer (after update-git-modeline activate)
  "TODO"
  (git-status-modeline-update (get-buffer (ad-get-arg 0))))
(defadvice switch-to-buffer (after update-git-modeline activate)
  "TODO"
  (git-status-modeline-update (get-buffer (ad-get-arg 0))))

;; elscreen uses this to switch screens, which also ends up displaying a differnt buffer.
(defadvice set-window-configuration (after update-git-modeline activate)
  "TODO"
  (git-status-modeline-update))


(add-hook 'comint-output-filter-functions 'git-status-modeline-comint-output-filter)

(defun git-status-modeline-comint-output-filter (output)
  "TODO"
  (when (and (string-match comint-prompt-regexp output)
             (not
              (string-match (concat comint-prompt-regexp ".*\n.*") output)))
                                    (git-status-modeline-update)))



(provide 'git-status-modeline)


;;; git-status-modeline.el ends here

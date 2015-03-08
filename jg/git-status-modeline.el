;;; git-modeline.el --- Provide git repo summary in modeline
;; https://github.com/jkitchin/jmax/tree/master/techela
;; http://kitchingroup.cheme.cmu.edu/blog/category/git/


;;; Commentary:
;; 

;;; Code:


(defvar git-status-modeline-dir-backed-major-modes-list
  (list 'shell-mode 'dired-mode)
  "A list of major modes that have a meaningful `default-directory' set,
which should show the git status in the modeline for that directory")

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

(defface git-status-modeline-modified-files-face
  '((t (:foreground "tomato")))
  "TODO `git-status-modeline'"
  :group 'git-status-modeline)

(defface git-status-modeline-untracked-files-face
  '((t (:foreground "plum")))
  "TODO `git-status-modeline'"
  :group 'git-status-modeline)

(defface git-status-modeline-safe-face
  '((t (:foreground "SeaGreen3")))
  "TODO `git-status-modeline'"
  :group 'git-status-modeline)



(defun git-status-modeline-in-git-p ()
  "Return if in a git repo or not."
  (not (string-match "^fatal" (shell-command-to-string "git rev-parse --git-dir"))))


(defun git-status-modeline-parse-status (status)
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
    (dolist (line (split-string status "\n"))

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

                    (if (> M 0)
                        (propertize
                         (format "M:%d" M)
                         'face 'git-status-modeline-modified-files-face
                         'help-echo M-files)
                      (propertize
                       "M:0"
                       'face 'git-status-modeline-safe-face
                       'help-echo M-files))

                    (when (> U 0)
                      (propertize
                       (format "?:%d" U)
                       'face 'git-status-modeline-untracked-files-face
                       'help-echo U-files))
                    
                    (when (> O 0)
                        (propertize
                         (format "|O:%d" O)
                         'face 'git-status-modeline-untracked-files-face
                         'help-echo O-files))
                    
                    )))
     ") ")))


(defun git-status-modeline-remote-status (repo branch commits-ahead commits-behind)
  "Get commits relative to remote, returns propertized string"
  (concat
   (propertize (concat repo ":") 'face 'git-status-modeline-repo-face)
   (propertize
    (format "[%s" branch)
    'face 'git-status-modeline-branch-face)

   (when (> (string-to-number commits-ahead) 0)
     (propertize
      (format "+%s" commits-ahead)
      'face 'git-status-modeline-commits-ahead-face))

   (when (> (string-to-number commits-behind) 0)
     (propertize
      (format "-%s" commits-behind)
      'face 'git-status-modeline-commits-behind-face))
   (propertize
    "]"
    'face 'git-status-modeline-branch-face)
   ))

(defvar git-status-modeline-repo nil
  "The buffer's repo.")
(make-variable-buffer-local 'git-status-modeline-repo)

(defun git-status-modeline-get-repo ()
  (or git-status-modeline-repo
      (setq git-status-modeline-repo
            (s-trim (shell-command-to-string
                     "git rev-parse --show-toplevel")))))

(defun git-status-modeline-generate ()
  "TODO"
  (if (file-remote-p default-directory)
      "" ; doesn't work in remote files/shells for now
    (progn
      (message "git-status generating for %s" (current-buffer))
      (let* (
             (repo (git-status-modeline-get-repo))
             (branch (s-trim (shell-command-to-string
                              "git rev-parse --abbrev-ref HEAD")))
             (commits (split-string
                       (s-trim (shell-command-to-string
                                "git rev-list --count --left-right HEAD...@{upstream}"))))
             (status (shell-command-to-string
                      "git status --porcelain"))
             (commits-ahead (nth 0 commits))
             (commits-behind (nth 1 commits)))

        (let ((result (concat
                       (git-status-modeline-remote-status (file-name-nondirectory repo) branch commits-ahead commits-behind)
                       (git-status-modeline-parse-status status))))
          (setq git-status-modeline-cache-plist (lax-plist-put git-status-modeline-cache-plist repo result))
          result
          )))))

(defun git-status-modeline-get ()
  "TODO"
  (or (lax-plist-get git-status-modeline-cache-plist (git-status-modeline-get-repo))
      (git-status-modeline-generate)))

(defvar git-status-modeline-cache-plist ()
  "Cache of the git status, by repository.")


(defun git-status-modeline-update (&optional a-buffer)
  "TODO"
  (run-with-idle-timer 0.5 nil
                       '(lambda (buffer)
                          (when (buffer-live-p buffer)
                            (with-current-buffer buffer
                              (when (git-status-modeline-should-update-p)
                                (setq git-status-modeline-string (git-status-modeline-get))
                                (force-mode-line-update)))))
                       (or a-buffer (current-buffer))))



(define-minor-mode git-status-modeline-mode
  "Minor mode to put git repo status in mode-line."
  :init-value nil
  :group 'git-status-modeline
  (if git-status-modeline-mode
      (progn (setq git-status-modeline-display t)
             (when (null (member git-status-modeline-element mode-line-format))
               (push git-status-modeline-element mode-line-format))
             (when (git-status-modeline-should-update-p)
               (git-status-modeline-update (current-buffer))))
    (setq git-status-modeline-display nil)))

(define-globalized-minor-mode git-status-modeline-global-mode git-status-modeline-mode
  (lambda () (git-status-modeline-mode t)))



(defun git-status-modeline-should-update-p ()
  "TODO should be called with correct (current-buffer)"
  (and (or
        (member major-mode git-status-modeline-dir-backed-major-modes-list)
        (buffer-file-name (current-buffer)))
       (git-status-modeline-in-git-p)))



;; (defadvice display-buffer (after update-git-modeline activate)
;;   "TODO"
;;   (when (git-status-modeline-should-update-p)
;;     (git-status-modeline-update)))

(defadvice switch-to-buffer (after update-git-modeline activate)
  "TODO"
  (when (git-status-modeline-should-update-p)
    (git-status-modeline-update)))

(defadvice set-window-configuration (after update-git-modeline activate)
  "TODO"
  (when (git-status-modeline-should-update-p)
    (git-status-modeline-update)))

(defadvice magit-refresh (after update-git-modeline activate)
  "TODO"
  (git-status-modeline-invalidate-cache))

(add-hook 'after-save-hook 'git-status-modeline-invalidate-cache)
(defun git-status-modeline-invalidate-cache ()
  "TODO"
  (interactive) ;; you can manually call this to refresh
  (when (not (file-remote-p default-directory))
    (setq git-status-modeline-cache-plist
          (lax-plist-put git-status-modeline-cache-plist (git-status-modeline-get-repo) nil))
    (git-status-modeline-update)))

(add-hook 'comint-output-filter-functions 'git-status-modeline-comint-output-filter)

(defun git-status-modeline-comint-output-filter (output)
  "TODO"
  (when (and (s-ends-with? "\n" output)
             (string-match comint-prompt-regexp output)
             (not
              (string-match (concat comint-prompt-regexp ".*\n.*") output)))
                                    (git-status-modeline-invalidate-cache)))



(provide 'git-status-modeline)


;;; git-status-modeline.el ends here

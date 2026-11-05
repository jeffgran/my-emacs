;;; jg-agent-comments-review.el --- Review documents for agent comments -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience

;;; Commentary:

;; Review documents: plain files an agent writes for a review session,
;; with commentary interleaved with verbatim git diff output.  This
;; module parses such a document, computes review citations from it,
;; and provides the major mode that reads it.

;;; Code:

(require 'jg-agent-comments)

(defun jg-agent-comments-review--target-parse (line)
  "Parse the Target: LINE of a review document.
Return a plist (:kind :left :right :paths) where :kind is one of
range, commit, worktree, staged, files.  A range target names the
left and right revisions; a commit target names the shown revision in
:right; files targets list their paths in :paths; the other targets
leave :left, :right and :paths nil."
  (cond
   ((string-match
     "\\`Target: git diff \\(.+\\)\\.\\.\\(.+\\)\\'" line)
    (list :kind 'range
          :left (match-string 1 line)
          :right (match-string 2 line)
          :paths nil))
   ((string-match "\\`Target: git show \\(.+\\)\\'" line)
    (list :kind 'commit :left nil :right (match-string 1 line)
          :paths nil))
   ((string-match-p "\\`Target: worktree diff\\'" line)
    (list :kind 'worktree :left nil :right nil :paths nil))
   ((string-match-p "\\`Target: staged diff\\'" line)
    (list :kind 'staged :left nil :right nil :paths nil))
   ((string-match "\\`Target: files: \\(.+\\)\\'" line)
    (list :kind 'files :left nil :right nil
          :paths (split-string (match-string 1 line) ", +")))
   (t (error "Unrecognized target line: %s" line))))

(defun jg-agent-comments-review--banner-line-p (line)
  "Return non-nil when LINE is a section banner, a run of = signs."
  (string-match-p "\\`=+\\'" line))

(defun jg-agent-comments-review--title-line-p (line)
  "Return non-nil when LINE is a step or uncovered-section title."
  (or (string-match-p "\\`[0-9]+/[0-9]+ — " line)
      (string-match-p "\\`Not covered by the walkthrough" line)))

(defun jg-agent-comments-review--diff-marker (line)
  "Return the path in LINE when it is a diff marker, else nil.
The marker may carry the sliced line range after the path; the path
survives without the suffix."
  (cond
   ((string-match
     "\\`--- diff: \\(.*\\) :[0-9]+-[0-9]+ ---\\'" line)
    (match-string 1 line))
   ((string-match "\\`--- diff: \\(.*\\) ---\\'" line)
    (match-string 1 line))))

(defun jg-agent-comments-review--excerpt-marker (line)
  "Return (PATH BEG END) when LINE is an excerpt marker, else nil."
  (and (string-match
        "\\`--- excerpt: \\(.*\\) :\\([0-9]+\\)-\\([0-9]+\\) ---\\'" line)
       (list (match-string 1 line)
             (string-to-number (match-string 2 line))
             (string-to-number (match-string 3 line)))))

(defun jg-agent-comments-review--section-start (lines i)
  "Return a section-start plist when LINES at index I opens one.
LINES is the document split into lines, 0-based; I is the 0-based
index of the candidate first line.  Return nil when no section opens
there."
  (let ((line (nth i lines))
        (next (nth (1+ i) lines))
        (after (nth (+ i 2) lines)))
    (cond
     ((and (jg-agent-comments-review--banner-line-p line)
           next
           (jg-agent-comments-review--title-line-p next)
           (jg-agent-comments-review--banner-line-p after))
      (list :kind (if (string-prefix-p "Not covered" next)
                      'uncovered
                    'step)
            :title next
            :start i
            :body-start (+ i 3)))
     ((jg-agent-comments-review--diff-marker line)
      (list :kind 'diff
            :path (jg-agent-comments-review--diff-marker line)
            :start i
            :body-start (1+ i)))
     ((jg-agent-comments-review--excerpt-marker line)
      (list :kind 'excerpt
            :path (car (jg-agent-comments-review--excerpt-marker line))
            :excerpt-beg (cadr (jg-agent-comments-review--excerpt-marker line))
            :excerpt-end (nth 2 (jg-agent-comments-review--excerpt-marker line))
            :start i
            :body-start (1+ i))))))

(defun jg-agent-comments-review--line-maps (lines body-start body-end)
  "Build the diff line maps for content LINES from BODY-START to
BODY-END, 0-based indices.  Return (NEW-MAP . OLD-MAP), alists of
\(1-based document line . file line).  @@ headers set both counters;
`+' lines consume the new side, `-' lines the old side, context lines
both.  Blank and meta lines map to neither."
  (let ((new-map '()) (old-map '()) new old)
    (cl-loop for i from body-start to body-end
             for line = (nth i lines)
             do (cond
                 ((string-match
                   "\\`@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
                   line)
                  (setq old (string-to-number (match-string 1 line))
                        new (string-to-number (match-string 3 line))))
                 ((string-match-p
                   "\\`\\(diff --git\\|index \\|--- \\|+++ \\|\\\\\\)" line))
                 ((and new (string-prefix-p "+" line))
                  (setq new-map (cons (cons (1+ i) new) new-map))
                  (setq new (1+ new)))
                 ((and old (string-prefix-p "-" line))
                  (setq old-map (cons (cons (1+ i) old) old-map))
                  (setq old (1+ old)))
                 ((and new old (string-prefix-p " " line))
                  (setq new-map (cons (cons (1+ i) new) new-map))
                  (setq old-map (cons (cons (1+ i) old) old-map))
                  (setq new (1+ new))
                  (setq old (1+ old)))))
    (cons (nreverse new-map) (nreverse old-map))))

(defun jg-agent-comments-review--parse (string)
  "Parse the review document STRING into its sections.
Return a list of plists (:kind :title :beg :end :path :new-map
:old-map) in document order: step, diff, excerpt and uncovered
sections.  :beg and :end are 1-based document line numbers spanning
the section; :title names step and uncovered sections; :path names
diff and excerpt sections; :new-map and :old-map map document lines
to file lines (diff sections per hunk, excerpt sections sequentially
against the excerpt's first line).  Diff sections carry :index, the
verbatim index line naming both sides' blob hashes."
  (let* ((lines (split-string string "\n"))
         (starts '())
         (i 0))
    ;; find every section start
    (while (< i (length lines))
      (when-let ((start (jg-agent-comments-review--section-start lines i)))
        (push start starts)
        (setq i (plist-get start :body-start)))
      (setq i (1+ i)))
    (setq starts (nreverse starts))
    ;; each section runs to the next one's start, trailing blank lines
    ;; trimmed
    (let ((sections '()))
      (cl-loop for idx from 0
               for start in starts
               do (let* ((next (nth (1+ idx) starts))
                         (limit (if next
                                    (plist-get next :start)
                                  (length lines)))
                         (end limit)
                         (kind (plist-get start :kind)))
                    (while (and (> end (plist-get start :start))
                                (string-empty-p (nth (1- end) lines)))
                      (setq end (1- end)))
                    (let ((section
                           (list :kind kind
                                 :title (plist-get start :title)
                                 :beg (1+ (plist-get start :start))
                                 :end end
                                 :path (plist-get start :path))))
                      (pcase kind
                        (`diff
                         (let* ((maps (jg-agent-comments-review--line-maps
                                       lines
                                       (plist-get start :body-start)
                                       (1- end)))
                                (index (catch 'index
                                         (cl-loop for k from (plist-get start :body-start)
                                                  to (1- end)
                                                  for l = (nth k lines)
                                                  when (string-prefix-p "index " l)
                                                  do (throw 'index l)))))
                           (setq section
                                 (append section
                                         (list :new-map (car maps)
                                               :old-map (cdr maps)
                                               :index index)))))
                        (`excerpt
                         (let ((map '())
                               (file (plist-get start :excerpt-beg)))
                           (cl-loop for j from (plist-get start :body-start)
                                    to (1- end)
                                    do (setq map (cons (cons (1+ j) file) map)
                                             file (1+ file)))
                           (setq section
                                 (append section
                                         (list :new-map (nreverse map)
                                               :old-map nil))))))
                      (push section sections))))
      (nreverse sections))))

(defvar-local jg-agent-comments-review--sections nil
  "Buffer-local cache of this review document's parsed sections.
Set when the mode activates; see `jg-agent-comments-review--parse'.")

(defvar-local jg-agent-comments-review--target nil
  "Buffer-local plist describing this document's review target.
The parse of the document's Target: line; see
`jg-agent-comments-review--target-parse'.")

(defun jg-agent-comments-review--citation-for (beg-line end-line sections target)
  "Return the citation for document lines BEG-LINE to END-LINE.
SECTIONS is the parsed document, TARGET the parse of its Target:
line.  Return nil when the lines name no reviewed content: banner or
prose lines sit in no diff or excerpt section.  A diff selection that
deletes nothing cites the right-side rev with new-side line numbers;
an all-deletion selection cites the left-side rev with old-side line
numbers.  Excerpt selections cite the file on disk, \"@ worktree\".
Worktree and staged targets cite \"@ worktree\" for both sides."
  (let* ((section (seq-find (lambda (s)
                              (and (>= beg-line (plist-get s :beg))
                                   (<= beg-line (plist-get s :end))))
                            sections))
         (kind (plist-get section :kind)))
    (when (memq kind '(diff excerpt))
      (let* ((last-line (min end-line (plist-get section :end)))
             (new-vals '()) (old-vals '()))
        (cl-loop for k from beg-line to last-line
                 do (let ((new (cdr (assq k (plist-get section :new-map))))
                          (old (cdr (assq k (plist-get section :old-map)))))
                      (when new (push new new-vals))
                      (when old (push old old-vals))))
        (let* ((added new-vals)
               (all-deleted (and (null added) old-vals))
               (rev (pcase (plist-get target :kind)
                      ((or 'worktree 'staged 'files) "worktree")
                      ('range (if all-deleted
                                  (plist-get target :left)
                                (plist-get target :right)))
                      ('commit (plist-get target :right))))
               (nums (cond (added new-vals) (all-deleted old-vals))))
          (when nums
            (let ((beg-num (apply #'min nums))
                  (end-num (apply #'max nums)))
              (if (= beg-num end-num)
                  (format "%s:%d @ %s" (plist-get section :path)
                          beg-num rev)
                (format "%s:%d-%d @ %s" (plist-get section :path)
                        beg-num end-num rev)))))))))

(defun jg-agent-comments-review--citation (beg end)
  "Return the citation for the selection between BEG and END.
Positions in the current review buffer.  The value installed as
`jg-agent-comments--review-context' in review buffers."
  (jg-agent-comments-review--citation-for
   (line-number-at-pos beg)
   (line-number-at-pos end)
   jg-agent-comments-review--sections
   jg-agent-comments-review--target))


;;; Review mode

(require 'diff-mode)
(require 'outline)

(defface jg-agent-comments-review-banner
  '((t :inherit 'font-lock-comment-face))
  "Face for the ====== banner lines of a review document.")

(defface jg-agent-comments-review-title
  '((t :inherit 'font-lock-function-name-face :weight bold))
  "Face for a review document's section title lines.")

(defface jg-agent-comments-review-marker
  '((t :inherit 'font-lock-preprocessor-face :weight bold))
  "Face for the --- diff:/excerpt: marker lines of a review document.")

(defun jg-agent-comments-review--diff-line-face ()
  "Return the diff face for the content line in the match data.
Only lines inside a diff section reach this; prose dash lines never
do, because font-lock enters diff regions only through the diff
marker keyword."
  (let ((line (buffer-substring (match-beginning 0) (match-end 0))))
    (cond
     ((string-prefix-p "diff --git" line) 'diff-file-header)
     ((string-prefix-p "index " line) 'diff-header)
     ((string-prefix-p "--- " line) 'diff-header)
     ((string-prefix-p "+++ " line) 'diff-header)
     ((string-prefix-p "@@" line) 'diff-hunk-header)
     ((string-prefix-p "+" line) 'diff-added)
     ((string-prefix-p "-" line) 'diff-removed)
     ((string-prefix-p " " line) 'diff-context)
     (t nil))))

(defun jg-agent-comments-review--match-diff-line (limit)
  "Font-lock matcher: match the next diff-content line up to LIMIT.
A line belongs to a diff section when the nearest section opener
above it is that section's --- diff: marker.  Prose dash lines and
banner lines sit under other openers and never match, whatever they
start with.  Match one line per call, advancing point, until LIMIT."
  (catch 'done
    (while (< (point) limit)
      (let* ((bol (line-beginning-position))
             (eol (line-end-position))
             (line (buffer-substring bol eol)))
        (goto-char (if (= eol (point-max)) eol (1+ eol)))
        (when (and (not (string-empty-p line))
                   (save-excursion
                     (goto-char bol)
                     (re-search-backward
                      "^\\(=\\{2,\\}\\|--- \\(?:diff\\|excerpt\\): \\)"
                      nil t))
                   (string-match-p "^--- diff: " (match-string 0)))
          (set-match-data (list bol eol))
          (throw 'done t))))
    nil))

(defconst jg-agent-comments-review--keywords
  '(("^=+$" . 'jg-agent-comments-review-banner)
    ("^\\(?:[0-9]+/[0-9]+ — \\|Not covered by the walkthrough\\).*$"
     . 'jg-agent-comments-review-title)
    ("^--- \\(?:diff\\|excerpt\\): .* ---$"
     . 'jg-agent-comments-review-marker)
    (jg-agent-comments-review--match-diff-line
     (0 (jg-agent-comments-review--diff-line-face))))
  "Font-lock keywords for review documents.
The last keyword enters a diff section through its marker line and
faces each content line until the next banner or marker; prose dash
lines outside a diff section are never visited by it.")

(defun jg-agent-comments-review-next-section ()
  "Move point to the next section title of the review document."
  (interactive)
  (if (re-search-forward
       "^\\(?:[0-9]+/[0-9]+ — \\|Not covered by the walkthrough\\)" nil t)
      (beginning-of-line)
    (goto-char (point-max))))

(defun jg-agent-comments-review-previous-section ()
  "Move point to the previous section title of the review document."
  (interactive)
  (if (re-search-backward
       "^\\(?:[0-9]+/[0-9]+ — \\|Not covered by the walkthrough\\)" nil t)
      (beginning-of-line)
    (goto-char (point-min))))

(defun jg-agent-comments-review--set-context ()
  "Parse this review document and set its buffer-local context.
Cache the sections and the target, and install the citation function
into `jg-agent-comments--review-context'."
  (setq jg-agent-comments-review--sections
        (jg-agent-comments-review--parse (buffer-string)))
  (save-excursion
    (goto-char (point-min))
    (setq jg-agent-comments-review--target
          (and (re-search-forward "^Target: .*$" nil t)
               (jg-agent-comments-review--target-parse
                (buffer-substring (line-beginning-position)
                                  (line-end-position))))))
  (setq jg-agent-comments--review-context
        #'jg-agent-comments-review--citation))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.review\\'" . jg-agent-comments-review-mode))

(defvar jg-agent-comments-review-mode nil
  "Non-nil in buffers running `jg-agent-comments-review-mode'.")

;;;###autoload
(define-derived-mode jg-agent-comments-review-mode text-mode "Review"
  "Major mode for reading an agent's review document.
A review document interleaves commentary prose with verbatim git
diff output; this mode styles both, folds sections, navigates
between them, and lets `jg-agent-comments-mode' comment on anything
in the document with citations back to the reviewed text."
  (setq-local jg-agent-comments-review-mode t)
  (setq-local font-lock-defaults
              '((jg-agent-comments-review--keywords) nil t))
  (setq-local outline-regexp "^\\(=\\{2,\\}\\|--- \\)")
  (setq-local outline-heading-end-regexp "\n")
  (jg-agent-comments-review--set-context)
  (jg-agent-comments-mode 1)
  (define-key jg-agent-comments-review-mode-map "n"
        #'jg-agent-comments-review-next-section)
  (define-key jg-agent-comments-review-mode-map "p"
        #'jg-agent-comments-review-previous-section)
  (define-key jg-agent-comments-review-mode-map (kbd "TAB")
        #'outline-cycle)
  (define-key jg-agent-comments-review-mode-map (kbd "RET")
        #'jg-agent-comments-review-visit-at-point)
  (define-key jg-agent-comments-review-mode-map "s"
        #'jg-agent-comments-review-send)
  (define-key jg-agent-comments-review-mode-map "q"
        #'jg-agent-comments-review-quit))

;;;###autoload
(defun jg-agent-comments-review-open (path)
  "Open the review document at PATH in the review mode.
The flow the review document is written for: the agent names the
file, the user opens it."
  (interactive "fReview document: ")
  (find-file path))



;;; RET: visit the reviewed content

(defun jg-agent-comments-review--index-hashes (section)
  "Return (LEFT . RIGHT) blob hash prefixes from SECTION's :index.
Nil when the section carries no index line."
  (when-let* ((index (plist-get section :index)))
    (string-match
     "\\`index \\([0-9a-f]+\\)\\.\\.\\([0-9a-f]+\\)" index)
    (cons (match-string 1 index) (match-string 2 index))))

(defun jg-agent-comments-review--hash-object (path)
  "Return the git hash of the file at PATH, or nil when missing."
  (with-temp-buffer
    (and (zerop (call-process "git" nil t nil "hash-object" "--" path))
         (string-trim (buffer-string)))))

(defun jg-agent-comments-review--view-target (section file-line)
  "Decide where to view SECTION's content at FILE-LINE.
Return (worktree PATH LINE) when the working tree file holds the
reviewed content, (blob REV PATH LINE) when the revision's own
content must be shown instead: deleted files, drifted files, and
every target whose content is not the working tree.  PATH is
absolute, resolved against the project root — the review document
itself may live in a subdirectory like .reviews/."
  (let* ((path (plist-get section :path))
         (target jg-agent-comments-review--target)
         (kind (plist-get target :kind))
         (full (expand-file-name path
                                 (jg-agent-comments--project-root))))
    (cond
     ;; excerpts quote the working file: open it whatever the target
     ((eq (plist-get section :kind) 'excerpt)
      (list 'worktree full file-line))
     ((memq kind '(worktree staged))
      (list 'worktree full file-line))
     (t
      (let* ((hashes (jg-agent-comments-review--index-hashes section))
             (right (cdr hashes))
             (live (and right
                        (file-exists-p full)
                        (string-prefix-p right
                                         (jg-agent-comments-review--hash-object
                                          full)))))
        (if live
            (list 'worktree full file-line)
          (list 'blob
                (if (and right (string-prefix-p "0000000" right))
                    (plist-get target :left)
                  (plist-get target :right))
                full file-line)))))))

(defun jg-agent-comments-review--open-blob (rev path)
  "Open REV's PATH in a read-only review blob buffer.
The buffer visits the revision's own content — deleted files,
drifted files — and its review context cites PATH's line numbers at
REV, so comments made there cite the blob."
  (let* ((root (jg-agent-comments--project-root))
         (rel (let ((r (file-relative-name path root)))
                ;; git rejects absolute paths in rev:path; fall back to
                ;; the raw path when it lives outside the project
                (if (string-prefix-p ".." r) path r)))
         (buffer (get-buffer-create
                  (format "*review %s:%s*" rev path))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (shell-command-to-string
          (concat "git show "
                  (shell-quote-argument rev)
                  ":"
                  (shell-quote-argument rel)))))
      (jg-agent-comments-mode 1)
      (setq-local jg-agent-comments--review-context
                  (lambda (beg end)
                    (let ((b (line-number-at-pos beg))
                          (e (line-number-at-pos end)))
                      (if (= b e)
                          (format "%s:%d @ %s" path b rev)
                        (format "%s:%d-%d @ %s" path b e rev)))))
      (read-only-mode 1))
    (pop-to-buffer buffer)
    buffer))

(defun jg-agent-comments-review--file-line-at-point ()
  "Return (SECTION . FILE-LINE) for the content under point.
Signal a user error when point names no reviewed content: banner and
prose lines, and diff header lines, have no file line to visit."
  (let* ((doc-line (line-number-at-pos))
         (section (seq-find
                   (lambda (s)
                     (and (memq (plist-get s :kind) '(diff excerpt))
                          (>= doc-line (plist-get s :beg))
                          (<= doc-line (plist-get s :end))))
                   jg-agent-comments-review--sections)))
    (unless section
      (user-error "Nothing to view here"))
    (let ((file-line (or (cdr (assq doc-line
                                    (plist-get section :new-map)))
                         (cdr (assq doc-line
                                    (plist-get section :old-map))))))
      (unless file-line
        (user-error "Nothing to view here"))
      (cons section file-line))))

(defun jg-agent-comments-review-visit-at-point ()
  "View the reviewed content under point.
Open the working tree file at that line when it holds the reviewed
content; otherwise open the revision's own blob read-only."
  (interactive)
  (pcase-let ((`(,section . ,file-line)
               (jg-agent-comments-review--file-line-at-point)))
    (pcase (jg-agent-comments-review--view-target section file-line)
      (`(worktree ,path ,line)
       (pop-to-buffer (find-file-noselect path))
       (goto-char (point-min))
       (forward-line (1- line)))
      (`(blob ,rev ,path ,line)
       (jg-agent-comments-review--open-blob rev path)
       (goto-char (point-min))
       (forward-line (1- line))))))


;;; Send

(defun jg-agent-comments-review--shell-buffer ()
  "Return the agent-shell buffer of this review document's project.
The most recent live shell wins; ask when several are live, and
error when this project has none."
  (let ((root (jg-agent-comments--project-root)))
    (pcase (seq-filter
            (lambda (buffer)
              (with-current-buffer buffer
                (and (derived-mode-p 'agent-shell-mode)
                     (jg-agent-comments--same-project-p root))))
            (buffer-list))
      (`(,shell) shell)
      (`() (user-error "No agent-shell buffer for this project"))
      (shells (get-buffer
               (completing-read "Send review comments to: "
                                (mapcar #'buffer-name shells)))))))

(defun jg-agent-comments-review-send ()
  "Send this review's pending comments to the agent, then clear.
Compose this buffer's comments plus same-project ride-along file
comments into one prompt, queue it into the project's agent-shell
buffer, and clear every contributed comment.  Say so and do nothing
when nothing is pending here or in same-project file buffers."
  (interactive)
  (if (and (null jg-agent-comments--comments)
           (null (jg-agent-comments--collect)))
      (message "No pending comments")
    (let ((prompt (jg-agent-comments--augment-prompt nil))
          (shell (jg-agent-comments-review--shell-buffer)))
      (with-current-buffer shell
        (agent-shell-prompt-queue prompt))
      (jg-agent-comments--clear))))

(defun jg-agent-comments-review-quit ()
  "Quit the review window, warning when comments are pending."
  (interactive)
  (when (and jg-agent-comments--comments
             (not (y-or-n-p "Pending comments; quit anyway? ")))
    (user-error "Cancelled"))
  (quit-window))

(provide 'jg-agent-comments-review)
;;; jg-agent-comments-review.el ends here

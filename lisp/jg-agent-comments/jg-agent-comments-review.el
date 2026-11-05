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
  "Return the path in LINE when it is a diff marker, else nil."
  (and (string-match "\\`--- diff: \\(.*\\) ---\\'" line)
       (match-string 1 line)))

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
                 ((string-prefix-p "+" line)
                  (setq new-map (cons (cons (1+ i) new) new-map))
                  (setq new (1+ new)))
                 ((string-prefix-p "-" line)
                  (setq old-map (cons (cons (1+ i) old) old-map))
                  (setq old (1+ old)))
                 ((string-prefix-p " " line)
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
against the excerpt's first line)."
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
                         (let ((maps (jg-agent-comments-review--line-maps
                                      lines
                                      (plist-get start :body-start)
                                      (1- end))))
                           (setq section
                                 (append section
                                         (list :new-map (car maps)
                                               :old-map (cdr maps))))))
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

(provide 'jg-agent-comments-review)
;;; jg-agent-comments-review.el ends here

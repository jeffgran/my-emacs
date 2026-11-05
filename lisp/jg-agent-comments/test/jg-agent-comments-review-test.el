;;; jg-agent-comments-review-test.el --- Tests for jg-agent-comments-review -*- lexical-binding: t; -*-

(require 'jg-agent-comments-review)

(defconst jg-agent-comments-review--sample-artifact
  "Target: git diff aaa..bbb

==============================================================
1/1 — The Subject
==============================================================
Prose about the change.
- a bullet that starts with dash

--- diff: some dir/file with spaces.el ---
diff --git a/some dir/file with spaces.el b/some dir/file with spaces.el
index 0000001..0000002 100644
--- a/some dir/file with spaces.el
+++ b/some dir/file with spaces.el
@@ -1,1 +7,3 @@
+added one
+added two
 kept
@@ -10,2 +12,0 @@
-removed a
-removed b

--- excerpt: plain.el :12-14 ---
  (defun plain ())
  ;; body line
  (plain))
"
  "A minimal review document exercising every grammar element:
a range target, a step section whose prose contains a dash line, a
two-hunk diff section for a path with spaces, and an excerpt section.")

(defun jg-agent-comments-review-test--line-of (needle)
  "Return the 1-based number of the first line of the sample artifact
containing NEEDLE."
  (let ((lines (split-string jg-agent-comments-review--sample-artifact "\n"))
        (n 0))
    (catch 'found
      (dolist (line lines)
        (setq n (1+ n))
        (when (string-match-p (regexp-quote needle) line)
          (throw 'found n))))))

(ert-deftest jg-agent-comments-review-parse-sections ()
  (let ((sections (jg-agent-comments-review--parse
                   jg-agent-comments-review--sample-artifact)))
    (should (= (length sections) 3))
    (should (eq (plist-get (nth 0 sections) :kind) 'step))
    (should (eq (plist-get (nth 1 sections) :kind) 'diff))
    (should (equal (plist-get (nth 1 sections) :path)
                   "some dir/file with spaces.el"))
    (should (eq (plist-get (nth 2 sections) :kind) 'excerpt))))

(ert-deftest jg-agent-comments-review-parse-diff-line-maps ()
  ;; the first `+' content line maps to the new-side line from the @@
  ;; header (7); the context line maps to the old-side line 1; the
  ;; second hunk's first `-` line maps to old-side 10
  (let* ((sections (jg-agent-comments-review--parse
                    jg-agent-comments-review--sample-artifact))
         (section (nth 1 sections))
         (diff-first-doc-line
          (jg-agent-comments-review-test--line-of "+added one"))
         (ctx-doc-line
          (jg-agent-comments-review-test--line-of " kept"))
         (del-doc-line
          (jg-agent-comments-review-test--line-of "-removed a")))
    (should (equal (cdr (assq diff-first-doc-line
                              (plist-get section :new-map)))
                   7))
    (should (equal (cdr (assq ctx-doc-line
                              (plist-get section :old-map)))
                   1))
    (should (equal (cdr (assq del-doc-line
                              (plist-get section :old-map)))
                   10))))

(ert-deftest jg-agent-comments-review-parse-prose-dash-line-is-not-diff ()
  ;; the prose section's "- bullet" line must not open a diff section
  (let ((sections (jg-agent-comments-review--parse
                   jg-agent-comments-review--sample-artifact)))
    (should (seq-every-p (lambda (s) (not (equal (plist-get s :path)
                                                 "bullet")))
                         sections))))

(ert-deftest jg-agent-comments-review-parse-path-with-spaces ()
  (let ((sections (jg-agent-comments-review--parse
                   jg-agent-comments-review--sample-artifact)))
    (should (equal (plist-get (nth 2 sections) :path) "plain.el"))))

(ert-deftest jg-agent-comments-review-target-parse-range ()
  (should (equal (jg-agent-comments-review--target-parse
                  "Target: git diff aaa..bbb")
                 (list :kind 'range :left "aaa" :right "bbb" :paths nil))))

(ert-deftest jg-agent-comments-review-citation-new-side ()
  ;; a selection over the two `+' lines cites the right-side rev with
  ;; the new-side line numbers
  (let* ((sections (jg-agent-comments-review--parse
                    jg-agent-comments-review--sample-artifact))
         (target (list :kind 'range :left "aaa" :right "bbb" :paths nil))
         (new-beg (jg-agent-comments-review-test--line-of "+added one"))
         (new-end (jg-agent-comments-review-test--line-of "+added two")))
    (should (equal (jg-agent-comments-review--citation-for
                    new-beg new-end sections target)
                   "some dir/file with spaces.el:7-8 @ bbb"))))

(ert-deftest jg-agent-comments-review-citation-deleted-lines-use-left-rev ()
  (let* ((sections (jg-agent-comments-review--parse
                    jg-agent-comments-review--sample-artifact))
         (target (list :kind 'range :left "aaa" :right "bbb" :paths nil))
         (del-beg (jg-agent-comments-review-test--line-of "-removed a"))
         (del-end (jg-agent-comments-review-test--line-of "-removed b")))
    (should (equal (jg-agent-comments-review--citation-for
                    del-beg del-end sections target)
                   "some dir/file with spaces.el:10-11 @ aaa"))))

(ert-deftest jg-agent-comments-review-citation-mixed-selection-uses-new-side ()
  (let* ((sections (jg-agent-comments-review--parse
                    jg-agent-comments-review--sample-artifact))
         (target (list :kind 'range :left "aaa" :right "bbb" :paths nil))
         (mixed-beg (jg-agent-comments-review-test--line-of "+added one"))
         (mixed-end (jg-agent-comments-review-test--line-of "-removed a")))
    (should (equal (jg-agent-comments-review--citation-for
                    mixed-beg mixed-end sections target)
                   "some dir/file with spaces.el:7-9 @ bbb"))))

(ert-deftest jg-agent-comments-review-citation-excerpt-uses-worktree ()
  (let* ((sections (jg-agent-comments-review--parse
                    jg-agent-comments-review--sample-artifact))
         (target (list :kind 'files :left nil :right nil :paths '("plain.el")))
         (exc-beg (jg-agent-comments-review-test--line-of "(defun plain ())"))
         (exc-end (jg-agent-comments-review-test--line-of "(plain))")))
    (should (equal (jg-agent-comments-review--citation-for
                    exc-beg exc-end sections target)
                   "plain.el:12-14 @ worktree"))))

(ert-deftest jg-agent-comments-review-citation-nil-outside-content ()
  ;; prose lines and banner lines sit in no content section: nil
  (let* ((sections (jg-agent-comments-review--parse
                    jg-agent-comments-review--sample-artifact))
         (target (list :kind 'range :left "aaa" :right "bbb" :paths nil))
         (banner-beg (jg-agent-comments-review-test--line-of "1/1 — The Subject"))
         (banner-end banner-beg))
    (should (null (jg-agent-comments-review--citation-for
                   banner-beg banner-end sections target)))
    (let ((prose (jg-agent-comments-review-test--line-of
                  "Prose about the change")))
      (should (null (jg-agent-comments-review--citation-for
                     prose prose sections target))))))

(ert-deftest jg-agent-comments-review-mode-enables-comment-mode ()
  (with-temp-buffer
    (insert jg-agent-comments-review--sample-artifact)
    (jg-agent-comments-review-mode)
    (should jg-agent-comments-review-mode)
    (should jg-agent-comments-mode)
    (should (functionp jg-agent-comments--review-context))))

(ert-deftest jg-agent-comments-review-mode-caches-sections ()
  (with-temp-buffer
    (insert jg-agent-comments-review--sample-artifact)
    (jg-agent-comments-review-mode)
    (should (= (length jg-agent-comments-review--sections) 3))))

(ert-deftest jg-agent-comments-review-mode-sets-target ()
  (with-temp-buffer
    (insert jg-agent-comments-review--sample-artifact)
    (jg-agent-comments-review-mode)
    (should (eq (plist-get jg-agent-comments-review--target :kind) 'range))
    (should (equal (plist-get jg-agent-comments-review--target :right) "bbb"))))

(ert-deftest jg-agent-comments-review-navigation-moves-between-titles ()
  (with-temp-buffer
    (insert jg-agent-comments-review--sample-artifact)
    (jg-agent-comments-review-mode)
    (goto-char (point-min))
    (jg-agent-comments-review-next-section)
    (should (looking-at "^\\([0-9]+/[0-9]+\\|Not covered\\)"))
    (jg-agent-comments-review-previous-section)
    (should (= (point) (point-min)))))

(ert-deftest jg-agent-comments-review-citation-context-cites-diff-line ()
  ;; the buffer's review context, called on a `+' line, returns the
  ;; citation the engine will record
  (with-temp-buffer
    (insert jg-agent-comments-review--sample-artifact)
    (jg-agent-comments-review-mode)
    (goto-char (point-min))
    (forward-line (1- (jg-agent-comments-review-test--line-of "+added one")))
    (should (equal (funcall jg-agent-comments--review-context
                            (point) (line-end-position))
                   "some dir/file with spaces.el:7 @ bbb"))))

;;; RET: visit working file or blob

(defun jg-agent-comments-review-test--git (repo &rest args)
  "Run git with ARGS in REPO, return stdout, error on failure."
  (let ((default-directory repo))
    (shell-command-to-string
     (concat "git " (mapconcat #'identity args " ")))))

(defun jg-agent-comments-review-test--make-repo ()
  "Create a temp git repo with a fixture.el and a deleted gone.el.
Return the repo directory.  fixture.el has two committed revisions;
gone.el existed in the first commit and the second commit deletes it.
Repos live under the repo's git-ignored var/ directory: git refuses
sandboxed writes outside it."
  (let* ((root (or (locate-dominating-file
                    (or load-file-name default-directory) ".git")
                   default-directory))
         (scratch (file-name-as-directory
                   (expand-file-name "var/tmp/jg-agent-comments-review-test"
                                     root)))
         (dir (progn (make-directory scratch t)
                     (file-name-as-directory
                      (make-temp-file (expand-file-name "repo-" scratch)
                                      t))))
         (default-directory dir))
    (call-process "git" nil nil nil "init")
    (call-process "git" nil nil nil "config" "user.email" "t@t")
    (call-process "git" nil nil nil "config" "user.name" "t")
    (with-temp-file (expand-file-name "fixture.el" dir)
      (insert "(defun fixture ())\n"))
    (with-temp-file (expand-file-name "gone.el" dir)
      (insert "(defun gone ())\n"))
    (call-process "git" nil nil nil "add" "-A")
    (call-process "git" nil nil nil "commit" "-m" "first")
    (with-temp-file (expand-file-name "fixture.el" dir)
      (insert "(defun fixture ())\n\n(defun fixture-two ())\n"))
    (call-process "git" nil nil nil "rm" "--cached" "gone.el")
    (call-process "git" nil nil nil "add" "-A")
    (call-process "git" nil nil nil "commit" "-m" "second")
    dir))

(ert-deftest jg-agent-comments-review-visit-unchanged-file-uses-worktree ()
  ;; fixture.el is unchanged in the worktree at the diff's right rev:
  ;; a section whose index line's right blob matches the file opens
  ;; the worktree file
  (let* ((repo (jg-agent-comments-review-test--make-repo))
         (head (string-trim
                (jg-agent-comments-review-test--git repo "rev-parse" "HEAD")))
         (artifact (concat "Target: git diff " head ".." head "\n"))
         (fixture-path (expand-file-name "fixture.el" repo))
         (hash (let ((default-directory repo))
                 (string-trim
                  (shell-command-to-string "git hash-object fixture.el"))))
         (doc (concat artifact "
==============================================================
1/1 — t
==============================================================
prose

--- diff: fixture.el ---
diff --git a/fixture.el b/fixture.el
index 0000001.." (substring hash 0 7) " 100644
--- a/fixture.el
+++ b/fixture.el
@@ -1,1 +1,3 @@
 (defun fixture ())
+added
+
"))
         (sections (jg-agent-comments-review--parse doc)))
    (unwind-protect
        (let ((default-directory repo)
              (jg-agent-comments-review--target
               (list :kind 'range :left head :right head :paths nil)))
          (should (equal (jg-agent-comments-review--view-target
                          (nth 1 sections) 1)
                         (list 'worktree fixture-path 1))))
      (delete-directory repo t))))

(ert-deftest jg-agent-comments-review-visit-deleted-file-opens-blob ()
  ;; gone.el is missing from the worktree: its blob view it is
  (let* ((repo (jg-agent-comments-review-test--make-repo))
         (head (string-trim
                (jg-agent-comments-review-test--git repo "rev-parse" "HEAD")))
         (first (string-trim
                 (jg-agent-comments-review-test--git repo "rev-parse" "HEAD~1")))
         (blob (string-trim
                (jg-agent-comments-review-test--git
                 repo "rev-parse" (concat head ":gone.el"))))
         (doc (concat "Target: git diff " first ".." head "\n"
"
==============================================================
1/1 — t
==============================================================
prose

--- diff: gone.el ---
diff --git a/gone.el b/gone.el
deleted file mode 100644
index " (substring blob 0 7) "..0000000
--- a/gone.el
+++ /dev/null
@@ -1,1 +0,0 @@
-(defun gone ())
"))
         (sections (jg-agent-comments-review--parse doc)))
    (unwind-protect
        (let ((default-directory repo)
              (jg-agent-comments-review--target
               (list :kind 'range :left first :right head :paths nil)))
          (should (eq (car (jg-agent-comments-review--view-target
                            (nth 1 sections) 1))
                      'blob)))
      (delete-directory repo t))))

(ert-deftest jg-agent-comments-review-visit-drifted-file-opens-blob ()
  ;; fixture.el drifts in the worktree after the diff was taken: blob
  (let* ((repo (jg-agent-comments-review-test--make-repo))
         (head (string-trim
                (jg-agent-comments-review-test--git repo "rev-parse" "HEAD")))
         (hash (string-trim
                (shell-command-to-string "git hash-object fixture.el")))
         (doc (concat "Target: git diff " head ".." head "\n"
"
==============================================================
1/1 — t
==============================================================
prose

--- diff: fixture.el ---
diff --git a/fixture.el b/fixture.el
index 0000001.." (substring hash 0 7) " 100644
--- a/fixture.el
+++ b/fixture.el
@@ -1,1 +1,3 @@
 (defun fixture ())
+added
+
"))
         (sections (jg-agent-comments-review--parse doc)))
    (unwind-protect
        (let ((default-directory repo)
              (jg-agent-comments-review--target
               (list :kind 'range :left head :right head :paths nil)))
          ;; drift: append a line to the worktree file
          (with-temp-file (expand-file-name "fixture.el" repo)
            (insert "(defun fixture ())\n\n(defun fixture-two ())\n\ndrifted\n"))
          (should (eq (car (jg-agent-comments-review--view-target
                            (nth 1 sections) 1))
                      'blob)))
      (delete-directory repo t))))

(provide 'jg-agent-comments-review-test)
;;; jg-agent-comments-review-test.el ends here

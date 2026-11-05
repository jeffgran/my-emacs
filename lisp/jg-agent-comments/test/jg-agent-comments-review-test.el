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

(provide 'jg-agent-comments-review-test)
;;; jg-agent-comments-review-test.el ends here

;;; jg-agent-comments-test.el --- Tests for jg-agent-comments -*- lexical-binding: t; -*-

(ert-deftest jg-agent-comments-mode-enables ()
  (with-temp-buffer
    (jg-agent-comments-mode 1)
    (should jg-agent-comments-mode)))

(ert-deftest jg-agent-comments-add-captures-invisible-text ()
  (with-temp-buffer
    (insert "visible\n")
    (insert "hidden-under-fold\n")
    (insert "visible-after\n")
    (put-text-property 9 25 'invisible t)
    (jg-agent-comments--add 1 (point-max) "why?")
    (should (equal (plist-get (car jg-agent-comments--comments) :snippet)
                   "visible\nhidden-under-fold\nvisible-after\n"))))

(ert-deftest jg-agent-comments-add-stores-comment-text ()
  (with-temp-buffer
    (insert "some agent prose")
    (jg-agent-comments--add 1 (point-max) "line one\nline two")
    (should (equal (plist-get (car jg-agent-comments--comments) :text)
                   "line one\nline two"))))

(ert-deftest jg-agent-comments-anchor-survives-region-deletion ()
  (with-temp-buffer
    (insert "anchor text here")
    (let ((record (jg-agent-comments--add 1 (point-max) "note")))
      (delete-region 1 (point-max))
      (should (member record jg-agent-comments--comments))
      (should (null (overlay-buffer (plist-get record :overlay)))))))

(ert-deftest jg-agent-comments-refuses-non-agent-text ()
  (with-temp-buffer
    (insert "user typed input")
    (should-error (jg-agent-comments--region-or-line))))

(ert-deftest jg-agent-comments-accepts-agent-output ()
  (with-temp-buffer
    (insert "agent said things")
    (put-text-property (point-min) (point-max) 'field 'output)
    (should (jg-agent-comments--agent-text-at-point-p))))

(ert-deftest jg-agent-comments-empty-commit-cancels ()
  (with-temp-buffer
    (insert "agent prose")
    (put-text-property (point-min) (point-max) 'field 'output)
    (jg-agent-comments--commit-edit-from-string "" 1 (point-max))
    (should (null jg-agent-comments--comments))))

(ert-deftest jg-agent-comments-inline-elides-multiline ()
  (should (equal (jg-agent-comments--inline-string "first\nsecond\nthird")
                 "first …"))
  (should (equal (jg-agent-comments--inline-string "only")
                 "only")))

(ert-deftest jg-agent-comments-render-strings-are-single-line ()
  (with-temp-buffer
    (insert "anchor")
    (let ((record (jg-agent-comments--add 1 (point-max) "a\nb")))
      (let ((ov (plist-get record :overlay)))
        (should (not (string-match-p "\n" (or (overlay-get ov 'before-string) ""))))
        (should (not (string-match-p "\n" (or (overlay-get ov 'after-string) ""))))))))

(provide 'jg-agent-comments-test)
;;; jg-agent-comments-test.el ends here

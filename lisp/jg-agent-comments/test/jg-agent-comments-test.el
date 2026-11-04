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

(ert-deftest jg-agent-comments-mode-line-empty-when-none ()
  (with-temp-buffer
    (should (equal (jg-agent-comments--mode-line-string) ""))))

(ert-deftest jg-agent-comments-mode-line-counts ()
  (with-temp-buffer
    (insert "agent prose")
    (jg-agent-comments--add 1 (point-max) "one")
    (jg-agent-comments--add 1 (point-max) "two")
    (should (equal (jg-agent-comments--mode-line-string) " [2 comments]"))))

(ert-deftest jg-agent-comments-compose-quotes-snippet-then-comment ()
  (with-temp-buffer
    (insert "agent prose one")
    (jg-agent-comments--add 1 (point-max) "why?\nexplain")
    (insert "\nagent prose two")   ; buffer is now "agent prose one\nagent prose two"
    (jg-agent-comments--add 17 (point-max) "ok")
    (should (equal (jg-agent-comments--compose)
                   "> agent prose one\n  why?\n  explain\n\n> agent prose two\n  ok"))))

(ert-deftest jg-agent-comments-send-uses-prompt-queue-and-clears ()
  (with-temp-buffer
    (insert "agent prose")
    (jg-agent-comments--add 1 (point-max) "note")
    (let ((submitted nil))
      (cl-letf (((symbol-function 'agent-shell-prompt-queue)
                 (lambda (prompt) (setq submitted prompt))))
        (jg-agent-comments-send))
      (should (string-prefix-p "> agent prose" submitted))
      (should (null jg-agent-comments--comments)))))

(ert-deftest jg-agent-comments-send-without-comments-is-noop ()
  (with-temp-buffer
    (should (progn (jg-agent-comments-send) t))))

(ert-deftest jg-agent-comments-delete-at-point-removes-record ()
  (with-temp-buffer
    (insert "agent prose")
    (let ((record (jg-agent-comments--add 1 (point-max) "note")))
      (goto-char 1)
      (jg-agent-comments-delete-at-point)
      (should (null jg-agent-comments--comments))
      (should (null (overlay-buffer (plist-get record :overlay)))))))

(provide 'jg-agent-comments-test)
;;; jg-agent-comments-test.el ends here

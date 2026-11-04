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

(ert-deftest jg-agent-comments-add-captures-property-free-snippet ()
  (with-temp-buffer
    (insert "alphabravocharliedelta")
    (put-text-property 6 11 'invisible t)
    (put-text-property 11 18 'field 'output)
    (jg-agent-comments--add 1 (point-max) "why?")
    (let ((snippet (plist-get (car jg-agent-comments--comments) :snippet)))
      (should (equal snippet "alphabravocharliedelta"))
      (should-not (text-property-any 0 (length snippet) 'invisible t snippet))
      (should-not (text-property-any 0 (length snippet) 'field 'output snippet)))))

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

(ert-deftest jg-agent-comments-covered-text-deletion-evaporates-anchor ()
  (with-temp-buffer
    (insert "prose line\nnext line\n")
    (let ((record (jg-agent-comments--add 12 22 "note")))
      (delete-region 12 21)
      (should (member record jg-agent-comments--comments))
      (should (null (overlay-buffer (plist-get record :overlay)))))))

(ert-deftest jg-agent-comments-refuses-non-agent-text ()
  "A plain comint buffer without agent output refuses to comment."
  (with-temp-buffer
    (comint-mode)
    (insert "user typed input")
    (let ((err (should-error (jg-agent-comments--region-or-line)
                             :type 'user-error)))
      (should (equal (error-message-string err) "Not on agent output")))))

(ert-deftest jg-agent-comments-refuses-live-prompt-input ()
  (with-temp-buffer
    (comint-mode)
    (insert "Claude> hello")
    (put-text-property 1 9 'field 'output)
    (let ((prompt-start (copy-marker 1))
          (prompt-end (copy-marker 8)))
      (unwind-protect
          (let ((comint-last-prompt (cons prompt-start prompt-end)))
            (goto-char 9)
            (should-error (jg-agent-comments--region-or-line))
            (goto-char 8)
            (should-error (jg-agent-comments--region-or-line)))
        (set-marker prompt-start nil)
        (set-marker prompt-end nil)))))

(ert-deftest jg-agent-comments-comments-work-on-file-buffer-line ()
  "A comment command run on a file-buffer line records a comment.
The commentable gate does not apply its agent-output rules outside
comint-derived buffers."
  (jg-agent-comments-test--with-file-buffer "one\ntwo\nthree"
    (jg-agent-comments-mode 1)
    (goto-char (point-min))
    (forward-line 1)
    (let ((bounds (jg-agent-comments--region-or-line)))
      (jg-agent-comments--add (car bounds) (cdr bounds) "line note"))
    (should (= (length jg-agent-comments--comments) 1))))

(ert-deftest jg-agent-comments-comments-work-on-file-buffer-region ()
  "A comment command run on a multi-line region in a file buffer
records a comment."
  (jg-agent-comments-test--with-file-buffer "one\ntwo\nthree"
    (jg-agent-comments-mode 1)
    (goto-char (point-min))
    (let ((transient-mark-mode t))
      (push-mark (point-max) nil t))
    (let ((bounds (jg-agent-comments--region-or-line)))
      (jg-agent-comments--add (car bounds) (cdr bounds) "region note"))
    (should (= (length jg-agent-comments--comments) 1))
    (should (equal (plist-get (car jg-agent-comments--comments) :text)
                   "region note"))))

(ert-deftest jg-agent-comments-comments-work-on-file-buffer-last-line ()
  "A comment command run on the last line of a file buffer records a
comment."
  (jg-agent-comments-test--with-file-buffer "one\ntwo\nthree"
    (jg-agent-comments-mode 1)
    (goto-char (point-max))
    (let ((bounds (jg-agent-comments--region-or-line)))
      (jg-agent-comments--add (car bounds) (cdr bounds) "last note"))
    (should (= (length jg-agent-comments--comments) 1))))

(ert-deftest jg-agent-comments-global-mode-enables-file-buffers ()
  "The global mode turns the buffer mode on in file-visiting buffers,
and off again when disabled."
  (jg-agent-comments-test--with-file-buffer "file prose"
    (jg-agent-comments-global-mode 1)
    (should jg-agent-comments-mode)
    (jg-agent-comments-global-mode -1)
    (should-not jg-agent-comments-mode)))

(ert-deftest jg-agent-comments-global-mode-skips-non-file-buffers ()
  "The global mode leaves buffers that visit no file alone, so chat
buffers keep getting the mode only through the agent-shell hook."
  (with-temp-buffer
    (jg-agent-comments-global-mode 1)
    (should-not jg-agent-comments-mode)
    (jg-agent-comments-global-mode -1)
    (should-not jg-agent-comments-mode)))

(ert-deftest jg-agent-comments-add-records-relative-file-and-line ()
  "A comment on line 3 of a temp file records the file path relative
to the project root and the anchor line numbers."
  (jg-agent-comments-test--with-file-buffer "one\ntwo\nthree"
    (goto-char (point-min))
    (forward-line 2)
    (let ((record (jg-agent-comments--add (pos-bol) (pos-eol) "note")))
      (should (equal (plist-get record :file)
                     (file-name-nondirectory file)))
      (should (= (plist-get record :line-beg) 3))
      (should (= (plist-get record :line-end) 3)))))

(ert-deftest jg-agent-comments-add-records-line-range ()
  "A comment over a region spanning lines records first and last line."
  (jg-agent-comments-test--with-file-buffer "one\ntwo\nthree"
    (let ((record (jg-agent-comments--add 1 (point-max) "note")))
      (should (= (plist-get record :line-beg) 1))
      (should (= (plist-get record :line-end) 3)))))

(ert-deftest jg-agent-comments-add-chat-records-no-file ()
  "A chat-buffer comment records no file and no line numbers."
  (with-temp-buffer
    (insert "agent prose")
    (let ((record (jg-agent-comments--add 1 (point-max) "note")))
      (should (plist-member record :file))
      (should (null (plist-get record :file)))
      (should (plist-member record :line-beg))
      (should (null (plist-get record :line-beg)))
      (should (plist-member record :line-end))
      (should (null (plist-get record :line-end))))))

(ert-deftest jg-agent-comments-compose-file-entry-single-line ()
  "A file comment composes with its location line, then the snippet
and comment block."
  (jg-agent-comments-test--with-file-buffer "alpha prose"
    (jg-agent-comments--add 1 (point-max) "note")
    (should (equal (jg-agent-comments--compose)
                   (concat "Inline comments on files:\n\n"
                           (file-name-nondirectory file)
                           ":1\n> alpha prose\n  note")))))

(ert-deftest jg-agent-comments-compose-file-entry-range ()
  "A file comment spanning lines composes with a path:beg-end location."
  (jg-agent-comments-test--with-file-buffer "one\ntwo\nthree"
    (jg-agent-comments--add 1 (point-max) "note")
    (should (equal (jg-agent-comments--compose)
                   (concat "Inline comments on files:\n\n"
                           (file-name-nondirectory file)
                           ":1-3\n> one\n> two\n> three\n  note")))))

(ert-deftest jg-agent-comments-compose-file-entries-group-by-path ()
  "File comments group per file in ascending path order, regardless of
the order the comments were added in."
  (with-temp-buffer
    (insert "prose one")
    (let ((b (jg-agent-comments--add 1 (point-max) "note b")))
      (plist-put b :file "lib/b.js")
      (plist-put b :line-beg 7)
      (plist-put b :line-end 7))
    (let ((a (jg-agent-comments--add 1 (point-max) "note a")))
      (plist-put a :file "lib/a.js")
      (plist-put a :line-beg 3)
      (plist-put a :line-end 5))
    (should (equal (jg-agent-comments--compose)
                   "Inline comments on files:\n\nlib/a.js:3-5\n> prose one\n  note a\n\nlib/b.js:7\n> prose one\n  note b"))))

(ert-deftest jg-agent-comments-compose-mixes-chat-and-file-sections ()
  "Chat comments compose first, then the file section follows."
  (with-temp-buffer
    (insert "chat prose")
    (jg-agent-comments--add 1 (point-max) "chat note")
    (let ((file-record (jg-agent-comments--add 1 (point-max) "file note")))
      (plist-put file-record :file "lib/foo.js")
      (plist-put file-record :line-beg 3)
      (plist-put file-record :line-end 4)
      (should (equal (jg-agent-comments--compose)
                     "> chat prose\n  chat note\n\nInline comments on files:\n\nlib/foo.js:3-4\n> chat prose\n  file note")))))

(ert-deftest jg-agent-comments-compose-file-only-has-no-chat-header ()
  "A buffer holding only file comments composes without the chat
block header."
  (jg-agent-comments-test--with-file-buffer "file prose"
    (jg-agent-comments--add 1 (point-max) "note")
    (should (equal (jg-agent-comments--compose)
                   (concat "Inline comments on files:\n\n"
                           (file-name-nondirectory file)
                           ":1\n> file prose\n  note")))))

(ert-deftest jg-agent-comments-compose-file-dead-anchor-keeps-location ()
  "A dead-anchor file comment composes with its stored location."
  (jg-agent-comments-test--with-file-buffer "anchor text"
    (let ((record (jg-agent-comments--add 1 (point-max) "note")))
      (delete-overlay (plist-get record :overlay))
      (should (equal (jg-agent-comments--compose)
                     (concat "Inline comments on files:\n\n"
                             (file-name-nondirectory file)
                             ":1\n> anchor text\n  note"))))))

(ert-deftest jg-agent-comments-ride-along-augments-and-clears-same-project ()
  "A chat submission collects the pending file comments of same-project
file buffers into the prompt, and clearing reaches into those buffers."
  (let ((project-dir (make-temp-file "jg-agent-comments-ride" 'dir)))
    (unwind-protect
        (let ((alpha (find-file-noselect
                      (expand-file-name "alpha.js" project-dir)))
              (beta (find-file-noselect
                     (expand-file-name "beta.js" project-dir))))
          (with-current-buffer alpha
            (insert "alpha prose")
            (jg-agent-comments-mode 1)
            (jg-agent-comments--add (point-min) (point-max) "alpha note"))
          (with-current-buffer beta
            (insert "beta prose")
            (jg-agent-comments-mode 1)
            (jg-agent-comments--add (point-min) (point-max) "beta note"))
          (let ((alpha-record (car (buffer-local-value
                                    'jg-agent-comments--comments alpha)))
                (beta-record (car (buffer-local-value
                                   'jg-agent-comments--comments beta))))
            (let ((default-directory project-dir))
              (jg-agent-comments-test--with-input "my question"
                (jg-agent-comments--add (point-min) prose-end "chat note")
                (jg-agent-comments-mode 1)
                (should (equal (jg-agent-comments--augment-prompt "my question")
                               (concat "my question\n\n---\n"
                                       "Inline comments replying to specific parts of your previous message:\n"
                                       "> agent prose\n  chat note\n\n"
                                       "Inline comments on files:\n\n"
                                       "alpha.js:1\n> alpha prose\n  alpha note\n\n"
                                       "beta.js:1\n> beta prose\n  beta note")))
                (jg-agent-comments--clear)
                (should (null jg-agent-comments--comments))
                (with-current-buffer alpha
                  (should (null jg-agent-comments--comments))
                  (should (null (overlay-buffer
                                 (plist-get alpha-record :overlay)))))
                (with-current-buffer beta
                  (should (null jg-agent-comments--comments))
                  (should (null (overlay-buffer
                                 (plist-get beta-record :overlay)))))))))
      (dolist (name '("alpha.js" "beta.js"))
        (let ((buffer (find-buffer-visiting
                       (expand-file-name name project-dir))))
          (when buffer
            (with-current-buffer buffer
              (set-buffer-modified-p nil))
            (let (kill-buffer-query-functions)
              (kill-buffer buffer)))))
      (ignore-errors (delete-directory project-dir 'recursive)))))

(ert-deftest jg-agent-comments-ride-along-ignores-other-projects ()
  "A file buffer whose project root differs contributes nothing and
keeps its comments when the chat clears."
  (let ((project-dir (make-temp-file "jg-agent-comments-ride" 'dir))
        (other-dir (make-temp-file "jg-agent-comments-other" 'dir)))
    (unwind-protect
        (let ((outsider (find-file-noselect
                         (expand-file-name "outsider.js" other-dir))))
          (with-current-buffer outsider
            (insert "outsider prose")
            (jg-agent-comments-mode 1)
            (jg-agent-comments--add (point-min) (point-max) "outsider note"))
          (let ((outsider-record (car (buffer-local-value
                                       'jg-agent-comments--comments outsider))))
            (let ((default-directory project-dir))
              (jg-agent-comments-test--with-input "my question"
                (jg-agent-comments--add (point-min) prose-end "chat note")
                (jg-agent-comments-mode 1)
                (should (equal (jg-agent-comments--augment-prompt "my question")
                               (concat "my question\n\n---\n"
                                       "Inline comments replying to specific parts of your previous message:\n"
                                       "> agent prose\n  chat note")))
                (jg-agent-comments--clear)
                (should (null jg-agent-comments--comments))
                (with-current-buffer outsider
                  (should jg-agent-comments--comments)
                  (should (overlay-buffer
                           (plist-get outsider-record :overlay))))))))
      (let ((buffer (find-buffer-visiting
                     (expand-file-name "outsider.js" other-dir))))
        (when buffer
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (let (kill-buffer-query-functions)
            (kill-buffer buffer))))
      (ignore-errors (delete-directory project-dir 'recursive))
      (ignore-errors (delete-directory other-dir 'recursive)))))

(ert-deftest jg-agent-comments-ride-along-refusal-keeps-file-comments ()
  "A refused submission restores the typed input and keeps the file
comments pending in their home buffers."
  (let ((project-dir (make-temp-file "jg-agent-comments-ride" 'dir)))
    (unwind-protect
        (let ((alpha (find-file-noselect
                      (expand-file-name "alpha.js" project-dir))))
          (with-current-buffer alpha
            (insert "alpha prose")
            (jg-agent-comments-mode 1)
            (jg-agent-comments--add (point-min) (point-max) "alpha note"))
          (let ((default-directory project-dir))
            (jg-agent-comments-test--with-input "my question"
              (jg-agent-comments--add (point-min) prose-end "chat note")
              (jg-agent-comments-mode 1)
              (should-error (jg-agent-comments--submit-advice
                             (lambda () (user-error "Busy, please wait")))
                            :type 'user-error)
              (should (equal (buffer-substring-no-properties pmark (point-max))
                             "my question"))
              (should jg-agent-comments--comments)
              (with-current-buffer alpha
                (should jg-agent-comments--comments))
              (jg-agent-comments--clear))))
      (let ((buffer (find-buffer-visiting
                     (expand-file-name "alpha.js" project-dir))))
        (when buffer
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (let (kill-buffer-query-functions)
            (kill-buffer buffer))))
      (ignore-errors (delete-directory project-dir 'recursive)))))

(ert-deftest jg-agent-comments-ride-along-alone-skips-chat-header ()
  "Collected file comments without chat comments compose without the
chat block header."
  (let ((project-dir (make-temp-file "jg-agent-comments-ride" 'dir)))
    (unwind-protect
        (let ((alpha (find-file-noselect
                      (expand-file-name "alpha.js" project-dir))))
          (with-current-buffer alpha
            (insert "alpha prose")
            (jg-agent-comments-mode 1)
            (jg-agent-comments--add (point-min) (point-max) "alpha note"))
          (let ((default-directory project-dir))
            (with-temp-buffer
              (should (equal (jg-agent-comments--augment-prompt nil)
                             "Inline comments on files:\n\nalpha.js:1\n> alpha prose\n  alpha note")))))
      (let ((buffer (find-buffer-visiting
                     (expand-file-name "alpha.js" project-dir))))
        (when buffer
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (let (kill-buffer-query-functions)
            (kill-buffer buffer))))
      (ignore-errors (delete-directory project-dir 'recursive)))))

(ert-deftest jg-agent-comments-send-refuses-outside-chat ()
  "Sending in a non-chat buffer says the comments ride along with the
next chat submission instead, and queues nothing."
  (jg-agent-comments-test--with-file-buffer "file prose"
    (jg-agent-comments-mode 1)
    (jg-agent-comments--add 1 (point-max) "note")
    (let ((queued :nothing)
          (said nil))
      (cl-letf (((symbol-function 'agent-shell-prompt-queue)
                 (lambda (prompt) (setq queued prompt)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq said (apply #'format-message format-string args)))))
        (jg-agent-comments-send))
      (should (eq queued :nothing))
      (should said)
      (should (string-match-p "ride along" said))
      (should jg-agent-comments--comments))))

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

(ert-deftest jg-agent-comments-commit-uses-captured-snippet ()
  (with-temp-buffer
    (insert "original anchor\nmore output\n")
    (put-text-property (point-min) (point-max) 'field 'output)
    (goto-char 1)
    (let ((source (current-buffer)))
      (jg-agent-comments-comment)
      (should (equal (buffer-name) "*jg-agent-comment*"))
      (with-current-buffer source
        (delete-region 1 16)
        (goto-char (point-min))
        (insert "replaced text"))
      (insert "late comment")
      (jg-agent-comments--commit-edit)
      (with-current-buffer source
        (let ((record (car jg-agent-comments--comments)))
          (should record)
          (should (equal (plist-get record :snippet) "original anchor\n"))
          (should (equal (plist-get record :text) "late comment")))))))

(ert-deftest jg-agent-comments-render-shows-comment-below-anchor ()
  (with-temp-buffer
    (insert "anchor")
    (let* ((record (jg-agent-comments--add 1 (point-max) "first line\nsecond line"))
           (ov (plist-get record :overlay))
           (after (overlay-get ov 'after-string)))
      (should (overlayp ov))
      (should (overlay-buffer ov))
      (should after)
      (should (string-prefix-p "\n" after))
      (should (string-match-p "first line" after))
      (should (null (overlay-get ov 'before-string)))
      (should (null (overlay-get ov 'display))))))

(ert-deftest jg-agent-comments-render-shows-full-multiline-comment ()
  (with-temp-buffer
    (insert "anchor")
    (let* ((record (jg-agent-comments--add 1 (point-max) "para one\n\npara two"))
           (after (overlay-get (plist-get record :overlay) 'after-string)))
      (should (string-match-p "para one" after))
      (should (string-match-p "para two" after))
      (should-not (string-match-p " …" after))
      (should (equal after "\n▐ para one\n▐ \n▐ para two")))))

(ert-deftest jg-agent-comments-render-shows-every-line ()
  (with-temp-buffer
    (insert "anchor")
    (let* ((record (jg-agent-comments--add 1 (point-max) "one\ntwo\nthree"))
           (after (overlay-get (plist-get record :overlay) 'after-string)))
      (should (equal after "\n▐ one\n▐ two\n▐ three")))))

(ert-deftest jg-agent-comments-render-text-renders-full-comment ()
  (let ((body (jg-agent-comments--render-text "one\ntwo\nthree")))
    (should (equal body "▐ one\n▐ two\n▐ three"))
    (should-not (string-match-p " …" body))))

(ert-deftest jg-agent-comments-render-text-gutter-on-every-line ()
  (should (equal (jg-agent-comments--render-text "para one\n\npara two")
                 "▐ para one\n▐ \n▐ para two")))

(ert-deftest jg-agent-comments-render-text-single-line ()
  (should (equal (jg-agent-comments--render-text "note") "▐ note")))

(ert-deftest jg-agent-comments-render-after-string-faces ()
  (with-temp-buffer
    (insert "anchor")
    (let* ((record (jg-agent-comments--add 1 (point-max) "first line\nsecond line"))
           (after (overlay-get (plist-get record :overlay) 'after-string)))
      (should (eq (get-text-property 1 'face after)
                  'jg-agent-comments-marker))
      (should (eq (get-text-property (length "\n▐ ") 'face after)
                  'jg-agent-comments-inline))
      (should (eq (get-text-property (length "\n▐ first line\n") 'face after)
                  'jg-agent-comments-marker))
      (should (eq (get-text-property (length "\n▐ first line\n▐ ") 'face after)
                  'jg-agent-comments-inline)))))

(ert-deftest jg-agent-comments-set-text-updates-record ()
  "Setting the text stores the new body on the record."
  (with-temp-buffer
    (insert "agent prose")
    (let ((record (jg-agent-comments--add 1 (point-max) "old note")))
      (jg-agent-comments--set-comment-text record "new note")
      (should (equal (plist-get record :text) "new note")))))

(ert-deftest jg-agent-comments-set-text-rerenders-overlay ()
  "Setting the text rebuilds the anchor overlay's after-string.
The new body replaces the old, gutter glyphs included."
  (with-temp-buffer
    (insert "agent prose")
    (let* ((record (jg-agent-comments--add 1 (point-max) "old note"))
           (ov (plist-get record :overlay)))
      (jg-agent-comments--set-comment-text record "new note")
      (let ((after (overlay-get ov 'after-string)))
        (should (equal after "\n▐ new note"))
        (should-not (string-match-p "old note" after))))))

(ert-deftest jg-agent-comments-set-text-dead-overlay-no-error ()
  "Setting the text on a record with a deleted anchor does not signal.
The record's body still updates."
  (with-temp-buffer
    (insert "agent prose")
    (let ((record (jg-agent-comments--add 1 (point-max) "note")))
      (delete-overlay (plist-get record :overlay))
      (should (jg-agent-comments--set-comment-text record "new note"))
      (should (equal (plist-get record :text) "new note")))))

(ert-deftest jg-agent-comments-add-renders-single-live-overlay ()
  (with-temp-buffer
    (insert "agent prose\n")
    (let* ((record (jg-agent-comments--add 1 (point-max)
                                           "first line\nsecond line"))
           (ov (plist-get record :overlay))
           (after (overlay-get ov 'after-string)))
      (should (overlayp ov))
      (should (overlay-buffer ov))
      (should (string-prefix-p "\n" after))
      (should (string-match-p "first line" after))
      (should (null (overlay-get ov 'before-string)))
      (should-not (memq :display-overlay record)))))

(ert-deftest jg-agent-comments-anchor-has-no-face ()
  (with-temp-buffer
    (insert "anchor")
    (let* ((record (jg-agent-comments--add 1 (point-max) "first line\nsecond line"))
           (ov (plist-get record :overlay)))
      (should-not (overlay-get ov 'face))
      (should (string-match-p "first line" (overlay-get ov 'after-string))))))

(ert-deftest jg-agent-comments-cancel-edit-kills-buffer-without-prompt ()
  (with-temp-buffer
    (insert "agent prose")
    (put-text-property (point-min) (point-max) 'field 'output)
    (goto-char 1)
    (let ((source (current-buffer)))
      (jg-agent-comments-comment)
      (should (equal (buffer-name) "*jg-agent-comment*"))
      (insert "do not keep")
      (let (prompted)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) (setq prompted t) (error "completing-read called"))))
          (jg-agent-comments-cancel-edit))
        (should-not prompted)
        (should (null (get-buffer "*jg-agent-comment*")))
        (with-current-buffer source
          (should (null jg-agent-comments--comments)))))))

(ert-deftest jg-agent-comments-commit-restores-window-configuration ()
  (with-temp-buffer
    (insert "agent prose")
    (put-text-property (point-min) (point-max) 'field 'output)
    (goto-char 1)
    (set-window-buffer (selected-window) (current-buffer))
    (let ((windows-before (length (window-list)))
          (source (current-buffer)))
      (jg-agent-comments-comment)
      (should (equal (buffer-name) "*jg-agent-comment*"))
      (should (> (length (window-list)) windows-before))
      (insert "late comment")
      (jg-agent-comments--commit-edit)
      (should (= (length (window-list)) windows-before))
      (should (eq (window-buffer (selected-window)) source)))))

(ert-deftest jg-agent-comments-commit-deactivates-mark ()
  "Committing a fresh comment deactivates the source buffer's mark.
In transient-mark-mode the region would stay highlighted after the
commit otherwise."
  (with-temp-buffer
    (insert "agent prose")
    (put-text-property (point-min) (point-max) 'field 'output)
    (goto-char 1)
    (push-mark (point-max) nil t)
    (let ((source (current-buffer))
          (transient-mark-mode t))
      (jg-agent-comments-comment)
      (should (equal (buffer-name) "*jg-agent-comment*"))
      (insert "late comment")
      (jg-agent-comments--commit-edit)
      (with-current-buffer source
        (should (null mark-active))))))

(ert-deftest jg-agent-comments-edit-commit-deactivates-mark ()
  "Committing an edit in replace mode deactivates the source buffer's mark."
  (with-temp-buffer
    (insert "agent prose")
    (let ((source (current-buffer)))
      (jg-agent-comments--add 1 (point-max) "old note")
      (goto-char 1)
      (push-mark (point-max) nil t)
      (let ((transient-mark-mode t))
        (jg-agent-comments-edit-at-point)
        (should (equal (buffer-name) "*jg-agent-comment*"))
        (erase-buffer)
        (insert "new note")
        (jg-agent-comments--commit-edit))
      (with-current-buffer source
        (should (null mark-active))))))

(ert-deftest jg-agent-comments-cancel-preserves-mark ()
  "Canceling the edit buffer leaves the source buffer's mark active."
  (with-temp-buffer
    (insert "agent prose")
    (put-text-property (point-min) (point-max) 'field 'output)
    (goto-char 1)
    (push-mark (point-max) nil t)
    (let ((source (current-buffer))
          (transient-mark-mode t))
      (jg-agent-comments-comment)
      (should (equal (buffer-name) "*jg-agent-comment*"))
      (insert "do not keep")
      (jg-agent-comments-cancel-edit)
      (with-current-buffer source
        (should mark-active)))))

(ert-deftest jg-agent-comments-edit-binding-e ()
  "The \"C-c ;\" prefix binds \"e\" to `jg-agent-comments-edit-at-point'."
  (should (eq (keymap-lookup jg-agent-comments-mode-map "C-c ; e")
              'jg-agent-comments-edit-at-point)))

(ert-deftest jg-agent-comments-edit-no-comment-errors ()
  "Editing with point off any comment's anchor signals `user-error'.
No edit buffer opens."
  (with-temp-buffer
    (insert "first line\nsecond line\n")
    (jg-agent-comments--add 1 12 "note")
    (goto-char 15)
    (should-error (jg-agent-comments-edit-at-point) :type 'user-error)
    (should (null (get-buffer "*jg-agent-comment*")))))

(ert-deftest jg-agent-comments-edit-prefills-text ()
  "Editing opens the comment buffer pre-filled with the comment's text."
  (with-temp-buffer
    (insert "agent prose")
    (let ((record (jg-agent-comments--add 1 (point-max) "fix this later")))
      (goto-char 1)
      (jg-agent-comments-edit-at-point)
      (should (equal (buffer-name) "*jg-agent-comment*"))
      (should (equal (buffer-string) (plist-get record :text)))
      (should (equal (buffer-string) "fix this later"))
      (jg-agent-comments-cancel-edit))))

(ert-deftest jg-agent-comments-edit-commit-overwrites ()
  "Committing an edit overwrites the comment instead of appending.
The record's text becomes the edit buffer's, the anchor overlay shows
the new body, and the comment list keeps its length."
  (with-temp-buffer
    (insert "agent prose")
    (let* ((windows-before (length (window-list)))
           (record (jg-agent-comments--add 1 (point-max) "old note"))
           (overlay (plist-get record :overlay)))
      (goto-char 1)
      (jg-agent-comments-edit-at-point)
      (erase-buffer)
      (insert "new note")
      (jg-agent-comments--commit-edit)
      (should (= (length (window-list)) windows-before))
      (should (equal (plist-get record :text) "new note"))
      (should (string-match-p "new note"
                              (overlay-get overlay 'after-string)))
      (should-not (string-match-p "old note"
                                  (overlay-get overlay 'after-string)))
      (should (= (length jg-agent-comments--comments) 1))
      (should (memq record jg-agent-comments--comments)))))

(ert-deftest jg-agent-comments-edit-cancel-preserves ()
  "Canceling an edit keeps the old comment exactly as it was."
  (with-temp-buffer
    (insert "agent prose")
    (let* ((record (jg-agent-comments--add 1 (point-max) "old note"))
           (overlay (plist-get record :overlay))
           (after (overlay-get overlay 'after-string)))
      (goto-char 1)
      (jg-agent-comments-edit-at-point)
      (erase-buffer)
      (insert "new note")
      (jg-agent-comments-cancel-edit)
      (should (equal (plist-get record :text) "old note"))
      (should (equal (overlay-get overlay 'after-string) after))
      (should (memq record jg-agent-comments--comments)))))

(ert-deftest jg-agent-comments-edit-whitespace-commit-preserves ()
  "Committing a whitespace-only edit keeps the old comment.
The record and its display stay unchanged and the edit buffer closes."
  (with-temp-buffer
    (insert "agent prose")
    (let* ((record (jg-agent-comments--add 1 (point-max) "old note"))
           (overlay (plist-get record :overlay))
           (after (overlay-get overlay 'after-string)))
      (goto-char 1)
      (jg-agent-comments-edit-at-point)
      (erase-buffer)
      (insert "  \n  ")
      (jg-agent-comments--commit-edit)
      (should (equal (plist-get record :text) "old note"))
      (should (equal (overlay-get overlay 'after-string) after))
      (should (memq record jg-agent-comments--comments))
      (should (null (get-buffer "*jg-agent-comment*"))))))

(ert-deftest jg-agent-comments-edit-dead-anchor-errors ()
  "Editing where the anchor overlay evaporated signals `user-error'."
  (with-temp-buffer
    (insert "agent prose")
    (let ((record (jg-agent-comments--add 1 (point-max) "note")))
      (delete-overlay (plist-get record :overlay))
      (goto-char 1)
      (should-error (jg-agent-comments-edit-at-point) :type 'user-error))))

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

(ert-deftest jg-agent-comments-augment-prompt-appends-comment-block ()
  "Augmenting a prompt appends the header and the composed entries.
The typed input comes first, then a blank line, then the header, then
the entries."
  (with-temp-buffer
    (insert "agent prose")
    (jg-agent-comments--add 1 (point-max) "note")
    (should (equal (jg-agent-comments--augment-prompt "my question")
                   "my question\n\n---\nInline comments replying to specific parts of your previous message:\n> agent prose\n  note"))))

(ert-deftest jg-agent-comments-augment-prompt-without-comments-is-identity ()
  "Augmenting a prompt without pending comments returns the input unchanged."
  (with-temp-buffer
    (should (equal (jg-agent-comments--augment-prompt "my question")
                   "my question"))))

(ert-deftest jg-agent-comments-augment-prompt-without-input-returns-block ()
  "Augmenting a nil prompt returns the header and the entries alone.
`jg-agent-comments-send' submits this shape: no prompt text above the
comment block."
  (with-temp-buffer
    (insert "agent prose")
    (jg-agent-comments--add 1 (point-max) "note")
    (should (equal (jg-agent-comments--augment-prompt nil)
                   "---\nInline comments replying to specific parts of your previous message:\n> agent prose\n  note"))))

(ert-deftest jg-agent-comments-send-uses-prompt-queue-and-clears ()
  "Sending submits the header and the composed entries as one prompt.
The prompt carries no other text above the comment block."
  (with-temp-buffer
    (comint-mode)
    (insert "agent prose")
    (jg-agent-comments--add 1 (point-max) "note")
    (let ((submitted nil))
      (cl-letf (((symbol-function 'agent-shell-prompt-queue)
                 (lambda (prompt) (setq submitted prompt))))
        (jg-agent-comments-send))
      (should (equal submitted
                     "---\nInline comments replying to specific parts of your previous message:\n> agent prose\n  note"))
      (should (null jg-agent-comments--comments)))))

(ert-deftest jg-agent-comments-send-without-comments-is-noop ()
  (with-temp-buffer
    (comint-mode)
    (should (progn (jg-agent-comments-send) t))))

(ert-deftest jg-agent-comments-submit-advice-installed ()
  "The package advises `agent-shell-submit' around with the submit wrapper.
The advice is in place after this file loads, before agent-shell
defines the function."
  (should (advice-member-p #'jg-agent-comments--submit-advice
                            'agent-shell-submit)))

(defmacro jg-agent-comments-test--with-input (typed &rest body)
  "Run BODY in a temp buffer that holds TYPED as the live prompt input.
The buffer holds agent prose, a fake prompt, then TYPED.  Bind `proc'
to a pipe process for the buffer, `pmark' to its process mark at the
start of the live input, and `prose-end' to the end of the agent
prose.  Kill the process after BODY."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert "agent prose")
     (let ((prose-end (point))
           (proc (start-process "jg-agent-comments-test" (current-buffer)
                                "cat")))
       (unwind-protect
           (progn
             (insert "\nClaude> ")
             (set-marker (process-mark proc) (point))
             (insert ,typed)
             (let ((pmark (process-mark proc)))
               ,@body))
         (delete-process proc)))))

(defmacro jg-agent-comments-test--with-file-buffer (contents &rest body)
  "Run BODY in a file-visiting temp buffer holding CONTENTS.
Bind `file' to the buffer's file name.  Kill the buffer and delete
the file after BODY, without any kill prompting."
  (declare (indent 1) (debug t))
  `(let ((file (make-temp-file "jg-agent-comments-test")))
     (unwind-protect
         (with-current-buffer (find-file-noselect file)
           (insert ,contents)
           ,@body)
       (let ((buffer (find-buffer-visiting file)))
         (when buffer
           (with-current-buffer buffer
             (set-buffer-modified-p nil))
           (let (kill-buffer-query-functions)
             (kill-buffer buffer)))
         (ignore-errors (delete-file file))))))

(ert-deftest jg-agent-comments-submit-advice-augments-typed-input ()
  "A submission with pending comments carries the comment block.
The submit function reads the augmented prompt from the input area.
The comments clear after the submission."
  (jg-agent-comments-test--with-input "my question"
    (jg-agent-comments--add (point-min) prose-end "note")
    (jg-agent-comments-mode 1)
    (let ((sent nil))
      (jg-agent-comments--submit-advice
       (lambda ()
         (setq sent (buffer-substring-no-properties pmark (point-max)))
         (set-marker pmark (point-max))
         (insert "\n")))
      (should (equal sent
                     "my question\n\n---\nInline comments replying to specific parts of your previous message:\n> agent prose\n  note"))
      (should (null jg-agent-comments--comments)))))

(ert-deftest jg-agent-comments-submit-advice-restores-input-when-refused ()
  "A refused submission restores the typed input and keeps the comments.
`agent-shell-submit' refuses a busy shell before it commits the input.
The wrapper re-signals the refusal, restores the typed input, and
leaves the comments pending for the next submission."
  (jg-agent-comments-test--with-input "my question"
    (jg-agent-comments--add (point-min) prose-end "note")
    (jg-agent-comments-mode 1)
    (should-error (jg-agent-comments--submit-advice
                   (lambda () (user-error "Busy, please wait")))
                  :type 'user-error)
    (should (equal (buffer-substring-no-properties pmark (point-max))
                   "my question"))
    (should jg-agent-comments--comments)))

(ert-deftest jg-agent-comments-submit-advice-passes-through-without-comments ()
  "Without pending comments the submit function receives the typed input as-is."
  (jg-agent-comments-test--with-input "my question"
    (jg-agent-comments-mode 1)
    (let ((sent :nothing))
      (jg-agent-comments--submit-advice
       (lambda ()
         (setq sent (buffer-substring-no-properties pmark (point-max)))))
      (should (equal sent "my question"))
      (should (equal (buffer-substring-no-properties pmark (point-max))
                     "my question")))))

(ert-deftest jg-agent-comments-submit-advice-passes-through-empty-input ()
  "An empty prompt submits as before and keeps the comments pending.
No typed input means no augmentation: the submit function sees the
empty input area, and the comments wait for the next submission."
  (jg-agent-comments-test--with-input ""
    (jg-agent-comments--add (point-min) prose-end "note")
    (jg-agent-comments-mode 1)
    (let ((sent :nothing))
      (jg-agent-comments--submit-advice
       (lambda ()
         (setq sent (buffer-substring-no-properties pmark (point-max)))))
      (should (equal sent ""))
      (should (equal (buffer-substring-no-properties pmark (point-max)) ""))
      (should jg-agent-comments--comments))))

(ert-deftest jg-agent-comments-submit-advice-passes-through-mode-off ()
  "The advice does not act in buffers where the mode is off."
  (jg-agent-comments-test--with-input "my question"
    (jg-agent-comments--add (point-min) prose-end "note")
    (let ((sent :nothing))
      (jg-agent-comments--submit-advice
       (lambda ()
         (setq sent (buffer-substring-no-properties pmark (point-max)))))
      (should (equal sent "my question"))
      (should jg-agent-comments--comments))))

(ert-deftest jg-agent-comments-delete-at-point-removes-record ()
  (with-temp-buffer
    (insert "agent prose")
    (let ((record (jg-agent-comments--add 1 (point-max) "note")))
      (goto-char 1)
      (jg-agent-comments-delete-at-point)
      (should (null jg-agent-comments--comments))
      (should (null (overlay-buffer (plist-get record :overlay)))))))

(ert-deftest jg-agent-comments-matcher-finds-comment ()
  "The shared matcher returns the record whose anchor covers point."
  (with-temp-buffer
    (insert "agent prose")
    (let ((record (jg-agent-comments--add 1 (point-max) "note")))
      (goto-char 1)
      (should (eq (jg-agent-comments--comment-at-point) record))
      (goto-char 6)
      (should (eq (jg-agent-comments--comment-at-point) record)))))

(ert-deftest jg-agent-comments-matcher-most-recent-wins
    ()
  "The matcher prefers the most recently added comment.
With several anchor overlays covering point, the newest record wins.
Deleting at that point removes the newer record and keeps the older."
  (with-temp-buffer
    (insert "first line\nsecond line\n")
    (let ((older (jg-agent-comments--add 1 6 "older note")))
      (let ((newer (jg-agent-comments--add 3 8 "newer note")))
        (goto-char 4)
        (should (eq (jg-agent-comments--comment-at-point) newer))
        (should (equal (plist-get (jg-agent-comments--comment-at-point)
                                  :text)
                       "newer note"))
        (jg-agent-comments-delete-at-point)
        (should (equal jg-agent-comments--comments (list older)))
        (should (eq (jg-agent-comments--comment-at-point) older))
        (should (equal (plist-get (jg-agent-comments--comment-at-point)
                                  :text)
                       "older note"))))))

(ert-deftest jg-agent-comments-set-text-preserves-highlight-overlay ()
  "Setting the text keeps a partial-line comment's highlight overlay.
The overlay stays alive over the same bounds with the same face."
  (with-temp-buffer
    (insert "hello world")
    (let* ((record (jg-agent-comments--add 1 6 "old note"))
           (highlight (plist-get record :highlight-overlay)))
      (jg-agent-comments--set-comment-text record "new note")
      (should (overlayp highlight))
      (should (overlay-buffer highlight))
      (should (= (overlay-start highlight) 1))
      (should (= (overlay-end highlight) 6))
      (should (eq (overlay-get highlight 'face)
                  'jg-agent-comments-highlight)))))

(ert-deftest jg-agent-comments-matcher-end-of-line-inclusive ()
  "The matcher includes the newline position of a whole-line anchor."
  (with-temp-buffer
    (insert "first line\nsecond line\n")
    (let ((record (jg-agent-comments--add 1 12 "note")))
      (goto-char 11)
      (should (eq (jg-agent-comments--comment-at-point) record)))))

(ert-deftest jg-agent-comments-matcher-next-line-exclusive ()
  "The matcher excludes the start of the line after a whole-line anchor."
  (with-temp-buffer
    (insert "first line\nsecond line\n")
    (let ((record (jg-agent-comments--add 1 12 "note")))
      (goto-char 12)
      (should (null (jg-agent-comments--comment-at-point))))))

(ert-deftest jg-agent-comments-matcher-last-line-of-buffer ()
  "The matcher finds a comment anchored on the final line, newline absent."
  (with-temp-buffer
    (insert "first line\nlast line")
    (let ((record (jg-agent-comments--add 12 (point-max) "note")))
      (goto-char 16)
      (should (eq (jg-agent-comments--comment-at-point) record))
      (goto-char (point-max))
      (should (eq (jg-agent-comments--comment-at-point) record)))))

(ert-deftest jg-agent-comments-delete-binding-d ()
  "The \"C-c ;\" prefix binds \"d\" to `jg-agent-comments-delete-at-point'."
  (should (eq (keymap-lookup jg-agent-comments-mode-map "C-c ; d")
              #'jg-agent-comments-delete-at-point)))

(ert-deftest jg-agent-comments-send-binding-ret ()
  "The \"C-c ;\" prefix binds \"RET\" to `jg-agent-comments-send'.
Key notation parses \"RET\" as the return-key event, the same event a
user pressing the return key produces."
  (should (eq (keymap-lookup jg-agent-comments-mode-map "C-c ; RET")
              'jg-agent-comments-send)))

(ert-deftest jg-agent-comments-highlight-covers-partial-line-selection ()
  (with-temp-buffer
    (insert "hello world")
    (let* ((record (jg-agent-comments--add 1 6 "note"))
           (overlay (plist-get record :highlight-overlay)))
      (should (overlayp overlay))
      (should (overlay-buffer overlay))
      (should (= (overlay-start overlay) 1))
      (should (= (overlay-end overlay) 6))
      (should (eq (overlay-get overlay 'face)
                  'jg-agent-comments-highlight)))))

(ert-deftest jg-agent-comments-no-highlight-for-whole-line-selection ()
  (with-temp-buffer
    (insert "hello world\n")
    (let ((record (jg-agent-comments--add 1 12 "note")))
      (should (null (plist-get record :highlight-overlay))))))

(ert-deftest jg-agent-comments-cleanup-removes-highlight-overlay ()
  (with-temp-buffer
    (insert "hello world")
    (let ((record (jg-agent-comments--add 1 6 "note")))
      (jg-agent-comments--clear)
      (should (null (overlay-buffer (plist-get record :highlight-overlay))))))
  (with-temp-buffer
    (insert "hello world")
    (let ((record (jg-agent-comments--add 1 6 "note")))
      (goto-char 1)
      (jg-agent-comments-delete-at-point)
      (should (null (overlay-buffer
                     (plist-get record :highlight-overlay)))))))

(ert-deftest jg-agent-comments-highlight-evaporates-with-covered-text ()
  (with-temp-buffer
    (insert "hello world")
    (let ((record (jg-agent-comments--add 1 6 "note")))
      (delete-region 1 6)
      (should (member record jg-agent-comments--comments))
      (should (null (overlay-buffer (plist-get record :highlight-overlay))))
      (should (overlay-buffer (plist-get record :overlay))))))

(ert-deftest jg-agent-comments-highlight-has-amber-background ()
  (should (equal (face-background 'jg-agent-comments-highlight) "#5a4210")))

(provide 'jg-agent-comments-test)
;;; jg-agent-comments-test.el ends here

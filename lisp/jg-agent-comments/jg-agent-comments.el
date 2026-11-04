;;; jg-agent-comments.el --- Inline comments for agent-shell buffers -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience

;;; Commentary:

;; Inline comments for agent-shell buffers.

;;; Code:

(defgroup jg-agent-comments nil
  "Inline comments for agent-shell buffers."
  :group 'convenience)

(defface jg-agent-comments-marker
  '((t :inherit warning :weight bold))
  "Face for inline comment markers.")

(defface jg-agent-comments-inline
  '((t :inherit 'font-lock-comment-face))
  "Face for inline comment text.")

(defface jg-agent-comments-snippet
  '((t :inherit 'shadow))
  "Face for dimmed code snippets shown with a comment.")

(defvar-local jg-agent-comments--comments nil
  "Buffer-local list of comment records, most recent first.
Each record is a plist with the shape (:snippet :text :beg :end :overlay):
:beg and :end are the buffer positions the comment refers to, :snippet
is the captured buffer text, :text is the comment body, and :overlay is
the overlay anchoring the record (nil until rendered).")

(defun jg-agent-comments--add (beg end text)
  "Record a comment about buffer text between BEG and END.
Capture the covered text (including invisible parts) as the record's
:snippet, store TEXT as the comment body, render the record's overlay,
and push the record onto `jg-agent-comments--comments'.  Return the
record, a plist with the shape (:snippet :text :beg :end :overlay)."
  (let ((record (list :snippet (filter-buffer-substring beg end)
                      :text text
                      :beg beg
                      :end end
                      :overlay nil)))
    (jg-agent-comments--render record)
    (push record jg-agent-comments--comments)
    record))

(defun jg-agent-comments--render (record)
  "Give RECORD its anchoring overlay if it does not have one yet.
The overlay spans the positions captured at add time (:beg and :end),
carries the `jg-agent-comments' property and the snippet face, and
evaporates when its buffer text is deleted.  The overlay is stored
back into the record's :overlay."
  (unless (plist-get record :overlay)
    (let ((overlay (make-overlay (plist-get record :beg)
                                 (plist-get record :end)
                                 nil t nil)))
      (overlay-put overlay 'jg-agent-comments t)
      (overlay-put overlay 'face 'jg-agent-comments-snippet)
      (overlay-put overlay 'evaporate t)
      (plist-put record :overlay overlay))))

(defvar-keymap jg-agent-comments-mode-map)

(define-minor-mode jg-agent-comments-mode
  "Inline comments for agent-shell buffers."
  :init-value nil
  :lighter nil)

(declare-function agent-shell-prompt-queue "agent-shell" (prompt))
(require 'agent-shell nil t)

(provide 'jg-agent-comments)
;;; jg-agent-comments.el ends here

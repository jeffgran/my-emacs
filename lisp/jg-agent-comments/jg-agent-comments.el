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

(defun jg-agent-comments--agent-text-at-point-p ()
  "Return non-nil when point is on agent output text."
  (or (eq (get-char-property (point) 'field) 'output)
      (eq (get-char-property (max (point-min) (1- (point))) 'field) 'output)))

(defun jg-agent-comments--region-or-line ()
  "Return the bounds (BEG . END) of the agent text to comment on.
Signal `user-error' when point is not on agent output."
  (unless (jg-agent-comments--agent-text-at-point-p)
    (user-error "Not on agent output"))
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (cons (pos-bol) (min (point-max) (1+ (pos-eol))))))

(defun jg-agent-comments--commit-edit-from-string (text beg end)
  "Commit comment TEXT about the text between BEG and END.
Whitespace-only TEXT cancels the comment and records nothing."
  (unless (string-match-p "\\`[[:space:]]*\\'" (or text ""))
    (jg-agent-comments--add beg end text)))

(defvar-local jg-agent-comments--edit-source nil
  "Buffer whose text the edit buffer's comment refers to.")

(defvar-local jg-agent-comments--edit-bounds nil
  "Cons of buffer positions the comment refers to in the source buffer.")

(defvar-keymap jg-agent-comments-edit-mode-map
  "C-c C-c" #'jg-agent-comments--commit-edit
  "C-c C-k" #'kill-buffer)

(defun jg-agent-comments--commit-edit ()
  "Commit the comment being edited in the current edit buffer."
  (interactive nil jg-agent-comments-edit-mode-map)
  (let ((text (buffer-string))
        (source jg-agent-comments--edit-source)
        (bounds jg-agent-comments--edit-bounds))
    (kill-buffer)
    (when (buffer-live-p source)
      (with-current-buffer source
        (jg-agent-comments--commit-edit-from-string
         text (car bounds) (cdr bounds))))))

(defun jg-agent-comments-comment ()
  "Comment on the agent output at point in a dedicated edit buffer."
  (interactive)
  (let* ((bounds (jg-agent-comments--region-or-line))
         (source (current-buffer)))
    (pop-to-buffer (get-buffer-create "*jg-agent-comment*"))
    (erase-buffer)
    (text-mode)
    (setq-local jg-agent-comments--edit-source source)
    (setq-local jg-agent-comments--edit-bounds bounds)
    (use-local-map jg-agent-comments-edit-mode-map)))

(defvar-keymap jg-agent-comments-mode-map)

(define-minor-mode jg-agent-comments-mode
  "Inline comments for agent-shell buffers."
  :init-value nil
  :lighter nil)

(declare-function agent-shell-prompt-queue "agent-shell" (prompt))
(require 'agent-shell nil t)

(provide 'jg-agent-comments)
;;; jg-agent-comments.el ends here

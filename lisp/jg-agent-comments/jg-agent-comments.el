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
Each record is a plist with the shape (:snippet :text :beg :end
:overlay): :beg and :end are the buffer positions the comment refers
to, :snippet is the captured buffer text, :text is the comment body,
and :overlay is the overlay anchoring the record (nil until
rendered).")

(defun jg-agent-comments--add (beg end text &optional snippet)
  "Record a comment about buffer text between BEG and END.
Capture the covered text (including invisible parts, without text
properties) as the record's :snippet.  Use SNIPPET as the :snippet when
SNIPPET is non-nil.  The caller captured that text earlier.  Store TEXT
as the comment body.  Render the record's overlay.  Push the record
onto `jg-agent-comments--comments'.  Return the record, a plist with
the shape (:snippet :text :beg :end :overlay)."
  (let ((record (list :snippet (or snippet
                                   (substring-no-properties
                                    (filter-buffer-substring beg end)))
                      :text text
                      :beg beg
                      :end end
                      :overlay nil)))
    (jg-agent-comments--render record)
    (push record jg-agent-comments--comments)
    record))

(defun jg-agent-comments--inline-string (text)
  "Return TEXT as a single-line string.
Keep the first line of TEXT, appending \" …\" when TEXT has more
lines."
  (if (string-match "\n" text)
      (concat (substring text 0 (match-beginning 0)) " …")
    text))

(defun jg-agent-comments--display-position (record)
  "Return the buffer position where RECORD's anchor overlay ends.
That is the end of the last line covered by the record's anchor.  The
inline text shows at that position, so it never starts on a line of
its own."
  (let ((end (plist-get record :end)))
    (if (and (> end (plist-get record :beg))
             (save-excursion (goto-char end) (bolp)))
        (1- end)
      end)))

(defun jg-agent-comments--render (record)
  "Give RECORD its anchor overlay if it has none yet.
The overlay spans the anchor text from :beg through the display
position.  It carries the `jg-agent-comments' property and the snippet
face.  It shows the marker gutter as its before-string.  It shows the
comment inline as its after-string: two-space padded, in the inline
face, and elided to one line.  It evaporates when a buffer change
empties it.  The overlay is stored back into the record."
  (unless (plist-get record :overlay)
    (let ((overlay (make-overlay (plist-get record :beg)
                                 (jg-agent-comments--display-position record)
                                 nil t nil)))
      (overlay-put overlay 'jg-agent-comments t)
      (overlay-put overlay 'face 'jg-agent-comments-snippet)
      (overlay-put overlay 'before-string
                   (propertize "▐ " 'face 'jg-agent-comments-marker))
      (overlay-put overlay 'after-string
                   (propertize (concat "  "
                                       (jg-agent-comments--inline-string
                                        (plist-get record :text)))
                               'face 'jg-agent-comments-inline))
      (overlay-put overlay 'evaporate t)
      (plist-put record :overlay overlay))))

(require 'comint)

(defun jg-agent-comments--agent-text-at-point-p ()
  "Return non-nil when point is on agent output text.
The live input area after the last prompt is not agent output.  When
`comint-last-prompt' is non-nil, refuse point at or after its end
marker.  Otherwise apply only the field check."
  (and (or (not (bound-and-true-p comint-last-prompt))
           (< (point) (marker-position (cdr comint-last-prompt))))
       (or (eq (get-char-property (point) 'field) 'output)
           (eq (get-char-property (max (point-min) (1- (point))) 'field) 'output))))

(defun jg-agent-comments--region-or-line ()
  "Return the bounds (BEG . END) of the agent text to comment on.
Signal `user-error' when point is not on agent output."
  (unless (jg-agent-comments--agent-text-at-point-p)
    (user-error "Not on agent output"))
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (cons (pos-bol) (min (point-max) (1+ (pos-eol))))))

(defun jg-agent-comments--commit-edit-from-string (text beg end &optional snippet)
  "Commit comment TEXT about the text between BEG and END.
Pass SNIPPET through to `jg-agent-comments--add'.  Whitespace-only TEXT
cancels the comment and records nothing."
  (unless (string-match-p "\\`[[:space:]]*\\'" (or text ""))
    (jg-agent-comments--add beg end text snippet)))

(defvar-local jg-agent-comments--edit-source nil
  "Buffer whose text the edit buffer's comment refers to.")

(defvar-local jg-agent-comments--edit-bounds nil
  "Cons of markers that delimit the anchor text in the source buffer.
The comment command placed the markers before it opened the edit
buffer.")

(defvar-local jg-agent-comments--edit-snippet nil
  "Anchor text that the comment command captured at comment time.")

(defvar-keymap jg-agent-comments-edit-mode-map
  "C-c C-c" #'jg-agent-comments--commit-edit
  "C-c C-k" #'kill-buffer)

(defun jg-agent-comments--commit-edit ()
  "Commit the comment that the current edit buffer holds.
The record uses the snippet and the marker positions that the comment
command captured.  Source buffer changes during editing do not change
what the comment refers to."
  (interactive nil jg-agent-comments-edit-mode-map)
  (let ((text (buffer-string))
        (source jg-agent-comments--edit-source)
        (snippet jg-agent-comments--edit-snippet)
        (bounds jg-agent-comments--edit-bounds))
    (kill-buffer)
    (when (buffer-live-p source)
      (with-current-buffer source
        (jg-agent-comments--commit-edit-from-string
         text
         (marker-position (car bounds))
         (marker-position (cdr bounds))
         snippet)))))

(defun jg-agent-comments-comment ()
  "Comment on the agent output at point in a dedicated edit buffer.
Capture the anchor text and place its markers before the edit buffer
opens.  A re-render during editing cannot change the captured text."
  (interactive)
  (let* ((bounds (jg-agent-comments--region-or-line))
         (source (current-buffer))
         (snippet (substring-no-properties
                   (filter-buffer-substring (car bounds) (cdr bounds))))
         (beg (copy-marker (car bounds)))
         (end (copy-marker (cdr bounds))))
    (pop-to-buffer (get-buffer-create "*jg-agent-comment*"))
    (erase-buffer)
    (text-mode)
    (setq-local jg-agent-comments--edit-source source)
    (setq-local jg-agent-comments--edit-bounds (cons beg end))
    (setq-local jg-agent-comments--edit-snippet snippet)
    (use-local-map jg-agent-comments-edit-mode-map)))

(defvar-keymap jg-agent-comments-prefix-map
  ";" #'jg-agent-comments-comment
  "s" #'jg-agent-comments-send
  "k" #'jg-agent-comments-delete-at-point)

(defvar-keymap jg-agent-comments-mode-map
  "C-c ;" jg-agent-comments-prefix-map)

(define-minor-mode jg-agent-comments-mode
  "Inline comments for agent-shell buffers."
  :init-value nil
  :lighter nil)

(defvar jg-agent-comments--mode-line-registered nil
  "Non-nil once the mode-line entry has been added to `global-mode-string'.")

(defun jg-agent-comments--mode-line-string ()
  "Return the mode-line fragment showing this buffer's comment count.
An empty string when the buffer has no comments; otherwise \" [N
comments]\" (singular for one)."
  (if (null jg-agent-comments--comments)
      ""
    (let ((count (length jg-agent-comments--comments)))
      (format " [%d comment%s]" count (if (= count 1) "" "s")))))

(unless jg-agent-comments--mode-line-registered
  (setq jg-agent-comments--mode-line-registered t)
  (add-to-list 'global-mode-string
               '(:eval (jg-agent-comments--mode-line-string))
               'append))

(declare-function agent-shell-prompt-queue "agent-shell" (prompt))
(require 'agent-shell nil t)

(defun jg-agent-comments--anchor-live-p (record)
  "Return non-nil when RECORD's anchor overlay is alive."
  (let ((overlay (plist-get record :overlay)))
    (and (overlayp overlay) (overlay-buffer overlay))))

(defun jg-agent-comments--entry-string (record)
  "Return RECORD rendered as one entry of the composed prompt.
The snippet's lines are each prefixed with \"> \" and the comment's
lines with two spaces; a single trailing newline on the snippet is
dropped."
  (let ((snippet (plist-get record :snippet))
        (text (plist-get record :text)))
    (when (string-suffix-p "\n" snippet)
      (setq snippet (substring snippet 0 -1)))
    (concat (mapconcat (lambda (line) (concat "> " line))
                       (split-string snippet "\n")
                       "\n")
            "\n"
            (mapconcat (lambda (line) (concat "  " line))
                       (split-string text "\n")
                       "\n"))))

(defun jg-agent-comments--compose ()
  "Return all pending comments as one prompt string in buffer order.
Records with a live anchor overlay sort by :beg; the rest come last,
keeping their relative order.  Entries are separated by a blank line."
  (let ((live '())
        (dead '()))
    (dolist (record jg-agent-comments--comments)
      (if (jg-agent-comments--anchor-live-p record)
          (push record live)
        (push record dead)))
    (mapconcat #'jg-agent-comments--entry-string
               (append (sort (nreverse live)
                             (lambda (a b)
                               (< (plist-get a :beg) (plist-get b :beg))))
                       (nreverse dead))
               "\n\n")))

(defun jg-agent-comments--clear ()
  "Forget all pending comments and delete their overlays."
  (dolist (record jg-agent-comments--comments)
    (let ((overlay (plist-get record :overlay)))
      (when (and (overlayp overlay) (overlay-buffer overlay))
        (delete-overlay overlay))))
  (setq jg-agent-comments--comments nil))

(defun jg-agent-comments-send ()
  "Submit all pending comments to the agent as one prompt, then clear.
Say so and do nothing when there are no pending comments."
  (interactive)
  (if (null jg-agent-comments--comments)
      (message "No pending comments")
    (agent-shell-prompt-queue (jg-agent-comments--compose))
    (jg-agent-comments--clear)))

(defun jg-agent-comments-delete-at-point ()
  "Delete the comment whose anchor overlay covers point.
Delete the record's overlay when it is still live and remove the
record from `jg-agent-comments--comments'.  Signal `user-error' when
point is not on any comment."
  (interactive nil jg-agent-comments-mode-map)
  (let ((record (seq-find
                 (lambda (record)
                   (let ((overlay (plist-get record :overlay)))
                     (and (overlayp overlay)
                          (overlay-buffer overlay)
                          (overlay-get overlay 'jg-agent-comments)
                          (>= (point) (overlay-start overlay))
                          (<= (point) (overlay-end overlay)))))
                 jg-agent-comments--comments)))
    (unless record
      (user-error "No comment at point"))
    (let ((overlay (plist-get record :overlay)))
      (when (and (overlayp overlay) (overlay-buffer overlay))
        (delete-overlay overlay)))
    (setq jg-agent-comments--comments
          (delq record jg-agent-comments--comments))))

(provide 'jg-agent-comments)
;;; jg-agent-comments.el ends here

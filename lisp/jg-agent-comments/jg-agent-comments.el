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

(defface jg-agent-comments-highlight
  '((t :background "#5a4210"))
  "Face for the tint over a partial-line comment selection.
A dim amber, matching the gutter marker's warning hue.")

(defvar-local jg-agent-comments--comments nil
  "Buffer-local list of comment records, most recent first.
Each record is a plist with the shape (:snippet :text :beg :end
:overlay :highlight-overlay): :beg and :end are the buffer positions
the comment refers to, :snippet is the captured buffer text, :text is
the comment body, :overlay is the overlay anchoring the record (nil
until rendered), and :highlight-overlay is the overlay tinting a
partial-line selection (nil when the selection covers whole lines).")

(defun jg-agent-comments--add (beg end text &optional snippet)
  "Record a comment about buffer text between BEG and END.
Capture the covered text (including invisible parts, without text
properties) as the record's :snippet.  Use SNIPPET as the :snippet when
SNIPPET is non-nil.  The caller captured that text earlier.  Store TEXT
as the comment body.  Render the record's overlay.  Push the record
onto `jg-agent-comments--comments'.  Return the record, a plist with
the shape (:snippet :text :beg :end :overlay
:highlight-overlay)."
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

(defun jg-agent-comments--render-text (text)
  "Return the propertized display body for the full comment TEXT.
Prefix every line with the \"▐ \" gutter in the marker face; give each
line's text the inline face.  A blank line keeps just its gutter."
  (mapconcat (lambda (line)
               (concat (propertize "▐ " 'face 'jg-agent-comments-marker)
                       (propertize line 'face 'jg-agent-comments-inline)))
             (split-string text "\n")
             "\n"))

(defun jg-agent-comments--display-position (record)
  "Return the buffer position where RECORD's anchor overlay ends.
That is the end of the last line covered by the record's anchor.  The
comment shows on a new line after that position."
  (let ((end (plist-get record :end)))
    (if (and (> end (plist-get record :beg))
             (save-excursion (goto-char end) (bolp)))
        (1- end)
      (save-excursion (goto-char end) (pos-eol)))))

(defun jg-agent-comments--covers-whole-lines-p (record)
  "Return non-nil when RECORD's selection covers whole lines.
The selection starts at the beginning of its line and ends at the end
of its line or at the beginning of the next line, newline included."
  (and (save-excursion (goto-char (plist-get record :beg)) (bolp))
       (let ((end (plist-get record :end)))
         (or (save-excursion (goto-char end) (eolp))
             (save-excursion (goto-char end) (bolp))))))

(defun jg-agent-comments--render-highlight (record)
  "Give RECORD its highlight overlay when its selection is partial.
A selection that does not cover whole lines gets an overlay spanning
exactly :beg to :end with the `jg-agent-comments-highlight' face; it
evaporates when its text goes away.  A whole-line selection gets nil,
leaving the line untouched.  The overlay is stored back into the
record under :highlight-overlay."
  (if (jg-agent-comments--covers-whole-lines-p record)
      (plist-put record :highlight-overlay nil)
    (let ((overlay (make-overlay (plist-get record :beg)
                                 (plist-get record :end)
                                 nil t nil)))
      (overlay-put overlay 'face 'jg-agent-comments-highlight)
      (overlay-put overlay 'evaporate t)
      (plist-put record :highlight-overlay overlay))))

(defun jg-agent-comments--render (record)
  "Give RECORD its anchor overlay if it has none yet.
The overlay spans the anchor text from :beg through the display
position.  It carries the `jg-agent-comments' property.  It shows the
comment on its own line below the anchor as its after-string: a
newline, then the full comment body rendered by
`jg-agent-comments--render-text'.  It evaporates when a buffer change
empties it.  A partial-line selection also gets a highlight overlay
tinting exactly the selected text.  Both overlays are stored back
into the record."
  (unless (plist-get record :overlay)
    (let ((overlay (make-overlay (plist-get record :beg)
                                 (jg-agent-comments--display-position record)
                                 nil t nil)))
      (overlay-put overlay 'jg-agent-comments t)
      (overlay-put overlay 'after-string
                   (concat "\n"
                           (jg-agent-comments--render-text
                            (plist-get record :text))))
      (overlay-put overlay 'evaporate t)
      (plist-put record :overlay overlay))
    (jg-agent-comments--render-highlight record)))

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

(defvar-local jg-agent-comments--saved-winconf nil
  "Window configuration of the source buffer before the edit buffer opened.")

(defun jg-agent-comments--finish-edit ()
  "Kill the current edit buffer and restore the saved window configuration.
Run with the edit buffer current.  Skip the restore when no window
configuration is left or restoring it would fail."

  (let ((conf jg-agent-comments--saved-winconf))
    (kill-buffer (current-buffer))
    (when (and conf (window-configuration-p conf))
      (condition-case nil
          (set-window-configuration conf)
        (error nil)))))

(defvar-keymap jg-agent-comments-edit-mode-map
  "C-c C-c" #'jg-agent-comments--commit-edit
  "C-c C-k" #'jg-agent-comments-cancel-edit)

(defun jg-agent-comments--commit-edit ()
  "Commit the comment that the current edit buffer holds.
The record uses the snippet and the marker positions that the comment
command captured.  Source buffer changes during editing do not change
what the comment refers to."
  (interactive nil jg-agent-comments-edit-mode-map)
  (let* ((edit (current-buffer))
         (text (buffer-string))
         (source jg-agent-comments--edit-source)
         (snippet jg-agent-comments--edit-snippet)
         (bounds jg-agent-comments--edit-bounds))
    (when (buffer-live-p source)
      (with-current-buffer source
        (jg-agent-comments--commit-edit-from-string
         text
         (marker-position (car bounds))
         (marker-position (cdr bounds))
         snippet)))
    (with-current-buffer edit
      (jg-agent-comments--finish-edit))))

(defun jg-agent-comments-cancel-edit ()
  "Discard the comment being edited and restore the window configuration.
Kill the edit buffer without committing anything."
  (interactive nil jg-agent-comments-edit-mode-map)
  (jg-agent-comments--finish-edit))

(defun jg-agent-comments-comment ()
  "Comment on the agent output at point in a dedicated edit buffer.
Capture the anchor text and place its markers before the edit buffer
opens.  A re-render during editing cannot change the captured text.
Save the window configuration so that finishing the edit, by commit or
by cancel, restores the pre-edit windows."
  (interactive)
  (let* ((winconf (current-window-configuration))
         (bounds (jg-agent-comments--region-or-line))
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
    (setq-local jg-agent-comments--saved-winconf winconf)
    (use-local-map jg-agent-comments-edit-mode-map)))

(defvar-keymap jg-agent-comments-prefix-map
  ";" #'jg-agent-comments-comment
  "s" #'jg-agent-comments-send
  "k" #'jg-agent-comments-delete-at-point
  "d" #'jg-agent-comments-delete-at-point)

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
        (delete-overlay overlay)))
    (let ((overlay (plist-get record :highlight-overlay)))
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
    (let ((overlay (plist-get record :highlight-overlay)))
      (when (and (overlayp overlay) (overlay-buffer overlay))
        (delete-overlay overlay)))
    (setq jg-agent-comments--comments
          (delq record jg-agent-comments--comments))))

(provide 'jg-agent-comments)
;;; jg-agent-comments.el ends here

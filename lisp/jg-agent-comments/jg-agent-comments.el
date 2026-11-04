;;; jg-agent-comments.el --- Inline comments for agent-shell buffers -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience

;;; Commentary:

;; Inline comments for agent-shell chat buffers and file-visiting
;; buffers.  File comments ride along with the next chat submission in
;; the same project.

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
Each record is a plist with the shape (:snippet :text :file :line-beg
:line-end :beg :end :overlay :highlight-overlay): :beg and :end are
the buffer positions the comment refers to, :file is the anchor file
relative to the project root and :line-beg and :line-end are the
anchor's first and last line numbers (all nil in chat buffers),
:snippet is the captured buffer text, :text is the comment body,
:overlay is the overlay anchoring the record (nil until rendered),
and :highlight-overlay is the overlay tinting a partial-line selection
(nil when the selection covers whole lines).")

(defun jg-agent-comments--add (beg end text &optional snippet)
  "Record a comment about buffer text between BEG and END.
Capture the covered text (including invisible parts, without text
properties) as the record's :snippet.  Use SNIPPET as the :snippet when
SNIPPET is non-nil.  The caller captured that text earlier.  Store TEXT
as the comment body.  In a buffer visiting a file, record the anchor
file relative to the project root as :file and the anchor's first and
last line numbers as :line-beg and :line-end.  An end at the start of
a line counts as the end of the previous line.  Chat buffers record
nil for all three.  Render the record's overlay.  Push the record onto
`jg-agent-comments--comments'.  Return the record, a plist with the
shape (:snippet :text :file :line-beg :line-end :beg :end :overlay
:highlight-overlay)."
  (let* ((in-file buffer-file-name)
         (line-end-pos (if (and in-file
                                (> end beg)
                                (save-excursion (goto-char end) (bolp)))
                           (1- end)
                         end))
         (record (list :snippet (or snippet
                                    (substring-no-properties
                                     (filter-buffer-substring beg end)))
                       :text text
                       :file (when in-file
                               (file-relative-name
                                in-file (jg-agent-comments--project-root)))
                       :line-beg (when in-file (line-number-at-pos beg))
                       :line-end (when in-file
                                   (line-number-at-pos line-end-pos))
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

(defun jg-agent-comments--anchor-after-string (record)
  "Return the anchor overlay's after-string for RECORD.
A newline, then the full comment body rendered by
`jg-agent-comments--render-text': the comment shows on its own line
below the anchor, one gutter glyph per line."
  (concat "\n"
          (jg-agent-comments--render-text (plist-get record :text))))

(defun jg-agent-comments--render (record)
  "Give RECORD its anchor overlay if it has none yet.
The overlay spans the anchor text from :beg through the display
position.  It carries the `jg-agent-comments' property.  It shows the
comment on its own line below the anchor as its after-string, built by
`jg-agent-comments--anchor-after-string'.  It evaporates when a buffer
change empties it.  A partial-line selection also gets a highlight
overlay tinting exactly the selected text.  Both overlays are stored
back into the record."
  (unless (plist-get record :overlay)
    (let ((overlay (make-overlay (plist-get record :beg)
                                 (jg-agent-comments--display-position record)
                                 nil t nil)))
      (overlay-put overlay 'jg-agent-comments t)
      (overlay-put overlay 'after-string
                   (jg-agent-comments--anchor-after-string record))
      (overlay-put overlay 'evaporate t)
      (plist-put record :overlay overlay))
    (jg-agent-comments--render-highlight record)))

(defun jg-agent-comments--set-comment-text (record text)
  "Set RECORD's comment body to TEXT and refresh its anchor display.
Replace a live anchor overlay's after-string with the same construction
`jg-agent-comments--render' gives a new record, so the displayed comment
never goes stale.  Leave a dead overlay alone.  Return RECORD."
  (plist-put record :text text)
  (let ((overlay (plist-get record :overlay)))
    (when (and (overlayp overlay) (overlay-buffer overlay))
      (overlay-put overlay 'after-string
                   (jg-agent-comments--anchor-after-string record))))
  record)

(require 'comint)
(require 'project)

(defun jg-agent-comments--project-root ()
  "Return the project root directory this buffer belongs to.
The root comes from `project-current' for the directory of the file
this buffer visits, or for `default-directory' when it visits none.
Without a project, a file-visiting buffer falls back to its file's
directory and any other buffer to `default-directory'."
  (let ((dir (or (and buffer-file-name
                      (file-name-directory buffer-file-name))
                 default-directory)))
    (let ((project (project-current nil dir)))
      (if project
          (project-root project)
        dir))))

(defun jg-agent-comments--agent-text-at-point-p ()
  "Return non-nil when point is on text that can take a comment.
Buffers that do not derive from `comint-mode' are fully commentable.
Comint-derived buffers keep the agent-shell rules: the live input area
after the last prompt is not agent output.  When `comint-last-prompt'
is non-nil, refuse point at or after its end marker.  Otherwise apply
only the field check."
  (or (not (derived-mode-p 'comint-mode))
      (and (or (not (bound-and-true-p comint-last-prompt))
               (< (point) (marker-position (cdr comint-last-prompt))))
           (or (eq (get-char-property (point) 'field) 'output)
               (eq (get-char-property (max (point-min) (1- (point))) 'field) 'output)))))

(defun jg-agent-comments--region-or-line ()
  "Return the bounds (BEG . END) of the agent text to comment on.
Signal `user-error' when point is not on agent output."
  (unless (jg-agent-comments--agent-text-at-point-p)
    (user-error "Not on agent output"))
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (cons (pos-bol) (min (point-max) (1+ (pos-eol))))))

(defun jg-agent-comments--commit-edit-from-string (text beg end &optional snippet target)
  "Commit comment TEXT about the text between BEG and END.
Pass SNIPPET through to `jg-agent-comments--add'.  Whitespace-only TEXT
cancels the comment and records nothing.  With TARGET, a record, commit
in replace mode instead: overwrite that record's text and re-render its
anchor display, keeping the old comment for whitespace-only TEXT.  BEG,
END and SNIPPET play no part in replace mode."
  (unless (string-match-p "\\`[[:space:]]*\\'" (or text ""))
    (if target
        (jg-agent-comments--set-comment-text target text)
      (jg-agent-comments--add beg end text snippet))))

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

(defvar-local jg-agent-comments--edit-replace-target nil
  "Record that the edit buffer's commit overwrites, or nil.
Non-nil when the edit buffer opened to edit an existing comment:
commit replaces that record's text.  Nil when it opened for a fresh
comment, which commits by append.")

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
what the comment refers to.  In replace mode, the buffer holds a
replacement for the record in `jg-agent-comments--edit-replace-target'
instead, and the captured snippet and markers play no part."
  (interactive nil jg-agent-comments-edit-mode-map)
  (let* ((edit (current-buffer))
         (text (buffer-string))
         (source jg-agent-comments--edit-source)
         (snippet jg-agent-comments--edit-snippet)
         (bounds jg-agent-comments--edit-bounds)
         (target jg-agent-comments--edit-replace-target))
    (when (buffer-live-p source)
      (with-current-buffer source
        (if target
            (jg-agent-comments--commit-edit-from-string text nil nil nil target)
          (jg-agent-comments--commit-edit-from-string
           text
           (marker-position (car bounds))
           (marker-position (cdr bounds))
           snippet))
        (deactivate-mark)))
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
    (setq-local jg-agent-comments--edit-replace-target nil)
    (use-local-map jg-agent-comments-edit-mode-map)))

(defun jg-agent-comments-edit-at-point ()
  "Edit the comment whose anchor overlay covers point.
Open the comment buffer pre-filled with that comment's text.  Commit
overwrites the comment's text and refreshes its anchor display;
cancel keeps the old comment.  Signal `user-error' when point is not
on any comment."
  (interactive nil jg-agent-comments-mode-map)
  (let ((record (jg-agent-comments--comment-at-point)))
    (unless record
      (user-error "No comment at point"))
    (let* ((winconf (current-window-configuration))
           (source (current-buffer)))
      (pop-to-buffer (get-buffer-create "*jg-agent-comment*"))
      (erase-buffer)
      (text-mode)
      (setq-local jg-agent-comments--edit-source source)
      (setq-local jg-agent-comments--saved-winconf winconf)
      (setq-local jg-agent-comments--edit-replace-target record)
      (insert (plist-get record :text))
      (use-local-map jg-agent-comments-edit-mode-map))))

(defvar-keymap jg-agent-comments-prefix-map
  ";" #'jg-agent-comments-comment
  "e" #'jg-agent-comments-edit-at-point
  "s" #'jg-agent-comments-send
  "RET" #'jg-agent-comments-send
  "k" #'jg-agent-comments-delete-at-point
  "d" #'jg-agent-comments-delete-at-point)

(defvar-keymap jg-agent-comments-mode-map
  "C-c ;" jg-agent-comments-prefix-map)

(defun jg-agent-comments--kill-query ()
  "Ask before a kill loses this buffer's pending comments.
Return nil when pending comments exist and the user keeps the buffer;
return t otherwise, so a buffer without pending comments kills without
prompting.  On `kill-buffer-query-functions', the kill proceeds only
when this returns non-nil."
  (or (null jg-agent-comments--comments)
      (y-or-n-p "Kill buffer with pending comments? ")))

(define-minor-mode jg-agent-comments-mode
  "Inline comments for agent-shell buffers and file-visiting buffers."
  :init-value nil
  :lighter nil
  (if jg-agent-comments-mode
      (when buffer-file-name
        (add-hook 'kill-buffer-query-functions
                  #'jg-agent-comments--kill-query nil t))
    (remove-hook 'kill-buffer-query-functions
                 #'jg-agent-comments--kill-query t)))

(defun jg-agent-comments--global-enable ()
  "Turn `jg-agent-comments-mode' on in this buffer when it visits a file.
Chat buffers never visit files, so the agent-shell hook and this
global activation never overlap."
  (when buffer-file-name
    (jg-agent-comments-mode 1)))

;; Turn the buffer mode on in every file-visiting buffer.  No path
;; scoping: comment anchoring works anywhere, and magit-style buffers
;; that cannot hold anchors keep the mode harmless until a later design.
(define-globalized-minor-mode jg-agent-comments-global-mode
  jg-agent-comments-mode jg-agent-comments--global-enable)

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

(defun jg-agent-comments--same-project-p (root)
  "Return non-nil when this buffer's project root equals ROOT."
  (and root
       (equal (directory-file-name (expand-file-name root))
              (directory-file-name
               (expand-file-name
                (jg-agent-comments--project-root))))))

(defun jg-agent-comments--collect ()
  "Return the file comments that ride along with this buffer's send.
Scan every enabled buffer that visits a file in this buffer's project
and holds pending file comments.  Return the records, each paired with
its home buffer, in buffer-list order."
  (let ((candidates '())
        (contributing '()))
    (dolist (buffer (buffer-list))
      (when (and (not (eq buffer (current-buffer)))
                 (buffer-local-value 'jg-agent-comments-mode buffer)
                 (buffer-local-value 'buffer-file-name buffer)
                 (buffer-local-value 'jg-agent-comments--comments buffer))
        (push buffer candidates)))
    (when candidates
      (let ((root (jg-agent-comments--project-root)))
        (dolist (buffer candidates)
          (with-current-buffer buffer
            (when (jg-agent-comments--same-project-p root)
              (dolist (record jg-agent-comments--comments)
                (when (plist-get record :file)
                  (push (cons record buffer) contributing))))))))
    (nreverse contributing)))

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

(defconst jg-agent-comments--file-header
  "Inline comments on files:\n\n"
  "Header that opens the composed file comment section.
The file entries follow after one blank line, grouped per file in
ascending path order.")

(defun jg-agent-comments--location-string (record)
  "Return RECORD's anchor location, \"path:line\" or \"path:beg-end\"."
  (let ((file (plist-get record :file))
        (beg (plist-get record :line-beg))
        (end (plist-get record :line-end)))
    (if (and end (/= beg end))
        (format "%s:%d-%d" file beg end)
      (format "%s:%d" file beg))))

(defun jg-agent-comments--file-entry-string (record)
  "Return RECORD rendered as one entry of the file section.
The entry opens with RECORD's location line, then the snippet and
comment block from `jg-agent-comments--entry-string'."
  (concat (jg-agent-comments--location-string record) "\n"
          (jg-agent-comments--entry-string record)))

(defun jg-agent-comments--file-entry-order-p (a b)
  "Return non-nil when file record A sorts before file record B.
Records group per file in ascending path order; within a file, by
line number, then by anchor position."
  (let ((path-a (plist-get a :file))
        (path-b (plist-get b :file)))
    (if (not (string= path-a path-b))
        (string-lessp path-a path-b)
      (let ((line-a (plist-get a :line-beg))
            (line-b (plist-get b :line-beg)))
        (if (= line-a line-b)
            (< (plist-get a :beg) (plist-get b :beg))
          (< line-a line-b))))))

(defun jg-agent-comments--compose (&optional records)
  "Return RECORDS as one prompt string in buffer order.
RECORDS defaults to this buffer's pending comments.  Records without
:file are chat comments: those with a live anchor overlay sort by
:beg, the rest come last, keeping their relative order.  Records with
:file are file comments: they follow in the file section, grouped per
file in ascending path order.  Entries are separated by a blank line;
the file section opens with `jg-agent-comments--file-header' only
when file comments are among RECORDS."
  (let ((records (or records jg-agent-comments--comments))
        (chat '())
        (files '())
        (chat-live '())
        (chat-dead '()))
    (dolist (record records)
      (if (plist-get record :file)
          (push record files)
        (push record chat)))
    (dolist (record chat)
      (if (jg-agent-comments--anchor-live-p record)
          (push record chat-live)
        (push record chat-dead)))
    (let* ((chat-entries
            (and chat
                 (mapconcat #'jg-agent-comments--entry-string
                            (append (sort (nreverse chat-live)
                                          (lambda (a b)
                                            (< (plist-get a :beg)
                                               (plist-get b :beg))))
                                    (nreverse chat-dead))
                            "\n\n")))
           (file-entries
            (and files
                 (concat jg-agent-comments--file-header
                         (mapconcat #'jg-agent-comments--file-entry-string
                                    (sort files
                                          #'jg-agent-comments--file-entry-order-p)
                                    "\n\n")))))
      (cond
        ((and chat-entries file-entries)
         (concat chat-entries "\n\n" file-entries))
        (chat-entries chat-entries)
        (file-entries file-entries)
        (t "")))))

(defconst jg-agent-comments--block-header
  "---\nInline comments replying to specific parts of your previous message:\n"
  "Header that introduces the composed comment block below a prompt.
A separator line, then a line that tells the agent what the block
holds.  Ends with a newline; the composed entries follow directly.")

(defun jg-agent-comments--augment-prompt (input)
  "Return INPUT with the pending comments appended as a block.
The block holds this buffer's own comments plus the file comments that
`jg-agent-comments--collect' gathers from same-project file buffers.
When nothing is pending, return INPUT unchanged.  Otherwise append to
INPUT a blank line, then the composed block, which carries the chat
block header only when chat comments are among them.  A nil or
whitespace-only INPUT drops the input part: the return value is the
block alone."
  (let ((collected (jg-agent-comments--collect)))
    (if (and (null jg-agent-comments--comments) (null collected))
        input
      (let* ((records (append jg-agent-comments--comments
                              (mapcar #'car collected)))
             (chat-p (seq-some (lambda (record)
                                 (null (plist-get record :file)))
                               records))
             (block (jg-agent-comments--compose records))
             (block (if chat-p
                        (concat jg-agent-comments--block-header block)
                      block)))
        (if (or (null input) (string-empty-p (string-trim input)))
            block
          (concat input "\n\n" block))))))

(defun jg-agent-comments--delete-record-overlays (records)
  "Delete the anchor and highlight overlays of RECORDS."
  (dolist (record records)
    (let ((overlay (plist-get record :overlay)))
      (when (and (overlayp overlay) (overlay-buffer overlay))
        (delete-overlay overlay)))
    (let ((overlay (plist-get record :highlight-overlay)))
      (when (and (overlayp overlay) (overlay-buffer overlay))
        (delete-overlay overlay)))))

(defun jg-agent-comments--clear ()
  "Forget all pending comments and delete their overlays.
Also clear the file buffers whose pending comments ride along with
this buffer's own: their overlays are deleted and their records are
dropped in the home buffers."
  (jg-agent-comments--delete-record-overlays jg-agent-comments--comments)
  (setq jg-agent-comments--comments nil)
  (dolist (contributing (jg-agent-comments--collect))
    (let ((buffer (cdr contributing)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (jg-agent-comments--delete-record-overlays
           (list (car contributing)))
          (setq jg-agent-comments--comments
                (delq (car contributing)
                      jg-agent-comments--comments)))))))

(defun jg-agent-comments-send ()
  "Submit all pending comments to the agent as one prompt, then clear.
The prompt carries the block header above the composed entries and no
other text.  Say so and do nothing when no comments are pending here
or in same-project file buffers.  In a buffer that does not derive
from `comint-mode', say that the comments ride along with the next
chat submission instead and do nothing."
  (interactive)
  (cond
   ((not (derived-mode-p 'comint-mode))
    (message "Comments here ride along: submit them from an agent-shell chat to send them"))
   ((and (null jg-agent-comments--comments)
         (null (jg-agent-comments--collect)))
    (message "No pending comments"))
   (t
    (agent-shell-prompt-queue (jg-agent-comments--augment-prompt nil))
    (jg-agent-comments--clear))))

(defun jg-agent-comments--comment-at-point ()
  "Return the record whose live anchor overlay covers point.
A record matches when its anchor overlay is live, carries the
`jg-agent-comments' property, and spans point.  When several anchor
overlays cover point, the most recently added comment wins.  Return
nil when no record matches."
  (seq-find
   (lambda (record)
     (let ((overlay (plist-get record :overlay)))
       (and (overlayp overlay)
            (overlay-buffer overlay)
            (overlay-get overlay 'jg-agent-comments)
            (>= (point) (overlay-start overlay))
            (<= (point) (overlay-end overlay)))))
   jg-agent-comments--comments))

(defun jg-agent-comments-delete-at-point ()
  "Delete the comment whose anchor overlay covers point.
Delete the record's overlay when it is still live and remove the
record from `jg-agent-comments--comments'.  Signal `user-error' when
point is not on any comment."
  (interactive nil jg-agent-comments-mode-map)
  (let ((record (jg-agent-comments--comment-at-point)))
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

(declare-function agent-shell-submit "agent-shell" ())

(defun jg-agent-comments--splice-input (pmark text)
  "Replace the live input area, which starts at PMARK, with TEXT.
PMARK is the process mark of the shell buffer's process.  Keep PMARK
at the start of the inserted TEXT, where `comint-send-input' expects
the input area to start."
  (save-excursion
    (let ((start (marker-position pmark)))
      (goto-char start)
      (let ((inhibit-read-only t))
        (delete-region start (point-max))
        (insert text))
      (set-marker pmark start))))

(defun jg-agent-comments--submit-advice (orig &rest _)
  "ORIG is `agent-shell-submit': make pending comments ride along with RET.
RET in an agent-shell buffer runs `agent-shell-submit', which reads
the typed input from the live input area: the process mark through
the end of the buffer.

When this buffer has the mode on, pending comments exist here or in
same-project file buffers, and typed input sits at the prompt, splice
the augmented prompt from `jg-agent-comments--augment-prompt' into
the input area before ORIG runs.  ORIG submits that text, so the
transcript and the history show what the agent received.  Clear the
comments after ORIG submits.

Every other case calls ORIG untouched: the mode off, no process, no
typed input, and no comments pending here or in same-project file
buffers keep the regular submit path exactly as it was.

When ORIG signals, restore the typed input when the input area still
holds the spliced text.  `agent-shell-submit' refuses before it
commits the input, so this is the normal refusal path: the typed
input stays editable, the comments stay pending, and they ride along
the next submission.  When the input area changed, the submission
committed; leave the buffer alone.  Re-signal either way."
  (let* ((buffer (current-buffer))
         (proc (get-buffer-process buffer)))
    (if (not (and (bound-and-true-p jg-agent-comments-mode)
                  proc
                  (or jg-agent-comments--comments
                      (jg-agent-comments--collect))))
        (funcall orig)
      (let* ((pmark (process-mark proc))
             (input (buffer-substring-no-properties
                     (marker-position pmark) (point-max))))
        (if (string-empty-p (string-trim input))
            (funcall orig)
          (let ((augmented (jg-agent-comments--augment-prompt input)))
            (condition-case err
                (progn
                  (jg-agent-comments--splice-input pmark augmented)
                  (let ((result (funcall orig)))
                    (jg-agent-comments--clear)
                    result))
              (error
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when (string= augmented
                                  (buffer-substring-no-properties
                                   (marker-position pmark) (point-max)))
                     (jg-agent-comments--splice-input pmark input))))
               (signal (car err) (cdr err))))))))))

;; `advice-add' holds this advice on the still-undefined symbol; it
;; applies when agent-shell later defines `agent-shell-submit'.
(advice-add 'agent-shell-submit :around #'jg-agent-comments--submit-advice)

(provide 'jg-agent-comments)
;;; jg-agent-comments.el ends here

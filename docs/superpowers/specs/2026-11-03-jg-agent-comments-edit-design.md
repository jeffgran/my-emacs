# Edit an existing comment (`C-c ; e`)

Date: 2026-11-03
Package: `lisp/jg-agent-comments`
Status: approved (short design)

## Intent and why

Comments are short edits of intent. When a comment says the wrong thing, the
user should fix it in place instead of deleting it and typing it again. Edit
reuses the existing comment buffer and its commit/cancel convention.

## Goals

- `C-c ; e` with point on a comment's anchor opens the comment buffer with the
  comment text pre-filled.
- `C-c C-c` saves the buffer text over the old comment and refreshes the
  displayed comment in the chat buffer.
- `C-c C-k` cancels and keeps the old comment unchanged.

## Non-goals

- Changing the snippet or anchor. Re-anchoring means delete plus re-comment.
- Editing comments whose anchor overlay evaporated (same rule as delete).

## Constraints

- The comment body lives in the anchor overlay's `after-string`, written once
  at creation. Commit must regenerate it or the display goes stale.
- The commit path in the edit buffer is hardwired to append a new comment. It
  must take a callback so "replace" and "append" share one buffer and one
  keymap.

## Decisions

- Point lookup is the same matcher delete uses: point within the anchor
  overlay. The cursor cannot sit on the comment line, so "point on a comment"
  and "point on the line it refers to" are one case. Region and partial
  selection anchors already work through the same span.
- Multiple comments on one anchor line: edit picks the same record delete
  picks, the most recent. To edit an older one, delete and re-comment.
- Committing whitespace-only text keeps the old comment. This matches how a
  fresh comment silently discards empty text.
- The comment stays visible while the edit buffer is open. Commit replaces it
  in place.
- Overwrite sets the record's `:text` and regenerates the anchor overlay's
  `after-string` from a generalized render call.

## Left to the builder

- Exact internal decomposition (callback shape, render generalization).
- Test structure, following the existing ERT patterns in the test file.

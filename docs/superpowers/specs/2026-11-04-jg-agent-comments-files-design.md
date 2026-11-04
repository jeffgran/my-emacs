# Comment on files (plan files and code) — Design

Date: 2026-11-04
Package: `lisp/jg-agent-comments`
Status: approved

## Intent and why

The user reviews plan files and code in Emacs and wants the same
comment-anchor-reply workflow that exists in agent-shell chat buffers,
applied to any file-visiting buffer. The comments must reach the agent
through the chat, framed well enough that the agent can find the exact
place in the file.

## Goals

- `C-c ; ;` on a line or region in any file-visiting buffer records a
  comment anchored there, displayed below the line, exactly as in chat.
- File comments carry the file path and line range, so the agent sees
  `lib/foo.js:415-418:` plus the highlighted snippet plus the comment.
- Delivery is ride-along only: a submission in an agent-shell chat buffer
  collects pending file comments from the same project and sends them with
  the chat's own comments. The user picks the session by submitting in it.
- Comments are short-lived: they clear on successful send. They are not
  tracked per session.

## Non-goals

- Sending comments from a file buffer (`C-c ; s` refuses there).
- Magit diff buffers. Anchoring into a magit buffer dies on refresh;
  a later design will anchor into visited files or section identities.
- Per-session comment targeting.

## Decisions

- Gate: buffers that derive from `comint-mode` keep today's rules
  (field `output`, prompt exclusion). Every other buffer with the mode on
  is fully commentable.
- Activation: a global companion mode turns the buffer-local mode on in
  every file-visiting buffer. No path scoping. Chat buffers never visit
  files, so the two activations do not overlap.
- Record gains `:file` (path relative to the project root, nil in chat),
  `:line-beg`, `:line-end` (line numbers at creation). Dead anchors keep
  the stored location, so a comment survives the agent rewriting the file.
- Compose: chat comments keep today's header and format. File comments
  follow in a second section, grouped per file in ascending path order,
  each entry opened by a `path:line` or `path:beg-end` line, then the
  `> snippet` lines, then the comment. Each section header appears only
  when that section has entries.
- Collection: a submission in a chat buffer reads that buffer's records
  plus records from every enabled file buffer whose project root equals
  the chat buffer's project root (`project-current`, falling back to
  `default-directory`).
- Clear on success reaches into the contributing file buffers. A refused
  send keeps every comment pending, as today.
- Kill safety: killing a file buffer with pending comments asks first.
- `C-c ; s` in a non-chat buffer refuses with a message naming the
  ride-along flow.

## Left to the builder

- Exact internal decomposition of collection and clearing.
- Test fixtures for temp file buffers, following existing patterns.

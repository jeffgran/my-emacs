# Design: Inline agent comments (`jg-agent-comments`)

Date: 2026-11-03
Status: draft

## Intent and why

Responding to an AI agent's chat answers means manually copying passages into
block quotes. This package replaces that with a PR-review-style flow: move
point into a response, attach a comment anchored to that spot, scatter several
comments, then send them all as one composed reply. The goal is the email
inline-reply / GitHub PR review idiom, without any copy/paste.

Ultimate destination is "comment on anything" — chat responses, code files,
diffs. Phase one is chat-buffer only: it is self-contained, easiest to build,
and the surface with the most immediate value. A later phase tests whether the
habit sticks before investing in the harder anchoring problems (files change,
diffs regenerate).

## Goals (MVP)

- Attach a comment to any line or region of an `agent-shell` chat buffer —
  including folded or otherwise invisible content, which is present buffer
  text and anchors normally.
- Enter the comment in a temporary buffer using the magit convention: `C-c`
  commits, `C-k` cancels. Multi-line comments are allowed.
- Display each comment anchored in place: a gutter/margin marker plus a
  one-line inline rendering of the comment's first line, with an elision
  indicator when the comment has more lines than shown.
- Show a count of pending comments (per buffer) in the mode line.
- One "send comments" command that composes every pending comment into a
  single blockquoted message and submits it as a normal prompt.
- Comments are consumed on send: markers clear, count resets.

## Non-goals (MVP)

- No dashboard/list view (a review-queue buffer with jump, edit, delete is a
  fast follow once the flow proves out).
- No commenting on code files or diffs.
- No persistence across buffer recreation or Emacs restarts.
- No editing UI beyond delete-and-re-add.

## Anti-goals

- Forking or patching agent-shell.
- Any structured annotation protocol: ACP has no annotation concept, so
  plain-text blockquote composition is the whole wire format.

## Constraints

- agent-shell re-renders chat buffers as responses stream; anchoring must
  tolerate edits to buffer content between comment and send.
- Overlay `display` strings must stay single-line (multi-line display strings
  hang Emacs redisplay; known agent-shell issue).
- Reading agent-shell private state (`agent-shell--state` etc.) is acceptable;
  existing user code already does this.
- Style: new package under `lisp/jg-agent-comments/`, wired via straight
  local-repo in `jg-straight.el`; minor mode keymap registered in
  `jg-mode-emulation-alist`; faces defined against the tokyo-night theme.

## Approaches considered

1. **Fork/patch agent-shell** — rejected: tracks upstream poorly and the
   feature is additive, not a change to agent-shell behavior.
2. **Companion package: overlays + markers + a thin send command** — chosen.
   Overlays anchored with markers survive buffer edits, and a companion
   package can advise/read agent-shell without owning it.
3. **ACP protocol extension** — rejected: no annotation concept exists in the
   Agent Client Protocol; anything sent must serialize to prompt text anyway.

## Design

- New Elisp package `jg-agent-comments`: a buffer-local minor mode
  auto-enabled from `agent-shell-mode-hook`, with its keymap registered in
  `jg-mode-emulation-alist`.
- Data model: a comment owns its own copy of the quoted snippet, captured at
  comment time from the raw buffer text (invisible text included). The
  overlay is a best-effort display anchor only. If agent-shell later deletes
  or rewrites the anchored text (folding, markdown re-render), the comment
  survives with its stored snippet and still goes out on send.
- "Comment at point" command: uses the active region, else the line at
  point; opens a temp buffer for the comment text with the magit
  commit-buffer convention (`C-c` to commit, `C-k` to cancel).
- Multi-line comments are stored in full; display truncates to the first
  line with an elision indicator.
- "Send comments" command: walks pending comments in buffer order (comments
  whose anchor died sort last), renders each as a quoted snippet followed by
  the comment, submits the composed message through agent-shell's normal
  prompt path (the prompt queue already handles the busy case), then deletes
  the overlays.
- Pending count renders via a `global-mode-string` entry (the config's
  doom-modeline setup does not display plain minor-mode lighters);
  suppressed when zero.
- Faces: marker face, inline comment face, quoted-snippet face.

## Left to the builder

- Exact keys (candidate: a two-key sequence on the existing jg keymaps).
- Whether gutter marking uses the fringe, display margin, or a prefix
  overlay — pick what survives agent-shell's rendering best.

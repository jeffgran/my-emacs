# Edit an Existing Comment — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `C-c ; e` on a comment's anchor opens the comment buffer pre-filled; `C-c C-c` overwrites that comment's text and refreshes its display; `C-c C-k` cancels unchanged.

**Architecture:** Reuse the delete-at-point matcher to find the target record. Extend the edit-buffer flow with a "replace" mode: a buffer-local target record that turns the commit callback from append into overwrite. Overwrite sets the record's `:text` and regenerates the anchor overlay's `after-string`, so the display never goes stale.

**Tech Stack:** Emacs Lisp, ERT batch tests.

**Spec:** `docs/superpowers/specs/2026-11-03-jg-agent-comments-edit-design.md`

## Global Constraints

- Branch `feature/jg-agent-comments`, repo `/Users/jgran/.emacs.d`. No worktree, no push.
- Only files under `lisp/jg-agent-comments/` change.
- Test command: `emacs --batch -Q -l /Users/jgran/.emacs.d/lisp/jg-agent-comments/test/helper.el -f ert-run-tests-batch-and-exit`
- TDD: each task writes the failing test, watches it fail, implements, full suite green, commit.
- `defvar-keymap` never rebinds a live variable: new keymap entries land in the existing `defvar-keymap` form (fresh loads and batch tests get them).

## Review Focus

- Point at end-of-line vs start of the next line on a whole-line anchor: matcher includes the line's newline position, excludes the next line's start. Test in Task 1.
- Anchor overlay evaporated (buffer text rewritten): `C-c ; e` errors `No comment at point`, no buffer opens. Test in Task 3.
- Whitespace-only edit commit: old comment kept, display unchanged, edit buffer still closes. Test in Task 3.
- Comment on the last line of the buffer (no trailing newline): matcher still finds it. Test in Task 1.

---

### Task 1: Extract the shared comment-at-point matcher

**Files:**
- Modify: `lisp/jg-agent-comments/jg-agent-comments.el` (delete-at-point, ~lines 352-376)
- Test: `lisp/jg-agent-comments/test/jg-agent-comments-test.el`

**Interfaces:**
- Produces: `jg-agent-comments--comment-at-point` — no args, returns the record plist whose live anchor overlay carries point, or nil. Behavior identical to the current inline match in `jg-agent-comments-delete-at-point`.

- [ ] **Step 1: Write the failing test**

In the test file, next to the existing delete-at-point tests, reuse the same fixture pattern they use. Add `jg-agent-comments-matcher-finds-comment`, `jg-agent-comments-matcher-end-of-line-inclusive` (point at the newline position of a whole-line anchor matches), `jg-agent-comments-matcher-next-line-exclusive` (point at start of the following line does not match), and `jg-agent-comments-matcher-last-line-of-buffer` (anchor on the final line, no trailing newline, matches). Assertions are normative; fixture lines follow the existing delete tests' setup.

- [ ] **Step 2: Run tests, expect FAIL** (`jg-agent-comments--comment-at-point` not defined; the fixture-based ones pass or fail only via the indirection added in Step 3 — write the tests against the new function name).

- [ ] **Step 3: Implement** `jg-agent-comments--comment-at-point`: move the inline `seq-find` match out of `jg-agent-comments-delete-at-point` into this function; delete calls it and errors `user-error "No comment at point"` on nil. No behavior change.

- [ ] **Step 4: Full suite green.** Commit: `jg-agent-comments: extract shared comment-at-point matcher`

### Task 2: Overwrite text and re-render the anchor overlay

**Files:**
- Modify: `lisp/jg-agent-comments/jg-agent-comments.el` (render helpers ~lines 58-124)
- Test: `lisp/jg-agent-comments/test/jg-agent-comments-test.el`

**Interfaces:**
- Consumes: record plists as defined by `--add`.
- Produces: `jg-agent-comments--set-comment-text` (record, text) — `plist-put`s `:text`, then, if `:overlay` is a live overlay, replaces its `after-string` with the same construction `--render` uses (`"\n"` plus `--render-text` of the record). `--render` refactored to build its `after-string` through that same construction so there is one copy of it.

- [ ] **Step 1: Write the failing tests** next to the render tests: `jg-agent-comments-set-text-updates-record` (record's `:text` equals the new text), `jg-agent-comments-set-text-rerenders-overlay` (overlay `after-string` contains the new text, not the old), `jg-agent-comments-set-text-dead-overlay-no-error` (record whose overlay was deleted; call must not signal).

- [ ] **Step 2: Run tests, expect FAIL.**

- [ ] **Step 3: Implement** as specified. Keep the gutter glyph inside the regenerated `after-string` (it comes from `--render-text`).

- [ ] **Step 4: Full suite green.** Commit: `jg-agent-comments: set-comment-text re-renders anchor overlay`

### Task 3: Replace-mode edit buffer, `jg-agent-comments-edit-at-point`, `e` binding

**Files:**
- Modify: `lisp/jg-agent-comments/jg-agent-comments.el` (edit-buffer locals ~lines 168-232, prefix map ~lines 234-239)
- Test: `lisp/jg-agent-comments/test/jg-agent-comments-test.el`

**Interfaces:**
- Consumes: `jg-agent-comments--comment-at-point` (Task 1), `jg-agent-comments--set-comment-text` (Task 2), existing edit-buffer locals and `--finish-edit`.
- Produces: interactive `jg-agent-comments-edit-at-point`; prefix-map entry `"e"` → it; buffer-local `jg-agent-comments--edit-replace-target` (record or nil) read by `--commit-edit-from-string`.

- [ ] **Step 1: Write the failing tests**, following the existing edit-buffer tests' fixture pattern:

  - `jg-agent-comments-edit-binding-e` — `(should (eq (keymap-lookup jg-agent-comments-mode-map "C-c ; e") 'jg-agent-comments-edit-at-point))`.
  - `jg-agent-comments-edit-no-comment-errors` — point off any anchor: signals `user-error "No comment at point"`, and no `*jg-agent-comment*` buffer appears.
  - `jg-agent-comments-edit-prefills-text` — comment created with known text; `C-c ; e`; buffer contains that text.
  - `jg-agent-comments-edit-commit-overwrites` — change text, `C-c C-c`: record's `:text` is the new text, overlay `after-string` shows it, and the comment list still has the same length (replace, not append).
  - `jg-agent-comments-edit-cancel-preserves` — change text, `C-c C-k`: record's `:text` and overlay unchanged, record still in the list.
  - `jg-agent-comments-edit-whitespace-commit-preserves` — replace text with only whitespace, `C-c C-c`: record's `:text` and overlay unchanged, buffer closes.
  - `jg-agent-comments-edit-dead-anchor-errors` — delete the record's overlay, call edit at the old position: `user-error "No comment at point"`.

- [ ] **Step 2: Run tests, expect FAIL.**

- [ ] **Step 3: Implement:**
  - `jg-agent-comments-edit-at-point`: `--comment-at-point`; nil → `user-error "No comment at point"`. Otherwise open `*jg-agent-comment*` exactly as `jg-agent-comments-comment` does (window-config save, buffer-locals), but instead of snippet/bounds work, set `--edit-replace-target` to the record and insert the record's `:text` as the buffer contents.
  - `--commit-edit-from-string`: when `--edit-replace-target` is non-nil, overwrite via `--set-comment-text` unless the text is whitespace-only (then do nothing); when nil, keep the existing append behavior. `--commit-edit` and `--finish-edit` unchanged.
  - Prefix map: `"e"` → `jg-agent-comments-edit-at-point`.

- [ ] **Step 4: Full suite green.** Commit: `jg-agent-comments: edit existing comment with C-c ; e`

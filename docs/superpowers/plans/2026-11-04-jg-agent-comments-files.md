# Comment on Files — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The full comment workflow (`C-c ; ;`, `e`, `k`/`d`, send) works in any file-visiting buffer, and pending file comments ride along on the next chat submission in the same project.

**Architecture:** The record/overlay/edit layer is already buffer-agnostic; this plan widens the one chat-specific gate, adds a global activation mode, records file location on each comment, extends composition with a per-file section, and makes send collect and clear across same-project buffers.

**Tech Stack:** Emacs Lisp, ERT batch tests.

**Spec:** `docs/superpowers/specs/2026-11-04-jg-agent-comments-files-design.md`

## Global Constraints

- Branch `feature/jg-agent-comments`, repo `/Users/jgran/.emacs.d`. No worktree, no push.
- Task files as listed per task; the wiring task also touches `jg/jg-straight.el`.
- Test command: `emacs --batch -Q -l /Users/jgran/.emacs.d/lisp/jg-agent-comments/test/helper.el -f ert-run-tests-batch-and-exit`
- TDD per task: failing test first, full suite green, commit.
- Suite is at 63 tests.

## Review Focus

- Chat-buffer behavior must not change: every chat-path test in the suite is the regression net. Run the full suite, never a single test.
- A submission refused by the agent must keep file-buffer comments pending. Test in Task 5.
- Two file buffers in one project compose as two groups; a file buffer outside the project is ignored. Test in Task 5.
- Killing a file buffer with no pending comments must not prompt. Test in Task 6.
- A plain comint buffer (not agent-shell) stays refused. Test in Task 1.

---

### Task 1: Generalize the commentable gate

**Files:**
- Modify: `lisp/jg-agent-comments/jg-agent-comments.el` (`--agent-text-at-point-p`, ~:145-153)
- Test: `lisp/jg-agent-comments/test/jg-agent-comments-test.el`

**Interfaces:**
- Produces: `--agent-text-at-point-p` returns t in any buffer that does not derive from `comint-mode`; comint-derived buffers keep today's rules unchanged.

- [ ] **Step 1: Failing tests** in a temp file buffer with the mode on: comment creation succeeds on a line, on a multi-line region, and on the last line of the buffer. Also: a plain comint buffer (no field property) stays refused with `Not on agent output`.
- [ ] **Step 2: FAIL.** The file-buffer tests currently signal the user-error.
- [ ] **Step 3: Implement** the two-branch gate via `derived-mode-p`.
- [ ] **Step 4: Full suite green.** Commit: `jg-agent-comments: allow comments in non-comint buffers`

### Task 2: Global activation mode + wiring

**Files:**
- Modify: `lisp/jg-agent-comments/jg-agent-comments.el` (after the mode definition ~:301-304)
- Modify: `jg/jg-straight.el` (enable it next to the existing `agent-shell-mode-hook` line)
- Test: `lisp/jg-agent-comments/test/jg-agent-comments-test.el`

**Interfaces:**
- Produces: `jg-agent-comments-global-mode`, a `define-globalized-minor-mode` whose predicate enables `jg-agent-comments-mode` only in file-visiting buffers.

- [ ] **Step 1: Failing tests:** with the global mode on, a temp file buffer gets the mode; a non-file buffer (e.g. `*scratch*`-like) does not; the agent-shell-mode hook path is untouched.
- [ ] **Step 2: FAIL.**
- [ ] **Step 3: Implement** the global mode; add `(jg-agent-comments-global-mode 1)` to `jg/jg-straight.el` inside the `use-package` block.
- [ ] **Step 4: Full suite green.** Commit: `jg-agent-comments: global activation in file-visiting buffers`

### Task 3: Record file location at comment time

**Files:**
- Modify: `lisp/jg-agent-comments/jg-agent-comments.el` (`--add` ~:38-56)
- Test: `lisp/jg-agent-comments/test/jg-agent-comments-test.el`

**Interfaces:**
- Produces: records gain `:file` (relative path from the project root via `project-current`, falling back to the file's directory; nil in chat), `:line-beg`, `:line-end` (line numbers at creation, nil in chat).

- [ ] **Step 1: Failing tests:** a comment on line 3 of a temp file records the relative path, `:line-beg` 3, `:line-end` 3; a region spanning lines records both; a chat-buffer comment records nil file and nil lines.
- [ ] **Step 2: FAIL.**
- [ ] **Step 3: Implement** in `--add` only.
- [ ] **Step 4: Full suite green.** Commit: `jg-agent-comments: record file and line range on comments`

### Task 4: Compose the file section

**Files:**
- Modify: `lisp/jg-agent-comments/jg-agent-comments.el` (`--compose`/`--entry-string`/header constants ~:332-385)
- Test: `lisp/jg-agent-comments/test/jg-agent-comments-test.el`

**Interfaces:**
- Consumes: `:file`, `:line-beg`, `:line-end` from Task 3.
- Produces: `--compose` output gains the `Inline comments on files:` section per the spec: grouped per file in ascending path order, each entry `path:line` or `path:beg-end`, then `> snippet` lines, then two-space comment lines. Chat header only when chat entries exist; file header only when file entries exist.

- [ ] **Step 1: Failing tests** pinning exact strings: single-line entry, range entry, two files grouped in path order, mixed chat+file sections in order, file-only (no chat header), and a dead-anchor file comment composing with its stored location.
- [ ] **Step 2: FAIL.**
- [ ] **Step 3: Implement.** Keep chat entry format byte-identical — existing compose tests are the net.
- [ ] **Step 4: Full suite green.** Commit: `jg-agent-comments: compose file comments with location and snippet`

### Task 5: Cross-buffer collect, clear, and the send refusal

**Files:**
- Modify: `lisp/jg-agent-comments/jg-agent-comments.el` (`--augment-prompt` ~:372-385, `--clear` ~:387-396, `send` ~:398-407)
- Test: `lisp/jg-agent-comments/test/jg-agent-comments-test.el`

**Interfaces:**
- Consumes: `:file` records (Task 3), composed file section (Task 4).
- Produces: collection that pairs each record with its home buffer; clearing deletes overlays in the home buffers; `send` in a non-comint buffer refuses with a message naming the ride-along flow.

- [ ] **Step 1: Failing tests:** chat buffer with one comment plus two same-project file buffers with comments — the augmented prompt (via the existing prompt-queue test seam or `--augment-prompt` directly, as current tests do) contains all three; clearing empties all three buffers. A file buffer whose project root differs contributes nothing. Refused send (existing refusal seam) restores input and keeps file comments pending. `send` invoked in a file buffer refuses without queueing.
- [ ] **Step 2: FAIL.**
- [ ] **Step 3: Implement.** Resolve both project roots the same way `--add` does.
- [ ] **Step 4: Full suite green.** Commit: `jg-agent-comments: ride-along collects and clears same-project file comments`

### Task 6: Kill safety

**Files:**
- Modify: `lisp/jg-agent-comments/jg-agent-comments.el` (mode definition ~:301-304)
- Test: `lisp/jg-agent-comments/test/jg-agent-comments-test.el`

**Interfaces:**
- Produces: `jg-agent-comments--kill-query` on the buffer-local `kill-buffer-query-functions` while the mode is on; returns nil when pending comments exist and the user says no; t otherwise.

- [ ] **Step 1: Failing tests:** with pending comments and `cl-letf`-mocked `y-or-n-p` answering no, the query function returns nil; answering yes returns t; with no pending comments it returns t without prompting; killing the buffer after a clear does not prompt.
- [ ] **Step 2: FAIL.**
- [ ] **Step 3: Implement** — add/remove the hook in the mode body.
- [ ] **Step 4: Full suite green.** Commit: `jg-agent-comments: ask before killing a buffer with pending comments`

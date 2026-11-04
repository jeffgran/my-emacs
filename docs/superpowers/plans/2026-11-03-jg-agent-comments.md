# jg-agent-comments Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Inline PR-review-style comments anchored in agent-shell chat buffers, with a batch "send comments as one prompt" command.

**Architecture:** A standalone companion package (`jg-agent-comments`) — no agent-shell modifications. The source of truth is a buffer-local list of comment records; each record owns a display-accurate snippet captured at comment time plus a best-effort overlay anchor for display. Send composes records into a blockquoted message and submits via `agent-shell-prompt-queue`.

**Tech Stack:** Emacs Lisp (Emacs 31), ERT tests run in batch, straight.el local-repo packaging, agent-shell public API (`agent-shell-prompt-queue`) and built-in `filter-buffer-substring`.

**Spec:** `docs/superpowers/specs/2026-11-03-agent-inline-comments-design.md`

## Global Constraints

- Package root: `/Users/jgran/.emacs.d/lisp/jg-agent-comments/jg-agent-comments.el`; tests in `/Users/jgran/.emacs.d/lisp/jg-agent-comments/test/`.
- Package must load without agent-shell present: `(require 'agent-shell nil t)` plus `declare-function` for `agent-shell-prompt-queue`. No hard require at top level.
- Overlay/carrier display strings must never contain newlines (multi-line `display` strings hang Emacs redisplay).
- Snippets come from `filter-buffer-substring` (display-accurate, keeps folded bodies, strips properties) — never `buffer-substring-no-properties`.
- Comment data record shape (used across all tasks): a plist `(:snippet STRING :text STRING :overlay OVERLAY-or-nil)`.
- Keymap conventions: buffer-local minor mode map named `jg-agent-comments-mode-map`; registered in `jg-mode-emulation-alist` in `/Users/jgran/.emacs.d/jg/jg-keys.el`; faces defined with `defface`, themed for tokyo-night but with sane defaults.
- Test run command for every task:
  `emacs --batch -Q -l /Users/jgran/.emacs.d/lisp/jg-agent-comments/test/helper.el -f ert-run-tests-batch-and-exit`
  `helper.el` adds `lisp/jg-agent-comments` to `load-path` and stubs nothing else; tests stub agent-shell with `cl-letf` where needed.
- Commit messages: `jg-agent-comments: <summary>`.

## Review Focus

- Comment on folded text: the snippet must include the hidden lines under the fold, because folded content is present buffer text. Test in Task 2.
- agent-shell deletes/rewrites anchored text (folds, markdown re-render, fragment updates) after a comment is made: the comment must survive and still be sent, using its stored snippet. Test in Task 2 and Task 6.
- Send while the agent is busy: must queue silently, never error — `agent-shell-prompt-queue` handles this, so the send command must use it and not `agent-shell--insert-to-shell-buffer` (which signals "Busy, try later"). Pinned by cl-letf test in Task 6.
- Commenting on the user's own live prompt input: must refuse. Input text has no `field` text property; agent-rendered text has `field` = `output`. Pinned in Task 3.
- Committing an empty comment buffer (`C-c C-c` on empty): treated as cancel, no record created. Pinned in Task 3.

---

### Task 1: Package scaffold, minor mode, faces

**Files:**
- Create: `/Users/jgran/.emacs.d/lisp/jg-agent-comments/jg-agent-comments.el`
- Create: `/Users/jgran/.emacs.d/lisp/jg-agent-comments/test/helper.el`
- Create: `/Users/jgran/.emacs.d/lisp/jg-agent-comments/test/jg-agent-comments-test.el`

**Interfaces:**
- Produces: `jg-agent-comments-mode` (buffer-local minor mode, `:init-value nil`, lighter `nil`), `jg-agent-comments-mode-map` (empty for now), faces `jg-agent-comments-marker`, `jg-agent-comments-inline`, `jg-agent-comments-snippet`, and `jg-agent-comments--comments` (buffer-local list, default nil).

- [ ] **Step 1: Write the failing test**

In `test/jg-agent-comments-test.el`:

```elisp
(ert-deftest jg-agent-comments-mode-enables ()
  (with-temp-buffer
    (jg-agent-comments-mode 1)
    (should jg-agent-comments-mode)))
```

`test/helper.el` contains only:

```elisp
(add-to-list 'load-path
             (expand-file-name ".." (file-name-directory load-file-name)))
(require 'jg-agent-comments)
```

- [ ] **Step 2: Run test to verify it fails**

Run: the Global Constraints test command.
Expected: FAIL — `jg-agent-comments-mode` not defined (file does not exist yet; the run errors).

- [ ] **Step 3: Implement the scaffold**

`jg-agent-comments.el`: header, group `jg-agent-comments`, the three `defface` definitions (marker: fringe/todo-style warning color; inline: comment-face style; snippet: dimmed), `(defvar-local jg-agent-comments--comments nil)`, `jg-agent-comments-mode-map` via `defvar-keymap` (empty body), and `define-minor-mode jg-agent-comments-mode` with docstring "Inline comments for agent-shell buffers." and `:lighter nil`. The mode does nothing else yet. Soft-require agent-shell at the bottom with `declare-function` for `agent-shell-prompt-queue` above it.

- [ ] **Step 4: Run test to verify it passes**

Run: the Global Constraints test command.
Expected: PASS

- [ ] **Step 5: Commit**

```bash
cd /Users/jgran/.emacs.d
git add lisp/jg-agent-comments
git commit -m "jg-agent-comments: package scaffold, minor mode, faces"
```

---

### Task 2: Comment records, snippet capture, anchoring

**Files:**
- Modify: `/Users/jgran/.emacs.d/lisp/jg-agent-comments/jg-agent-comments.el`
- Modify: `/Users/jgran/.emacs.d/lisp/jg-agent-comments/test/jg-agent-comments-test.el`

**Interfaces:**
- Consumes: `jg-agent-comments--comments` (Task 1).
- Produces: `(jg-agent-comments--add beg end text)` → returns the record plist and pushes it onto `jg-agent-comments--comments`; `(jg-agent-comments--render record)` → creates the display overlay and stores it in the record's `:overlay`. Overlay is made with `(make-overlay beg end nil t nil)` (front-advance) and carries property `jg-agent-comments t`.

- [ ] **Step 1: Write the failing tests**

```elisp
(ert-deftest jg-agent-comments-add-captures-invisible-text ()
  (with-temp-buffer
    (insert "visible\n")
    (insert "hidden-under-fold\n")
    (insert "visible-after\n")
    (put-text-property 9 25 'invisible t)  ; the fold
    (jg-agent-comments--add 1 (point-max) "why?")
    (should (equal (plist-get (car jg-agent-comments--comments) :snippet)
                   "visible\nhidden-under-fold\nvisible-after\n"))))

(ert-deftest jg-agent-comments-add-stores-comment-text ()
  (with-temp-buffer
    (insert "some agent prose")
    (jg-agent-comments--add 1 (point-max) "line one\nline two")
    (should (equal (plist-get (car jg-agent-comments--comments) :text)
                   "line one\nline two"))))

(ert-deftest jg-agent-comments-anchor-survives-region-deletion ()
  (with-temp-buffer
    (insert "anchor text here")
    (let ((record (jg-agent-comments--add 1 (point-max) "note")))
      (delete-region 1 (point-max))     ; agent-shell rewrote the block
      (should (member record jg-agent-comments--comments))
      (should (null (overlay-buffer (plist-get record :overlay)))))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: the Global Constraints test command.
Expected: FAIL — `jg-agent-comments--add` not defined.

- [ ] **Step 3: Implement `jg-agent-comments--add` and `jg-agent-comments--render`**

`--add`: capture snippet with `(filter-buffer-substring beg end)`, build the record plist, call `--render`, push onto `jg-agent-comments--comments`, return the record. `--render`: make the overlay, put `jg-agent-comments t`, `face` `jg-agent-comments-snippet`, and `evaporate t`; display strings (marker, inline text) are added in Task 4 — this task only creates and registers the anchor.

- [ ] **Step 4: Run tests to verify they pass**

Run: the Global Constraints test command.
Expected: PASS (all three)

- [ ] **Step 5: Commit**

```bash
cd /Users/jgran/.emacs.d
git add lisp/jg-agent-comments
git commit -m "jg-agent-comments: comment records with stored snippets and anchors"
```

---

### Task 3: Comment-at-point command and edit buffer

**Files:**
- Modify: `/Users/jgran/.emacs.d/lisp/jg-agent-comments/jg-agent-comments.el`
- Modify: `/Users/jgran/.emacs.d/lisp/jg-agent-comments/test/jg-agent-comments-test.el`

**Interfaces:**
- Consumes: `jg-agent-comments--add` (Task 2).
- Produces: `(jg-agent-comments-comment)` interactive command; internal `(jg-agent-comments--region-or-line)` → `(beg . end)`; `(jg-agent-comments--agent-text-at-point-p)` → boolean; edit-buffer commit function `(jg-agent-comments--commit-edit)`.

- [ ] **Step 1: Write the failing tests**

```elisp
(ert-deftest jg-agent-comments-refuses-non-agent-text ()
  (with-temp-buffer
    (insert "user typed input")        ; no 'field property
    (should-error (jg-agent-comments--region-or-line))))

(ert-deftest jg-agent-comments-accepts-agent-output ()
  (with-temp-buffer
    (insert "agent said things")
    (put-text-property (point-min) (point-max) 'field 'output)
    (should (jg-agent-comments--agent-text-at-point-p))))

(ert-deftest jg-agent-comments-empty-commit-cancels ()
  (with-temp-buffer
    (insert "agent prose")
    (put-text-property (point-min) (point-max) 'field 'output)
    (jg-agent-comments--commit-edit-from-string "")  ; no comment buffer used
    (should (null jg-agent-comments--comments))))
```

Note: `jg-agent-comments--commit-edit-from-string` is the test seam — the same function the `C-c C-c` binding calls after reading the edit buffer's contents. Split the real commit path as: `--commit-edit` reads `(buffer-string)` of the edit buffer, kills it, calls `--commit-edit-from-string`.

- [ ] **Step 2: Run tests to verify they fail**

Run: the Global Constraints test command.
Expected: FAIL — commands not defined.

- [ ] **Step 3: Implement the commands**

`--agent-text-at-point-p`: `(eq (get-char-property (point) 'field) 'output)`. `--region-or-line`: error with "Not on agent output" when the check fails; otherwise active region `(region-beginning . region-end)` when `use-region-p`, else the bounds of the line at point via `(pos-bol)` and `(1+ (pos-eol))` capped at `point-max`. `jg-agent-comments-comment`: call `--region-or-line`, open buffer `*jg-agent-comment*`, switch to it, set a local map with `C-c C-c` → `--commit-edit` and `C-c C-k` → kill buffer and cancel; on commit, call `jg-agent-comments--add` in the originating buffer with the entered text.

- [ ] **Step 4: Run tests to verify they pass**

Run: the Global Constraints test command.
Expected: PASS (all three)

- [ ] **Step 5: Commit**

```bash
cd /Users/jgran/.emacs.d
git add lisp/jg-agent-comments
git commit -m "jg-agent-comments: comment-at-point with magit-style edit buffer"
```

---

### Task 4: Inline display with elision, gutter marker

**Files:**
- Modify: `/Users/jgran/.emacs.d/lisp/jg-agent-comments/jg-agent-comments.el`
- Modify: `/Users/jgran/.emacs.d/lisp/jg-agent-comments/test/jg-agent-comments-test.el`

**Interfaces:**
- Consumes: `jg-agent-comments--render` (Task 2, extended here).
- Produces: display-complete `--render`: overlay gains a `before-string` gutter marker on the first line and an `after-string` one-line inline rendering. Helper `(jg-agent-comments--inline-string text)` → single-line string.

- [ ] **Step 1: Write the failing tests**

```elisp
(ert-deftest jg-agent-comments-inline-elides-multiline ()
  (should (equal (jg-agent-comments--inline-string "first\nsecond\nthird")
                 "first …"))
  (should (equal (jg-agent-comments--inline-string "only")
                 "only")))

(ert-deftest jg-agent-comments-render-strings-are-single-line ()
  (with-temp-buffer
    (insert "anchor")
    (let ((record (jg-agent-comments--add 1 (point-max) "a\nb")))
      (let ((ov (plist-get record :overlay)))
        (should (not (string-match-p "\n" (or (overlay-get ov 'before-string) ""))))
        (should (not (string-match-p "\n" (or (overlay-get ov 'after-string) ""))))))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: the Global Constraints test command.
Expected: FAIL — `--inline-string` not defined; `after-string` not set.

- [ ] **Step 3: Implement the display**

`--inline-string`: take the first line; append `" …"` when more lines exist. `--render` extension: overlay `after-string` is the inline string with face `jg-agent-comments-inline`, prefixed by a two-space pad so it reads as an annotation; `before-string` is a single gutter glyph (e.g. `"▐"`) with face `jg-agent-comments-marker`. Both strings must be single-line.

- [ ] **Step 4: Run tests to verify they pass**

Run: the Global Constraints test command.
Expected: PASS (both, plus Task 2/3 tests still pass)

- [ ] **Step 5: Commit**

```bash
cd /Users/jgran/.emacs.d
git add lisp/jg-agent-comments
git commit -m "jg-agent-comments: inline one-line comment display with elision"
```

---

### Task 5: Mode-line count

**Files:**
- Modify: `/Users/jgran/.emacs.d/lisp/jg-agent-comments/jg-agent-comments.el`
- Modify: `/Users/jgran/.emacs.d/lisp/jg-agent-comments/test/jg-agent-comments-test.el`

**Interfaces:**
- Consumes: `jg-agent-comments--comments` (Task 1).
- Produces: `(jg-agent-comments--mode-line-string)` → `""` or `" [N comments]"`; registered as a `:eval` entry appended to `global-mode-string` at package load.

- [ ] **Step 1: Write the failing tests**

```elisp
(ert-deftest jg-agent-comments-mode-line-empty-when-none ()
  (with-temp-buffer
    (should (equal (jg-agent-comments--mode-line-string) ""))))

(ert-deftest jg-agent-comments-mode-line-counts ()
  (with-temp-buffer
    (insert "agent prose")
    (jg-agent-comments--add 1 (point-max) "one")
    (jg-agent-comments--add 1 (point-max) "two")
    (should (equal (jg-agent-comments--mode-line-string) " [2 comments]"))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: the Global Constraints test command.
Expected: FAIL — function not defined.

- [ ] **Step 3: Implement**

Return `""` when the current buffer is not `agent-shell-mode` or `jg-agent-comments--comments` is empty; otherwise `" [N comments]"` (singular `[1 comment]`). Register with `(add-to-list 'global-mode-string '(:eval (jg-agent-comments--mode-line-string)) 'append)` at package load.

- [ ] **Step 4: Run tests to verify they pass**

Run: the Global Constraints test command.
Expected: PASS

- [ ] **Step 5: Commit**

```bash
cd /Users/jgran/.emacs.d
git add lisp/jg-agent-comments
git commit -m "jg-agent-comments: pending-comment count in mode line"
```

---

### Task 6: Compose and send

**Files:**
- Modify: `/Users/jgran/.emacs.d/lisp/jg-agent-comments/jg-agent-comments.el`
- Modify: `/Users/jgran/.emacs.d/lisp/jg-agent-comments/test/jg-agent-comments-test.el`

**Interfaces:**
- Consumes: `jg-agent-comments--comments` (Tasks 1–2), `agent-shell-prompt-queue` (declared).
- Produces: `(jg-agent-comments--compose)` → message string; `(jg-agent-comments-send)` interactive; `(jg-agent-comments--clear)`.

- [ ] **Step 1: Write the failing tests**

```elisp
(ert-deftest jg-agent-comments-compose-quotes-snippet-then-comment ()
  (with-temp-buffer
    (insert "agent prose one")
    (jg-agent-comments--add 1 (point-max) "why?\nexplain")
    (insert "\nagent prose two")   ; buffer is now "agent prose one\nagent prose two"
    (jg-agent-comments--add 17 (point-max) "ok")
    (should (equal (jg-agent-comments--compose)
                   "> agent prose one\n  why?\n  explain\n\n> agent prose two\n  ok"))))

(ert-deftest jg-agent-comments-send-uses-prompt-queue-and-clears ()
  (with-temp-buffer
    (insert "agent prose")
    (jg-agent-comments--add 1 (point-max) "note")
    (let ((submitted nil))
      (cl-letf (((symbol-function 'agent-shell-prompt-queue)
                 (lambda (prompt) (setq submitted prompt))))
        (jg-agent-comments-send))
      (should (string-prefix-p "> agent prose" submitted))
      (should (null jg-agent-comments--comments)))))

(ert-deftest jg-agent-comments-send-without-comments-is-noop ()
  (with-temp-buffer
    (should (progn (jg-agent-comments-send) t))))  ; must not error
```

- [ ] **Step 2: Run tests to verify they fail**

Run: the Global Constraints test command.
Expected: FAIL — functions not defined.

- [ ] **Step 3: Implement**

`--compose`: records in buffer order — live anchors sorted by `(overlay-start)`, dead-anchor records last, stable within groups; each record renders as `"> "` prefix per snippet line, then two-space-indented comment lines; entries separated by one blank line. `--send`: if `--comments` empty, `(message "No pending comments")` and return; else call `(agent-shell-prompt-queue (jg-agent-comments--compose))` — this is the busy-safe path — then `--clear`. `--clear`: delete all live overlays, empty `jg-agent-comments--comments`.

- [ ] **Step 4: Run tests to verify they pass**

Run: the Global Constraints test command.
Expected: PASS (all three; earlier tasks still pass)

- [ ] **Step 5: Commit**

```bash
cd /Users/jgran/.emacs.d
git add lisp/jg-agent-comments
git commit -m "jg-agent-comments: compose and send pending comments as one prompt"
```

---

### Task 7: Keys, delete command, wiring into the config

**Files:**
- Modify: `/Users/jgran/.emacs.d/lisp/jg-agent-comments/jg-agent-comments.el`
- Modify: `/Users/jgran/.emacs.d/jg/jg-keys.el`
- Modify: `/Users/jgran/.emacs.d/jg/jg-straight.el`
- Modify: `/Users/jgran/.emacs.d/lisp/jg-agent-comments/test/jg-agent-comments-test.el`

**Interfaces:**
- Consumes: all prior tasks.
- Produces: `(jg-agent-comments-delete-at-point)` interactive; key bindings; straight integration.

- [ ] **Step 1: Write the failing test**

```elisp
(ert-deftest jg-agent-comments-delete-at-point-removes-record ()
  (with-temp-buffer
    (insert "agent prose")
    (let ((record (jg-agent-comments--add 1 (point-max) "note")))
      (goto-char 1)
      (jg-agent-comments-delete-at-point)
      (should (null jg-agent-comments--comments))
      (should (null (overlay-buffer (plist-get record :overlay)))))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: the Global Constraints test command.
Expected: FAIL — command not defined.

- [ ] **Step 3: Implement delete and bindings**

`jg-agent-comments-delete-at-point`: find the comment record whose overlay covers point (error with "No comment at point" if none), delete its overlay, remove it from `--comments`. Bind in `jg-agent-comments-mode-map`: prefix `C-c ;` keymap with `";"` → `jg-agent-comments-comment`, `"s"` → `jg-agent-comments-send`, `"k"` → `jg-agent-comments-delete-at-point`. Register `jg-agent-comments-mode` in `jg-mode-emulation-alist` in `jg-keys.el` (follow the existing entry pattern). In `jg-straight.el`, add the straight local-repo declaration matching the `jg-project-sidebar` pattern (`:type nil :local-repo "jg-agent-comments"`) and enable via `(add-hook 'agent-shell-mode-hook #'jg-agent-comments-mode)` in its `:config`.

- [ ] **Step 4: Run tests to verify they pass**

Run: the Global Constraints test command.
Expected: PASS

- [ ] **Step 5: Smoke test in a real shell (manual, not batch)**

Start Emacs, open an agent-shell (`M-a o`), let one response finish, comment on two spots (one on plain prose, one on a folded block), check the mode line shows `[2 comments]`, run `C-c ; s`, and confirm the agent receives a blockquoted two-comment message.

- [ ] **Step 6: Commit**

```bash
cd /Users/jgran/.emacs.d
git add lisp/jg-agent-comments jg/jg-keys.el jg/jg-straight.el
git commit -m "jg-agent-comments: key bindings, delete command, config wiring"
```

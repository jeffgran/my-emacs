;;; jg-project-sidebar.el --- Global project sidebar over perspective-el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: jgran
;; Package-Requires: ((emacs "29.1") (agent-shell "0.66.1") (perspective "2.22") (projectile))

;; The user's own project-management sidebar.  It is no longer affiliated
;; with the agent-shell-hq fork it once descended from: the transient peek,
;; session labelling, keyboard navigation, and the key-hint footer have all
;; been removed.  What remains is a passive, mouse-only display built around
;; perspective-el instead of living inside a dedicated "*agent-shell*"
;; perspective.  It never takes focus — toggling, refreshing, and clicking an
;; entry all leave the previously selected window selected.

;;; Commentary:

;; A persistent, global sidebar (a side window, not a perspective) that lists
;; every open project and doubles as a project switcher:

;;   ● ▾ projA                       <- current perspective (project)
;;       $ *shell*                   <- regular shell/terminal buffer
;;       ✓ my-agent-session          <- agent-shell buffer (state icon)
;;     ▾ projB
;;       $ *shell<projB>*
;;       ◔ other-session

;; Data model
;;   One entry per perspective (perspective-el is the source of truth:
;;   projects opened via `C-x p' / projectile + persp-projectile each get a
;;   perspective named after the project).  Sub-entries are the perspective's
;;   agent-shell buffers (state icons) and regular shell/terminal buffers
;;   (`$'; see `jg-project-sidebar-shell-modes').  agent-shell buffers whose
;;   project has no perspective are shown in trailing dimmed "(loose)" groups.
;;   Agent entries also show agent-shell's own session title — `(:session
;;   :title)' state, seeded with the session's first prompt and refined by
;;   the agent — as a dimmed sub-line under the buffer name.

;; Global by construction
;;   The sidebar is never trapped in a perspective.  Perspective switches
;;   restore saved window configurations, so `persp-activated-hook'
;;   re-displays the side window whenever it was lost, and the sidebar buffer
;;   is made a member of every perspective (so `persp-kill' forgets it rather
;;   than killing it).  A polling timer re-renders only when the structural
;;   signature (projects, buffers, states) actually changed.

;; Clicking = switching wholesale
;;   Clicking a project header runs `persp-switch' to that project; clicking
;;   a buffer entry switches to the owning perspective *and* displays the
;;   buffer.  Buffer lists (`C-v' helm-buffers-list, etc.) follow, because
;;   the perspective really changed.  Focus always returns to the main
;;   window: the sidebar is a passive display and never stays selected.

;; One-way sync
;;   The sidebar is display-only.  Background refreshes (perspective
;;   switches, the polling timer) never change what any window displays or
;;   where focus sits.  In the other direction it follows reality:
;;   `window-buffer-change-functions' plus the poll move the selection to
;;   the entry matching the main window's buffer (or the current
;;   perspective's header), expanding collapsed groups as needed — never
;;   changing what any window displays.  Only an explicit mouse click
;;   changes the main window.

;; Shells land in their project perspective
;;   perspective-el scopes `C-v' (helm-buffers-list) to the current
;;   perspective (its hard-wired helm advice; C-u C-v bypasses it) — which
;;   is the point — but shells started from a stray context used to join
;;   whatever perspective was current (the catch-all "main" perspective is
;;   seeded with the whole buffer list at startup), so `C-v' from a
;;   project showed files OR shells, never both.  `jg-project-sidebar-setup'
;;   (called from jg-straight.el once agent-shell loads) fixes this three
;;   ways: :before advice on agent-shell's entry points `persp-switch'es to
;;   the project's perspective before any shell is created (the same switch
;;   `projectile-persp-switch-project' performs for C-x p); a mode-hook
;;   backstop gives freshly created shell buffers membership in their
;;   project perspective when they sneak in through another path
;;   (membership only — never display changes, so it cannot fight
;;   perspective-el's display advice); and `jg-project-sidebar-adopt-shells'
;;   repairs existing strays one-shot.  A `persp-add-buffer' guard keeps
;;   shells OUT of foreign perspectives when accidental displays (agent
;;   toggles, helm actions, stale restores) would otherwise leak them in.
;;
;; Post-switch sanity
;;   `persp-activate' restores the perspective's saved window
;;   configuration; when that configuration recorded a buffer that has
;;   since been killed, `set-window-configuration' silently leaves the
;;   PRE-switch buffer visible ("stale buffer after cycling").  After every
;;   activation, `jg-project-sidebar--sanity-after-activation' re-points
;;   windows that clearly show foreign buffers to the perspective's own
;;   best buffer — display-only, so nothing is killed and no memberships
;;   change.  The stale configuration then re-saves clean on the next
;;   switch away.

;; Tab-bar note
;;   This sidebar replaces the perspective listing in the header line —
;;   perspective-el's `persp-show-modestring' is set to nil in jg-straight.el
;;   so the header-line "tab bar" is gone entirely (agent-shell buffers used
;;   to hide it anyway by installing their own `header-line-format').  If you
;;   ever want it back, set `persp-show-modestring' to 'header before
;;   `persp-mode' runs — or see `agent-shell-header-style' (nil / text) for
;;   agent shells' own top bar.

;; Interaction (mouse only):
;;   Click a shell or agent entry -> switch to its project perspective and
;;   display that buffer in the main window.
;;   Click a project header      -> wholesale `persp-switch' to that project.
;;   Click empty space           -> return focus to the main window.
;;   The sidebar window never stays selected after any click.
;; `M-h' toggles the sidebar on and off without moving focus.
;;
;; Global bindings (see jg-navigation-mode.el):
;;   M-{ / M-}  cycle projects in the sidebar's rendered top-to-bottom
;;              order — M-} moves one line DOWN the visible list (wrapping
;;              from bottom to top), M-{ one line UP (top to bottom wrap).
;;              A wholesale `persp-switch' (project switcher replacement
;;              for `persp-prev'/`persp-next' and the removed header "tab
;;              bar"); the cycle order is derived from the same model the
;;              renderer draws, so it cannot drift from the display.
;;              Works with the sidebar hidden, and a visible sidebar's
;;              selection follows via the main-window sync.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'agent-shell)
(require 'agent-shell-viewport)
(require 'perspective)
(require 'projectile)

;;;; Customization

(defgroup jg-project-sidebar nil
  "Global project sidebar over perspective-el."
  :group 'convenience
  :prefix "jg-project-sidebar-")

(defcustom jg-project-sidebar-width 50
  "Width of the project sidebar in columns."
  :type 'integer
  :group 'jg-project-sidebar)

(defcustom jg-project-sidebar-side 'left
  "Edge of the frame for the sidebar side window."
  :type '(choice (const left) (const right))
  :group 'jg-project-sidebar)

(defcustom jg-project-sidebar-refresh-interval 2
  "Seconds between sidebar refresh polls.
The poll re-renders only when the sidebar's structural signature
\(projects, buffers, states) changed."
  :type 'number
  :group 'jg-project-sidebar)

(defcustom jg-project-sidebar-excluded-persps nil
  "Perspective names never listed in the sidebar.
The internal frame-global perspective is always excluded."
  :type '(repeat string)
  :group 'jg-project-sidebar)

(defcustom jg-project-sidebar-shell-modes
  '(shell-mode eshell-mode eat-mode mistty-mode term-mode vterm-mode)
  "Major modes (checked with `derived-mode-p') listed as regular shells.
Projects opened via `C-x p' auto-start a `shell-mode' buffer
\(`projectile-switch-project-action' is `projectile-run-shell')."
  :type '(repeat symbol)
  :group 'jg-project-sidebar)

(defcustom jg-project-sidebar-fallback-icons
  '((idle . ("✓" . success))
    (busy . ("◔" . warning))
    (blocked . ("⯄" . error))
    (dead . ("✗" . error)))
  "Fallback Unicode characters for TUI Emacs.
Each entry is (STATE . (CHAR . FACE)), used when image display is
unavailable (e.g. terminal Emacs).  Uses `font-lock-face' so the
color survives outer `face' text properties in the render code."
  :type '(alist :key-type (choice (const idle) (const busy) (const blocked) (const dead))
                :value-type (cons string face))
  :group 'jg-project-sidebar)

(defcustom jg-project-sidebar-session-title-max 40
  "Maximum displayed characters of an agent session title.
Titles come from agent-shell's `(:session :title)' state (seeded with
the session's first prompt, refined by the agent); longer titles are
truncated with an ellipsis on the entry's title sub-line."
  :type 'integer
  :group 'jg-project-sidebar)

;;;; Faces

(defface jg-project-sidebar-project
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for project (perspective) headers in the sidebar.")

(defface jg-project-sidebar-project-current
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for the header of the current perspective in the sidebar.")

(defface jg-project-sidebar-loose
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for loose groups (agent shells with no matching perspective).")

(defface jg-project-sidebar-session-title
  '((t :inherit shadow :slant italic))
  "Face for the agent session-title sub-line in the sidebar.
Mirrors agent-shell's own `agent-shell-session-title' treatment: the
title is metadata about the session, visually dimmer than the entry
name.")

(defface jg-project-sidebar-selection
  '((((class color) (background dark))
     :background "#2d3b2d" :extend t)
    (((class color) (background light))
     :background "#d4e4d4" :extend t))
  "Face for the selected entry in the sidebar.
Intentionally dim — just enough to show position without glare.")

;;;; Constants and internal state

(defconst jg-project-sidebar--buffer-name " *jg-project-sidebar*")

;; Sidebar entries: plist with :type ('project or 'buffer), :persp NAME,
;; :loose-p BOOL, and for project entries :root ROOT; for buffer entries
;; :buffer BUF and :kind ('agent or 'shell).
(defvar jg-project-sidebar--entries nil
  "Flat navigable entries including project headers and buffer lines.")

(defvar jg-project-sidebar--current-idx 0
  "Index of the highlighted entry.")

(defvar jg-project-sidebar--collapsed nil
  "List of perspective names currently collapsed.")

(defvar jg-project-sidebar--main-window nil
  "Main (non-side) window that receives focus on click.
Captured when the sidebar is shown; always validated before use.")

(defvar jg-project-sidebar--refresh-timer nil
  "Repeating timer that refreshes the sidebar when its content changes.")

(defvar jg-project-sidebar--last-signature nil
  "Serialized model at last render, compared by the refresh poll.")

(defvar jg-project-sidebar--last-main-state nil
  "Last synced \(PERSP-NAME . MAIN-BUFFER-NAME) of the main window.
Compared by `jg-project-sidebar--sync-from-main-window' so the
main-window -> sidebar sync only acts when reality actually changed
\(compare-before-act; O(1) on every hook firing).")

(defvar jg-project-sidebar--highlight-overlay nil
  "Overlay used to highlight the selected entry in the sidebar.")

(defvar jg-project-sidebar--mouse-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "<mouse-1>")        'jg-project-sidebar-mouse-select)
    (define-key map (kbd "<double-mouse-1>") 'jg-project-sidebar-mouse-select-double)
    map)
  "Buffer-local keymap for sidebar mouse clicks.  Mouse events only.")

;;;; Shared helpers

(defun jg-project-sidebar--buffer-state (buf)
  "Return `busy', `blocked', `idle', or `dead' for BUF.
Tolerates buffers that are not live agent-shell sessions (every probe
is guarded); those classify as `idle'."
  (if (buffer-live-p buf)
      (with-current-buffer buf
        (cond
         ((and (fboundp 'agent-shell-status)
               (eq (ignore-errors (agent-shell-status :shell-buffer buf)) 'blocked))
          'blocked)
         ((and (fboundp 'agent-shell--permission-pending-p)
               (ignore-errors (agent-shell--permission-pending-p :shell-buffer buf)))
          'blocked)
         ((and (fboundp 'shell-maker-busy)
               (ignore-errors (shell-maker-busy)))
          'busy)
         (t 'idle)))
    'dead))

(defun jg-project-sidebar--session-title (buf)
  "Return BUF's agent-shell session title, or nil.
The title is agent-shell's `(:session :title)' state — seeded with the
session's first prompt and refined by agent notifications (see
`agent-shell--set-session-title'); agent-shell's own session picker
displays it next to the buffer name.  Only the first line is used,
trimmed.  Nil for buffers without agent-shell state (regular shells,
fake test buffers), which renders without a title sub-line."
  (ignore-errors
    (when-let* ((title (map-nested-elt
                        (buffer-local-value 'agent-shell--state buf)
                        '(:session :title))))
      (string-trim (car (split-string (substring-no-properties title)
                                      "\n"))))))

(defun jg-project-sidebar--make-agent-entry (buf)
  "Return the model entry for agent-shell buffer BUF.
State for the icon, plus the session title for display and for the
poll's structural signature (title changes re-render the sidebar)."
  (list :buffer buf :kind 'agent
        :state (jg-project-sidebar--buffer-state buf)
        :title (jg-project-sidebar--session-title buf)))

(defvar jg-project-sidebar--icon-cache nil
  "Alist of (STATE . IMAGE) for buffer status icons.")

(defconst jg-project-sidebar--icon-svgs
  '((idle . "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 20 20\">
  <circle cx=\"10\" cy=\"10\" r=\"8.5\" fill=\"#4E9A72\"/>
  <polyline points=\"4.5,10.5 8.5,14.5 16,5.5\"
            stroke=\"white\" stroke-width=\"2.5\" fill=\"none\"
            stroke-linecap=\"round\" stroke-linejoin=\"round\"/>
</svg>")
    (busy . "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 20 20\">
  <polygon points=\"10,2 18.5,17.5 1.5,17.5\" fill=\"#C9922A\"/>
</svg>")
    (blocked . "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 20 20\">
  <polygon points=\"18.5,6.5 13.5,1.5 6.5,1.5 1.5,6.5 1.5,13.5 6.5,18.5 13.5,18.5 18.5,13.5\" fill=\"#D32F2F\"/>
  <polygon points=\"16.5,7.0 13.0,3.5 7.0,3.5 3.5,7.0 3.5,13.0 7.0,16.5 13.0,16.5 16.5,13.0\" fill=\"none\" stroke=\"white\" stroke-width=\"1.2\"/>
</svg>")
    (dead . "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 20 20\">
  <circle cx=\"10\" cy=\"10\" r=\"8.5\" fill=\"#C0392B\"/>
  <line x1=\"6.5\" y1=\"6.5\" x2=\"13.5\" y2=\"13.5\" stroke=\"white\" stroke-width=\"2.5\" stroke-linecap=\"round\"/>
  <line x1=\"13.5\" y1=\"6.5\" x2=\"6.5\" y2=\"13.5\" stroke=\"white\" stroke-width=\"2.5\" stroke-linecap=\"round\"/>
</svg>"))
  "Inline SVG strings for each buffer state.")

(defun jg-project-sidebar--svg-icon (state)
  "Return the cached SVG image for STATE (`busy', `blocked', `idle', or `dead')."
  (unless jg-project-sidebar--icon-cache
    (setq jg-project-sidebar--icon-cache
          (mapcar (lambda (pair)
                    (cons (car pair)
                          (create-image (cdr pair) 'svg t :ascent 'center)))
                  jg-project-sidebar--icon-svgs)))
  (alist-get state jg-project-sidebar--icon-cache))

(defun jg-project-sidebar--icon (state)
  "Return a propertized string displaying the icon for STATE.
In GUI Emacs with SVG support, this is a space with a `display' SVG
image property.  Otherwise, this is a Unicode character with a face."
  (if (and (display-graphic-p) (image-type-available-p 'svg))
      (propertize " " 'display (jg-project-sidebar--svg-icon state))
    (let ((fallback (or (alist-get state jg-project-sidebar-fallback-icons)
                        '("?" . default))))
      (propertize (car fallback) 'face (cdr fallback) 'font-lock-face (cdr fallback)))))

(defun jg-project-sidebar--preferred-buffer (shell-buf)
  "Return the best buffer to display for SHELL-BUF.
Uses the existing viewport buffer when one already exists, so its mode
\(view or edit) is preserved.  Falls back to the shell buffer itself."
  (or (ignore-errors
        (agent-shell-viewport--buffer :shell-buffer shell-buf :existing-only t))
      shell-buf))

;;;; Perspective helpers
;;
;; Perspective buffer lists are read and mutated through the persp struct
;; directly (`persp-buffers' / `perspectives-hash').  The public wrappers
;; `persp-get-buffers' and `with-perspective' run full `persp-switch' round
;; trips — each one restores window configurations twice — which is far too
;; heavy (and surprising) for a sidebar that re-queries every few seconds.

(defun jg-project-sidebar--listed-persp-names ()
  "Names of perspectives the sidebar should list, in `persp-names' order."
  (if (bound-and-true-p persp-mode)
      (seq-remove (lambda (name)
                    (or (equal name persp-frame-global-perspective-name)
                        (member name jg-project-sidebar-excluded-persps)))
                  (persp-names))
    nil))

(defun jg-project-sidebar--persp-buffers (name)
  "Live buffers of perspective NAME, read without switching perspectives."
  (when-let* ((persp (and (bound-and-true-p persp-mode)
                         (gethash name (perspectives-hash)))))
    (seq-filter #'buffer-live-p (persp-buffers persp))))

(defun jg-project-sidebar--persp-add-buffer (name buf)
  "Associate BUF with perspective NAME by direct struct update.
Idempotent; avoids `with-perspective' window-configuration round trips."
  (when-let* ((persp (and (bound-and-true-p persp-mode)
                         (gethash name (perspectives-hash)))))
    (cl-pushnew buf (persp-buffers persp) :test #'eq)))

(defun jg-project-sidebar--add-to-all-persps (buf)
  "Make BUF a member of every perspective on every frame.
Membership in multiple perspectives is what keeps `persp-kill' from
killing BUF (it only forgets buffers that live in other perspectives)."
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (when (bound-and-true-p persp-mode)
        (maphash (lambda (_name persp)
                   (cl-pushnew buf (persp-buffers persp) :test #'eq))
                 (perspectives-hash))))))

(defun jg-project-sidebar--remove-from-all-persps (buf)
  "Disassociate BUF from every perspective on every frame."
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (when (bound-and-true-p persp-mode)
        (maphash (lambda (_name persp)
                   (setf (persp-buffers persp) (remq buf (persp-buffers persp))))
                 (perspectives-hash))))))

;;;; Model

(defun jg-project-sidebar--shell-buffer-p (buf agent-set)
  "Return non-nil when BUF is a regular shell/terminal buffer.
AGENT-SET is the list of known agent-shell buffers, which take
precedence over mode-based classification."
  (and (buffer-live-p buf)
       (not (memq buf agent-set))
       (not (string-prefix-p " " (buffer-name buf)))
       (with-current-buffer buf
         (apply #'derived-mode-p jg-project-sidebar-shell-modes))))

(defun jg-project-sidebar--guess-root (bufs)
  "Derive a project root directory for a group from BUFFERS.
Uses the default-directory of its first shell-ish buffer, which for
projectile-opened projects is the project root."
  (cl-loop for buf in bufs
           when (buffer-live-p buf)
           return (buffer-local-value 'default-directory buf)))

(defun jg-project-sidebar--sort-entries (entries)
  "Sort ENTRIES: agent-shell buffers first, then by buffer name."
  (sort entries
        (lambda (a b)
          (let ((ka (if (eq (plist-get a :kind) 'agent) 0 1))
                (kb (if (eq (plist-get b :kind) 'agent) 0 1)))
            (if (/= ka kb)
                (< ka kb)
              (string< (buffer-name (plist-get a :buffer))
                       (buffer-name (plist-get b :buffer))))))))

(defun jg-project-sidebar--model ()
  "Build the live sidebar model (a pure query, no cached state).
Return a list of group plists:
  (:name PERSP-NAME :root ROOT :current-p BOOL :loose-p BOOL
   :buffers ((:buffer BUF :kind agent|shell [:state STATE]) ...))
ORDER (load-bearing): groups are returned in the order the sidebar
RENDERS them — real perspectives in REVERSE `persp-names' order (the
final `nreverse', there to keep loose groups last, flips them),
followed by loose agent-shell groups.  The renderer draws groups
top-to-bottom in this order, and project cycling
\(`jg-project-sidebar--rendered-project-names') derives its cycle
order from this same model — do not \"fix\" the reversal on one side
without the other, or cycling and display will drift apart."
  (if (not (bound-and-true-p persp-mode))
      nil
    (let* ((names (jg-project-sidebar--listed-persp-names))
           (current (persp-current-name))
           (agents (or (ignore-errors (agent-shell-buffers)) '()))
           (persp-bufs (mapcar (lambda (name)
                                 (cons name (jg-project-sidebar--persp-buffers name)))
                               names))
           (groups (mapcar (lambda (pair)
                             (list :name (car pair)
                                   :root nil
                                   :current-p (equal (car pair) current)
                                   :loose-p nil
                                   :buffers nil))
                           persp-bufs))
           (group-for (lambda (name)
                        (cl-loop for g in groups
                                 when (equal (plist-get g :name) name)
                                 return g)))
           loose-agents)
      ;; Regular shells: perspective membership is the truth.
      (dolist (pair persp-bufs)
        (dolist (buf (cdr pair))
          (when (jg-project-sidebar--shell-buffer-p buf agents)
            (let ((g (funcall group-for (car pair))))
              (plist-put g :buffers
                         (cons (list :buffer buf :kind 'shell)
                               (plist-get g :buffers)))))))
      ;; Agent shells: membership first, else project-name match, else loose.
      (dolist (buf agents)
        (let* ((owner (cl-loop for pair in persp-bufs
                               when (memq buf (cdr pair))
                               return (car pair)))
               (name (or owner
                         (let ((pname (ignore-errors
                                        (with-current-buffer buf
                                          (agent-shell--project-name)))))
                           (and pname (member pname names) pname))))
               (group (and name (funcall group-for name))))
           (if group
               (plist-put group :buffers
                          (cons (jg-project-sidebar--make-agent-entry buf)
                                (plist-get group :buffers)))
             (push buf loose-agents))))
      ;; Loose agent shells grouped by project name.
      (dolist (pair (seq-group-by
                     (lambda (buf)
                       (or (ignore-errors
                             (with-current-buffer buf (agent-shell--project-name)))
                           "other"))
                     (nreverse loose-agents)))
         (push (list :name (car pair)
                     :root nil :current-p nil :loose-p t
                     :buffers (jg-project-sidebar--sort-entries
                               (mapcar #'jg-project-sidebar--make-agent-entry
                                       (cdr pair))))
               groups))
      ;; Sort each group's entries and derive roots (a group buffer's
      ;; default-directory is the project root for projectile projects).
      (dolist (g groups)
        (let* ((bufs (plist-get g :buffers))
               (root (jg-project-sidebar--guess-root
                      (append (mapcar (lambda (e) (plist-get e :buffer)) bufs)
                              (cdr (cl-loop for pair in persp-bufs
                                            when (equal (car pair) (plist-get g :name))
                                            return pair))))))
          (plist-put g :buffers (jg-project-sidebar--sort-entries bufs))
          (plist-put g :root root)))
      (nreverse groups))))

(defun jg-project-sidebar--signature (model)
  "Serialize MODEL into an `equal'-comparable structure."
  (mapcar (lambda (g)
            (list (plist-get g :name)
                  (plist-get g :current-p)
                  (plist-get g :loose-p)
                  (plist-get g :root)
                  (mapcar (lambda (e)
                            (list (buffer-name (plist-get e :buffer))
                                  (plist-get e :kind)
                                  (plist-get e :state)
                                  (plist-get e :title)))
                          (plist-get g :buffers))))
          model))

;;;; Rendering

(defun jg-project-sidebar--render ()
  "Render the sidebar buffer and rebuild the entries list."
  (let ((model (jg-project-sidebar--model)))
    (with-current-buffer (get-buffer-create jg-project-sidebar--buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq jg-project-sidebar--entries nil)
        ;; Top heading
        (insert (propertize (format " Projects (%d)\n" (length model))
                            'face '(:inherit font-lock-function-name-face :weight bold)))
        (insert "\n")
        (dolist (group model)
          (let* ((name      (plist-get group :name))
                 (root      (plist-get group :root))
                 (loose     (plist-get group :loose-p))
                 (current   (plist-get group :current-p))
                 (collapsed (member name jg-project-sidebar--collapsed)))
            ;; Project header — always a clickable entry
            (push (list :type 'project :persp name :root root :loose-p loose)
                  jg-project-sidebar--entries)
            (insert (propertize
                     (concat (if current "● " "  ")
                             (if collapsed "▸ " "▾ ")
                             name
                             (if loose "  (loose)" "")
                             "\n")
                     'face (cond (current 'jg-project-sidebar-project-current)
                                 (loose   'jg-project-sidebar-loose)
                                 (t       'jg-project-sidebar-project))
                     'jg-project-sidebar-persp name))
            (unless collapsed
              (let ((bufs (plist-get group :buffers)))
                (if (null bufs)
                    (insert (propertize "      – no shells –\n" 'face 'shadow))
                   (dolist (entry bufs)
                     (let* ((buf  (plist-get entry :buffer))
                            (kind (plist-get entry :kind))
                            (bname (buffer-name buf))
                            (title (plist-get entry :title)))
                       (push (list :type 'buffer :buffer buf :kind kind
                                   :persp name :loose-p loose)
                             jg-project-sidebar--entries)
                       (insert (propertize
                                (concat "    "
                                        (if (eq kind 'agent)
                                            (concat (jg-project-sidebar--icon
                                                     (or (plist-get entry :state)
                                                         (jg-project-sidebar--buffer-state buf)))
                                                    " ")
                                          "$ ")
                                        bname
                                        "\n")
                                'jg-project-sidebar-buffer buf))
                       ;; Agent session title sub-line (agent-shell's
                       ;; `(:session :title)': first prompt, refined by the
                       ;; agent).  Not an entry itself — the highlight
                       ;; operates on the name line above it.
                       (when (and (eq kind 'agent) title)
                         (let* ((max jg-project-sidebar-session-title-max)
                                (display (if (> (length title) max)
                                             (concat (substring title 0 (1- max))
                                                     "…")
                                           title)))
                           (insert (propertize
                                    (concat "          " display "\n")
                                    'face 'jg-project-sidebar-session-title)))))))))
            (insert "\n")))
        (setq jg-project-sidebar--entries
              (nreverse jg-project-sidebar--entries))
        (setq buffer-read-only t)
        (setq-local cursor-type nil))
      (use-local-map jg-project-sidebar--mouse-map))
    (setq jg-project-sidebar--last-signature
          (jg-project-sidebar--signature model))))

;;;; Highlight + point sync

(defun jg-project-sidebar--highlight (idx)
  "Highlight entry at IDX and sync point to that line in the sidebar window."
  (with-current-buffer (get-buffer-create jg-project-sidebar--buffer-name)
    (let ((inhibit-read-only t))
      (unless (overlayp jg-project-sidebar--highlight-overlay)
        (setq jg-project-sidebar--highlight-overlay
              (make-overlay (point-min) (point-min))))
      (let ((ov jg-project-sidebar--highlight-overlay))
        (move-overlay ov (point-min) (point-min))
        (when-let* ((entry (nth idx jg-project-sidebar--entries))
                    (type  (plist-get entry :type))
                    (pos   (if (eq type 'project)
                               (text-property-any (point-min) (point-max)
                                                  'jg-project-sidebar-persp
                                                  (plist-get entry :persp))
                             (text-property-any (point-min) (point-max)
                                                'jg-project-sidebar-buffer
                                                (plist-get entry :buffer)))))
          (move-overlay ov pos
                        (min (1+ (save-excursion (goto-char pos) (line-end-position)))
                             (point-max)))
          (overlay-put ov 'face 'jg-project-sidebar-selection)
          (goto-char pos)
          (when-let* ((win (get-buffer-window (current-buffer))))
            (set-window-point win pos)))))))

;;;; Cursor helpers

(defun jg-project-sidebar--current-buffer ()
  "Return the buffer object for the currently highlighted entry, or nil."
  (when-let* ((entry (nth jg-project-sidebar--current-idx
                         jg-project-sidebar--entries)))
    (plist-get entry :buffer)))

(defun jg-project-sidebar--restore-idx (buf)
  "After a re-render, set `jg-project-sidebar--current-idx' to BUF.
If BUF is live and still in the entries list, move to its new position.
Otherwise clamp the old index to the new list length."
  (setq jg-project-sidebar--current-idx
        (or (and (buffer-live-p buf)
                 (cl-position-if (lambda (e)
                                   (and (eq (plist-get e :type) 'buffer)
                                        (eq (plist-get e :buffer) buf)))
                                 jg-project-sidebar--entries))
            (min jg-project-sidebar--current-idx
                 (max 0 (1- (length jg-project-sidebar--entries)))))))

;;;; Window management

(defun jg-project-sidebar--main-window ()
  "Return the main (non-side) window that clicks should focus.
Prefers the window captured when the sidebar was shown; falls back to
the first regular (non-side) window of the selected frame."
  (or (and (window-live-p jg-project-sidebar--main-window)
           (eq (window-frame jg-project-sidebar--main-window) (selected-frame))
           (not (window-parameter jg-project-sidebar--main-window 'window-side))
           jg-project-sidebar--main-window)
      (seq-find (lambda (w) (not (window-parameter w 'window-side)))
                (window-list))
      (selected-window)))

;;;; Main-window -> sidebar sync
;;
;; The other direction of the two-way sync: when the main window's buffer
;; changes (C-x p / projectile, persp-switch by any means, manually popping
;; a shell), the sidebar's selection/highlight moves to the matching entry.
;; Display only — this never changes what any window shows.

(defun jg-project-sidebar--entry-for-buffer (buf)
  "Return the sidebar entry displaying BUF, or nil.
Agent-shell entries match both their session buffer and its
viewport-preferred display buffer, since the main window may show
either."
  (cl-loop for e in jg-project-sidebar--entries
           when (and (eq (plist-get e :type) 'buffer)
                     (if (eq (plist-get e :kind) 'agent)
                         (or (eq (plist-get e :buffer) buf)
                             (eq (ignore-errors
                                   (jg-project-sidebar--preferred-buffer
                                    (plist-get e :buffer)))
                                 buf))
                       (eq (plist-get e :buffer) buf)))
           return e))

(defun jg-project-sidebar--sync-selection ()
  "Move the sidebar selection to the entry matching the main window.
Prefers the buffer entry for the main window's buffer (shell, agent
session, or agent viewport); falls back to the current perspective's
project header.  Expands the target's group when collapsed.  Never
changes what any window displays."
  (when (get-buffer jg-project-sidebar--buffer-name)
    (let* ((main-win (jg-project-sidebar--main-window))
           (main-buf (and (window-live-p main-win)
                          (window-buffer main-win)))
           (persp (and (bound-and-true-p persp-mode)
                       (persp-current-name)))
           (target (or (and main-buf
                            (jg-project-sidebar--entry-for-buffer main-buf))
                       (cl-loop for e in jg-project-sidebar--entries
                                when (and (eq (plist-get e :type) 'project)
                                          (equal (plist-get e :persp) persp)
                                          (not (plist-get e :loose-p)))
                                return e))))
      (when target
        ;; A collapsed group omits its buffer entries, so expand first and
        ;; re-resolve — the buffer entry only exists once expanded.
        (let ((name (plist-get target :persp)))
          (when (and name (member name jg-project-sidebar--collapsed))
            (setq jg-project-sidebar--collapsed
                  (delete name jg-project-sidebar--collapsed))
            (jg-project-sidebar--render)
            (setq target (or (and main-buf
                                  (jg-project-sidebar--entry-for-buffer main-buf))
                             target))))
        (when-let* ((idx (cl-position target jg-project-sidebar--entries
                                      :test #'equal)))
          (setq jg-project-sidebar--current-idx idx)
          (jg-project-sidebar--highlight idx))))))

(defun jg-project-sidebar--sync-from-main-window ()
  "Sync the sidebar selection to the main window if reality changed.
Computes the current (PERSP . MAIN-BUFFER) state and compares it with
`jg-project-sidebar--last-main-state' — O(1), so it is safe to run from
`window-buffer-change-functions' on every redisplay.  On change:
refresh the structure if stale, then move the selection.  Never
changes what any window displays."
  (when (get-buffer jg-project-sidebar--buffer-name)
    (let* ((main-win (jg-project-sidebar--main-window))
           (main-buf (and (window-live-p main-win)
                          (window-buffer main-win)))
           (persp (and (bound-and-true-p persp-mode)
                       (persp-current-name)))
           (state (cons persp (and main-buf (buffer-name main-buf)))))
      (unless (equal state jg-project-sidebar--last-main-state)
        (setq jg-project-sidebar--last-main-state state)
        (jg-project-sidebar--refresh-if-stale)
        (jg-project-sidebar--sync-selection)))))

;;;; Commands

(defun jg-project-sidebar-select ()
  "On a project header: switch to that project's perspective.
On a buffer entry: switch to the owning perspective and display the
buffer.  Loose entries display the buffer in the current perspective
\(standard perspective-el import semantics).

Focus hygiene: the main window is selected BEFORE any `persp-switch'
runs.  `persp-switch' saves the SOURCE perspective's window
configuration as the frame stands at that moment — with the sidebar
window selected (a mouse click runs from the sidebar), that
configuration would record the sidebar as the selected window and
restore focus there on every later switch to the source project,
sending the next helm action into the sidebar window."
  (interactive)
  (when-let* ((entry (nth jg-project-sidebar--current-idx
                         jg-project-sidebar--entries)))
    ;; Belt 1: leave the sidebar window before switching perspective, so
    ;; the source perspective's saved configuration has a main window
    ;; selected.
    (select-window (jg-project-sidebar--main-window))
    (if (eq (plist-get entry :type) 'project)
        (if (plist-get entry :loose-p)
            (jg-project-sidebar-collapse)
          (persp-switch (plist-get entry :persp))
          (select-window (jg-project-sidebar--main-window)))
      (let* ((buf   (plist-get entry :buffer))
             (persp (plist-get entry :persp))
             (loose (plist-get entry :loose-p))
             (disp  (if (eq (plist-get entry :kind) 'agent)
                        (jg-project-sidebar--preferred-buffer buf)
                      buf)))
        (when (buffer-live-p disp)
          (unless (or loose (not (bound-and-true-p persp-mode)))
            (persp-switch persp))
          (select-window (jg-project-sidebar--main-window))
          (switch-to-buffer disp))))))

(defun jg-project-sidebar-collapse ()
  "Toggle collapse of the current entry's project group."
  (interactive)
  (when-let* ((entry (nth jg-project-sidebar--current-idx
                          jg-project-sidebar--entries))
              (name  (plist-get entry :persp)))
    (if (member name jg-project-sidebar--collapsed)
        (setq jg-project-sidebar--collapsed
              (delete name jg-project-sidebar--collapsed))
      (push name jg-project-sidebar--collapsed))
    ;; Preserve position on the same project header after re-render
    (jg-project-sidebar--render)
    (let ((new-idx (or (cl-position-if
                        (lambda (e)
                          (and (eq (plist-get e :type) 'project)
                               (equal (plist-get e :persp) name)))
                        jg-project-sidebar--entries)
                       0)))
      (setq jg-project-sidebar--current-idx new-idx)
      (jg-project-sidebar--highlight new-idx))))

;;;###autoload
(defun jg-project-sidebar-refresh ()
  "Re-render the sidebar to pick up project, buffer, and state changes.
No-ops when the sidebar is not open."
  (interactive)
  (when (get-buffer jg-project-sidebar--buffer-name)
    (let ((buf (jg-project-sidebar--current-buffer)))
      (jg-project-sidebar--render)
      (jg-project-sidebar--restore-idx buf)
      (jg-project-sidebar--highlight jg-project-sidebar--current-idx))))

;;;; Project cycling (M-{ / M-})

(defun jg-project-sidebar--rendered-project-names ()
  "Return perspective names in the sidebar's top-to-bottom order.
Derived from `jg-project-sidebar--model' — the exact order the
renderer draws — so cycling can never drift from the displayed list
\(the model's group order is deliberately NOT `persp-names' order:
loose groups are kept last, which reverses the real-perspective
section; see the model's commentary).  Loose groups are not
perspectives and are excluded."
  (cl-loop for g in (jg-project-sidebar--model)
           unless (plist-get g :loose-p)
           collect (plist-get g :name)))

(defun jg-project-sidebar--cycle-project (step)
  "Switch to the project STEP lines away in the sidebar's display.
STEP is +1 for next (M-}, one line DOWN the rendered sidebar) or -1
for previous (M-{, one line UP).  The cycle list comes from
`jg-project-sidebar--rendered-project-names' — the same model the
renderer draws from — so cycling always matches the visible order,
wrapping around.  This is a wholesale `persp-switch', not a display
change.  When the sidebar is visible its selection follows via the
main-window -> sidebar sync; when hidden, nothing is displayed.  No-op
\(with a message) when there are fewer than two projects."
  (let* ((names (jg-project-sidebar--rendered-project-names))
         (count (length names)))
    (if (< count 2)
        (message "jg-project-sidebar: %s"
                 (if (zerop count)
                     "no projects to cycle"
                   (format "only one project (%s)" (car names))))
      (let* ((current (and (bound-and-true-p persp-mode)
                           (persp-current-name)))
             (pos (cl-position current names :test #'equal))
             (target (if pos
                         (nth (mod (+ pos step) count) names)
                       ;; Current perspective is not listed (excluded or
                       ;; GLOBAL): step into the list from the nearest end.
                       (nth (if (> step 0) 0 (1- count)) names))))
        (when (and target (not (equal target current)))
          (persp-switch target))))))

;;;###autoload
(defun jg-project-sidebar-next-project ()
  "Switch to the next project, down the sidebar's listing (wraps around).
Bound to `M-}'.  Moves one project DOWN the rendered sidebar — the
project on the next line below the current one — wrapping from the
bottom back to the top.  Works whether or not the sidebar is visible."
  (interactive)
  (jg-project-sidebar--cycle-project 1))

;;;###autoload
(defun jg-project-sidebar-previous-project ()
  "Switch to the previous project, up the sidebar's listing (wraps around).
Bound to `M-{'.  Moves one project UP the rendered sidebar — the
project on the line above the current one — wrapping from the top
back to the bottom.  See `jg-project-sidebar-next-project'."
  (interactive)
  (jg-project-sidebar--cycle-project -1))

;;;; Shells land in their project perspective
;;
;; `C-v' (helm-buffers-list) is perspective-scoped by perspective-el's
;; hard-wired helm advice — desired.  The pieces below make sure shells and
;; agent shells are MEMBERS of their project's perspective, so that filter
;; shows files + shells + agents together when called from anywhere in the
;; project, including from a shell.

(defun jg-project-sidebar--project-persp-name ()
  "Return the perspective name for the current buffer's project.
Computed exactly the way `projectile-persp-switch-project' names the
perspective it creates for `C-x p': `projectile-project-name-function'
applied to `projectile-project-root' (by default, the basename of the
project root).  Nil when projectile is unavailable, the current
directory is remote, or it is not inside a project."
  (when (and (fboundp 'projectile-project-root)
             (boundp 'projectile-project-name-function)
             (not (file-remote-p default-directory)))
    (when-let* ((root (ignore-errors (projectile-project-root))))
      (funcall projectile-project-name-function root))))

(defun jg-project-sidebar--ensure-project-persp (&rest _)
  "Before a shell is created: switch to its project's perspective.
Attached as :before advice to agent-shell's entry points (see
`jg-project-sidebar-setup').  Switching first — the same `persp-switch'
`projectile-persp-switch-project' performs — makes perspective-el's
display advice associate the new shell buffer with the project
perspective instead of with whatever perspective happens to be current
\(e.g. the catch-all \"main\" when browsing from a stray shell).
Creates the perspective when the project has none yet.  No-op outside
a project or when already in the project's perspective."
  (when (bound-and-true-p persp-mode)
    (when-let* ((name (jg-project-sidebar--project-persp-name)))
      (unless (equal name (persp-current-name))
        (persp-switch name)))))

(defun jg-project-sidebar--adopt-on-shell-mode ()
  "Backstop from `shell-mode-hook'/`agent-shell-mode-hook':
give a freshly created shell buffer membership in its project
perspective.  Runs in the new buffer, whose `default-directory' is the
project root at mode-init time (shell-maker inherits it before calling
the mode).  Membership only — no display changes, no disassociation —
so it cannot fight perspective-el's display advice (at worst the
buffer is a member of both its project's perspective and the one it
was displayed in).  Only adopts into perspectives that already exist;
paths that need a brand-new perspective are handled by the advice."
  (when (and (bound-and-true-p persp-mode)
             (not (string-prefix-p " " (buffer-name))))
    (when-let* ((name (jg-project-sidebar--project-persp-name))
                ((not (equal name (persp-current-name)))))
      (jg-project-sidebar--persp-add-buffer name (current-buffer)))))

(defun jg-project-sidebar--shell-kind (buf agent-set)
  "Return `agent' or `shell' when BUF is an agent-shell or shell buffer.
AGENT-SET is the list of known agent-shell buffers."
  (cond ((memq buf agent-set) 'agent)
        ((jg-project-sidebar--shell-buffer-p buf agent-set) 'shell)))

(defconst jg-project-sidebar--shell-entry-points
  '(agent-shell agent-shell-new-shell agent-shell-opencode-start-agent)
  "Agent-shell entry points advised by `jg-project-sidebar-setup'.
`agent-shell-manager-new' routes through these, so every creation
path is covered.")

(defun jg-project-sidebar-setup ()
  "Install the shell -> project-perspective co-location.
Adds `:before' advice on `jg-project-sidebar--shell-entry-points' so
any path that creates an agent shell (M-a s, M-a o, `M-x agent-shell',
the manager) first switches to the shell's project perspective;
installs the mode-hook backstop for regular shell buffers; and installs
the `persp-add-buffer' guard that stops shell buffers from being
adopted into a foreign perspective by
accidental displays (agent-shell DWIM toggles, helm actions, stale
window-configuration restores).  Idempotent.  Called from
jg-straight.el's use-package :config, which runs as soon as
agent-shell loads — i.e. before the first M-a s can execute."
  (interactive)
  (dolist (fn jg-project-sidebar--shell-entry-points)
    (when (fboundp fn)
      (advice-add fn :before #'jg-project-sidebar--ensure-project-persp)))
  (advice-add 'persp-add-buffer
              :around #'jg-project-sidebar--keep-shells-in-project-persp)
  (add-hook 'shell-mode-hook #'jg-project-sidebar--adopt-on-shell-mode)
  (add-hook 'agent-shell-mode-hook #'jg-project-sidebar--adopt-on-shell-mode)
  (message "jg-project-sidebar: shells will land in their project perspectives"))

(defun jg-project-sidebar--keep-shells-in-project-persp
    (origfn buffer-or-name &rest args)
  "Around advice for `persp-add-buffer': shells join only their project.
`persp-add-buffer' is the single choke point of perspective-el's
display advice — every `switch-to-buffer'/`display-buffer'/
`set-window-buffer' lands here — so an accidental display of a shell
while another perspective is current (agent-shell DWIM toggle, a helm
action, a stale window-configuration restore) would otherwise leak the
shell into that perspective's buffer list, and the leaked membership
keeps re-infecting saved window configurations.  When BUFFER-OR-NAME
is a shell/agent-shell buffer whose project perspective exists and is
not the current perspective, skip the add and instead make it a member
of its project perspective.  Everything else passes through
untouched."
  (if-let* (((bound-and-true-p persp-mode))
            (buf (get-buffer buffer-or-name))
            ((buffer-live-p buf))
            ((jg-project-sidebar--shell-kind
              buf (or (ignore-errors (agent-shell-buffers)) '())))
            (name (with-current-buffer buf
                    (jg-project-sidebar--project-persp-name)))
            ((gethash name (perspectives-hash)))
            ((not (equal name (persp-current-name)))))
      (jg-project-sidebar--persp-add-buffer name buf)
    (apply origfn buffer-or-name args)))

;;;###autoload
(defun jg-project-sidebar-adopt-shells ()
  "Repair shell/agent-shell buffer memberships: exactly one project each.
For every live buffer that is an agent-shell session or a regular
shell buffer (`jg-project-sidebar-shell-modes'), derive its project
from the buffer's `default-directory' and make it a member of that
project's perspective — and ONLY that perspective: membership is
removed from every other perspective, including foreign project
perspectives and the catch-all \"main\" and GLOBAL perspectives (a
shell in GLOBAL would show in every perspective's `C-v').  All edits
are direct membership changes — no window displays change, no buffers
are killed.  Buffers outside any project are left alone; shells whose
project has no perspective yet are reported (open that project via
C-x p to create one).  Diagnostic: \\[universal-argument] C-v lists
all buffers, bypassing the perspective filter."
  (interactive)
  (unless (bound-and-true-p persp-mode)
    (user-error "persp-mode is not active"))
  (let ((agents (or (ignore-errors (agent-shell-buffers)) '()))
        (adopted 0) (nopersp 0) (noproj 0))
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (not (string-prefix-p " " (buffer-name buf)))
                 (jg-project-sidebar--shell-kind buf agents))
        (if-let* ((name (with-current-buffer buf
                          (jg-project-sidebar--project-persp-name))))
            (if (gethash name (perspectives-hash))
                (progn
                  (jg-project-sidebar--persp-add-buffer name buf)
                  ;; Exclusive membership: out of every OTHER perspective
                  ;; (foreign projects, catch-alls, GLOBAL).
                  (dolist (other (persp-names))
                    (unless (equal other name)
                      (when-let* ((persp (gethash other (perspectives-hash))))
                        (setf (persp-buffers persp)
                              (remq buf (persp-buffers persp))))))
                  (setq adopted (1+ adopted)))
              (setq nopersp (1+ nopersp)))
          (setq noproj (1+ noproj)))))
    (message "jg-project-sidebar-adopt-shells: %d adopted, %d without a project perspective, %d outside projects"
             adopted nopersp noproj)
    adopted))

;;;; Post-switch sanity: re-point foreign windows
;;
;; `set-window-configuration' (the heart of `persp-activate') leaves a
;; window showing the PRE-switch buffer when the saved configuration
;; recorded a dead buffer — the "stale buffer after cycling" bug — and
;; restores foreign buffers recorded by earlier membership
;; contamination.  After every perspective activation, re-point any
;; window that is clearly showing a foreign buffer to the perspective's
;; own most appropriate buffer.  Display-only; kills nothing, edits no
;; memberships, and cannot fight perspective-el's display advice.

(defun jg-project-sidebar--sanity-candidate ()
  "Return the best buffer to display for the current perspective.
Prefers the most-recently-used member that is a real working buffer
\(visiting a file, or a shell/agent shell); falls back to any live
non-internal member, then to the perspective's scratch buffer."
  (when (bound-and-true-p persp-mode)
    (let* ((members (seq-filter #'buffer-live-p (persp-current-buffers)))
           (agents (or (ignore-errors (agent-shell-buffers)) '()))
           (own-bufs (list (get-buffer jg-project-sidebar--buffer-name)))
           (real-p (lambda (buf)
                     (and (not (string-prefix-p " " (buffer-name buf)))
                          (not (string-match-p "\\`\\*\\(helm\\|Minibuf\\|Echo\\)"
                                              (buffer-name buf)))
                          (not (memq buf own-bufs))
                          (or (buffer-file-name buf)
                              (jg-project-sidebar--shell-kind buf agents)))))
           (any-p (lambda (buf)
                    (not (string-prefix-p " " (buffer-name buf))))))
      (or (seq-find real-p members)
          (seq-find any-p members)
          (and (fboundp 'persp-get-scratch-buffer)
               (persp-get-scratch-buffer (persp-current-name)))))))

(defun jg-project-sidebar--sanity-after-activation ()
  "Re-point windows that show foreign buffers after a persp switch.
Runs from `persp-activated-hook' (every `persp-switch' path).  For each
non-side window of the selected frame whose buffer is clearly foreign
to the now-current perspective — not a member, not in the GLOBAL
perspective, not the perspective's own scratch, not an internal/helm/
minibuffer buffer, not this package's buffers — display the
perspective's `jg-project-sidebar--sanity-candidate' instead.
Display-only: no buffers are killed and no memberships change (buffer
association is inhibited around the re-point).  Also heals focus: if
the restored window configuration has a SIDE window selected (a
configuration saved while focus was in the sidebar — the \"taint\"),
select a main window instead, so the next helm action cannot replace
the sidebar window.  Skipped during recursive perspective switches
and while the minibuffer is active (helm sessions)."
  (when (and (bound-and-true-p persp-mode)
             (not (frame-parameter nil 'persp--recursive))
             (not (active-minibuffer-window))
             (persp-curr))
    ;; Belt 2: focus must never rest on a side window after a switch —
    ;; display-only, safe inside this hook, and heals every path (M-x
    ;; persp-switch, cycling, clicks, stale saved configurations).
    (when (window-parameter (selected-window) 'window-side)
      (select-window (jg-project-sidebar--main-window)))
    (when-let* ((candidate (jg-project-sidebar--sanity-candidate)))
      (let* ((members (persp-current-buffers))
             (global-bufs (when (gethash persp-frame-global-perspective-name
                                         (perspectives-hash))
                            (persp-buffers
                             (gethash persp-frame-global-perspective-name
                                      (perspectives-hash)))))
             (own-scratch (persp-scratch-buffer (persp-current-name)))
             (own-bufs (list (get-buffer jg-project-sidebar--buffer-name)))
             (foreign-p (lambda (buf)
                          (and (buffer-live-p buf)
                               (not (memq buf members))
                               (not (memq buf global-bufs))
                               (not (equal (buffer-name buf) own-scratch))
                               (not (memq buf own-bufs))
                               (not (string-prefix-p " " (buffer-name buf)))
                               (not (string-match-p "\\`\\*\\(helm\\|Minibuf\\|Echo\\)"
                                                   (buffer-name buf)))))))
        (dolist (win (window-list (selected-frame)))
          (unless (window-parameter win 'window-side)
            (when-let* ((buf (window-buffer win))
                        ((funcall foreign-p buf)))
              (let ((persp--inhibit-buffer-association t))
                (set-window-buffer win candidate)))))))))

;; Independent of the sidebar's visibility: installed as soon as the
;; package loads (first M-h / M-{ / M-} press or agent-shell loading).
(add-hook 'persp-activated-hook #'jg-project-sidebar--sanity-after-activation)

;;;; Mouse commands
;;
;; The sidebar's only interaction is the mouse.  There is deliberately no
;; keyboard keymap: the buffer-local map (see `jg-project-sidebar--mouse-map'
;; near the top of the file) binds mouse events only, so clicking works
;; whether or not the window was selected, and every handler finishes by
;; selecting the main window.

(defun jg-project-sidebar-mouse-select (event)
  "Select the sidebar entry clicked by mouse EVENT, then refocus main window.
On a buffer entry, `persp-switch' to the owning perspective and display
the buffer in the main window; on a project header, wholesale
`persp-switch' to that project.  Clicking empty space does nothing but
return focus to the main window.

The sidebar window never ends up selected: focus is explicitly returned
to the main window after every click, including clicks on empty space."
  (interactive "e")
  (let ((main (jg-project-sidebar--main-window)))
    (with-current-buffer (get-buffer jg-project-sidebar--buffer-name)
      (let* ((pos        (posn-point (event-start event)))
             (buf-prop   (when pos (get-text-property pos 'jg-project-sidebar-buffer)))
             (persp-prop (when pos (get-text-property pos 'jg-project-sidebar-persp))))
        (cond
         (buf-prop
          (when-let* ((idx (cl-position-if
                            (lambda (e)
                              (and (eq (plist-get e :type) 'buffer)
                                   (eq (plist-get e :buffer) buf-prop)))
                            jg-project-sidebar--entries)))
            (setq jg-project-sidebar--current-idx idx)
            (jg-project-sidebar--highlight idx)
            (jg-project-sidebar-select)))
         (persp-prop
          (when-let* ((idx (cl-position-if
                            (lambda (e)
                              (and (eq (plist-get e :type) 'project)
                                   (equal (plist-get e :persp) persp-prop)))
                            jg-project-sidebar--entries)))
            (setq jg-project-sidebar--current-idx idx)
            (jg-project-sidebar--highlight idx)
            (jg-project-sidebar-select))))))
    ;; Always end with focus in the main window — also the empty-space case.
    (when (window-live-p main)
      (select-window main))))

(defun jg-project-sidebar-mouse-select-double (event)
  "Select the sidebar entry double-clicked by mouse EVENT.
Single click already performs the full select (see
`jg-project-sidebar-mouse-select'); the double-click event just
repeats it, which is idempotent."
  (interactive "e")
  (jg-project-sidebar-mouse-select event))

;;;; Show / hide / toggle

(defun jg-project-sidebar--display-alist ()
  "Display-buffer alist for the sidebar side window."
  `((side          . ,jg-project-sidebar-side)
    (window-width  . ,jg-project-sidebar-width)
    (window-parameters . ((no-delete-other-windows . t)
                          (no-other-window . t)))))

(defun jg-project-sidebar--ensure-window ()
  "Display the sidebar side window if it is missing."
  (when (and (get-buffer jg-project-sidebar--buffer-name)
             (not (get-buffer-window jg-project-sidebar--buffer-name)))
    (display-buffer-in-side-window
     (get-buffer jg-project-sidebar--buffer-name)
     (jg-project-sidebar--display-alist))))

(defun jg-project-sidebar--install-hooks ()
  "Install the perspective/window hooks and refresh poll that keep the
sidebar global and in sync.  Idempotent: an already-running poll timer
is cancelled first."
  (add-hook 'persp-activated-hook #'jg-project-sidebar--on-persp-activated)
  (add-hook 'persp-created-hook  #'jg-project-sidebar--on-persp-created)
  ;; Main-window -> sidebar sync: fires during redisplay whenever any
  ;; window's buffer changed.  The handler is O(1) unless the tracked
  ;; main window / current perspective actually changed.
  (add-hook 'window-buffer-change-functions
            #'jg-project-sidebar--on-window-buffer-change)
  (when jg-project-sidebar--refresh-timer
    (cancel-timer jg-project-sidebar--refresh-timer))
  (setq jg-project-sidebar--refresh-timer
        (run-with-timer jg-project-sidebar-refresh-interval
                        jg-project-sidebar-refresh-interval
                        #'jg-project-sidebar--poll)))

(defun jg-project-sidebar--teardown ()
  "Cancel hooks and timer; delete the window; kill the sidebar buffer."
  (when jg-project-sidebar--refresh-timer
    (cancel-timer jg-project-sidebar--refresh-timer)
    (setq jg-project-sidebar--refresh-timer nil))
  (remove-hook 'persp-activated-hook #'jg-project-sidebar--on-persp-activated)
  (remove-hook 'persp-created-hook  #'jg-project-sidebar--on-persp-created)
  (remove-hook 'window-buffer-change-functions
               #'jg-project-sidebar--on-window-buffer-change)
  (when-let* ((win (get-buffer-window jg-project-sidebar--buffer-name)))
    (delete-window win))
  (when-let* ((buf (get-buffer jg-project-sidebar--buffer-name)))
    (jg-project-sidebar--remove-from-all-persps buf)
    (kill-buffer buf))
  (setq jg-project-sidebar--entries          nil
        jg-project-sidebar--current-idx      0
        jg-project-sidebar--collapsed        nil
        jg-project-sidebar--main-window      nil
        jg-project-sidebar--last-signature   nil
        jg-project-sidebar--last-main-state  nil))

(defun jg-project-sidebar--show ()
  "Open the sidebar as a side window without moving focus.
The window selected before the call stays selected."
  (unless (bound-and-true-p persp-mode)
    (persp-mode 1))
  (unless (window-parameter (selected-window) 'window-side)
    (setq jg-project-sidebar--main-window (selected-window)))
  (save-selected-window
    (let ((buf (get-buffer-create jg-project-sidebar--buffer-name)))
      ;; Membership in every perspective keeps `persp-kill' (M-w) from
      ;; killing the sidebar buffer when a project perspective dies.
      (jg-project-sidebar--add-to-all-persps buf)
      (display-buffer-in-side-window buf (jg-project-sidebar--display-alist)))
    (setq jg-project-sidebar--current-idx 0
          jg-project-sidebar--collapsed   nil
          jg-project-sidebar--last-main-state nil)
    (jg-project-sidebar--render)
    (jg-project-sidebar--install-hooks)
    (jg-project-sidebar--highlight 0)
    ;; Land the selection on the current project/buffer and prime the
    ;; main-window state tracker.
    (jg-project-sidebar--sync-from-main-window)))

;;;###autoload
(defun jg-project-sidebar-toggle ()
  "Toggle the global project sidebar without moving focus.

The sidebar lists every perspective (project) with its agent-shell and
regular shell buffers, and is never scoped to a single perspective.
The window selected before the toggle stays selected; the sidebar is a
passive display whose only interaction is clicking entries
\(see `jg-project-sidebar-mouse-select')."
  (interactive)
  (save-selected-window
    (let ((win (get-buffer-window jg-project-sidebar--buffer-name)))
      (cond
       ((and (get-buffer jg-project-sidebar--buffer-name) win)
        (jg-project-sidebar--teardown))
       ((get-buffer jg-project-sidebar--buffer-name)
        (jg-project-sidebar--ensure-window)
        (jg-project-sidebar--install-hooks)
        (jg-project-sidebar-refresh))
       (t (jg-project-sidebar--show))))))

;;;; Perspective/window hooks and refresh poll

(defun jg-project-sidebar--on-persp-activated ()
  "After any perspective activation: restore the sidebar window if the
restored window configuration dropped it, then poll (refresh + sync)."
  (when (get-buffer jg-project-sidebar--buffer-name)
    (jg-project-sidebar--ensure-window)
    (jg-project-sidebar--poll)))

(defun jg-project-sidebar--on-persp-created ()
  "After any perspective is created: make the sidebar buffer a member
\(runs with the new perspective current) and poll (refresh + sync)."
  (when (get-buffer jg-project-sidebar--buffer-name)
    (persp-add-buffer (get-buffer jg-project-sidebar--buffer-name))
    (jg-project-sidebar--poll)))

(defun jg-project-sidebar--on-window-buffer-change (_frame)
  "Main-window -> sidebar sync trigger.
`window-buffer-change-functions' calls this during redisplay whenever
any window's buffer changed (C-x p / projectile flows, `persp-switch'
restoring window configurations, manually popping a shell).  Delegates
to `jg-project-sidebar--sync-from-main-window', whose compare-before-
act gate makes this O(1) for unrelated changes."
  (jg-project-sidebar--sync-from-main-window))

(defun jg-project-sidebar--refresh-if-stale ()
  "Re-render the sidebar when its structural signature changed.
Re-rendering never changes what any window displays or where focus sits."
  (when (get-buffer jg-project-sidebar--buffer-name)
    (let ((signature (jg-project-sidebar--signature
                      (jg-project-sidebar--model))))
      (unless (equal signature jg-project-sidebar--last-signature)
        (jg-project-sidebar-refresh)))))

(defun jg-project-sidebar--poll ()
  "Periodic safety net (timer entry): refresh the render when the
structural signature changed, then sync the selection to the main
window.  Never changes what any window displays."
  (when (get-buffer jg-project-sidebar--buffer-name)
    (jg-project-sidebar--refresh-if-stale)
    (jg-project-sidebar--sync-from-main-window)))

(provide 'jg-project-sidebar)
;;; jg-project-sidebar.el ends here

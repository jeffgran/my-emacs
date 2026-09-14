;;; jg-tokyo-night.el --- JG customizations on top of tokyo-night -*- lexical-binding: t; -*-

;; JG's customization layer for the `tokyo-night' theme.
;;
;; Two independent mechanisms:
;;
;;   1. `tokyo-night-override-colors-alist' -- remaps *palette slot values*
;;      before the theme is applied.  The theme hard-wires semantic roles to
;;      slot names (e.g. font-lock-string-face -> "tokyo-green"), so pointing a
;;      slot at a different hue is how we "rearrange the scheme".  This MUST be
;;      set before `load-theme', which is why jg-straight.el requires this file
;;      in its `:init' block.  Changing it later requires `tokyo-night-reload'.
;;
;;   2. `jg-tokyo-night-apply-face-tweaks' -- fixes faces that the palette
;;      cannot reach: helm hard-codes its own colors and tokyo-night defines no
;;      helm faces at all, and `font-lock-variable-name-face' is wired directly
;;      to `tokyo-fg' (which is the global foreground, so it cannot be
;;      repointed without repainting the whole UI).
;;
;; Everything below is meant to be an easily edited playground: each entry says
;; which role(s) it drives.  See jg/color-themes/jg-zenburn/jg-zenburn-theme.el
;; for the old, disabled theme that inspired these role assignments.

;;; Code:

;; Silence byte-compiler: these live in the `tokyo-night' package, which is not
;; a compile-time dependency of this file (we do not want to force it to load
;; merely to compile, and it may not be installed yet on a fresh machine).
(declare-function tokyo-night-get-color "tokyo-night" (name &optional theme))
;; Defined by tokyo-night's `defcustom'.  Declaring it here lets us `setq' it
;; before the package loads (the whole point: overrides must precede
;; `load-theme') without a free-variable warning.  A bare `defvar' leaves an
;; already-bound value alone, so tokyo-night's later defcustom will not clobber
;; what we set below.
(defvar tokyo-night-override-colors-alist)

;;; Palette slot remaps
;;
;; Role -> slot -> theme face(s), as defined by tokyo-night.el:
;;
;;   string        font-lock-string-face                   reads "tokyo-green"
;;   keyword       font-lock-keyword-face                  reads "tokyo-magenta"
;;   function      font-lock-function-name-face            reads "tokyo-blue"
;;   variable      font-lock-variable-name-face            reads "tokyo-fg"   <-- handled in the face tweaks below
;;   constant      font-lock-constant-face / -number-face  reads "tokyo-orange"
;;   builtin       font-lock-builtin-face                  reads "tokyo-cyan-bright"
;;   type          font-lock-type-face                     reads "tokyo-cyan"
;;   comment/doc   font-lock-comment(-delimiter)-/doc-face reads "tokyo-comment"
;;   preprocessor  font-lock-preprocessor-face             reads "tokyo-teal"
;;
;; NOTE: these slots are shared with non-font-lock faces too (diff, dired,
;; search, ...), so moving a slot moves those as well.  That is deliberate --
;; it keeps one hue per "meaning" across the frame -- but it is the reason a
;; slot swap can have wide-reaching effects.
(setq tokyo-night-override-colors-alist
      '(;; STRINGS become ORANGE ----------------------------------------------
        ;; font-lock-string-face reads "tokyo-green", so put the orange hex
        ;; there.  (#ff9e64 is the stock "tokyo-orange" value.)
        ("tokyo-green"   . "#ff9e64")

        ;; KEYWORDS become BLUE ----------------------------------------------
        ;; font-lock-keyword-face reads "tokyo-magenta", so put the blue hex
        ;; there.  (#7aa2f7 is the stock "tokyo-blue" value; functions below
        ;; use the same blue, so keyword/function share it.)
        ("tokyo-magenta" . "#7aa2f7")

        ;; FUNCTIONS stay BLUE ----------------------------------------------
        ;; font-lock-function-name-face already reads "tokyo-blue"; no entry
        ;; needed.  Uncomment to pin it explicitly:
        ;; ("tokyo-blue" . "#7aa2f7")

        ;; CONSTANTS become RED ---------------------------------------------
        ;; font-lock-constant-face and font-lock-number-face read
        ;; "tokyo-orange", so put the red hex there.  (#f7768e is the stock
        ;; "tokyo-red" value.)
        ("tokyo-orange"  . "#f7768e")

        ;; TYPES become GREEN -----------------------------------------------
        ;; font-lock-type-face reads "tokyo-cyan", so put the green hex there.
        ;; (#9ece6a is the stock "tokyo-green" value -- i.e. green and orange
        ;; have simply traded slots.)
        ("tokyo-cyan"    . "#9ece6a")

        ;; BUILTINS stay CYAN -----------------------------------------------
        ;; font-lock-builtin-face reads "tokyo-cyan-bright"; no entry needed.
        ;; ("tokyo-cyan-bright" . "#2ac3de")

        ;; COMMENTS (and doc strings) are BRIGHTENED ------------------------
        ;; font-lock-comment-face, font-lock-comment-delimiter-face and
        ;; font-lock-doc-face all read "tokyo-comment".  Stock #565f89 is too
        ;; dim to read comfortably, so lift it in place.  (The old jg-zenburn
        ;; made comments green; swap the hex for "#9ece6a" to get that back.)
        ("tokyo-comment" . "#7a88b8")))
;; Set at load time; see the comments above for role-by-role intent.

;;; Post-load face tweaks

(defun jg-tokyo-night-apply-face-tweaks (&optional theme)
  "Apply JG's hand-tuned face overrides for the Tokyo Night THEME.

Needed because tokyo-night defines no helm faces -- so helm's hard-coded
defaults show through (notably `helm-ff-directory' as DarkRed on
LightGray) -- and because `font-lock-variable-name-face' is wired to the
global foreground `tokyo-fg', which cannot be repointed safely.

Colors are read from the live palette (including
`tokyo-night-override-colors-alist') via `tokyo-night-get-color', so this
tracks slot remaps and variant changes.  Faces are set in the `user'
theme, which both applies immediately and persists across
`load-theme'/`tokyo-night-reload'.

Registered on `tokyo-night-after-load-hook' and also called once from
jg-straight.el immediately after the initial `load-theme'."
  (let* ((theme     (or theme 'tokyo-night))
         (bg        (tokyo-night-get-color "tokyo-bg" theme))
         (bg-dark   (tokyo-night-get-color "tokyo-bg-dark" theme))
         (bg-hl     (tokyo-night-get-color "tokyo-bg-highlight" theme))
         (fg        (tokyo-night-get-color "tokyo-fg" theme))
         (fg-muted  (tokyo-night-get-color "tokyo-fg-muted" theme))
         (cyan      (tokyo-night-get-color "tokyo-cyan-pale" theme))
         (cyan-dim  (tokyo-night-get-color "tokyo-cyan-bright" theme))
         (teal      (tokyo-night-get-color "tokyo-teal" theme))
         (yellow    (tokyo-night-get-color "tokyo-yellow" theme))
         (red       (tokyo-night-get-color "tokyo-red" theme))
         (magenta   (tokyo-night-get-color "tokyo-magenta-hot" theme)))
    (custom-theme-set-faces
     'user
     ;; -- font-lock faces the palette cannot reach -------------------------
     ;; Yellow variables; see the role table at the top of this file.
     `(font-lock-variable-name-face ((t (:foreground ,yellow))))

     ;; -- helm-find-files directory listing --------------------------------
     ;; Kill helm's DarkRed-on-LightGray defaults; use theme bg + accent fg.
     `(helm-ff-directory
       ((t (:foreground ,cyan :background ,bg :weight bold :extend t))))
     `(helm-ff-dotted-directory
       ((t (:foreground ,cyan-dim :background ,bg :weight bold :extend t))))
     `(helm-ff-symlink
       ((t (:foreground ,yellow :background ,bg :weight bold :extend t))))
     `(helm-ff-invalid-symlink
       ((t (:foreground ,red :background ,bg :weight bold :extend t))))
     `(helm-ff-file-extension
       ((t (:foreground ,fg-muted :background ,bg :weight normal :extend t))))
     `(helm-ff-suid
       ((t (:foreground ,bg :background ,red :weight bold :extend t))))

     ;; -- helm chrome ------------------------------------------------------
     ;; Selection highlight: theme bg-highlight instead of helm's ForestGreen.
     `(helm-selection
       ((t (:background ,bg-hl :foreground ,fg :extend t))))
     `(helm-selection-line ((t (:background ,bg-hl :extend t))))
     `(helm-source-header
       ((t (:foreground ,teal :background ,bg-dark :weight bold :extend t))))
     `(helm-header-line-left-margin
       ((t (:foreground ,bg :background ,yellow :weight bold :extend t))))
     `(helm-candidate-number
       ((t (:foreground ,teal :background ,bg-dark :weight bold :extend t))))
     ;; Matches / visible marks pop against the dark UI.
     `(helm-match ((t (:foreground ,yellow :background ,bg :weight bold :extend t))))
     `(helm-visible-mark
       ((t (:foreground ,bg :background ,magenta :weight bold :extend t)))))))

;; Re-run on `tokyo-night-reload' (and `tokyo-night-select') so a slot remap
;; takes effect without restarting Emacs.  `add-hook' before `tokyo-night'
;; itself is loaded is fine: it creates the hook variable, and the package's
;; later `defcustom' leaves an already-bound value alone.
(add-hook 'tokyo-night-after-load-hook #'jg-tokyo-night-apply-face-tweaks)

;; Belt-and-suspenders: the `user' theme settings above win over helm's
;; `defface' defaults regardless of order, but re-apply once helm is actually
;; present in case a future helm version recalculates its faces on load.
(with-eval-after-load 'helm
  (jg-tokyo-night-apply-face-tweaks))

(provide 'jg-tokyo-night)

;;; jg-tokyo-night.el ends here

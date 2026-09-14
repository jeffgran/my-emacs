(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;-------------------------------------

(use-package tokyo-night
  :straight t
  :ensure t
  :init
  ;; Must happen BEFORE `load-theme': the palette slot remaps in
  ;; jg-tokyo-night.el are read at theme-application time, so requiring this
  ;; file here is what makes the overrides land on the very first load.
  (require 'jg-tokyo-night)
  :config
  (load-theme 'tokyo-night t)
  ;; Apply the helm/face fixes once the theme is active.  The same function is
  ;; also on `tokyo-night-after-load-hook' (set up in jg-tokyo-night.el) so it
  ;; re-runs after `tokyo-night-reload'.
  (jg-tokyo-night-apply-face-tweaks))

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Avenir" :height 180 :weight normal))))
 '(fixed-pitch ((t ( :family "Menlo" :height 160)))))

(use-package agent-shell
  :straight t
  :ensure t
  :config
  ;;(setq agent-shell-opencode-default-model-id "fireworks-ai/accounts/fireworks/models/glm-5p2")
  (setq agent-shell-opencode-default-model-id "fireworks-ai/accounts/fireworks/models/kimi-k3")

  :bind (
         :map agent-shell-mode-map
         ("C-RET" . newline)
         )
  )

(use-package jg-project-sidebar
  ;; The user's own global project-management sidebar, usurping the local
  ;; agent-shell-hq fork (lisp/agent-shell-hq-perspective/, no longer wired
  ;; up).  Lists every perspective (project) with its agent-shell AND regular
  ;; shell buffers, and doubles as a project switcher: RET on an entry runs
  ;; `persp-switch' so buffer lists follow.  The sidebar is global — a side
  ;; window over whatever perspective is current, re-displayed by
  ;; `persp-activated-hook' after perspective switches — instead of living in
  ;; a dedicated "*agent-shell*" perspective like the fork did.
  ;; Source of truth lives in lisp/jg-project-sidebar/, symlinked into
  ;; straight/repos/jg-project-sidebar for straight to build.  Entry points
  ;; (`jg-project-sidebar-toggle' etc.) are bound to "M-a h", "M-{" and
  ;; "M-}" in jg-navigation-mode.el — straight activates the package
  ;; (build + autoloads) at startup, so those bindings work before the
  ;; package body ever loads.
  ;; :after agent-shell + setup: the moment agent-shell loads (i.e. before
  ;; the first "M-a s" / "M-a o" executes), `jg-project-sidebar-setup'
  ;; advises agent-shell's entry points so new shells always land in their
  ;; project's perspective — keeping the perspective-scoped C-v
  ;; (helm-buffers-list) complete per project (files + shells + agents).
  :straight (jg-project-sidebar :type nil :local-repo "jg-project-sidebar")
  :after agent-shell
  :config
  (jg-project-sidebar-setup)
  )

(use-package agent-recall
  :ensure t
  :straight t
  :after agent-shell
  :hook (agent-shell-mode . agent-recall-track-sessions)
  )

(straight-use-package 'async)

(use-package back-button
  :straight t
  :init
  (back-button-mode)
  (defadvice back-button-pop-local-mark (after center-after-back-button-local activate)
    "Center the view after moving it"
    (recenter))
  )

(use-package bash-completion
  :straight t
  :init
  (require 'bash-completion)
  (bash-completion-setup)
  )

(straight-use-package 'bind-key)
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c c" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  :init
  (setq claude-code-ide-terminal-backend 'eat)
  )
(straight-use-package 'coffee-mode)
(use-package company
  :straight t
  :bind (
         :map company-active-map
         ("C-n" . 'company-select-next)
         ("C-p" . 'company-select-previous)
         ("C-f" . 'company-filter-candidates)
         )
  )
(straight-use-package 'company-flow)
(straight-use-package 'company-go)
(straight-use-package 'company-native-complete)
(straight-use-package 'company-shell)
(straight-use-package 'company-terraform)
(straight-use-package 'csv-mode)
(straight-use-package 'dash)
(straight-use-package 'deferred)
(straight-use-package 'dired-filter)
(straight-use-package 'dired-subtree)
(straight-use-package 'dockerfile-mode)

(use-package doom-modeline
  :straight t
  :ensure t
  :init
  (doom-modeline-mode 1)
  (setq doom-modeline-height 16)
  ;;(setq doom-modeline-persp-name nil)
  (doom-modeline-mode +1)
  ;; perspective-el provides the `persp-mode-line' function (and the
  ;; feature is named `perspective', not `persp-mode' -- that name is
  ;; Bad-ptr's incompatible package, which we no longer install).
  ;; With `persp-show-modestring' nil (perspective block below) nothing is
  ;; ever added to `global-mode-string', so this delete is defensive only —
  ;; it strips the modeline modestring if that setting is ever re-enabled.
  (with-eval-after-load 'perspective
    (setq global-mode-string (delete '(:eval (persp-mode-line)) global-mode-string)))
  )

(use-package dotenv
  :ensure nil
  :straight (:host github :repo "pkulev/dotenv.el")
  :config
  (dotenv-update-env (dotenv-load "~/.env"))
  )

(straight-use-package 'drag-stuff)
(straight-use-package 'dumb-jump)
(use-package eat
  :straight t
  :bind (
         :map
         eat-semi-char-mode-map
         ("M-v" . eat-yank)
         )
  :config
  (add-hook 'eat-mode-hook #'(lambda ()
                               (display-line-numbers-mode -1)))
  )
(straight-use-package 'elixir-mode)
(straight-use-package 'emojify)
(use-package enh-ruby-mode
  :straight t
  :bind (
         :map enh-ruby-mode-map
         ("M-h" . 'enh-ruby-backward-sexp)
         ("M-'" . 'enh-ruby-forward-sexp)
         ("C-M-h" . 'enh-ruby-beginning-of-block)
         ("C-M-'" . 'enh-ruby-end-of-block)
         )
  )
(straight-use-package 'epl)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'expand-region)
(straight-use-package 'f)
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-flow)
(straight-use-package 'flycheck-package)
(straight-use-package 'forge)
(straight-use-package 'flycheck-golangci-lint)
(straight-use-package 'ggtags)
(straight-use-package 'git-link)
(straight-use-package 'git-timemachine)
(straight-use-package 'go-mode)
(straight-use-package 'graphql-mode)
(straight-use-package 'haml-mode)
(use-package helm
  :after helm-icons exec-path-from-shell
  :straight t
  :demand t
  :bind (
         :map helm-map
         ("TAB" . 'helm-execute-persistent-action)
         ("C-/" . 'helm-select-action)
         ("C-M-p" . 'helm-scroll-up)
         ("C-M-n" . 'helm-scroll-down)
         ("M-v" . 'clipboard-yank)
         ("M-z" . 'undo-tree-undo)
         ("M-Z" . 'undo-tree-redo)
         ("M-SPC" . 'helm-toggle-visible-mark)

         ("C-l" . 'forward-char)
         ("C-j" . 'backward-char)
         ("C-;" . 'forward-word)
         ("C-h" . 'backward-word)
         ("C-SPC" . nil)
         ("C-g" . 'jg-helm-keyboard-quit)
         ("M-i" . 'helm-yank-text-at-point)
         ("M-y" . 'helm-yank-selection)
         ("M-k" . 'helm-kill-selection-and-quit)
         )
  :custom
  (helm-move-to-line-cycle-in-source nil)
  (helm-split-window-inside-p t)
  :config
  (helm-mode)
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (defun jg-helm-keyboard-quit () (interactive) (if mark-active (deactivate-mark) (helm-keyboard-quit)))
  )

(use-package helm-ag
  :after transient helm-projectile
  :straight t
  :bind (
         :map helm-ag-mode-map
         ("RET" . helm-ag-mode-jump-other-window)
         ("TAB" . next-logical-line)
         ("<backtab>" . previous-logical-line)
         )
  :config
  (setq helm-ag-fuzzy-match t)
  (setq helm-ag-command-option "--hidden")
  (defun helm-ag-with-prefix ()
    (interactive)
    (let ((current-prefix-arg 4)) ;; emulate C-u / universal prefix arg
      (call-interactively 'helm-ag)))

  ;; set this back to the default, it is getting nuked for some reason during boot. My theory
  ;; is because at the moment it tries to set the default the path isn't set correctly yet
  ;; so (executable-find "ag") returns nil so it sets it to nil?

  (transient-define-prefix jg-dispatch-helm-ag ()
    ["Helm-ag"
     [
      ("d" "this directory" helm-do-ag)
      ("f" "this file" helm-swoop)
      ("b" "buffers" helm-multi-swoop-projectile)
      ("p" "Project" helm-do-ag-project-root)
      ]
     ]
    )
  :commands (jg-dispatch-helm-ag helm-ag)
  )
(use-package helm-rg :straight t)
(use-package helm-files
  :after helm
  :bind (
         :map helm-find-files-map
         ("TAB" . 'helm-execute-persistent-action)
         ("C-d" . 'helm-ff-persistent-delete)
         ("C-." . 'helm-find-files-down-last-level)
         ("C-," . 'helm-find-files-up-one-level)
         ("C-l" . nil)
         ("C-/" . 'helm-select-action)

         :map helm-read-file-map
         ("C-." . 'helm-find-files-down-last-level)
         ("C-," . 'helm-find-files-up-one-level)
         ("C-l" . nil)
         )
  :init
  ;; Push the "." and ".." entries to the end of the find-files candidate
  ;; list so repeated TAB (helm-execute-persistent-action) walks down
  ;; single-child directory trees without landing on them first.
  (declare-function helm-basename "helm-lib")
  (defun jg/helm-ff-move-dot-entries-last (candidates)
    "Move the \".\" and \"..\" directory entries to the end of CANDIDATES."
    (let* ((base (lambda (c) (helm-basename (if (consp c) (cdr c) c))))
           (dotp (lambda (c) (member (funcall base c) '("." "..")))))
      (append (cl-remove-if dotp candidates)
              (cl-remove-if-not dotp candidates))))
  :config
  ;; `helm-ff-sort-candidates' is the last candidate transformer in the
  ;; find-files source, so filtering its return value reorders the final list.
  (declare-function jg/helm-ff-move-dot-entries-last "jg-straight")
  (advice-add 'helm-ff-sort-candidates :filter-return
              #'jg/helm-ff-move-dot-entries-last)
  )
(use-package helm-buffers
  :bind (
         :map helm-buffer-map
         ("C-d" . 'helm-buffer-run-kill-persistent)
         ("C-l" . nil)
         ("C-/" . 'helm-select-action)
         )
  :custom
  ;; Enable fuzzy matching for filtering, but we'll preserve MRU order
  (helm-buffers-fuzzy-matching t)
  (helm-buffer-skip-remote-checking t)
  :config
  ;; Customize buffer source: fuzzy filter but keep MRU order
  (setq helm-source-buffers-list
        (helm-make-source "Buffers" 'helm-source-buffers
          :candidate-transformer
          (lambda (candidates)
            ;; Remove current buffer and filter out any nil values
            (let ((current (buffer-name (current-buffer))))
              (delq nil
                    (mapcar (lambda (cand)
                              (when cand
                                (let ((buf-name (if (consp cand) (car cand) cand)))
                                  (unless (equal buf-name current)
                                    cand))))
                            candidates))))
          :filtered-candidate-transformer
          (lambda (candidates _source)
            ;; After fuzzy filtering, re-sort by buffer-list order (MRU)
            ;; but only if we have candidates
            (when candidates
              (let ((buffer-order (mapcar #'buffer-name (buffer-list))))
                (sort candidates
                      (lambda (c1 c2)
                        (let ((n1 (if (consp c1) (car c1) c1))
                              (n2 (if (consp c2) (car c2) c2)))
                          (< (or (cl-position n1 buffer-order :test #'equal) 9999)
                             (or (cl-position n2 buffer-order :test #'equal) 9999)))))))))
        ))

(use-package helm-icons
  :straight t
  :init
  (setq helm-icons-provider 'nerd-icons)
  (helm-icons-enable)
  )

;; full fuzzy helm for files within project
(use-package helm-projectile
  :after (helm projectile)
  :straight t
  :config
  (require 'helm-for-files)
  (helm-projectile-on)

  ;; Customize project switcher: fuzzy filter but keep MRU order
  (setq helm-source-projectile-projects
        (helm-build-sync-source "Projectile projects"
          :candidates (lambda ()
                        ;; Get projects excluding current project (if in a project)
                        (let ((current (projectile-project-root)))
                          (if current
                              (cl-remove-if (lambda (proj)
                                              (equal (file-truename proj)
                                                     (file-truename current)))
                                            projectile-known-projects)
                            ;; Not in a project, return all projects
                            projectile-known-projects)))
          :fuzzy-match t
          :filtered-candidate-transformer
          (lambda (candidates _source)
            ;; After fuzzy filtering, re-sort by projectile-known-projects order (MRU)
            (when candidates
              (let ((project-order projectile-known-projects))
                (sort candidates
                      (lambda (c1 c2)
                        (< (or (cl-position c1 project-order :test #'equal) 9999)
                           (or (cl-position c2 project-order :test #'equal) 9999)))))))
          :action 'helm-source-projectile-projects-actions))
  )

(use-package helm-swoop
  :straight t
  :init
  ;;(setq helm-swoop-use-fuzzy-match t)

  :custom
  (helm-swoop-pre-input-function '(lambda () nil))
  )

(straight-use-package 'htmlize)
(straight-use-package 'idle-highlight-mode)
(straight-use-package 'jenkinsfile-mode)
(use-package jest
  :straight t
  :after (js2-mode)
  :hook
  (js2-mode . jest-minor-mode)
  (typescript-mode . jest-minor-mode)
  :bind (
         :map jest-minor-mode-map
         ("C-c ," . jest-popup)
         )
  )
(straight-use-package 'js2-mode)
(straight-use-package 'json-mode)
(straight-use-package 'k8s-mode)
(straight-use-package 'keyfreq)
(straight-use-package 'less-css-mode)
(straight-use-package 'let-alist)
(straight-use-package 'list-utils)
(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (define-key lsp-mode-map (kbd "H-l") lsp-command-map)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setf (alist-get 'web-mode lsp--formatting-indent-alist) 'web-mode-code-indent-offset)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor\\'")
  :hook (
         (typescript-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         )
  :commands (lsp lsp-deferred)
  :custom
  (lsp-go-gopls-server-path "gopls")
  )

(straight-use-package 'lsp-ui)
(straight-use-package 'lua-mode)
(use-package magit
  :straight t
  :config
  (add-hook 'magit-mode-hook #'(lambda ()
                                 (jg-code-mode 0)
                                 (jg-navigation-mode 1)))
  (transient-append-suffix 'magit-dispatch "r" '("s" "Status" magit-status))
  )

(use-package markdown-mode
  :straight t
  :custom
  ;; scaled, bold headings (h1 > h2 > h3)
  (markdown-header-scaling t)
  ;; hide **, ##, link syntax etc. so formatting renders as formatting
  ;; (toggle any time with M-j m t)
  (markdown-hide-markup t)
  ;; syntax-highlight fenced code blocks
  (markdown-fontify-code-blocks-natively t)
  ;; display -, +, * list markers as typographic bullets, cycling by depth
  (markdown-list-item-bullets '("•" "◦" "▪"))
  :config
  ;; mixed-pitch keeps font-lock faces fixed-pitch, and markdown-header-face
  ;; inherits font-lock-function-name-face — so headings would stay monospace.
  ;; Inherit variable-pitch FIRST (family) then font-lock-function-name-face
  ;; (color), keeping :weight bold from the original face.
  (set-face-attribute 'markdown-header-face nil
                      :inherit '(variable-pitch font-lock-function-name-face)
                      :weight 'bold))

;; Render GFM pipe tables as wrapped Unicode box-drawing views on top of the
;; canonical raw source (org-latex-preview pattern: buffer text stays a real
;; table; entering a table reveals source for editing). Cell content wraps to
;; fit the text column, composing with visual-fill-column instead of fighting
;; it. Replaces the old table-alignment package.
(use-package markdown-table-wrap-pretty
  :straight (:host github :repo "dnouri/markdown-table-wrap"
                   :files ("markdown-table-wrap.el" "markdown-table-wrap-pretty.el"))
  :custom
  (markdown-table-wrap-pretty-default-on-major-modes '(markdown-mode poly-markdown-mode)))

(straight-use-package 'maxframe)
(use-package mermaid-mode
  :straight t)


(use-package mistty
  :straight t)

;; variable-pitch prose in markdown buffers while keeping code, tables,
;; and markup in the default monospace face.
(use-package mixed-pitch
  :straight t)

(straight-use-package 'multiple-cursors)

(with-eval-after-load 'transient
  (transient-define-prefix jg-dispatch-mc ()
    ["Multiple Cursors"
     [
      ("l" "lines (mc/edit-lines)" mc/edit-lines)
      ("d" "dwim (mc/mark-all-dwim" mc/mark-all-dwim)
      ("f" "func (mc/mark-all-like-this-in-defun" mc/mark-all-like-this-in-defun)
      ("n" "next (mc/mark-next-like-this" mc/mark-next-like-this)
      ("p" "prev (mc/mark-previous-like-this" mc/mark-previous-like-this)
      ]
     ]
    )
  )

(straight-use-package 'nav-flash)

(use-package nerd-icons :straight t)
(use-package nerd-icons-dired
  :straight t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(straight-use-package 'nix-mode)
(straight-use-package 'package-build)
(straight-use-package 'package-lint)
(straight-use-package 'pallet)
(use-package paredit
  :straight t
  :bind (
         :map paredit-mode-map
         ("C-j" . nil)
         ("M-s" . nil)
         ("C-M-n" . nil)
         ("C-M-p" . nil)
         )
  )
(when (memq window-system '(mac ns))
	;; allows me to copy from emacs in the terminal, and get it in the osx pasteboard
  (straight-use-package 'pbcopy)
	(require 'pbcopy)
  (turn-on-pbcopy))
(straight-use-package 'pcache)
(straight-use-package 'persistent-soft)
(use-package persp-projectile
  :straight t
  :after (perspective projectile helm-projectile)
  :demand t
  :bind (
         :map projectile-command-map
         ("p" . 'projectile-persp-switch-project)
         )
  :config
  (defun persp-projectile-helm-switch-project (project)
    (let ((projectile-completion-system 'helm))
      (projectile-persp-switch-project project)
      ))


  :custom
  (helm-source-projectile-projects-actions
   '(("Switch to project" . persp-projectile-helm-switch-project)
     ("Open Dired in project's directory `C-d'" . dired)
     ("Open project root in vc-dir or magit `M-g'"
      . helm-projectile-vc)
     ("Switch to Eshell `M-e'" . helm-projectile-switch-to-shell)
     ("Grep in projects `C-s'" . helm-projectile-grep)
     ("Compile project `M-c'. With C-u, new compile command"
      . helm-projectile-compile-project)
     ("Remove project(s) from project list `M-D'"
      . helm-projectile-remove-known-project)))
  )
(use-package perspective
  :straight t
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  ;; Turn perspective-el's own perspective-name display off entirely (the
  ;; header-line "tab bar" it used to show via `persp-show-modestring'
  ;; 'header').  jg-project-sidebar (M-a h; M-{/M-} project cycling) now
  ;; owns that role.  nil is the defcustom's documented "Off" value; it must
  ;; be set before `persp-mode' runs so `persp-init-frame' never installs
  ;; the header-line or modeline entry in the first place.
  (setq persp-show-modestring nil)
  (persp-mode)
  :custom
  (persp-mode-prefix-key nil)
  :config
  (setq persp-modestring-short nil)
  ;;(setq persp-switch-hook '(lambda () (treemacs--show-single-project default-directory default-directory)))
  (setq persp-switch-hook nil)
  )
(straight-use-package 'pkg-info)
(use-package poly-markdown
  :straight t
  :bind
  (
   :map polymode-minor-mode-map
   ("M-n" . nil)
   ("C-c n" . polymode-map)
   )
  ;;unbind the default M-n prefix
  )
(straight-use-package 'ponylang-mode)
(straight-use-package 'popup)
(straight-use-package 'popwin)
(straight-use-package 'processing-mode)
(straight-use-package 'prodigy)
(use-package projectile
  :straight t
  :bind (
         :map projectile-mode-map
         ("C-c C-p" . 'projectile-command-map)
         )
  :init
  (projectile-mode +1)
  :config
  (projectile-discover-projects-in-search-path)
  )
(straight-use-package 'python-mode)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'rainbow-mode)
(straight-use-package 'rake)
(straight-use-package 'rbenv)

(use-package restart-emacs :straight t)

(straight-use-package 'rspec-mode)
(straight-use-package 'rubocop)
(straight-use-package 'rust-mode)
(straight-use-package 's)
(straight-use-package 'scad-mode)
(straight-use-package 'shut-up)
(straight-use-package 'slim-mode)
(use-package smartparens
  :straight t
  :bind (
         :map smartparens-mode-map
         ("C-)" . sp-forward-slurp-sexp)
         ("C-M-)" . sp-slurp-hybrid-sexp)
         ("C-(" . sp-backward-slurp-sexp)
         ("C-}" . sp-forward-barf-sexp)
         ("C-{" . sp-backward-barf-sexp)
         ("C-\\" . sp-rewrap-sexp)
         ("C-|" . sp-backward-unwrap-sexp)
         ("M-r" . nil)
         )
  )
(straight-use-package 'smartrep)
(use-package ssh
  :straight t
  :bind (
         :map ssh-mode-map
         ("TAB" . nil)
         )
  )
(straight-use-package 'swift-mode)
(straight-use-package 'terraform-mode)
(straight-use-package 'tide)

(use-package treemacs
  :straight t
  :bind (
         :map treemacs-mode-map
         ("M-." . 'treemacs-root-down)
         ("M-," . 'treemacs-root-up)
         )
  )

(use-package treemacs-nerd-icons
  :straight t
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-projectile
  :straight t)
(use-package treemacs-perspective
  :straight t
  :demand t
  )


(straight-use-package 'typescript-mode)
(straight-use-package 'ucs-utils)
(straight-use-package 'undo-tree)
;;(straight-use-package 'unicode-fonts)

(straight-use-package 'vimrc-mode)

;; Wrap visual-line-mode buffers at fill-column and center text (used for
;; markdown reading buffers). adaptive-wrap is no longer needed:
;; visual-wrap-prefix-mode is built into Emacs 30+.
(use-package visual-fill-column
  :straight (:type git :host codeberg :repo "joostkremers/visual-fill-column")
  :preface
  (defun jg-visual-reading-mode ()
    "Centered, soft-wrapped, prettified reading experience for prose buffers."
    ;;(setq-local visual-fill-column-center-text t)
    (visual-fill-column-mode 1)
    (visual-wrap-prefix-mode 1)
    ;; pretty typography: variable-pitch prose, wrapped box-drawing tables,
    ;; no line numbers, a little breathing room between lines
    (variable-pitch-mode 1)
    (markdown-table-wrap-pretty-mode 1)
    ;;(display-line-numbers-mode -1)
    (setq-local line-spacing 0.1)
    (mixed-pitch-mode 1))
  ;;:custom
  ;;(visual-fill-column-enable-sensible-window-split t)
  :hook (markdown-mode . jg-visual-reading-mode))

;;(straight-use-package 'vue-mode)
(straight-use-package 'web-mode)

(straight-use-package 'which-key)
(straight-use-package 'with-editor)
(use-package ws-butler :ensure t :hook (prog-mode . ws-butler-mode))
;;(straight-use-package '(ws-butler :type git :host github :repo "lewang/ws-butler"))
(straight-use-package 'xterm-color)
(straight-use-package 'yaml-mode)
(straight-use-package 'yard-mode)
(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-keymap (kbd "TAB") nil)
  (define-key yas-keymap (kbd "C-e") 'yas-next-field)
  (define-key yas-keymap (kbd "C-a") 'yas-prev-field)
  (define-key yas-minor-mode-map (kbd "C-y") 'yas-expand)
  (define-key yas-minor-mode-map (kbd "C-M-e") 'yas-expand)
  )
(use-package yasnippet-snippets :straight t)
(straight-use-package 'sass-mode)

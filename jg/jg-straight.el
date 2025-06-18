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

(straight-use-package 'adaptive-wrap)

(use-package aidermacs
  :straight t
  :bind (("C-c a" . aidermacs-transient-menu))
  :custom
  (aidermacs-default-model "gemini-2.0-flash"))

(straight-use-package 'async)

(use-package back-button
  :straight t
  :init
  (back-button-mode)
  (defadvice back-button-pop-local-mark (after center-after-back-button-local activate)
    "Center the view after moving it"
    (recenter))
  )

(straight-use-package 'bind-key)
(straight-use-package 'coffee-mode)
;; (use-package combobulate
;;     :preface
;;     ;; You can customize Combobulate's key prefix here.
;;     ;; Note that you may have to restart Emacs for this to take effect!
;;     (setq combobulate-key-prefix "C-c o")

;;     ;; Optional, but recommended.
;;     ;;
;;     ;; You can manually enable Combobulate with `M-x
;;     ;; combobulate-mode'.
;;     :hook ((python-ts-mode . combobulate-mode)
;;            (js-ts-mode . combobulate-mode)
;;            (css-ts-mode . combobulate-mode)
;;            (yaml-ts-mode . combobulate-mode)
;;            (json-ts-mode . combobulate-mode)
;;            (typescript-ts-mode . combobulate-mode)
;;            (tsx-ts-mode . combobulate-mode))
;;     ;; Amend this to the directory where you keep Combobulate's source
;;     ;; code.
;;     :load-path "combobulate")
(straight-use-package 'company)
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
  (with-eval-after-load 'persp-mode
    (setq global-mode-string (delete '(:eval (persp-mode-line)) global-mode-string)))
  )
(straight-use-package 'drag-stuff)
(straight-use-package 'dumb-jump)
(straight-use-package 'elixir-mode)
(straight-use-package 'emojify)
(straight-use-package 'enh-ruby-mode)
(straight-use-package 'epl)
(straight-use-package 'eww-lnum)
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
  :after helm-icons
  :straight t
  :demand t
  :bind (
         :map helm-map
         ("TAB" . 'helm-execute-persistent-action)
         ("C-/" . 'helm-select-action)
         ("M-C-p" . 'helm-scroll-up)
         ("M-C-n" . 'helm-scroll-down)
         ("M-v" . 'helm-yank-selection)
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
         ("M-z" . 'helm-undo-yank-text-at-point)
         )
  :custom
  (helm-move-to-line-cycle-in-source nil)
  :config
  (helm-mode)
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (defun jg-helm-keyboard-quit () (interactive) (if mark-active (deactivate-mark) (helm-keyboard-quit)))
  )

(use-package helm-ag
  :after transient
  :straight t
  :demand t
  :bind (
         :map helm-ag-mode-map
         ("RET" . helm-ag-mode-jump-other-window)
         ("TAB" . next-logical-line)
         ("<backtab>" . previous-logical-line)
         )
  :config
  (setq helm-ag-fuzzy-match t)
  (defun helm-ag-with-prefix ()
    (interactive)
    (let ((current-prefix-arg 4)) ;; emulate C-u / universal prefix arg
      (call-interactively 'helm-ag)))
  )

(use-package helm-files
  :after helm
  :bind (
         :map helm-find-files-map
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
  )
(use-package helm-buffers
  :bind (
         :map helm-buffer-map
         ("C-d" . 'helm-buffer-run-kill-persistent)
         ("C-l" . nil)
         ("C-/" . 'helm-select-action)
         )
  )

(use-package helm-icons
  :straight t
  :init
  (setq helm-icons-provider 'nerd-icons)
  (helm-icons-enable)
  )

;; full fuzzy helm for files within project
(use-package helm-projectile
  :after (helm projectile)
  :demand t
  :straight t
  :config
  (require 'helm-for-files)
  (helm-projectile-on)
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
(straight-use-package 'js2-mode)
(straight-use-package 'json-mode)
(straight-use-package 'k8s-mode)
(straight-use-package 'keyfreq)
(straight-use-package 'less-css-mode)
(straight-use-package 'let-alist)
(straight-use-package 'list-utils)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lua-mode)
(straight-use-package 'magit)
(straight-use-package 'markdown-mode)
(straight-use-package 'maxframe)
(straight-use-package 'multiple-cursors)
(straight-use-package 'nav-flash)

(use-package nerd-icons-dired
  :straight t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(straight-use-package 'nix-mode)
(straight-use-package 'package-build)
(straight-use-package 'package-lint)
(straight-use-package 'pallet)
(straight-use-package 'paredit)
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
  (persp-mode)
  :custom
  (persp-show-modestring 'header)
  :config
  (setq persp-modestring-short nil)
  ;;(setq persp-switch-hook '(lambda () (treemacs--show-single-project default-directory default-directory)))
  (setq persp-switch-hook nil)
  )
(straight-use-package 'pkg-info)
(straight-use-package 'poly-markdown)
(straight-use-package 'ponylang-mode)
(straight-use-package 'popup)
(straight-use-package 'popwin)
(straight-use-package 'processing-mode)
(straight-use-package 'prodigy)
(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/dev/" "~/dox/" "~/dox/gems"))
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
(straight-use-package 'smartparens)
(straight-use-package 'smartrep)
(straight-use-package 'ssh)
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
;;(straight-use-package 'vue-mode)
(straight-use-package 'web-mode)
(straight-use-package 'which-key)
(straight-use-package 'with-editor)
(straight-use-package '(ws-butler :type git :host github :repo "lewang/ws-butler"))
(straight-use-package 'xterm-color)
(straight-use-package 'yaml-mode)
(straight-use-package 'yard-mode)
(straight-use-package 'sass-mode)

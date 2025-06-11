(require 'unicode-fonts)
(unicode-fonts-setup)

(require 'tramp)
(setq tramp-default-method "ssh") ; default is "scp"

(add-to-list 'auto-mode-alist '("\\.el" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
(add-hook 'emacs-lisp-mode-hook #'(lambda ()
                                    (paredit-mode t)
                                    ))

(cua-mode -1)
;; save my place in each file
(save-place-mode t)


(customize-set-variable 'persp-show-modestring 'header)
(projectile-mode +1)
(setq projectile-project-search-path '("~/dev/" "~/dox/" "~/dox/gems"))
(require 'perspective)
(setq persp-suppress-no-prefix-key-warning t)
(persp-mode)
(require 'persp-projectile)
(setq projectile-switch-project-action 'projectile-run-shell)
(add-hook 'kill-emacs-hook #'persp-state-save)
(setq persp-modestring-short nil)

(require 'avy)

(require 'undo-tree)
;; i stole this from the undo-tree code to override it because its "heuristic"
;; to determine whether to *actually* enable global undo-tree-mode is wrong,
;; and there is no variable/mechanism to stop this behavior.
;; (define-globalized-minor-mode global-undo-tree-mode
;;   undo-tree-mode turn-on-undo-tree-mode)
(define-globalized-minor-mode jg-global-undo-tree-mode
  undo-tree-mode undo-tree-mode)
(jg-global-undo-tree-mode)
(setq undo-tree-auto-save-history nil)

(require 'ag)
(setq ag-group-matches nil)
(setq ag-highlight-search t)
(customize-set-variable 'ag-arguments '("-W" "200"
                                        "--line-number"
                                        "--smart-case"
                                        "--nogroup"
                                        "--column"
                                        "--stats"
                                        "--ignore" "node_modules"
                                        "--ignore" "*.js.map"
                                        "--ignore" "*.min.js"
                                        "--hidden"
                                        "--"
                                        ))



(multiple-cursors-mode)
(delete-selection-mode)

(customize-set-variable 'git-link-default-branch "master")

(require 'jg-paredit)

(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(setq sp-highlight-pair-overlay nil)

(require 'jg-quicknav)

(require 'jg-switch-buffer)

(require 'view)

(global-subword-mode 1)


(require 'doom-modeline)
(setq doom-modeline-height 16)
;;(setq doom-modeline-persp-name nil)
(doom-modeline-mode +1)
(setq global-mode-string (delete '(:eval (persp-mode-line)) global-mode-string))

(require 'magit)
(with-eval-after-load 'magit
  (require 'forge))

;; original:
;;(setq magit-status-headers-hook '(magit-insert-error-header magit-insert-diff-filter-header magit-insert-head-branch-header magit-insert-upstream-branch-header magit-insert-push-branch-header magit-insert-tags-header))
(setq magit-status-headers-hook '(magit-insert-error-header magit-insert-diff-filter-header magit-insert-head-branch-header magit-insert-upstream-branch-header magit-insert-push-branch-header)) ; remove tags header, it's slow and I don't care about it

(setq auth-sources '("~/.authinfo"))

(setq forge-post-mode-hook nil) ;; turns on flyspell by default which has annoying keybindings that conflict with jg-*-mode


(column-number-mode 1)
(size-indication-mode 1)


;; stolen from ruby-mode to make expand-region work with enhanced ruby mode
(defvar ruby-block-end-re "\\<end\\>")
(defvar ruby-block-beg-keywords
  '("class" "module" "def" "if" "unless" "case" "while" "until" "for" "begin" "do")
  "Keywords at the beginning of blocks.")
(defvar ruby-block-beg-re
  (regexp-opt ruby-block-beg-keywords)
  "Regexp to match the beginning of blocks.")
(require 'expand-region)
;; (require 'ruby-mode)
;; (setq ruby-insert-encoding-magic-comment nil)
;; (setq ruby-use-smie nil)
;; (setq ruby-deep-indent-paren nil)
;; (setq ruby-deep-indent-paren-style 'space)
;; (setq ruby-align-to-stmt-keywords nil)
;; (setq ruby-indent-level 2)


(require 'enh-ruby-mode)
(add-hook 'enh-ruby-mode-hook 'erm-define-faces)


(setq enh-ruby-program "~/.rbenv/shims/ruby") ; so that still works if ruby points to ruby1.8 or jruby
(setq ruby-insert-encoding-magic-comment nil) ; for ruby-mode
(setq enh-ruby-add-encoding-comment-on-save nil) ; for enh-ruby-mode


(require 'lsp-mode)
(setq lsp-keymap-prefix "H-l")
(define-key lsp-mode-map (kbd "H-l") lsp-command-map)
(setq lsp-headerline-breadcrumb-enable nil)


;; (require 'lsp-sourcekit)
;; ;;(setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Library/Developer/Toolchains/swift-DEVELOPMENT-SNAPSHOT-2018-11-01-a.xctoolchain")
;; (setq lsp-sourcekit-executable (expand-file-name "/Users/jgran/dev/sourcekit-lsp/.build/debug/sourcekit-lsp"))

;; (use-package swift-mode
;;   :hook (swift-mode . (lambda () (lsp))))


;; Markdown support
;; (autoload 'markdown-mode "markdown-mode.el"
;;   "Major mode for editing Markdown files" t)
;; (add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(require 'markdown-mode)
(require 'poly-markdown)

;; use web mode for html snippets inside markdown mode.
;; example:
;; ## Markdown header
;; <>
;; <div> some html code </div>
;; </>
;; (define-innermode poly-markdown-web-mode-innermode poly-markdown-root-innermode
;;   :mode 'web-mode
;;   ;; :head-matcher (cons "^<>$" 1)
;;   ;; :tail-matcher (cons "^</>$" 1)
;;   :head-matcher "^###"
;;   :tail-matcher "^---"
;;   )
(define-innermode poly-markdown-web-innermode
  :mode 'web-mode
  :head-matcher "^<!-- html -->$"
  :tail-matcher "^<!-- /html -->$"
  :head-mode 'host
  :tail-mode 'host)

(define-polymode poly-markdown-mode
  :hostmode 'poly-markdown-hostmode
  :innermodes '(poly-markdown-fenced-code-innermode
                ;; poly-markdown-inline-code-innermode
                poly-markdown-web-innermode
                poly-markdown-displayed-math-innermode
                poly-markdown-inline-math-innermode
                poly-markdown-yaml-metadata-innermode))


;; php mode
(require 'php-mode)


;; js mode
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq sgml-basic-offset 2)

(require 'web-mode)

(add-hook 'web-mode-hook  '(lambda ()
                             (setq web-mode-style-padding 0
                                   web-mode-script-padding 0
                                   web-mode-block-padding 0
                                   web-mode-markup-indent-offset 2
                                   web-mode-css-indent-offset 2
                                   web-mode-code-indent-offset 2)
                             ))
(setf (alist-get 'web-mode lsp--formatting-indent-alist) 'web-mode-code-indent-offset) ;; fixes indentation
(add-to-list 'auto-mode-alist '("\\.tsx?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (or
                   (string-equal "tsx" (file-name-extension buffer-file-name))
                   (string-equal "ts" (file-name-extension buffer-file-name)))
              (setup-tide-mode))))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "vue" (file-name-extension buffer-file-name))
              (lsp-deferred))))

(add-to-list 'auto-mode-alist '("\\.conkerorrc$" . web-mode))
(add-to-list 'interpreter-mode-alist '("node" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.eex$" . web-mode))


(add-to-list 'web-mode-content-types
             '("sass" . "\\.sass\\'"))


;; set to jsx mode by default in web-mode
(add-hook 'web-mode-hook
          (lambda ()
            (if (equal web-mode-content-type "javascript")
                (web-mode-set-content-type "jsx")
              (message "now set to: %s" web-mode-content-type))))

;; another thing to try...
;; (setq web-mode-content-types-alist
;;   '(("jsx" . "\\.js[x]?\\'")))

;; (web-mode-set-content-type "jsx") ; to force jsx mode

(add-hook 'web-mode-hook
          #'(lambda() (flycheck-mode)))


;; adjust indents for web-mode to 2 spaces
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-attr-indent-offset 2)
(setq web-mode-enable-auto-indentation nil)

(eval-after-load 'web-mode #'(lambda ()
                               (define-key web-mode-map (kbd "M-;") 'demi-brolin)))

;;(add-to-list 'auto-mode-alist '("\\.tsx?$" . typescript-mode))
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(add-hook 'typescript-mode-hook #'setup-tide-mode)


(require 'flycheck)

(setq-default flycheck-temp-prefix ".flycheck")

;; add web-mode to the list of valid modes that these flycheck checkers can run in
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'typescript-mode)


;; coffeescript mode
(add-to-list 'load-path "~/.emacs.d/coffee-mode")
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2)
  (set (make-local-variable 'coffee-cleanup-whitespace) nil)
  )
(add-hook 'coffee-mode-hook
          #'(lambda() (coffee-custom)))

(customize-set-variable 'coffee-tab-width 2)

;;(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
;;(autoload 'css-mode "css-mode" "CSS editing mode" t)
(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))



;;(require 'shell-script-mode)
(add-to-list 'auto-mode-alist '("\\.aliases$" . sh-mode))

;; terraform
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
(company-terraform-init)





(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  ;; (if (not (string-match "go" compile-command))   ; set compile command default
  ;;     (set (make-local-variable 'compile-command)
  ;;          "go build -v && go test -v && go vet"))

  ;;(flycheck-mode)

  ;; Key bindings specific to go-mode
  ;; (local-set-key (kbd "M-.") 'godef-jump)         ; Go to definition
  ;; (local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
  ;; (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  ;; (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  ;; (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  ;; (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg

  )                         ; Enable auto-complete mode
(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'go-mode-hook 'lsp-deferred)




(require 'which-key)
(which-key-mode)



;; make comments automatically go to multiple lines for long ones
(auto-fill-mode t)
(setq comment-auto-fill-only-comments t)



(add-hook 'ruby-mode-hook #'(lambda() (flycheck-mode))) ; for rubocop/ruby-mode
(add-hook 'enh-ruby-mode-hook #'(lambda() (flycheck-mode))) ; for rubocop/enh-ruby-mode
(add-hook 'ruby-ts-mode-hook #'(lambda() (flycheck-mode))) ; for rubocop/enh-ruby-mode
(add-hook 'enh-ruby-mode-hook 'lsp-deferred)
(add-hook 'enh-ruby-mode-hook 'yard-mode)
(add-hook 'enh-ruby-mode-hook 'eldoc-mode)
(setq flycheck-rubocoprc ".ruby-style.yml")

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json_builder$" . ruby-ts-mode))
;;(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\.builder\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json_builder\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("Thorfile\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("Fastfile" . ruby-ts-mode))


(require 'xterm-color)
(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))


;; bash
(setq shell-file-name "/usr/bin/bash")
(setq comint-process-echoes t)
(setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))


(add-hook 'shell-mode-hook
          (lambda ()
            (setq comint-process-echoes t)
            ;; Disable font-locking in this buffer to improve performance
            (font-lock-mode -1)
            ;; Prevent font-locking from being re-enabled in this buffer
            (make-local-variable 'font-lock-function)
            (setq font-lock-function (lambda (_) nil))
            (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)
            ))


(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil) ; don't override the prompt colors


;; use c++ for torquescript
(add-to-list 'auto-mode-alist '("\\.cs$" . c++-mode))
(setq c-basic-offset 4)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))


;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)



;; set exterior coding system so copy/paste of utf-8 stuff will work.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setenv "LANG" "en_US.UTF-8")


(require 'back-button)
(back-button-mode 1)



(setq-default save-interprogram-paste-before-kill t)
(setq-default indent-tabs-mode nil)

(require 'grep-buffers)

(global-display-line-numbers-mode 1)

;; successor to smex. better M-x
(amx-mode)

;; minibuffer completions vertical display
;(setq prescient-filter-method '(literal fuzzy regexp initialism))
(setq prescient-filter-method '(literal initialism prefix regexp fuzzy)
      prescient-use-char-folding t
      prescient-use-case-folding 'smart
      prescient-sort-full-matches-first t ; Works well with `initialism'.
      prescient-sort-length-enable t
      vertico-prescient-enable-sorting nil)

(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
;;(setq completion-styles '(basic substring flex initials))

(vertico-prescient-mode 1)
(prescient-persist-mode 1)
(setq vertico-cycle t)

(defun vertico-delete-buffer ()
  (interactive)
  (persp-kill-buffer* (vertico--candidate))
  (setq vertico--candidates (persp-current-buffer-names)
        vertico--total (length (persp-current-buffer-names))
        vertico--index (min vertico--total 0)
        )
  )
;;completion-table-with-predicate

;; taken from https://github.com/minad/vertico/wiki#pre-select-previous-directory-when-entering-parent-directory-from-within-find-file
;;-----------------------------------------------------------------------------
(defvar previous-directory nil
  "The directory that was just left. It is set when leaving a directory and
    set back to nil once it is used in the parent directory.")

(defun set-previous-directory ()
  "Set the directory that was just exited from within find-file."
  ;; (when (> (minibuffer-prompt-end) (point))
  (when t
    (save-excursion
      (goto-char (1- (point)))
      (when (search-backward "/" (minibuffer-prompt-end) t)
        ;; set parent directory
        (setq previous-directory (buffer-substring (1+ (point)) (point-max)))
        ;; set back to nil if not sorting by directories or what was deleted is not a directory
        (when (not (string-suffix-p "/" previous-directory))
          (setq previous-directory nil))
        t))))

(advice-add #'vertico-directory-up :before #'set-previous-directory)

(define-advice vertico--update (:after (&rest _) choose-candidate)
  "Pick the previous directory rather than the prompt after updating candidates."
  (cond
   (previous-directory                  ; select previous directory
    (setq vertico--index (or (seq-position vertico--candidates previous-directory)
                             vertico--index))
    (setq previous-directory nil))))

;;-----------------------------------------------------------------------------


(require 'ws-butler)
(ws-butler-global-mode)

;; kill the buffer upon completion of the process.
(defun kill-buffer-on-exit-shell ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (lexical-let ((buff buff))
      (set-process-sentinel proc (lambda (process event)
                                   (if (string= event "finished\n")
                                       (progn
                                         (kill-buffer buff))))))))

;; term-mode
;;(add-hook 'term-exec-hook 'kill-buffer-on-exit-shell)

;; shell mode
(add-hook 'shell-mode-hook 'kill-buffer-on-exit-shell)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)


;; (defadvice display-message-or-buffer (before ansi-color activate)
;;   "Process ANSI color codes in shell output."
;;   (let ((buf (ad-get-arg 0)))
;;     (and (bufferp buf)
;;          (string= (buffer-name buf) "*Shell Command Output*")
;;          (with-current-buffer buf
;;            (ansi-color-apply-on-region (point-min) (point-max))))))



(require 'dired-subtree)
(setq diredp-hide-details-initially-flag nil)
(dired-filter-mode)


(recentf-mode 1)
(setq recentf-max-saved-items nil)


(pixel-scroll-precision-mode)
(setq scroll-preserve-screen-position "yes")


;; even sweeter auto-complete!?
(global-company-mode)
(global-set-key (kbd "TAB") 'company-complete)
(setq company-idle-delay nil)
(setq company-dabbrev-downcase nil)
(setq company-tooltip-align-annotations t)



(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-y") 'yas-expand)
(define-key yas-minor-mode-map (kbd "C-M-e") 'yas-expand)



;; ASIDE: if you call ssh from shell directly, add "-t" to explicit-ssh-args to enable terminal.

;; bash autocomplete working perfectly!
(with-eval-after-load 'shell
  (native-complete-setup-bash))
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-native-complete))
;;(add-to-list 'company-backends '(company-shell company-shell-env))
(setq tramp-shell-prompt-pattern ".*[#$%>)] *")



(setq tags-revert-without-query t)
(global-set-key (kbd "C-c C-t") 'ctags-create-or-update-tags-table)
(setq tags-case-fold-search nil)
;;notes
;; try (tags-search)
;; try (find-tag)


;; scad mode
(autoload 'scad-mode "scad-mode" "Major mode for editing SCAD code." t)
(add-to-list 'auto-mode-alist '("\\.scad$" . scad-mode))
(add-to-list 'auto-mode-alist '("\\.escad$" . scad-mode))



;;; org mode
;; (setq org-todo-keywords
;;       '((sequence "TODO" "WORKING" "DONE")))


(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)


;; tree-sitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;;(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)) ; compiles all languages above ^ - should be done periodically to get latest versions


(global-emojify-mode)

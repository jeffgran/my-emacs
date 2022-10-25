(add-to-list 'auto-mode-alist '("\\.el" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                   (paredit-mode t)
                                   ))

(cua-mode -1)
;; save my place in each file
(require 'saveplace)
(setq save-place-file (concat emacs-root "saved-places"))
(setq-default save-place t)


(projectile-mode +1)
(setq projectile-project-search-path '("~/dev/" "~/dox/" "~/dox/gems"))
(define-key projectile-mode-map (kbd "H-p") 'projectile-command-map)
(require 'perspective)
(global-set-key (kbd "C-x C-b") 'persp-list-buffers)
;;(customize-set-variable 'persp-mode-prefix-key (kbd "C-x p"))
(customize-set-variable 'persp-mode-prefix-key nil)
(persp-mode)
(require 'persp-projectile)
(define-key projectile-command-map (kbd "p") 'projectile-persp-switch-project)
(setq projectile-switch-project-action 'projectile-run-shell)

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
(setq ag-arguments (list
                    "-W" "200"
                    "--line-number"
                    "--smart-case"
                    "--nogroup"
                    "--column"
                    "--stats"
                    "--ignore" "proxima_nova.scss"
                    "--ignore" "react_bundle.js"
                    "--ignore" "node_modules"
                    "--ignore" "*.js.map"
                    "--ignore" "*.min.js"
                    "--"
                    ))



(multiple-cursors-mode)
(delete-selection-mode)

;; allows me to copy from emacs in the terminal, and get it in the osx pasteboard
(turn-on-pbcopy)

(require 'jg-paredit)

(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(setq sp-highlight-pair-overlay nil)

(require 'jg-quicknav)

(require 'jg-switch-buffer)

;; god-mode cursor switch:
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

(require 'view)

(setq helm-idle-delay 1
      helm-input-idle-delay 1
      helm-quick-update nil
      helm-buffer-max-length 70
      helm-recentf-fuzzy-match t
      )

(require 'helm-find)
(require 'helm-fuzzy-find)

(global-subword-mode 1)

(require 'smart-mode-line)
(add-hook 'after-init-hook '(lambda ()
                              (sml/setup)
                              (setq sml/no-confirm-load-theme t)
                              (sml/apply-theme 'respectful)))


(require 'magit)
(with-eval-after-load 'magit
  (require 'forge))
(setq auth-sources '("~/.authinfo"))

(column-number-mode 1)
(size-indication-mode 1)

;; (require 'rbenv)
;; (global-rbenv-mode)

;; sml weirdly puts the cursor location info in this... undo that.
(setq-default mode-line-front-space "")

(setq-default mode-line-format
      `(
       "%e"
       mode-line-front-space
       mode-line-mule-info
       mode-line-client
       mode-line-modified
       mode-line-remote
       mode-line-frame-identification
       mode-line-buffer-identification
       sml/pos-id-separator
       mode-line-position
       smartrep-mode-line-string
       rbenv--modestring
       ;;(vc-mode vc-mode)
       sml/pre-modes-separator
       mode-line-modes
       mode-line-misc-info
       mode-line-end-spaces
       ))



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

(add-to-list 'auto-mode-alist '("\\.conkerorrc$" . web-mode))
(add-to-list 'interpreter-mode-alist '("node" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.eex$" . web-mode))


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
          '(lambda() (flycheck-mode)))


;; adjust indents for web-mode to 2 spaces
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-attr-indent-offset 2)
(setq web-mode-enable-auto-indentation nil)

(eval-after-load 'web-mode '(lambda ()
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

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

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
          '(lambda() (coffee-custom)))

(custom-set-variables '(coffee-tab-width 2))

;;(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
;;(autoload 'css-mode "css-mode" "CSS editing mode" t)
(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
;;(custom-set-variables '(css-indent-offset 2))



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









;; make comments automatically go to multiple lines for long ones
(auto-fill-mode t)
(setq comment-auto-fill-only-comments t)


(add-hook 'ruby-mode-hook '(lambda() (flycheck-mode))) ; for rubocop/ruby-mode
(add-hook 'enh-ruby-mode-hook '(lambda() (flycheck-mode))) ; for rubocop/enh-ruby-mode
(setq flycheck-rubocoprc ".ruby-style.yml")

(defadvice ruby-electric-setup-keymap (after undo-some-keybindings-from-ruby-electric-mode activate)
  "undo some stuff ruby-electric tries to force on us"
  ;; fucking ruby-electric remaps keys in ruby-mode-map. USE YOUR OWN MAP!
  (define-key ruby-mode-map (kbd "TAB") nil)
  (define-key ruby-mode-map (kbd "RET") nil)
  (define-key ruby-mode-map (kbd "C-m") nil)
  (define-key ruby-mode-map (kbd "SPC") nil)
  (define-key ruby-mode-map (kbd "C-j") nil)
  (define-key ruby-mode-map (kbd "C-M-n") nil)
  (define-key ruby-mode-map (kbd "C-M-p") nil))


(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.json_builder$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.json_builder\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Fastfile" . enh-ruby-mode))


(require 'xterm-color)
(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))

(add-hook 'shell-mode-hook
          (lambda ()
            ;; Disable font-locking in this buffer to improve performance
            (font-lock-mode -1)
            ;; Prevent font-locking from being re-enabled in this buffer
            (make-local-variable 'font-lock-function)
            (setq font-lock-function (lambda (_) nil))
            (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))


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

(require 'linum)
(global-linum-mode 1)

;; successor to smex. better M-x
(amx-mode)

;; minibuffer completions vertical display
(selectrum-mode)
(selectrum-prescient-mode) ; not just prefix matching in minibuffer completions
(hotfuzz-selectrum-mode)   ; fuzzy matching in completion

;;(setq selectrum-display-action '(display-buffer-in-tab)) ;; there are different options
(setq selectrum-display-action nil) ;; default


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


(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))



(require 'dired-subtree)
(setq diredp-hide-details-initially-flag nil)


;;(require 'smooth-scroll)
;;(smooth-scroll-mode nil)
;;(setq smooth-scroll/vscroll-step-size 4)
(setq scroll-preserve-screen-position "yes")


;; even sweeter auto-complete!?
(global-company-mode)
(global-set-key (kbd "TAB") 'company-complete)
(setq company-idle-delay nil)
(setq company-dabbrev-downcase nil)
(setq company-tooltip-align-annotations t)



(require 'yasnippet)
;; (setq yas-snippet-dirs '("~/.emacs.d/snippets" yas-installed-snippets-dir)) ;; default in case i need to reset it
(setq yas-snippet-dirs (append yas-snippet-dirs `(,(concat emacs-root "jg/yas"))))

(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-y") 'yas-expand)
(define-key yas-minor-mode-map (kbd "C-M-e") 'yas-expand)





(setq shell-file-name "/usr/local/bin/bash")
(setq explicit-shell-file-name "/usr/local/bin/bash")
(setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
(setq comint-process-echoes t)
;; ASIDE: if you call ssh from shell directly, add "-t" to explicit-ssh-args to enable terminal.

;; bash autocomplete working perfectly!
(with-eval-after-load 'shell
  (native-complete-setup-bash))
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-native-complete))

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
(setq org-todo-keywords
       '((sequence "TODO" "WORKING" "DONE")))

(setq fiplr-ignored-globs '((directories (".git" ".svn" "tmp" "temp"))
                            (files ("*.jpg" "*.png" "*.zip" "*~" ".keep"))))


(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)


(require 'ql-mode-base)

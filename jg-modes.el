(add-to-list 'auto-mode-alist '("\\.el" . emacs-lisp-mode))
(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                   (paredit-mode t)
                                   ))




;; only turn the tabs stuff on in windowed mode (not the terminal)
(if window-system
    (progn
      ;; Elscreen (tabs/session management)
      ;; my custom elscreen buffer list (separate buffer list per screen). :)
      (elscreen-start)
      (setq elscreen-tab-display-kill-screen nil) ;; turn off the [x] button for the mouse
      (setq elscreen-tab-display-control nil) ;; turn off the <-> tab switch button for the mouse
      (setq elscreen-display-screen-number nil) ;; turn off the tab number display in the mode-line
      (require 'jg-elscreen-buffer-list)
      ))


;; allows me to copy from emacs in the terminal, and get it in the osx pasteboard
(require 'pbcopy)
(turn-on-pbcopy)

(require 'jg-paredit)

(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(setq sp-highlight-pair-overlay nil)

(require 'jg-quicknav)

(require 'view)

(setq helm-idle-delay 0
      helm-input-idle-delay 0
      helm-quick-update nil
      helm-buffer-max-length 70
      )



;; stolen from ruby-mode to make expand-region work with enhanced ruby mode
(defvar ruby-block-end-re "\\<end\\>")
(defvar ruby-block-beg-keywords
  '("class" "module" "def" "if" "unless" "case" "while" "until" "for" "begin" "do")
  "Keywords at the beginning of blocks.")
(defvar ruby-block-beg-re
  (regexp-opt ruby-block-beg-keywords)
  "Regexp to match the beginning of blocks.")
(require 'expand-region)
(require 'ruby-mode)

                                        ; Markdown support
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; HTML mode
;; nXhtml mode (new and improved)
;; (setq
;;  nxhtml-global-minor-mode nil
;;  mumamo-chunk-coloring 'submode-colored
;;  nxhtml-skip-welcome t
;;  indent-region-mode t
;;  rng-nxml-auto-validate-flag nil
;;  ;;mumamo-margin-use (quote (right-margin 13))
;;  nxml-degraded t)
(add-hook 'nxml-mode-hook '(lambda () (abbrev-mode -1)))


;;(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . eruby-nxhtml-mumamo-mode))

(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(mmm-add-mode-ext-class 'html-erb-mode "\\.html\\.erb\\'" 'erb)
(mmm-add-mode-ext-class 'html-erb-mode nil 'html-js)
(mmm-add-mode-ext-class 'html-erb-mode nil 'html-css)
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . html-erb-mode))

(add-to-list 'mmm-mode-ext-classes-alist '(nxml-mode nil html-js))
(add-to-list 'mmm-mode-ext-classes-alist '(nxml-mode nil html-erb-mode))


(add-hook 'mmm-html-mode-hook '(lambda () (abbrev-mode -1)))
(add-hook 'mmm-ruby-mode-hook '(lambda () (abbrev-mode -1)))
(add-hook 'html-erb-mode-hook '(lambda () (abbrev-mode -1)))


(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
;;(require 'ruby-mode) ;; for enh-ruby

;; RNC mode
(require 'rnc-mode)
(add-to-list 'auto-mode-alist '("\\.rnc$" . rnc-mode))


;; php mode
(require 'php-mode)


;; js mode
(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.conkerorrc$" . js-mode))


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



;;(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
;;(autoload 'css-mode "css-mode" "CSS editing mode" t)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))


;;(require 'shell-script-mode)
(add-to-list 'auto-mode-alist '("\\.aliases$" . sh-mode))



;; make comments automatically go to multiple lines for long ones
(auto-fill-mode t)
(setq comment-auto-fill-only-comments t)

(add-hook 'ruby-mode-hook 'robe-mode)
;; (add-hook 'ruby-mode-hook
;;           '(lambda ()
;;              (setq company-backend '(company-robe company-keywords) )
;;              ))

;; (add-hook 'ruby-mode-hook
;;           '(lambda ()
;;              ;;(make-local-variable 'paragraph-start)
;;              ;;(setq paragraph-start (concat "@[[:alpha:]]+\\|" paragraph-start))
;;              ;;
;;              ;;(make-local-variable 'paragraph-separate)
;;              ;;(setq paragraph-separate (concat "---+\\|" paragraph-separate))
;;              ;;(ruby-end-mode t)

;;              (make-local-variable 'post-command-hook)
;;              (add-hook 'post-command-hook 'font-lock-fontify-buffer)

;;              ;; turn electric pair mode off; ruby has its own electricity
;;              (electric-pair-mode -1)
;;              ;;(ruby-electric-mode t)
;;              ))

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




(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.json_builder\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))

(setq enh-ruby-program "/Users/jeffgran/.rbenv/shims/ruby") ; so that still works if ruby points to ruby1.8 or jruby
;;(require 'ruby-mode) ;; currently using enhanced ruby...
;;(setq ruby-deep-indent-paren     nil
;;      ruby-hanging-indent-level  2
;;      ruby-extra-keywords        '("raise"))
;;(ruby-local-enable-extra-keywords)


(require 'ansi-color)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; use c++ for torquescript
(add-to-list 'auto-mode-alist '("\\.cs$" . c++-mode))
(setq c-basic-offset 4)


(require 'centered-cursor-mode)
(setq ccm-recenter-at-end-of-file t)

(require 'srb-adaptive-wrap-mode)
;;; srb-adaptive-wrap hooks
(add-hook 'ruby-mode-hook (lambda () (srb-adaptive-wrap-mode 1)))

(require 'zoom-frm)

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)


(require 'back-button)
(back-button-mode 1)

(require 'fuzzy-find-in-project)


(setq-default save-interprogram-paste-before-kill t)
(setq-default indent-tabs-mode nil)

(require 'redo)
(require 'grep-buffers)

(require 'linum)
(global-linum-mode 1)


(require 'wcy-swbuff)


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
(add-hook 'term-exec-hook 'kill-buffer-on-exit-shell)

;; shell mode
(add-hook 'shell-mode-hook 'kill-buffer-on-exit-shell)

(add-hook 'inf-ruby-mode-hook 'kill-buffer-on-exit-shell)



(defadvice display-message-or-buffer (before ansi-color activate)
  "Process ANSI color codes in shell output."
  (let ((buf (ad-get-arg 0)))
    (and (bufferp buf)
         (string= (buffer-name buf) "*Shell Command Output*")
         (with-current-buffer buf
           (ansi-color-apply-on-region (point-min) (point-max))))))


(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))



(require 'dired+)

(eval-after-load "eww"
  '(progn (define-key eww-mode-map "f" 'eww-lnum-follow)
          (define-key eww-mode-map "F" 'eww-lnum-universal)))

;; sweet auto-complete!
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/my-emacs/ac-dict")
;; (ac-config-default)
;; (global-auto-complete-mode t)

;; (define-key ac-complete-mode-map "\C-n" 'ac-next)
;; (define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; (setq ac-auto-start nil)
;; (setq ac-auto-show-menu t)
;; (setq ac-use-fuzzy nil)
;; ;;(setq ac-delay 0.8)
;; (setq ac-expand-on-auto-complete t)
;; (setq ac-menu-height 15)


;; (global-set-key (kbd "TAB") 'ac-start)
;; (define-key ac-completing-map (kbd "C-f") 'ac-isearch)
;; (define-key ac-completing-map (kbd "RET") 'ac-complete)
;; (define-key ac-completing-map (kbd "TAB") 'auto-complete)

;; even sweeter auto-complete!?
(global-company-mode)
(global-set-key (kbd "TAB") 'company-complete)
(setq company-idle-delay nil)


(setq shell-file-name "/usr/local/bin/bash")
(setq explicit-shell-file-name "/usr/local/bin/bash")
(setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
(setq comint-process-echoes t)
;; ASIDE: if you call ssh from shell directly, add "-t" to explicit-ssh-args to enable terminal.
;;(require 'readline-complete)

;;(add-to-list 'ac-modes 'ssh-mode)
;;(add-hook 'ssh-mode-hook 'ac-rlc-setup-sources)
;;(add-hook 'ssh-mode-hook 'jg-setup-ac-rlc)

(setq tramp-shell-prompt-pattern ".*[#$%>] *")


;;(add-to-list 'ac-modes 'shell-mode)
;;(add-hook 'shell-mode-hook 'ac-rlc-setup-sources)
;;(add-hook 'shell-mode-hook 'jg-setup-ac-rlc)

;; (defun jg-setup-ac-rlc ()
;;   ;; for some reason have to override these again here
;;   (setq ac-auto-start nil)
;;   (setq ac-auto-show-menu t)
;;   (setq ac-use-fuzzy nil)
;; )

;; Rsense + Autocomplete
;; (setq rsense-home "/usr/local/Cellar/rsense/0.3/libexec/")
;; (require 'rsense)

;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (add-to-list 'ac-sources 'ac-source-rsense-method)
;;             (add-to-list 'ac-sources 'ac-source-rsense-constant)))

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

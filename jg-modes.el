;;; TODO:;;;   but still use jg-code-mode. Use paredit and rainbow delimiters as well.
;;;
;;;
;;;


;; undo stuff prelude is trying to force on me
;;(setq before-save-hook nil) ;; cleaning up whitespace for me.
;;(setq prelude-lisp-coding-hook 'rainbow-delimiters-mode) ;; setting annoying paredit mode
;;(setq prelude-interactive-lisp-coding-hook 'rainbow-delimiters-mode) ;; setting annoying paredit mode
;;(add-hook 'prog-mode-hook 'whitespace-turn-off t)
;;(whitespace-mode -1)


(add-to-list 'auto-mode-alist '("\\.el" . emacs-lisp-mode))
(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                   (paredit-mode t)
                                   ))




;; only turn the tabs stuff on in windowed mode (not the terminal)
(if window-system
    (progn
      ;; Elscreen (tabs/session management)
      ;; my custom elscreen buffer list (separate buffer list per screen). :)
      (load "elscreen" "ElScreen" t)
      (setq elscreen-tab-display-kill-screen nil) ;; turn off the [x] button for the mouse
      (setq elscreen-tab-display-control nil) ;; turn off the <-> tab switch button for the mouse
      (setq elscreen-display-screen-number nil) ;; turn off the tab number display in the mode-line
      (require 'jg-elscreen-buffer-list)
      ))


;; allows me to copy from emacs in the terminal, and get it in the osx pasteboard
(require 'pbcopy)
(turn-on-pbcopy)

(require 'jg-paredit)

(require 'jg-quicknav)

;; prelude gives me helm and yasnippet... I think I might like this
;; way of accessing the snippets better
;;(require 'helm-c-yasnippet)
;; about fuzzy matching for helm
;; ,----[ C-h v helm-mp-matching-method RET ]
;; | helm-mp-matching-method is a variable defined in `helm-match-plugin.el'.
;; | Its value is multi3
;; |
;; | Documentation:
;; | Matching method for helm match plugin.
;; | You can set here different methods to match candidates in helm.
;; | Here are the possible value of this symbol and their meaning:
;; | - multi1: Respect order, prefix of pattern must match.
;; | - multi2: Same but with partial match.
;; | - multi3: The best, multiple regexp match, allow negation.
;; | - multi3p: Same but prefix must match.
;; | Default is multi3.
;; |
;; | You can customize this variable.
;; `----

(setq helm-idle-delay 0
      helm-input-idle-delay 0
      helm-quick-update nil
      helm-buffer-max-length 70
      )




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

(require 'erc)
(setq erc-autojoin-channels-alist '(("192.168.10.10" "#eng" "#daveco")))


;; (require 'rails)
;; (setq ruby-insert-encoding-magic-comment nil) ;; turn off the # -*- coding: utf-8 -*- comments

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


(require 'rvm)
(rvm-use-default)
;;(rvm-autodetect-ruby) ;; using my project switcher now instead of this.

(require 'rspec-mode)
(require 'shoulda-mode)
(setq shoulda-use-rvm t)
(setq rspec-use-rvm t)

;; make comments automatically go to multiple lines for long ones
(auto-fill-mode t)
(setq comment-auto-fill-only-comments t)

;;(require 'ruby-end)
(add-hook 'ruby-mode-hook
          '(lambda ()
             ;;(make-local-variable 'paragraph-start)
             ;;(setq paragraph-start (concat "@[[:alpha:]]+\\|" paragraph-start))
             ;;
             ;;(make-local-variable 'paragraph-separate)
             ;;(setq paragraph-separate (concat "---+\\|" paragraph-separate))
             ;;(ruby-end-mode t)

             (make-local-variable 'post-command-hook)
             (add-hook 'post-command-hook 'font-lock-fontify-buffer)

             ;; turn electric pair mode off; ruby has its own electricity
             (electric-pair-mode -1)
             (ruby-electric-mode t)))

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

(setq enh-ruby-program "~/.rvm/rubies/ruby-1.9.3-p327/bin/ruby") ; so that still works if ruby points to ruby1.8 or jruby
(require 'ruby-mode)
(setq ruby-deep-indent-paren     nil
      ruby-hanging-indent-level  2
      ruby-extra-keywords        '("raise"))
(ruby-local-enable-extra-keywords)



;; stolen from ruby-mode to make expand-region work with enhanced ruby mode
(defconst ruby-block-end-re "\\<end\\>")
(defconst ruby-block-beg-keywords
  '("class" "module" "def" "if" "unless" "case" "while" "until" "for" "begin" "do")
  "Keywords at the beginning of blocks.")
(defconst ruby-block-beg-re
  (regexp-opt ruby-block-beg-keywords)
  "Regexp to match the beginning of blocks.")
(require 'expand-region)

(require 'rspec-mode)
(require 'ansi-color)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; use c++ for torquescript
(add-to-list 'auto-mode-alist '("\\.cs$" . c++-mode))
(setq c-basic-offset 4)


(require 'centered-cursor-mode)
(setq ccm-recenter-at-end-of-file t)

(require 'srb-adaptive-wrap-mode)
;;; srb-adaptive-wrap hooks
(add-hook 'ruby-mode-hook (lambda () (srb-adaptive-wrap-mode 1)))
;;(add-hook 'nxhtml-mode-hook (lambda () (srb-adaptive-wrap-mode 1)))
;;(add-hook 'nxhtml-mode-hook (lambda () (srb-adaptive-wrap-mode 1)))
;;(add-hook 'after-change-major-mode-hook (lambda () (srb-adaptive-wrap-mode 1)))
(require 'zoom-frm)

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)


;; now handled by Prelude
;; (require 'flyspell)
;; (setq default-ispell-program-name "aspell")

(require 'back-button)
(back-button-mode 1)

;; (autoload 'visual-mark-ring-mode "visual-mark-ring-mode"
;;   "Displays the position of marks in the mark-ring" t)


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
                                         (kill-buffer buff)
                                         )
                                     ))))))

;; term-mode 
(add-hook 'term-exec-hook 'kill-buffer-on-exit-shell)



;; shell mode
(add-hook 'shell-mode-hook 'kill-buffer-on-exit-shell)



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



;; sweet auto-complete!
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/my-emacs/ac-dict")
(ac-config-default)
(global-auto-complete-mode t)

(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

(setq ac-auto-start nil)
(setq ac-auto-show-menu t)
;;(setq ac-delay 0.8)
(setq ac-expand-on-auto-complete t)
(setq ac-menu-height 15)


(global-set-key (kbd "TAB") 'ac-start)
(define-key ac-completing-map (kbd "C-f") 'ac-isearch)
(define-key ac-completing-map (kbd "RET") 'ac-complete)
(define-key ac-completing-map (kbd "TAB") 'auto-complete)

;; (defadvice ac-start (after jg-ac-tab-again-if-only-one-candidate activate)
;;   (message "%d" (length ac-candidates))
;;   (if ac-dwim-enable (ac-complete))
;;   )

;;(setq explicit-shell-file-name "/usr/local/bin/bash")
(setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
(setq comint-process-echoes t)
;; ASIDE: if you call ssh from shell directly, add "-t" to explicit-ssh-args to enable terminal.
(require 'readline-complete)

(add-to-list 'ac-modes 'ssh-mode)
(add-hook 'ssh-mode-hook 'ac-rlc-setup-sources)
(add-hook 'ssh-mode-hook 'jg-setup-ac-rlc)

(setq tramp-shell-prompt-pattern ".*[#$%>] *")

(add-to-list 'ac-modes 'shell-mode)
(add-hook 'shell-mode-hook 'ac-rlc-setup-sources)
(add-hook 'shell-mode-hook 'jg-setup-ac-rlc)

(defun jg-setup-ac-rlc ()
  ;; for some reason have to override these again here
  (setq ac-auto-start nil)
  (setq ac-auto-show-menu t)
)

;; Rsense + Autocomplete
(require 'rsense)
(add-hook 'ruby-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-rsense-method)
            (add-to-list 'ac-sources 'ac-source-rsense-constant)))



(require 'flex-isearch)
(global-flex-isearch-mode t)


;;(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)
;;(add-hook 'ruby-mode-hook  'turn-on-ctags-auto-update-mode)

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


(require 'mu4e)
(require 'tls)
(setq tls-program '("openssl s_client -debug -crlf -connect %h:%p -ssl2 -ign_eof"))


(setq
 mu4e-maildir "~/.maildir/openlogic"                      ; top-level Maildir
 mu4e-sent-folder   "/Sent Items"                         ; folder for sent messages
 mu4e-drafts-folder "/Drafts"                             ; unfinished messages
 mu4e-trash-folder  "/Deleted Messages"                   ; trashed messages 
 
 mu4e-get-mail-command "mbsync -a"
 mu4e-update-interval 300                                 ; update every 5 minutes

 
 mu4e-headers-date-format "%Y-%m-%d"                      ; date format
 mu4e-headers-time-format "%H:%M"                         ; time format
 message-kill-buffer-on-exit t
 mu4e-use-fancy-chars nil
 
 mu4e-view-show-images t
 mu4e-view-image-max-width 1000
 mu4e-html2text-command "html2text -utf8 -width 72 -nobs")

(setq mu4e-headers-fields
      '((:human-date    .  15)
        (:flags         .  10)
        (:from          .  25)
        (:subject       .  nil)))


(setq
 message-send-mail-function 'smtpmail-send-it
 send-mail-function 'smtpmail-send-it
 ;;smtpmail-local-domain "localhost"
 smtpmail-local-domain nil ;"openlogic.com"
 ;;smtpmail-smtp-server "pod51018.outlook.com"
 smtpmail-smtp-server "localhost"
 ;;smtpmail-default-smtp-server "pod51018.outlook.com"
 smtpmail-default-smtp-server "localhost"
 ;;smtpmail-smtp-service 587
 smtpmail-smtp-service 1025
 ;;smtpmail-stream-type 'tls
 smtpmail-stream-type nil
 smtpmail-queue-dir "~/.smtp-mail-queue")


;;(setq smtpmail-debug-info nil) ; only to debug problems

(require 'smtpmail)

(setq mu4e-reply-to-address "jeff.gran@openlogic.com"
      user-mail-address "jeff.gran@openlogic.com"
      user-full-name  "Jeff Gran")

(setq mu4e-attachment-dir "~/Downloads")

(defconst mu4e~main-buffer-name "Mu4e-main"
  "*internal* Name of the mu4e main view buffer.")

(add-to-list 'mu4e-bookmarks
             '("maildir:/INBOX"       "Inbox"     ?i))
(add-to-list 'mu4e-bookmarks
             '("flag:flagged"         "Flagged"   ?f))

(setq mu4e-headers-results-limit 500)

;; (setq mu4e-confirm-quit nil
;;       
;;       mu4e-html2text-command "html2text -utf8 -width 72")


;; (define-key mu4e-main-mode-map (kbd "q") 'mu4e-quit-session)
;; (define-key mu4e-headers-mode-map (kbd "M-u") 'mu4e-update-mail-show-window)




;;; org mode
(setq org-todo-keywords
       '((sequence "TODO" "WORKING" "DONE")))

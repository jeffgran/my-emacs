;; undo stuff prelude is trying to force on me
(setq before-save-hook nil) ;; cleaning up whitespace for me.
(setq prelude-lisp-coding-hook 'rainbow-delimiters-mode) ;; setting annoying paredit mode
(setq prelude-interactive-lisp-coding-hook 'rainbow-delimiters-mode) ;; setting annoying paredit mode
(add-hook 'prog-mode-hook 'whitespace-turn-off t)
(whitespace-mode -1)

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

(setq helm-idle-delay 0.03)
(setq helm-input-idle-delay 0.03)
(setq helm-quick-update t)




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
(setq
 nxhtml-global-minor-mode nil
 mumamo-chunk-coloring 'submode-colored
 nxhtml-skip-welcome t
 indent-region-mode t
 rng-nxml-auto-validate-flag nil
 ;;mumamo-margin-use (quote (right-margin 13))
 nxml-degraded t)

(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . eruby-nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.dryml$" . eruby-nxhtml-mumamo-mode))



(require 'erc)
(setq erc-autojoin-channels-alist '(("192.168.10.10" "#eng" "#daveco")))


;; (require 'rails)
;; (setq ruby-insert-encoding-magic-comment nil) ;; turn off the # -*- coding: utf-8 -*- comments


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


;; doesn't seem to work to override existing css-mode?
                                        ;(add-to-list 'load-path "~/.emacs.d/emacs-css-mode")
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
;; (add-hook 'css-mode-hook
;;           (lambda()
;;             (local-set-key (kbd "RET") 'open-line-below)))


(require 'rvm)
(rvm-use-default)
;;(rvm-autodetect-ruby) ;; using my project switcher now instead of this.

(require 'rspec-mode)
(require 'shoulda-mode)
(setq shoulda-use-rvm t)
(setq rspec-use-rvm t)

(add-hook 'ruby-mode-hook
          (lambda ()
            (make-local-variable 'paragraph-start)
            (setq paragraph-start (concat "@[[:alpha:]]+\\|" paragraph-start))
            (make-local-variable 'paragraph-separate)
            (setq paragraph-separate (concat "---+\\|" paragraph-separate))
            ;; turn electric pair mode off; ruby has its own electricity
            (electric-pair-mode -1)
            )
          )

(require 'rspec-mode)
(require 'ansi-color)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; use c++ for torquescript
(add-to-list 'auto-mode-alist '("\\.cs$" . c++-mode))
(setq c-basic-offset 4)


(require 'srb-adaptive-wrap-mode)
;;; srb-adaptive-wrap hooks
(add-hook 'ruby-mode-hook (lambda () (srb-adaptive-wrap-mode 1)))
(add-hook 'nxhtml-mode-hook (lambda () (srb-adaptive-wrap-mode 1)))
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



(require 'fuzzy-find-in-project)


(setq-default save-interprogram-paste-before-kill t)
(setq-default indent-tabs-mode nil)

(require 'redo)
(require 'grep-buffers)

(require 'linum)
(global-linum-mode 1)


(require 'wcy-swbuff)


;; term-mode stuff

;; kill the buffer upon completion of the process.
;; kill emacs instead?
(add-hook 'term-exec-hook (lambda ()
                            (let* ((buff (current-buffer))
                                   (proc (get-buffer-process buff)))
                              (lexical-let ((buff buff))
                                (set-process-sentinel proc (lambda (process event)
                                                             (if (string= event "finished\n")
                                                                 (progn
                                                                   (kill-buffer buff)
                                                                   (delete-frame))
                                                               )))))))

;; shell mode
(add-hook 'shell-mode-hook (lambda ()
                             (linum-mode) ;; toggle it off i guess? using ARG=nil doesn't work
                             ))




(require 'ansi-color)

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

;; sweet auto-complete?
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/my-emacs//ac-dict")
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


(setq explicit-shell-file-name "/usr/local/bin/bash")
(setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
(setq comint-process-echoes t)
;; ASIDE: if you call ssh from shell directly, add "-t" to explicit-ssh-args to enable terminal.
(require 'readline-complete)
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
;;(global-set-key (kbd "C-8")  'ctags-search)

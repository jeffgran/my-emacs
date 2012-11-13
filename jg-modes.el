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
(require 'helm-c-yasnippet)
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




;; icicles... don't think I like it
;;(require 'icicles)


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


(require 'rails)
(setq ruby-insert-encoding-magic-comment nil) ;; turn off the # -*- coding: utf-8 -*- comments


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

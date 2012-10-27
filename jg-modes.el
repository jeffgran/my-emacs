; Magit # I don't really like it. command line is faster and more clear.
;(add-to-list 'exec-path "/usr/local/git/bin/")
;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
;(require 'magit)
;(global-set-key (kbd "M-g") 'magit-status)


;; Elscreen (tabs/session management)
(load "elscreen" "ElScreen" t)


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

;; Ruby mode
(setq enh-ruby-program "/Users/jgran/.rvm/rubies/ruby-1.9.2-p0/bin/ruby") ; so that still works if ruby points to ruby1.8
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
;(require 'ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rxml$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))


(require 'rails)

;; RNC mode
(require 'rnc-mode)
(add-to-list 'auto-mode-alist '("\\.rnc$" . rnc-mode))

;; php mode
(require 'php-mode)

;; js mode
(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))


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
(add-hook 'css-mode-hook
          (lambda()
            (local-set-key (kbd "RET") 'open-line-below)))


(require 'rvm)
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
          )
)

(require 'rspec-mode)
(require 'ansi-color)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; failed attempt to fix test output into a *compile* buffer
;; (add-hook 'rails-test:compilation-mode-hook (lambda ()
;;   (ansi-color-for-comint-mode-on)
;;   (setq ansi-color-context nil)
;;   (add-hook 'after-change-functions 'colorize-compilation-buffer)
;;   )
;; )
;; (add-hook 'rails-script:output-mode-hook (lambda ()
;;   (ansi-color-for-comint-mode-on)
;;   (setq ansi-color-context nil)
;;   (add-hook 'after-change-functions 'colorize-compilation-buffer)
;;   )
;; )


;; use c++ for torquescript
(add-to-list 'auto-mode-alist '("\\.cs$" . c++-mode))
(setq c-basic-offset 4)


(require 'srb-adaptive-wrap-mode)
;;; srb-adaptive-wrap hooks
(add-hook 'ruby-mode-hook (lambda () (srb-adaptive-wrap-mode 1)))
(add-hook 'nxhtml-mode-hook (lambda () 
			      (srb-adaptive-wrap-mode 1)))
(require 'zoom-frm)

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)


(require 'flyspell)
(setq default-ispell-program-name "aspell")

(require 'breadcrumb)

(require 'auto-mark)
(global-auto-mark-mode 1)


(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(add-hook 'ido-setup-hook 'ido-jg-keys)
(defun ido-jg-keys ()
 "Add my keybindings for ido."
 (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
 (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
)

(require 'fuzzy-find-in-project)

(setq-default save-interprogram-paste-before-kill t)
(setq-default indent-tabs-mode nil)

(require 'redo)
(require 'grep-buffers)

(require 'linum)
(global-linum-mode 1)

(add-hook 'nxhtml-mode-hook (lambda () (srb-adaptive-wrap-mode 1)))
(add-hook 'after-change-major-mode-hook (lambda () (srb-adaptive-wrap-mode 1)))

(require 'wcy-swbuff)

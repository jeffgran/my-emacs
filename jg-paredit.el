(require 'paredit)
(defvar jg-paredit-mode-map (make-sparse-keymap) "jg-paredit-mode-map.")
(define-minor-mode jg-paredit-mode "JG Paredit Keys" nil " [JGp]" 'jg-paredit-mode-map)

;; (add-hook 'emacs-lisp-mode '(lambda ()
;;                               (enable-paredit-mode)
;;                               (jg-paredit-mode 1)
;;                               ))

(add-hook 'paredit-mode-hook '(lambda ()
                                (jg-paredit-mode t)
                                (define-key jg-code-mode-map (kbd "C-k") 'paredit-kill)
                                ))


;;(jg-paredit-mode 0)

(define-key jg-paredit-mode-map (kbd "RET") 'paredit-newline)
(define-key jg-paredit-mode-map (kbd "DEL") 'jg-delete-region-or-paredit-delete)
(define-key jg-paredit-mode-map (kbd "C-<backspace>") 'paredit-backward-kill-word)


;; movement by list element
(define-key jg-paredit-mode-map (kbd "M-'") 'paredit-forward)
(define-key jg-paredit-mode-map (kbd "M-h") 'paredit-backward)

;; improve this to generate a region around the biggest sexp that's
;; not the whole file and indent region?
(define-key jg-paredit-mode-map (kbd "M-\\") 'paredit-reindent-defun)


(define-key paredit-mode-map (kbd "M-q") nil)

;; idea: M-S-h, M-S-' to "drag" sexps with cursor. use (transpose-sexps)



;; dunno what these are really
;; (define-key jg-paredit-mode-map (kbd "") 'paredit-forward-up)
;; (define-key jg-paredit-mode-map (kbd "") 'paredit-backward-up)
;; (define-key jg-paredit-mode-map (kbd "") 'paredit-forward-down)
;; (define-key jg-paredit-mode-map (kbd "") 'paredit-backward-down)

;; some defaults:

;; -- slurp and barf
;; (define-key jg-paredit-mode-map (kbd "C-)") 'paredit-forward-slurp-sexp)
;; (define-key jg-paredit-mode-map (kbd "") 'paredit-forward-slurp-into-list)
;; (define-key jg-paredit-mode-map (kbd "C-}") 'paredit-forward-barf-sexp)

;; (define-key jg-paredit-mode-map (kbd "C-(") 'paredit-backward-slurp-sexp)
;; (define-key jg-paredit-mode-map (kbd "") 'paredit-backward-slurp-into-list)
;; (define-key jg-paredit-mode-map (kbd "C-{") 'paredit-backward-barf-sexp)


;; -- wrap and unwrap in parens
(define-key jg-paredit-mode-map (kbd "M-)") 'paredit-splice-sexp)
(define-key jg-paredit-mode-map (kbd "M-(") 'paredit-wrap-round)


;; functions

(defun jg-delete-region-or-paredit-delete ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'delete-region)
    (paredit-backward-delete)
    )
  )

(provide 'jg-paredit)

;;; jg-agent-comments.el --- Inline comments for agent-shell buffers -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience

;;; Commentary:

;; Inline comments for agent-shell buffers.

;;; Code:

(defgroup jg-agent-comments nil
  "Inline comments for agent-shell buffers."
  :group 'convenience)

(defface jg-agent-comments-marker
  '((t :inherit warning :weight bold))
  "Face for inline comment markers.")

(defface jg-agent-comments-inline
  '((t :inherit 'font-lock-comment-face))
  "Face for inline comment text.")

(defface jg-agent-comments-snippet
  '((t :inherit 'shadow))
  "Face for dimmed code snippets shown with a comment.")

(defvar-local jg-agent-comments--comments nil
  "Buffer-local list of comment record plists.")

(defvar-keymap jg-agent-comments-mode-map)

(define-minor-mode jg-agent-comments-mode
  "Inline comments for agent-shell buffers."
  :init-value nil
  :lighter nil)

(declare-function agent-shell-prompt-queue "agent-shell" (prompt))
(require 'agent-shell nil t)

(provide 'jg-agent-comments)
;;; jg-agent-comments.el ends here

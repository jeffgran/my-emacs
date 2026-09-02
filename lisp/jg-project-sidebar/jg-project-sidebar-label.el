;;; jg-project-sidebar-label.el --- Async session labelling for jg-project-sidebar  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: jgran
;; Package-Requires: ((emacs "29.1") (agent-shell "0.66.1"))

;; Ported from the local agent-shell-hq fork (lisp/agent-shell-hq-perspective/,
;; originally from https://github.com/sreenivasvrao/agent-shell-hq) and renamed
;; for the jg-project-sidebar package.  Logic is unchanged.

;;; Commentary:

;; Async session labelling: shells out to a configurable CLI
;; (`jg-project-sidebar-label-command', default a haiku-model claude -p)
;; and uses the reply to rename the agent-shell session buffer.

;;; Code:

(require 'agent-shell)

;;;; Customization

(defgroup jg-project-sidebar-label nil
  "Auto-titling for jg-project-sidebar sessions."
  :group 'jg-project-sidebar
  :prefix "jg-project-sidebar-label-")

(defcustom jg-project-sidebar-label-command '("claude" "-p" "--model" "haiku")
  "Command and arguments for the titling subprocess.
The prompt text is appended as the final argument.

Examples:
  \\='(\"claude\" \"-p\" \"--model\" \"haiku\")
  \\='(\"llm\")
  \\='(\"ollama\" \"run\" \"llama3.2\")"
  :type '(repeat string))

(defcustom jg-project-sidebar-label-context-chars 2000
  "Characters of buffer content sent as context to the titler.
Taken from the end of the buffer, since the start is always the
static welcome banner (which alone can exceed this many characters)."
  :type 'integer)

(defcustom jg-project-sidebar-label-prompt
  "Reply with ONLY a terse 8-10 word title for this conversation, \
lowercase, no punctuation:\n\n%s"
  "Format string for the titling prompt.
%s is replaced with the buffer context."
  :type 'string)

;;;; Public API

;;;###autoload
(defun jg-project-sidebar-label (&optional shell-buf)
  "Auto-title SHELL-BUF via `jg-project-sidebar-label-command'.
When called interactively, resolves the shell buffer from any
agent-shell context."
  (interactive)
  (let* ((buf (or shell-buf
                  (agent-shell--shell-buffer :no-create t :no-error t)
                  (user-error "No agent-shell buffer found")))
         (context (with-current-buffer buf
                    (buffer-substring-no-properties
                     (max (point-min)
                          (- (point-max) jg-project-sidebar-label-context-chars))
                     (point-max))))
         (prompt (format jg-project-sidebar-label-prompt context))
         (output ""))
    (message "jg-project-sidebar-label: labeling %s…" (buffer-name buf))
    (let ((proc (make-process
                 :name       "jg-project-sidebar-label"
                 :buffer     nil
                 :command    (append jg-project-sidebar-label-command (list prompt))
                 :connection-type 'pipe
                 :filter   (lambda (_proc chunk)
                             (setq output (concat output chunk)))
                 :sentinel (lambda (_proc event)
                             (when (string-prefix-p "finished" event)
                               (let ((title (string-trim output)))
                                 (when (and (buffer-live-p buf) (> (length title) 0))
                                   ;; `rename-buffer' alone leaves the buffer detached from
                                   ;; its process: shell-maker resolves buffer/process via
                                   ;; `shell-maker-buffer-name', which only respects the new
                                   ;; name if `shell-maker--buffer-name-override' is updated.
                                   (let ((old-viewport
                                          (get-buffer (concat (buffer-name buf) " [viewport]"))))
                                     (shell-maker-set-buffer-name buf title)
                                     (when (buffer-live-p old-viewport)
                                       (with-current-buffer old-viewport
                                         (rename-buffer (concat title " [viewport]") t))))
                                   (message "jg-project-sidebar-label: labeled %s" title)
                                   ;; The sidebar's polling timer picks up renames via its
                                   ;; structural signature, but refresh immediately when the
                                   ;; sidebar is loaded and active.
                                    (when (fboundp 'jg-project-sidebar-refresh)
                                      (jg-project-sidebar-refresh)))))))))
      ;; Close stdin immediately so the subprocess doesn't wait for piped input.
      (process-send-eof proc))))

(provide 'jg-project-sidebar-label)
;;; jg-project-sidebar-label.el ends here

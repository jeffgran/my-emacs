;;*************************************************
;;               JG Navigation Mode
;;*************************************************
(defvar-keymap jg-navigation-mode-map
  "C-l" 'forward-char
  "C-j" 'backward-char
  "C-;" '(lambda () (interactive) (forward-word-strictly))
  "C-\"" nil
  "C-h" '(lambda () (interactive) (backward-word-strictly))

  ;; real toggle mark command. how does this not exist? but it doesn't
  "C-f" #'(lambda ()
            (interactive)
            (if (region-active-p)
                (deactivate-mark)
              (call-interactively 'set-mark-command)))
  ;; "C-t" 'exchange-point-and-mark

  "C-t" 'forward-to-char
  "C-b" 'backward-to-char

  ;; scrolling and line movement
  "C-M-n" 'View-scroll-half-page-forward
  "C-M-p" 'View-scroll-half-page-backward
  "C-M-." 'end-of-buffer
  "C-M-," 'beginning-of-buffer
  "M-l" 'goto-line
  ;; skipping to matching brackets
  "C-M-h" 'backward-list
  "C-M-;" 'forward-list

  "M-i" 'indent-for-tab-command

  ;; "windows" (in emacs parlance)
  "M-0" 'other-window
  "M-1" 'delete-other-windows
  "M-2" 'split-window-below
  "M-3" 'split-window-right

  ;; project
  "C-x p" 'helm-projectile-switch-project

  ;;screens (tabs)
  "M-w" #'(lambda () (interactive) (persp-kill (persp-current-name)))
  "M-{" 'persp-prev
  "M-}" 'persp-next

  ;;buffers
  "C-w" #'(lambda () (interactive) (kill-buffer (current-buffer)))
  "C-q" 'keyboard-quit
  "M-g" nil

  ;; select by semantic units. super helpful!
  "M-SPC" 'er/expand-region
  "C-M-SPC" 'er/contract-region

  ;; my custom selection stuff
  "M-e" 'select-whole-line-or-lines
  "M-a" 'select-whole-line-or-lines-backwards
  "C-a" 'back-to-indentation-or-beginning
  "<home>" 'back-to-indentation-or-beginning
  "<end>" 'move-end-of-line

  ;; "C-=" 'cua-set-mark

  "<help>" 'set-mark-command
  "<escape>" 'keyboard-quit

  ;; "C-x p" 'ido-jg-set-project-root

  "C-x k" 'rake

  "C-x C-b" 'ibuffer

  "C-v" 'helm-buffers-list
  "C-<tab>" 'helm-buffers-list

  ;; "M-b" 'electric-buffer-list
  ;; "C-o" 'ido-find-file
  "C-o" 'helm-find-files
  "C-S-o" 'helm-projectile-find-file
  "C-x C-r" 'recentf-open
  "H-p x r" 'projectile-recentf
  "C-M-o" 'find-file-at-point-with-line
  "M-o" 'other-window
  "C-/" #'(lambda ()
            (interactive)
            (dired default-directory))

  "M-t" '(lambda () (interactive) (treemacs-with-toggle (treemacs-add-and-display-current-project-exclusively)))

  ;; forward/back buttons like in a browser. go to the last place I was.
  "C-<" 'back-button-global-backward
  "C->" 'back-button-global-forward
  "C-," 'back-button-local-backward
  "C-." 'back-button-local-forward
  ;; same, but only within the current file

  ;; lsp mode
  "H-M-." 'lsp-find-definition

  ;; isearch
  "M-f" 'isearch-forward
  "M-r" 'isearch-backward
  ;; "H-f" 'flex-isearch-forward
  ;; "H-r" 'flex-isearch-backward

  ;; i guess exiting completely is navigation
  "M-q" 'save-buffers-kill-terminal

  ;; amx is m-x but with auto-completion
  "H-x" 'helm-M-x
  "s-x" 'helm-M-x ; for when hyper is broken
  "C-c M-x" 'execute-extended-command

  "M-j" 'jg-dispatch

  "C-s s" 'jg-open-ssh

  "M-x" 'kill-region

  ;;***********************
  ;; Cut/Copy/Open/Save/Etc
  ;;***********************
  "M-c" 'kill-ring-save
  "M-v" 'clipboard-yank
  "M-V" 'yank-pop
  "C-M-v" 'paste-unshift

  "C-x C-s" 'switch-to-shell

  "H-SPC" #'(lambda () (interactive) (insert-char ?_ 1)))

(if window-system
    (progn
      ;; emacs is weird. have to have this to be able to bind C-i without also binding <tab>
      ;; doesn't work in terminal!
      (keyboard-translate ?\C-i ?\H-i)
      (define-key jg-navigation-mode-map (kbd "H-i") 'back-to-indentation)
      (define-key jg-navigation-mode-map (kbd "C-M-i") 'recenter-top-bottom)

      ;; same for C-m == RET
      (keyboard-translate ?\C-m ?\H-m)
      (define-key jg-navigation-mode-map (kbd "C-m") nil)
      (define-key jg-navigation-mode-map (kbd "H-m") 'set-mark-command)
      ))

(define-minor-mode jg-navigation-mode
  "JG Navigation Mode Keys"
  :init-value t
  :lighter " [JGn]"
  :keymap jg-navigation-mode-map)

(jg-navigation-mode 1)

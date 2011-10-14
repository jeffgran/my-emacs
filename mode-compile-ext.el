(defun baseline-compilation ()
  "rename the compilation buffer so that you can work through it fixing test"
  (interactive)
  (let ((bl-name (concat (buffer-name) "-baseline")))
    (if (get-buffer bl-name)
	(kill-buffer bl-name)
    (rename-buffer bl-name))))

    
(add-hook 'mode-compile-hook
          (lambda ()
            (local-set-key (kbd "C-c ") 'baseline-compilation)))


(ert-deftest jg-agent-comments-mode-enables ()
  (with-temp-buffer
    (jg-agent-comments-mode 1)
    (should jg-agent-comments-mode)))

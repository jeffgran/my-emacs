;;; helper.el --- Load package and tests for batch ERT -*- lexical-binding: t; -*-
(add-to-list 'load-path
             (expand-file-name ".." (file-name-directory load-file-name)))
(add-to-list 'load-path
             (expand-file-name "." (file-name-directory load-file-name)))
(require 'jg-agent-comments)
(require 'jg-agent-comments-review)
(require 'jg-agent-comments-test)
(require 'jg-agent-comments-review-test)

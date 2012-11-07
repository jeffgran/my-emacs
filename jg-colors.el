;; zenburn is default in prelude
(disable-theme 'zenburn)

(deftheme jg "Jeff Gran's custom color theme.")


(custom-theme-set-faces
 'jg
 '(default ((t (:inherit nil
                         :stipple nil
                         :background "#000000"
                         :foreground "#eeeeee"
                         :inverse-video nil
                         :box nil
                         :strike-through nil
                         :overline nil
                         :underline nil
                         :slant normal
                         :weight normal
                         :height 125
                         :width normal
                         :foundry
                         (if (eq system-type 'darwin)
                             "Apple"
                           "Bitstream"
                           )
                         :family
                         (if (eq system-type 'darwin)
                             "Menlo"
                           "Bitstream Vera Sans Mono"
                           )

                         ))))

 ;;'(background "blue")
 '(font-lock-comment-face ((t (:italic t :foreground "green4"))))
 '(font-lock-doc-string-face ((t (:foreground "green2"))))
 '(font-lock-function-name-face ((t (:foreground "turquoise2"))))
 '(font-lock-keyword-face ((t (:bold t :foreground "RoyalBlue3"))))
 '(font-lock-preprocessor-face ((t (:italic nil :foreground "green4"))))
 '(font-lock-reference-face ((t (:foreground "DodgerBlue"))))
 '(font-lock-string-face ((t (:foreground "orange"))))
 '(font-lock-type-face ((t (:bold t :foreground "chartreuse"))))
 '(font-lock-variable-name-face ((t (:foreground "gold1"))))
 '(highlight ((t (:background "DarkSeaGreen4"))))
 '(linum ((t (:background "#111111" :foreground "#666666"))))
 '(hl-line ((t (:inherit t :background "#111111"))))
 '(list-mode-item-selected ((t (:background "gold"))))
 '(makefile-space-face ((t (:background "wheat"))))
 '(mode-line ((t (:background "navy"))))
 '(region ((t (:underline t :background "#000050"))))
 '(paren-match ((t (:background "darkseagreen4"))))
 '(show-paren-match ((t (:foreground "white" :background "turquoise4"))))
 '(show-paren-mismatch ((((class color)) (:foreground "white" :background "red"))))

 ;;whitespace-mode
 '(whitespace-line ((t (:inherit t :background "#000000" )))) ; don't care about long lines
 '(whitespace-space ((t (:background "#000000" :foreground "#222222"))))
 '(whitespace-newline ((t (:foreground "#222222" ))))
 '(whitespace-empty ((t (:background "#222222"))))
 '(trailing-whitespace ((t (:background "#111111"))))
 '(whitespace-trailing ((t (:background "#222222" :foreground "#000000"))))

 ;;flyspell
 '(flyspell-incorrect ((t (:inherit t :background "#3f0000" :weight bold))))
 '(flyspell-duplicate ((t (:inherit t :background "#3f0000" :weight bold))))

;;erc (emacs irc)
 '(erc-prompt-face ((t (:foreground "DodgerBlue" :weight bold))))
 ;;'(erc-nick-default-face ((t (:inherit t :foreground "wheat3" :weight bold))))
 '(erc-nick-default-face nil)
 '(erc-my-nick-face ((t (:inherit t :foreground "gold1" :weight bold))))
 '(erc-input-face ((t (:inherit t :foreground "orange4" :weight bold)))) ;; my typing
 


 ;; elscreen tabs
 '(elscreen-tab-background-face ((((type x w32 mac) (class color)) (:background "Gray20"))))
 '(elscreen-tab-control-face ((((type x w32 mac) (class color)) (:background "grey20" :foreground "grey90"))))
 '(elscreen-tab-current-screen-face ((((class color)) (:background "grey40" :foreground "grey90"))))
 '(elscreen-tab-other-screen-face ((((type x w32 mac) (class color)) (:background "Gray20" :foreground "Gray90"))))

  '(mumamo-background-chunk-major (( t (:background "#002010"))))
  '(mumamo-background-chunk-submode1 (( t (:background "#000022"))))
  '(mumamo-background-chunk-submode2 (( t (:background "#001133"))))
  '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "#999"))))
  '(font-lock-constant-face ((((class color) (background dark)) (:foreground "VioletRed3"))))

; ruby-specific
 '(ruby-op-face ((t (:foreground "SkyBlue1"))))
 '(ruby-string-delimiter-face ((t (:foreground "orange"))))
)


;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'jg)

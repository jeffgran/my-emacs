;; Color themes
;(add-to-list 'load-path "~/.emacs.d/color-theme")
;(require 'color-theme)
;(color-theme-initialize)


(custom-set-faces
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
   
 '(background "blue")
 '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "#999"))))
 '(font-lock-comment-face ((t (:italic t :foreground "Green4"))))
 '(font-lock-constant-face ((((class color) (background dark)) (:foreground "VioletRed3"))))
 '(font-lock-doc-string-face ((t (:foreground "green2"))))
 '(font-lock-function-name-face ((t (:foreground "turquoise2"))))
 '(font-lock-keyword-face ((t (:bold t :foreground "RoyalBlue3"))))
 '(font-lock-preprocessor-face ((t (:italic nil :foreground "Green4"))))
 '(font-lock-reference-face ((t (:foreground "DodgerBlue"))))
 '(font-lock-string-face ((t (:foreground "orange"))))
 '(font-lock-type-face ((t (:bold t :foreground "Chartreuse"))))
 '(font-lock-variable-name-face ((t (:foreground "gold1"))))
 '(font-lock-warning-face ((((class color) (background dark)) (:foreground "yellow" :background "red"))))
 '(highlight ((t (:background "DarkSeaGreen4"))))
 '(linum ((t (:inherit (shadow default) :foreground "#666666"))))
 '(hl-line ((t (:background "#111111"))))
 '(list-mode-item-selected ((t (:background "gold"))))
 '(makefile-space-face ((t (:background "wheat"))))
 '(mode-line ((t (:background "Navy"))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) (:background "#0c0c24"))))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) (:background "#000000"))))
 '(region ((t (:underline t :background "#000050"))))
 '(paren-match ((t (:background "darkseagreen4"))))
 '(trailing-whitespace ((t (:background "#111111"))))
 '(show-paren-match ((t (:foreground "black" :background "wheat"))))
 '(show-paren-mismatch ((((class color)) (:foreground "white" :background "red"))))
 '(speedbar-button-face ((((class color) (background dark)) (:foreground "green4"))))
 '(speedbar-directory-face ((((class color) (background dark)) (:foreground "khaki"))))
 '(speedbar-file-face ((((class color) (background dark)) (:foreground "cyan"))))
 '(speedbar-tag-face ((((class color) (background dark)) (:foreground "Springgreen"))))
 '(widget-field ((((class grayscale color) (background light)) (:background "DarkBlue"))))

; ruby-specific
 '(ruby-op-face ((t (:foreground "SkyBlue1"))))
 '(ruby-string-delimiter-face ((t (:foreground "orange"))))
)
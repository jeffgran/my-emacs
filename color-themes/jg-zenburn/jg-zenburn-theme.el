;;; jg-zenburn-theme.el --- A low contrast color theme for Emacs.
;; Copyright (C) 2011 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar.batsov@gmail.com>
;; URL: http://github.com/bbatsov/zenburn-emacs
;; Version: 1.7

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A port of the popular Vim theme Zenburn for Emacs 24, built on top of
;; the new built-in theme support in Emacs 24. There exists one other version of the
;; theme by Daniel Brockman. My version was originally based on it,
;; but it was in such a disarray, that I decided to rewrite it from
;; scratch in a more maintainable manner (hopefully).
;;
;;; Installation:
;;
;;   Drop the theme in a folder that is on `custom-theme-load-path'
;; and enjoy!
;;
;; Don't forget that the theme requires Emacs 24.
;;
;;; Bugs
;;
;; None that I'm aware of.
;;
;;; Credits
;;
;; Jani Nurminen created the original theme for vim on such this port
;; is based.
;;
;;; Note from JG
;; I blatantly ripped this off from the zenburn theme, obviously.
;; It's a nicely set up system, so I could change the colors to higher-contrast ones,
;; and fiddle around with which colors are on which face. It would be cool
;; if there were a generator that would give you something like this,
;; with more default color variables, that would name everything with a custom prefix.
;; 
;;; Code
(deftheme jg-zenburn "The Jg-Zenburn color theme")

(let ((class '((class color) (min-colors 89)))
      ;; Jg-Zenburn palette
      ;; colors with +x are lighter, colors with -x are darker

      (jg-zenburn-fg "#f0f0f0")
      (jg-zenburn-fg-1 "#e0e0e0")

      (jg-zenburn-bg-1 "#103063")
      (jg-zenburn-bg-05 "#102a33")
      (jg-zenburn-bg "#000000")
      (jg-zenburn-bg+1 "#1f1f26")
      (jg-zenburn-bg+2 "#262630")
      (jg-zenburn-bg+3 "#2a2a39")

      (jg-zenburn-red+1 "#dd5288")
      (jg-zenburn-red "#cd3278")
      (jg-zenburn-red-1 "#bd3268")
      (jg-zenburn-red-2 "#ad1258")
      (jg-zenburn-red-3 "#8d0238")
      (jg-zenburn-red-4 "#6d0028")
      (jg-zenburn-red-5 "#3d0008")

      (jg-zenburn-orange "#FF7b39")
      (jg-zenburn-orange-1 "#e55517")
      (jg-zenburn-orange-2 "#b94300")
      (jg-zenburn-orange-3 "#972100")
      (jg-zenburn-orange-4 "#520900")

      (jg-zenburn-yellow "#FFEA77")
      (jg-zenburn-yellow-1 "#dfca67")
      (jg-zenburn-yellow-2 "#cfba47")

      (jg-zenburn-green-1 "#446C10")
      (jg-zenburn-green "#94BC2D")
      (jg-zenburn-green+1 "#a4cC3D")
      (jg-zenburn-green+2 "#b4dC4D")
      (jg-zenburn-green+3 "#c4eC5D")
      (jg-zenburn-green+4 "#d4fC6D")

      (jg-zenburn-cyan+3 "#ccffef")
      (jg-zenburn-cyan+2 "#aaffee")
      (jg-zenburn-cyan+1 "#8AeCfD")
      (jg-zenburn-cyan "#5ccCdf")
      (jg-zenburn-cyan-1 "#4AbCcf")
      (jg-zenburn-cyan-2 "#3AaCbf")
      (jg-zenburn-cyan-3 "#188a9a")
      (jg-zenburn-cyan-4 "#065869")

      (jg-zenburn-blue+1 "#6cb0f3")
      (jg-zenburn-blue "#4c90d3")
      (jg-zenburn-blue-1 "#3c80c3")
      (jg-zenburn-blue-2 "#2c70b3")
      (jg-zenburn-blue-3 "#1c60a3")
      (jg-zenburn-blue-4 "#0c5093")
      (jg-zenburn-blue-5 "#004083")
      (jg-zenburn-blue-6 "#002043")

      (jg-zenburn-purple "#bE76dB")
      (jg-zenburn-purple-1 "#8c54ba")
      (jg-zenburn-purple-2 "#6a3290")
      (jg-zenburn-purple-3 "#4a1670")

      (jg-zenburn-magenta "#FA4785"))


  (custom-theme-set-faces
   'jg-zenburn
   `(button ((t (:underline t :weight bold :foreground ,jg-zenburn-cyan-1))))
   `(link ((t (:foreground ,jg-zenburn-cyan :underline t :weight bold))))
   `(link-visited ((t (:foreground ,jg-zenburn-purple :underline t :weight normal))))

;;; basic coloring
   `(default ((t (:foreground ,jg-zenburn-fg :background ,jg-zenburn-bg 
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
                                )))))

   `(cursor ((t (:foreground ,jg-zenburn-fg :background "white"))))
   `(escape-glyph-face ((t (:foreground ,jg-zenburn-red))))
   `(fringe ((t (:foreground ,jg-zenburn-fg :background ,jg-zenburn-bg+1))))
   `(header-line ((t (:foreground ,jg-zenburn-yellow
                                  :background ,jg-zenburn-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,jg-zenburn-blue-3))))

   `(error ((t (:foreground ,jg-zenburn-red-2 :weight bold :underline t))))

;;; compilation
   `(compilation-column-face ((t (:foreground ,jg-zenburn-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,jg-zenburn-green))))
   `(compilation-error-face ((t (:foreground ,jg-zenburn-red-2 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,jg-zenburn-fg))))
   `(compilation-info-face ((t (:foreground ,jg-zenburn-blue))))
   `(compilation-info ((t (:foreground ,jg-zenburn-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,jg-zenburn-green))))
   `(compilation-line-face ((t (:foreground ,jg-zenburn-yellow))))
   `(compilation-line-number ((t (:foreground ,jg-zenburn-yellow))))
   `(compilation-message-face ((t (:foreground ,jg-zenburn-blue))))
   `(compilation-warning-face ((t (:foreground ,jg-zenburn-yellow-1 :weight bold :underline t))))

;;; grep
   `(grep-context-face ((t (:foreground ,jg-zenburn-fg))))
   `(grep-error-face ((t (:foreground ,jg-zenburn-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,jg-zenburn-blue))))
   `(grep-match-face ((t (:foreground ,jg-zenburn-orange :weight bold))))
   `(match ((t (:background ,jg-zenburn-bg-1 :foreground ,jg-zenburn-orange :weight bold))))

   ;; faces used by isearch
   `(isearch ((t (:foreground ,jg-zenburn-yellow :background ,jg-zenburn-bg-1))))
   `(isearch-fail ((t (:foreground ,jg-zenburn-fg :background ,jg-zenburn-red-4))))
   `(lazy-highlight ((t (:foreground ,jg-zenburn-yellow :background ,jg-zenburn-bg+2))))

   `(menu ((t (:foreground ,jg-zenburn-fg :background ,jg-zenburn-bg))))
   `(minibuffer-prompt ((t (:foreground ,jg-zenburn-blue-3 :weight bold))))
   `(mode-line
     ((,class (:foreground ,jg-zenburn-cyan
                           :background ,jg-zenburn-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,jg-zenburn-green+2 :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,jg-zenburn-green-1
                      :background ,jg-zenburn-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,jg-zenburn-bg-1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,jg-zenburn-bg-05))))
   `(trailing-whitespace ((t (:background ,jg-zenburn-bg+1))))
   `(vertical-border ((t (:foreground ,jg-zenburn-fg))))

;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,jg-zenburn-cyan))))
   `(font-lock-comment-face ((t (:foreground ,jg-zenburn-green-1))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,jg-zenburn-green-1))))
   `(font-lock-constant-face ((t (:foreground ,jg-zenburn-red))))
   `(font-lock-doc-face ((t (:foreground ,jg-zenburn-green+1))))
   `(font-lock-doc-string-face ((t (:foreground ,jg-zenburn-blue-2))))
   `(font-lock-function-name-face ((t (:foreground ,jg-zenburn-blue))))
   `(font-lock-keyword-face ((t (:foreground ,jg-zenburn-cyan-3 :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,jg-zenburn-fg))))
   `(font-lock-preprocessor-face ((t (:foreground ,jg-zenburn-blue+1))))
   `(font-lock-string-face ((t (:foreground ,jg-zenburn-orange-1))))
   `(font-lock-type-face ((t (:foreground ,jg-zenburn-green))))
   `(font-lock-variable-name-face ((t (:foreground ,jg-zenburn-yellow))))
   `(font-lock-warning-face ((t (:foreground ,jg-zenburn-yellow-1 :weight bold :underline t))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))

   ;; [JG] stuff zenburn didn't have
   `(elscreen-tab-background-face ((t (:background ,jg-zenburn-bg :box (:line-width 4 :color ,jg-zenburn-bg)))))
   `(elscreen-tab-control-face ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-cyan))))
   `(elscreen-tab-current-screen-face ((t (:background ,jg-zenburn-blue-5 :foreground ,jg-zenburn-cyan+1 :style released-button :underline nil))))
   `(elscreen-tab-other-screen-face ((t (:background ,jg-zenburn-blue-6 :foreground ,jg-zenburn-cyan-2 :style released-button  :underline nil))))

   ;; ruby stuff
   `(ruby-op-face ((t (:foreground ,jg-zenburn-cyan+1))))
   `(ruby-string-delimiter-face ((t (:foreground ,jg-zenburn-orange-2))))
   ;;`(ruby-heredoc-delimiter-face ((t (:foreground ,jg-zenburn-orange-1))))
   

   `(cua-rectangle ((t (:background ,jg-zenburn-red-3))))

   ;; dired and dired+

   ;; `(dired-header ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-yellow-2))))
   ;; `(dired-mark ((t (:background ,jg-zenburn-bg))))
   ;; `(dired-marked ((t (:background ,jg-zenburn-bg))))
   ;; `(dired-flagged ((t (:background ,jg-zenburn-bg))))
   ;; `(dired-warning ((t (:background ,jg-zenburn-bg))))
   ;; `(dired-perm-write ((t (:background ,jg-zenburn-bg))))
   ;; `(dired-directory ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-yellow))))
   ;; `(dired-symlink ((t (:background ,jg-zenburn-bg))))
   ;; `(dired-ignored ((t (:background ,jg-zenburn-bg))))

   `(diredp-compressed-file-suffix ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-bg+3))))
   `(diredp-date-time ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-blue-3))))
   `(diredp-dir-heading ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-yellow))))
   `(diredp-display-msg ((t (:background ,jg-zenburn-bg))))

   `(diredp-file-name ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-cyan+1))))
   `(diredp-file-suffix ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-blue))))

   `(diredp-get-file-or-dir-name ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-yellow))))
   `(diredp-ignored-file-name ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-bg+2))))
   `(diredp-symlink ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-purple))))

   `(diredp-number ((t (:background ,jg-zenburn-bg))))
   
   `(diredp-executable-tag ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-red-1))))

   `(diredp-deletion ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-red))))
   `(diredp-deletion-file-name ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-red))))
   `(diredp-flag-mark ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-blue+1))))
   `(diredp-flag-mark-line ((t (:background ,jg-zenburn-blue-5 :foreground ,jg-zenburn-cyan+1))))
   `(diredp-mode-line-flagged ((t (:background ,jg-zenburn-bg))))
   `(diredp-mode-line-marked ((t (:background ,jg-zenburn-bg))))
   
   `(diredp-dir-priv ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-yellow))))
   `(diredp-link-priv ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-purple))))

   `(diredp-no-priv ((t (:background ,jg-zenburn-bg))))
   `(diredp-other-priv ((t (:background ,jg-zenburn-bg))))
   `(diredp-rare-priv ((t (:background ,jg-zenburn-bg))))

   `(diredp-read-priv ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-blue))))
   `(diredp-write-priv ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-orange))))
   `(diredp-exec-priv ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-red+1))))
   


   ;; [/JG]

;;; newsticker
   `(newsticker-date-face ((t (:foreground ,jg-zenburn-fg))))
   `(newsticker-default-face ((t (:foreground ,jg-zenburn-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,jg-zenburn-green+3))))
   `(newsticker-extra-face ((t (:foreground ,jg-zenburn-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,jg-zenburn-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,jg-zenburn-green))))
   `(newsticker-new-item-face ((t (:foreground ,jg-zenburn-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,jg-zenburn-red))))
   `(newsticker-old-item-face ((t (:foreground ,jg-zenburn-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,jg-zenburn-fg))))
   `(newsticker-treeview-face ((t (:foreground ,jg-zenburn-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,jg-zenburn-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,jg-zenburn-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,jg-zenburn-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,jg-zenburn-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,jg-zenburn-bg+3))))
   `(newsticker-treeview-selection-face ((t (:foreground ,jg-zenburn-yellow))))

;;; external

   ;; full-ack
   `(ack-separator ((t (:foreground ,jg-zenburn-fg))))
   `(ack-file ((t (:foreground ,jg-zenburn-blue))))
   `(ack-line ((t (:foreground ,jg-zenburn-yellow))))
   `(ack-match ((t (:foreground ,jg-zenburn-orange :background ,jg-zenburn-bg-1 :weight bold))))

   ;; auctex
   `(font-latex-bold ((t (:inherit bold))))
   `(font-latex-warning ((t (:inherit font-lock-warning))))
   `(font-latex-sedate ((t (:foreground ,jg-zenburn-yellow :weight bold ))))
   `(font-latex-title-4 ((t (:inherit variable-pitch :weight bold))))

   ;; auto-complete
   `(ac-selection-face ((t (:background ,jg-zenburn-blue-5 :foreground ,jg-zenburn-cyan+1))))
   `(ac-candidate-face ((t (:background ,jg-zenburn-blue-6 :foreground ,jg-zenburn-cyan))))
   `(popup-tip-face ((t (:background ,jg-zenburn-yellow-2 :foreground "black"))))
   `(popup-scroll-bar-foreground-face ((t (:background ,jg-zenburn-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,jg-zenburn-bg-1))))
   `(popup-isearch-match ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-fg))))

   ;; company
   `(company-tooltip ((t (:background ,jg-zenburn-blue-6 :foreground ,jg-zenburn-cyan))))
   
   `(company-scrollbar-fg ((t (:background ,jg-zenburn-blue-5))))
   `(company-scrollbar-bg ((t (:background ,jg-zenburn-bg-1))))
   
   `(company-tooltip-selection ((t (:background ,jg-zenburn-blue-5 :foreground ,jg-zenburn-cyan+1))))
   `(company-tooltip-annotation ((t (:inherit company-tooltip :foreground ,jg-zenburn-yellow))))
   
   `(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :foreground ,jg-zenburn-yellow))))
   `(company-tooltip-search ((t (:inherit company-tooltip :foreground ,jg-zenburn-orange))))
   ;;com

   ;; diff
   `(diff-added ((,class (:foreground ,jg-zenburn-green+4))
                 (t (:foreground ,jg-zenburn-green-1))))
   `(diff-changed ((t (:foreground ,jg-zenburn-yellow))))
   `(diff-removed ((,class (:foreground ,jg-zenburn-red))
                   (t (:foreground ,jg-zenburn-red-3))))
   `(diff-header ((,class (:background ,jg-zenburn-bg+2))
                  (t (:background ,jg-zenburn-fg :foreground ,jg-zenburn-bg))))
   `(diff-file-header
     ((,class (:background ,jg-zenburn-bg+2 :foreground ,jg-zenburn-fg :bold t))
      (t (:background ,jg-zenburn-fg :foreground ,jg-zenburn-bg :bold t))))

   ;; ert
   `(ert-test-result-expected ((t (:foreground ,jg-zenburn-green+4 :background ,jg-zenburn-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,jg-zenburn-red :background ,jg-zenburn-bg))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,jg-zenburn-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,jg-zenburn-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment))))
   `(eshell-ls-directory ((t (:foreground ,jg-zenburn-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,jg-zenburn-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,jg-zenburn-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning))))
   `(eshell-ls-product ((t (:inherit font-lock-doc))))
   `(eshell-ls-special ((t (:foreground ,jg-zenburn-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,jg-zenburn-cyan :weight bold))))

   ;; shell reset
   `(comint-highlight-prompt ((t (:inherit nil))))
   ;; flymake
   `(flymake-errline ((t (:background ,jg-zenburn-red-5 :weight bold :underline t))))
   `(flymake-warnline ((t (:background ,jg-zenburn-orange-4 :weight bold :underline t))))

   ;; flyspell
   `(flyspell-duplicate ((t (:underline t))))
   `(flyspell-incorrect ((t (:underline t))))

   ;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,jg-zenburn-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning))))
   `(erc-default-face ((t (:foreground ,jg-zenburn-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,jg-zenburn-yellow))))
   `(erc-keyword-face ((t (:foreground ,jg-zenburn-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,jg-zenburn-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,jg-zenburn-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,jg-zenburn-green))))
   `(erc-pal-face ((t (:foreground ,jg-zenburn-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,jg-zenburn-orange :background ,jg-zenburn-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,jg-zenburn-green+1))))
   `(erc-underline-face ((t (:underline t))))


   ;; mu4e
   `(mu4e-unread-face ((t (:bold t :foreground ,jg-zenburn-green))))
   `(mu4e-flagged-face ((t (:bold t :foreground ,jg-zenburn-yellow))))

   
   ;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,jg-zenburn-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,jg-zenburn-blue))))
   `(gnus-summary-high-read ((t (:foreground ,jg-zenburn-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,jg-zenburn-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,jg-zenburn-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,jg-zenburn-blue))))
   `(gnus-summary-low-read ((t (:foreground ,jg-zenburn-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,jg-zenburn-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,jg-zenburn-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,jg-zenburn-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,jg-zenburn-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,jg-zenburn-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,jg-zenburn-fg))))
   `(gnus-summary-selected ((t (:foreground ,jg-zenburn-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,jg-zenburn-blue))))
   `(gnus-cite-10 ((t (:foreground ,jg-zenburn-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,jg-zenburn-yellow))))
   `(gnus-cite-2 ((t (:foreground ,jg-zenburn-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,jg-zenburn-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,jg-zenburn-green+2))))
   `(gnus-cite-5 ((t (:foreground ,jg-zenburn-green+1))))
   `(gnus-cite-6 ((t (:foreground ,jg-zenburn-green))))
   `(gnus-cite-7 ((t (:foreground ,jg-zenburn-red))))
   `(gnus-cite-8 ((t (:foreground ,jg-zenburn-red-1))))
   `(gnus-cite-9 ((t (:foreground ,jg-zenburn-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,jg-zenburn-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,jg-zenburn-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,jg-zenburn-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,jg-zenburn-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,jg-zenburn-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,jg-zenburn-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,jg-zenburn-bg+2))))
   `(gnus-signature ((t (:foreground ,jg-zenburn-yellow))))
   `(gnus-x ((t (:background ,jg-zenburn-fg :foreground ,jg-zenburn-bg))))

   ;; helm
   `(helm-header
     ((t (:foreground ,jg-zenburn-green
                      :background ,jg-zenburn-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,jg-zenburn-yellow
                      :background ,jg-zenburn-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,jg-zenburn-bg+3 :underline nil))))

   `(helm-selection-line ((t (:background ,jg-zenburn-bg+1))))
   `(helm-visible-mark ((t (:foreground ,jg-zenburn-bg :background ,jg-zenburn-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,jg-zenburn-green+4 :background ,jg-zenburn-bg-1))))

   ;; hl-line-mode
   `(hl-line-face ((,class (:background ,jg-zenburn-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,jg-zenburn-bg-05)) ; old emacsen
              (t :weight bold)))

   ;; hl-sexp
   `(hl-sexp-face ((,class (:background ,jg-zenburn-bg+1))
                   (t :weight bold)))

   ;; ido-mode
   `(ido-first-match ((t (:foreground ,jg-zenburn-green+3 :weight bold))))
   `(ido-only-match ((t (:foreground ,jg-zenburn-green+4 :weight bold))))
   `(ido-subdir ((t (:foreground ,jg-zenburn-cyan-2))))

   ;; js2-mode
   `(js2-warning-face ((t (:underline ,jg-zenburn-orange))))
   `(js2-error-face ((t (:foreground ,jg-zenburn-red :weight bold))))
   `(js2-jsdoc-tag-face ((t (:foreground ,jg-zenburn-green-1))))
   `(js2-jsdoc-type-face ((t (:foreground ,jg-zenburn-green+2))))
   `(js2-jsdoc-value-face ((t (:foreground ,jg-zenburn-green+3))))
   `(js2-function-param-face ((t (:foreground, jg-zenburn-green+3))))
   `(js2-external-variable-face ((t (:foreground ,jg-zenburn-orange))))

   ;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,jg-zenburn-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,jg-zenburn-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,jg-zenburn-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,jg-zenburn-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,jg-zenburn-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,jg-zenburn-red+1))))
   `(jabber-activity-face((t (:foreground ,jg-zenburn-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,jg-zenburn-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))

   ;; linum-mode
   `(linum ((t (:foreground ,jg-zenburn-bg+3 :background ,jg-zenburn-bg))))

   ;; magit
   `(magit-section-title ((t (:foreground ,jg-zenburn-yellow :weight bold))))
   `(magit-branch ((t (:foreground ,jg-zenburn-orange :weight bold))))
   `(magit-item-highlight ((t (:background ,jg-zenburn-bg+1))))

   ;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment))))
   `(message-header-name ((t (:foreground ,jg-zenburn-green+1))))
   `(message-header-other ((t (:foreground ,jg-zenburn-green))))
   `(message-header-to ((t (:foreground ,jg-zenburn-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,jg-zenburn-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,jg-zenburn-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,jg-zenburn-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,jg-zenburn-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,jg-zenburn-green))))
   `(message-mml ((t (:foreground ,jg-zenburn-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment))))

   ;; mew
   `(mew-face-header-subject ((t (:foreground ,jg-zenburn-orange))))
   `(mew-face-header-from ((t (:foreground ,jg-zenburn-yellow))))
   `(mew-face-header-date ((t (:foreground ,jg-zenburn-green))))
   `(mew-face-header-to ((t (:foreground ,jg-zenburn-red))))
   `(mew-face-header-key ((t (:foreground ,jg-zenburn-green))))
   `(mew-face-header-private ((t (:foreground ,jg-zenburn-green))))
   `(mew-face-header-important ((t (:foreground ,jg-zenburn-blue))))
   `(mew-face-header-marginal ((t (:foreground ,jg-zenburn-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,jg-zenburn-red))))
   `(mew-face-header-xmew ((t (:foreground ,jg-zenburn-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,jg-zenburn-red))))
   `(mew-face-body-url ((t (:foreground ,jg-zenburn-orange))))
   `(mew-face-body-comment ((t (:foreground ,jg-zenburn-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,jg-zenburn-green))))
   `(mew-face-body-cite2 ((t (:foreground ,jg-zenburn-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,jg-zenburn-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,jg-zenburn-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,jg-zenburn-red))))
   `(mew-face-mark-review ((t (:foreground ,jg-zenburn-blue))))
   `(mew-face-mark-escape ((t (:foreground ,jg-zenburn-green))))
   `(mew-face-mark-delete ((t (:foreground ,jg-zenburn-red))))
   `(mew-face-mark-unlink ((t (:foreground ,jg-zenburn-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,jg-zenburn-green))))
   `(mew-face-mark-unread ((t (:foreground ,jg-zenburn-red-2))))
   `(mew-face-eof-message ((t (:foreground ,jg-zenburn-green))))
   `(mew-face-eof-part ((t (:foreground ,jg-zenburn-yellow))))

   ;; mic-paren
   `(paren-face-match ((t (:foreground ,jg-zenburn-cyan :background ,jg-zenburn-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,jg-zenburn-bg :background ,jg-zenburn-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,jg-zenburn-bg :background ,jg-zenburn-red :weight bold))))

   ;; nav
   `(nav-face-heading ((t (:foreground ,jg-zenburn-yellow))))
   `(nav-face-button-num ((t (:foreground ,jg-zenburn-cyan))))
   `(nav-face-dir ((t (:foreground ,jg-zenburn-green))))
   `(nav-face-hdir ((t (:foreground ,jg-zenburn-red))))
   `(nav-face-file ((t (:foreground ,jg-zenburn-fg))))
   `(nav-face-hfile ((t (:foreground ,jg-zenburn-red-4))))

   ;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,jg-zenburn-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,jg-zenburn-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,jg-zenburn-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,jg-zenburn-bg+1))))

   ;; mmm (new and improved mumamo)
   `(mmm-default-submode-face ((t (:background ,jg-zenburn-red-5))))
   
   ;; org-mode
   `(org-agenda-date-today
     ((t (:foreground "white" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,jg-zenburn-fg :weight bold))))
   `(org-checkbox ((t (:background ,jg-zenburn-bg+2 :foreground "white"
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,jg-zenburn-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,jg-zenburn-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,jg-zenburn-green+3))))
   `(org-formula ((t (:foreground ,jg-zenburn-yellow-2))))
   `(org-headline-done ((t (:foreground ,jg-zenburn-green+3))))
   `(org-hide ((t (:foreground ,jg-zenburn-bg-1))))
   `(org-level-1 ((t (:foreground ,jg-zenburn-orange))))
   `(org-level-2 ((t (:foreground ,jg-zenburn-green+1))))
   `(org-level-3 ((t (:foreground ,jg-zenburn-blue-1))))
   `(org-level-4 ((t (:foreground ,jg-zenburn-yellow-2))))
   `(org-level-5 ((t (:foreground ,jg-zenburn-cyan))))
   `(org-level-6 ((t (:foreground ,jg-zenburn-green-1))))
   `(org-level-7 ((t (:foreground ,jg-zenburn-red-4))))
   `(org-level-8 ((t (:foreground ,jg-zenburn-blue-4))))
   `(org-link ((t (:foreground ,jg-zenburn-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,jg-zenburn-green+4))))
   `(org-scheduled-previously ((t (:foreground ,jg-zenburn-red-4))))
   `(org-scheduled-today ((t (:foreground ,jg-zenburn-blue+1))))
   `(org-special-keyword ((t (:foreground ,jg-zenburn-fg-1 :weight normal))))
   `(org-table ((t (:foreground ,jg-zenburn-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,jg-zenburn-orange))))
   `(org-todo ((t (:bold t :foreground ,jg-zenburn-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,jg-zenburn-red :weight bold :underline nil))))
   `(org-column ((t (:background ,jg-zenburn-bg-1))))
   `(org-column-title ((t (:background ,jg-zenburn-bg-1 :underline t :weight bold))))

   ;; outline
   `(outline-8 ((t (:inherit default))))
   `(outline-7 ((t (:inherit outline-8 :height 1.0))))
   `(outline-6 ((t (:inherit outline-7 :height 1.0))))
   `(outline-5 ((t (:inherit outline-6 :height 1.0))))
   `(outline-4 ((t (:inherit outline-5 :height 1.0))))
   `(outline-3 ((t (:inherit outline-4 :height 1.0))))
   `(outline-2 ((t (:inherit outline-3 :height 1.0))))
   `(outline-1 ((t (:inherit outline-2 :height 1.0))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,jg-zenburn-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,jg-zenburn-green+2))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,jg-zenburn-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,jg-zenburn-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,jg-zenburn-green-1))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,jg-zenburn-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,jg-zenburn-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,jg-zenburn-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,jg-zenburn-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,jg-zenburn-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,jg-zenburn-green))))
   `( rainbow-delimiters-depth-12-face ((t (:foreground ,jg-zenburn-blue-5))))

   ;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,jg-zenburn-green))))
   `(rpm-spec-doc-face ((t (:foreground ,jg-zenburn-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,jg-zenburn-red))))
   `(rpm-spec-macro-face ((t (:foreground ,jg-zenburn-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,jg-zenburn-red))))
   `(rpm-spec-package-face ((t (:foreground ,jg-zenburn-red))))
   `(rpm-spec-section-face ((t (:foreground ,jg-zenburn-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,jg-zenburn-blue))))
   `(rpm-spec-var-face ((t (:foreground ,jg-zenburn-red))))

   ;; rst-mode
   `(rst-level-1-face ((t (:foreground ,jg-zenburn-orange))))
   `(rst-level-2-face ((t (:foreground ,jg-zenburn-green+1))))
   `(rst-level-3-face ((t (:foreground ,jg-zenburn-blue-1))))
   `(rst-level-4-face ((t (:foreground ,jg-zenburn-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,jg-zenburn-cyan))))
   `(rst-level-6-face ((t (:foreground ,jg-zenburn-green-1))))

   ;; show-paren
   `(show-paren-mismatch ((t (:foreground ,jg-zenburn-red-3 :weight bold))))
   `(show-paren-match ((t (:underline t))))

   ;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))

   ;; SLIME
   `(slime-repl-inputed-output-face ((t (:foreground ,jg-zenburn-red))))
   
   ;; smartparens
   `(sp-show-pair-match-face ((t (:underline nil :box (:line-width -1 :color ,jg-zenburn-blue-1)))))

   ;; tabbar
   `(tabbar-button ((t (:foreground ,jg-zenburn-fg
                                    :background ,jg-zenburn-bg))))
   `(tabbar-selected ((t (:foreground ,jg-zenburn-fg
                                      :background ,jg-zenburn-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,jg-zenburn-fg
                                        :background ,jg-zenburn-bg+1
                                        :box (:line-width -1 :style released-button)))))

   ;; volatile-highlights
   `(vhl/default-face ((t (:background ,jg-zenburn-bg+1))))

   ;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,jg-zenburn-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,jg-zenburn-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,jg-zenburn-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,jg-zenburn-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,jg-zenburn-green+2 :background ,jg-zenburn-bg))))
   `(w3m-lnum-match ((t (:background ,jg-zenburn-bg-1
                                     :foreground ,jg-zenburn-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,jg-zenburn-yellow))))

   ;; whitespace-mode
   `(whitespace-space ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-bg+1))))
   `(whitespace-hspace ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-bg+1))))
   `(whitespace-tab ((t (:background ,jg-zenburn-bg :foreground ,jg-zenburn-red))))
   `(whitespace-newline ((t (:foreground ,jg-zenburn-bg+1))))
   `(whitespace-trailing ((t (:foreground ,jg-zenburn-red :background ,jg-zenburn-bg))))
   `(whitespace-line ((t (:background ,jg-zenburn-bg-05 :foreground ,jg-zenburn-magenta))))
   `(whitespace-space-before-tab ((t (:background ,jg-zenburn-orange :foreground ,jg-zenburn-orange))))
   `(whitespace-indentation ((t (:background ,jg-zenburn-yellow :foreground ,jg-zenburn-red))))
   `(whitespace-empty ((t (:background ,jg-zenburn-yellow :foreground ,jg-zenburn-red))))
   `(whitespace-space-after-tab ((t (:background ,jg-zenburn-yellow :foreground ,jg-zenburn-red))))

   ;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,jg-zenburn-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,jg-zenburn-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,jg-zenburn-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,jg-zenburn-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,jg-zenburn-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,jg-zenburn-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,jg-zenburn-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,jg-zenburn-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,jg-zenburn-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,jg-zenburn-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,jg-zenburn-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,jg-zenburn-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,jg-zenburn-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,jg-zenburn-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,jg-zenburn-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,jg-zenburn-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,jg-zenburn-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,jg-zenburn-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,jg-zenburn-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,jg-zenburn-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,jg-zenburn-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,jg-zenburn-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,jg-zenburn-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,jg-zenburn-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,jg-zenburn-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))

   
   ;; which-func-mode
   `(which-func ((t (:foreground ,jg-zenburn-green+4))))

   ;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,jg-zenburn-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,jg-zenburn-bg-1 :foreground ,jg-zenburn-bg-1))))
   

;;; custom theme variables
   (custom-theme-set-variables
    'jg-zenburn
    `(ansi-color-names-vector [,jg-zenburn-bg ,jg-zenburn-red ,jg-zenburn-green ,jg-zenburn-yellow
                                              ,jg-zenburn-blue-4 ,jg-zenburn-magenta ,jg-zenburn-cyan-2 ,jg-zenburn-fg])

    ;; fill-column-indicator
    `(fci-rule-color ,jg-zenburn-bg-05))

;;; colors for the ansi-term
   ;; (eval-after-load 'term
   ;;   `(setq ansi-term-color-vector
   ;;          (vector 'unspecified ,jg-zenburn-bg ,jg-zenburn-red ,jg-zenburn-green ,jg-zenburn-yellow
   ;;                  ,jg-zenburn-blue-4 ,jg-zenburn-magenta ,jg-zenburn-cyan-1 ,jg-zenburn-fg)))
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'jg-zenburn)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; jg-zenburn-theme.el ends here.

;;; material-candy-theme.el --- Material-Candy Theme

;; Copyright 2015-present, All rights reserved
;;
;; Code licensed under the MIT license

;; Author: shaneikennedy
;; Version: 1.7.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/shaneikennedy/emacs-material-material-candy

;;; Commentary:
;; A port of material material-candy by millsp.

;;; Code:
(require 'cl-lib)
(deftheme material-candy)


;;;; Configuration options:

(defgroup material-candy nil
  "Material-Candy theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom material-candy-enlarge-headings t
  "Use different font sizes for some headings and titles."
  :type 'boolean
  :group 'material-candy)

(defcustom material-candy-height-title-1 1.3
  "Font size 100%."
  :type 'number
  :group 'material-candy)

(defcustom material-candy-height-title-2 1.1
  "Font size 110%."
  :type 'number
  :group 'material-candy)

(defcustom material-candy-height-title-3 1.0
  "Font size 130%."
  :type 'number
  :group 'material-candy)

(defcustom material-candy-height-doc-title 1.44
  "Font size 144%."
  :type 'number
  :group 'material-candy)

(defcustom material-candy-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'boolean
  :group 'material-candy)

(defvar material-candy-use-24-bit-colors-on-256-colors-terms nil
  "Use true colors even on terminals announcing less capabilities.

Beware the use of this variable.  Using it may lead to unwanted
behavior, the most common one being an ugly blue background on
terminals, which don't understand 24 bit colors.  To avoid this
blue background, when using this variable, one can try to add the
following lines in their config file after having load the
Material-Candy theme:

    (unless (display-graphic-p)
      (set-face-background 'default \"black\" nil))")


;;;; Theme definition:

;; Assigment form: VARIABLE COLOR [256-COLOR [TTY-COLOR]]
(let ((colors '(;; Upstream theme color
                (material-candy-bg      "#161616" "unspecified-bg" "unspecified-bg") ; material-candy background
                (material-candy-fg      "#BBBBBB" "#BBBBBB" "brightwhite") ; material-candy foreground
                (material-candy-current "#44475a" "#44475a" "brightblack") ; material-candy current-line/selection
                (material-candy-comment "#6A9955" "#6A9955" "blue")        ; material-candy comment
                (material-candy-cyan    "#5fc1df" "#5fc1df" "brightcyan")  ; material-candy cyan
                (material-candy-green   "#cab760" "#cab760" "darkyellow")       ; material-candy dark yellow
                (material-candy-orange  "#ffb86c" "#ffb86c" "brightred")   ; material-candy orange
                (material-candy-pink    "#df5f74" "#df5f74" "magenta")     ; material-candy pink
                (material-candy-purple  "#4fb497" "#4fb497" "brightmagenta") ; material-candy purple
                (material-candy-red     "#ff5555" "#ff5555" "red")         ; material-candy red
                (material-candy-yellow  "#f1fa8c" "#f1fa8c" "yellow")      ; material-candy yellow
                (material-candy-active-modeline  "#6272a4" "#5f5faf" "modeline")      ; material-candy active mode-line

                ;; Other colors
                (bg2             "#373844" "#121212" "brightblack")
                (bg3             "#464752" "#262626" "brightblack")
                (bg4             "#565761" "#444444" "brightblack")
                (fg2             "#e2e2dc" "#e4e4e4" "brightwhite")
                (fg3             "#ccccc7" "#c6c6c6" "white")
                (fg4             "#b6b6b2" "#b2b2b2" "white")
                (other-blue      "#0189cc" "#0087ff" "brightblue")))
      (faces '(;; default
               (cursor :background , material-candy-pink)
               (completions-first-difference :foreground ,material-candy-pink :weight bold)
               (default :background ,material-candy-bg :foreground ,material-candy-fg)
               (default-italic :slant italic)
               (ffap :foreground ,fg4)
               (fringe :background ,material-candy-bg :foreground ,fg4)
               (highlight :foreground ,fg2 :background ,bg3)
               (hl-line :background ,material-candy-current :extend t)
               (info-quoted-name :foreground ,material-candy-orange)
               (info-string :foreground ,material-candy-yellow)
               (lazy-highlight :foreground ,fg2 :background ,bg2)
               (link :foreground ,material-candy-cyan :underline t)
               (linum :slant italic :foreground ,bg4 :background ,material-candy-bg)
               (line-number :slant italic :foreground ,bg4 :background ,material-candy-bg)
               (match :background ,material-candy-yellow :foreground ,material-candy-bg)
               (minibuffer-prompt
                ,@(if material-candy-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground material-candy-fg)
                    (list :weight 'bold :foreground material-candy-pink)))
               (read-multiple-choice-face :inherit completions-first-difference)
               (region :inherit match :extend t)
               (trailing-whitespace :foreground "unspecified-fg" :background ,material-candy-orange)
               (vertical-border :foreground ,bg2)
               (success :foreground ,material-candy-green)
               (warning :foreground ,material-candy-orange)
               (error :foreground ,material-candy-red)
               (header-line :background ,material-candy-bg)
               ;; syntax
               (font-lock-builtin-face :foreground ,material-candy-orange)
               (font-lock-comment-face :foreground ,material-candy-comment)
               (font-lock-comment-delimiter-face :foreground ,material-candy-comment)
               (font-lock-constant-face :foreground ,material-candy-cyan)
               (font-lock-doc-face :foreground ,material-candy-comment)
               (font-lock-function-name-face :foreground ,material-candy-green :weight bold)
               (font-lock-keyword-face :weight bold :foreground ,material-candy-pink)
               (font-lock-negation-char-face :foreground ,material-candy-cyan)
               (font-lock-preprocessor-face :foreground ,material-candy-orange)
               (font-lock-reference-face :foreground ,material-candy-cyan)
               (font-lock-regexp-grouping-backslash :foreground ,material-candy-cyan)
               (font-lock-regexp-grouping-construct :foreground ,material-candy-purple)
               (font-lock-string-face :foreground ,material-candy-yellow)
               (font-lock-type-face :foreground ,material-candy-purple)
               (font-lock-variable-name-face :foreground ,material-candy-pink
                                             :weight bold)
               (font-lock-warning-face :foreground ,material-candy-orange :background ,bg2)
               ;; auto-complete
               (ac-completion-face :underline t :foreground ,material-candy-pink)
               ;; company
               (company-echo-common :foreground ,material-candy-bg :background ,material-candy-fg)
               (company-preview :background ,material-candy-current :foreground ,other-blue)
               (company-preview-common :inherit company-preview
                                       :foreground ,material-candy-pink)
               (company-preview-search :inherit company-preview
                                       :foreground ,material-candy-green)
               (company-scrollbar-bg :background ,material-candy-comment)
               (company-scrollbar-fg :foreground ,other-blue)
               (company-tooltip :foreground ,material-candy-fg :background ,material-candy-current)
               (company-tooltip-search :foreground ,material-candy-green
                                       :underline t)
               (company-tooltip-search-selection :background ,material-candy-green
                                                 :foreground ,material-candy-bg)
               (company-tooltip-selection :inherit match)
               (company-tooltip-mouse :background ,material-candy-bg)
               (company-tooltip-common :foreground ,material-candy-pink :weight bold)
               (company-tooltip-annotation :foreground ,material-candy-cyan)
               ;; diff-hl
               (diff-hl-change :foreground ,material-candy-orange :background ,material-candy-orange)
               (diff-hl-delete :foreground ,material-candy-pink :background ,material-candy-pink)
               (diff-hl-insert :foreground ,material-candy-cyan :background ,material-candy-cyan)
               ;; dired
               (dired-directory :foreground ,material-candy-green :weight normal)
               (dired-flagged :foreground ,material-candy-pink)
               (dired-header :foreground ,fg3 :background ,material-candy-bg)
               (dired-ignored :inherit shadow)
               (dired-mark :foreground ,material-candy-fg :weight bold)
               (dired-marked :foreground ,material-candy-orange :weight bold)
               (dired-perm-write :foreground ,fg3 :underline t)
               (dired-symlink :foreground ,material-candy-yellow :weight normal :slant italic)
               (dired-warning :foreground ,material-candy-orange :underline t)
               (diredp-compressed-file-name :foreground ,fg3)
               (diredp-compressed-file-suffix :foreground ,fg4)
               (diredp-date-time :foreground ,material-candy-fg)
               (diredp-deletion-file-name :foreground ,material-candy-pink :background ,material-candy-current)
               (diredp-deletion :foreground ,material-candy-pink :weight bold)
               (diredp-dir-heading :foreground ,fg2 :background ,bg4)
               (diredp-dir-name :inherit dired-directory)
               (diredp-dir-priv :inherit dired-directory)
               (diredp-executable-tag :foreground ,material-candy-orange)
               (diredp-file-name :foreground ,material-candy-fg)
               (diredp-file-suffix :foreground ,fg4)
               (diredp-flag-mark-line :foreground ,fg2 :slant italic :background ,material-candy-current)
               (diredp-flag-mark :foreground ,fg2 :weight bold :background ,material-candy-current)
               (diredp-ignored-file-name :foreground ,material-candy-fg)
               (diredp-mode-line-flagged :foreground ,material-candy-orange)
               (diredp-mode-line-marked :foreground ,material-candy-orange)
               (diredp-no-priv :foreground ,material-candy-fg)
               (diredp-number :foreground ,material-candy-cyan)
               (diredp-other-priv :foreground ,material-candy-orange)
               (diredp-rare-priv :foreground ,material-candy-orange)
               (diredp-read-priv :foreground ,material-candy-purple)
               (diredp-write-priv :foreground ,material-candy-pink)
               (diredp-exec-priv :foreground ,material-candy-yellow)
               (diredp-symlink :foreground ,material-candy-orange)
               (diredp-link-priv :foreground ,material-candy-orange)
               (diredp-autofile-name :foreground ,material-candy-yellow)
               (diredp-tagged-autofile-name :foreground ,material-candy-yellow)
               ;; elfeed
               (elfeed-search-date-face :foreground ,material-candy-comment)
               (elfeed-search-title-face :foreground ,material-candy-fg)
               (elfeed-search-unread-title-face :foreground ,material-candy-pink :weight bold)
               (elfeed-search-feed-face :foreground ,material-candy-fg :weight bold)
               (elfeed-search-tag-face :foreground ,material-candy-green)
               (elfeed-search-last-update-face :weight bold)
               (elfeed-search-unread-count-face :foreground ,material-candy-pink)
               (elfeed-search-filter-face :foreground ,material-candy-green :weight bold)
               (elfeed-log-error-level-face :foreground ,material-candy-red)
               (elfeed-log-warn-level-face :foreground ,material-candy-orange)
               (elfeed-log-info-level-face :foreground ,material-candy-cyan)
               (elfeed-log-debug-level-face :foreground ,material-candy-comment)
               ;; enh-ruby
               (enh-ruby-heredoc-delimiter-face :foreground ,material-candy-yellow)
               (enh-ruby-op-face :foreground ,material-candy-pink)
               (enh-ruby-regexp-delimiter-face :foreground ,material-candy-yellow)
               (enh-ruby-string-delimiter-face :foreground ,material-candy-yellow)
               ;; flyspell
               (flyspell-duplicate :underline (:style wave :color ,material-candy-orange))
               (flyspell-incorrect :underline (:style wave :color ,material-candy-red))
               ;; font-latex
               (font-latex-bold-face :foreground ,material-candy-purple)
               (font-latex-italic-face :foreground ,material-candy-pink :slant italic)
               (font-latex-match-reference-keywords :foreground ,material-candy-cyan)
               (font-latex-match-variable-keywords :foreground ,material-candy-fg)
               (font-latex-string-face :foreground ,material-candy-green)
               ;; gnus-group
               (gnus-group-mail-1 :foreground ,material-candy-pink :weight bold)
               (gnus-group-mail-1-empty :inherit gnus-group-mail-1 :weight normal)
               (gnus-group-mail-2 :foreground ,material-candy-cyan :weight bold)
               (gnus-group-mail-2-empty :inherit gnus-group-mail-2 :weight normal)
               (gnus-group-mail-3 :foreground ,material-candy-comment :weight bold)
               (gnus-group-mail-3-empty :inherit gnus-group-mail-3 :weight normal)
               (gnus-group-mail-low :foreground ,material-candy-current :weight bold)
               (gnus-group-mail-low-empty :inherit gnus-group-mail-low :weight normal)
               (gnus-group-news-1 :foreground ,material-candy-pink :weight bold)
               (gnus-group-news-1-empty :inherit gnus-group-news-1 :weight normal)
               (gnus-group-news-2 :foreground ,material-candy-cyan :weight bold)
               (gnus-group-news-2-empty :inherit gnus-group-news-2 :weight normal)
               (gnus-group-news-3 :foreground ,material-candy-comment :weight bold)
               (gnus-group-news-3-empty :inherit gnus-group-news-3 :weight normal)
               (gnus-group-news-4 :inherit gnus-group-news-low)
               (gnus-group-news-4-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-5 :inherit gnus-group-news-low)
               (gnus-group-news-5-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-6 :inherit gnus-group-news-low)
               (gnus-group-news-6-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-low :foreground ,material-candy-current :weight bold)
               (gnus-group-news-low-empty :inherit gnus-group-news-low :weight normal)
               (gnus-header-content :foreground ,material-candy-pink)
               (gnus-header-from :foreground ,material-candy-fg)
               (gnus-header-name :foreground ,material-candy-purple)
               (gnus-header-subject :foreground ,material-candy-green :weight bold)
               (gnus-summary-markup-face :foreground ,material-candy-cyan)
               (gnus-summary-high-unread :foreground ,material-candy-pink :weight bold)
               (gnus-summary-high-read :inherit gnus-summary-high-unread :weight normal)
               (gnus-summary-high-ancient :inherit gnus-summary-high-read)
               (gnus-summary-high-ticked :inherit gnus-summary-high-read :underline t)
               (gnus-summary-normal-unread :foreground ,other-blue :weight bold)
               (gnus-summary-normal-read :foreground ,material-candy-comment :weight normal)
               (gnus-summary-normal-ancient :inherit gnus-summary-normal-read :weight light)
               (gnus-summary-normal-ticked :foreground ,material-candy-pink :weight bold)
               (gnus-summary-low-unread :foreground ,material-candy-comment :weight bold)
               (gnus-summary-low-read :inherit gnus-summary-low-unread :weight normal)
               (gnus-summary-low-ancient :inherit gnus-summary-low-read)
               (gnus-summary-low-ticked :inherit gnus-summary-low-read :underline t)
               (gnus-summary-selected :inverse-video t)
               ;; haskell-mode
               (haskell-operator-face :foreground ,material-candy-pink)
               (haskell-constructor-face :foreground ,material-candy-purple)
               ;; helm
               (helm-bookmark-w3m :foreground ,material-candy-purple)
               (helm-buffer-not-saved :foreground ,material-candy-purple :background ,material-candy-bg)
               (helm-buffer-process :foreground ,material-candy-orange :background ,material-candy-bg)
               (helm-buffer-saved-out :foreground ,material-candy-fg :background ,material-candy-bg)
               (helm-buffer-size :foreground ,material-candy-fg :background ,material-candy-bg)
               (helm-candidate-number :foreground ,material-candy-bg :background ,material-candy-fg)
               (helm-ff-directory :foreground ,material-candy-green :background ,material-candy-bg :weight bold)
               (helm-ff-dotted-directory :foreground ,material-candy-green :background ,material-candy-bg :weight normal)
               (helm-ff-executable :foreground ,other-blue :background ,material-candy-bg :weight normal)
               (helm-ff-file :foreground ,material-candy-fg :background ,material-candy-bg :weight normal)
               (helm-ff-invalid-symlink :foreground ,material-candy-pink :background ,material-candy-bg :weight bold)
               (helm-ff-prefix :foreground ,material-candy-bg :background ,material-candy-pink :weight normal)
               (helm-ff-symlink :foreground ,material-candy-pink :background ,material-candy-bg :weight bold)
               (helm-grep-cmd-line :foreground ,material-candy-fg :background ,material-candy-bg)
               (helm-grep-file :foreground ,material-candy-fg :background ,material-candy-bg)
               (helm-grep-finish :foreground ,fg2 :background ,material-candy-bg)
               (helm-grep-lineno :foreground ,material-candy-fg :background ,material-candy-bg)
               (helm-grep-match :foreground "unspecified-fg" :background "unspecified-bg" :inherit helm-match)
               (helm-grep-running :foreground ,material-candy-green :background ,material-candy-bg)
               (helm-header :foreground ,fg2 :background ,material-candy-bg :underline nil :box nil)
               (helm-moccur-buffer :foreground ,material-candy-green :background ,material-candy-bg)
               (helm-selection :background ,bg2 :underline nil)
               (helm-selection-line :background ,bg2)
               (helm-separator :foreground ,material-candy-purple :background ,material-candy-bg)
               (helm-source-go-package-godoc-description :foreground ,material-candy-yellow)
               (helm-source-header :foreground ,material-candy-pink :background ,material-candy-bg :underline nil :weight bold)
               (helm-time-zone-current :foreground ,material-candy-orange :background ,material-candy-bg)
               (helm-time-zone-home :foreground ,material-candy-purple :background ,material-candy-bg)
               (helm-visible-mark :foreground ,material-candy-bg :background ,bg3)
               ;; highlight-indentation minor mode
               (highlight-indentation-face :background ,bg2)
               ;; icicle
               (icicle-whitespace-highlight :background ,material-candy-fg)
               (icicle-special-candidate :foreground ,fg2)
               (icicle-extra-candidate :foreground ,fg2)
               (icicle-search-main-regexp-others :foreground ,material-candy-fg)
               (icicle-search-current-input :foreground ,material-candy-pink)
               (icicle-search-context-level-8 :foreground ,material-candy-orange)
               (icicle-search-context-level-7 :foreground ,material-candy-orange)
               (icicle-search-context-level-6 :foreground ,material-candy-orange)
               (icicle-search-context-level-5 :foreground ,material-candy-orange)
               (icicle-search-context-level-4 :foreground ,material-candy-orange)
               (icicle-search-context-level-3 :foreground ,material-candy-orange)
               (icicle-search-context-level-2 :foreground ,material-candy-orange)
               (icicle-search-context-level-1 :foreground ,material-candy-orange)
               (icicle-search-main-regexp-current :foreground ,material-candy-fg)
               (icicle-saved-candidate :foreground ,material-candy-fg)
               (icicle-proxy-candidate :foreground ,material-candy-fg)
               (icicle-mustmatch-completion :foreground ,material-candy-purple)
               (icicle-multi-command-completion :foreground ,fg2 :background ,bg2)
               (icicle-msg-emphasis :foreground ,material-candy-green)
               (icicle-mode-line-help :foreground ,fg4)
               (icicle-match-highlight-minibuffer :foreground ,material-candy-orange)
               (icicle-match-highlight-Completions :foreground ,material-candy-green)
               (icicle-key-complete-menu-local :foreground ,material-candy-fg)
               (icicle-key-complete-menu :foreground ,material-candy-fg)
               (icicle-input-completion-fail-lax :foreground ,material-candy-pink)
               (icicle-input-completion-fail :foreground ,material-candy-pink)
               (icicle-historical-candidate-other :foreground ,material-candy-fg)
               (icicle-historical-candidate :foreground ,material-candy-fg)
               (icicle-current-candidate-highlight :foreground ,material-candy-orange :background ,bg3)
               (icicle-Completions-instruction-2 :foreground ,fg4)
               (icicle-Completions-instruction-1 :foreground ,fg4)
               (icicle-completion :foreground ,material-candy-fg)
               (icicle-complete-input :foreground ,material-candy-orange)
               (icicle-common-match-highlight-Completions :foreground ,material-candy-purple)
               (icicle-candidate-part :foreground ,material-candy-fg)
               (icicle-annotation :foreground ,fg4)
               ;; icomplete
               (icompletep-determined :foreground ,material-candy-orange)
               ;; ido
               (ido-first-match
                ,@(if material-candy-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground material-candy-green)
                    (list :weight 'bold :foreground material-candy-pink)))
               (ido-only-match :foreground ,material-candy-orange)
               (ido-subdir :foreground ,material-candy-yellow)
               (ido-virtual :foreground ,material-candy-cyan)
               (ido-incomplete-regexp :inherit font-lock-warning-face)
               (ido-indicator :foreground ,material-candy-fg :background ,material-candy-pink)
               ;; isearch
               (isearch :inherit match :weight bold)
               (isearch-fail :foreground ,material-candy-bg :background ,material-candy-orange)
               ;; jde-java
               (jde-java-font-lock-constant-face :foreground ,material-candy-cyan)
               (jde-java-font-lock-modifier-face :foreground ,material-candy-pink)
               (jde-java-font-lock-number-face :foreground ,material-candy-fg)
               (jde-java-font-lock-package-face :foreground ,material-candy-fg)
               (jde-java-font-lock-private-face :foreground ,material-candy-pink)
               (jde-java-font-lock-public-face :foreground ,material-candy-pink)
               ;; js2-mode
               (js2-external-variable :foreground ,material-candy-purple)
               (js2-function-param :foreground ,material-candy-cyan)
               (js2-jsdoc-html-tag-delimiter :foreground ,material-candy-green)
               (js2-jsdoc-html-tag-name :foreground ,other-blue)
               (js2-jsdoc-value :foreground ,material-candy-yellow)
               (js2-private-function-call :foreground ,material-candy-cyan)
               (js2-private-member :foreground ,fg3)
               ;; js3-mode
               (js3-error-face :underline ,material-candy-orange)
               (js3-external-variable-face :foreground ,material-candy-fg)
               (js3-function-param-face :foreground ,material-candy-pink)
               (js3-instance-member-face :foreground ,material-candy-cyan)
               (js3-jsdoc-tag-face :foreground ,material-candy-pink)
               (js3-warning-face :underline ,material-candy-pink)
               ;; lsp
               (lsp-ui-peek-peek :background ,material-candy-bg)
               (lsp-ui-peek-list :background ,bg2)
               (lsp-ui-peek-filename :foreground ,material-candy-pink :weight bold)
               (lsp-ui-peek-line-number :foreground ,material-candy-fg)
               (lsp-ui-peek-highlight :inherit highlight :distant-foreground ,material-candy-bg)
               (lsp-ui-peek-header :background ,bg3 :foreground ,fg3, :weight bold)
               (lsp-ui-peek-footer :inherit lsp-ui-peek-header)
               (lsp-ui-peek-selection :inherit match)
               (lsp-ui-sideline-symbol :foreground ,fg4 :box (:line-width -1 :color ,fg4) :height 0.99)
               (lsp-ui-sideline-current-symbol :foreground ,material-candy-fg :weight ultra-bold
                                               :box (:line-width -1 :color material-candy-fg) :height 0.99)
               (lsp-ui-sideline-code-action :foreground ,material-candy-yellow)
               (lsp-ui-sideline-symbol-info :slant italic :height 0.99)
               (lsp-ui-doc-background :background ,material-candy-bg)
               (lsp-ui-doc-header :foreground ,material-candy-bg :background ,material-candy-cyan)
               ;; magit
               (magit-branch-local :foreground ,material-candy-cyan)
               (magit-branch-remote :foreground ,material-candy-green)
               (magit-tag :foreground ,material-candy-orange)
               (magit-section-heading :foreground ,material-candy-pink :weight bold)
               (magit-section-highlight :background ,bg3 :extend t)
               (magit-diff-context-highlight :background ,bg3
                                             :foreground ,fg3
                                             :extend t)
               (magit-diff-revision-summary :foreground ,material-candy-orange
                                            :background ,material-candy-bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :foreground ,material-candy-orange
                                                      :background ,bg3
                                                      :weight bold
                                                      :extend t)
               ;; the four following lines are just a patch of the
               ;; upstream color to add the extend keyword.
               (magit-diff-added :background "#335533"
                                 :foreground "#ddffdd"
                                 :extend t)
               (magit-diff-added-highlight :background "#336633"
                                           :foreground "#cceecc"
                                           :extend t)
               (magit-diff-removed :background "#553333"
                                   :foreground "#ffdddd"
                                   :extend t)
               (magit-diff-removed-highlight :background "#663333"
                                             :foreground "#eecccc"
                                             :extend t)
               (magit-diff-file-heading :foreground ,material-candy-fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight)
               (magit-diffstat-added :foreground ,material-candy-green)
               (magit-diffstat-removed :foreground ,material-candy-red)
               (magit-hash :foreground ,fg2)
               (magit-hunk-heading :background ,bg3)
               (magit-hunk-heading-highlight :background ,bg3)
               (magit-item-highlight :background ,bg3)
               (magit-log-author :foreground ,fg3)
               (magit-process-ng :foreground ,material-candy-orange :weight bold)
               (magit-process-ok :foreground ,material-candy-green :weight bold)
               ;; markdown
               (markdown-blockquote-face :foreground ,material-candy-orange)
               (markdown-code-face :foreground ,material-candy-orange)
               (markdown-footnote-face :foreground ,other-blue)
               (markdown-header-face :weight normal)
               (markdown-header-face-1
                :inherit bold :foreground ,material-candy-pink
                ,@(when material-candy-enlarge-headings
                    (list :height material-candy-height-title-1)))
               (markdown-header-face-2
                :inherit bold :foreground ,material-candy-purple
                ,@(when material-candy-enlarge-headings
                    (list :height material-candy-height-title-2)))
               (markdown-header-face-3
                :foreground ,material-candy-green
                ,@(when material-candy-enlarge-headings
                    (list :height material-candy-height-title-3)))
               (markdown-header-face-4 :foreground ,material-candy-yellow)
               (markdown-header-face-5 :foreground ,material-candy-cyan)
               (markdown-header-face-6 :foreground ,material-candy-orange)
               (markdown-header-face-7 :foreground ,other-blue)
               (markdown-header-face-8 :foreground ,material-candy-fg)
               (markdown-inline-code-face :foreground ,material-candy-yellow)
               (markdown-plain-url-face :inherit link)
               (markdown-pre-face :foreground ,material-candy-orange)
               (markdown-table-face :foreground ,material-candy-purple)
               ;; message
               (message-header-to :foreground ,material-candy-fg :weight bold)
               (message-header-cc :foreground ,material-candy-fg :bold bold)
               (message-header-subject :foreground ,material-candy-orange)
               (message-header-newsgroups :foreground ,material-candy-purple)
               (message-header-other :foreground ,material-candy-purple)
               (message-header-name :foreground ,material-candy-green)
               (message-header-xheader :foreground ,material-candy-cyan)
               (message-separator :foreground ,material-candy-cyan :slant italic)
               (message-cited-text :foreground ,material-candy-purple)
               (message-cited-text-1 :foreground ,material-candy-purple)
               (message-cited-text-2 :foreground ,material-candy-orange)
               (message-cited-text-3 :foreground ,material-candy-comment)
               (message-cited-text-4 :foreground ,fg2)
               (message-mml :foreground ,material-candy-green :weight normal)
               ;; mode-line
               (mode-line :background ,material-candy-active-modeline
                          :box ,material-candy-current :inverse-video nil
                          ,@(if material-candy-alternate-mode-line-and-minibuffer
                                (list :foreground fg3)
                              (list :foreground "unspecified-fg")))
               (mode-line-inactive
                :inverse-video nil
                ,@(if material-candy-alternate-mode-line-and-minibuffer
                      (list :foreground material-candy-comment :background material-candy-bg
                            :box material-candy-bg)
                    (list :foreground material-candy-fg :background bg2 :box bg2)))
               ;; mu4e
               (mu4e-unread-face :foreground ,material-candy-pink :weight normal)
               (mu4e-view-url-number-face :foreground ,material-candy-purple)
               (mu4e-highlight-face :background ,material-candy-bg
                                    :foreground ,material-candy-yellow
                                    :extend t)
               (mu4e-header-highlight-face :background ,material-candy-current
                                           :foreground ,material-candy-fg
                                           :underline nil :weight bold
                                           :extend t)
               (mu4e-header-key-face :inherit message-mml)
               (mu4e-header-marks-face :foreground ,material-candy-purple)
               (mu4e-cited-1-face :foreground ,material-candy-purple)
               (mu4e-cited-2-face :foreground ,material-candy-orange)
               (mu4e-cited-3-face :foreground ,material-candy-comment)
               (mu4e-cited-4-face :foreground ,fg2)
               (mu4e-cited-5-face :foreground ,fg3)
               ;; neotree
               (neo-banner-face :foreground ,material-candy-orange :weight bold)
               (neo-dir-link-face :foreground ,material-candy-purple)
               (neo-expand-btn-face :foreground ,material-candy-fg)
               (neo-file-link-face :foreground ,material-candy-cyan)
               (neo-header-face :background ,material-candy-bg
                                :foreground ,material-candy-fg
                                :weight bold)
               (neo-root-dir-face :foreground ,material-candy-purple :weight bold)
               (neo-vc-added-face :foreground ,material-candy-orange)
               (neo-vc-conflict-face :foreground ,material-candy-red)
               (neo-vc-default-face :inherit neo-file-link-face)
               (neo-vc-edited-face :foreground ,material-candy-orange)
               (neo-vc-ignored-face :foreground ,material-candy-comment)
               (neo-vc-missing-face :foreground ,material-candy-red)
               (neo-vc-needs-merge-face :foreground ,material-candy-red
                                        :weight bold)
               (neo-vc-unlocked-changes-face :foreground ,material-candy-red)
               (neo-vc-up-to-date-face :foreground ,material-candy-green)
               (neo-vc-user-face :foreground ,material-candy-purple)
               ;; org
               (org-agenda-date :foreground ,material-candy-cyan :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,material-candy-comment)
               (org-agenda-done :foreground ,material-candy-green)
               (org-agenda-structure :foreground ,material-candy-purple)
               (org-block :foreground ,material-candy-orange)
               (org-code :foreground ,material-candy-yellow)
               (org-column :background ,bg4)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,material-candy-cyan :underline t)
               (org-document-info :foreground ,other-blue)
               (org-document-info-keyword :foreground ,material-candy-comment)
               (org-document-title :weight bold :foreground ,material-candy-orange
                                   ,@(when material-candy-enlarge-headings
                                       (list :height material-candy-height-doc-title)))
               (org-done :foreground ,material-candy-green)
               (org-ellipsis :foreground ,material-candy-comment)
               (org-footnote :foreground ,other-blue)
               (org-formula :foreground ,material-candy-pink)
               (org-headline-done :foreground ,material-candy-comment
                                  :weight normal :strike-through t)
               (org-hide :foreground ,material-candy-bg :background ,material-candy-bg)
               (org-level-1 :inherit bold :foreground ,material-candy-pink
                            ,@(when material-candy-enlarge-headings
                                (list :height material-candy-height-title-1)))
               (org-level-2 :inherit bold :foreground ,material-candy-purple
                            ,@(when material-candy-enlarge-headings
                                (list :height material-candy-height-title-2)))
               (org-level-3 :weight normal :foreground ,material-candy-green
                            ,@(when material-candy-enlarge-headings
                                (list :height material-candy-height-title-3)))
               (org-level-4 :weight normal :foreground ,material-candy-yellow)
               (org-level-5 :weight normal :foreground ,material-candy-cyan)
               (org-level-6 :weight normal :foreground ,material-candy-orange)
               (org-level-7 :weight normal :foreground ,other-blue)
               (org-level-8 :weight normal :foreground ,material-candy-fg)
               (org-link :foreground ,material-candy-cyan :underline t)
               (org-priority :foreground ,material-candy-cyan)
               (org-scheduled :foreground ,material-candy-green)
               (org-scheduled-previously :foreground ,material-candy-yellow)
               (org-scheduled-today :foreground ,material-candy-green)
               (org-sexp-date :foreground ,fg4)
               (org-special-keyword :foreground ,material-candy-yellow)
               (org-table :foreground ,material-candy-purple)
               (org-tag :foreground ,material-candy-pink :weight bold :background ,bg2)
               (org-todo :foreground ,material-candy-orange :weight bold :background ,bg2)
               (org-upcoming-deadline :foreground ,material-candy-yellow)
               (org-warning :weight bold :foreground ,material-candy-pink)
               ;; outline
               (outline-1 :foreground ,material-candy-pink)
               (outline-2 :foreground ,material-candy-purple)
               (outline-3 :foreground ,material-candy-green)
               (outline-4 :foreground ,material-candy-yellow)
               (outline-5 :foreground ,material-candy-cyan)
               (outline-6 :foreground ,material-candy-orange)
               ;; powerline
               (powerline-evil-base-face :foreground ,bg2)
               (powerline-evil-emacs-face :inherit powerline-evil-base-face :background ,material-candy-yellow)
               (powerline-evil-insert-face :inherit powerline-evil-base-face :background ,material-candy-cyan)
               (powerline-evil-motion-face :inherit powerline-evil-base-face :background ,material-candy-purple)
               (powerline-evil-normal-face :inherit powerline-evil-base-face :background ,material-candy-green)
               (powerline-evil-operator-face :inherit powerline-evil-base-face :background ,material-candy-pink)
               (powerline-evil-replace-face :inherit powerline-evil-base-face :background ,material-candy-red)
               (powerline-evil-visual-face :inherit powerline-evil-base-face :background ,material-candy-orange)
               ;; rainbow-delimiters
               (rainbow-delimiters-depth-1-face :foreground ,material-candy-fg)
               (rainbow-delimiters-depth-2-face :foreground ,material-candy-cyan)
               (rainbow-delimiters-depth-3-face :foreground ,material-candy-purple)
               (rainbow-delimiters-depth-4-face :foreground ,material-candy-pink)
               (rainbow-delimiters-depth-5-face :foreground ,material-candy-orange)
               (rainbow-delimiters-depth-6-face :foreground ,material-candy-green)
               (rainbow-delimiters-depth-7-face :foreground ,material-candy-yellow)
               (rainbow-delimiters-depth-8-face :foreground ,other-blue)
               (rainbow-delimiters-unmatched-face :foreground ,material-candy-orange)
               ;; rpm-spec
               (rpm-spec-dir-face :foreground ,material-candy-green)
               (rpm-spec-doc-face :foreground ,material-candy-pink)
               (rpm-spec-ghost-face :foreground ,material-candy-purple)
               (rpm-spec-macro-face :foreground ,material-candy-yellow)
               (rpm-spec-obsolete-tag-face :inherit font-lock-warning-face)
               (rpm-spec-package-face :foreground ,material-candy-purple)
               (rpm-spec-section-face :foreground ,material-candy-yellow)
               (rpm-spec-tag-face :foreground ,material-candy-cyan)
               (rpm-spec-var-face :foreground ,material-candy-orange)
               ;; show-paren
               (show-paren-match-face :background unspecified
                                      :foreground ,material-candy-cyan
                                      :weight bold)
               (show-paren-match :background unspecified
                                 :foreground ,material-candy-cyan
                                 :weight bold)
               (show-paren-match-expression :inherit match)
               (show-paren-mismatch :inherit font-lock-warning-face)
               ;; slime
               (slime-repl-inputed-output-face :foreground ,material-candy-purple)
               ;; spam
               (spam :inherit gnus-summary-normal-read :foreground ,material-candy-orange
                     :strike-through t :slant oblique)
               ;; speedbar (and sr-speedbar)
               (speedbar-button-face :foreground ,material-candy-green)
               (speedbar-file-face :foreground ,material-candy-cyan)
               (speedbar-directory-face :foreground ,material-candy-purple)
               (speedbar-tag-face :foreground ,material-candy-yellow)
               (speedbar-selected-face :foreground ,material-candy-pink)
               (speedbar-highlight-face :inherit match)
               (speedbar-separator-face :background ,material-candy-bg
                                        :foreground ,material-candy-fg
                                        :weight bold)
               ;; tab-bar & tab-line (since Emacs 27.1)
               (tab-bar :foreground ,material-candy-purple :background ,material-candy-current
                        :inherit variable-pitch)
               (tab-bar-tab :foreground ,material-candy-pink :background ,material-candy-bg
                            :box (:line-width 2 :color ,material-candy-bg :style nil))
               (tab-bar-tab-inactive :foreground ,material-candy-purple :background ,bg2
                                     :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line :foreground ,material-candy-purple :background ,material-candy-current
                         :height 0.9 :inherit variable-pitch)
               (tab-line-tab :foreground ,material-candy-pink :background ,material-candy-bg
                             :box (:line-width 2 :color ,material-candy-bg :style nil))
               (tab-line-tab-inactive :foreground ,material-candy-purple :background ,bg2
                                      :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line-tab-current :inherit tab-line-tab)
               (tab-line-close-highlight :foreground ,material-candy-red)
               ;; term
               (term :foreground ,material-candy-fg :background ,material-candy-bg)
               (term-color-black :foreground ,material-candy-bg :background ,material-candy-bg)
               (term-color-blue :foreground ,material-candy-purple :background ,material-candy-purple)
               (term-color-cyan :foreground ,material-candy-cyan :background ,material-candy-cyan)
               (term-color-green :foreground ,material-candy-green :background ,material-candy-green)
               (term-color-magenta :foreground ,material-candy-pink :background ,material-candy-pink)
               (term-color-red :foreground ,material-candy-red :background ,material-candy-red)
               (term-color-white :foreground ,material-candy-fg :background ,material-candy-fg)
               (term-color-yellow :foreground ,material-candy-yellow :background ,material-candy-yellow)
               ;; undo-tree
               (undo-tree-visualizer-current-face :foreground ,material-candy-orange)
               (undo-tree-visualizer-default-face :foreground ,fg2)
               (undo-tree-visualizer-register-face :foreground ,material-candy-purple)
               (undo-tree-visualizer-unmodified-face :foreground ,material-candy-fg)
               ;; web-mode
               (web-mode-builtin-face :inherit ,font-lock-builtin-face)
               (web-mode-comment-face :inherit ,font-lock-comment-face)
               (web-mode-constant-face :inherit ,font-lock-constant-face)
               (web-mode-doctype-face :inherit ,font-lock-comment-face)
               (web-mode-function-name-face :inherit ,font-lock-function-name-face)
               (web-mode-html-attr-name-face :foreground ,material-candy-purple)
               (web-mode-html-attr-value-face :foreground ,material-candy-green)
               (web-mode-html-tag-face :foreground ,material-candy-pink :weight bold)
               (web-mode-keyword-face :foreground ,material-candy-pink)
               (web-mode-string-face :foreground ,material-candy-green)
               (web-mode-type-face :inherit ,font-lock-type-face)
               (web-mode-warning-face :inherit ,font-lock-warning-face)
               ;; which-func
               (which-func :inherit ,font-lock-function-name-face)
               ;; whitespace
               (whitespace-big-indent :background ,material-candy-red :foreground ,material-candy-red)
               (whitespace-empty :background ,material-candy-orange :foreground ,material-candy-red)
               (whitespace-hspace :background ,bg3 :foreground ,material-candy-comment)
               (whitespace-indentation :background ,material-candy-orange :foreground ,material-candy-red)
               (whitespace-line :background ,material-candy-bg :foreground ,material-candy-pink)
               (whitespace-newline :foreground ,material-candy-comment)
               (whitespace-space :background ,material-candy-bg :foreground ,material-candy-comment)
               (whitespace-space-after-tab :background ,material-candy-orange :foreground ,material-candy-red)
               (whitespace-space-before-tab :background ,material-candy-orange :foreground ,material-candy-red)
               (whitespace-tab :background ,bg2 :foreground ,material-candy-comment)
               (whitespace-trailing :inherit trailing-whitespace)
               ;; yard-mode
               (yard-tag-face :inherit ,font-lock-builtin-face)
               (yard-directive-face :inherit ,font-lock-builtin-face))))

  (apply #'custom-theme-set-faces
         'material-candy
         (let ((color-names (mapcar #'car colors))
               (graphic-colors (mapcar #'cadr colors))
               (term-colors (mapcar #'car (mapcar #'cddr colors)))
               (tty-colors (mapcar #'car (mapcar #'last colors)))
               (expand-for-kind
                (lambda (kind spec)
                  (when (and (string= (symbol-name kind) "term-colors")
                             material-candy-use-24-bit-colors-on-256-colors-terms)
                    (setq kind 'graphic-colors))
                  (cl-progv color-names (symbol-value kind)
                    (eval `(backquote ,spec))))))
           (cl-loop for (face . spec) in faces
                    collect `(,face
                              ((((min-colors 16777216)) ; fully graphical envs
                                ,(funcall expand-for-kind 'graphic-colors spec))
                               (((min-colors 256))      ; terminal withs 256 colors
                                ,(funcall expand-for-kind 'term-colors spec))
                               (t                       ; should be only tty-like envs
                                ,(funcall expand-for-kind 'tty-colors spec))))))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'material-candy)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; material-candy-theme.el ends here

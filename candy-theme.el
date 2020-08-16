;;; candy-theme.el --- Candy Theme

;; Copyright 2015-present, All rights reserved
;;
;; Code licensed under the MIT license

;; Author: shaneikennedy
;; Version: 1.7.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/shaneikennedy/emacs-material-candy

;;; Commentary:
;; A port of material candy by millsp.

;;; Code:
(require 'cl-lib)
(deftheme candy)


;;;; Configuration options:

(defgroup candy nil
  "Candy theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom candy-enlarge-headings t
  "Use different font sizes for some headings and titles."
  :type 'boolean
  :group 'candy)

(defcustom candy-height-title-1 1.3
  "Font size 100%."
  :type 'number
  :group 'candy)

(defcustom candy-height-title-2 1.1
  "Font size 110%."
  :type 'number
  :group 'candy)

(defcustom candy-height-title-3 1.0
  "Font size 130%."
  :type 'number
  :group 'candy)

(defcustom candy-height-doc-title 1.44
  "Font size 144%."
  :type 'number
  :group 'candy)

(defcustom candy-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'boolean
  :group 'candy)

(defvar candy-use-24-bit-colors-on-256-colors-terms nil
  "Use true colors even on terminals announcing less capabilities.

Beware the use of this variable.  Using it may lead to unwanted
behavior, the most common one being an ugly blue background on
terminals, which don't understand 24 bit colors.  To avoid this
blue background, when using this variable, one can try to add the
following lines in their config file after having load the
Candy theme:

    (unless (display-graphic-p)
      (set-face-background 'default \"black\" nil))")


;;;; Theme definition:

;; Assigment form: VARIABLE COLOR [256-COLOR [TTY-COLOR]]
(let ((colors '(;; Upstream theme color
                (candy-bg      "#161616" "unspecified-bg" "unspecified-bg") ; candy background
                (candy-fg      "#BBBBBB" "#BBBBBB" "brightwhite") ; candy foreground
                (candy-current "#44475a" "#44475a" "brightblack") ; candy current-line/selection
                (candy-comment "#6A9955" "#6A9955" "blue")        ; candy comment
                (candy-cyan    "#5fc1df" "#5fc1df" "brightcyan")  ; candy cyan
                (candy-green   "#cab760" "#cab760" "darkyellow")       ; candy dark yellow
                (candy-orange  "#ffb86c" "#ffb86c" "brightred")   ; candy orange
                (candy-pink    "#df5f74" "#df5f74" "magenta")     ; candy pink
                (candy-purple  "#4fb497" "#4fb497" "brightmagenta") ; candy purple
                (candy-red     "#ff5555" "#ff5555" "red")         ; candy red
                (candy-yellow  "#f1fa8c" "#f1fa8c" "yellow")      ; candy yellow
                (candy-active-modeline  "#6272a4" "#5f5faf" "modeline")      ; candy active mode-line

                ;; Other colors
                (bg2             "#373844" "#121212" "brightblack")
                (bg3             "#464752" "#262626" "brightblack")
                (bg4             "#565761" "#444444" "brightblack")
                (fg2             "#e2e2dc" "#e4e4e4" "brightwhite")
                (fg3             "#ccccc7" "#c6c6c6" "white")
                (fg4             "#b6b6b2" "#b2b2b2" "white")
                (other-blue      "#0189cc" "#0087ff" "brightblue")))
      (faces '(;; default
               (cursor :background , candy-pink)
               (completions-first-difference :foreground ,candy-pink :weight bold)
               (default :background ,candy-bg :foreground ,candy-fg)
               (default-italic :slant italic)
               (ffap :foreground ,fg4)
               (fringe :background ,candy-bg :foreground ,fg4)
               (highlight :foreground ,fg2 :background ,bg3)
               (hl-line :background ,candy-current :extend t)
               (info-quoted-name :foreground ,candy-orange)
               (info-string :foreground ,candy-yellow)
               (lazy-highlight :foreground ,fg2 :background ,bg2)
               (link :foreground ,candy-cyan :underline t)
               (linum :slant italic :foreground ,bg4 :background ,candy-bg)
               (line-number :slant italic :foreground ,bg4 :background ,candy-bg)
               (match :background ,candy-yellow :foreground ,candy-bg)
               (minibuffer-prompt
                ,@(if candy-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground candy-fg)
                    (list :weight 'bold :foreground candy-pink)))
               (read-multiple-choice-face :inherit completions-first-difference)
               (region :inherit match :extend t)
               (trailing-whitespace :foreground "unspecified-fg" :background ,candy-orange)
               (vertical-border :foreground ,bg2)
               (success :foreground ,candy-green)
               (warning :foreground ,candy-orange)
               (error :foreground ,candy-red)
               (header-line :background ,candy-bg)
               ;; syntax
               (font-lock-builtin-face :foreground ,candy-orange)
               (font-lock-comment-face :foreground ,candy-comment)
               (font-lock-comment-delimiter-face :foreground ,candy-comment)
               (font-lock-constant-face :foreground ,candy-cyan)
               (font-lock-doc-face :foreground ,candy-comment)
               (font-lock-function-name-face :foreground ,candy-green :weight bold)
               (font-lock-keyword-face :weight bold :foreground ,candy-pink)
               (font-lock-negation-char-face :foreground ,candy-cyan)
               (font-lock-preprocessor-face :foreground ,candy-orange)
               (font-lock-reference-face :foreground ,candy-cyan)
               (font-lock-regexp-grouping-backslash :foreground ,candy-cyan)
               (font-lock-regexp-grouping-construct :foreground ,candy-purple)
               (font-lock-string-face :foreground ,candy-yellow)
               (font-lock-type-face :foreground ,candy-purple)
               (font-lock-variable-name-face :foreground ,candy-pink
                                             :weight bold)
               (font-lock-warning-face :foreground ,candy-orange :background ,bg2)
               ;; auto-complete
               (ac-completion-face :underline t :foreground ,candy-pink)
               ;; company
               (company-echo-common :foreground ,candy-bg :background ,candy-fg)
               (company-preview :background ,candy-current :foreground ,other-blue)
               (company-preview-common :inherit company-preview
                                       :foreground ,candy-pink)
               (company-preview-search :inherit company-preview
                                       :foreground ,candy-green)
               (company-scrollbar-bg :background ,candy-comment)
               (company-scrollbar-fg :foreground ,other-blue)
               (company-tooltip :foreground ,candy-fg :background ,candy-current)
               (company-tooltip-search :foreground ,candy-green
                                       :underline t)
               (company-tooltip-search-selection :background ,candy-green
                                                 :foreground ,candy-bg)
               (company-tooltip-selection :inherit match)
               (company-tooltip-mouse :background ,candy-bg)
               (company-tooltip-common :foreground ,candy-pink :weight bold)
               (company-tooltip-annotation :foreground ,candy-cyan)
               ;; diff-hl
               (diff-hl-change :foreground ,candy-orange :background ,candy-orange)
               (diff-hl-delete :foreground ,candy-pink :background ,candy-pink)
               (diff-hl-insert :foreground ,candy-cyan :background ,candy-cyan)
               ;; dired
               (dired-directory :foreground ,candy-green :weight normal)
               (dired-flagged :foreground ,candy-pink)
               (dired-header :foreground ,fg3 :background ,candy-bg)
               (dired-ignored :inherit shadow)
               (dired-mark :foreground ,candy-fg :weight bold)
               (dired-marked :foreground ,candy-orange :weight bold)
               (dired-perm-write :foreground ,fg3 :underline t)
               (dired-symlink :foreground ,candy-yellow :weight normal :slant italic)
               (dired-warning :foreground ,candy-orange :underline t)
               (diredp-compressed-file-name :foreground ,fg3)
               (diredp-compressed-file-suffix :foreground ,fg4)
               (diredp-date-time :foreground ,candy-fg)
               (diredp-deletion-file-name :foreground ,candy-pink :background ,candy-current)
               (diredp-deletion :foreground ,candy-pink :weight bold)
               (diredp-dir-heading :foreground ,fg2 :background ,bg4)
               (diredp-dir-name :inherit dired-directory)
               (diredp-dir-priv :inherit dired-directory)
               (diredp-executable-tag :foreground ,candy-orange)
               (diredp-file-name :foreground ,candy-fg)
               (diredp-file-suffix :foreground ,fg4)
               (diredp-flag-mark-line :foreground ,fg2 :slant italic :background ,candy-current)
               (diredp-flag-mark :foreground ,fg2 :weight bold :background ,candy-current)
               (diredp-ignored-file-name :foreground ,candy-fg)
               (diredp-mode-line-flagged :foreground ,candy-orange)
               (diredp-mode-line-marked :foreground ,candy-orange)
               (diredp-no-priv :foreground ,candy-fg)
               (diredp-number :foreground ,candy-cyan)
               (diredp-other-priv :foreground ,candy-orange)
               (diredp-rare-priv :foreground ,candy-orange)
               (diredp-read-priv :foreground ,candy-purple)
               (diredp-write-priv :foreground ,candy-pink)
               (diredp-exec-priv :foreground ,candy-yellow)
               (diredp-symlink :foreground ,candy-orange)
               (diredp-link-priv :foreground ,candy-orange)
               (diredp-autofile-name :foreground ,candy-yellow)
               (diredp-tagged-autofile-name :foreground ,candy-yellow)
               ;; elfeed
               (elfeed-search-date-face :foreground ,candy-comment)
               (elfeed-search-title-face :foreground ,candy-fg)
               (elfeed-search-unread-title-face :foreground ,candy-pink :weight bold)
               (elfeed-search-feed-face :foreground ,candy-fg :weight bold)
               (elfeed-search-tag-face :foreground ,candy-green)
               (elfeed-search-last-update-face :weight bold)
               (elfeed-search-unread-count-face :foreground ,candy-pink)
               (elfeed-search-filter-face :foreground ,candy-green :weight bold)
               (elfeed-log-error-level-face :foreground ,candy-red)
               (elfeed-log-warn-level-face :foreground ,candy-orange)
               (elfeed-log-info-level-face :foreground ,candy-cyan)
               (elfeed-log-debug-level-face :foreground ,candy-comment)
               ;; enh-ruby
               (enh-ruby-heredoc-delimiter-face :foreground ,candy-yellow)
               (enh-ruby-op-face :foreground ,candy-pink)
               (enh-ruby-regexp-delimiter-face :foreground ,candy-yellow)
               (enh-ruby-string-delimiter-face :foreground ,candy-yellow)
               ;; flyspell
               (flyspell-duplicate :underline (:style wave :color ,candy-orange))
               (flyspell-incorrect :underline (:style wave :color ,candy-red))
               ;; font-latex
               (font-latex-bold-face :foreground ,candy-purple)
               (font-latex-italic-face :foreground ,candy-pink :slant italic)
               (font-latex-match-reference-keywords :foreground ,candy-cyan)
               (font-latex-match-variable-keywords :foreground ,candy-fg)
               (font-latex-string-face :foreground ,candy-green)
               ;; gnus-group
               (gnus-group-mail-1 :foreground ,candy-pink :weight bold)
               (gnus-group-mail-1-empty :inherit gnus-group-mail-1 :weight normal)
               (gnus-group-mail-2 :foreground ,candy-cyan :weight bold)
               (gnus-group-mail-2-empty :inherit gnus-group-mail-2 :weight normal)
               (gnus-group-mail-3 :foreground ,candy-comment :weight bold)
               (gnus-group-mail-3-empty :inherit gnus-group-mail-3 :weight normal)
               (gnus-group-mail-low :foreground ,candy-current :weight bold)
               (gnus-group-mail-low-empty :inherit gnus-group-mail-low :weight normal)
               (gnus-group-news-1 :foreground ,candy-pink :weight bold)
               (gnus-group-news-1-empty :inherit gnus-group-news-1 :weight normal)
               (gnus-group-news-2 :foreground ,candy-cyan :weight bold)
               (gnus-group-news-2-empty :inherit gnus-group-news-2 :weight normal)
               (gnus-group-news-3 :foreground ,candy-comment :weight bold)
               (gnus-group-news-3-empty :inherit gnus-group-news-3 :weight normal)
               (gnus-group-news-4 :inherit gnus-group-news-low)
               (gnus-group-news-4-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-5 :inherit gnus-group-news-low)
               (gnus-group-news-5-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-6 :inherit gnus-group-news-low)
               (gnus-group-news-6-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-low :foreground ,candy-current :weight bold)
               (gnus-group-news-low-empty :inherit gnus-group-news-low :weight normal)
               (gnus-header-content :foreground ,candy-pink)
               (gnus-header-from :foreground ,candy-fg)
               (gnus-header-name :foreground ,candy-purple)
               (gnus-header-subject :foreground ,candy-green :weight bold)
               (gnus-summary-markup-face :foreground ,candy-cyan)
               (gnus-summary-high-unread :foreground ,candy-pink :weight bold)
               (gnus-summary-high-read :inherit gnus-summary-high-unread :weight normal)
               (gnus-summary-high-ancient :inherit gnus-summary-high-read)
               (gnus-summary-high-ticked :inherit gnus-summary-high-read :underline t)
               (gnus-summary-normal-unread :foreground ,other-blue :weight bold)
               (gnus-summary-normal-read :foreground ,candy-comment :weight normal)
               (gnus-summary-normal-ancient :inherit gnus-summary-normal-read :weight light)
               (gnus-summary-normal-ticked :foreground ,candy-pink :weight bold)
               (gnus-summary-low-unread :foreground ,candy-comment :weight bold)
               (gnus-summary-low-read :inherit gnus-summary-low-unread :weight normal)
               (gnus-summary-low-ancient :inherit gnus-summary-low-read)
               (gnus-summary-low-ticked :inherit gnus-summary-low-read :underline t)
               (gnus-summary-selected :inverse-video t)
               ;; haskell-mode
               (haskell-operator-face :foreground ,candy-pink)
               (haskell-constructor-face :foreground ,candy-purple)
               ;; helm
               (helm-bookmark-w3m :foreground ,candy-purple)
               (helm-buffer-not-saved :foreground ,candy-purple :background ,candy-bg)
               (helm-buffer-process :foreground ,candy-orange :background ,candy-bg)
               (helm-buffer-saved-out :foreground ,candy-fg :background ,candy-bg)
               (helm-buffer-size :foreground ,candy-fg :background ,candy-bg)
               (helm-candidate-number :foreground ,candy-bg :background ,candy-fg)
               (helm-ff-directory :foreground ,candy-green :background ,candy-bg :weight bold)
               (helm-ff-dotted-directory :foreground ,candy-green :background ,candy-bg :weight normal)
               (helm-ff-executable :foreground ,other-blue :background ,candy-bg :weight normal)
               (helm-ff-file :foreground ,candy-fg :background ,candy-bg :weight normal)
               (helm-ff-invalid-symlink :foreground ,candy-pink :background ,candy-bg :weight bold)
               (helm-ff-prefix :foreground ,candy-bg :background ,candy-pink :weight normal)
               (helm-ff-symlink :foreground ,candy-pink :background ,candy-bg :weight bold)
               (helm-grep-cmd-line :foreground ,candy-fg :background ,candy-bg)
               (helm-grep-file :foreground ,candy-fg :background ,candy-bg)
               (helm-grep-finish :foreground ,fg2 :background ,candy-bg)
               (helm-grep-lineno :foreground ,candy-fg :background ,candy-bg)
               (helm-grep-match :foreground "unspecified-fg" :background "unspecified-bg" :inherit helm-match)
               (helm-grep-running :foreground ,candy-green :background ,candy-bg)
               (helm-header :foreground ,fg2 :background ,candy-bg :underline nil :box nil)
               (helm-moccur-buffer :foreground ,candy-green :background ,candy-bg)
               (helm-selection :background ,bg2 :underline nil)
               (helm-selection-line :background ,bg2)
               (helm-separator :foreground ,candy-purple :background ,candy-bg)
               (helm-source-go-package-godoc-description :foreground ,candy-yellow)
               (helm-source-header :foreground ,candy-pink :background ,candy-bg :underline nil :weight bold)
               (helm-time-zone-current :foreground ,candy-orange :background ,candy-bg)
               (helm-time-zone-home :foreground ,candy-purple :background ,candy-bg)
               (helm-visible-mark :foreground ,candy-bg :background ,bg3)
               ;; highlight-indentation minor mode
               (highlight-indentation-face :background ,bg2)
               ;; icicle
               (icicle-whitespace-highlight :background ,candy-fg)
               (icicle-special-candidate :foreground ,fg2)
               (icicle-extra-candidate :foreground ,fg2)
               (icicle-search-main-regexp-others :foreground ,candy-fg)
               (icicle-search-current-input :foreground ,candy-pink)
               (icicle-search-context-level-8 :foreground ,candy-orange)
               (icicle-search-context-level-7 :foreground ,candy-orange)
               (icicle-search-context-level-6 :foreground ,candy-orange)
               (icicle-search-context-level-5 :foreground ,candy-orange)
               (icicle-search-context-level-4 :foreground ,candy-orange)
               (icicle-search-context-level-3 :foreground ,candy-orange)
               (icicle-search-context-level-2 :foreground ,candy-orange)
               (icicle-search-context-level-1 :foreground ,candy-orange)
               (icicle-search-main-regexp-current :foreground ,candy-fg)
               (icicle-saved-candidate :foreground ,candy-fg)
               (icicle-proxy-candidate :foreground ,candy-fg)
               (icicle-mustmatch-completion :foreground ,candy-purple)
               (icicle-multi-command-completion :foreground ,fg2 :background ,bg2)
               (icicle-msg-emphasis :foreground ,candy-green)
               (icicle-mode-line-help :foreground ,fg4)
               (icicle-match-highlight-minibuffer :foreground ,candy-orange)
               (icicle-match-highlight-Completions :foreground ,candy-green)
               (icicle-key-complete-menu-local :foreground ,candy-fg)
               (icicle-key-complete-menu :foreground ,candy-fg)
               (icicle-input-completion-fail-lax :foreground ,candy-pink)
               (icicle-input-completion-fail :foreground ,candy-pink)
               (icicle-historical-candidate-other :foreground ,candy-fg)
               (icicle-historical-candidate :foreground ,candy-fg)
               (icicle-current-candidate-highlight :foreground ,candy-orange :background ,bg3)
               (icicle-Completions-instruction-2 :foreground ,fg4)
               (icicle-Completions-instruction-1 :foreground ,fg4)
               (icicle-completion :foreground ,candy-fg)
               (icicle-complete-input :foreground ,candy-orange)
               (icicle-common-match-highlight-Completions :foreground ,candy-purple)
               (icicle-candidate-part :foreground ,candy-fg)
               (icicle-annotation :foreground ,fg4)
               ;; icomplete
               (icompletep-determined :foreground ,candy-orange)
               ;; ido
               (ido-first-match
                ,@(if candy-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground candy-green)
                    (list :weight 'bold :foreground candy-pink)))
               (ido-only-match :foreground ,candy-orange)
               (ido-subdir :foreground ,candy-yellow)
               (ido-virtual :foreground ,candy-cyan)
               (ido-incomplete-regexp :inherit font-lock-warning-face)
               (ido-indicator :foreground ,candy-fg :background ,candy-pink)
               ;; isearch
               (isearch :inherit match :weight bold)
               (isearch-fail :foreground ,candy-bg :background ,candy-orange)
               ;; jde-java
               (jde-java-font-lock-constant-face :foreground ,candy-cyan)
               (jde-java-font-lock-modifier-face :foreground ,candy-pink)
               (jde-java-font-lock-number-face :foreground ,candy-fg)
               (jde-java-font-lock-package-face :foreground ,candy-fg)
               (jde-java-font-lock-private-face :foreground ,candy-pink)
               (jde-java-font-lock-public-face :foreground ,candy-pink)
               ;; js2-mode
               (js2-external-variable :foreground ,candy-purple)
               (js2-function-param :foreground ,candy-cyan)
               (js2-jsdoc-html-tag-delimiter :foreground ,candy-green)
               (js2-jsdoc-html-tag-name :foreground ,other-blue)
               (js2-jsdoc-value :foreground ,candy-yellow)
               (js2-private-function-call :foreground ,candy-cyan)
               (js2-private-member :foreground ,fg3)
               ;; js3-mode
               (js3-error-face :underline ,candy-orange)
               (js3-external-variable-face :foreground ,candy-fg)
               (js3-function-param-face :foreground ,candy-pink)
               (js3-instance-member-face :foreground ,candy-cyan)
               (js3-jsdoc-tag-face :foreground ,candy-pink)
               (js3-warning-face :underline ,candy-pink)
               ;; lsp
               (lsp-ui-peek-peek :background ,candy-bg)
               (lsp-ui-peek-list :background ,bg2)
               (lsp-ui-peek-filename :foreground ,candy-pink :weight bold)
               (lsp-ui-peek-line-number :foreground ,candy-fg)
               (lsp-ui-peek-highlight :inherit highlight :distant-foreground ,candy-bg)
               (lsp-ui-peek-header :background ,bg3 :foreground ,fg3, :weight bold)
               (lsp-ui-peek-footer :inherit lsp-ui-peek-header)
               (lsp-ui-peek-selection :inherit match)
               (lsp-ui-sideline-symbol :foreground ,fg4 :box (:line-width -1 :color ,fg4) :height 0.99)
               (lsp-ui-sideline-current-symbol :foreground ,candy-fg :weight ultra-bold
                                               :box (:line-width -1 :color candy-fg) :height 0.99)
               (lsp-ui-sideline-code-action :foreground ,candy-yellow)
               (lsp-ui-sideline-symbol-info :slant italic :height 0.99)
               (lsp-ui-doc-background :background ,candy-bg)
               (lsp-ui-doc-header :foreground ,candy-bg :background ,candy-cyan)
               ;; magit
               (magit-branch-local :foreground ,candy-cyan)
               (magit-branch-remote :foreground ,candy-green)
               (magit-tag :foreground ,candy-orange)
               (magit-section-heading :foreground ,candy-pink :weight bold)
               (magit-section-highlight :background ,bg3 :extend t)
               (magit-diff-context-highlight :background ,bg3
                                             :foreground ,fg3
                                             :extend t)
               (magit-diff-revision-summary :foreground ,candy-orange
                                            :background ,candy-bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :foreground ,candy-orange
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
               (magit-diff-file-heading :foreground ,candy-fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight)
               (magit-diffstat-added :foreground ,candy-green)
               (magit-diffstat-removed :foreground ,candy-red)
               (magit-hash :foreground ,fg2)
               (magit-hunk-heading :background ,bg3)
               (magit-hunk-heading-highlight :background ,bg3)
               (magit-item-highlight :background ,bg3)
               (magit-log-author :foreground ,fg3)
               (magit-process-ng :foreground ,candy-orange :weight bold)
               (magit-process-ok :foreground ,candy-green :weight bold)
               ;; markdown
               (markdown-blockquote-face :foreground ,candy-orange)
               (markdown-code-face :foreground ,candy-orange)
               (markdown-footnote-face :foreground ,other-blue)
               (markdown-header-face :weight normal)
               (markdown-header-face-1
                :inherit bold :foreground ,candy-pink
                ,@(when candy-enlarge-headings
                    (list :height candy-height-title-1)))
               (markdown-header-face-2
                :inherit bold :foreground ,candy-purple
                ,@(when candy-enlarge-headings
                    (list :height candy-height-title-2)))
               (markdown-header-face-3
                :foreground ,candy-green
                ,@(when candy-enlarge-headings
                    (list :height candy-height-title-3)))
               (markdown-header-face-4 :foreground ,candy-yellow)
               (markdown-header-face-5 :foreground ,candy-cyan)
               (markdown-header-face-6 :foreground ,candy-orange)
               (markdown-header-face-7 :foreground ,other-blue)
               (markdown-header-face-8 :foreground ,candy-fg)
               (markdown-inline-code-face :foreground ,candy-yellow)
               (markdown-plain-url-face :inherit link)
               (markdown-pre-face :foreground ,candy-orange)
               (markdown-table-face :foreground ,candy-purple)
               ;; message
               (message-header-to :foreground ,candy-fg :weight bold)
               (message-header-cc :foreground ,candy-fg :bold bold)
               (message-header-subject :foreground ,candy-orange)
               (message-header-newsgroups :foreground ,candy-purple)
               (message-header-other :foreground ,candy-purple)
               (message-header-name :foreground ,candy-green)
               (message-header-xheader :foreground ,candy-cyan)
               (message-separator :foreground ,candy-cyan :slant italic)
               (message-cited-text :foreground ,candy-purple)
               (message-cited-text-1 :foreground ,candy-purple)
               (message-cited-text-2 :foreground ,candy-orange)
               (message-cited-text-3 :foreground ,candy-comment)
               (message-cited-text-4 :foreground ,fg2)
               (message-mml :foreground ,candy-green :weight normal)
               ;; mode-line
               (mode-line :background ,candy-active-modeline
                          :box ,candy-current :inverse-video nil
                          ,@(if candy-alternate-mode-line-and-minibuffer
                                (list :foreground fg3)
                              (list :foreground "unspecified-fg")))
               (mode-line-inactive
                :inverse-video nil
                ,@(if candy-alternate-mode-line-and-minibuffer
                      (list :foreground candy-comment :background candy-bg
                            :box candy-bg)
                    (list :foreground candy-fg :background bg2 :box bg2)))
               ;; mu4e
               (mu4e-unread-face :foreground ,candy-pink :weight normal)
               (mu4e-view-url-number-face :foreground ,candy-purple)
               (mu4e-highlight-face :background ,candy-bg
                                    :foreground ,candy-yellow
                                    :extend t)
               (mu4e-header-highlight-face :background ,candy-current
                                           :foreground ,candy-fg
                                           :underline nil :weight bold
                                           :extend t)
               (mu4e-header-key-face :inherit message-mml)
               (mu4e-header-marks-face :foreground ,candy-purple)
               (mu4e-cited-1-face :foreground ,candy-purple)
               (mu4e-cited-2-face :foreground ,candy-orange)
               (mu4e-cited-3-face :foreground ,candy-comment)
               (mu4e-cited-4-face :foreground ,fg2)
               (mu4e-cited-5-face :foreground ,fg3)
               ;; neotree
               (neo-banner-face :foreground ,candy-orange :weight bold)
               (neo-dir-link-face :foreground ,candy-purple)
               (neo-expand-btn-face :foreground ,candy-fg)
               (neo-file-link-face :foreground ,candy-cyan)
               (neo-header-face :background ,candy-bg
                                :foreground ,candy-fg
                                :weight bold)
               (neo-root-dir-face :foreground ,candy-purple :weight bold)
               (neo-vc-added-face :foreground ,candy-orange)
               (neo-vc-conflict-face :foreground ,candy-red)
               (neo-vc-default-face :inherit neo-file-link-face)
               (neo-vc-edited-face :foreground ,candy-orange)
               (neo-vc-ignored-face :foreground ,candy-comment)
               (neo-vc-missing-face :foreground ,candy-red)
               (neo-vc-needs-merge-face :foreground ,candy-red
                                        :weight bold)
               (neo-vc-unlocked-changes-face :foreground ,candy-red)
               (neo-vc-up-to-date-face :foreground ,candy-green)
               (neo-vc-user-face :foreground ,candy-purple)
               ;; org
               (org-agenda-date :foreground ,candy-cyan :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,candy-comment)
               (org-agenda-done :foreground ,candy-green)
               (org-agenda-structure :foreground ,candy-purple)
               (org-block :foreground ,candy-orange)
               (org-code :foreground ,candy-yellow)
               (org-column :background ,bg4)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,candy-cyan :underline t)
               (org-document-info :foreground ,other-blue)
               (org-document-info-keyword :foreground ,candy-comment)
               (org-document-title :weight bold :foreground ,candy-orange
                                   ,@(when candy-enlarge-headings
                                       (list :height candy-height-doc-title)))
               (org-done :foreground ,candy-green)
               (org-ellipsis :foreground ,candy-comment)
               (org-footnote :foreground ,other-blue)
               (org-formula :foreground ,candy-pink)
               (org-headline-done :foreground ,candy-comment
                                  :weight normal :strike-through t)
               (org-hide :foreground ,candy-bg :background ,candy-bg)
               (org-level-1 :inherit bold :foreground ,candy-pink
                            ,@(when candy-enlarge-headings
                                (list :height candy-height-title-1)))
               (org-level-2 :inherit bold :foreground ,candy-purple
                            ,@(when candy-enlarge-headings
                                (list :height candy-height-title-2)))
               (org-level-3 :weight normal :foreground ,candy-green
                            ,@(when candy-enlarge-headings
                                (list :height candy-height-title-3)))
               (org-level-4 :weight normal :foreground ,candy-yellow)
               (org-level-5 :weight normal :foreground ,candy-cyan)
               (org-level-6 :weight normal :foreground ,candy-orange)
               (org-level-7 :weight normal :foreground ,other-blue)
               (org-level-8 :weight normal :foreground ,candy-fg)
               (org-link :foreground ,candy-cyan :underline t)
               (org-priority :foreground ,candy-cyan)
               (org-scheduled :foreground ,candy-green)
               (org-scheduled-previously :foreground ,candy-yellow)
               (org-scheduled-today :foreground ,candy-green)
               (org-sexp-date :foreground ,fg4)
               (org-special-keyword :foreground ,candy-yellow)
               (org-table :foreground ,candy-purple)
               (org-tag :foreground ,candy-pink :weight bold :background ,bg2)
               (org-todo :foreground ,candy-orange :weight bold :background ,bg2)
               (org-upcoming-deadline :foreground ,candy-yellow)
               (org-warning :weight bold :foreground ,candy-pink)
               ;; outline
               (outline-1 :foreground ,candy-pink)
               (outline-2 :foreground ,candy-purple)
               (outline-3 :foreground ,candy-green)
               (outline-4 :foreground ,candy-yellow)
               (outline-5 :foreground ,candy-cyan)
               (outline-6 :foreground ,candy-orange)
               ;; powerline
               (powerline-evil-base-face :foreground ,bg2)
               (powerline-evil-emacs-face :inherit powerline-evil-base-face :background ,candy-yellow)
               (powerline-evil-insert-face :inherit powerline-evil-base-face :background ,candy-cyan)
               (powerline-evil-motion-face :inherit powerline-evil-base-face :background ,candy-purple)
               (powerline-evil-normal-face :inherit powerline-evil-base-face :background ,candy-green)
               (powerline-evil-operator-face :inherit powerline-evil-base-face :background ,candy-pink)
               (powerline-evil-replace-face :inherit powerline-evil-base-face :background ,candy-red)
               (powerline-evil-visual-face :inherit powerline-evil-base-face :background ,candy-orange)
               ;; rainbow-delimiters
               (rainbow-delimiters-depth-1-face :foreground ,candy-fg)
               (rainbow-delimiters-depth-2-face :foreground ,candy-cyan)
               (rainbow-delimiters-depth-3-face :foreground ,candy-purple)
               (rainbow-delimiters-depth-4-face :foreground ,candy-pink)
               (rainbow-delimiters-depth-5-face :foreground ,candy-orange)
               (rainbow-delimiters-depth-6-face :foreground ,candy-green)
               (rainbow-delimiters-depth-7-face :foreground ,candy-yellow)
               (rainbow-delimiters-depth-8-face :foreground ,other-blue)
               (rainbow-delimiters-unmatched-face :foreground ,candy-orange)
               ;; rpm-spec
               (rpm-spec-dir-face :foreground ,candy-green)
               (rpm-spec-doc-face :foreground ,candy-pink)
               (rpm-spec-ghost-face :foreground ,candy-purple)
               (rpm-spec-macro-face :foreground ,candy-yellow)
               (rpm-spec-obsolete-tag-face :inherit font-lock-warning-face)
               (rpm-spec-package-face :foreground ,candy-purple)
               (rpm-spec-section-face :foreground ,candy-yellow)
               (rpm-spec-tag-face :foreground ,candy-cyan)
               (rpm-spec-var-face :foreground ,candy-orange)
               ;; show-paren
               (show-paren-match-face :background unspecified
                                      :foreground ,candy-cyan
                                      :weight bold)
               (show-paren-match :background unspecified
                                 :foreground ,candy-cyan
                                 :weight bold)
               (show-paren-match-expression :inherit match)
               (show-paren-mismatch :inherit font-lock-warning-face)
               ;; slime
               (slime-repl-inputed-output-face :foreground ,candy-purple)
               ;; spam
               (spam :inherit gnus-summary-normal-read :foreground ,candy-orange
                     :strike-through t :slant oblique)
               ;; speedbar (and sr-speedbar)
               (speedbar-button-face :foreground ,candy-green)
               (speedbar-file-face :foreground ,candy-cyan)
               (speedbar-directory-face :foreground ,candy-purple)
               (speedbar-tag-face :foreground ,candy-yellow)
               (speedbar-selected-face :foreground ,candy-pink)
               (speedbar-highlight-face :inherit match)
               (speedbar-separator-face :background ,candy-bg
                                        :foreground ,candy-fg
                                        :weight bold)
               ;; tab-bar & tab-line (since Emacs 27.1)
               (tab-bar :foreground ,candy-purple :background ,candy-current
                        :inherit variable-pitch)
               (tab-bar-tab :foreground ,candy-pink :background ,candy-bg
                            :box (:line-width 2 :color ,candy-bg :style nil))
               (tab-bar-tab-inactive :foreground ,candy-purple :background ,bg2
                                     :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line :foreground ,candy-purple :background ,candy-current
                         :height 0.9 :inherit variable-pitch)
               (tab-line-tab :foreground ,candy-pink :background ,candy-bg
                             :box (:line-width 2 :color ,candy-bg :style nil))
               (tab-line-tab-inactive :foreground ,candy-purple :background ,bg2
                                      :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line-tab-current :inherit tab-line-tab)
               (tab-line-close-highlight :foreground ,candy-red)
               ;; term
               (term :foreground ,candy-fg :background ,candy-bg)
               (term-color-black :foreground ,candy-bg :background ,candy-bg)
               (term-color-blue :foreground ,candy-purple :background ,candy-purple)
               (term-color-cyan :foreground ,candy-cyan :background ,candy-cyan)
               (term-color-green :foreground ,candy-green :background ,candy-green)
               (term-color-magenta :foreground ,candy-pink :background ,candy-pink)
               (term-color-red :foreground ,candy-red :background ,candy-red)
               (term-color-white :foreground ,candy-fg :background ,candy-fg)
               (term-color-yellow :foreground ,candy-yellow :background ,candy-yellow)
               ;; undo-tree
               (undo-tree-visualizer-current-face :foreground ,candy-orange)
               (undo-tree-visualizer-default-face :foreground ,fg2)
               (undo-tree-visualizer-register-face :foreground ,candy-purple)
               (undo-tree-visualizer-unmodified-face :foreground ,candy-fg)
               ;; web-mode
               (web-mode-builtin-face :inherit ,font-lock-builtin-face)
               (web-mode-comment-face :inherit ,font-lock-comment-face)
               (web-mode-constant-face :inherit ,font-lock-constant-face)
               (web-mode-doctype-face :inherit ,font-lock-comment-face)
               (web-mode-function-name-face :inherit ,font-lock-function-name-face)
               (web-mode-html-attr-name-face :foreground ,candy-purple)
               (web-mode-html-attr-value-face :foreground ,candy-green)
               (web-mode-html-tag-face :foreground ,candy-pink :weight bold)
               (web-mode-keyword-face :foreground ,candy-pink)
               (web-mode-string-face :foreground ,candy-green)
               (web-mode-type-face :inherit ,font-lock-type-face)
               (web-mode-warning-face :inherit ,font-lock-warning-face)
               ;; which-func
               (which-func :inherit ,font-lock-function-name-face)
               ;; whitespace
               (whitespace-big-indent :background ,candy-red :foreground ,candy-red)
               (whitespace-empty :background ,candy-orange :foreground ,candy-red)
               (whitespace-hspace :background ,bg3 :foreground ,candy-comment)
               (whitespace-indentation :background ,candy-orange :foreground ,candy-red)
               (whitespace-line :background ,candy-bg :foreground ,candy-pink)
               (whitespace-newline :foreground ,candy-comment)
               (whitespace-space :background ,candy-bg :foreground ,candy-comment)
               (whitespace-space-after-tab :background ,candy-orange :foreground ,candy-red)
               (whitespace-space-before-tab :background ,candy-orange :foreground ,candy-red)
               (whitespace-tab :background ,bg2 :foreground ,candy-comment)
               (whitespace-trailing :inherit trailing-whitespace)
               ;; yard-mode
               (yard-tag-face :inherit ,font-lock-builtin-face)
               (yard-directive-face :inherit ,font-lock-builtin-face))))

  (apply #'custom-theme-set-faces
         'candy
         (let ((color-names (mapcar #'car colors))
               (graphic-colors (mapcar #'cadr colors))
               (term-colors (mapcar #'car (mapcar #'cddr colors)))
               (tty-colors (mapcar #'car (mapcar #'last colors)))
               (expand-for-kind
                (lambda (kind spec)
                  (when (and (string= (symbol-name kind) "term-colors")
                             candy-use-24-bit-colors-on-256-colors-terms)
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

(provide-theme 'candy)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; candy-theme.el ends here

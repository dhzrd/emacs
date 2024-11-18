;;; Startup

(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

;; Minimize directory clutter
(use-package no-littering)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory)) ; keep customizations out of init file

;; Enable package repos
(setopt package-archives
	'(("gnu" . "https://elpa.gnu.org/packages/")
	  ("nongnu" . "https://elpa.nongnu.org/nongnu/")
	  ("melpa" . "https://melpa.org/packages/")))

(use-package minions
  :ensure t
  :config 
  (minions-mode 1))


(use-package org
  :delight org-mode "org-mode")

;;; MacOS modkeys
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
	mac-left-option-modifier 'super
	mac-function-modifier 'hyper
	mac-right-option-modifier nil))

(use-package recentf
  :config
  ;; (setq recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
  ;;                         "[0-9a-f]\\{32\\}-[0-9a-f]\\{32\\}\\.org"
  ;;                         ".*png$" ".*cache$"))
  (setq recentf-max-saved-items 500))

;;; Colors

;; (use-package modus-themes
;;   :ensure t
;;   :config
;;   ;; Add all your customizations prior to loading the themes

;;   ;; Maybe define some palette overrides, such as by using our presets
;;   (setq modus-themes-common-palette-overrides
;;         modus-themes-preset-overrides-warmer)

;;   ;; Load the theme of your choice.
;;   (load-theme 'modus-operandi-tinted)

;;   (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)                ;; orig. previous-matching-history-element
	 ;; Other commands
	 ("C-x C-r" . consult-recent-file)) ;; orig. find-file-read-only

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

(use-package ace-window
  :ensure t
  :bind ("M-o". ace-window))

(use-package zoom-window
  :ensure t
  :bind ("C-\\" . zoom-window-zoom))	; originally toggleinput-method

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package vertico
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
	 ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "◀── now ─────────────────────────────────────────────────")

;; Ellipsis styling
;; (setq org-ellipsis "…")
;; (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)


(with-eval-after-load 'org (global-org-modern-mode))

;; org-appear conflicts with org-modern
;; (use-package org-appear
;;   :ensure t
;;   :hook (org-mode . org-appear-mode))

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :font "Akkurat Mono"))

(global-visual-line-mode 1)

(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x C-g" . magit-status)))

(use-package emacs
  :config
  (defun prot/toggle-line-numbers ()
    "Toggles the display of line numbers.  Applies to all buffers."
    (interactive)
    (if (bound-and-true-p display-line-numbers-mode)
	(display-line-numbers-mode -1)
      (display-line-numbers-mode)))
  :bind (("C-z" . prot/toggle-line-numbers)))

;; (use-package simple-modeline
;;   :hook (after-init . simple-modeline-mode)
;;   :custom
;;   (simple-modeline-segments
;;    '((simple-modeline-segment-modified
;;       simple-modeline-segment-buffer-name)
;;      (simple-modeline-segment-minor-modes
;;       simple-modeline-segment-vc
;;       simple-modeline-segment-misc-info
;;       simple-modeline-segment-process
;;       simple-modeline-segment-major-mode)))
;;   :custom-face
;;   (simple-modeline-space ((t (:box nil)))))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package transient)

(use-package py-vterm-interaction
  :hook (python-mode . py-vterm-interaction-mode)
  :config
  ;;; Suggested:
  ;; (setq-default py-vterm-interaction-repl-program "ipython")
  ;; (setq-default py-vterm-interaction-silent-cells t)
  )

(use-package casual-dired
  :commands casual-dired-tmenu
  :bind (:map dired-mode-map ("C-o" . #'casual-dired-tmenu)))

;; (use-package tabspaces
;;   :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup. 
;;   :commands (tabspaces-switch-or-create-workspace
;; 	     tabspaces-open-or-create-project-and-workspace)
;;   :custom
;;   (tabspaces-use-filtered-buffers-as-default t)
;;   (tabspaces-default-tab "Default")
;;   (tabspaces-remove-to-default t)
;;   (tabspaces-include-buffers '("*scratch*"))
;;   (tabspaces-initialize-project-with-todo t)
;;   (tabspaces-todo-file-name "project-todo.org")
;;   ;; sessions
;;   (tabspaces-session t)
;;   (tabspaces-session-auto-restore t))

(setq tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
(setq tab-bar-close-button-show nil)       ;; hide tab close / X button
;; (setq tab-bar-new-tab-choice "*scratch*");; buffer to show in new tabs
(setq tab-bar-tab-hints nil)
;; show tab numbers
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
;; elements to include in bar

;; (use-package vim-tab-bar
;;   :ensure t
;;   :commands vim-tab-bar-mode
;;   :hook
;;   (after-init . vim-tab-bar-mode))

(use-package activities
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))

;; (use-package goggles
;;   :hook ((prog-mode text-mode) . goggles-mode)
;;   :config
;;   (setq-default goggles-pulse t)) ;; set to nil to disable pulsing

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package denote
  :ensure t
  :bind (("C-c n n" . denote)
         ("C-c n s" . denote-subdirectory)
         ("C-c n i" . denote-link)
         ("C-c n l" . denote-link-find-file)
         ("C-c n b" . denote-link-backlinks))
  :hook
  (dired-mode . denote-dired-mode)
  :custom
  (denote-directory "~/notes/"))

(use-package consult-denote
  :ensure t
  :bind (("C-c n f" . consult-denote-find)
	 ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

;; (use-package consult-notes
;;   :ensure t
;;   :commands (consult-notes
;;              consult-notes-search-in-all-notes)
;;   :bind ("C-c n f" . consult-notes)
;;   :config
;;   (consult-notes-denote-mode))


(use-package ultra-scroll-mac
  :if (eq window-system 'mac)
  ;:load-path "~/code/emacs/ultra-scroll-mac" ; if you git clone'd instead of package-vc-install
  :init
  (setq scroll-conservatively 101 ; important!
	scroll-margin 0) 
  :config
  (ultra-scroll-mac-mode 1))

;; (use-package mood-line

;;   ;; Enable mood-line
;;   :config
;;   (mood-line-mode)

;;   ;; Use pretty Fira Code-compatible glyphs
;;   :custom
;;   (mood-line-glyph-alist mood-line-glyphs-unicode))

;; (set-face-attribute 'mode-line nil
;;                     :box '(:line-width 1 :color "gray20"))

;; (set-face-attribute 'mode-line nil :font "Akkurat Mono")

(use-package geiser-mit 
  :ensure t
  :config
  (setq geiser-racket-binary (executable-find "Racket")))

(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo))

(use-package ace-link
  :ensure t
  :config
  (ace-link-setup-default))

(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package avy
  :ensure t
  :bind 
  ("C-'" . avy-goto-char))

(use-package tempel
  :ensure t
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
)

;; mode-line-format is a variable defined in ‘C source code’.

;; Its value is shown below.

;; Template for displaying mode line for a window’s buffer.

;; The value may be nil, a string, a symbol or a list.

;; A value of nil means don’t display a mode line.

;; For any symbol other than t or nil, the symbol’s value is processed as
;;  a mode line construct.  As a special exception, if that value is a
;;  string, the string is processed verbatim, without handling any
;;  %-constructs (see below).  Also, unless the symbol has a non-nil
;;  ‘risky-local-variable’ property, all properties in any strings, as
;;  well as all :eval and :propertize forms in the value, are ignored.

;; When the value is processed, the window’s buffer is temporarily the
;; current buffer.

;; A list whose car is a string or list is processed by processing each
;;  of the list elements recursively, as separate mode line constructs,
;;  and concatenating the results.

;; A list of the form ‘(:eval FORM)’ is processed by evaluating FORM and
;;  using the result as a mode line construct.  Be careful--FORM should
;;  not load any files, because that can cause an infinite recursion.

;; A list of the form ‘(:propertize ELT PROPS...)’ is processed by
;;  processing ELT as the mode line construct, and adding the text
;;  properties PROPS to the result.

;; A list whose car is a symbol is processed by examining the symbol’s
;;  value, and, if that value is non-nil, processing the cadr of the list
;;  recursively; and if that value is nil, processing the caddr of the
;;  list recursively.

;; A list whose car is an integer is processed by processing the cadr of
;;  the list, and padding (if the number is positive) or truncating (if
;;  negative) to the width specified by that number.

;; A string is printed verbatim in the mode line except for %-constructs:
;;   %b -- print buffer name.      %f -- print visited file name.
;;   %F -- print frame name.
;;   %* -- print %, * or hyphen.   %+ -- print *, % or hyphen.
;; 	%& is like %*, but ignore read-only-ness.
;; 	% means buffer is read-only and * means it is modified.
;; 	For a modified read-only buffer, %* gives % and %+ gives *.
;;   %s -- print process status.   %l -- print the current line number.
;;   %c -- print the current column number (this makes editing slower).
;;         Columns are numbered starting from the left margin, and the
;;         leftmost column is displayed as zero.
;;         To make the column number update correctly in all cases,
;; 	‘column-number-mode’ must be non-nil.
;;   %C -- Like %c, but the leftmost column is displayed as one.
;;   %i -- print the size of the buffer.
;;   %I -- like %i, but use k, M, G, etc., to abbreviate.
;;   %o -- print percent of window travel through buffer, or Top, Bot or All.
;;   %p -- print percent of buffer above top of window, or Top, Bot or All.
;;   %P -- print percent of buffer above bottom of window, perhaps plus Top,
;;         or print Bottom or All.
;;   %q -- print percent of buffer above both the top and the bottom of the
;;         window, separated by ‘-’, or ‘All’.
;;   %n -- print Narrow if appropriate.
;;   %z -- print mnemonics of keyboard, terminal, and buffer coding systems.
;;   %Z -- like %z, but including the end-of-line format.
;;   %e -- print error message about full memory.
;;   %@ -- print @ or hyphen.  @ means that default-directory is on a
;;         remote machine.
;;   %[ -- print one [ for each recursive editing level.  %] similar.
;;   %% -- print %.   %- -- print infinitely many dashes.
;; Decimal digits after the % specify field width to which to pad.

;;   Automatically becomes buffer-local when set.
;;   Calls these functions when changed: (#<subr set-buffer-redisplay>)
;;   This variable may be risky if used as a file-local variable.
;;   You can customize this variable.
;;   Probably introduced at or before Emacs version 18.

;; Value:
;; ("%e" mode-line-front-space
;;  (:propertize
;;   ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
;;   display
;;   (min-width
;;    (5.0)))
;;  mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
;;  (vc-mode vc-mode)
;;  "  " minions-mode-line-modes mode-line-misc-info mode-line-end-spaces)
;; Original value was 
;; ("%e" mode-line-front-space
;;  (:propertize
;;   ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
;;   display
;;   (min-width
;;    (5.0)))
;;  mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
;;  (vc-mode vc-mode)
;;  "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)

(use-package tempel-collection
  :ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a75aff58f0d5bbf230e5d1a02169ac2fbf45c930f816f3a21563304d5140d245" "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c" "c8863bf29722cf36c5d293ac723fe9698e16f778326d791516dd717951ea8cd6" "7bf34d114ec815e05a1ecb7f1acfd61ef453bfd27d12cc4c2babfa08ca1314da" "24b6ade0e3cabdfee9fa487961b089d059e048d77fe13137ea4788c1b62bd99d" "013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100" "2e7dc2838b7941ab9cabaa3b6793286e5134f583c04bde2fba2f4e20f2617cf7" "29a073e66535bad18e11e9bcaa17d7f2d17e4c79f01023e59e9841633915c232" "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" "6a5584ee8de384f2d8b1a1c30ed5b8af1d00adcbdcd70ba1967898c265878acf" "2b501400e19b1dd09d8b3708cefcb5227fda580754051a24e8abf3aff0601f87" "d41229b2ff1e9929d0ea3b4fde9ed4c1e0775993df9d998a3cdf37f2358d386b" "712dda0818312c175a60d94ba676b404fc815f8c7e6c080c9b4061596c60a1db" "cfb9139155392ffe48beba2f76e8b9a93a79729a3cd18feac428746c05f2dbd0" "b58b5aa5664a927866daa481ae5f0795423ed3982ce5f64e56c4106261dbd13e" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "f1c8202c772d1de83eda4765fe21429a528a4fb350a28394d3705fe9678ed1f9" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "c32fbcb7c68d9a3cddf5e213e58afc9c29c55ff3835d10562280e4a690292590" "1114c56feb07fb44ad12ede602c566a1387aeffcf5990a446952d54fba620be3" "b350d78e608ff87218a78f62c9832e1710714c7279321fa72a3da889bfe3d408" "af238e93bc03da7ee4b2d30f2b3ea6e1553eb05b7d827da83bf35be1f6401992" "fbf73690320aa26f8daffdd1210ef234ed1b0c59f3d001f342b9c0bbf49f531c" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" default))
 '(package-selected-packages
   '(py-vterm-interaction pdf-tools org-appear consult-denote popper tempel-collection vundo denote ace-window avy link-hint vim-tab-bar geiser-racket geiser-mit org-modern catppuccin-theme doom-themes ef-themes modus-themes activities corfu orderless vertico no-littering zoom-window base16-theme acme-theme solarized-theme dracula-theme embark embark-consult consult marginalia mood-line stimmung-themes ct ultra-scroll-mac))
 '(package-vc-selected-packages
   '((ultra-scroll-mac :vc-backend Git :url "https://github.com/jdtsmith/ultra-scroll-mac"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

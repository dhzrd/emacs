(use-package no-littering
  :init
  (eval-and-compile ; Ensure values don't differ at compile time.
    (setq no-littering-etc-directory
          (expand-file-name "etc/" user-emacs-directory))
    (setq no-littering-var-directory
          (expand-file-name "var/" user-emacs-directory)))
  :config
  (no-littering-theme-backups)
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

(setopt package-archives
	'(("gnu" . "https://elpa.gnu.org/packages/")
	  ("nongnu" . "https://elpa.nongnu.org/nongnu/")
	  ("melpa" . "https://melpa.org/packages/")))

(set-face-attribute 'default nil :font "Input Mono")
(set-face-attribute 'fixed-pitch nil :font "Input Mono" :height 1.0)
(set-face-attribute 'variable-pitch nil :font "Input Sans" :height 1.0)
(global-visual-line-mode 1)

(use-package mixed-pitch
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode))

(use-package org
  :delight org-mode "org-mode"		; define mode-line lighter
  :custom
  (org-startup-indented t)
  (org-ellipsis " …")
  (set-face-underline 'org-ellipsis nil) ; remove underline from custom org-ellipsis
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  :config

  ;; Rebind some commands in org-mode so as to free up some key sequences (that are elsewhere defined)

  (unbind-key "C-j" org-mode-map)	; originally org-return-and-maybe-indent
  (bind-key "C-m" 'org-return-and-maybe-indent org-mode-map) ; originally (org-return) which is also available at RET

  (unbind-key "C-," org-mode-map)	; originally org-cycle-agenda-files
  (unbind-key "C-'" org-mode-map)	; originally org-cycle-agenda-files
  (bind-key "C-<" 'org-cycle-agenda-files org-mode-map))

(use-package org-modern
  :ensure t
  :custom
  (org-modern-hide-stars nil)		; adds extra indentation
  (org-modern-block-name '("" . ""))
  (org-modern-fold-stars
   '(("►" . "▼") ("►" . "▼") ("►" . "▼") ("►" . "▼") ("►" . "▼")))
  :hook
  (org-mode . org-modern-mode))

(use-package org-modern-indent
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package ns-win
  :when (eq system-type 'darwin)
  :init
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'hyper
        mac-right-option-modifier nil))

(use-package free-keys
  :custom
  (free-keys-modifiers '("" "C" "M" "C-M" "s" "H")))

(use-package recentf
  :config
  ;; (setq recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
  ;;                         "[0-9a-f]\\{32\\}-[0-9a-f]\\{32\\}\\.org"
  ;;                         ".*png$" ".*cache$"))
  (setq recentf-max-saved-items 500))

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico
  :hook ((after-init . vertico-mode)
         (vertico-mode . vertico-multiform-mode)))

(use-package consult
  :ensure t
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


  :hook
  ;; Enable automatic preview at point in the *Completions* buffer.
  (completion-list-mode . consult-preview-at-point-mode)

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
  (setq consult-narrow-key "<"))

(use-package embark
  :ensure t
  :after vertico
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)	; originally x-ref-goto-xref
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :custom
  ;; replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package transient)

;; Configure Tempel
(use-package tempel
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

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection)

;; from http://whattheemacsd.com/editing-defuns.el-02.html
(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))
(bind-key "C-s-<down>" 'move-line-down)

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))
(bind-key "C-s-<up>" 'move-line-up)

(use-package avy
  :ensure t
  :bind 
  ("C-j" . avy-goto-char))		; originally (eval-print-last-sexp)

(use-package ace-link
  :ensure t
  :config
  (ace-link-setup-default))

(use-package ace-window
  :ensure t
  :bind ("M-o". ace-window))

(use-package zoom-window
  :ensure t
  :bind ("C-\\" . zoom-window-zoom))	; originally toggleinput-method

(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

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

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; (setq tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
(customize-set-variable 'tab-bar-show nil) ; on customize-set-variable see https://emacs.stackexchange.com/a/106
;; (setq tab-bar-close-button-show nil)       ;; hide tab close / X button
;; (setq tab-bar-tab-hints nil) ;; show tab numbers
;; (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)) ;; elements to include in bar

(use-package minions
  :ensure t
  :config 
  (minions-mode 1))

(use-package mode-line-bell
  :init (mode-line-bell-mode))

(use-package activities
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)
  :config
  (bind-keys :prefix-map my-activities-map
             :prefix "s-a"
             :prefix-docstring "Keymap for activities-mode"
             ("s-n" . activities-new)
             ("s-d" . activities-define)
             ("s-a" . activities-resume)
             ("s-s" . activities-suspend)
             ("s-k" . activities-kill)
             ("RET" . activities-switch)
             ("b" . activities-switch-buffer)
             ("g" . activities-revert)
             ("l" . activities-list)))

(use-package popper
  :ensure t
  :bind (("s-`"   . popper-toggle)
         ("s-1"   . popper-cycle)
         ("s-2" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(setq dired-dwim-target t)

(use-package dired+)

(use-package casual-dired
  :commands casual-dired-tmenu
  :bind (:map dired-mode-map ("C-z d h" . #'casual-dired-tmenu)))

(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x C-g" . magit-status)))

(use-package geiser-mit 
  :ensure t
  :config
  (setq geiser-racket-binary (executable-find "Racket")))

;; (use-package ultra-scroll-mac
;;   :if (eq window-system 'mac)
;;   ;:load-path "~/code/emacs/ultra-scroll-mac" ; if you git clone'd instead of package-vc-install
;;   :init
;;   (setq scroll-conservatively 101 ; important!
;;         scroll-margin 0) 
;;   :config
;;   (ultra-scroll-mac-mode 1))



;; (use-package py-vterm-interaction
;;   :hook (python-mode . py-vterm-interaction-mode)
;;   :config
;;   ;;; Suggested:
;;   ;; (setq-default py-vterm-interaction-repl-program "ipython")
;;   ;; (setq-default py-vterm-interaction-silent-cells t)
;;   )

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-langs-install-grammars :skip-if-installed))

(use-package treesit-auto
  :after tree-sitter
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package python
  :mode ("\\.py\\'" . python-ts-mode)
  ;; :hook (python-ts-mode (lambda () (run-hooks 'python-mode-hook)))
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  (python-shell-completion-native-enable nil)) ; see https://emacs.stackexchange.com/questions/30082/your-python-shell-interpreter-doesn-t-seem-to-support-readline

(use-package pdf-tools
 :pin manual ;; manually update
 :mode  ("\\.pdf\\'" . pdf-view-mode)
 :config
 ;; initialise
 (pdf-tools-install :no-query)
 ;; open pdfs scaled to fit page
 (setq-default pdf-view-display-size 'fit-width)
 ;; automatically annotate highlights
 (setq pdf-annot-activate-created-annotations t)
 ;; use normal isearch
 (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package info-variable-pitch
  :ensure t
  :hook (info-mode . info-variable-pitch-mode))

(use-package olivetti
  :ensure t)

(use-package buffer-flip
  :ensure t
  :bind  (("M-\\" . buffer-flip)	; originally delete-horizontal-space
          :map buffer-flip-map
          ( "M-\\" .   buffer-flip-forward) 
          ( "M-|" . buffer-flip-backward) ; originally shell-command-on-region
          ( "M-ESC" .     buffer-flip-abort))
  ;; :config
  ;; (setq buffer-flip-skip-patterns
  ;;       '("^\\*helm\\b"
  ;;         "^\\*swiper\\*$"))
  )

(use-package fwb-cmds
  :ensure t
  :bind ("s-\\" . fwb-toggle-window-split))

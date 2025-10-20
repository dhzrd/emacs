;;; init.el --- My Emacs Init File -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
;;; Some preferences

(use-package no-littering
  :init
  (eval-and-compile	   ; Ensure values don't differ at compile time.
    (setq no-littering-etc-directory
          (expand-file-name "etc/" user-emacs-directory))
    (setq no-littering-var-directory
          (expand-file-name "var/" user-emacs-directory)))
  :config
  (no-littering-theme-backups)
  (setq custom-file
        (expand-file-name "custom.el" user-emacs-directory)))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package package
  :ensure nil
  :config
  (add-to-list 'package-archives ; the default list contains ELPA and non-GNU ELPA
               '("melpa" . "https://melpa.org/packages/") t))

(setopt backup-by-copying t
        comment-multi-line t
        confirm-kill-emacs 'yes-or-no-p
        create-lockfiles nil
        delete-old-versions t
        fill-column 80
        help-window-select t
        recentf-max-saved-items 500
        ring-bell-function 'ignore
        savehist-mode t
        sentence-end-double-space nil
        tab-always-indent 'complete
        uniquify-buffer-name-style 'forward
        use-dialog-box nil
        use-file-dialog nil
        use-short-answers t
        vc-follow-symlinks t
        version-control t)

;;;; GUI

(use-package faces
  :ensure nil
  :config
  (when (member "Input Mono Narrow" (font-family-list))
    (set-face-attribute 'default nil :font "Input Mono Narrow" :height 140)
    (set-face-attribute 'fixed-pitch nil :family "Input Sans Narrow"))
  ;; (when (member "JetBrains Mono" (font-family-list))
  ;;   (set-face-attribute 'default nil :font "JetBrains Mono" :height 130)
  ;;   (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono"))
    )

(use-package minions
  :custom
  (minions-mode-line-delimiters nil)
  :config
  (minions-mode 1))

(use-package mood-line
  :after minions
  :preface
  (defun my/mood-line-segment-major-mode ()
    "Return the name of the major mode of the current buffer."
    (concat (format-mode-line minions-mode-line-modes 'mood-line-major-mode) ""))
  :custom
  (mood-line-glyph-alist mood-line-glyphs-unicode)
  (mood-line-format
   (mood-line-defformat
    :left
    (((mood-line-segment-buffer-status) . " ")
     ((mood-line-segment-buffer-name)   . " : ")
     (my/mood-line-segment-major-mode)
     (mood-line-segment-vc)         . "  ")
    :right
    (((mood-line-segment-scroll)             . " ")
     ((mood-line-segment-cursor-position)    . "  ")
     ((when (mood-line-segment-checker) "|") . "  ")
     ((mood-line-segment-checker)            . "  ")))
   :config
   (mood-line-mode)))

(use-package mixed-pitch
  :disabled
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode))

;; (use-package tab-bar
;;   :custom ((tab-bar-auto-width nil)
;;            (tab-bar-select-tab-modifiers '(super))
;;            (tab-bar-close-button-show nil)))


;; (setopt tab-bar-show nil) ; on customize-set-variable see https://emacs.stackexchange.com/a/106
;; (setq tab-bar-close-button-show nil)       ;; hide tab close / X button
;; (setq tab-bar-tab-hints nil) ;; show tab numbers
;; (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)) ;; elements to include in bar

;;; Keybindings

(use-package emacs
  :bind
  (:map global-map

	("s-a" . beginning-of-buffer)
	("s-e" . end-of-buffer)

	;; Control-z
        :prefix-map my-ctrl-z-prefix-map
        :prefix "C-z"
	
	;; Super-f prefix map: frame cmds
	:prefix-map my-super-f-prefix-map
	:prefix "s-f"
	:prefix-docstring
        "Prefix map for frame commands."
	("s-f" . make-frame-command)

	;; Super-v prefix map: narrowing cmds
	:prefix-map my-global-narrow-prefix-map
        :prefix "s-v"
        :prefix-docstring
        "Prefix map for narrowing commands."
        ("n" . narrow-to-region)
        ("d" . narrow-to-defun)
        ("p" . narrow-to-page)
        ("w" . widen)

	;; Super-x prefix map
	:prefix-map my-super-x-prefix-map
        :prefix "s-x"

	;; Super-z prefix map
	:prefix-map my-super-z-prefix-map
        :prefix "s-z"))

(use-package free-keys
  :ensure t
  :custom
  (free-keys-modifiers '("" "C" "M" "C-M" "s" "H")))

(use-package ns-win
  :when (eq system-type 'darwin)
  :init
  (setopt mac-command-modifier 'super
          mac-option-modifier 'meta
          mac-right-option-modifier 'nil
          mac-right-control-modifier 'hyper))

;;; Editing and navigating

(bind-keys :prefix-map my-hyper-x-prefix-map
           :prefix "H-x"
           ("f" . write-region))

(use-package ace-link
  :ensure t
  :config
  (ace-link-setup-default))

(use-package avy
  :ensure t
  :preface

  ;; Define new actions to be called from avy-dispatch-alist.

  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (defun avy-action-kill-line (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (kill-line))
      (avy-resume))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-kill-whole-line (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (kill-whole-line)
          (avy-resume)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  :custom
  ;; When only one candidate is found, DON'T jump to it automatically.
  (avy-single-candidate-jump nil)
  (avy-dispatch-alist '((?. . avy-action-embark)
                        (?= . avy-action-define)
                        (?M . avy-action-mark-to-char)
                        (?k . avy-action-kill-line)
                        (?W . avy-action-copy-whole-line)
                        (?K . avy-action-kill-whole-line)
                        (?Y . avy-action-yank-whole-line)
                        (?T . avy-action-teleport-whole-line)
                        ;; Default alist below
                        (?x . avy-action-kill-move)
                        (?X . avy-action-kill-stay)
                        (?t . avy-action-teleport)
                        (?m . avy-action-mark)
                        (?w . avy-action-copy)
                        (?y . avy-action-yank)
                        (?Y . avy-action-yank-line)
                        (?i . avy-action-ispell)
                        (?z . avy-action-zap-to-char)))
  :bind
  ("s-j" . avy-goto-char-timer))	; †exchange-point-and-mark

(use-package avy-zap
  :after avy
  :ensure t
  :bind
  ([remap zap-up-to-char] . avy-zap-up-to-char-dwim)
  ([remap zap-to-char] . avy-zap-to-char-dwim))

(use-package comment-dwim-2
  :ensure t
  :demand
  :bind
  ("s-;" . comment-dwim-2))

(use-package display-fill-column-indicator
  :ensure nil
  :custom (display-fill-column-indicator-character nil)
  :custom-face (fill-column-indicator
                ((t (:height 1.0 :foreground "grey"))))
  :hook (prog-mode . display-fill-column-indicator-mode))

(use-package expand-region
  :ensure t
  :bind
  ("s-m" . er/expand-region)
  ("s-M" . er/contract-region))

(use-package goto-chg
  :ensure t
  :bind ("s-q" . goto-last-change))

(use-package move-text
  :ensure t
  :bind (("s-p" . move-text-up)
         ("s-n" . move-text-down)))

(use-package misc
  :ensure nil
  :bind
  ;; Rebind 'M-z' as 'zap-up-to-char'. 'zap-to-char' is still available
  ;; at 'M-Z'. But see 'avy-zap' further below.
  ("M-z" . zap-up-to-char)
  ("s-y" . duplicate-dwim))		; †ns-paste-secondary

(use-package outli
  :ensure t
  :bind (:map outli-mode-map
	      ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading))))
  :hook ((prog-mode text-mode) . outli-mode))

(use-package ultra-scroll
  ;; :ensure t
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(use-package vundo			; better undo-tree
  :ensure t
  :bind (("C-x u" . vundo)
         ("H-/" . vundo)))

(use-package ws-butler
  :ensure t
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;;;; Completion

(use-package vertico
  :hook ((after-init . vertico-mode)
         (vertico-mode . vertico-multiform-mode)))

(use-package consult
  :ensure t
  :bind
  (;; C-c bindings in `mode-specific-map'
   ("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ([remap Info-search] . consult-info)
   ;; C-x bindings in `ctl-x-map'
   ("C-x M-:" . consult-complex-command) ;; †repeat-complex-command
   ("C-x b" . consult-buffer)		 ;; †switch-to-buffer
   ("s-b" . consult-buffer)		 ;; †switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; †switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame) ;; †switch-to-buffer-other-frame
   ("C-x t b" . consult-buffer-other-tab) ;; †switch-to-buffer-other-tab
   ("C-x r b" . consult-bookmark)	  ;; †bookmark-jump
   ("C-x p b" . consult-project-buffer)	  ;; †project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store) ;; †abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-y" . consult-yank-pop) ;; †yank-pop
   ;; M-g bindings in `goto-map'
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)
   ("M-g g" . consult-goto-line)	 ;; †goto-line
   ("M-g M-g" . consult-goto-line)	 ;; †goto-line
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;; M-s bindings in `search-map'
   ("M-s d" . consult-find)
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
   ("M-e" . consult-isearch-history)         ;; †isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; †isearch-edit-string
   ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)	   ;; †next-matching-history-element
   ("M-r" . consult-history)	   ;; †previous-matching-history-element
   ;; Other commands
   ("C-x C-r" . consult-recent-file)) ;; †find-file-read-only

  :hook
  ;; Enable automatic preview at point in the *Completions* buffer.
  (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode-line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))	; :preview-key "M-."
  (setq consult-narrow-key "<"))

(use-package embark
  :ensure t
  :bind (("s-," . embark-act)		; †customize
         ("s-." . embark-dwim)
         ("C-h B" . embark-bindings))

  :config
  ;; Hide mode-line in embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after embark
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom


  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package marginalia
  :ensure t
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package which-key
  :ensure nil 				; must be nil on Emacs 30+
  :config
  (which-key-mode))


;;;;; Expansion

(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand))

(use-package tempel
  :disabled
  :ensure t
  :preface
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
  :custom
  (tempel-trigger-prefix ",")
  (tempel-path (locate-user-emacs-file "etc/templates/tempo.eld"))
  :bind (("s-\\" . tempel-complete)
         ("s-|" . tempel-insert)
         :map tempel-map
         ([tab] . tempel-next)
         ([backtab] . tempel-previous)))

(use-package tempel-collection
  :disabled
  :ensure t
  :after tempel)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  :bind ("H-s" . yas-insert-snippet))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :pin gnu)

;;; Managing frames, windows, and buffers

(use-package ace-window
  :ensure t
  :demand
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; (use-package activities
;;   :disabled
;;   :init
;;   (activities-mode)
;;   (activities-tabs-mode)
;;   :bind
;;   ("H-b" . activities-switch-buffer)
;;   ("H-/" . activities-revert)
;;   ("H-z" . activities-switch)
;;   (:map global-map
;;         :prefix-map my-activities-prefix-map
;;         :prefix "H-a"
;;         :prefix-docstring
;;         "Prefix keymap for activities-mode."
;;         ("n" . activities-new)
;;         ("d" . activities-define)
;;         ("a" . activities-resume)
;;         ("s" . activities-suspend)
;;         ("k" . activities-kill)
;;         ("RET" . activities-switch)
;;         ("b" . activities-switch-buffer)
;;         ("g" . activities-revert)
;;         ("l" . activities-list)))

(use-package bufferlo
  :ensure t
  :demand t
  ;; :after (ibuffer consult)
  :init
  (defvar bufferlo-command-map (make-sparse-keymap)
    "Keymap for bufferlo commands.")
  :config
  (defvar my:bufferlo-consult--source-local-buffers
    (list :name "Bufferlo Local Buffers"
          :narrow   ?l
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
  				:predicate #'bufferlo-local-buffer-p
  				:sort 'visibility
  				:as #'buffer-name)))
    "Local Bufferlo buffer candidate source for `consult-buffer'.")

  (defvar my:bufferlo-consult--source-other-buffers
    (list :name "Bufferlo Other Buffers"
          :narrow   ?b
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :items    (lambda () (consult--buffer-query
  				:predicate #'bufferlo-non-local-buffer-p
  				:sort 'visibility
  				:as #'buffer-name)))
    "Non-local Bufferlo buffer candidate source for `consult-buffer'.")

  ;; add in the reverse order of display preference
  (add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-other-buffers)
  (add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-local-buffers)
  (bufferlo-mode)
  (bufferlo-anywhere-mode)
  :custom
  (bufferlo-bookmark-frame-load-make-frame t) ; default is nil for backward compatibility
  (bufferlo-bookmark-frame-load-make-frame 'restore-geometry)
  ;; (bufferlo-bookmarks-auto-save-interval 120)
  :bind-keymap ("s-b" . bufferlo-command-map)
  :bind (:map bufferlo-command-map
  	      ;; buffer / ibuffer
  	      ("b" . bufferlo-switch-to-buffer)
  	      ("s-b" . bufferlo-ibuffer)
  	      ("o" . bufferlo-ibuffer-orphans)
  	      ("-" . bufferlo-remove)
  	      ;; general bookmark (interactive)
  	      ("m l" . bufferlo-bms-load)
  	      ("m s" . bufferlo-bms-save)
  	      ("m c" . bufferlo-bms-close)
  	      ("m r" . bufferlo-bm-raise)
  	      ;; dwim frame or tab bookmarks
  	      ("s-s" . bufferlo-bm-save)
  	      ("s-l" . bufferlo-bm-load)
  	      ("s-0" . bufferlo-bm-close)
  	      ;; tabs
  	      ("t s" . bufferlo-bm-tab-save)               ; save
  	      ("t u" . bufferlo-bm-tab-save-curr)          ; update
  	      ("t l" . bufferlo-bm-tab-load)               ; load
  	      ("t r" . bufferlo-bm-tab-load-curr)          ; reload
  	      ("t 0" . bufferlo-bm-tab-close-curr)         ; kill
  	      ;; frames
  	      ("f s" . bufferlo-bm-frame-save)             ; save
  	      ("f u" . bufferlo-bm-frame-save-curr)        ; update
  	      ("f l" . bufferlo-bm-frame-load)             ; load
  	      ("f r" . bufferlo-bm-frame-load-curr)        ; reload
  	      ("f m" . bufferlo-bm-frame-load-merge)       ; merge
  	      ("f 0" . bufferlo-bm-frame-close-curr)       ; kill
  	      ;; sets
  	      ("s s" . bufferlo-set-save)                  ; save
  	      ("s u" . bufferlo-set-save-curr)             ; update
  	      ("s +" . bufferlo-set-add)                   ; add bookmark
  	      ("s =" . bufferlo-set-add)                   ; add bookmark
  	      ("s -" . bufferlo-set-remove)                ; remove bookmark
  	      ("s l" . bufferlo-set-load)                  ; load
  	      ("s 0" . bufferlo-set-close)                 ; kill
  	      ("s c" . bufferlo-set-clear)                 ; clear
  	      ("s L" . bufferlo-set-list)                  ; list contents of selected active sets
  	      ))

(use-package fwb-cmds
  :ensure t
  :bind ("C-x C-1" . fwb-toggle-window-split)) ; †next-window-any-frame

(use-package ibuffer
  :ensure nil
  :bind ([remap list-buffers] . ibuffer)) ; C-x C-b

;; (use-package ibuffer-vc
;;   :after ibuffer)

(use-package popper
  :ensure t
  :bind (("H-`"   . popper-toggle)
         ("H-<tab>"   . popper-cycle)
         (:map popper-mode-map
               ("H-1" . popper-toggle-type)))
  :custom
  (popper-echo-mode t)
  ;; specify buffer types for popper to control
  (popper-reference-buffers '("Output\\*$"
                              "\\*Async Shell Command\\*"
                              help-mode
                              compilation-mode
                              ;; and suppress or hide some of them
                              ("\\*Messages\\*" . hide)
                              ("\\*Warnings\\*" . hide)))
  :hook
  (after-init . popper-mode))

(use-package windmove
  :config
  (windmove-default-keybindings 'hyper)
  ;; (windmove-delete-default-keybindings)
  ;; (windmove-display-default-keybindings)
  (windmove-swap-states-default-keybindings '(hyper shift)))

(use-package winner
  :hook (after-init . winner-mode)
  :bind (("s-/" . winner-undo)
         ("s-?" . winner-redo))
  :custom
  (winner-dont-bind-my-keys t)
  (winner-boring-buffers '("*Completions*"
                           "*Compile-Log*"
                           "*inferior-lisp*"
                           "*Fuzzy Completions*"
                           "*Apropos*"
                           "*Help*"
                           "*Buffer List*"
                           "*Ibuffer*"
                           "*Warnings*"
                           "*Messages*"
                           "*Activities (error)")))

;;; Working with code

(use-package elec-pair
  :ensure nil
  :hook ((js-mode python-mode) . electric-pair-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error) ; optional but recommended error navigation
              ("M-p" . flycheck-previous-error)))

(use-package js-mode
  :ensure nil
  :mode "\\.js\\'")

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package lua-mode
  :ensure t
  :mode "\\.lua$")

(use-package lsp-mode
  :diminish "LSP"
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :none)       ; Using Corfu as the provider
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil) ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil) ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)	 ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)	 ; Use xref to find references
  (lsp-auto-configure t) ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t) ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)	 ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)	    ; I use prettier
  (lsp-enable-links nil)	    ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t) ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)	; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil)  ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)      ; Important to provide full JSX completion
  (lsp-completion-show-kind t)	       ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t) ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)	; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1) ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)		; Show docs for symbol at point
  (lsp-eldoc-render-all nil) ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil) ; Related to highlighting, and we defer to treesitter

  :init
  (setq lsp-use-plists t))

(use-package lsp-completion
  :no-require
  :hook ((lsp-mode . lsp-completion-mode)))

(use-package lsp-eslint
  :demand t
  :after lsp-mode)

(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :config (setq lsp-ui-doc-enable t
                lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point))

(use-package magit
  :ensure t
  :bind ("H-g" . magit-status))

(use-package paredit
  :ensure t
  :demand
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (eval-expression-minibuffer-setup . paredit-mode)))

(use-package python
  :preface
  ;; Define commands to toggle between buffer and shell. From https://www.masteringemacs.org/article/toggling-python-buffers
  (defvar python-last-buffer nil
    "Name of the Python buffer that last invoked `toggle-between-python-buffers'")

  (make-variable-buffer-local 'python-last-buffer)

  (defun toggle-between-python-buffers ()
    "Toggles between a `python-mode' buffer and its inferior Python process

When invoked from a `python-mode' buffer it will switch the
active buffer to its associated Python process. If the command is
invoked from a Python process, it will switch back to the `python-mode' buffer."
    (interactive)
    ;; check if `major-mode' is `python-mode' and if it is, we check if
    ;; the process referenced in `python-buffer' is running
    (if (and (eq major-mode 'python-mode)
             (processp (get-buffer-process python-buffer)))
        (progn
          ;; store a reference to the current *other* buffer; relying
          ;; on `other-buffer' alone wouldn't be wise as it would never work
          ;; if a user were to switch away from the inferior Python
          ;; process to a buffer that isn't our current one.
          (switch-to-buffer python-buffer)
          (setq python-last-buffer (other-buffer)))
      ;; switch back to the last `python-mode' buffer, but only if it
      ;; still exists.
      (when (eq major-mode 'inferior-python-mode)
        (if (buffer-live-p python-last-buffer)
            (switch-to-buffer python-last-buffer)
          ;; buffer's dead; clear the variable.
          (setq python-last-buffer nil)))))

  (unbind-key "s-o")
  :mode ("\\.py\\'" . python-ts-mode)
  ;; :hook (python-ts-mode (lambda () (run-hooks 'python-mode-hook)))
  :custom
  (fill-column 72)
  (python-indent-offset 4)
  (python-shell-completion-native-enable nil) ; see https://emacs.stackexchange.com/questions/30082/your-python-shell-interpreter-doesn-t-seem-to-support-readline

  :bind (:map python-mode-map ("s-o" . toggle-between-python-buffers)
              :map inferior-python-mode-map ("s-o" . toggle-between-python-buffers)))

(use-package py-vterm-interaction
  :hook (python-mode . py-vterm-interaction-mode)
  :custom
  (py-vterm-interaction-repl-program "ipython -i")
  (py-vterm-interaction-silent-cells t))

(use-package racket-mode
  :ensure t
  :mode("\\.rkt?\\'" . racket-mode)
  :custom (racket-program "/Applications/Racket v8.15/bin/racket"))

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ;; More modes defined here...
         )
  :preface
  (defun my/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               ;; (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               ))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (my/setup-install-grammars))









;;;; Webdev

(use-package astro-ts-mode
  :ensure t
  :mode "\\.astro\\'")

(use-package emmet-mode
  :ensure t
  :custom
  (emmet-move-cursor-between-quotes t)
  ;; (emmet-use-css-transform t)		; necessary for some reason for css
  :bind (:map emmet-mode-keymap
              ("M-n"  . emmet-next-edit-point)
              ("M-p"  . emmet-prev-edit-point)
              ("C-c p" . emmet-preview-mode))
  :hook (mhtml-mode css-mode astro-ts-mode))

(use-package nodejs-repl
  :ensure t
  :if (executable-find "node")
  :bind (:map js-mode-map
              ("C-c C-z" . nodejs-repl-switch-to-repl)
              ("C-x C-e" . nodejs-repl-send-last-expression)
              ("C-c C-b" . nodejs-repl-send-buffer)
              ("C-c C-r" . nodejs-repl-send-region))
  :hook ((js-mode js-ts-mode) . nodejs-repl-minor-mode))

(use-package websocket
  :ensure t)

(use-package server
  :ensure nil
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

;;; Org

(use-package org
  :custom
  (org-startup-indented t)
  (org-ellipsis " …")
  ;; Remove underline from org-ellipsis just defined.
  (set-face-underline 'org-ellipsis nil)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  :hook (org-mode . visual-line-mode)
  :bind
  ("H-o t" . org-babel-tangle-file)
  (:map org-mode-map
        (:prefix-map my-org-narrowing-prefix-map
                     :prefix "s-v"
                     :prefix-docstring
                     "Prefix map for narrowing commands in Org-mode."
                     ("s" . org-narrow-to-subtree)
                     ("b" . org-narrow-to-block)
                     ("w" . widen))))

(use-package org-modern
  :after org
  :custom
  (org-modern-hide-stars nil)		; adds extra indentation
  (org-modern-block-name '("" . ""))
  (org-modern-fold-stars
   ;; '(("►" . "▼") ("►" . "▼") ("►" . "▼") ("►" . "▼") ("►" . "▼"))
   ;; Use with Jetbrains Mono font
   '(("▹" . "▿") ("▹" . "▿") ("▹" . "▿") ("▹" . "▿") ("▹" . "▿")))
  :hook
  (org-mode . org-modern-mode))

(use-package org-modern-indent
  :after org-modern
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-tidy
  :disabled
  :hook
  (org-mode . org-tidy-mode))

(use-package org-capture
  :ensure nil
  :custom
  (org-capture-templates
   ''("s" "macOS Safari clipboard capture" entry
      (file+olp+datetree "~/notes/notes-from-safari.org")
      "* %?    :safari:note:\n%U\n\n%i\n")))

;; From Aimé Bertrand's config on Gitlab. See also blog post from 2022:
;; https://macowners.club/posts/org-capture-from-everywhere-macos/

;; bind key to: emacsclient -ne "(timu-org-make-capture-frame)"
;; with a macos Automator.app created Service
(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defun timu-org-make-capture-frame ()
  "Create a new frame and run `org-capture'."
  (interactive)
  (make-frame '((name . "capture")
                (top . 300)
                (left . 700)
                (width . 80)
                (height . 25)))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
          (org-capture)))

;; capture and/or org-yank from macos clipboard
;; credit: http://www.howardism.org/Technical/Emacs/capturing-content.html
;; credit: https://gitlab.com/howardabrams/spacemacs.d/-/tree/master/layers
(defun timu-org-cmd-with-exit-code (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply #'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(defun timu-org-convert-applescript-to-html (contents)
  "Return the Applescript's clipboard CONTENTS in a packed array.
Convert and return this encoding into a UTF-8 string."
  (cl-flet ((hex-pack-bytes (tuple) (string-to-number (apply #'string tuple) 16)))
    (let* ((data (-> contents
                     (substring 10 -2) ; strips off the =«data RTF= and =»\= bits
                     (string-to-list)))
           (byte-seq (->> data
                          (-partition 2)  ; group each two hex characters into tuple
                          (mapcar #'hex-pack-bytes))))
      (decode-coding-string
       (mapconcat #'byte-to-string byte-seq "") 'utf-8))))

(defun timu-org-get-os-clipboard ()
  "Return a list where the first entry is the either :html or :text.
The second is the clipboard contents."
  (if (eq system-type 'darwin)
      (timu-org-get-mac-clipboard)
    (timu-org-get-linux-clipboard)))

(defun timu-org-get-mac-clipboard ()
  "Return the clipbaard for a macOS system.
See `timu-org-get-os-clipboard'."
  (cl-destructuring-bind (exit-code contents)
      (timu-org-cmd-with-exit-code "/usr/bin/osascript" "-e" "the clipboard as \"HTML\"")
    (if (= 0 exit-code)
        (list :html (timu-org-convert-applescript-to-html contents))
      (list :text (shell-command-to-string "/usr/bin/osascript -e 'the clipboard'")))))

(defun timu-org-get-linux-clipboard ()
  "Return the clipbaard for a Linux system.
See `timu-org-get-os-clipboard'."
  (cl-destructuring-bind (exit-code contents)
      (timu-org-cmd-with-exit-code "xclip" "-o" "-sel" "clip" "-t" "text/html")
    (if (= 0 exit-code)
        (list :html contents)
      (list :text (shell-command-to-string "xclip -o")))))

(defun timu-org-clipboard ()
  "Return the contents of the clipboard in `org-mode' format."
  (cl-destructuring-bind (type contents) (timu-org-get-os-clipboard)
    (with-temp-buffer
      (insert contents)
      (if (eq :html type)
          (shell-command-on-region
           (point-min)
           (point-max) (concat (executable-find "pandoc") " -f  -t org --wrap=none") t t)
        (shell-command-on-region
         (point-min)
         (point-max) (concat (executable-find "pandoc") " -f markdown -t org --wrap=none") t t))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun timu-org-yank-clipboard ()
  "Yank the contents of the Mac clipboard in an `org-mode' compatible format."
  (interactive)
  (insert (timu-org-clipboard)))

(defun timu-org-quick-outlook-capture ()
  "Call `org-capture-string' on the currently selected outlook msg."
  (org-capture-string (org-mac-link-outlook-message-get-links) "o")
  (ignore-errors)
  (org-capture-finalize))

(defun timu-org-safari-capture ()
  "Call `org-capture-string' on the contents of the Apple clipboard.
Use `org-mac-link-safari-get-frontmost-url' to capture content from Safari.
Triggered by a custom macOS Quick Action with keybinding."
  (org-capture-string (timu-org-clipboard) "s")
  (ignore-errors)
  (insert (org-mac-link-safari-get-frontmost-url))
  (org-capture-finalize))

(defun timu-org-safari-url-capture ()
  "Call `org-capture-string' on the current front most Safari window.
Use `org-mac-link-safari-get-frontmost-url' to capture url from Safari.
Triggered by a custom macOS Quick Action with a keyboard shortcut."
  (org-capture-string (org-mac-link-safari-get-frontmost-url) "u")
  (ignore-errors)
  (org-capture-finalize)
  (find-file timu-org-notes-file)
  (goto-char (point-max))
  (org-previous-visible-heading 1)
  (timu-org-notes-header-clean)
  (write-file timu-org-notes-file))

(defun timu-org-quick-safari-blog-idea-capture ()
  "Call `org-capture-string' on the current most front Safari window.
Use `org-mac-link-safari-get-frontmost-url' to capture blog ideas from Safari.
Triggered by a custom macOS Quick Action with keybinding."
  (org-capture-string (org-mac-link-safari-get-frontmost-url) "b")
  (ignore-errors)
  (org-capture-finalize))

(defun timu-org-qutebrowser-capture ()
  "Call `org-capture-string' on the contents of the Apple clipboard.
Use `org-mac-link-qutebrowser-get-frontmost-url' to capture qutebrowser content.
Triggered by a custom macOS Quick Action with keybinding."
  (org-capture-string (timu-org-clipboard) "q")
  (ignore-errors)
  (insert (org-mac-link-qutebrowser-get-frontmost-url))
  (org-capture-finalize))

;;; Reading and writing

(use-package consult-denote

  :bind (("C-c n f" . consult-denote-find)
	 ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

(use-package denote

  :bind (("C-c n n" . denote)
	 ("C-c n s" . denote-subdirectory)
	 ("C-c n i" . denote-link)
	 ("C-c n l" . denote-link-find-file)
	 ("C-c n b" . denote-link-backlinks))
  :hook
  (dired-mode . denote-dired-mode)
  :custom
  (denote-directory "~/notes/"))

(use-package graphviz-dot-mode
  :ensure t
  :custom
  (graphviz-dot-indent-width 4)
  (graphviz-dot-preview-extension "svg"))

(use-package info-variable-pitch
  :hook (info-mode . info-variable-pitch-mode))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

(use-package olivetti)

(use-package pdf-tools

  :pin manual ;; manually update
  :mode  ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  ;; :init
  ;; (pdf-loader-install)
  :config
  (pdf-loader-install)
  ;; (pdf-tools-install :no-query)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-width))

(use-package typst-ts-mode
  ;; https://codeberg.org/meow_king/typst-ts-mode
  :custom
  (typst-ts-watch-options "--open")
  (typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory))
  (typst-ts-mode-enable-raw-blocks-highlight t)
  :config
  (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-tmenu)
  ;; The remaining lines are suggested by typst-preview.
  ;; (add-to-list 'lsp-language-id-configuration '(typst-ts-mode . "typst"))
  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-stdio-connection "typst-lsp")
  ;;   :major-modes '(typst-ts-mode)
  ;;   :server-id 'typst-lsp))
  )

(use-package typst-preview
  ;; https://github.com/havarddj/typst-preview.el
  :config
  (setq typst-preview-browser "xwidget")
  (define-key typst-preview-mode-map (kbd "C-c C-j") 'typst-preview-send-position))

;;; Dired
(use-package dired
  :ensure nil
  :custom
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  (load-prefer-newer t))

(use-package dired-aux
  :ensure nil
  :after dired)

(use-package dired-x
  :ensure nil
  :disabled
  :after dired-aux
  :hook
  (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-extensions ".DS_Store")
  (dired-find-subdir t))

(use-package dired-plus
  :ensure t
  :disabled
  :after dired-x)

(provide 'init)

;;; init.el ends here

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

(use-package no-littering
  :init
  (eval-and-compile ; Ensure values don't differ at compile time.
    (setq no-littering-etc-directory
          (expand-file-name "etc/" user-emacs-directory))
    (setq no-littering-var-directory
          (expand-file-name "var/" user-emacs-directory)))
  :config
  (no-littering-theme-backups)
  (setq custom-file
        (expand-file-name "custom.el" user-emacs-directory)))

(use-package package
  :config
  (add-to-list 'package-archives ; the default list contains ELPA and non-GNU ELPA
               '("melpa" . "https://melpa.org/packages/") t))

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

(use-package mode-line-bell
  :init (mode-line-bell-mode))

(set-face-attribute 'default nil :font "Input Mono")
(set-face-attribute 'fixed-pitch nil :font "Input Mono" :height 1.0)
(set-face-attribute 'variable-pitch nil :font "Input Sans" :height 1.0)
(global-visual-line-mode 1)

(use-package mixed-pitch
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode))

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
  (:prefix-map my-org-narrowing-prefix-map
               :prefix "s-n"
               :prefix-docstring
               "Prefix map for narrowing commands in Org-mode."
               ("s" . org-narrow-to-subtree)
               ("b" . org-narrow-to-block)
               ("w" . widen)))

(use-package org-modern
  :after org
  :custom
  (org-modern-hide-stars nil)		; adds extra indentation
  (org-modern-block-name '("" . ""))
  (org-modern-fold-stars
   '(("►" . "▼") ("►" . "▼") ("►" . "▼") ("►" . "▼") ("►" . "▼")))
  :hook
  (org-mode . org-modern-mode))

(use-package org-modern-indent
  :after org-modern
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package ns-win
  :when (eq system-type 'darwin)
  :init
  (setopt mac-command-modifier 'super
          mac-option-modifier 'meta
          mac-right-option-modifier 'nil
          mac-right-control-modifier 'hyper))

(use-package free-keys
  :custom
  (free-keys-modifiers '("" "C" "M" "C-M" "s" "H")))

(use-package emacs
  :bind
  (:map global-map
        :prefix-map my-global-narrow-prefix-map
        :prefix "s-n"
        :prefix-docstring
        "Prefix map for narrowing commands."
        ("n" . narrow-to-region)
        ("d" . narrow-to-defun)
        ("p" . narrow-to-page)
        ("w" . widen)))

(use-package misc
  :ensure nil
  :bind
  ;; Rebind 'M-z' as 'zap-up-to-char'. 'zap-to-char' is still available
  ;; at 'M-Z'. But see 'avy-zap' further below.
  ("M-z" . zap-up-to-char)
  ("s-y" . duplicate-dwim))		; †ns-paste-secondary

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
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often biound to M-TAB.
  (tab-always-indent 'complete)
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
  :ensure t
  :config
  (which-key-mode))

(use-package transient)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand))

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

(bind-key "s-a" 'beginning-of-buffer)
(bind-key "s-e" 'end-of-buffer)

(bind-keys :prefix-map my-hyper-x-prefix-map
           :prefix "H-x"
           ("f" . write-region))

(use-package comment-dwim-2
  :ensure t
  :demand
  :bind
  ("s-;" . comment-dwim-2))

(use-package ws-butler
  :ensure t
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package avy
  :ensure t
  :bind 
  ("s-i" . avy-goto-char)
  ("s-f" . avy-goto-char-timer))

(use-package avy-zap
  :after avy
  :ensure t
  :bind
  ([remap zap-up-to-char] . avy-zap-up-to-char-dwim)
  ([remap zap-to-char] . avy-zap-to-char-dwim))

(use-package ace-link
  :ensure t
  :config
  (ace-link-setup-default))

(use-package vundo
  :ensure t
  :bind (("C-x u" . vundo)
         ("H-/" . vundo)))

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

(use-package consult-denote

  :bind (("C-c n f" . consult-denote-find)
	 ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

(use-package tab-bar
  :custom ((tab-bar-show nil)		; hide bar if <= 1 tabs open
           (tab-bar-auto-width nil)
           (tab-bar-select-tab-modifiers '(super))
           (tab-bar-close-button-show nil)))


;; (setopt tab-bar-show nil) ; on customize-set-variable see https://emacs.stackexchange.com/a/106
;; (setq tab-bar-close-button-show nil)       ;; hide tab close / X button
;; (setq tab-bar-tab-hints nil) ;; show tab numbers
;; (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)) ;; elements to include in bar

(use-package activities
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)
  :init
  (bind-keys :prefix-map my-activities-prefix-map
             :prefix "H-a"
             ("n" . activities-new)
             ("d" . activities-define)
             ("a" . activities-resume)
             ("s" . activities-suspend)
             ("k" . activities-kill)
             ("RET" . activities-switch)
             ("b" . activities-switch-buffer)
             ("g" . activities-revert)
             ("l" . activities-list)))

(use-package popper
  :ensure t
  :bind (("H-`"   . popper-toggle)
         ("H-<tab>"   . popper-cycle)
         (:map popper-mode-map
               ("H-~" . popper-toggle-type)))
  :custom
  ;; specify buffer types for popper to control
  (popper-reference-buffers '("Output\\*$"
                              "\\*Async Shell Command\\*"
                              help-mode
                              compilation-mode
                              ;; and suppress or hide some of them
                              ("\\*Messages\\*" . hide)
                              ("\\*Warnings\\*" . hide)))
  :hook
  (after-init . popper-mode)
  (After-init . popper-echo-mode))

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

(use-package windmove
  :config
  (windmove-default-keybindings 'hyper)
  (windmove-swap-states-default-keybindings '(hyper shift)))

;; (windmove-delete-default-keybindings)
;; (windmove-display-default-keybindings)

(use-package fwb-cmds
  :ensure t
  :bind ("s-'" . fwb-toggle-window-split)) ; †next-window-any-frame

(use-package ace-window
  :ensure t
  :demand
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package zoom-window

  :bind ("C-\\" . zoom-window-zoom))	; originally toggleinput-method

(setq dired-dwim-target t)

(use-package reveal-in-osx-finder
  :ensure t
  :bind ("H-f" . reveal-in-osx-finder))

(use-package magit
  :bind ("H-g" . magit-status))

;; (use-package geiser-mit

;;   :config
;;   (setq geiser-racket-binary (executable-find "Racket")))

;; (use-package ultra-scroll-mac
;;   :if (eq window-system 'mac)
;;   ;:load-path "~/code/emacs/ultra-scroll-mac" ; if you git clone'd instead of package-vc-install
;;   :init
;;   (setq scroll-conservatively 101 ; important!
;;         scroll-margin 0)
;;   :config
;;   (ultra-scroll-mac-mode 1))


(use-package py-vterm-interaction
  :hook (python-mode . py-vterm-interaction-mode)
  :custom
  (py-vterm-interaction-repl-program "ipython -i")
  (py-vterm-interaction-silent-cells t))

(use-package paredit
  :ensure t
  :demand
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (eval-expression-minibuffer-setup . paredit-mode)))

(use-package tree-sitter
  )

(use-package tree-sitter-langs

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

(use-package racket-mode
  :ensure t
  :mode("\\.rkt?\\'" . racket-mode)
  :custom (racket-program "/Applications/Racket v8.15/bin/racket"))

(use-package display-fill-column-indicator
  :custom (display-fill-column-indicator-character nil)
  :custom-face (fill-column-indicator
                ((t (:height 1.0 :foreground "grey"))))
  :hook (prog-mode . display-fill-column-indicator-mode))

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

(use-package info-variable-pitch

  :hook (info-mode . info-variable-pitch-mode))

(use-package olivetti
  )

(provide 'init)

;;; init.el ends here

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
        (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file 'noerror))

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

(setopt  comment-multi-line t
	 
	 
	 
	 
	 help-window-select t
	 
	 
	 savehist-mode t
	 
	 tab-always-indent 'complete
	 uniquify-buffer-name-style 'forward
	 
	 
	 
	 vc-follow-symlinks t
	 )

(use-package savehist
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(use-package emacs
  :ensure nil
  :custom
  (create-lockfiles nil)
  (fill-column 80)
  (ring-bell-function 'ignore)
  (use-dialog-box nil)
  (use-file-dialog nil)
  (use-short-answers t))

(use-package use-package
  :ensure nil
  :custom (use-package-enable-imenu-support t))



;;;; GUI

;; Declare font variables
(defcustom my-default-font "Input Mono Narrow"
  "The default font to use."
  :type 'string
  :group 'faces)

(defcustom my-default-font-height 140
  "The default font height (1/10 of a point)."
  :type 'integer
  :group 'faces)

(defcustom my-variable-font "Baskerville"
  "The variable font to use."
  :type 'string
  :group 'faces)

(defcustom my-variable-font-height 140
  "The variable font height (1/10 of a point)."
  :type 'integer
  :group 'faces)

(defcustom my-ui-font "Input Mono Compressed"
  "Font to use for UI elements."
  :type 'string
  :group 'faces)

(defcustom my-ui-font-height 120
  "The UI font height (1/10 of a point)."
  :type 'integer
  :group 'faces)

(defcustom my-ui-italic-font "InputMonoCompressed-Italic"
  "Italic font to use for UI elements."
  :type 'string
  :group 'faces)

(defcustom my-ui-italic-font-height 120
  "The UI italic font height (1/10 of a point)."
  :type 'integer
  :group 'faces)

;; Define ui face with fallback
(defface ui
  `((t :family ,(if (find-font (font-spec :name my-ui-font))
                    my-ui-font
                  "Monospace")))
  "Custom UI face for interface elements."
  :group 'basic-faces)

(use-package faces
  :ensure nil
  :demand t
  :config
  (when (find-font (font-spec :name my-default-font))
    (set-face-attribute 'default nil
                        :font my-default-font
                        :height my-default-font-height))
  (when (find-font (font-spec :name my-variable-font))
    (set-face-attribute 'variable-pitch nil
                        :font my-variable-font
                        :height my-variable-font-height)))

(use-package emacs
  :ensure nil
  :custom
  (mode-line-mule-info nil)
  (mode-line-remote nil)
  (mode-line-modified nil)
  (mode-line-position nil)
  :config
  ;; Set mode-line to inherit from ui face
  (dolist (face '(mode-line mode-line-inactive))
    (set-face-attribute face nil :inherit 'ui))

  ;; Define buffer name faces with italic font or slant fallback
  (let ((use-italic-font (find-font (font-spec :name my-ui-italic-font))))
    (dolist (spec '((my-mode-line-buffer-name-modified "red")
                    (my-mode-line-buffer-name-read-only "blue")
                    (my-mode-line-buffer-name-read-only-modified "purple")))
      (eval `(defface ,(car spec)
               '((t :foreground ,(cadr spec)
                    :weight bold
                    ,@(if use-italic-font
                          `(:family ,my-ui-italic-font)
                        '(:slant italic))
                    :inherit ui))
               ,(format "Face for %s buffer name." (symbol-name (car spec)))
               :group 'mode-line))))

  ;; Customize buffer name display
  (setq-default mode-line-buffer-identification
                '(:eval (let ((name (propertize "%b" 'help-echo (buffer-file-name))))
                          (cond ((and buffer-read-only (buffer-modified-p))
                                 (propertize name 'face 'my-mode-line-buffer-name-read-only-modified))
                                (buffer-read-only
                                 (propertize name 'face 'my-mode-line-buffer-name-read-only))
                                ((buffer-modified-p)
                                 (propertize name 'face 'my-mode-line-buffer-name-modified))
                                (t name)))))

  ;; Customize mode-line format
  (setq-default mode-line-format
                '("%e"
                  mode-line-front-space
                  " "
                  mode-line-buffer-identification
                  "  "
                  ;; VC info (Git branch)
                  (:eval (when vc-mode
                           (concat " " (substring-no-properties vc-mode))))
		  "  "
		 
                  ;; Right-aligned section
                  mode-line-format-right-align
		  ;; mode-line-percent-position
		  "  "
                  mode-line-modes
                  " "
                  mode-line-end-spaces)))

(use-package tab-bar
  :custom
  (tab-bar-select-tab-modifiers '(hyper))
  (tab-bar-close-button-show nil)
  :custom-face
  (tab-bar ((t (:inherit ui)))))

(use-package minions
:custom
(minions-mode-line-delimiters t)
:config
(minions-mode 1))

(use-package mixed-pitch
:disabled
:hook
;; If you want it in all text modes:
(text-mode . mixed-pitch-mode))



;; (setopt tab-bar-show nil) ; on customize-set-variable see https://emacs.stackexchange.com/a/106
;; (setq tab-bar-close-button-show nil)       ;; hide tab close / X button
;; (setq tab-bar-tab-hints nil) ;; show tab numbers
;; (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)) ;; elements to include in bar

;;; Keybindings

(use-package emacs
  :bind
  (:map global-map

	;; unbind these keys globally
	("C-z" . nil)
	("s-x" . nil)

	;; bind these unprefixed commands globally
	("s-a" . beginning-of-buffer)
	("s-e" . end-of-buffer)

	;; Control-z
        :prefix-map my-ctrl-z-prefix-map
        :prefix "C-z"
	

	;; Super-c prefix map
	:prefix-map my-super-x-prefix-map
        :prefix "s-c"

	;; Super-x prefix map
	:prefix-map my-super-x-prefix-map
        :prefix "s-x"

	;; Super-z prefix map
	:prefix-map my-super-z-prefix-map
        :prefix "s-z"

	;; H-x
	:prefix-map my-hyper-x-prefix-map
        :prefix "H-x"
        ("f" . write-region))
  :init
  (put 'narrow-to-page 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  )

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

(use-package transient
  :ensure t
  :config
  (transient-bind-q-to-quit)

  (defun my-transient-consult-theme ()
    "Select color theme and return to toggles transient menu."
    (interactive)
    (call-interactively #'consult-theme)
    (buffer-toggles-menu))

  (defun my-disable-theme-dwim ()
    "Disable all themes without prompting and clean up lingering customizations."
    (interactive)
    ;; Disable all enabled themes
    (mapc #'disable-theme custom-enabled-themes)
    ;; Clean up lingering customizations by toggling modes
    (when (bound-and-true-p outli-mode)
      (outli-mode -1)
      (outli-mode 1))
    (message "Disabled all themes and refreshed modes"))
  
  (transient-define-prefix buffer-toggles-menu ()
    "Menu to toggle in-buffer preferences on/off."
    :transient-suffix t
    ["Buffer toggles"
     [""
      ("c" "set color theme" my-transient-consult-theme)
      ("d" "disable color theme" my-disable-theme-dwim)
      ]
     [""
      ("f" "focus" focus-mode)
      ("o" "olivetti" olivetti-mode)
      ("t" "typo" typo-mode)
      ("v" "variable fonts" variable-pitch-mode)]
     [""
      ("x" "Apply and exit menu" transient-quit-one)]])

  (transient-define-prefix lettercase-commands-menu ()
    "Menu for narrowing and folding commands."
    ["Capitalization"
     ("g" "region" capitalize-region)
     ("m" "word or active region" capitalize-dwim)
     ("t" "word" capitalize-word)]	; orig. M-c
    ["Uppercase"
     ("C" "character" upcase-char)
     ("I" "capitalize words" upcase-initials-region )
     ("R" "region" upcase-region)	; orig. C-x C-u
     ("U" "word or active region" upcase-dwim)
     ("W" "word" upcase-word) 		; orig. M-u
     ]
    ["Lowercase"
     ("d" "word or active region" downcase-dwim)
     ("r" "region" downcase-region)	; orig. C-x C-l
     ("w" "word" downcase-word)]	; orig. M-l
    ;; dired-specific commands (not working)
    ["Uppercase" :if-mode dired
     ("F" "this filename" diredp-upcase-this-file)
     ("S" "selected filenames" diredp-upcase)
     ("A" "selected filenames, recursively" diredp-upcase-recursive)
     ]
    ["Lowercase" :if-mode dired
     ("f" "this filename" diredp-downcase-this-file)
     ("s" "selected filenames" diredp-downcase)
     ("a" "selected filenames, recursively" diredp-downcase-recursive)])

  (transient-define-prefix narrowing-commands-menu ()
    "Menu for buffer narrowing commands."
    ["Narrow"
     ("n" "to region" narrow-to-region)
     ("d" "to defun" narrow-to-defun)
     ("p" "to page" narrow-to-page)]
    ["Org" :if-mode org-mode
     ("b" "narrow to block" org-narrow-to-block)
     ("s" "narrow to subtree" org-narrow-to-subtree)]
    [""
     ("w" "widen" widen)])

  :bind
  ("H-l" . lettercase-commands-menu)
  ("H-m" . buffer-toggles-menu)
  ("H-n" . narrowing-commands-menu))

;;; Editing and navigating

(use-package ace-link
  :ensure t
  :config
  (ace-link-setup-default))

(use-package aggressive-indent
  :ensure t
  :hook ((after-init . global-aggressive-indent-mode)))

(use-package avy
  :ensure t
  :config

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

  (defun my-occur-avy-goto-line-and-jump ()
    "Use avy to jump to a line in occur buffer, then go to occurrence and quit."
    (interactive)
    (let ((avy-all-windows nil))
      (avy-goto-line))
    (my-occur-goto-occurrence-and-quit))

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
  :bind (("s-f" . avy-goto-char-timer) ; †exchange-point-and-mark
	 :map isearch-mode-map
	 ("s-f" . avy-isearch)
	 :map occur-mode-map
	 ("s-l" . my-occur-avy-goto-line-and-jump)))

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

(use-package imenu
  :ensure nil
  :bind (("s-i" . imenu)))

(use-package isearch
  :ensure nil
  :config
  (defun my-isearch-occur-and-switch ()
    "Run `isearch-occur', then exit isearch and switch to the occur buffer."
    (interactive)
    (let ((regexp (if isearch-regexp
                      isearch-string
                    (regexp-quote isearch-string))))
      (isearch-occur regexp))
    (isearch-exit)
    (pop-to-buffer "*Occur*"))
  
  :bind (:map isearch-mode-map
	      ("C-o" . my-isearch-occur-and-switch)))

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

(use-package paragraphs
  :ensure nil
  :custom
  (sentence-end-double-space nil)
  :bind (("s-a" . backward-paragraph)
	 ("s-e" . forward-paragraph)

	 ))

(use-package pulsar
  :ensure t
  :defines pulsar-pulse-functions
  :config
  (add-to-list 'pulsar-pulse-functions 'my-other-window-dwim)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)
  (add-to-list 'pulsar-pulse-functions 'goto-char)
  (add-to-list 'pulsar-pulse-functions 'xref-find-definitions)
  (add-to-list 'pulsar-pulse-functions 'xref-find-definitions-other-window)
  :hook
  (after-init . pulsar-global-mode))

(use-package replace
  :ensure nil
  :config
  (defun my-occur-goto-occurrence-and-quit ()
    "Go to the occurrence and quit the occur buffer window."
    (interactive)
    (let ((occur-window (selected-window)))
      (occur-mode-goto-occurrence)
      (quit-window nil occur-window)))
  :bind (("s-r" . query-replace) 	; orig M-%
	 :map occur-mode-map
	 ("C-j" . my-occur-goto-occurrence-and-quit)))

(use-package simple
  :ensure nil
  :bind (("s-[" . beginning-of-buffer)
	 ("s-]" . end-of-buffer)))

(use-package ultra-scroll
  ;; :ensure t
  :init
  (setq scroll-conservatively 101	; important!
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
  ;; :custom
  ;; (vertico-multiform-categories
  ;;  '(embark-keybinding grid))
  :init
  (vertico-mode)
  :config
  (vertico-multiform-mode))

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
   ("s-g" . consult-mark)
   ("M-g k" . consult-global-mark)
   ([remap imenu] . consult-imenu)
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

(use-package consult-project-extra
  :ensure t
  :custom (consult-project-function #'consult-project-extra-project-fn) ;; Optional but recommended for a more consistent UI
  :bind
  (("s-b" . consult-project-extra-find)
   ("s-B" . consult-project-extra-find-other-window)))

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
                 (window-parameters (mode-line-format . none))))
  :custom
  (embark-quit-after-action nil)
  ;; (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  )

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

(use-package yasnippet
  :custom
  (yas-snippet-dirs
   (list (expand-file-name "snippets" user-emacs-directory) ; Default personal snippets
         (expand-file-name "elpa/yasnippet-snippets-1.0/snippets" user-emacs-directory)))
  :bind ("H-s" . yas-insert-snippet)
  :init (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

;;;; Files and Dired

(use-package emacs
  :ensure nil

  :config
  (defun open-in-finder-file-dir ()
    "Open the directory of the currently visted file in macOS Finder."
    (interactive)
    (let ((file-path (buffer-file-name)))
      (if file-path
          (let ((dir-path (file-name-directory file-path)))
            (shell-command (concat "open " (shell-quote-argument dir-path))))
	(message "No file is associated with this buffer"))))

  :bind (:map global-map
	      ("H-f" . open-in-finder-file-dir)))

(use-package files
  :ensure nil
  :custom
  (backup-by-copying t)
  (delete-old-versions t)
  (version-control t ))

(use-package recentf
  :ensure nil
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-exclude '("/auto-install/"
                     ".recentf"
                     "/repos/"
                     "/elpa/"
                     "\\.mime-example"
                     "\\.ido.last"
                     "COMMIT_EDITMSG"
                     ".gz"
                     "~$"
                     "/ssh:"
                     "/sudo:"
                     "/scp:"))
  (recentf-max-menu-items 50)
  (recentf-max-saved-items 1000)
  :hook
  (after-init . recentf-mode))

(use-package dired
  :ensure nil

  :config
  (defun my-dired-shorten-header ()
    "Replace absolute path with shortened version in Dired header."
    (let ((inhibit-read-only t))  ; Temporarily allow modifications
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward "^  \\(/.*\\):$" nil t)
          (let* ((full-path (match-string 1))
		 (short-path (abbreviate-file-name full-path)))
            (replace-match (concat "  " short-path ":") t t))))))

  (defun open-in-finder-current-dired ()
    "Open the currently visited Dired directory in macOS Finder."
    (interactive)
    (if (eq major-mode 'dired-mode)
	(let ((dir-path (dired-current-directory)))
          (shell-command (concat "open " (shell-quote-argument dir-path))))
      (message "Not in dired mode")))

  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-free-space 'separate)
  (dired-hide-details-hide-symlink-targets nil)

  :bind (:map dired-mode-map
	      ("H-f" . open-in-finder-current-dired))

  :hook
  (dired-mode . dired-hide-details-mode) ; alternatively set in dired+
  (dired-after-readin-hook . my-dired-shorten-header))

(use-package dired+
  :ensure nil
  :vc (:url "https://github.com/emacsmirror/dired-plus"
	    :rev :newest))

(use-package dired-x
  :ensure nil
  :custom
  (dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")
  :hook (dired-mode . dired-omit-mode))

;;; Managing frames, tabs, windows, and buffers

(use-package ace-window
  :ensure t
  :preface
  
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

;; (use-package bufferlo
;;   :ensure t
;;   :demand t
;;   ;; :after (ibuffer consult)
;;   :init
;;   (defvar bufferlo-command-map (make-sparse-keymap)
;;     "Keymap for bufferlo commands.")
;;   :config
;;   (defvar my:bufferlo-consult--source-local-buffers
;;     (list :name "Bufferlo Local Buffers"
;;           :narrow   ?l
;;           :category 'buffer
;;           :face     'consult-buffer
;;           :history  'buffer-name-history
;;           :state    #'consult--buffer-state
;;           :default  t
;;           :items    (lambda () (consult--buffer-query
;;   				:predicate #'bufferlo-local-buffer-p
;;   				:sort 'visibility
;;   				:as #'buffer-name)))
;;     "Local Bufferlo buffer candidate source for `consult-buffer'.")

;;   (defvar my:bufferlo-consult--source-other-buffers
;;     (list :name "Bufferlo Other Buffers"
;;           :narrow   ?b
;;           :category 'buffer
;;           :face     'consult-buffer
;;           :history  'buffer-name-history
;;           :state    #'consult--buffer-state
;;           :items    (lambda () (consult--buffer-query
;;   				:predicate #'bufferlo-non-local-buffer-p
;;   				:sort 'visibility
;;   				:as #'buffer-name)))
;;     "Non-local Bufferlo buffer candidate source for `consult-buffer'.")

;;   ;; add in the reverse order of display preference
;;   (add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-other-buffers)
;;   (add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-local-buffers)
;;   (bufferlo-mode)
;;   (bufferlo-anywhere-mode)
;;   :custom
;;   (bufferlo-bookmark-frame-load-make-frame t) ; default is nil for backward compatibility
;;   (bufferlo-bookmark-frame-load-make-frame 'restore-geometry)
;;   ;; (bufferlo-bookmarks-auto-save-interval 120)
;;   :bind-keymap ("s-b" . bufferlo-command-map)
;;   :bind (:map bufferlo-command-map
;;   	      ;; buffer / ibuffer
;;   	      ("b" . bufferlo-switch-to-buffer)
;;   	      ("s-b" . bufferlo-ibuffer)
;;   	      ("o" . bufferlo-ibuffer-orphans)
;;   	      ("-" . bufferlo-remove)
;;   	      ;; general bookmark (interactive)
;;   	      ("m l" . bufferlo-bms-load)
;;   	      ("m s" . bufferlo-bms-save)
;;   	      ("m c" . bufferlo-bms-close)
;;   	      ("m r" . bufferlo-bm-raise)
;;   	      ;; dwim frame or tab bookmarks
;;   	      ("s-s" . bufferlo-bm-save)
;;   	      ("s-l" . bufferlo-bm-load)
;;   	      ("s-0" . bufferlo-bm-close)
;;   	      ;; tabs
;;   	      ("t s" . bufferlo-bm-tab-save)               ; save
;;   	      ("t u" . bufferlo-bm-tab-save-curr)          ; update
;;   	      ("t l" . bufferlo-bm-tab-load)               ; load
;;   	      ("t r" . bufferlo-bm-tab-load-curr)          ; reload
;;   	      ("t 0" . bufferlo-bm-tab-close-curr)         ; kill
;;   	      ;; frames
;;   	      ("f s" . bufferlo-bm-frame-save)             ; save
;;   	      ("f u" . bufferlo-bm-frame-save-curr)        ; update
;;   	      ("f l" . bufferlo-bm-frame-load)             ; load
;;   	      ("f r" . bufferlo-bm-frame-load-curr)        ; reload
;;   	      ("f m" . bufferlo-bm-frame-load-merge)       ; merge
;;   	      ("f 0" . bufferlo-bm-frame-close-curr)       ; kill
;;   	      ;; sets
;;   	      ("s s" . bufferlo-set-save)                  ; save
;;   	      ("s u" . bufferlo-set-save-curr)             ; update
;;   	      ("s +" . bufferlo-set-add)                   ; add bookmark
;;   	      ("s =" . bufferlo-set-add)                   ; add bookmark
;;   	      ("s -" . bufferlo-set-remove)                ; remove bookmark
;;   	      ("s l" . bufferlo-set-load)                  ; load
;;   	      ("s 0" . bufferlo-set-close)                 ; kill
;;   	      ("s c" . bufferlo-set-clear)                 ; clear
;;   	      ("s L" . bufferlo-set-list)                  ; list contents of selected active sets
;;   	      ))

(use-package ibuffer
  :ensure nil
  :bind ([remap list-buffers] . ibuffer)) ; C-x C-b

;; (use-package ibuffer-vc
;;   :after ibuffer)

;; (use-package otpp
;;   :ensure t
;;   :after project
;;   :init
;;   ;; Enable `otpp-mode` globally
;;   (otpp-mode 1)
;;   ;; If you want to advice the commands in `otpp-override-commands`
;;   ;; to be run in the current's tab (so, current project's) root directory
;;   ;; (otpp-override-mode 1)
;;   )

(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
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

(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers '(".dir-locals.el" "package.json" ".project" ".project.el"))
  :bind (("s-x b" . project-switch-project)))

;; (use-package tabspaces
;;   :ensure t
;;   :config
;;   (transient-define-prefix tabspaces-menu ()
;;     "Transient menu for tabspaces-mode"
;;     ["Tabspaces"
;;      ["Workspaces"
;;       ("K" "Kill workspace but not buffers" tabspaces-close-workspace)
;;       ("k" "Kill workspace and local buffers" tabspaces-kill-buffers-close-workspace)
;;       ("o" "Open/create" tabspaces-open-or-create-project-and-workspace)
;;       ("s" "Switch/create" tabspaces-switch-or-create-workspace)]
;;      ["Manage"
;;       ("w" "Show workspaces" tabspaces-show-workspaces)]
;;      ["Buffers"
;;       ("b" "Switch to local buffer" tabspaces-switch-to-buffer)
;;       ("c" "Clear other buffers" tabspaces-clear-buffers)
;;       ("r" "Remove current buffer from workspace" tabspaces-remove-current-buffer)
;;       ("R" "Remove selected buffer from workspace" tabspaces-remove-selected-buffer)
;;       ("t" "Switch to buffer in workspace" tabspaces-switch-buffer-and-tab)]
;;      ]
;;     )
;;   :bind ("H-<tab>" . tabspaces-menu)
;;   :hook (after-init . tabspaces-mode)
;;   :commands (tabspaces-switch-or-create-workspace
;;              tabspaces-open-or-create-project-and-workspace)
;;   :custom
;;   (tabspaces-use-filtered-buffers-as-default t)
;;   (tabspaces-default-tab "Default")
;;   (tabspaces-remove-to-default t)
;;   (tabspaces-include-buffers '("*scratch*"))
;;   (tabspaces-initialize-project-with-todo t)
;;   (tabspaces-todo-file-name "project-todo.org")
;;   ;; sessions
;;   (tabspaces-session t)
;;   ;;(tabspaces-session-auto-restore t)
;;   (tab-bar-new-tab-choice "*scratch*"))

(use-package transpose-frame
  :ensure t
  :bind (("s-(" . rotate-frame-anticlockwise)
	 ("s-)" . rotate-frame-clockwise)
	 ("s-*" . transpose-frame)))

(use-package windmove
  :config
  (windmove-default-keybindings 'hyper)
  ;; (windmove-delete-default-keybindings)
  ;; (windmove-display-default-keybindings)
  (windmove-swap-states-default-keybindings '(hyper shift)))

(use-package window
  :ensure nil
  :config

  (defun my-delete-other-windows-dwim ()
    "Delete the windows in the same column with WINDOW, but not WINDOW itself.

If there are only two windows in the current frame, call `delete-other-windows'
to maximize the current window."
    (interactive)
    (if (= (length (window-list)) 2)
	(delete-other-windows)
      (delete-other-windows-vertically)))

  (defun my-other-window-dwim (arg &optional all-frames)
    "Switch to other window. If there is only one window in the current frame,
split it horizontally and move point to the newly created window. If called with
a prefix argument, split the window vertically before moving point to newly
created window."
    (interactive "P")
    (if (one-window-p)
	(progn
	  (if arg
	      (split-window-vertically)
            (split-window-horizontally))
          (other-window 1))
      (other-window (prefix-numeric-value arg) all-frames)))

  :bind
  ("C-x 1" . my-delete-other-windows-dwim)
  ("C-x C-1" . delete-other-windows)
  ("M-o" . my-other-window-dwim)
  ("s-v" . scroll-up-line)
  ("s-M-v" . scroll-down-line)
  )

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

(use-package eglot
  :ensure nil
  :defer t
  ;; :hook (
  ;; 	 (css-basemode)
  ;; 	 (js-mode)
  ;; 	 (json-mode)
  ;; 	 (lua-mode)
  ;; 	 (typescript-ts-base-mode)
  ;; 	 . eglot-ensure)
  )

;; wrapper around https://github.com/blahgeek/emacs-lsp-booster?tab=readme-ov-file#obtain-or-build-emacs-lsp-booster
(use-package eglot-booster
  :disabled				; need to install Rust toolchain and then emacs-lsp-booster first
  :vc (:url "https://github.com/jdtsmith/eglot-booster"
	    :rev :newest)
  :after eglot
  :config (eglot-booster-mode))

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

;; (use-package lsp-mode
;;   :diminish "LSP"
;;   :ensure t
;;   :hook ((lsp-mode . lsp-diagnostics-mode)
;;          (lsp-mode . lsp-enable-which-key-integration)
;;          ((tsx-ts-mode
;;            typescript-ts-mode
;;            js-ts-mode) . lsp-deferred))
;;   :custom
;;   (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
;;   (lsp-completion-provider :none)       ; Using Corfu as the provider
;;   (lsp-diagnostics-provider :flycheck)
;;   (lsp-session-file (locate-user-emacs-file ".lsp-session"))
;;   (lsp-log-io nil) ; IMPORTANT! Use only for debugging! Drastically affects performance
;;   (lsp-keep-workspace-alive nil) ; Close LSP server if all project buffers are closed
;;   (lsp-idle-delay 0.5)	 ; Debounce timer for `after-change-function'
;;   ;; core
;;   (lsp-enable-xref t)	 ; Use xref to find references
;;   (lsp-auto-configure t) ; Used to decide between current active servers
;;   (lsp-eldoc-enable-hover t) ; Display signature information in the echo area
;;   (lsp-enable-dap-auto-configure t)     ; Debug support
;;   (lsp-enable-file-watchers nil)
;;   (lsp-enable-folding nil)	 ; I disable folding since I use origami
;;   (lsp-enable-imenu t)
;;   (lsp-enable-indentation nil)	    ; I use prettier
;;   (lsp-enable-links nil)	    ; No need since we have `browse-url'
;;   (lsp-enable-on-type-formatting nil)   ; Prettier handles this
;;   (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
;;   (lsp-enable-symbol-highlighting t) ; Shows usages of symbol at point in the current buffer
;;   (lsp-enable-text-document-color nil)	; This is Treesitter's job

;;   (lsp-ui-sideline-show-hover nil)  ; Sideline used only for diagnostics
;;   (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
;;   ;; completion
;;   (lsp-completion-enable t)
;;   (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
;;   (lsp-enable-snippet t)      ; Important to provide full JSX completion
;;   (lsp-completion-show-kind t)	       ; Optional
;;   ;; headerline
;;   (lsp-headerline-breadcrumb-enable t) ; Optional, I like the breadcrumbs
;;   (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
;;   (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
;;   (lsp-headerline-breadcrumb-icons-enable nil)
;;   ;; modeline
;;   (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
;;   (lsp-modeline-diagnostics-enable nil)	; Already supported through `flycheck'
;;   (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
;;   (lsp-signature-doc-lines 1) ; Don't raise the echo area. It's distracting
;;   (lsp-ui-doc-use-childframe t)		; Show docs for symbol at point
;;   (lsp-eldoc-render-all nil) ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
;;   ;; lens
;;   (lsp-lens-enable nil)                 ; Optional, I don't need it
;;   ;; semantic
;;   (lsp-semantic-tokens-enable nil) ; Related to highlighting, and we defer to treesitter

;;   :init
;;   (setq lsp-use-plists t))

;; (use-package lsp-completion
;;   :no-require
;;   :hook ((lsp-mode . lsp-completion-mode)))

;; (use-package lsp-eslint
;;   :demand t
;;   :after lsp-mode)

;; (use-package lsp-ui
;;   :ensure t
;;   :commands
;;   (lsp-ui-doc-show
;;    lsp-ui-doc-glance)
;;   :bind (:map lsp-mode-map
;;               ("C-c C-d" . 'lsp-ui-doc-glance))
;;   :config (setq lsp-ui-doc-enable t
;;                 lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
;;                 lsp-ui-doc-include-signature t       ; Show signature
;;                 lsp-ui-doc-position 'at-point))

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
  (org-cycle-separator-lines 0)
  (org-ellipsis " …")
  ;; Remove underline from org-ellipsis just defined.
  (set-face-underline 'org-ellipsis nil)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-agenda-files (list org-directory))
  (org-directory "~/Dropbox/org")
  (org-default-notes-file (concat org-directory "/capture.org"))

  (org-capture-templates
   ;; See (info "(org) Template expansion")
   '(("a" "Appointment" entry (file "agenda.org")
      "* Appointment with Dr. %^{Who?} %^{When?}t :appointment:")
     ("c" "Chore" entry (file "chores.org")
      "* TODO %?")
     ("e" "Errand" entry (file "errands.org")
      "* TODO %?")
     ("m" "Meeting" entry (file "agenda.org")
      "* TODO Meeting with %^{Who?} %^{When?}t %? :meeting:")
     ("t" "Task" entry (file "capture.org")
      "* TODO %?")
     ("w" "Consulting" entry (file "consulting.org")
      "* TODO %?")
     ("z" "Capture" entry (file "capture.org")
      "* %?")))

  (org-refile-targets
   '((nil :maxlevel . 1)
     (org-agenda-files :maxlevel . 1)))

  (org-refile-use-outline-path 'file)

  :config
  (defun my-org-insert-heading-above ()
    (interactive)
    (move-beginning-of-line nil)
    (org-insert-heading))

  :hook (org-mode . visual-line-mode)
  :bind (("H-a" . org-agenda )
	 ("H-c" . org-capture)
	 ("H-o t" . org-babel-tangle-file)
	 :map org-mode-map
	 ;; ("C-\\" . my-org-insert-heading-above)
	 ))

(use-package org-modern
  :after org
  :custom
  (org-modern-hide-stars nil)		; adds extra indentation
  (org-modern-block-name '("" . ""))
  (org-modern-fold-stars
   ;; '(("►" . "▼") ("►" . "▼") ("►" . "▼") ("►" . "▼") ("►" . "▼"))
   ;; Use with Jetbrains Mono font
   '(("▹" . "▿") ("▹" . "▿") ("▹" . "▿") ("▹" . "▿") ("▹" . "▿")))
  (org-modern-keyword t)
  :hook
  (org-mode . org-modern-mode))

;; (use-package org-modern-indent
;;   :after org-modern
;;   :config ; add late to hook
;;   (add-hook 'org-mode-hook #'org-modern-indent-mode 90))




;;; Reading and writing

(use-package consult-denote
  :after (consult denote)
  :bind (("C-c n f" . consult-denote-find)
	 ("C-c n g" . consult-denote-grep))
  
  :config
  (when (executable-find "fd")
    (setopt consult-denote-find-command #'consult-fd))
  (when (executable-find "rg")
    (setopt consult-denote-grep-command #'consult-ripgrep))
  (consult-denote-mode 1))

(use-package consult-notes
  :disabled
  :after (consult denote)
  :ensure t
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :custom
  (consult-notes-denote-files-function #'denote-directory-files) ; Search only for text files in denote dir
  (consult-notes-use-rg (and (executable-find "rg") t))
  :config
  (consult-notes-denote-mode))

(use-package denote

  :bind (("C-c n n" . denote)
	 ("C-c n s" . denote-subdirectory)
	 ("C-c n i" . denote-link)
	 ("C-c n l" . denote-link-find-file)
	 ("C-c n b" . denote-link-backlinks))
  :hook
  (dired-mode . denote-dired-mode)
  :custom
  (denote-directory "~/Dropbox/notes")
  (denote-file-name-slug-functions
   '((title . denote-sluggify-title)
     (keyword . denote-sluggify-keyword)
     (signature . denote-sluggify-signature))))

(use-package focus-mode :commands focus-mode)

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing

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

(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 60)
  ;; (olivetti-style 'fancy)
  )

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

(use-package typo
  :commands typo-mode)

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


(provide 'init)

;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)

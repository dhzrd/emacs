;;; -*- lexical-binding: t -*-

(add-to-list 'default-frame-alist '(undecorated-round . t))
(setq frame-resize-pixelwise t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setopt native-comp-async-report-warnings-errors nil)

  ;; Make native compilation happens asynchronously
  (setopt native-comp-deferred-compilation t))

(when (and (fboundp 'startup-redirect-eln-cache)
	   (fboundp 'native-comp-available-p)
	   (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

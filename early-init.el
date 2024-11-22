(add-to-list 'default-frame-alist '(undecorated-round . t))
(setq frame-resize-pixelwise t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

(when (and (fboundp 'startup-redirect-eln-cache)
	   (fboundp 'native-comp-available-p)
	   (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

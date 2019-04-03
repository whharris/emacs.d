(package-initialize)

(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Setup packages
(require 'setup-package)
;; Install extensions if they're missing
(defun my/install-packages ()
  (packages-install
   '(
     counsel
     counsel-projectile
     evil
     general
     ivy
     projectile
     spacemacs-theme
     which-key
     )))

(condition-case nil
    (my/install-packages)
  (error
   (package-refresh-contents)
   (my/install-packages)))

(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(set-face-attribute 'default nil :font "Inconsolata")
(set-face-attribute 'default nil :height 150)

(load-theme 'spacemacs-dark t)

(require 'better-defaults)

(require 'evil)
(evil-mode 1)

(projectile-mode +1)
(setq projectile-completion-system 'ivy)


(ivy-mode 1)
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

;; Keybindings
(defun my/load-init-file ()
  (interactive)
  (load user-init-file))

(defun my/edit-init-file ()
  (interactive)
  (find-file user-init-file))

(which-key-mode)
(require 'general)
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
 ;; bind to simple key press
  "/"   'counsel-projectile-ag
  "SPC"	'counsel-M-x
  "TAB"	'switch-to-prev-buffer
  ;; Buffers
  "b"   '(:ignore t :which-key "buffers")
  "bb"  '(ivy-switch-buffer :which-key "switch buffer")
  "bd"  '(kill-current-buffer :which-key "kill buffer")
  ;; Files
  "f"   '(:ignore t :which-key "files")
  "ff"  '(counsel-find-file :which-key "find file")
  "fr"  '(counsel-recentf :which-key "recent files")
  "fs"  '(save-buffer :which-key "save buffer")
  ;; Emacs config
  "fe"  '(:ignore t :which-key "emacs config")
  "fed" '(my/edit-init-file :which-key "edit emacs config")
  "feR" '(my/load-init-file :which-key "reload emacs config")
  ;; Projects
  "p"   '(:ignore t :which-key "project")
  "pa"  '(projectile-add-known-project :which-key "add project")
  "pf"  '(counsel-projectile-find-file :which-key "find file in project")
  "pp"  '(counsel-projectile-switch-project :which-key "switch project")
  ;; Windows
  "w"   '(:ignore t :which-key "windows")
  "w/"  '(split-window-right :which-key "split window vertically")
  "w-"  '(split-window-below :which-key "split window horizontally")
  "wd"  '(delete-window :which-key "delete window")
  "wh"  '(windmove-left :which-key "window left")
  "wj"  '(windmove-down :which-key "window below")
  "wk"  '(windmove-up :which-key "window above")
  "wl"  '(windmove-right :which-key "windo right")
  ;; Frames
  "F"   '(:ignore t :which-key "frames")
  "FN"  '(make-frame-command :which-key "new frame")
  ;; Frames
  "t"   '(:ignore t :which-key "toggle")
  "tF"  '(toggle-frame-fullscreen :which-key "toggle fullscreen")
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
    (counsel-projectile counsel ivy helm-projectile helm projectile spacemacs-theme evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

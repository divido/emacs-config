
;; ---- Load Paths ---------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "~/emacs/helpers"))
(add-to-list 'load-path (expand-file-name "~/emacs/modes"))
(add-to-list 'load-path (expand-file-name "~/emacs/modes/git-modes"))
(add-to-list 'load-path (expand-file-name "~/emacs/skeletons"))

;; ---- Package repositories -----------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; ---- Set Backups to use their own special directory ---------------------------
(setq backup-directory-alist `(("." . "~/.emacs-backups")))
(setq backup-by-copying-when-linked t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)

;; ---- Configuring the display / general setup ----------------------------------
(setq-default show-trailing-whitespace t)
(setq-default split-height-threshold nil)
(setq-default fill-column 80)
(setq-default truncate-lines t)
(transient-mark-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(windmove-default-keybindings)
(global-set-key "\M-F" 'windmove-right)
(global-set-key "\M-B" 'windmove-left)
(global-set-key "\M-P" 'windmove-up)
(global-set-key "\M-N" 'windmove-down)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(require 'color-theme)
(if (not (eq window-system 'nil))
	(color-theme-solarized-dark))

(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;; ---- C++ Coding Stuff ----------------------------------------------------------
(require 'smart-tabs)
(require 'c++-skeletons)

(setq c-default-style "bsd")
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; ---- Build System Integration --------------------------------------------------
(require 'cmake-mode)
(setq-default compilation-scroll-output t)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; ---- Git Modes -----------------------------------------------------------------
(require 'git-commit-mode)
(require 'gitconfig-mode)
(require 'gitignore-mode)

;; ---- Auto Fill -----------------------------------------------------------------
(defun auto-fill-80 ()
  (interactive)
  (auto-fill-mode)
  (setq fill-column 80))

;; ---- Documents -----------------------------------------------------------------
(add-hook 'tex-mode-hook 'flyspell-mode)

;; ---- Custom Key Bindings -------------------------------------------------------
(global-unset-key "\M-c") ;; Who needs capitialize-word? I use this as a prefix key for my own stuff
(global-set-key "\M-c\M-w" 'delete-trailing-whitespace)
(global-set-key "\M-c\M-c" 'uncomment-region)
(global-set-key "\M-c\M-f" 'auto-fill-80)
(global-set-key "\M-c`" 'compile)
(global-set-key "\M-csc" 'c++-class-skeleton)
(global-set-key "\M-csf" 'c++-stl-foreach)
(global-set-key "\M-csd" 'c++-doxygen-block)
(global-set-key "\M-cl" 'align)
(global-set-key "\M-c\M-l" 'align-regexp)
(global-set-key "\M-cmn" 'linum-mode)

;; ---- Auto Mode -----------------------------------------------------------------
(setq auto-mode-alist
      (append
       '(("\\.for\\'"  . fortran-mode)
         ("\\.TEXT\\'" . fortran-mode)
         ("\\.h\\'"    . c++-mode)
         ("\\.Make\\'" . makefile-mode)
         ("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

;; ---- Emacs Customize Variables -------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "#839496" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "raster" :family "Courier New"))))
 '(semantic-decoration-on-unknown-includes ((((class color) (background light)) (:background "#333333")))))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)

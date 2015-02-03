
;; ---- Load Paths ---------------------------------------------------------------
(let ((default-directory "~/emacs"))
  (normal-top-level-add-subdirs-to-load-path))

;; ---- Package repositories -----------------------------------------------------
(require 'package)
(package-initialize)

;; Create the packages-up-to-date file if you want to skip this package checking
;; on this site. It makes emacs launch faster, but these commands are important
;; on the first invocation after a clean setup.
(unless (file-exists-p "~/emacs/packages-up-to-date")
  (progn
	(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
	(package-refresh-contents)

	;; This iterates through a list of packages that are necessary for processing
	;; this initialization file, and prompts for installation if any are missing.
	(mapc
	 (lambda (package)
	   (or (package-installed-p package)
		   (if (y-or-n-p (format "Package %s is missing. Install it? " package))
			   (package-install package))))
	 '(color-theme color-theme-solarized git-commit-mode gitconfig-mode gitignore-mode))))

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

;; For moving the screen a little without moving the point
(defun scroll-down-1 () (interactive) (scroll-down 1))
(defun scroll-up-1   () (interactive) (scroll-up 1))
(global-set-key "\M-p" 'scroll-down-1)
(global-set-key "\M-n" 'scroll-up-1)

(ido-mode)
(ido-everywhere)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(require 'color-theme)
(if (not (eq window-system 'nil))
	(color-theme-solarized-dark))

;; ---- C++ Coding Stuff ----------------------------------------------------------
(require 'smart-tabs)
(require 'c++-skeletons)
(require 'c++-extra-keywords)

(setq c-default-style "bsd")
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; ---- Shell Commands ------------------------------------------------------------

(global-set-key "\C-z" 'eshell)

(defun eshell-init-cmds ()
  (interactive)
  (local-set-key (kbd "C-M-z")
				 '(lambda ()
					(interactive)
					(switch-to-buffer nil)))
  (setq show-trailing-whitespace nil))

(defun eshell/make (&rest args)
  "Invokes make with supplied arguments in a compilation buffer"
  (compile (apply 'eshell-flatten-and-stringify "make" args)))

(add-hook 'eshell-mode-hook 'eshell-init-cmds)

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

;; ---- Web Mode ------------------------------------------------------------------
(require 'web-mode)

;; ---- Auto Fill -----------------------------------------------------------------
(defun auto-fill-80 ()
  (interactive)
  (auto-fill-mode)
  (setq fill-column 80))

;; ---- Documents -----------------------------------------------------------------
(add-hook 'latex-mode-hook
		  (lambda ()
			(auto-fill-mode)
			(setq fill-column 132)
			(setq fill-prefix "  ")
			(flyspell-mode)))

;; Useful for Guitar tabs
(defun split-and-follow ()
  ""
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (follow-mode 1)
  (goto-char (point-min)))

;; ---- Custom Key Bindings -------------------------------------------------------
(global-unset-key "\M-c") ;; Who needs capitialize-word? I use this as a prefix key for my own stuff
(global-set-key "\M-c\M-w" 'delete-trailing-whitespace)
(global-set-key "\M-c\M-c" 'uncomment-region)
(global-set-key "\M-c\M-f" 'auto-fill-80)
(global-set-key "\M-c\M-s" 'split-and-follow)
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
         ("\\.cmake\\'" . cmake-mode)
		 ("\\.tex$" . LaTeX-mode)
		 ("\\.html$" . web-mode)
		 ("\\.js$" . web-mode)
		 ("\\.php$" . web-mode))
       auto-mode-alist))

;; ---- Emacs Customize Variables -------------------------------------------------

;; Set up the default font to be OS dependent
(cond
 ((or (eq system-type 'cygwin) (eq system-type 'windows-nt))
  (setq fontFamily "Courier New"))

 (t
  (setq fontFamily "Liberation Mono")))

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
 '(default ((t (:height 80 :family fontFamily))))
 '(semantic-decoration-on-unknown-includes ((((class color) (background light)) (:background "#333333")))))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)

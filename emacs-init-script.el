
;; ---- Load Paths ---------------------------------------------------------------
(let ((default-directory "~/emacs"))
  (normal-top-level-add-subdirs-to-load-path))

;; ---- Package repositories -----------------------------------------------------
(require 'package)
(package-initialize)

;; This hack seems to fix a problem for emacs 26.2 with regard to the base packages
;; It should be removed once emacs updates to 26.3 or better
;; See: https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; To optimize scanning for new packages, I store a special package status file
;; in the emacs directory. This file contains a hash of the package list that is
;; installed on the system currently. If the package list is edited, the hash
;; won't match, and then emacs will invoke a full package update and install the
;; specified list. Otherwise, it doesn't bother downloading the package list.
(setq package-status-file "~/emacs/package-status")
(setq package-install-list
	  '(color-theme-sanityinc-solarized
		gitconfig-mode
		gitignore-mode
		smart-tabs-mode
		sass-mode
		tide
		company
		wgrep
		use-package))

(defun read-current-package-hash ()
  "Retrieve the hash from our specially named status file and return it"
  (if (file-exists-p package-status-file)
	  (with-temp-buffer
		(insert-file-contents package-status-file)
		(buffer-string))
	"Missing"))

(defun update-current-package-hash (new-hash)
  "Write the new hash value out to the specially name status file"
  (with-temp-file package-status-file (insert new-hash)))

(defun hash-package-list (package-list)
  "Computes a hash for the supplied package list"
  (secure-hash 'sha1 (mapconcat 'symbol-name package-list " ")))

(defun check-single-package (package)
  "Checks to see if a package needs to be installed, returns the package if it is, or nil if not"
  (if (or (package-installed-p package)
		  (when (y-or-n-p (format "Package %s is missing. Install it? " package))
			(message "Installing %s" package)
			package))
	  package))

(defun check-package-list (package-list)
  "Checks a list of packages, returning a list of all ones installed"
  (remove nil (mapcar 'check-single-package package-list)))

;; Here's the startup check for the package status
(if (equal (hash-package-list package-install-list) (read-current-package-hash))
	(message "Packages up to date")
  (progn
	(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
	(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
	(package-refresh-contents)
	(mapc
	 (lambda (package) (package-install package))
	 package-install-list)
	(update-current-package-hash
	 (hash-package-list
	  (check-package-list package-install-list)))))

;; ---- Set Backups to use their own special directory ---------------------------
(setq backup-directory-alist `(("." . "~/.emacs-backups")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs-backups" t)))
(setq create-lockfiles nil)
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
(setq ring-bell-function 'ignore)

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
(setq ido-enable-flex-matching t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-unset-key [C-down-mouse-1])
(global-unset-key [S-down-mouse-1])

;; ---- The Only Way To Indent ----------------------------------------------------
(smart-tabs-add-language-support web web-mode-hook
  ((web-mode-indent-line . web-mode-code-indent-offset)))

(defvaralias 'web-mode-markup-indent-offset 'web-mode-code-indent-offset)
(defvaralias 'web-mode-css-indent-offset 'web-mode-code-indent-offset)
(defvaralias 'web-mode-sql-indent-offset 'web-mode-code-indent-offset)

(smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python 'ruby 'nxml 'web)

;; ---- C++ Coding Stuff ----------------------------------------------------------
(require 'c++-skeletons)
(require 'c++-extra-keywords)

(setq c-default-style "bsd")
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; ---- Python --------------------------------------------------------------------
(add-hook 'python-mode-hook '(lambda () (setq tab-width 4)) t)

;; ---- Encryption ----------------------------------------------------------------
(require 'epa-file)
(setenv "GPG_AGENT_INFO" nil)

;; ---- Fish Shell Mode -----------------------------------------------------------
(require 'fish-mode)

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

(require 'compile)
(add-to-list 'compilation-error-regexp-alist
             'maven)
(add-to-list
 'compilation-error-regexp-alist-alist
 '(maven
   "\\[ERROR\\] \\(.+?\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\].\*" 1 2 3))

(when (eq system-type 'cygwin)
  (progn
	(setq compilation-parse-errors-filename-function
		  '(lambda (path)
			 (replace-regexp-in-string
			  "\n" "" (shell-command-to-string
					   (concat "cygpath --unix '" path "'")))))
	(defun cygpathWindows (cygwinPath)
	  (replace-regexp-in-string
	   "\n" "" (shell-command-to-string (concat "cygpath --windows '" cygwinPath "'"))))))

;; ---- Git Modes -----------------------------------------------------------------
(require 'gitconfig-mode)
(require 'gitignore-mode)

;; ---- Web Mode ------------------------------------------------------------------
(require 'web-mode)
(require 'js-skeletons)

(require 'typescript-mode)
(require 'sass-mode)

(add-hook 'sass-mode-hook (lambda () (setq indent-tabs-mode t)))
(add-hook 'scss-mode (global-set-key "\C-c\C-c" 'comment-region))
;; ----------------------------------------
;; Tide Stuff

;; (require 'typescript-mode)
;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1))

;; ;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; ;; formats the buffer before saving
;; ;(add-hook 'before-save-hook 'tide-format-before-save)

;; ;(add-hook 'typescript-mode-hook #'setup-tide-mode)
;; (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file tss.log"))
;; (setq tide-tsserver-executable "/home/dmd274/scripts/cygpath-tsserver")

;; ;; format options
;; (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
;; ----------------------------------------


;; ----------------------------------------
;; TSS Stuff

;; (require 'typescript)
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;; (require 'tss)

;; ;; Key binding
;; (setq tss-popup-help-key "C-:")
;; (setq tss-jump-to-definition-key "C->")
;; (setq tss-implement-definition-key "C-c i")

;; ;; Make config suit for you. About the config item, eval the following sexp.
;; ;; (customize-group "tss")

;; ;; Do setting recommemded configuration
;; (tss-config-default)
;; ----------------------------------------

;; ---- Writable Grep Buffers -----------------------------------------------------
(require 'wgrep)

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
(global-set-key "\M-csl" 'js-console-log-var)
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
		 ("\\.jsp$" . web-mode)
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
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
	("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(inhibit-startup-screen t)
 '(sass-indent-offset 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 120 :family fontFamily))))
 '(semantic-decoration-on-unknown-includes ((((class color) (background light)) (:background "#333333")))))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)

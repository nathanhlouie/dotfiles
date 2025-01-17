;;; init.el -*- lexical-binding: t -*-

(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
							  :ref nil :depth 1
							  :files (:defaults "elpaca-test.el" (:exclude "extensions"))
							  :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
	   (build (expand-file-name "elpaca/" elpaca-builds-directory))
	   (order (cdr elpaca-order))
	   (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
	(make-directory repo t)
	(when (< emacs-major-version 28) (require 'subr-x))
	(condition-case-unless-debug err
		(if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
				  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
												  ,@(when-let* ((depth (plist-get order :depth)))
													  (list (format "--depth=%d" depth) "--no-single-branch"))
												  ,(plist-get order :repo) ,repo))))
				  ((zerop (call-process "git" nil buffer t "checkout"
										(or (plist-get order :ref) "--"))))
				  (emacs (concat invocation-directory invocation-name))
				  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
										"--eval" "(byte-recompile-directory \".\" 0 'force)")))
				  ((require 'elpaca))
				  ((elpaca-generate-autoloads "elpaca" repo)))
			(progn (message "%s" (buffer-string)) (kill-buffer buffer))
		  (error "%s" (with-current-buffer buffer (buffer-string))))
	  ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
	(require 'elpaca)
	(elpaca-generate-autoloads "elpaca" repo)
	(load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(setq frame-inhibit-implied-resize t
	  frame-resize-pixelwise t
	  frame-title-format '("%b")
	  ring-bell-function 'ignore
	  split-width-threshold 300
	  visible-bell nil)

(setq pixel-scroll-precision-mode t
	  pixel-scroll-precision-use-momentum nil)

(setq inhibit-splash-screen t
	  inhibit-startup-buffer-menu t
	  inhibit-startup-echo-area-message user-login-name
	  inhibit-startup-message t
	  inhibit-startup-screen t
	  initial-buffer-choice t
	  initial-scratch-message "")

(setq pop-up-windows nil
	  use-dialog-box nil
	  use-file-dialog nil
	  use-short-answers t
	  show-help-function nil
	  warning-minimum-level :emergency)

(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

(menu-bar-mode 1)

(setq create-lockfiles nil
	  make-backup-files nil)

(setq auto-save-default t
	  auto-save-interval 200
	  auto-save-timeout 20)

(setq delete-by-moving-to-trash t)

(prefer-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "English")
(set-terminal-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq ispell-dictionary "en_US")

(global-auto-revert-mode t)
(global-auto-revert-non-file-buffers t)

(setq sentence-end-double-space nil)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(setq-default indent-tabs-mode nil)

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
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))

(elpaca-wait)

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

(setq cursor-in-non-selected-windows nil
      indicate-empty-lines nil
      use-dialog-box nil
      use-file-dialog nil
      use-short-answers t
      show-help-function nil
      warning-minimum-level :emergency)

(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

(if (display-graphic-p)
    (menu-bar-mode 1)
  (menu-bar-mode -1))

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

(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen t
	mac-option-key-is-meta nil
	mac-command-key-is-meta t
	mac-command-modifier 'meta
	mac-option-modifier nil
	mac-use-title-bar nil))

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(when (and (not (display-graphic-p))
	   (eq system-type 'darwin))
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

(global-auto-revert-mode t)

(setq sentence-end-double-space nil)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

(global-hl-line-mode 1)

(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(winner-mode 1)
(xterm-mouse-mode 1)

(use-package vertico
  :demand t
  :custom
  (enable-recursive-minibuffers t)
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-cycle t)
  :config
  (vertico-mode))

(use-package orderless
  :defer 1
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :defer 2
  :config
  (marginalia-mode))

(use-package consult
  :demand t
  :config
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref))

(use-package embark
  :demand t)

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package cape
  :commands (cape-file)
  :config
  (global-set-key (kbd "C-x C-f") 'cape-file)
  (global-set-key (kbd "C-x C-l") 'cape-line))

(use-package avy
  :demand t
  :config
  (avy-setup-default)
  (global-set-key (kbd "C-;") 'avy-goto-char)
  (global-set-key (kbd "M-g f") 'avy-goto-line)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1))

(use-package anzu
  :defer 10
  :config (global-anzu-mode))

(use-package corfu
  :ensure t
  :defer 5
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :config
  (global-corfu-mode))

(use-package projectile
  :demand t
  :config
  (add-to-list 'projectile-globally-ignored-directories "*node_modules")
  (projectile-mode))

(setq treesit-font-lock-level 4)

(set-face-attribute 'default nil :family "RobotoMono Nerd Font" :height 240 :weight 'medium)

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
      show-help-function nil)

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

(let ((auto-save-dir (concat user-emacs-directory "auto-save/")))
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir))
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t)))
  (setq tramp-auto-save-directory auto-save-dir))

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
(setq recentf-auto-cleanup 'never)
(savehist-mode 1)
(save-place-mode 1)
(winner-mode 1)
(xterm-mouse-mode 1)

(unload-feature 'eldoc t)
(setq custom-delayed-init-variables '())
(setq global-eldoc-mode nil)

(elpaca eldoc
  (require 'eldoc)
  (global-eldoc-mode))

(defun +elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

(defun +elpaca-seq-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package)))
(elpaca `(seq :build ,(+elpaca-seq-build-steps)))

(use-package jsonrpc
  :ensure (:wait t)
  :defer t)

(use-package vertico
  :demand t
  :bind (:map vertico-map ("C-q" . #'vertico-quick-insert))
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (vertico-scroll-margin 0)
  (vertico-count 10)
  (vertico-resize t)
  (vertico-cycle t)
  (vertico-multiform-commands '((consult-line buffer)
                                (consult-imenu reverse buffer)))
  :config
  (vertico-mode)
  (vertico-multiform-mode)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (defun +embark-live-vertico ()
    "Shrink Vertico minibuffer when `embark-live' is active."
    (when-let (win (and (string-prefix-p "*Embark Live" (buffer-name))
                        (active-minibuffer-window)))
      (with-selected-window win
        (when (and (bound-and-true-p vertico--input)
                   (fboundp 'vertico-multiform-unobtrusive))
          (vertico-multiform-unobtrusive)))))
  (add-hook 'embark-collect-mode-hook #'+embark-live-vertico))

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
  :demand t
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

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

(use-package flymake
  :config
  (flymake-mode))

(use-package eglot
  :defer t
  :config
  (add-hook 'eglot-server-initialized-hook #'flymake-mode))

(use-package apheleia
  :config
  (apheleia-global-mode))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package dape
  :config
  (dape-breakpoint-global-mode)
  (repeat-mode))

(use-package vterm)

(setq tramp-terminal-type "tramp")

(use-package transient
  :defer t)

(use-package magit
  :defer t
  :custom
  (magit-diff-refine-hunk 'all)
  :config
  (transient-bind-q-to-quit))

(use-package forge
  :after magit)

(use-package diff-hl
  :defer t
  :config
  (global-diff-hl-mode))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode))

(setq dired-mouse-drag-files t)
(setq dired-listing-switches "-alh")
(setq dired-kill-when-opening-new-dired-buffer t)

(setq treesit-font-lock-level 4)

(set-face-attribute 'default nil :family "RobotoMono Nerd Font" :height 240 :weight 'medium)

(use-package doom-modeline
  :defer 2
  :custom
  (doom-modeline-time-analogue-clock nil)
  (doom-modeline-time-icon nil)
  (doom-modeline-unicode-fallback nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-icon t)
  :config
  (doom-modeline-mode))

(use-package org
  :defer t)

(use-package olivetti
  :defer t)

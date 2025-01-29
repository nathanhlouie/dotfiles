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
  (vertico-multiform-categories '((embark-keybinding grid)
                                  (file flat)))
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
  :bind (("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ("M-y" . consult-yank-pop)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (register-preview-delay 0.5)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")
  (consult-preview-key 'any)
  (consult-line-numbers-widen t)
  :config
  (advice-add #'register-preview :override #'consult-register-window)
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

(use-package embark
  :demand t
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export))
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :defer 1
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :defer 1)

(use-package cape
  :demand t
  :bind ("C-c p" . cape-prefix-map)
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-line)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package avy
  :demand t
  :bind (("C-;" . avy-resume)
         ("M-g c" . avy-goto-char)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1))
  :config
  (avy-setup-default))

(use-package anzu
  :defer 10
  :config (global-anzu-mode))

(use-package corfu
  :demand t
  :custom
  (corfu-quit-no-match t)
  (global-corfu-minibuffer
   (lambda ()
     (not (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map)))))
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (completion-category-overrides '((eglot (styles orderless))
                                   (eglot-capf (styles orderless))))
  :config
  (keymap-unset corfu-map "RET")
  (global-corfu-mode))

(use-package corfu-candidate-overlay
  :demand t
  ;;:bind (("<tab>" . completion-at-point)
  ;;       ("C-<tab>" . corfu-candidate-overlay-complete-at-point))
  :config
  (corfu-candidate-overlay-mode))

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

(use-package which-key)

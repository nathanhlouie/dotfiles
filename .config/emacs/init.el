;;; init.el -*- no-byte-compile: t; lexical-binding: t; -*-

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

(elpaca-wait)

(setq ffap-machine-p-known 'reject)

(setq ad-redefinition-action 'accept)
(setq warning-suppress-types '((lexical-binding)))
(setq warning-minimum-level :error)

(setq enable-recursive-minibuffers t)

(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face
                  minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq idle-update-delay 1.0)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add #'yes-or-no-p :override #'y-or-n-p))
(defalias #'view-hello-file #'ignore)

(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(show-paren-mode +1)

(setq compilation-always-kill t
      compilation-ask-about-save nil
      compilation-scroll-output 'first-error)

(setq whitespace-line-column nil)

(setq rainbow-delimiters-max-face-count 5)

(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)

(setq truncate-string-ellipsis "…")

(setq read-process-output-max (* 512 1024))

(setq redisplay-skip-fontification-on-input t)

(setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)

(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))

(setq x-underline-at-descent-line nil)
(setq indicate-buffer-boundaries 'left)

(winner-mode 1)
(delete-selection-mode 1)
(electric-pair-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'visual-line-mode)
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

(setq delete-by-moving-to-trash (not noninteractive))
(setq find-file-suppress-same-file-warnings t)

(setq find-file-visit-truename t
      vc-follow-symlinks t)

(setq split-width-threshold 170
      split-height-threshold nil)

(setq window-divider-default-bottom-width 1
      window-divider-default-places t
      window-divider-default-right-width 1)

(setq create-lockfiles nil
      make-backup-files nil)

(setq auto-save-default t
      auto-save-include-big-deletions t
      auto-save-interval 200
      auto-save-timeout 20)

(let ((auto-save-dir (concat user-emacs-directory "auto-save/")))
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir))
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t)))
  (setq auto-save-list-file-prefix auto-save-dir)
  (setq tramp-auto-save-directory auto-save-dir))

(setq auto-save-include-big-deletions t)

(setq revert-without-query (list ".")
      auto-revert-stop-on-user-input nil
      auto-revert-verbose t)

(setq auto-revert-avoid-polling t
      auto-revert-interval 5
      auto-revert-check-vc-info t)

(setq global-auto-revert-non-file-buffers t)

(setq switch-to-buffer-obey-display-actions t)

(setq uniquify-buffer-name-style 'forward)

(setq comint-prompt-read-only t)
(setq comint-buffer-maximum-size 2048)

(setq confirm-nonexistent-file-or-buffer nil)

(setq vc-git-print-log-follow t)
(setq vc-make-backup-files nil)

(setq recentf-max-saved-items 300)
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup 'never)
(defun minimal-emacs--cleanup-hook ()
  "Run `recentf-cleanup' if `recentf' is loaded and `recentf-mode' is enabled."
  (when (and (featurep 'recentf)
             recentf-mode
             (fboundp 'recentf-cleanup))
    (recentf-cleanup)))
(add-hook 'kill-emacs-hook #'minimal-emacs--cleanup-hook)

(setq recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq save-place-limit 600)

(setq history-length 300)
(setq savehist-save-minibuffer-history t)

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(setq window-resize-pixelwise nil)

(setq resize-mini-windows 'grow-only)

(setq scroll-error-top-bottom t)

(setq scroll-preserve-screen-position t)

(setq scroll-conservatively 10)

(setq scroll-step 1)

(setq auto-window-vscroll nil)

(setq scroll-margin 0)

(setq hscroll-margin 2
      hscroll-step 1)

(setq pixel-scroll-precision-use-momentum nil)
(pixel-scroll-precision-mode 1)

(setq mouse-yank-at-point nil)

(setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

(blink-cursor-mode -1)

(setq blink-matching-paren nil)

(setq x-stretch-cursor nil)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq visible-bell nil)
(setq ring-bell-function #'ignore)

(setq delete-pair-blink-delay 0.03)

(setq-default left-fringe-width  8)
(setq-default right-fringe-width 8)

(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)

(setq-default word-wrap t)

(setq-default truncate-lines t)

(setq truncate-partial-width-windows nil)

(setq-default indent-tabs-mode nil
              tab-width 4)

(setq-default tab-always-indent 'complete)

(setq read-extended-command-predicate #'command-completion-default-include-p)

(setq comment-multi-line t)

(setq-default fill-column 80)

(setq sentence-end-double-space nil)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(setq require-final-newline t)

(setq kill-do-not-save-duplicates t)

(setq comment-empty-lines t)

(setq lazy-highlight-initial-delay 0)

(setq display-time-default-load-average nil)
(setq line-number-mode t)
(setq column-number-mode t)

(setq python-indent-guess-indent-offset-verbose nil)

(setq sh-indent-after-continuation 'always)

(setq dired-free-space nil
      dired-dwim-target t
      dired-deletion-confirmer 'y-or-n-p
      dired-filter-verbose nil
      dired-recursive-deletes 'top
      dired-recursive-copies  'always
      dired-create-destination-dirs 'ask
      dired-auto-revert-buffer #'dired-buffer-stale-p
      image-dired-thumb-size 150)

(setq dired-clean-confirm-killing-deleted-buffers nil)

(setq dired-omit-verbose nil)
(setq dired-omit-files (concat "\\`[.]?#\\|\\`[.][.]?\\'"
                               "\\|\\(?:\\.js\\)?\\.meta\\'"
                               "\\|\\.\\(?:elc|a\\|o\\|pyc\\|pyo\\|swp\\|class\\)\\'"
                               "\\|^\\.DS_Store\\'"
                               "\\|^\\.\\(?:svn\\|git\\)\\'"
                               "\\|^\\.ccls-cache\\'"
                               "\\|^__pycache__\\'"
                               "\\|^\\.project\\(?:ile\\)?\\'"
                               "\\|^flycheck_.*"
                               "\\|^flymake_.*"))

(setq ls-lisp-verbosity nil)
(setq ls-lisp-dirs-first t)

(setq global-text-scale-adjust-resizes-frames nil)

(setq treesit-font-lock-level 4)

(set-face-attribute 'default nil :family "RobotoMono Nerd Font" :height 240 :weight 'medium)

(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)

(setq apropos-do-all t)

(setq help-enable-completion-autoload nil)
(setq help-enable-autoload nil)
(setq help-enable-symbol-autoload nil)

(prefer-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq ispell-dictionary "en_US")

(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen t
        mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier nil
        mac-use-title-bar nil)
  (setq dired-use-ls-dired nil))

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

(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)

(use-package which-key
  :custom
  (which-key-idle-delay 0.2)
  :config
  (which-key-mode))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(setq tramp-terminal-type "tramp")

(use-package gcmh
  :demand t
  :config
  (gcmh-mode))

(use-package exec-path-from-shell
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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

(setq eglot-sync-connect 1
      eglot-autoshutdown t)
(setq eglot-extend-to-xref t)
(setq jsonrpc-event-hook nil)
(setq eglot-events-buffer-size 0)
(setq eglot-report-progress nil)
(fset #'jsonrpc--log-event #'ignore)

(setq eglot-events-buffer-config '(:size 0 :format full))

(setq flymake-fringe-indicator-position 'left-fringe)
(setq flymake-show-diagnostics-at-end-of-line nil)
(setq flymake-suppress-zero-counters t)
(setq flymake-wrap-around nil)

(use-package transient
  :defer t)

(use-package magit
  :defer t
  :bind
  (("C-c g b" . 'magit-branch)
   ("C-c g B" . 'magit-blame)
   ("C-c g c" . 'magit-clone)
   ("C-c g f" . 'magit-find-file)
   ("C-c g i" . 'magit-init)
   ("C-c g l" . 'magit-log-buffer-file)
   ("C-c g m" . 'magit-dispatch)
   ("C-c g S" . 'magit-stage-file)
   ("C-c g s" . 'magit-status)
   ("C-c g U" . 'magit-unstage-file))
  :custom
  (magit-diff-refine-hunk 'all)
  :config
  (transient-bind-q-to-quit))

(use-package forge
  :after magit
  :custom
  (forge-add-default-bindings nil)
  (forge-display-in-status-buffer nil)
  (forge-add-pullreq-refspec nil))

(use-package vertico
  :demand t
  :bind
  (:map vertico-map ("C-q" . #'vertico-quick-insert)
        ("RET" . #'vertico-directory-enter)
        ("DEL" . #'vertico-directory-delete-word)
        ("M-d" . #'vertico-directory-delete-char))
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (vertico-count 20)
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

(use-package consult
  :demand t
  :bind (([remap Info-search] . consult-info)
         ([rebind switch-to-buffer] . consult-buffer)
         ([remap goto-line] . consult-goto-line)
         ([remap yank-pop] . consult-yank-pop)
         ("M-s M-g" . consult-grep)
         ("M-s M-f" . consult-find)
         ("M-s M-o" . consult-outline)
         ("M-s M-l" . consult-line)
         ("M-s M-b" . consult-buffer))
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

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :demand t
  :config
  (marginalia-mode))

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
  :after (embark)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :demand t
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

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

(use-package flymake
  :demand t
  :config
  (add-hook 'prog-mode-hook #'flymake-mode))

(use-package eglot
  :demand t
  :config
  (add-hook 'prog-mode-hook #'eglot-ensure)
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "basedpyright-langserver" "--stdio")))

(use-package eglot-booster
  :ensure (:type git :host github :repo "jdtsmith/eglot-booster")
  :after (eglot)
  :config
  (eglot-booster-mode))

(use-package apheleia
  :demand t
  :config
  (add-hook 'prog-mode-hook #'apheleia-mode))

(use-package corfu
  :demand t
  :custom
  (global-corfu-minibuffer
   (lambda ()
     (not (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map)))))
  (corfu-quit-no-match t)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-preselect 'prompt)
  (completion-category-overrides '((eglot (styles orderless))
                                   (eglot-capf (styles orderless))))
  (text-mode-ispell-word-completion nil)
  :config
  (keymap-unset corfu-map "RET")
  (corfu-popupinfo-mode)
  (add-hook 'prog-mode-hook #'corfu-mode)
  (add-hook 'shell-mode-hook #'corfu-mode)
  (add-hook 'eshell-mode-hook (lambda ()
                                (setq-local corfu-auto nil)
                                (corfu-mode))))

(use-package diff-hl
  :demand t
  :config
  (add-hook 'prog-mode-hook #'diff-hl-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package indent-bars
  :demand t
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  :config
  (require 'indent-bars-ts)
  (add-hook 'prog-mode-hook #'indent-bars-mode))

(use-package editorconfig
  :defer t
  :config
  (defun oxcl/update-indent-bars-with-editorconfig (size)
    (when (bound-and-true-p indent-bars-mode)
      (setq indent-bars-spacing-override size)
      (indent-bars-reset)))
  (dolist (_mode editorconfig-indentation-alist)
    (let ((_varlist (cdr _mode)))
      (setcdr _mode (append '((_ . oxcl/update-indent-bars-with-editorconfig))
                            (if (listp _varlist) _varlist `(,_varlist))))))
  (editorconfig-mode t))

(use-package rg
  :demand t
  :bind ("C-x p /" . rg-project))

(use-package dape
  :defer t
  :config
  (repeat-mode))

(use-package eat
  :demand t
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-enable-mouse t))

(use-package auctex
  :elpaca (auctex :repo "https://git.savannah.gnu.org/git/auctex.git" :branch "main"
                  :pre-build (("make" "elpa"))
                  :build (:not elpaca--compile-info) ;; Make will take care of this step
                  :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
                  :version (lambda (_) (require 'auctex) AUCTeX-version))
  :custom
  (TeX-auto-save t)
  (TeXX-parse-self t)
  :config
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode))

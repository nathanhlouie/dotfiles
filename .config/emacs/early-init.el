;;; early-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          #'(lambda () (setq gc-cons-threshold (* 16 1024 1024)
                             gc-cons-percentage 0.1)))

(set-language-environment "UTF-8")
(setq default-input-method nil)

(setq load-prefer-newer t)
(setq inhibit-compacting-font-caches t)

(unless (daemonp)
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (set-default-toplevel-value 'file-name-handler-alist
                                (if (eval-when-compile
                                      (locate-file-internal "calc-loaddefs.el"
                                                            load-path))
                                    nil
                                  (list (rassq 'jka-compr-handler old-value))))
    (put 'file-name-handler-alist 'initial-value old-value)
    (define-advice command-line-1 (:around (fn args-left) respect-file-handlers)
      (let ((file-name-handler-alist
             (if args-left old-value file-name-handler-alist)))
        (funcall fn args-left)))
    (add-hook 'emacs-startup-hook
              (lambda ()
                (set-default-toplevel-value
                 'file-name-handler-alist
                 (delete-dups (append file-name-handler-alist old-value))))
              101))

  (unless noninteractive

    (setq-default inhibit-message t)

    (defun minimal-emacs--reset-inhibited-vars-h ()
      (setq-default inhibit-message nil)
      (remove-hook 'post-command-hook #'minimal-emacs--reset-inhibited-vars-h))

    (add-hook 'post-command-hook
              #'minimal-emacs--reset-inhibited-vars-h -100)

    (put 'mode-line-format 'initial-value
         (default-toplevel-value 'mode-line-format))
    (setq-default mode-line-format nil)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (setq mode-line-format nil)))

    (defun minimal-emacs--startup-load-user-init-file (fn &rest args)
      "Advice for startup--load-user-init-file to reset mode-line-format."
      (unwind-protect
          (apply fn args)
        (setq-default inhibit-message nil)
        (unless (default-toplevel-value 'mode-line-format)
          (setq-default mode-line-format
                        (get 'mode-line-format 'initial-value)))))

    (advice-add 'startup--load-user-init-file :around
                #'minimal-emacs--startup-load-user-init-file))

  (setq frame-inhibit-implied-resize t)
  (setq auto-mode-case-fold nil)

  (setq inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message user-login-name)
  (setq initial-buffer-choice nil
        inhibit-startup-buffer-menu t
        inhibit-x-resources t)

  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)

  (setq bidi-inhibit-bpa t)

  (advice-add #'display-startup-echo-area-message :override #'ignore)

  (advice-add #'display-startup-screen :override #'ignore)

  (setq initial-major-mode 'fundamental-mode
        initial-scratch-message nil)

  (unless (eq system-type 'darwin)
    (setq command-line-ns-option-alist nil))
  (unless (memq initial-window-system '(x pgtk))
    (setq command-line-x-option-alist nil)))

(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (setq native-comp-jit-compilation t
          package-native-compile t)
  (setq features (delq 'native-compile features)))

(setq native-comp-async-report-warnings-errors 'silent)
(setq native-comp-warning-on-missing-source nil)

(setq debug-on-error nil
      jka-compr-verbose nil)

(setq byte-compile-warnings nil)
(setq byte-compile-verbose nil)

(setq frame-title-format "%b"
      icon-title-format "%b")

(setq inhibit-splash-screen t)

(push '(menu-bar-lines . 0) default-frame-alist)
(unless (memq window-system '(mac ns))
  (setq menu-bar-mode nil))

(unless (daemonp)
  (unless noninteractive
    (when (fboundp 'tool-bar-setup)
      (advice-add #'tool-bar-setup :override #'ignore)
      (define-advice startup--load-user-init-file
          (:after (&rest _) minimal-emacs-setup-toolbar)
        (advice-remove #'tool-bar-setup #'ignore)
        (when tool-bar-mode
          (tool-bar-setup))))))
(push '(tool-bar-lines . 0) default-frame-alist)
(setq tool-bar-mode nil)

(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

(setq use-file-dialog nil)
(setq use-dialog-box nil)

(setq package-enable-at-startup nil
      use-package-always-ensure t)

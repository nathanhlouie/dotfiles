;;; early-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(setq package-enable-at-startup nil)

(setq site-run-file nil
      inhibit-default-init t
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'after-init-hook
          #'(lambda () (setq gc-cons-threshold (* 8 1024 1024)
                             gc-cons-percentage 0.1)))

(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-compile-prune-cache t))

;;; early-init.el --- Early initialization file -*- lexical-binding: t; -*-


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Things that load before the initial frame is rendered should go here.

;;; Code:

;; No site-wide run-time initializations / default library
(setq site-run-file nil
      inhibit-default-init t)

;; Very large threshold for garbage collector during initialization which
;; will be reset later
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          #'(lambda () (setq gc-cons-threshold (* 16 1024 1024)
                             gc-cons-percentage 0.1)))

;; Use UTF-8 instead of English
(set-language-environment "UTF-8")

;; Don't resize the frame when UI elements open
(setq frame-inhibit-implied-resize t)

;; Don't try again to match case insensitive through alist
(setq auto-mode-case-fold nil)

;; Disable startup screens and messages
(setq inhibit-splash-screen t
      inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-x-resources t)

;; Set initial buffer / major-mode / message
(setq initial-buffer-choice t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Native compilation
(when (native-comp-available-p)
  (setopt native-comp-async-report-warnings-errors 'silent
          native-comp-warning-on-missing-source nil
          package-native-compile t))


;; Verbosity settings
(setq jka-compr-verbose nil)
(setq byte-compile-verbose nil
      byte-compile-warnings nil)

;; Hide UI elements
(setq frame-title-format "%b"
      icon-title-format "%b")
(push '(menu-bar-lines . 0) default-frame-alist)
(if (display-graphic-p)
    (setq menu-bar-mode t)
  (setq menu-bar-mode nil))
(setq tool-bar-mode nil)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(setq use-dialog-box nil)
(setq use-file-dialog nil)

;; Elpaca
(setq package-enable-at-startup nil)

;;; early-init.el ends here

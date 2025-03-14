;;; theme.el --- Define theme -*- lexical-binding: t; -*-


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

;; Derives all the faces in various modes.

;;; Code:

(require 'faces)

(defcustom theme-var nil
  "Variable which sets the default startup theme as light or dark."
  :group 'config
  :type 'string)

;; When we set a face, we take care of removing any previous settings
(defun set-face (face style)
  "Reset FACE and make it inherit STYLE."
  (if (facep face)
      (set-face-attribute face nil
                          :foreground 'unspecified :background 'unspecified
                          :family     'unspecified :slant      'unspecified
                          :weight     'unspecified :height     'unspecified
                          :underline  'unspecified :overline   'unspecified
                          :box        'unspecified :inherit    style)))

(defun theme--basics ()
  "Derive basic Emacs faces from faces and color-theme."

  (set-foreground-color color-foreground)
  (set-background-color color-background)

  ;; XXX the following seems to be a no-op, should it be removed?
  (set-face-attribute 'default nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)
                      :weight     'light
                      :family     (face-attribute 'face-default :family)
                      :height     (face-attribute 'face-default :height))

  (if (display-graphic-p)
      (set-face-attribute 'bold nil :weight 'regular)
    (set-face-attribute 'bold nil :weight 'bold))

  ;; Structural
  (set-face 'bold                                     'face-strong)
  (set-face 'italic                                    'face-faded)
  (set-face 'bold-italic                              'face-strong)
  (set-face 'region                                   'face-subtle)
  (set-face 'highlight                                'face-subtle)
  (set-face 'fixed-pitch-serif                       'face-default)
  (set-face 'cursor                                  'face-default)
  (if 'font-family-proportional
      (set-face-attribute 'variable-pitch nil
                          :foreground (face-foreground 'default)
                          :background (face-background 'default)
                          :family     (face-attribute 'face-variable-pitch :family)
                          :height     (face-attribute 'face-variable-pitch :height))
    (set-face 'variable-pitch                     'face-default))

  (set-face-attribute 'cursor nil
                      :background (face-foreground 'face-default))
  (set-face-attribute 'window-divider nil
                      :foreground (face-background 'face-default))
  (set-face-attribute 'window-divider-first-pixel nil
                      :foreground color-background)
  (set-face-attribute 'window-divider-last-pixel nil
                      :foreground color-background)
  (set-face-foreground 'vertical-border color-subtle)

  ;; Semantic
  (set-face 'shadow                                    'face-faded)
  (set-face 'success                                 'face-salient)
  (set-face 'warning                                  'face-popout)
  (set-face 'error                                  'face-critical)
  (set-face 'match                                    'face-popout)

  ;; General
  (set-face 'buffer-menu-buffer                       'face-strong)
  (set-face 'minibuffer-prompt                        'face-strong)
  (set-face 'link                                    'face-salient)
  (set-face 'fringe                                    'face-faded)
  (set-face-attribute 'fringe nil
                      :foreground (face-background 'face-subtle)
                      :background (face-background 'default))
  (set-face 'isearch                                  'face-strong)
  (set-face 'isearch-fail                              'face-faded)
  (set-face 'lazy-highlight                           'face-subtle)
  (set-face 'trailing-whitespace                      'face-subtle)
  (set-face 'show-paren-match                         'face-popout)
  (set-face 'show-paren-mismatch                           'face-normal)
  (set-face-attribute 'tooltip nil                         :height 0.85)
  (set-face 'secondary-selection                      'face-subtle)
  (set-face 'completions-common-part                   'face-faded)
  (set-face 'completions-first-difference            'face-default))
(defun theme--font-lock ()
  "Derive font-lock faces from faces."
  (set-face 'font-lock-comment-face                    'face-faded)
  (set-face 'font-lock-doc-face                        'face-faded)
  (set-face 'font-lock-string-face                    'face-popout)
  (set-face 'font-lock-constant-face                 'face-salient)
  (set-face 'font-lock-warning-face                   'face-popout)
  (set-face 'font-lock-function-name-face             'face-strong)
  (set-face 'font-lock-variable-name-face             'face-strong)
  (set-face 'font-lock-builtin-face                  'face-salient)
  (set-face 'font-lock-type-face                     'face-salient)
  (set-face 'font-lock-keyword-face                  'face-salient))


(defun theme--mode-line ()
  "Derive mode-line and header-line faces from faces."
  (set-face-attribute 'mode-line nil
                      :height 0.1
                      :foreground (if (display-graphic-p)
                                      (face-background 'face-default)
                                    (face-foreground 'face-default))
                      :background (face-background 'face-default)
                      :underline  (if (display-graphic-p)
                                      (face-background 'face-subtle)
                                    t)
                      :overline nil
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :height 0.1
                      :foreground (if (display-graphic-p)
                                      (face-background 'face-default)
                                    (face-foreground 'face-default))
                      :background (face-background 'face-default)
                      :underline (if (display-graphic-p)
                                     (face-background 'face-subtle)
                                   t)
                      :overline nil
                      :inherit nil
                      :box nil)

  (set-face-attribute 'header-line nil
                      :weight 'light
                      :foreground (face-foreground 'face-default)
                      :background (face-background 'face-default)

                      :overline nil
                      :underline nil
                      :box nil
                      :box `(:line-width 1
                                         :color ,(face-background 'face-default)
                                         :style nil)
                      :inherit nil)

  (set-face-attribute 'internal-border nil
                      :background (face-background 'face-default)))


(defun theme--minibuffer ()
  "Derive minibuffer / echo area faces from nano faces."
  ;; Minibuffer / echo area
  (dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                        " *Minibuf-1*" " *Echo Area 1*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (face-remap-add-relative 'default 'face-faded)))))


(defun theme--hl-line ()
  "Derive hl-line faces from nano faces."
  (with-eval-after-load 'hl-line
    (set-face-attribute 'hl-line nil
                        :background color-highlight)))



(defun theme--buttons ()
  "Derive button faces from nano faces."
  ;; Buttons
  (with-eval-after-load 'cus-edit
    (set-face-attribute 'custom-button nil
                        :foreground (face-foreground 'face-faded)
                        :background (face-background 'face-default)
                        :box `(:line-width 1
                                           :color ,(face-foreground 'face-faded)
                                           :style nil))
    (set-face-attribute 'custom-button-mouse nil
                        :foreground (face-foreground 'face-faded)
                        :background (face-background 'face-subtle)
                        :box `(:line-width 1
                                           :color ,(face-foreground 'face-faded)
                                           :style nil))
    (set-face-attribute 'custom-button-pressed nil
                        :foreground (face-background 'default)
                        :background (face-foreground 'face-salient)
                        :inherit 'face-salient
                        :box `(:line-width 1
                                           :color ,(face-foreground 'face-salient)
                                           :style nil)
                        :inverse-video nil)))


(defun theme--info ()
  "Derive info faces from nano faces."
  (with-eval-after-load 'info
    (set-face 'info-menu-header                       'face-strong)
    (set-face 'info-header-node                      'face-default)
    (set-face 'info-index-match                      'face-salient)
    (set-face 'Info-quoted                             'face-faded)
    (set-face 'info-title-1                           'face-strong)
    (set-face 'info-title-2                           'face-strong)
    (set-face 'info-title-3                           'face-strong)
    (set-face 'info-title-4                           'face-strong)))


(defun theme--speedbar ()
  "Derive speedbar faces from nano faces."
  (with-eval-after-load 'speedbar
    (set-face 'speedbar-button-face                    'face-faded)
    (set-face 'speedbar-directory-face                'face-strong)
    (set-face 'speedbar-file-face                    'face-default)
    (set-face 'speedbar-highlight-face             'face-highlight)
    (set-face 'speedbar-selected-face                 'face-subtle)
    (set-face 'speedbar-separator-face                 'face-faded)
    (set-face 'speedbar-tag-face                       'face-faded)))


(defun theme--bookmark ()
  "Derive bookmark faces from nano faces."
  (with-eval-after-load 'bookmark
    (set-face 'bookmark-menu-heading                  'face-strong)
    (set-face 'bookmark-menu-bookmark                'face-salient)))


(defun theme--message ()
  "Derive message faces from nano faces."
  (with-eval-after-load 'message
    (unless (version< emacs-version "27.0")
      (set-face 'message-cited-text-1                  'face-faded)
      (set-face 'message-cited-text-2                  'face-faded)
      (set-face 'message-cited-text-3                  'face-faded)
      (set-face 'message-cited-text-4                 'face-faded))
    (set-face 'message-cited-text                      'face-faded)
    (set-face 'message-header-cc                     'face-default)
    (set-face 'message-header-name                    'face-strong)
    (set-face 'message-header-newsgroups             'face-default)
    (set-face 'message-header-other                  'face-default)
    (set-face 'message-header-subject                'face-salient)
    (set-face 'message-header-to                     'face-salient)
    (set-face 'message-header-xheader                'face-default)
    (set-face 'message-mml                            'face-popout)
    (set-face 'message-separator                       'face-faded)))


(defun theme--outline ()
  "Derive outline faces from nano faces."
  (with-eval-after-load 'outline
    (set-face 'outline-1                              'face-strong)
    (set-face 'outline-2                              'face-strong)
    (set-face 'outline-3                              'face-strong)
    (set-face 'outline-4                              'face-strong)
    (set-face 'outline-5                              'face-strong)
    (set-face 'outline-6                              'face-strong)
    (set-face 'outline-7                              'face-strong)
    (set-face 'outline-8                              'face-strong)))


(defun theme--customize ()
  "Derive customize faces from nano faces."
  (with-eval-after-load 'cus-edit
    (set-face 'widget-field                           'face-subtle)
    (set-face 'widget-button                          'face-strong)
    (set-face 'widget-single-line-field               'face-subtle)
    (set-face 'custom-group-subtitle                  'face-strong)
    (set-face 'custom-group-tag                       'face-strong)
    (set-face 'custom-group-tag-1                     'face-strong)
    (set-face 'custom-comment                          'face-faded)
    (set-face 'custom-comment-tag                      'face-faded)
    (set-face 'custom-changed                        'face-salient)
    (set-face 'custom-modified                       'face-salient)
    (set-face 'custom-face-tag                        'face-strong)
    (set-face 'custom-variable-tag                    'face-strong)
    (set-face 'custom-invalid                         'face-popout)
    (set-face 'custom-visibility                     'face-salient)
    (set-face 'custom-state                          'face-salient)
    (set-face 'custom-link                           'face-salient)))


(defun theme--package ()
  "Derive package faces from nano faces."
  (with-eval-after-load 'package
    (set-face 'package-description                   'face-default)
    (set-face 'package-help-section-name             'face-default)
    (set-face 'package-name                          'face-salient)
    (set-face 'package-status-avail-obso               'face-faded)
    (set-face 'package-status-available              'face-default)
    (set-face 'package-status-built-in               'face-salient)
    (set-face 'package-status-dependency             'face-salient)
    (set-face 'package-status-disabled                 'face-faded)
    (set-face 'package-status-external               'face-default)
    (set-face 'package-status-held                   'face-default)
    (set-face 'package-status-incompat                 'face-faded)
    (set-face 'package-status-installed              'face-salient)
    (set-face 'package-status-new                    'face-default)
    (set-face 'package-status-unsigned               'face-default))


  ;; Button face is hardcoded, we have to redefine the relevant function
  (defun package-make-button (text &rest properties)
    "Insert button labeled TEXT with button PROPERTIES at point.
PROPERTIES are passed to `insert-text-button', for which this
function is a convenience wrapper used by `describe-package-1'."
    (let ((button-text (if (display-graphic-p)
                           text (concat "[" text "]")))
          (button-face (if (display-graphic-p)
                           `(:box `(:line-width 1
                                                :color ,color-subtle
                                                :style nil)
                                  :foreground ,color-faded
                                  :background ,color-subtle)
                         'link)))
      (apply #'insert-text-button button-text
             'face button-face 'follow-link t properties))))


(defun theme--flyspell ()
  "Derive flyspell faces from nano faces."
  (with-eval-after-load 'flyspell
    (set-face 'flyspell-duplicate                     'face-popout)
    (set-face 'flyspell-incorrect                     'face-popout)))


(defun theme--ido ()
  "Derive ido faces from nano faces."
  (with-eval-after-load 'ido
    (set-face 'ido-first-match                       'face-salient)
    (set-face 'ido-only-match                          'face-faded)
    (set-face 'ido-subdir                             'face-strong)))


(defun theme--diff ()
  "Derive diff faces from nano faces."
  (with-eval-after-load 'diff-mode
    (set-face 'diff-header                             'face-faded)
    (set-face 'diff-file-header                       'face-strong)
    (set-face 'diff-context                          'face-default)
    (set-face 'diff-removed                            'face-faded)
    (set-face 'diff-changed                           'face-popout)
    (set-face 'diff-added                            'face-salient)
    (set-face 'diff-refine-added                    '(face-salient
                                                      face-strong))
    (set-face 'diff-refine-changed                    'face-popout)
    (set-face 'diff-refine-removed                    'face-faded)
    (set-face-attribute     'diff-refine-removed nil :strike-through t)))


(defun theme--term ()
  "Derive term faces from nano faces, and material theme colors."
  (with-eval-after-load 'term
    (set-face 'term-bold                                   'face-strong)
    (set-face-attribute 'term-color-black nil
                        :foreground (face-foreground 'face-default)
                        :background (face-foreground 'face-default))
    (set-face-attribute 'term-color-white nil
                        :foreground (face-background 'face-default)
                        :background (face-background 'face-default))
    (set-face-attribute 'term-color-blue nil
                        :foreground "#42A5F5"
                        :background "#BBDEFB")
    (set-face-attribute 'term-color-cyan nil
                        :foreground "#26C6DA"
                        :background "#B2EBF2")
    (set-face-attribute 'term-color-green nil
                        :foreground "#66BB6A"
                        :background "#C8E6C9")
    (set-face-attribute 'term-color-magenta nil
                        :foreground "#AB47BC"
                        :background "#E1BEE7")
    (set-face-attribute 'term-color-red nil
                        :foreground "#EF5350"
                        :background "#FFCDD2")
    (set-face-attribute 'term-color-yellow nil
                        :foreground "#FFEE58"
                        :background "#FFF9C4")))


(defun theme--calendar ()
  "Derive calendar faces from nano faces."
  (with-eval-after-load 'calendar
    (set-face 'calendar-today                         'face-strong)))


(defun theme--agenda ()
  "Derive agenda faces from nano faces."
  (with-eval-after-load 'org-agenda
    (set-face 'org-agenda-calendar-event             'face-default)
    (set-face 'org-agenda-calendar-sexp              'face-salient)
    (set-face 'org-agenda-clocking                     'face-faded)
    (set-face 'org-agenda-column-dateline              'face-faded)
    (set-face 'org-agenda-current-time                'face-strong)
    (set-face 'org-agenda-date                       'face-salient)
    (set-face 'org-agenda-date-today                '(face-salient
                                                      face-strong))
    (set-face 'org-agenda-date-weekend                 'face-faded)
    (set-face 'org-agenda-diary                        'face-faded)
    (set-face 'org-agenda-dimmed-todo-face             'face-faded)
    (set-face 'org-agenda-done                         'face-faded)
    (set-face 'org-agenda-filter-category              'face-faded)
    (set-face 'org-agenda-filter-effort                'face-faded)
    (set-face 'org-agenda-filter-regexp                'face-faded)
    (set-face 'org-agenda-filter-tags                  'face-faded)
    ;;  (set-face 'org-agenda-property-face                'face-faded)
    (set-face 'org-agenda-restriction-lock             'face-faded)
    (set-face 'org-agenda-structure                   'face-strong)))


(defun theme--org ()
  "Derive org faces from nano faces."
  (with-eval-after-load 'org
    (set-face 'org-archived                            'face-faded)

    (set-face 'org-block                                       'hl-line)
    (set-face 'org-block-begin-line                    'face-faded)
    (set-face 'org-block-end-line                      'face-faded)
    (unless (version< emacs-version "27.0")
      (set-face-attribute 'org-block nil                      :extend t)
      (set-face-attribute 'org-block-begin-line nil           :extend t)
      (set-face-attribute 'org-block-end-line nil             :extend t))

    (set-face 'org-checkbox                            'face-faded)
    (set-face 'org-checkbox-statistics-done            'face-faded)
    (set-face 'org-checkbox-statistics-todo            'face-faded)
    (set-face 'org-clock-overlay                       'face-faded)
    (set-face 'org-code                                'face-faded)
    (set-face 'org-column                              'face-faded)
    (set-face 'org-column-title                        'face-faded)
    (set-face 'org-date                                'face-faded)
    (set-face 'org-date-selected                       'face-faded)
    (set-face 'org-default                             'face-faded)
    (set-face 'org-document-info                       'face-faded)
    (set-face 'org-document-info-keyword               'face-faded)
    (set-face 'org-document-title                      'face-faded)
    (set-face 'org-done                              'face-default)
    (set-face 'org-drawer                              'face-faded)
    (set-face 'org-ellipsis                            'face-faded)
    (set-face 'org-footnote                            'face-faded)
    (set-face 'org-formula                             'face-faded)
    (set-face 'org-headline-done                       'face-faded)
    ;; (set-face 'org-hide                             'face-faded)
    ;; (set-face 'org-indent                           'face-faded)
    (set-face 'org-latex-and-related                   'face-faded)
    (set-face 'org-level-1                            'face-strong)
    (set-face 'org-level-2                            'face-strong)
    (set-face 'org-level-3                            'face-strong)
    (set-face 'org-level-4                            'face-strong)
    (set-face 'org-level-5                            'face-strong)
    (set-face 'org-level-6                            'face-strong)
    (set-face 'org-level-7                            'face-strong)
    (set-face 'org-level-8                            'face-strong)
    (set-face 'org-link                              'face-salient)
    (set-face 'org-list-dt                             'face-faded)
    (set-face 'org-macro                               'face-faded)
    (set-face 'org-meta-line                           'face-faded)
    (set-face 'org-mode-line-clock                     'face-faded)
    (set-face 'org-mode-line-clock-overrun             'face-faded)
    (set-face 'org-priority                            'face-faded)
    (set-face 'org-property-value                      'face-faded)
    (set-face 'org-quote                               'face-faded)
    (set-face 'org-scheduled                           'face-faded)
    (set-face 'org-scheduled-previously                'face-faded)
    (set-face 'org-scheduled-today                   '(face-salient
                                                       +                                                      face-strong))
    (set-face 'org-sexp-date                           'face-faded)
    (set-face 'org-special-keyword                     'face-faded)
    (set-face 'org-table                               'face-faded)
    (set-face 'org-tag                                'face-popout)
    (set-face 'org-tag-group                           'face-faded)
    (set-face 'org-target                              'face-faded)
    (set-face 'org-time-grid                           'face-faded)
    (set-face 'org-todo                              'face-salient)
    (set-face 'org-upcoming-deadline                 'face-default)
    (set-face 'org-verbatim                           'face-popout)
    (set-face 'org-verse                               'face-faded)
    (set-face 'org-warning                            'face-popout)))


(defun theme--mu4e ()
  "Derive mu4e faces from nano faces."
  (with-eval-after-load 'mu4e
    (set-face 'mu4e-attach-number-face                'face-strong)
    (set-face 'mu4e-cited-1-face                       'face-faded)
    (set-face 'mu4e-cited-2-face                       'face-faded)
    (set-face 'mu4e-cited-3-face                       'face-faded)
    (set-face 'mu4e-cited-4-face                       'face-faded)
    (set-face 'mu4e-cited-5-face                       'face-faded)
    (set-face 'mu4e-cited-6-face                       'face-faded)
    (set-face 'mu4e-cited-7-face                       'face-faded)
    (set-face 'mu4e-compose-header-face                'face-faded)
    (set-face 'mu4e-compose-separator-face             'face-faded)
    (set-face 'mu4e-contact-face                     'face-salient)
    (set-face 'mu4e-context-face                       'face-faded)
    (set-face 'mu4e-draft-face                         'face-faded)
    (set-face 'mu4e-flagged-face                      'face-popout)
    (set-face 'mu4e-footer-face                        'face-faded)
    (set-face 'mu4e-forwarded-face                   'face-default)
    (set-face 'mu4e-header-face                      'face-default)
    (set-face 'mu4e-header-highlight-face                      'hl-line)
    (set-face 'mu4e-header-key-face                   'face-strong)
    (set-face 'mu4e-header-marks-face                  'face-faded)
    (set-face 'mu4e-header-title-face                 'face-strong)
    (set-face 'mu4e-header-value-face                'face-default)
    (set-face 'mu4e-highlight-face                    'face-popout)
    (set-face 'mu4e-link-face                        'face-salient)
    (set-face 'mu4e-modeline-face                      'face-faded)
    (set-face 'mu4e-moved-face                         'face-faded)
    (set-face 'mu4e-ok-face                            'face-faded)
    (set-face 'mu4e-region-code                        'face-faded)
    (set-face 'mu4e-replied-face                     'face-default)
    (set-face 'mu4e-special-header-value-face        'face-default)
    (set-face 'mu4e-system-face                        'face-faded)
    (set-face 'mu4e-title-face                        'face-strong)
    (set-face 'mu4e-trashed-face                       'face-faded)
    (set-face 'mu4e-unread-face                       'face-strong)
    ;;(set-face-attribute 'mu4e-unread-face nil :weight 'regular)
    (set-face 'mu4e-url-number-face                    'face-faded)
    (set-face 'mu4e-view-body-face                   'face-default)
    (set-face 'mu4e-warning-face                      'face-popout)))


(defun theme--elfeed ()
  "Derive elfeed faces from nano faces."
  (with-eval-after-load 'elfeed
    (set-face 'elfeed-log-date-face                    'face-faded)
    (set-face 'elfeed-log-info-level-face            'face-default)
    (set-face 'elfeed-log-debug-level-face           'face-default)
    (set-face 'elfeed-log-warn-level-face             'face-popout)
    (set-face 'elfeed-log-error-level-face            'face-popout)
    (set-face 'elfeed-search-tag-face                  'face-faded)
    (set-face 'elfeed-search-date-face                 'face-faded)
    (set-face 'elfeed-search-feed-face               'face-salient)
    (set-face 'elfeed-search-filter-face               'face-faded)
    (set-face 'elfeed-search-last-update-face        'face-salient)
    (set-face 'elfeed-search-title-face              'face-default)
    (set-face 'elfeed-search-tag-face                  'face-faded)
    (set-face 'elfeed-search-unread-count-face        'face-strong)
    (set-face 'elfeed-search-unread-title-face        'face-strong)))

(defun theme--deft ()
  "Derive deft faces from nano faces."
  (with-eval-after-load 'deft
    (set-face 'deft-filter-string-error-face         'face-popout)
    (set-face 'deft-filter-string-face              'face-default)
    (set-face 'deft-header-face                     'face-salient)
    (set-face 'deft-separator-face                    'face-faded)
    (set-face 'deft-summary-face                      'face-faded)
    (set-face 'deft-time-face                       'face-salient)
    (set-face 'deft-title-face                       'face-strong)))

(defun theme--rst ()
  "Derive rst faces from nano faces."
  (with-eval-after-load 'rst
    (set-face 'rst-adornment                           'face-faded)
    (set-face 'rst-block                             'face-default)
    (set-face 'rst-comment                             'face-faded)
    (set-face 'rst-definition                        'face-salient)
    (set-face 'rst-directive                         'face-salient)
    (set-face 'rst-emphasis1                           'face-faded)
    (set-face 'rst-emphasis2                          'face-strong)
    (set-face 'rst-external                          'face-salient)
    (set-face 'rst-level-1                            'face-strong)
    (set-face 'rst-level-2                            'face-strong)
    (set-face 'rst-level-3                            'face-strong)
    (set-face 'rst-level-4                            'face-strong)
    (set-face 'rst-level-5                            'face-strong)
    (set-face 'rst-level-6                            'face-strong)
    (set-face 'rst-literal                           'face-salient)
    (set-face 'rst-reference                         'face-salient)
    (set-face 'rst-transition                        'face-default)))


(defun theme--markdown ()
  "Derive markdown faces from nano faces."
  (with-eval-after-load 'markdown-mode
    (set-face 'markdown-blockquote-face              'face-default)
    (set-face 'markdown-bold-face                     'face-strong)
    (set-face 'markdown-code-face                    'face-default)
    (set-face 'markdown-comment-face                   'face-faded)
    (set-face 'markdown-footnote-marker-face         'face-default)
    (set-face 'markdown-footnote-text-face           'face-default)
    (set-face 'markdown-gfm-checkbox-face            'face-default)
    (set-face 'markdown-header-delimiter-face          'face-faded)
    (set-face 'markdown-header-face                   'face-strong)
    (set-face 'markdown-header-face-1                 'face-strong)
    (set-face 'markdown-header-face-2                 'face-strong)
    (set-face 'markdown-header-face-3                 'face-strong)
    (set-face 'markdown-header-face-4                 'face-strong)
    (set-face 'markdown-header-face-5                 'face-strong)
    (set-face 'markdown-header-face-6                'face-strong)
    (set-face 'markdown-header-rule-face             'face-default)
    (set-face 'markdown-highlight-face               'face-default)
    (set-face 'markdown-hr-face                      'face-default)
    (set-face 'markdown-html-attr-name-face          'face-default)
    (set-face 'markdown-html-attr-value-face         'face-default)
    (set-face 'markdown-html-entity-face             'face-default)
    (set-face 'markdown-html-tag-delimiter-face      'face-default)
    (set-face 'markdown-html-tag-name-face           'face-default)
    (set-face 'markdown-inline-code-face              'face-popout)
    (set-face 'markdown-italic-face                    'face-faded)
    (set-face 'markdown-language-info-face           'face-default)
    (set-face 'markdown-language-keyword-face        'face-default)
    (set-face 'markdown-line-break-face              'face-default)
    (set-face 'markdown-link-face                    'face-salient)
    (set-face 'markdown-link-title-face              'face-default)
    (set-face 'markdown-list-face                      'face-faded)
    (set-face 'markdown-markup-face                    'face-faded)
    (set-face 'markdown-math-face                    'face-default)
    (set-face 'markdown-metadata-key-face              'face-faded)
    (set-face 'markdown-metadata-value-face            'face-faded)
    (set-face 'markdown-missing-link-face            'face-default)
    (set-face 'markdown-plain-url-face               'face-default)
    (set-face 'markdown-pre-face                     'face-default)
    (set-face 'markdown-reference-face               'face-salient)
    (set-face 'markdown-strike-through-face            'face-faded)
    (set-face 'markdown-table-face                   'face-default)
    (set-face 'markdown-url-face                     'face-salient)))


(defun theme--ivy ()
  "Derive ivy faces from nano faces."
  (with-eval-after-load 'ivy
    (set-face 'ivy-action                              'face-faded)
    (set-face 'ivy-completions-annotations             'face-faded)
    (set-face 'ivy-confirm-face                        'face-faded)
    (set-face 'ivy-current-match    '(face-strong face-subtle))
    (set-face 'ivy-cursor                             'face-strong)
    (set-face 'ivy-grep-info                          'face-strong)
    (set-face 'ivy-grep-line-number                    'face-faded)
    (set-face 'ivy-highlight-face                     'face-strong)
    (set-face 'ivy-match-required-face                 'face-faded)
    (set-face 'ivy-minibuffer-match-face-1             'face-faded)
    (set-face 'ivy-minibuffer-match-face-2             'face-faded)
    (set-face 'ivy-minibuffer-match-face-3             'face-faded)
    (set-face 'ivy-minibuffer-match-face-4             'face-faded)
    (set-face 'ivy-minibuffer-match-highlight         'face-strong)
    (set-face 'ivy-modified-buffer                    'face-popout)
    (set-face 'ivy-modified-outside-buffer            'face-strong)
    (set-face 'ivy-org                                 'face-faded)
    (set-face 'ivy-prompt-match                        'face-faded)
    (set-face 'ivy-remote                            'face-default)
    (set-face 'ivy-separator                           'face-faded)
    (set-face 'ivy-subdir                              'face-faded)
    (set-face 'ivy-virtual                             'face-faded)
    (set-face 'ivy-yanked-word                         'face-faded)))

(defun theme--helm ()
  "Derive helm faces from nano faces."
  (with-eval-after-load 'helm
    (set-face 'helm-selection                '(face-strong face-subtle))
    (set-face 'helm-match                                       'face-strong)
    (set-face 'helm-source-header                              'face-salient)
    (set-face 'helm-visible-mark                                'face-strong)))

(defun theme--helm-swoop ()
  "Derive helm faces from nano faces."
  (with-eval-after-load 'helm-swoop
    (set-face 'helm-swoop-target-line-face   '(face-strong face-subtle))))

(defun theme--helm-occur ()
  "Derive helm faces from nano faces."
  (with-eval-after-load 'helm-occur
    (set-face 'helm-moccur-buffer                               'face-strong)))

(defun theme--helm-ff ()
  "Derive helm faces from nano faces."
  (with-eval-after-load 'helm-ff
    (set-face 'helm-ff-file                                      'face-faded)
    (set-face 'helm-ff-prefix                                   'face-strong)
    (set-face 'helm-ff-dotted-directory                          'face-faded)
    (set-face 'helm-ff-directory                                'face-strong)
    (set-face 'helm-ff-executable                               'face-popout)))

(defun theme--helm-grep ()
  "Derive helm faces from nano faces."
  (with-eval-after-load 'helm-grep
    (set-face 'helm-grep-match                                  'face-strong)
    (set-face 'helm-grep-file                                    'face-faded)
    (set-face 'helm-grep-lineno                                  'face-faded)
    (set-face 'helm-grep-finish                                'face-default)))

(defun theme--company ()
  "Derive company tooltip window from nano faces."
  (with-eval-after-load 'company
    (set-face 'company-tooltip-selection                   '(face-strong face-subtle))
    (set-face-attribute 'company-tooltip-selection nil :background color-popout)

    (set-face 'company-tooltip                                               'face-subtle)

    (set-face 'company-scrollbar-fg                                          'face-faded)
    (set-face-attribute 'company-scrollbar-fg nil :background color-foreground)

    (set-face 'company-scrollbar-bg                                          'face-default)
    (set-face-attribute 'company-scrollbar-bg nil :background color-faded)

    (set-face 'company-tooltip-common                                        'face-faded)
    (set-face 'company-tooltip-common-selection            '(face-strong face-subtle))
    (set-face-attribute 'company-tooltip-common-selection nil :background color-popout)

    (set-face 'company-tooltip-annotation                                    'face-default)
    (set-face 'company-tooltip-annotation-selection        '(face-strong face-subtle))))

(defun theme ()
  "Derive many, many faces from the core nano faces."
  (theme--basics)
  (theme--font-lock)
  (theme--mode-line)
  (theme--minibuffer)
  (theme--buttons)
  (theme--info)
  (theme--bookmark)
  (theme--speedbar)
  (theme--message)
  (theme--outline)
  (theme--customize)
  (theme--package)
  (theme--flyspell)
  (theme--ido)
  (theme--diff)
  (theme--term)
  (theme--calendar)
  (theme--agenda)
  (theme--org)
  (theme--mu4e)
  (theme--elfeed)
  (theme--deft)
  (theme--rst)
  (theme--markdown)
  (theme--ivy)
  (theme--helm)
  (theme--helm-swoop)
  (theme--helm-occur)
  (theme--helm-ff)
  (theme--helm-grep)
  (theme--hl-line)
  (theme--company))

(defun refresh-theme ()
  "Convenience function which refreshes the theme."
  (interactive)
  (progn
    (faces)
    (theme)))


(defun toggle-theme ()
  "Function to interactively toggle between light and dark nano themes."
  (interactive)
  (cond ((string= theme-var "light")
         (theme-set-dark))
        ((string= theme-var "dark")
         (theme-set-light))
        (t nil))
  (refresh-theme))

(provide 'theme)

;;; theme.el ends here

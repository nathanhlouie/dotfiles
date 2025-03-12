;;; base-colors.el --- Define base colors -*- lexical-binding: t; -*-


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

;; Defines the nine basic colors.

;;; Code:

(defgroup config '() "Faces and colors for the theme." :group 'convenience)

(defvar base-colors--defaults
  `((foreground . ,(face-foreground 'default nil t))
    (background . ,(face-background 'default nil t))
    (highlight . ,(face-background 'fringe nil t))
    (critical . ,(face-foreground 'error nil t))
    (salient . ,(face-foreground 'font-lock-keyword-face nil t))
    (strong . ,(face-foreground 'default nil t))
    (popout . ,(face-foreground 'font-lock-string-face nil t))
    (subtle . ,(face-background 'mode-line-inactive nil t))
    (faded . ,(face-foreground 'shadow nil t))))

(defun base-colors--get (name)
  "Get default color associated with symbol NAME."
  (cdr (assoc name base-colors--defaults)))

(defcustom config-color-foreground (base-colors--get 'foreground)
  ""
  :type 'color
  :group 'config)

(defcustom color-background (base-colors--get 'background)
  ""
  :type 'color
  :group 'config)

(defcustom color-highlight (base-colors--get 'highlight)
  ""
  :type 'color
  :group 'config)

(defcustom color-critical (base-colors--get 'critical)
  ""
  :type 'color
  :group 'config)

(defcustom color-salient (base-colors--get 'salient)
  ""
  :type 'color
  :group 'config)

(defcustom color-strong (base-colors--get 'strong)
  ""
  :type 'color
  :group 'config)

(defcustom color-popout (base-colors--get 'popout)
  ""
  :type 'color
  :group 'config)

(defcustom color-subtle (base-colors--get 'subtle)
  ""
  :type 'color
  :group 'config)

(defcustom color-faded (base-colors--get 'faded)
  ""
  :type 'color
  :group 'config)

(provide 'base-colors)

;;; base-colors.el ends here

;;; faces.el --- Define faces -*- lexical-binding: t; -*-


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

;; Defines several basic faces for use in other places.

;;; Code:

(require 'base-colors)

(defcustom font-family-monospaced "RobotoMono Nerd Font"
  "Default monospaced font."
  :group 'config
  :type 'string)

(defcustom font-family-proportional nil
  "Default proportional font."
  :group 'config
  :type 'string)

(defcustom nano-font-size 24
  "Default font size."
  :group 'config
  :type 'integer)

;; A theme is fully defined by these seven faces

(defface face-default nil
  "Default face is used for regular information."
  :group 'config)

(defface face-variable-pitch nil
  "Default variable-pitch face is used for variable pitch mode."
  :group 'config)

(defface face-critical nil
  "Critical face is for information that requires immediate action."
  :group 'config)

(defface face-popout nil
  "Popout face is used for information that needs attention."
  :group 'config)

(defface face-strong nil
  "Strong face is used for information of a structural nature."
  :group 'config)

(defface face-salient nil
  "Salient face is used for information that are important."
  :group 'config)

(defface face-faded nil
  "Faded face is for information that are less important."
  :group 'config)

(defface face-subtle nil
  "Subtle face is used to suggest a physical area on the screen."
  :group 'config)

(defface face-header-default nil
  "Default face for the header line."
  :group 'config)

(defface face-header-critical nil
  "Critical face for the header line."
  :group 'config)

(defface face-header-popout nil
  "Popout face for the header line."
  :group 'config)

(defface face-header-strong nil
  "Strong face for the header line."
  :group 'config)

(defface face-header-salient nil
  "Salient face for the header line."
  :group 'config)

(defface face-header-faded nil
  "Faded face for the header line."
  :group 'config)

(defface face-header-subtle nil
  "Subtle face for ther header line."
  :group 'config)

(defface face-header-highlight nil
  "Highlight face for ther header line."
  :group 'config)

(defface face-header-separator nil
  "Face for separating item in the header line (internal use)."
  :group 'config)

(defface face-header-filler nil
  "Face compensating spaces in the header line (internal use)."
  :group 'config)

(defface face-tag-default nil
  "Default face for tags."
  :group 'config)

(defface face-tag-faded nil
  "Faded face for tags."
  :group 'config)

(defface face-tag-strong nil
  "Strong face for tags."
  :group 'config)

(defface face-tag-salient nil
  "Salient face for tags."
  :group 'config)

(defface face-tag-popout nil
  "Popout face for tags."
  :group 'config)

(defface face-tag-critical nil
  "Critical face for tags."
  :group 'config)

(defun what-faces (pos)
  "Get the font faces at POS."
  (interactive "d")
  (let ((faces (remq nil
                     (list
                      (get-char-property pos 'read-face-name)
                      (get-char-property pos 'face)
                      (plist-get (text-properties-at pos) 'face)))))
    (message "Faces: %s" faces)))

(defun faces ()
  "Derive face attributes for faces using theme values."
  (set-face-attribute 'face-default nil
                      :foreground color-foreground
                      :background color-background
                      :family     font-family-monospaced
                      :height       (* font-size 10))
  (set-face-attribute 'face-critical nil
                      :foreground color-foreground
                      :background color-critical)
  (set-face-attribute 'face-popout nil
                      :foreground color-popout)

  (set-face-attribute 'face-variable-pitch nil
                      :foreground (face-foreground 'face-default)
                      :background (face-background 'face-default)
                      :family font-family-proportional
                      :height (* font-size 10))
  (if (display-graphic-p)
      (set-face-attribute 'face-strong nil
                          :foreground color-strong
                          :weight 'medium)
    (set-face-attribute 'face-strong nil
                        :foreground color-strong
                        :weight 'bold))

  (set-face-attribute 'face-salient nil
                      :foreground color-salient
                      :weight 'light)

  (set-face-attribute 'face-faded nil
                      :foreground color-faded
                      :weight 'light)

  (set-face-attribute 'face-subtle nil
                      :background color-subtle)

  (set-face-attribute 'face-header-default nil
                      :foreground color-foreground
                      :background color-subtle
                      :box `(:line-width 1
                                         :color ,color-background
                                         :style nil))

  (set-face-attribute 'face-tag-default nil
                      :foreground color-foreground
                      :background color-background
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 font-size)))
                                1)
                      :box `(:line-width 1
                                         :color ,color-foreground
                                         :style nil))

  (set-face-attribute 'face-header-strong nil
                      :foreground color-strong
                      :background color-subtle
                      :inherit 'face-strong
                      :box `(:line-width 1
                                         :color ,color-background
                                         :style nil))

  (set-face-attribute 'face-tag-strong nil
                      :foreground color-strong
                      :background color-subtle
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 font-size)))
                                1)
                      :box `(:line-width 1
                                         :color ,color-strong
                                         :style nil))

  (set-face-attribute 'face-header-salient nil
                      :foreground color-background
                      :background color-salient
                      :box `(:line-width 1
                                         :color ,color-background
                                         :style nil))

  (set-face-attribute 'face-tag-salient nil
                      :foreground color-background
                      :background color-salient
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 font-size)))
                                1)
                      :box `(:line-width 1
                                         :color ,color-salient
                                         :style nil))

  (set-face-attribute 'face-header-popout nil
                      :foreground color-background
                      :background color-popout
                      :box `(:line-width 1
                                         :color ,color-background
                                         :style nil))

  (set-face-attribute 'face-tag-popout nil
                      :foreground color-background
                      :background color-popout
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 font-size)))
                                1)
                      :box `(:line-width 1
                                         :color ,color-popout
                                         :style nil))

  (set-face-attribute 'face-header-faded nil
                      :foreground color-background
                      :background color-faded
                      :box `(:line-width 1
                                         :color ,color-background
                                         :style nil))

  (set-face-attribute 'face-tag-faded nil
                      :foreground color-background
                      :background color-faded
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 font-size)))
                                1)
                      :box `(:line-width 1
                                         :color ,color-faded
                                         :style nil))

  (set-face-attribute 'face-header-subtle nil)

  (set-face-attribute 'face-header-critical nil
                      :foreground color-background
                      :background color-critical
                      :box `(:line-width 1
                                         :color ,color-background
                                         :style nil))
  (set-face-attribute 'face-tag-critical nil
                      :foreground color-background
                      :background color-critical
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 font-size)))
                                1)
                      :box `(:line-width 1
                                         :color ,color-critical
                                         :style nil))

  (set-face-attribute 'face-header-separator nil
                      :inherit 'face-default
                      :height 0.1)
  (set-face-attribute 'face-header-filler nil
                      :inherit 'face-header-default
                      :height 0.1)
  (set-face-attribute 'face-header-highlight nil
                      :inherit 'face-header-faded
                      :box nil))

(provide 'faces)

;;; faces.el ends here

;;; colormaps.el --- Colormaps for Emacs

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Keywords: colors, colormaps
;; URL: https://github.com/lepisma/colormaps.el

;;; Commentary:

;; colormaps.el lets you use color schemes from popular plotting libraries
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'color)
(require 'pcase)

(defconst colormaps-cmap-viridis
  '((0.00 . (68 1 84))
    (0.13 . (71 44 122))
    (0.25 . (59 81 139))
    (0.38 . (44 113 142))
    (0.50 . (33 144 141))
    (0.63 . (39 173 129))
    (0.75 . (92 200 99))
    (0.88 . (170 220 50))
    (1.00 . (253 231 37)))
  "Color definitions for viridis scheme")

(defun colormaps-interpolate (value cmap-lo cmap-hi)
  "Interpolate the given value using given colormaps."
  (pcase (list cmap-lo cmap-hi)
    (`((,val-lo . ,color-lo) (,val-hi . ,color-hi))
     (let ((val-pos (/ (- value val-lo) (float (- val-hi val-lo)))))
       (mapcar* (lambda (c1 c2) (round (+ c1 c2)))
                (mapcar (lambda (x) (* x (- 1 val-pos))) color-lo)
                (mapcar (lambda (x) (* x val-pos)) color-hi))))))

(defun colormaps-get-def-range (value cmap)
  "Get lower and upper color definition for given value."
  (assert (and (>= value 0.0) (<= value 1.0)) nil "Value not in range [0.0, 1.0]")
  (defun colormaps--check-defs (defs prev)
    (let ((current (car defs)))
      (if (>= (car current) value)
          (cons prev current)
        (colormaps--check-defs (cdr defs) current))))
  (colormaps--check-defs (cdr cmap) (car cmap)))

(defun colormaps-get-color (value &optional cmap)
  "Get hex color for given value and color map"
  (pcase (colormaps-get-def-range value (or cmap colormaps-cmap-viridis))
    (`(,cmap-lo . ,cmap-hi)
     (apply #'color-rgb-to-hex (mapcar (lambda (x) (/ x 255.0)) (colormaps-interpolate value cmap-lo cmap-hi))))))

(provide 'colormaps)
;;; colormaps.el ends here

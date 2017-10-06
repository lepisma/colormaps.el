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

(defconst colormaps-cmaps
  '((viridis . ((0.00 . (68  1   84 ))
                (0.13 . (71  44  122))
                (0.25 . (59  81  139))
                (0.38 . (44  113 142))
                (0.50 . (33  144 141))
                (0.63 . (39  173 129))
                (0.75 . (92  200 99 ))
                (0.88 . (170 220 50 ))
                (1.00 . (253 231 37 ))))
    (inferno . ((0.00 . (0   0   4  ))
                (0.13 . (31  12  72 ))
                (0.25 . (85  15  109))
                (0.38 . (136 34  106))
                (0.50 . (186 54  85 ))
                (0.63 . (227 89  51 ))
                (0.75 . (249 140 10 ))
                (0.88 . (249 201 50 ))
                (1.00 . (252 255 164))))
    (magma . ((0.00 . (0   0   4  ))
              (0.13 . (28  16  68 ))
              (0.25 . (79  18  123))
              (0.38 . (129 37  129))
              (0.50 . (181 54  122))
              (0.63 . (229 80  100))
              (0.75 . (251 135 97 ))
              (0.88 . (254 194 135))
              (1.00 . (252 253 191))))
    (plasma . ((0.00 . (13  8   135))
               (0.13 . (75  3   161))
               (0.25 . (125 3   168))
               (0.38 . (168 34  150))
               (0.50 . (203 70  121))
               (0.63 . (229 107 93 ))
               (0.75 . (248 148 65 ))
               (0.88 . (253 195 40 ))
               (1.00 . (240 249 33 )))))
  "Color map definitions")

(defun colormaps-interpolate (value cmap-lo cmap-hi)
  "Interpolate the given value using given colormaps."
  (pcase (list cmap-lo cmap-hi)
    (`((,val-lo . ,color-lo) (,val-hi . ,color-hi))
     (let ((val-pos (/ (- value val-lo) (float (- val-hi val-lo)))))
       (mapcar* (lambda (c1 c2) (round (+ c1 c2)))
                (mapcar (lambda (x) (* x (- 1 val-pos))) color-lo)
                (mapcar (lambda (x) (* x val-pos)) color-hi))))))

(defun colormaps-get-def-range (value cmap &optional defs prev)
  "Get lower and upper color definition for given value."
  (assert (and (>= value 0.0) (<= value 1.0)) nil "Value not in range [0.0, 1.0]")
  (if (null defs)
      (colormaps-get-def-range value cmap (cdr cmap) (car cmap))
    (let ((current (car defs)))
      (if (>= (car current) value)
          (cons prev current)
        (colormaps-get-def-range value cmap (cdr defs) current)))))

(defun colormaps-get-color (value &optional cmap-id)
  "Get hex color for given value and color map"
  (pcase (colormaps-get-def-range value (alist-get (or cmap-id 'viridis) colormaps-cmaps))
    (`(,cmap-lo . ,cmap-hi)
     (apply #'color-rgb-to-hex (mapcar (lambda (x) (/ x 255.0)) (colormaps-interpolate value cmap-lo cmap-hi))))))

(provide 'colormaps)
;;; colormaps.el ends here

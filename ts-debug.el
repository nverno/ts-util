;;; ts-debug.el --- Tree-sitter debugging -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ts-util
;; Created: 28 July 2024

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:

(require 'ts-util)


(defvar-local ts-debug--overlays nil)

(defun ts-debug@indent (orig node parent bol &rest args)
  "Advice for `treesit-simple-indent' to highlight unmatched lines."
  (let ((res (apply orig node parent bol args)))
    (when (and parent (equal res (cons nil nil)))
      ;; No rules matched
      (save-excursion
        (goto-char (treesit-node-start node))
        (push (ts-util--make-overlay
                (line-beginning-position) (line-end-position)
                'ts-indent t
                'face 'ts-error-face)
              ts-debug--overlays)))
    (cons nil nil)))

;;;###autoload
(defun ts-debug-show-missing-indent ()
  "Toggle highlighting of lines with no matching indentation rules."
  (interactive)
  (if (null ts-debug--overlays)
      (unwind-protect
          (progn
            (advice-add 'treesit-simple-indent :around #'ts-debug@indent)
            (indent-region (point-min) (point-max))
            (message "%d lines missing indentation"
                     (length ts-debug--overlays)))
        (advice-remove 'treesit-simple-indent #'ts-debug@indent))
    (remove-overlays (point-min) (point-max) 'ts-indent t)
    (setq ts-debug--overlays nil)))

(provide 'ts-debug)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ts-debug.el ends here

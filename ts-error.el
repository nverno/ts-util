;;; ts-error.el --- Tree-sitter errors -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ts-util
;; Created:  3 November 2023

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
;;
;; Toggle display for error and missing nodes.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'dash))
(require 'ts-util)

(defface ts-error-face
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t :underline t :inherit error))
  "Face for tree sitter errors / missing nodes."
  :group 'ts-util)

(defvar ts-error-and-missing-query
  ;; FIXME: need to adapt 'identifier to langs that don't have that node
  '(((identifier) @id (:equal "" @id))
    (ERROR) @err)
  "Tree-sitter query to capture errors/missing nodes.")

(defvar ts-error-query '((ERROR) @err) "Query to capture error nodes.")

(defvar ts-error-queries (make-hash-table)
  "Hash language to compiled queries.")

(defvar-local ts-error--langs nil
  "Languages with errors currently being highlighted.")

(defun ts-error--query (language &optional missing)
  "Get error query (include MISSING nodes) for LANGUAGE."
  (--when-let (or (gethash language ts-error-queries)
                  (ts-error--query-compile language missing))
    (cl-pushnew language ts-error--langs)
    it))

(defun ts-error--query-compile (language &optional _missing)
  "Compile error/missing query for LANGUAGE."
  (condition-case _err
      (progn (treesit-query-capture language ts-error-query) t)
    (treesit-query-error
     (user-error "Invalid error query for '%S': %S" language ts-error-query)))
  (--when-let (treesit-query-compile language ts-error-query)
    (puthash language it ts-error-queries)))

(defun ts-error--make-error-overlay (start end language)
  "Make error overlay from START to END for LANGUAGE."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'ts-error t)
    (overlay-put ov 'face 'ts-error-face)
    (overlay-put ov 'ts-language language)))

;;; FIXME: update in range given to `after-change-functions'
(defun ts-error--update-errors (&optional _start _end &rest _)
  "Update error/missing node overlays."
  (remove-overlays (point-min) (point-max) 'ts-error t)
  (dolist (lang ts-error--langs)
    (pcase-dolist (`(,beg . ,end)
                   (treesit-query-range
                    lang (gethash lang ts-error-queries) (point-min) (point-max)))
      (ts-error--make-error-overlay beg (1+ end) lang))))

;;;###autoload
(defun ts-error-toggle (&optional add)
  "Toggle tree sitter error overlays in buffer.
With prefix, ADD additional language to those currently active."
  (interactive "P")
  (if (and (not add) (memq #'ts-error--update-errors after-change-functions))
      (progn
        (remove-overlays (point-min) (point-max) 'ts-error t)
        (setq ts-error--langs nil)
        (remove-hook 'after-change-functions #'ts-error--update-errors t)
        (message "Errors disabled"))
    (let ((lang (intern (ts-util-read-buffer-language))))
      (if (ts-error--query lang)
          (add-hook 'after-change-functions #'ts-error--update-errors nil t)
        (user-error "Failed to compiled error query for '%S'" lang)))))

(provide 'ts-error)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ts-error.el ends here

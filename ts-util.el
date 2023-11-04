;;; ts-util.el --- Treesitter util -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ts-util
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Created: 15 October 2023
;; Keywords: tools

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

(eval-when-compile 'cl-lib)
(require 'treesit)
(require 'transient)

(defvar ts-util--dir
  (file-name-as-directory
   (directory-file-name
    (file-name-directory
     (cond (load-in-progress load-file-name)
           ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
            byte-compile-current-file)
           (t (buffer-file-name)))))))

(defun ts-util-read-buffer-language (&optional parsers)
  (or parsers (setq parsers (treesit-parser-list nil nil t)))
  (if (length< parsers 2)
      (treesit-parser-language (car parsers))
    (completing-read
     "Language: " (seq-uniq (mapcar #'treesit-parser-language parsers)))))

(defun ts-util-read-buffer-parser ()
  (let* ((parsers (treesit-parser-list nil nil t))
         (name (ts-util-read-buffer-language parsers)))
    (seq-filter (lambda (p) (string= name (treesit-parser-language p))) parsers)))

(defun ts-util-validate-query (language query)
  (condition-case nil
      (progn (treesit-query-capture language query) t)
    (treesit-query-error nil)))

(defun ts-util--make-overlay (start end &rest property-values)
  (let ((ov (make-overlay start end)))
    (cl-loop for (k v) on property-values by #'cddr
             do (overlay-put ov k v))
    ov))
(put 'ts-util--make-overlay 'lisp-indent-function 'defun)

;; -------------------------------------------------------------------
;;; Transient 

(declare-function ts-query-remove-highlights "ts-query")

;;;###autoload(autoload 'ts-util-menu "ts-util")
(transient-define-prefix ts-util-menu ()
  "TS util"
  ["Query"
   ("q" "Highlight query" ts-query-highlight-query :transient t)
   ("Q" "Remove highlights" ts-query-remove-highlights :transient t)]
  ["Parsers"
   ("l" "List Nodes" ts-parser-list-nodes :transient t)
   ("r" "Show ranges" ts-parser-toggle-ranges :transient t)]
  ["Errors"
   ("e" "Toggle errors" ts-error-toggle :transient t)])

(provide 'ts-util)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ts-util.el ends here

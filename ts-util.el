;;; ts-util.el --- Treesitter util -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ts-util
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Created: 15 October 2023
;; Keywords:

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

(require 'treesit)

(defvar ts-util--dir
  (file-name-directory
   (cond (load-in-progress load-file-name)
         ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
          byte-compile-current-file)
         (t (buffer-file-name)))))

(defsubst ts-util--read-parser ()
  (expand-file-name
   (read-file-name "Parser: " (locate-user-emacs-file "tree-sitter/"))))

;;;###autoload
(defun ts-util-nodes (parser &optional types)
  "Get node and field names for PARSER.
With \\[universal-argument] prompt for TYPES to limit results."
  (interactive
   (list (ts-util--read-parser)
         (and current-prefix-arg
              (completing-read "Type: " '("all" "named" "anon" "field")))))
  (let ((name (car (last (split-string (file-name-sans-extension parser) "-")))))
    (process-lines
     "python3" (expand-file-name "bin/nodes.py" ts-util--dir)
     "-t" (or types "all") name parser)))

(provide 'ts-util)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ts-util.el ends here

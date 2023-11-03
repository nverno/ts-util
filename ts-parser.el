;;; ts-parser.el --- Tree-sitter parsers -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ts-util

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
;; Tree-sitter parser utilities.
;;
;;; Code:

(require 'ts-util)

(defvar ts-parser-directory (locate-user-emacs-file "tree-sitter/")
  "Directory containing tree-sitter parsers.")

(eval-when-compile
  (defsubst ts:parser-lib-name (lib)
    (car (last (split-string (file-name-sans-extension lib) "-"))))

  (defsubst ts:parser-lib-read ()
    (expand-file-name (read-file-name "Parser: " ts-parser-directory))))

(defconst ts-parser--overlay-name 'ts-parser)

;; (defvar-keymap ts-parser-range-map)

(defun ts-parser--make-overlay (begin end &optional name)
  (let ((ov (make-overlay begin end (current-buffer) nil t)))
    (overlay-put ov ts-parser--overlay-name t)
    (when name (overlay-put ov name t))
    (overlay-put ov 'face 'highlight)
    ;; (overlay-put ov 'keymap ts-parser-range-map)
    ;; (overlay-put ov 'insert-in-front-hooks '())
    ;; (overlay-put ov 'insert-behind-hooks '())
    ;; (overlay-put ov 'modification-hooks '())
    (overlay-put ov 'priority 100)
    ov))

(defvar-local ts-parser--range-overlays nil)

(defvar-keymap ts-parser-repeat-range-map
  :repeat (:enter (ts-parser-toggle-ranges))
  "r" #'ts-parser-toggle-ranges)

;;;###autoload
(defun ts-parser-toggle-ranges ()
  "Toggle highlighting of parser ranges."
  (interactive)
  (if (null ts-parser--range-overlays)
      (let ((parsers (ts-util-read-buffer-parser)))
        (when parsers
          (let ((lang (treesit-parser-language (car parsers))))
            (dolist (parser parsers)
              (pcase-dolist (`(,beg . ,end)
                             (treesit-parser-included-ranges parser))
                (push (ts-parser--make-overlay beg end lang)
                      ts-parser--range-overlays))))))
    (remove-overlays (point-min) (point-max) ts-parser--overlay-name t)
    (setq ts-parser--range-overlays nil)))

;;;###autoload
(defun ts-parser-list-nodes (parser &optional types)
  "Get node and field names for PARSER.
With \\[universal-argument] prompt for TYPES to limit results."
  (interactive
   (list (ts:parser-lib-read)
         (and current-prefix-arg
              (completing-read "Type: " '("all" "named" "anon" "field")))))
  (let* ((name (ts:parser-lib-name parser))
         (bufname (format "*%s-nodes*" name)))
    (unless (get-buffer bufname)
      ;; (process-lines
      ;;  "python3" (expand-file-name "bin/nodes.py" ts-util--dir)
      ;;  "-t" (or types "all") name parser)
      (call-process-shell-command
       (format "%s -t %s %s %s" (expand-file-name "bin/nodes.py" ts-util--dir)
               (or types "all") name parser)
       nil (get-buffer-create bufname) t))
    (with-current-buffer bufname
      (view-mode)
      (goto-char (point-min)))
    (display-buffer bufname)))

(provide 'ts-parser)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ts-parser.el ends here

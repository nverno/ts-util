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

(eval-when-compile 'cl-lib)
(require 'treesit)
(require 'transient)

(defvar ts-util-parser-directory (locate-user-emacs-file "tree-sitter/"))

(defvar ts-util--dir
  (file-name-directory
   (cond (load-in-progress load-file-name)
         ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
          byte-compile-current-file)
         (t (buffer-file-name)))))

(eval-when-compile
  (defsubst ts:util--parser-lib-name (lib)
    (car (last (split-string (file-name-sans-extension lib) "-"))))

  (defsubst ts:util--parser-lib-read ()
    (expand-file-name (read-file-name "Parser: " ts-util-parser-directory))))

(defun ts-util-read-buffer-parser ()
  (let ((parsers (treesit-parser-list nil nil t)))
    (if (length< parsers 2)
        (car parsers)
      (let ((name (completing-read
                   "Parser: " (seq-uniq (mapcar #'treesit-parser-language parsers)))))
        (cl-find name parsers :key #'treesit-parser-language :test #'string=)))))


;; (defvar-keymap ts-util-range-map)
(defconst ts-util--overlay-name 'ts-util-overlay)

(defun ts-util-make-overlay (begin end &optional name)
  (let ((ov (make-overlay begin end (current-buffer) nil t)))
    (overlay-put ov ts-util--overlay-name t)
    (when name (overlay-put ov name t))
    (overlay-put ov 'face 'highlight)
    ;; (overlay-put ov 'keymap ts-util-range-map)
    ;; (overlay-put ov 'insert-in-front-hooks '())
    ;; (overlay-put ov 'insert-behind-hooks '())
    ;; (overlay-put ov 'modification-hooks '())
    (overlay-put ov 'priority 100)
    ov))


;;; Nodes

;;;###autoload
(defun ts-util-list-nodes (parser &optional types)
  "Get node and field names for PARSER.
With \\[universal-argument] prompt for TYPES to limit results."
  (interactive
   (list (ts:util--parser-lib-read)
         (and current-prefix-arg
              (completing-read "Type: " '("all" "named" "anon" "field")))))
  (let* ((name (ts:util--parser-lib-name parser))
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


;;; Parsers

(defvar-local ts-util--range-overlays nil)

(defvar-keymap ts-util-repeat-range-map
  :repeat (:enter (ts-util-toggle-ranges))
  "r" #'ts-util-toggle-ranges)

(defun ts-util-toggle-ranges ()
  "Toggle highlight of current ranges for parser."
  (interactive)
  (if (null ts-util--range-overlays)
      (let ((parser (ts-util-read-buffer-parser)))
        (when parser
          (let ((ranges (treesit-parser-included-ranges parser)))
            ;; (pulse-delay 0.05)
            ;; (pulse-momentary-highlight-region beg end)
            (pcase-dolist (`(,beg . ,end) ranges)
              (push (ts-util-make-overlay beg end (treesit-parser-language parser))
                    ts-util--range-overlays)))))
    (remove-overlays (point-min) (point-max) ts-util--overlay-name t)
    (setq ts-util--range-overlays nil)))


;;; Transient

;;;###autoload(autoload 'ts-util-menu "ts-util")
(transient-define-prefix ts-util-menu ()
  "TS util"
  ["Nodes"
   ("l" "List" ts-util-list-nodes :transient t)]
  ["Parsers"
   ("r" "Show ranges" ts-util-toggle-ranges :transient t)])

(provide 'ts-util)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ts-util.el ends here

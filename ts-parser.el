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

(declare-function hs-hide-all "hideshow")
(declare-function hs-toggle-hiding "hideshow")

(eval-when-compile
  (defsubst ts:parser-lib-name (lib)
    (car (last (split-string (file-name-sans-extension lib) "-"))))

  (defsubst ts:parser-lib-read ()
    (expand-file-name (read-file-name "Parser: " ts-util-parser-directory))))

(defconst ts-parser--overlay-name 'ts-parser)

;; (defvar-keymap ts-parser-range-map)

(defvar-local ts-parser--range-overlays nil)

(defun ts-parser--make-overlay (start end &optional name)
  (let ((ov (ts-util--make-overlay start end
              ts-parser--overlay-name t
              ;; 'keymap ts-parser-range-map
              ;; 'insert-in-front-hooks '()
              ;; 'insert-behind-hooks '()
              ;; 'modification-hooks '()
              'face 'highlight
              'priority 100)))
    (when name (overlay-put ov name t))
    ov))

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

;; -------------------------------------------------------------------
;;; List Parser Nodes

(defvar-keymap ts-parser-nodes-mode-map
  "<tab>" #'hs-toggle-hiding)

(define-derived-mode ts-parser-nodes-mode fundamental-mode "TsNodes"
  "Mode for viewing parser nodes."
  (require 'hideshow)
  (setq-local comment-start "")
  (setq-local hs-special-modes-alist
              `((ts-parser-nodes-mode ,(rx bol (or "Named" "Anon" "Fields") ":")
                              "^$")))
  (setq-local forward-sexp-function     ; for hideshow
              (lambda (&optional arg) (re-search-forward "^$" nil t (or arg 1))))
  (hs-minor-mode)
  (hs-hide-all)
  (view-mode)
  (message
   (substitute-command-keys
    "Press \\<ts-node-mode-map>\\[hs-toggle-hiding] to toggle section hiding")))

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
      (call-process-shell-command
       (format "python %s -t %s %s %s"
               (expand-file-name "bin/nodes.py" ts-util--dir)
               (or types "all") name parser)
       nil (get-buffer-create bufname) t))
    (with-current-buffer bufname
      (goto-char (point-min))
      (ts-parser-nodes-mode)
      (pop-to-buffer (current-buffer)))))

(provide 'ts-parser)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ts-parser.el ends here

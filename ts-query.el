;;; ts-query.el --- Tree-sitter queries -*- lexical-binding: t; -*-

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
;;; Code:

(require 'ts-util)

(defvar-local ts-query--langs nil)

;;;###autoload
(defun ts-query-highlight-query (lang query &optional face)
  "Highlight QUERY captures for LANG in buffer.

QUERY is highlighted with FACE if non-nil, or capture name if it is a
face (eg. \\='@error) or \\='hightlight."
  (interactive (list (intern (ts-util-read-buffer-language))
                     (read--expression "Query: ")
                     (if current-prefix-arg (read-face-name "Face: " 'highlight))))
  (unless (ts-util-validate-query lang query)
    (user-error "Invalid query for %s: '%S'" lang query))
  (let ((captures (treesit-query-capture lang query))
        res)
    (pcase-dolist (`(,name . ,node) captures)
      ;; skip captures prefixed by "_"
      (unless (string-prefix-p "_" (symbol-name name))
        (push (ts-util--make-overlay
                (treesit-node-start node) (treesit-node-end node)
                'ts-query t
                'face `( :inherit ,(or face (and (facep name) name) 'highlight)
                         :inherit 'org-block)
                'ts-lang (intern (format ":%S" lang))
                'priority 100)
              res)))
    (when res (cl-pushnew lang ts-query--langs))
    res))

(defun ts-query-remove-highlights (&optional start end lang)
  "Remove query highlights in buffer from START to END.
With prefix, remove hightlights for LANG."
  (interactive
   (let ((region-p (region-active-p)))
     (list (if region-p (region-beginning) (point-min))
           (if region-p (region-end) (point-max))
           (and current-prefix-arg ts-query--langs
                (intern (concat ":" (completing-read "Remove: " ts-query--langs)))))))
  (funcall #'remove-overlays start end (if lang
                                           (list 'ts-lang lang)
                                         (list 'ts-query t)))
  (setq ts-query--langs (and lang (--filter (not (eq it lang)) ts-query--langs))))

(provide 'ts-query)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ts-query.el ends here

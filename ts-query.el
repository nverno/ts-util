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
(require 'ts-lang nil t)


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


;; -------------------------------------------------------------------
;;; Interactive query mode

(defun ts-query--read (&optional stream)
  "Read query from STREAM.
Point should be at start of query."
  (let ((res (ignore-errors (read stream)))
        sexp)
    (and res (setq res (list res)))
    (while (and (setq sexp (ignore-errors (read (current-buffer))))
                (symbolp sexp)
                (string-prefix-p "@" (symbol-name sexp)))
      (push sexp res))
    (nreverse res)))

(defun ts-query--read-tl-query (&optional point)
  "Read the top-level node for POINT."
  (save-excursion
    (goto-char (or point (point)))
    (forward-comment (- (point-max)))
    (let ((node (--when-let (treesit-node-at (point))
                  (treesit-parent-while
                   it (lambda (n) (not (string-match-p "program" (treesit-node-type n))))))))
      (when node
        (goto-char (treesit-node-start node))
        (ts-query--read (current-buffer))))))

;;; FIXME(5/12/24): wrap in struct
(defvar-local ts-query--src-buf nil)
(defvar-local ts-query--parser-list nil)
(defvar-local ts-query--parser nil)

(defun ts-query-execute-query (&optional point)
  "Execute the query at POINT, applying highlights in source buffer."
  (interactive (list (point)) ts-query-minor-mode)
  (--when-let (ts-query--read-tl-query point)
    (let ((parser ts-query--parser))
      (with-current-buffer ts-query--src-buf
        (ts-query-highlight-query parser it)))))

(defun ts-query-clear-queries ()
  "Remove query highlights."
  (interactive nil ts-query-minor-mode)
  (with-current-buffer ts-query--src-buf
    (ts-query-remove-highlights)))

(defvar-keymap ts-query-minor-mode-map
  "C-c C-c" #'ts-query-execute-query
  "C-c C-k" #'ts-query-clear-queries)

(define-minor-mode ts-query-minor-mode
  "Minor mode active in interactive query buffers."
  :abbrev-table nil)

;;;###autoload
(defun ts-query ()
  "Open interactive query buffer."
  (interactive)
  (let* ((src-buf (current-buffer))
         (parsers (treesit-parser-list src-buf))
         (buf (get-buffer-create
               (format "*ts-query[%s]*"
                       (file-name-base (buffer-name src-buf))))))
    (with-current-buffer buf
      (query-ts-mode)
      (ts-query-minor-mode)
      (setq ts-query--src-buf src-buf
            ts-query--parser-list parsers
            ts-query--parser (car parsers))
      (pop-to-buffer buf))))


(provide 'ts-query)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ts-query.el ends here

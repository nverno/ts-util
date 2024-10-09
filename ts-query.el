;;; ts-query.el --- Tree-sitter queries -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ts-util
;; Package-Requires: ((emacs "29.1") (dash "2.19") (ts-lang "0.0.1"))

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
;; Run interactive tree-sitter queries.
;;
;; `ts-query' creates a query buffer associated with a source buffer.
;;
;; The query buffers use `query-ts-mode' when possible, and if `ts-lang' is
;; installed, there is `completion-at-point' for any installed parsers when
;; writing queries.
;;
;; Query buffer commands:
;;  + `ts-query-execute-query'
;;     Runs the top-level query at or before point in the source buffer,
;;     applying any font-locking captures.
;;  + `ts-query-clear-queries'
;;     Removes changes applied in the source buffer.
;;
;; If the query has a trailing capture that isn't enclosed in parens, the point
;; needs to be after the capture group to include it in the query.
;;
;; For example, the point should be at '|' to include the capture groups in the
;; following query.
;;
;;     (identifier) @bold @italic '|'
;;
;; When the queries are enclosed in parens, the point can be anywhere in the query.
;;
;; TODO(5/17/24): Let captures be any functions that should run on nodes.
;;
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'dash))
(require 'ts-util)
(require 'ts-lang nil t)

(defvar-local ts-query--langs nil)

(defsubst ts-query--read-active-lang (&optional prompt active-langs)
  "Read a highlighted lang with PROMPT from ACTIVE-LANGS."
  (or active-langs (setq active-langs ts-query--langs))
  (when active-langs
    (completing-read (or prompt "Lang: ") active-langs nil t)))

;;;###autoload
(defun ts-query-highlight-query (parser query &optional face)
  "Highlight QUERY captures for PARSER in buffer.

QUERY is highlighted with FACE if non-nil, or capture name if it is a
face (eg. \\='@error) or \\='hightlight."
  (interactive (list (intern (ts-util-read-buffer-language))
                     (read--expression "Query: ")
                     (if current-prefix-arg (read-face-name "Face: " 'highlight))))
  (unless (ts-util-validate-query parser query)
    (user-error "Invalid query for %s: '%S'" parser query))
  (let ((captures (treesit-query-capture parser query))
        res)
    (pcase-dolist (`(,name . ,node) captures)
      ;; Skip captures prefixed by "_"
      (unless (string-prefix-p "_" (symbol-name name))
        (push (ts-util--make-overlay
                (treesit-node-start node) (treesit-node-end node)
                'ts-query t
                'face `( :inherit ,(or face (and (facep name) name) 'highlight)
                         :inherit 'org-block)
                'ts-lang (intern (format ":%S" parser))
                'priority 100)
              res)))
    (when res (cl-pushnew parser ts-query--langs))
    res))

(defun ts-query-remove-highlights (&optional start end lang)
  "Remove query highlight in buffer from START to END.
With prefix, remove hightlights for LANG."
  (interactive
   (let ((region-p (region-active-p)))
     (list (if region-p (region-beginning) (point-min))
           (if region-p (region-end) (point-max))
           (and current-prefix-arg (ts-query--read-active-lang "Remove: ")))))
  (and lang (not (symbolp lang))
       (setq lang (intern (concat ":" lang))))
  (funcall #'remove-overlays start end (if lang
                                           (list 'ts-lang lang)
                                         (list 'ts-query t)))
  (setq ts-query--langs (and lang (--filter (not (eq it lang)) ts-query--langs))))


;;; Parse queries
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


;;; Internal datastructure
(cl-defstruct (ts--query (:constructor ts-query--make))
  "Holds info about buffer's query."
  src-buf                               ; Source buffer to run queries
  parser-list                           ; List of all parsers in source
  parser                                ; Parser to use for queries
  ;; Internal: holds ts-lang pointer
  lang)

(defvar-local ts-query--current nil "Local `ts--query'.")

(defun ts-query-make-current (&rest args)
  "Setup `ts-query--current' from ARGS.
ARGS should be a plist of arguments suitable for `ts-query--make'."
  (when (treesit-parser-p (plist-get args :parser))
    (plist-put args :parser (treesit-parser-language (plist-get args :parser))))
  (setq ts-query--current (apply #'ts-query--make args))
  (when (require 'ts-lang nil t)
    (when-let* ((parser (plist-get args :parser))
                (file (assoc-default parser (ts-util-installed-parsers))))
      (setf (ts--query-lang ts-query--current)
            (ts-lang-parser-info file)))))

(cl-defmacro with-ts-query (fields &rest body)
  "Do BODY with FIELDS bound to values from `ts-query--current'."
  (declare (indent defun)
           (debug (&define cl-macro-list def-form cl-declarations def-body)))
  (while (keywordp (car body)) (setq body (cddr body)))
  `(pcase-let (((cl-struct ts--query ,@fields) ts-query--current))
     ,@body))

(defmacro with-ts-query-src-buffer (&rest body)
  "Do BODY in source buffer associated with query buffer."
  `(with-ts-query (src-buf)
     (with-current-buffer src-buf
       ,@body)))


;;; Completion-at-point

(defun ts-query-completion-at-point ()
  "Completion at point function for `ts-query-minor-mode'."
  (let* ((end (point))
         (named-p)                               ; named node
         (capture-p)                             ; capture
         (anon-p (nth 3 (syntax-ppss (point))))  ; anonymous node
         (beg (save-excursion
                (skip-syntax-backward "w_")
                (prog1 (point)
                  (or anon-p
                      (setq capture-p (eq ?@ (char-before)))
                      (setq named-p
                            (progn (skip-syntax-backward " ")
                                   (eq ?\( (char-before)))))))))
    ;; XXX(5/16/24): offer font-lock names for capture?
    (unless capture-p
      (with-ts-query (lang)
        (when lang
          (list beg end
                (pcase-let (((cl-struct ts-lang--info named anon fields) lang))
                  (cond (named-p named)
                        (anon-p anon)
                        (t fields)))))))))


;;; Queries

(defun ts-query-execute-query (&optional point)
  "Execute the query at POINT in source buffer."
  (interactive (list (point)) ts-query-minor-mode)
  (--when-let (ts-query--read-tl-query point)
    (with-ts-query (src-buf parser)
      (with-current-buffer src-buf
        (ts-query-highlight-query parser it)
        (display-buffer-other-frame (current-buffer))))))

(defun ts-query-clear-queries ()
  "Remove query highlighting applied in source buffer."
  (interactive nil ts-query-minor-mode)
  (with-ts-query-src-buffer
   (call-interactively #'ts-query-remove-highlights)))

(defun ts-query-pop-to-source ()
  "Pop to source buffer."
  (interactive nil ts-query-minor-mode)
  (with-ts-query-src-buffer
   (pop-to-buffer (current-buffer))))


;;; Query Minor Mode

(defvar-keymap ts-query-minor-mode-map
  :doc "Keymap in `ts-query-minor-mode'."
  "C-c C-c" #'ts-query-execute-query
  "C-c C-k" #'ts-query-clear-queries
  "C-c C-z" #'ts-query-pop-to-source)

(easy-menu-define ts-query-minor-mode-menu ts-query-minor-mode-map
  "Menu for Ts-Query minor mode."
  '("TsQuery"
    ["Execute query" ts-query-execute-query]
    ["Clear queries" ts-query-clear-queries]
    ["Pop to source" ts-query-pop-to-source]))

(defun ts-query--read-args (&optional plist)
  "Read query setup args, optionally returning a PLIST."
  (let* ((src-buffer (get-buffer (read-buffer "Source buffer: " nil t)))
         (parser-list
          (let ((parser (intern
                         (completing-read
                           "Parser: " (ts-util-installed-parsers)))))
            (unless (memq parser (mapcar #'treesit-parser-language
                                         (treesit-parser-list src-buffer)))
              (treesit-parser-create parser src-buffer))
            (list parser)))
         (parser (car parser-list)))
    (if plist
        (list :src-buf src-buffer :parser parser :parser-list parser-list)
      (list src-buffer parser parser-list))))

(define-minor-mode ts-query-minor-mode
  "Minor mode active in interactive query buffers."
  :lighter " TsQuery"
  :abbrev-table nil
  (if (null ts-query-minor-mode)
      (remove-hook 'completion-at-point-functions
                   #'ts-query-completion-at-point t)
    (unless ts-query--current
      (apply #'ts-query-make-current (ts-query--read-args 'plist)))
    (when (require 'ts-lang nil t)
      (add-hook 'completion-at-point-functions
                #'ts-query-completion-at-point nil t))))


;;;###autoload
(defun ts-query (src-buffer &optional parser parser-list)
  "Open interactive query buffer for SRC-BUFFER with PARSER.
SRC-BUFFER, PARSER, and PARSER-LIST are args for `ts-query-make-current'."
  (interactive
   (if current-prefix-arg (ts-query--read-args)
     (let ((parsers (mapcar #'treesit-parser-language
                            (treesit-parser-list (current-buffer)))))
       (list (current-buffer) (car parsers) parsers))))
  (let ((buf (get-buffer-create
              (format "*Ts Query on %s*"
                      (file-name-nondirectory
                       (buffer-name src-buffer))))))
    (with-current-buffer buf
      (when (fboundp 'query-ts-mode)
        (query-ts-mode))
      (ts-query-make-current
       :src-buf src-buffer
       :parser parser
       :parser-list parser-list)
      (ts-query-minor-mode)
      (prog1 buf
        (pop-to-buffer buf)))))


(provide 'ts-query)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ts-query.el ends here

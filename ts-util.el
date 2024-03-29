;;; ts-util.el --- Treesitter util -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ts-util
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (dash "2.19"))
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

(eval-when-compile
  (require 'cl-lib)
  (require 'dash))
(require 'treesit)
(require 'transient)

(declare-function xterm-color-filter "xterm-color")


(defvar ts-util-parser-directory (locate-user-emacs-file "tree-sitter/")
  "Directory containing tree-sitter parsers.")

(defvar ts-util-grammar-directory (expand-file-name "~/scratch/parsers/")
  "Directory to download/build grammar sources.")

(eval-and-compile
  (defvar ts-util-nvim-treesitter-directory (expand-file-name "~/src/nvim-treesitter/")
    "Path to nvim-treesitter source directory.")

  (defvar ts-util-neovim-directory (expand-file-name "~/src/neovim/")
    "Path to neovim source directory.")

  (defvar ts-util--dir
    (file-name-as-directory
     (directory-file-name
      (file-name-directory
       (cond (load-in-progress load-file-name)
             ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
              byte-compile-current-file)
             (t (buffer-file-name))))))))


(defun ts-util-read-buffer-language (&optional parsers)
  (or parsers (setq parsers (treesit-parser-list nil nil t)))
  (if (length< parsers 2)
      (symbol-name (treesit-parser-language (car parsers)))
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
;;; Parser Sources used by Neovim

(defvar shell-mode-hook)
(declare-function shell-mode "shell")

(defmacro ts-util:call-process (cmd &rest on-success)
  (declare (indent 1))
  (let ((res (make-symbol "res")))
    `(with-current-buffer (get-buffer-create
                           (generate-new-buffer-name "*ts-util*"))
       (let ((,res ,cmd))
         (if (processp ,res)
             (cl-letf (;; (filter
                       ;;  (lambda (p s)
                       ;;    (when (buffer-live-p (process-buffer p))
                       ;;      (with-current-buffer (process-buffer p)
                       ;;        (let ((inhibit-read-only t))
                       ;;          (goto-char (point-max))
                       ;;          (insert (xterm-color-filter
                       ;;                   (replace-regexp-in-string
                       ;;                    "[\r\n]+" "\n" s))))))))
                       (callback
                        (lambda (p _m)
                          (with-current-buffer (process-buffer p)
                            (if (zerop (process-exit-status p))
                                (unwind-protect (progn ,@on-success)
                                  (kill-buffer))
                              (pop-to-buffer (current-buffer)))))))
               (require 'shell)
               (setq mode-line-process '(":%s"))
               (let (shell-mode-hook)
                 (shell-mode))
               (set-process-filter ,res #'comint-output-filter)
               (set-process-sentinel ,res callback)
               ,res)
           (if (zerop ,res)
               (unwind-protect (progn ,@on-success)
                 (kill-buffer))
             (pop-to-buffer (current-buffer))))))))

;; Get neovim tree-sitter parser sources
(defun ts-util--get-sources ()
  (ts-util:call-process
      (call-process-shell-command
       (format
        "LUA_PATH=\"%s/runtime/lua/?.lua;%s/lua/?.lua;${LUA_PATH:-;}\" %s"
        ts-util-neovim-directory
        ts-util-nvim-treesitter-directory
        (expand-file-name "bin/sources.lua" ts-util--dir))
       nil (current-buffer))
    (goto-char (point-min))
    (read (current-buffer))))

(defvar ts-util--sources (ignore-errors (ts-util--get-sources)))

(defun ts-util-sources ()
  (or ts-util--sources
      (setq ts-util--sources (ts-util--get-sources))
      (user-error "Failed to get neovim sources (is nvim installed?)")))

;; -------------------------------------------------------------------
;;; Parser List Mode

(defun ts-util-browse-grammar ()
  "Browse the url of the grammar at point."
  (interactive)
  (when-let ((entry (tabulated-list-get-entry (point))))
    (browse-url (elt entry 1))))

(defun ts-util-install-grammar ()
  "Install the tree-sitter grammar at point, using neovim recipe."
  (interactive)
  (when-let ((entry (tabulated-list-get-entry (point))))
    (let* ((lang (tabulated-list-get-id (point)))
           (lst (mapcar (lambda (s) (if (string-empty-p s) nil s))
                        (append entry nil)))
           (treesit-language-source-alist (list (cons lang (cdr lst)))))
      (treesit-install-language-grammar lang))))

(defun ts-util--clone-grammar (url &optional build)
  "Clone grammar from URL.
If BUILD, attempt to run install and generate after cloning. Returns a cons
of source directory and a process object or nil if the source already
exists."
  (unless (file-exists-p ts-util-grammar-directory)
    (make-directory ts-util-grammar-directory t))
  (let* ((default-directory ts-util-grammar-directory)
         (src-dir (expand-file-name (file-name-base url))))
    (if (file-exists-p src-dir)
        (cons src-dir nil)
      (let* ((cmd (concat "git clone --depth=1 " url))
             (proc (ts-util:call-process
                    (start-process-shell-command
                     "ts-source" (current-buffer)
                     (if (not build) cmd
                       (concat
                        cmd "&& cd " src-dir
                        "&& npm --loglevel=info --progress=true install"))))))
        (cons src-dir proc)))))

(defun ts-util-clone-grammar (url &optional build callback)
  "Clone grammar at point in `ts-util-grammar-directory'.
With prefix, attempt to BUILD after cloning."
  (interactive
   (list (and-let* ((entry (tabulated-list-get-entry (point)))) (elt entry 1))
         current-prefix-arg))
  (unless url (user-error "No url"))
  (let* ((res (ts-util--clone-grammar url build))
         (src-dir (car res)))
    (cl-letf* ((proc (cdr res))
               (go-fn
                (or callback
                    (lambda (src-dir)
                      (let ((grammar (expand-file-name "grammar.js" src-dir)))
                        (if (file-exists-p grammar)
                            (find-file-other-window grammar)
                          (dired-jump-other-window src-dir))))))
               (sentinel
                (lambda (p _m)
                  (if (memq (process-status p) '(exit signal))
                      (unwind-protect (funcall go-fn src-dir)
                        (kill-buffer (process-buffer p)))
                    (pop-to-buffer (process-buffer p))))))
      (if (processp proc)
          (with-current-buffer (process-buffer proc)
            (message "Cloning %s to %s" url src-dir)
            (set-process-sentinel proc sentinel)
            (display-buffer (current-buffer)))
        (funcall go-fn src-dir)))))

(defvar-keymap ts-util-sources-mode-map
  "w" #'ts-util-browse-grammar
  "i" #'ts-util-install-grammar
  "c" #'ts-util-clone-grammar)

(define-derived-mode ts-util-sources-mode tabulated-list-mode "TsSrc"
  "Mode to view neovim tree-sitter parsers."
  (setq tabulated-list-format
        [("Parser" 10 t) ("Url" 50 t) ("Revision" 8 t) ("Location" 15 t)])
  (setq tabulated-list-sort-key '("Parser" . nil))
  (setq tabulated-list-entries (ts-util-sources))
  (tabulated-list-init-header)
  (tabulated-list-print))

;;;###autoload
(defun ts-util-list-sources (&optional language)
  "List tree-sitter parser sources used by neovim.
With prefix, prompt for LANGUAGE and return its source."
  (interactive
   (list (and current-prefix-arg
              (intern (completing-read "Language: " (ts-util-sources))))))
  (if language
      (let ((source (elt (car (assoc-default language (ts-util-sources))) 1)))
        (prog1 source (message source)))
    (with-current-buffer (get-buffer-create "*treesitter-parsers*")
      (ts-util-sources-mode)
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun ts-util-jump-to-queries (&optional source)
  "Jump to nvim tree-sitter queries for language.
With prefix, look for the queries from the source repo."
  (interactive (list current-prefix-arg))
  (if (null source)
      (let ((default-directory
             (expand-file-name "queries/" ts-util-nvim-treesitter-directory)))
        (call-interactively #'find-file-other-window))
    (cl-letf* ((lang (completing-read "Language: " (ts-util-sources) nil t))
               (callback (lambda (src-dir)
                           (let ((dir (expand-file-name "queries/" src-dir)))
                             (dired-other-window
                              (if (file-exists-p dir) dir src-dir)))))
               (url (elt (car (assoc-default (intern lang) (ts-util-sources))) 1)))
      (ts-util-clone-grammar url nil callback))))

;; -------------------------------------------------------------------
;;; Transient

(declare-function ts-query-remove-highlights "ts-query")
(declare-function ts-parser-list-nodes "ts-parser")

;;;###autoload(autoload 'ts-util-menu "ts-util" nil t)
(transient-define-prefix ts-util-menu ()
  "TS util"
  [["Query"
    ("q" "Highlight query" ts-query-highlight-query :transient t)
    ("Q" "Remove highlights" ts-query-remove-highlights)
    ("j" "Jump to queries" ts-util-jump-to-queries)]
   ["Parsers"
    ("l" "List Nodes" ts-parser-list-nodes)
    ("L" "List sources" ts-util-list-sources)
    ("r" "Show ranges" ts-parser-toggle-ranges :transient t)]
   ["Errors"
    ("e" "Toggle errors" ts-error-toggle :transient t)]])

(provide 'ts-util)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ts-util.el ends here

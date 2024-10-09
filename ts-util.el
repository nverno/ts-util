;;; ts-util.el --- Treesitter util -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ts-util
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (dash "2.19"))
;; Created: 15 October 2023
;; Keywords: tree-sitter, tools, convenience

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

(defmacro ts-util--call-process (cmd &rest on-success)
  (declare (indent 1))
  (let ((res (make-symbol "res")))
    `(with-current-buffer (get-buffer-create
                           (generate-new-buffer-name "*ts-util*"))
       (let ((,res ,cmd))
         (if (processp ,res)
             (cl-letf ((callback
                        (lambda (p m)
                          (let ((stat (process-exit-status p)))
                            (if (zerop stat)
                                (with-current-buffer (process-buffer p)
                                  (unwind-protect (progn ,@on-success)
                                    (kill-buffer)))
                              (pop-to-buffer (process-buffer p))
                              (error "%s: %S" m (process-exit-status p)))))))
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
             (error "%S" ,res)))))))

(defun ts-util--get-sources ()
  "Get neovim's tree-sitter parser sources."
  (with-demoted-errors "Error getting neovim parser sources: %S"
    (ts-util--call-process
        (call-process-shell-command
         (format
          "LUA_PATH=\"%s/runtime/lua/?.lua;%s/lua/?.lua;${LUA_PATH:-;}\" %s"
          ts-util-neovim-directory
          ts-util-nvim-treesitter-directory
          (expand-file-name "bin/sources.lua" ts-util--dir))
         nil (current-buffer))
      (goto-char (point-min))
      (read (current-buffer)))))

(defvar ts-util--sources (ignore-errors (ts-util--get-sources)))

(defun ts-util-sources ()
  "List all available tree-sitter parser sources from nvim."
  (or ts-util--sources
      (setq ts-util--sources (ts-util--get-sources))
      (user-error "Failed to get neovim sources (is nvim installed?)")))

(defun ts-util-installed-parsers ()
  "List installed parsers as list of cons of parser name and its path.
Checks in directory \"tree-sitter\" under `user-emacs-directory' and paths in
`treesit-extra-load-path'."
  (let* ((paths (seq-uniq
                 (cons (expand-file-name "tree-sitter" user-emacs-directory)
                       treesit-extra-load-path)))
         (parsers (cl-loop for dir in paths
                           when (file-exists-p dir)
                           nconc (directory-files dir t "^[^.]"))))
    (--map (cons (intern (and (string-match ".*-\\([^-]+\\)[.].*$" it)
                              (match-string 1 it)))
                 it)
           parsers)))

;;;###autoload
(defun ts-util-add-treesit-sources ()
  "Add sources to `treesit-language-source-alist'."
  (interactive)
  (pcase-dolist (`(,src [,_ ,url ,revision ,src-dir]) (ts-util-sources))
    (and src-dir
         (not (string-empty-p src-dir))
         (not (string-suffix-p "src" src-dir))
         (setq src-dir (concat src-dir "/src")))
    (add-to-list 'treesit-language-source-alist
                 (mapcar (lambda (e) (if (string= e "") nil e))
                         (list src url revision src-dir)))))

(autoload 'ts-sources-clone-grammar "ts-sources")

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
      (ts-sources-clone-grammar url nil callback))))

;;;###autoload
(defun ts-util-extract-corpus-tests (corpus &optional buffer show)
  "Extract code examples from tree-sitter CORPUS file.
Insert results into BUFFER if non-nil. Prompt for BUFFER with prefix.
If SHOW is non-nil pop to results buffer."
  (interactive
   (list (read-file-name "Corpus: ")
         (and current-prefix-arg (read-buffer "Output buffer: ")) t))
  (let ((exe (expand-file-name "bin/corpus.rb" ts-util--dir))
        (buf (or buffer
                 (let ((bufname
                        (format "*ts-util[%s]*"
                                (file-name-base
                                 (file-name-sans-extension corpus)))))
                   (with-current-buffer (get-buffer-create bufname)
                     (erase-buffer)
                     (current-buffer))))))
    (call-process exe corpus buf show)
    (and show (pop-to-buffer buf))))


(provide 'ts-util)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ts-util.el ends here

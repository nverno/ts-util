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
(require 'transient)


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
             (cl-letf ((callback
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
  "Get parser sources from nvim."
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

;; -------------------------------------------------------------------
;;; Parser List Mode

;;;###autoload
(defun ts-util-browse-repo (entry)
  "Goto the source repo for parser.
When called from `ts-util-sources-mode' use the grammar at point.
Otherwise, prompt for ENTRY."
  (interactive
   (list (if (eq major-mode 'ts-util-sources-mode)
             (tabulated-list-get-entry (point))
           (let ((srcs (ts-util-sources)))
             (and-let* ((grammar (completing-read "Grammar: " srcs nil t)))
               (car (assoc-default (intern grammar) srcs)))))))
  (and entry (browse-url (elt entry 2))))

(defun ts-util-install-grammar ()
  "Install the tree-sitter grammar at point, using neovim recipe."
  (interactive)
  (when-let ((entry (tabulated-list-get-entry (point))))
    (let* ((lang (tabulated-list-get-id (point)))
           (lst (mapcar (lambda (s) (if (string-empty-p s) nil s))
                        (cdr (append entry nil))))
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
   (list (and-let* ((entry (tabulated-list-get-entry (point)))) (elt entry 2))
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
  "w" #'ts-util-browse-repo
  "i" #'ts-util-install-grammar
  "c" #'ts-util-clone-grammar)

(define-derived-mode ts-util-sources-mode tabulated-list-mode "TsSrc"
  "Mode to view tree-sitter parser sources."
  (setq tabulated-list-format
        [("Inst" 5 t) ("Parser" 10 t) ("Url" 50) ("Revision" 8) ("Location" 15)])
  (setq tabulated-list-sort-key '("Parser" . nil))
  (let ((installed (ts-util-installed-parsers)))
    (setq tabulated-list-entries
          (cl-loop for s in (ts-util-sources)
                   for name = (car s)
                   for entry =
                   (apply #'vector
                          (if (assq name installed)
                              (propertize "✓" 'face '(:foreground "green"))
                            "")
                          (append (cadr s) nil))
                   collect (list name entry))))
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

;; -------------------------------------------------------------------
;;; Transient

;; (declare-function ts-query-remove-highlights "ts-query")
;; (declare-function ts-parser-list-nodes "ts-parser")

;; ;;;###autoload(autoload 'ts-util-menu "ts-util" nil t)
;; (transient-define-prefix ts-util-menu ()
;;   "TS util"
;;   [["Query"
;;     ("q" "Highlight query" ts-query-highlight-query :transient t)
;;     ("Q" "Remove highlights" ts-query-remove-highlights)
;;     ("j" "Jump to queries" ts-util-jump-to-queries)]
;;    ["Parsers"
;;     ("l" "List Nodes" ts-parser-list-nodes)
;;     ("L" "List sources" ts-util-list-sources)
;;     ("r" "Show ranges" ts-parser-toggle-ranges :transient t)]
;;    ["Errors"
;;     ("e" "Toggle errors" ts-error-toggle :transient t)]
;;    ["Tests"
;;     ("x" "Extract corpus tests" ts-util-extract-corpus-tests)]])

(provide 'ts-util)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ts-util.el ends here

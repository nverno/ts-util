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

(declare-function xterm-color-filter "xterm-color")
(declare-function hs-hide-all "hideshow")
(declare-function hs-toggle-hiding "hideshow")


(defvar ts-parser-directory (locate-user-emacs-file "tree-sitter/")
  "Directory containing tree-sitter parsers.")

(defvar ts-parser-grammar-directory (expand-file-name "~/scratch/parsers/")
  "Directory to download/build grammar sources.")

(eval-and-compile
  (defvar ts-parser-nvim-treesitter-directory (expand-file-name "~/src/nvim-treesitter/")
    "Path to nvim-treesitter source directory.")

  (defvar ts-parser-neovim-directory (expand-file-name "~/src/neovim/")
    "Path to neovim source directory."))

(eval-when-compile
  (defsubst ts:parser-lib-name (lib)
    (car (last (split-string (file-name-sans-extension lib) "-"))))

  (defsubst ts:parser-lib-read ()
    (expand-file-name (read-file-name "Parser: " ts-parser-directory))))

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

(defvar-keymap ts-node-mode-map
  "<tab>" #'hs-toggle-hiding)

(define-derived-mode ts-node-mode fundamental-mode "Nodes"
  "Mode for viewing parser nodes."
  (require 'hideshow)
  (setq-local comment-start "")
  (setq-local hs-special-modes-alist
              `((ts-node-mode ,(rx bol (or "Named" "Anon" "Fields") ":")
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
      (ts-node-mode)
      (pop-to-buffer (current-buffer)))))

;; -------------------------------------------------------------------
;;; Parser List Mode

(eval-and-compile
  (defmacro ts-parser:call-process (cmd &rest on-success)
    (declare (indent 1))
    (let ((res (make-symbol "res")))
      `(with-current-buffer (get-buffer-create "*ts-parser*")
         (erase-buffer)
         (let ((,res ,cmd))
           (if (processp ,res)
               (cl-letf ((filter
                          (lambda (p s)
                            (when (buffer-live-p (process-buffer p))
                              (with-current-buffer (process-buffer p)
                                (let ((inhibit-read-only t))
                                  (goto-char (point-max))
                                  (insert (xterm-color-filter
                                           (replace-regexp-in-string
                                            "[\r\n]+" "\n" s))))))))
                         (callback
                          (lambda (p _m)
                            (with-current-buffer (process-buffer p)
                              (if (zerop (process-exit-status p))
                                  (unwind-protect (progn ,@on-success)
                                    (kill-buffer))
                                (pop-to-buffer (current-buffer)))))))
                 (set-process-filter ,res filter)
                 (set-process-sentinel ,res callback))
             (if (zerop ,res)
                 (unwind-protect (progn ,@on-success)
                   (kill-buffer))
               (pop-to-buffer (current-buffer))))))))

  ;; Get neovim tree-sitter parser sources
  (defun ts-parser--get-sources ()
    (ts-parser:call-process
        (call-process-shell-command
         (format
          "LUA_PATH=\"%s/runtime/lua/?.lua;%s/lua/?.lua;${LUA_PATH:-;}\" %s"
          ts-parser-neovim-directory
          ts-parser-nvim-treesitter-directory
          (expand-file-name "bin/sources.lua" ts-util--dir))
         nil (current-buffer))
      (goto-char (point-min))
      (read (current-buffer)))))

(defvar ts-parser--sources (eval-when-compile (ts-parser--get-sources)))
(defun ts-parser-sources ()
  (or ts-parser--sources
      (setq ts-parser--sources (ts-parser--get-sources))
      (user-error "Failed to get neovim sources (is nvim installed?)")))

(defun ts-parser-list-browse-grammar ()
  "Browse the url of the grammar at point."
  (interactive)
  (when-let ((entry (tabulated-list-get-entry (point))))
    (browse-url (elt entry 1))))

(defun ts-parser-list-install-grammar ()
  "Install the tree-sitter grammar at point, using neovim recipe."
  (interactive)
  (when-let ((entry (tabulated-list-get-entry (point))))
    (let* ((lang (tabulated-list-get-id (point)))
           (lst (mapcar (lambda (s) (if (string-empty-p s) nil s))
                        (append entry nil)))
           (treesit-language-source-alist (list (cons lang (cdr lst)))))
      (treesit-install-language-grammar lang))))

(defun ts-parser-list-build-grammar ()
  "Download and build grammar at point in `ts-parser-grammar-directory'."
  (interactive)
  (when-let* ((entry (tabulated-list-get-entry (point)))
              (url (elt entry 1)))
    (unless (file-exists-p ts-parser-grammar-directory)
      (make-directory ts-parser-grammar-directory t))
    (let* ((default-directory ts-parser-grammar-directory)
           (src-dir (expand-file-name (file-name-base url))))
      (if (file-exists-p src-dir)
          (dired src-dir)
        (message "Building %s in %s..." url src-dir)
        (ts-parser:call-process
            (start-process-shell-command
             "ts-grammar" (current-buffer)
             (concat "git clone --depth=1 " url
                     "&& cd " src-dir
                     "&& npm --loglevel=info --progress=true install"
                     "&& {\nnpm run generate || true\n}"))
          (let ((grammar (expand-file-name "grammar.js" src-dir)))
            (if (file-exists-p grammar)
                (find-file-other-window grammar)
              (dired src-dir))))))))

(defvar ts-parser-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "w" #'ts-parser-list-browse-grammar)
    (define-key map "i" #'ts-parser-list-install-grammar)
    (define-key map "b" #'ts-parser-list-build-grammar)
    map))

(define-derived-mode ts-parser-list-mode tabulated-list-mode "TsParser"
  "Mode to view neovim tree-sitter parsers."
  (setq tabulated-list-format
        [("Parser" 10 t) ("Url" 50 t) ("Revision" 8 t) ("Location" 15 t)])
  (setq tabulated-list-sort-key '("Parser" . nil))
  (setq tabulated-list-entries (ts-parser-sources))
  (tabulated-list-init-header)
  (tabulated-list-print))

;;;###autoload
(defun ts-parser-list-sources (&optional language)
  "List tree-sitter parser sources used by neovim.
With prefix, prompt for LANGUAGE and return its source."
  (interactive (list (if current-prefix-arg (intern (read-string "Language: ")))))
  (if language
      (let ((source (cadr (assq language (ts-parser-sources)))))
        (prog1 source
          (message source)))
    (with-current-buffer (get-buffer-create "*treesitter-parsers*")
      (ts-parser-list-mode)
      (pop-to-buffer (current-buffer)))))

(provide 'ts-parser)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ts-parser.el ends here

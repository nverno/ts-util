;;; ts-sources.el --- Manage tree-sitter parser sources -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ts-util
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Created:  9 October 2024
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
;;
;; List, browse, clone, or install available tree-sitter parsers.
;;
;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'ts-util)


(defvar ts-sources-buffer "*Treesitter Parsers*")

(defun ts-sources-browse-repo (entry)
  "Goto the source repo for parser.
When called from `ts-sources-mode' use the grammar at point.
Otherwise, prompt for ENTRY."
  (interactive
   (list (if (eq major-mode 'ts-sources-mode)
             (tabulated-list-get-entry (point))
           (let ((srcs (ts-util-sources)))
             (and-let* ((grammar (completing-read "Grammar: " srcs nil t)))
               (car (assoc-default (intern grammar) srcs)))))))
  (and entry (browse-url (elt entry 2))))

(defun ts-sources-install-grammar ()
  "Install the tree-sitter grammar at point, using neovim recipe."
  (interactive)
  (when-let ((entry (tabulated-list-get-entry (point))))
    (let* ((lang (tabulated-list-get-id (point)))
           (lst (mapcar (lambda (s) (if (string-empty-p s) nil s))
                        (cdr (append entry nil))))
           (treesit-language-source-alist (list (cons lang (cdr lst)))))
      (treesit-install-language-grammar lang))))

(defun ts-sources--clone-grammar (url &optional build)
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
      (let ((cmd (apply #'concat
                        "git clone --depth=1 " url
                        (when build
                          (list "&& cd" src-dir
                                "&& npm --loglevel=info "
                                "--progress=true install")))))
        (cons src-dir (ts-util--call-process
                          (start-process-shell-command
                           "ts-source" (current-buffer) cmd)))))))

(defun ts-sources-clone-grammar (url &optional build callback)
  "Clone grammar at point in `ts-util-grammar-directory'.
With prefix, attempt to BUILD after cloning."
  (interactive (list (and-let* ((entry (tabulated-list-get-entry (point))))
                       (elt entry 2))
                     current-prefix-arg))
  (or url (user-error "No url"))
  (let* ((res (ts-sources--clone-grammar url build))
         (src-dir (car res)))
    (cl-letf*
        ((proc (cdr res))
         (go-fn (or callback
                    (lambda (src-dir)
                      (let ((grammar (expand-file-name
                                      "grammar.js" src-dir)))
                        (if (file-exists-p grammar)
                            (find-file-other-window grammar)
                          (dired-jump-other-window src-dir))))))
         (sentinel (lambda (p _m)
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


;;; Sources mode

(defvar-keymap ts-sources-mode-map
  :doc "Keymap in `ts-sources-mode'."
  "w" #'ts-sources-browse-repo
  "i" #'ts-sources-install-grammar
  "c" #'ts-sources-clone-grammar)

(easy-menu-define ts-sources-mode-menu ts-sources-mode-map
  "Menu for Ts Sources mode."
  '("Ts-Sources"
    ["Browse repo" ts-sources-browse-repo]
    ["Install grammar" ts-sources-install-grammar]
    ["Clone repo" ts-sources-clone-grammar]))

(define-derived-mode ts-sources-mode tabulated-list-mode "TsSources"
  "Mode to browse and install tree-sitter parser sources."
  (setq tabulated-list-format
        [("Inst" 5 t) ("Parser" 10 t) ("Url" 50) ("Revision" 8)
         ("Location" 15)])
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
(defun ts-sources (&optional language)
  "List tree-sitter parser sources used by neovim.
With prefix, prompt for LANGUAGE and return its source."
  (interactive (list (when current-prefix-arg
                       (intern (completing-read "Language: "
                                 (ts-util-sources))))))
  (if language
      (message (elt (car (cdr (assq language (ts-util-sources)))) 1))
    (with-current-buffer (get-buffer-create ts-sources-buffer)
      (ts-sources-mode)
      (pop-to-buffer (current-buffer)))))


(provide 'ts-sources)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ts-sources.el ends here

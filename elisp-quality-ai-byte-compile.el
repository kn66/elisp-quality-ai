;;; elisp-quality-ai-byte-compile.el --- Byte-compile collector for elisp-quality-ai -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: elisp-quality-ai contributors
;; Maintainer: elisp-quality-ai contributors
;; Package-Requires: ((emacs "29.1"))
;; Keywords: lisp, tools, maint

;;; Commentary:

;; Built-in collector that normalizes byte compiler warnings.

;;; Code:

(require 'bytecomp)
(require 'subr-x)
(require 'elisp-quality-ai-collector)

(defcustom elisp-quality-ai-byte-compile-load-path nil
  "Additional directories to add to `load-path' while byte-compiling files.
Relative paths are expanded against the directory of the file being compiled."
  :type '(repeat directory)
  :group 'elisp-quality-ai)

(defun elisp-quality-ai-byte-compile--output-file (file output-directory)
  "Return the temporary byte-compile output file for FILE in OUTPUT-DIRECTORY."
  (expand-file-name
   (concat (file-name-base file) ".elc")
   output-directory))

(defun elisp-quality-ai-byte-compile--load-path (file)
  "Return the `load-path' to use while compiling FILE."
  (let ((file-directory (file-name-directory (expand-file-name file))))
    (append
     (mapcar
      (lambda (directory)
        (expand-file-name directory file-directory))
      elisp-quality-ai-byte-compile-load-path)
     (list file-directory)
     load-path)))

(defun elisp-quality-ai-byte-compile--line-column (position)
  "Return a cons of one-based line and column for POSITION."
  (if (and (integer-or-marker-p position)
           (bound-and-true-p byte-compile-current-buffer)
           (buffer-live-p byte-compile-current-buffer))
      (with-current-buffer byte-compile-current-buffer
        (save-excursion
          (goto-char (min (point-max) (max (point-min) position)))
          (cons (line-number-at-pos)
                (1+ (current-column)))))
    (cons 1 nil)))

(defun elisp-quality-ai-byte-compile--diagnostic
    (file message position level)
  "Return a normalized byte compiler diagnostic for FILE.
MESSAGE, POSITION, and LEVEL come from `byte-compile-log-warning-function'."
  (let* ((location (elisp-quality-ai-byte-compile--line-column position))
         (line (car location))
         (column (cdr location))
         (severity (if (eq level :error) "error" "warning"))
         (diagnostic
          `(("source" . "byte-compile")
            ("collector" . "byte-compile")
            ("category" . "compile")
            ("severity" . ,severity)
            ("file" . ,(expand-file-name file))
            ("line" . ,line)
            ("message" . ,(string-trim message))
            ("suggestion" . "Resolve the byte compiler warning while preserving runtime behavior."))))
    (if column
        (append diagnostic `(("column" . ,column)))
      diagnostic)))

(defun elisp-quality-ai-byte-compile-collect-file (file)
  "Byte-compile FILE in a temporary directory and return diagnostics."
  (let* ((file (expand-file-name file))
         (output-directory (make-temp-file "elisp-quality-ai-byte-compile-" t))
         (log-buffer (generate-new-buffer-name " *elisp-quality-ai-byte-compile*"))
         diagnostics)
    (unwind-protect
        (let ((byte-compile-dest-file-function
               (lambda (source)
                 (elisp-quality-ai-byte-compile--output-file
                  source output-directory)))
              (byte-compile-log-buffer log-buffer)
              (byte-compile-verbose nil)
              (byte-compile-error-on-warn nil)
              (load-path (elisp-quality-ai-byte-compile--load-path file))
              (inhibit-message t))
          (let ((byte-compile-log-warning-function
                 (lambda (message position fill level)
                   (ignore fill)
                   (push (elisp-quality-ai-byte-compile--diagnostic
                          file message position level)
                         diagnostics))))
            (byte-compile-file file)))
      (when-let ((buffer (get-buffer log-buffer)))
        (kill-buffer buffer))
      (when (file-directory-p output-directory)
        (delete-directory output-directory t)))
    (nreverse diagnostics)))

(elisp-quality-ai-register-collector
 "byte-compile" #'elisp-quality-ai-byte-compile-collect-file
 :requires 'bytecomp
 :description "Run Emacs byte compilation in a temporary output directory."
 :available (lambda () (featurep 'bytecomp)))

(provide 'elisp-quality-ai-byte-compile)
;;; elisp-quality-ai-byte-compile.el ends here

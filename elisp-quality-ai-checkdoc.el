;;; elisp-quality-ai-checkdoc.el --- Checkdoc collector for elisp-quality-ai -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: elisp-quality-ai contributors
;; Maintainer: elisp-quality-ai contributors
;; Package-Requires: ((emacs "29.1"))
;; Keywords: lisp, tools, maint

;;; Commentary:

;; Built-in collector that normalizes `checkdoc' findings.

;;; Code:

(require 'checkdoc)
(require 'subr-x)
(require 'elisp-quality-ai-collector)

(defconst elisp-quality-ai-checkdoc--diagnostic-regexp
  "\\`\\(.+\\.el\\):\\([0-9]+\\): \\(.*\\)\\'"
  "Regexp matching one `checkdoc' diagnostic line.")

(defun elisp-quality-ai-checkdoc--parse-diagnostic-line (file line)
  "Return a normalized diagnostic for FILE from checkdoc output LINE."
  (when (string-match elisp-quality-ai-checkdoc--diagnostic-regexp line)
    (let ((line-number (max 1 (string-to-number (match-string 2 line))))
          (message (string-trim (match-string 3 line))))
      `(("source" . "checkdoc")
        ("collector" . "checkdoc")
        ("category" . "style")
        ("severity" . "warning")
        ("file" . ,(expand-file-name file))
        ("line" . ,line-number)
        ("message" . ,message)
        ("suggestion" . "Review the checkdoc warning and update comments, docstrings, or package structure.")))))

(defun elisp-quality-ai-checkdoc--diagnostic-lines (buffer-name)
  "Return raw diagnostic lines from BUFFER-NAME."
  (when-let ((buffer (get-buffer buffer-name)))
    (with-current-buffer buffer
      (split-string (buffer-string) "\n" t))))

(defun elisp-quality-ai-checkdoc-collect-file (file)
  "Run `checkdoc' on FILE and return normalized diagnostics."
  (let* ((file (expand-file-name file))
         (diagnostic-buffer (generate-new-buffer-name
                             " *elisp-quality-ai-checkdoc*"))
         diagnostics)
    (unwind-protect
        (with-temp-buffer
          (insert-file-contents file)
          (setq buffer-file-name file)
          (let ((default-directory (file-name-directory file))
                (checkdoc-autofix-flag 'never)
                (checkdoc-diagnostic-buffer diagnostic-buffer)
                (checkdoc-generate-compile-warnings-flag t)
                (checkdoc-package-keywords-flag nil)
                (checkdoc-pending-errors nil)
                (checkdoc-spellcheck-documentation-flag nil)
                (inhibit-message t))
            (emacs-lisp-mode)
            (checkdoc-current-buffer t)
            (setq diagnostics
                  (delq
                   nil
                   (mapcar
                    (lambda (line)
                      (elisp-quality-ai-checkdoc--parse-diagnostic-line
                       file line))
                    (elisp-quality-ai-checkdoc--diagnostic-lines
                     diagnostic-buffer))))))
      (when-let ((buffer (get-buffer diagnostic-buffer)))
        (kill-buffer buffer)))
    diagnostics))

(elisp-quality-ai-register-collector
 "checkdoc" #'elisp-quality-ai-checkdoc-collect-file
 :requires 'checkdoc
 :description "Run Emacs built-in checkdoc over each source file."
 :available (lambda () (featurep 'checkdoc)))

(provide 'elisp-quality-ai-checkdoc)
;;; elisp-quality-ai-checkdoc.el ends here

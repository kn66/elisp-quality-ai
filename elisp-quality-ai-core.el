;;; elisp-quality-ai-core.el --- Core analysis for elisp-quality-ai -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: elisp-quality-ai contributors
;; Maintainer: elisp-quality-ai contributors
;; Package-Requires: ((emacs "29.1"))
;; Keywords: lisp, tools, maint

;;; Commentary:

;; Core project scanning and definition inventory for elisp-quality-ai.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'elisp-quality-ai-collector)
(require 'elisp-quality-ai-task)

(defvar elisp-quality-ai-core--collecting-directory nil
  "Non-nil while collecting a directory report.")

(defcustom elisp-quality-ai-definition-length-threshold 80
  "Line-count threshold for definition size diagnostics."
  :type 'integer
  :group 'elisp-quality-ai)

(defcustom elisp-quality-ai-scan-excluded-directory-regexp
  (rx (or bos "/") (or ".git" ".eask" ".eldev" ".elsa" "dist" "build")
      (or "/" eos))
  "Regexp matching directories excluded from project scanning."
  :type 'regexp
  :group 'elisp-quality-ai)

(defconst elisp-quality-ai-core-definition-forms
  '(defun defmacro defsubst cl-defun cl-defmacro cl-defmethod
    defvar defconst defcustom
    define-minor-mode define-globalized-minor-mode define-derived-mode)
  "Top-level forms treated as named definitions.")

(defun elisp-quality-ai-core-project-root (directory)
  "Return the project root for DIRECTORY, or DIRECTORY if none is found."
  (let ((default-directory (file-name-as-directory (expand-file-name directory))))
    (if-let ((project (project-current nil)))
        (expand-file-name (project-root project))
      default-directory)))

(defun elisp-quality-ai-core--excluded-path-p (path root)
  "Return non-nil when PATH under ROOT should be excluded from scanning."
  (string-match-p elisp-quality-ai-scan-excluded-directory-regexp
                  (file-relative-name path root)))

(defun elisp-quality-ai-core-elisp-files (directory)
  "Return Emacs Lisp source files under DIRECTORY."
  (let ((root (file-name-as-directory (expand-file-name directory))))
    (seq-remove
     (lambda (file)
       (elisp-quality-ai-core--excluded-path-p file root))
     (directory-files-recursively
      root "\\.el\\'" nil
      (lambda (subdirectory)
        (not (elisp-quality-ai-core--excluded-path-p subdirectory root)))))))

(defun elisp-quality-ai-core--definition-form-p (form)
  "Return non-nil when FORM is a named top-level definition."
  (and (consp form)
       (memq (car form) elisp-quality-ai-core-definition-forms)
       (elisp-quality-ai-core--definition-name-form-p (cadr form))))

(defun elisp-quality-ai-core--definition-name-form-p (name)
  "Return non-nil when NAME is a supported definition name form."
  (or (and name (symbolp name))
      (and (consp name)
           (eq (car name) 'setf)
           (symbolp (cadr name))
           (null (cddr name)))))

(defun elisp-quality-ai-core--definition-name-string (name)
  "Return NAME as a JSON-friendly definition name string."
  (cond
   ((symbolp name) (symbol-name name))
   ((and (consp name) (eq (car name) 'setf) (symbolp (cadr name)))
    (format "(setf %s)" (symbol-name (cadr name))))
   (t (format "%S" name))))

(defun elisp-quality-ai-core--cl-defmethod-tail (form)
  "Return FORM's `cl-defmethod' argument list and following forms."
  (let ((tail (cddr form)))
    (while (and (consp tail) (keywordp (car tail)))
      (setq tail (if (eq (car tail) :extra)
                     (cddr tail)
                   (cdr tail))))
    tail))

(defun elisp-quality-ai-core--definition-docstring (form)
  "Return FORM's docstring, or nil when none is obvious."
  (pcase (car form)
    ((or 'defun 'defmacro 'defsubst 'cl-defun 'cl-defmacro)
     (and (stringp (nth 3 form)) (nth 3 form)))
    ('cl-defmethod
     (let ((tail (elisp-quality-ai-core--cl-defmethod-tail form)))
       (and (stringp (cadr tail)) (cadr tail))))
    ((or 'defvar 'defconst 'defcustom)
     (and (stringp (nth 3 form)) (nth 3 form)))
    ('define-minor-mode
     (and (stringp (nth 2 form)) (nth 2 form)))
    ('define-globalized-minor-mode
     (and (stringp (nth 4 form)) (nth 4 form)))
    ('define-derived-mode
     (and (stringp (nth 4 form)) (nth 4 form)))
    (_ nil)))

(defun elisp-quality-ai-core--declaration-form-p (form)
  "Return non-nil when FORM only declares an external binding.
A bare `(defvar SYMBOL)' tells the byte compiler about a special variable
without claiming package ownership of that public variable."
  (and (eq (car form) 'defvar)
       (symbolp (cadr form))
       (null (cddr form))))

(defun elisp-quality-ai-core--definition-body (form)
  "Return the body forms from FORM where that shape is known."
  (pcase (car form)
    ((or 'defun 'defmacro 'defsubst 'cl-defun 'cl-defmacro)
     (nthcdr 3 form))
    ('cl-defmethod
     (let ((tail (elisp-quality-ai-core--cl-defmethod-tail form)))
       (when tail
         (setq tail (cdr tail))
         (if (stringp (car tail)) (cdr tail) tail))))
    (_ nil)))

(defun elisp-quality-ai-core--interactive-p (form)
  "Return non-nil if FORM appears to define an interactive command."
  (or (memq (car form) '(define-minor-mode define-globalized-minor-mode
                         define-derived-mode))
      (cl-some
       (lambda (body-form)
         (and (consp body-form) (eq (car body-form) 'interactive)))
       (elisp-quality-ai-core--definition-body form))))

(defun elisp-quality-ai-core--public-symbol-p (symbol)
  "Return non-nil if SYMBOL does not look package-internal."
  (not (string-match-p
        "--"
        (if (symbolp symbol)
            (symbol-name symbol)
          (elisp-quality-ai-core--definition-name-string symbol)))))

(defun elisp-quality-ai-core--definition-at (form file start end)
  "Build a JSON-friendly definition object for FORM in FILE from START to END."
  (let* ((symbol (cadr form))
         (name (elisp-quality-ai-core--definition-name-string symbol))
         (kind (car form))
         (start-line (line-number-at-pos start))
         (end-line (line-number-at-pos end))
         (line-count (max 1 (1+ (- end-line start-line))))
         (docstring (elisp-quality-ai-core--definition-docstring form))
         (declaration (elisp-quality-ai-core--declaration-form-p form)))
    `(("name" . ,name)
      ("kind" . ,(symbol-name kind))
      ("file" . ,(expand-file-name file))
      ("line" . ,start-line)
      ("end_line" . ,end-line)
      ("public" . ,(if (elisp-quality-ai-core--public-symbol-p symbol) t :json-false))
      ("interactive" . ,(if (elisp-quality-ai-core--interactive-p form) t :json-false))
      ("docstring" . ,(if docstring t :json-false))
      ("declaration" . ,(if declaration t :json-false))
      ("metrics" . (("lines" . ,line-count))))))

(defun elisp-quality-ai-core--apply-safe-file-local-variables (file)
  "Apply safe file-local variables used by the Lisp reader for FILE.
Return nil on success, or a diagnostic when file-local variables cannot be
parsed or applied safely."
  (let ((enable-local-variables :safe)
        (enable-local-eval nil)
        (inhibit-message t))
    (condition-case error-data
        (progn
          (hack-local-variables)
          nil)
      (error
       `(("source" . "elisp-quality-ai")
         ("category" . "compile")
         ("severity" . "error")
         ("file" . ,(expand-file-name file))
         ("line" . 1)
         ("message" . ,(format "File-local variables could not be applied: %s"
                                (string-trim
                                 (error-message-string error-data))))
         ("suggestion" . "Fix malformed or unsafe file-local variables before relying on reader-sensitive analysis."))))))

(defun elisp-quality-ai-core--read-error-diagnostic (file start error-data)
  "Return a diagnostic for ERROR-DATA while reading FILE at START."
  `(("source" . "elisp-quality-ai")
    ("category" . "compile")
    ("severity" . "error")
    ("file" . ,(expand-file-name file))
    ("line" . ,(line-number-at-pos start))
    ("message" . ,(string-trim (error-message-string error-data)))
    ("suggestion" . "Fix the Emacs Lisp read syntax before relying on the definition inventory.")))

(defun elisp-quality-ai-core--read-definition-data (file)
  "Read top-level definition data from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (setq buffer-file-name file)
    (cl-letf (((symbol-function 'message) (lambda (&rest _args) nil)))
      ;; Avoid mode setup applying malformed file-local variables before we
      ;; can turn them into a normal diagnostic below.
      (let ((enable-local-variables nil)
            (enable-local-eval nil))
        (emacs-lisp-mode)))
    (let (definitions diagnostics)
      (when-let ((diagnostic
                  (elisp-quality-ai-core--apply-safe-file-local-variables
                   file)))
        (push diagnostic diagnostics))
      (goto-char (point-min))
      (catch 'done
        (while t
          (forward-comment (point-max))
          (when (eobp)
            (throw 'done nil))
          (let ((start (point)))
            (condition-case error-data
                (let* ((form (read (current-buffer)))
                       (end (point)))
                  (when (elisp-quality-ai-core--definition-form-p form)
                    (push (elisp-quality-ai-core--definition-at
                           form file start end)
                          definitions)))
              ((end-of-file invalid-read-syntax)
               (push (elisp-quality-ai-core--read-error-diagnostic
                      file start error-data)
                     diagnostics)
               (throw 'done nil))))))
      `(("definitions" . ,(vconcat (nreverse definitions)))
        ("diagnostics" . ,(vconcat (nreverse diagnostics)))))))

(defun elisp-quality-ai-core--read-definitions (file)
  "Read top-level definitions from FILE."
  (cdr (assoc "definitions"
              (elisp-quality-ai-core--read-definition-data file))))

(defun elisp-quality-ai-core--definition-diagnostics (definition)
  "Return built-in diagnostics for DEFINITION."
  (let* ((name (cdr (assoc "name" definition)))
         (file (cdr (assoc "file" definition)))
         (line (cdr (assoc "line" definition)))
         (public (eq (cdr (assoc "public" definition)) t))
         (docstring (eq (cdr (assoc "docstring" definition)) t))
         (declaration (eq (cdr (assoc "declaration" definition)) t))
         (lines (cdr (assoc "lines" (cdr (assoc "metrics" definition)))))
         diagnostics)
    (when (> lines elisp-quality-ai-definition-length-threshold)
      (push `(("source" . "elisp-quality-ai")
              ("category" . "size")
              ("severity" . "warning")
              ("file" . ,file)
              ("symbol" . ,name)
              ("line" . ,line)
              ("metric" . "definition-lines")
              ("value" . ,lines)
              ("threshold" . ,elisp-quality-ai-definition-length-threshold)
              ("message" . ,(format "%s has %d lines; threshold is %d."
                                     name lines
                                     elisp-quality-ai-definition-length-threshold))
              ("suggestion" . "Consider extracting smaller helpers while preserving the public interface."))
            diagnostics))
    (when (and public (not docstring) (not declaration))
      (push `(("source" . "elisp-quality-ai")
              ("category" . "documentation")
              ("severity" . "info")
              ("file" . ,file)
              ("symbol" . ,name)
              ("line" . ,line)
              ("message" . ,(format "%s has no obvious docstring." name))
              ("suggestion" . "Add a concise docstring that states behavior and important arguments."))
            diagnostics))
    (nreverse diagnostics)))

(defun elisp-quality-ai-core--file-diagnostics (definitions)
  "Return diagnostics for DEFINITIONS."
  (vconcat
   (apply #'append
          (mapcar #'elisp-quality-ai-core--definition-diagnostics
                  (append definitions nil)))))

(defun elisp-quality-ai-core-analyze-file (file)
  "Return an AI-oriented report object for FILE."
  (unless elisp-quality-ai-core--collecting-directory
    (elisp-quality-ai-collector-clear-failures))
  (let* ((file (expand-file-name file))
         (definition-data (elisp-quality-ai-core--read-definition-data file))
         (definitions (cdr (assoc "definitions" definition-data)))
         (read-diagnostics (cdr (assoc "diagnostics" definition-data)))
         (core-diagnostics
          (vconcat (elisp-quality-ai-core--file-diagnostics definitions)
                   read-diagnostics))
         (collector-diagnostics
          (elisp-quality-ai-run-collectors-for-file file))
         (diagnostics (vconcat core-diagnostics collector-diagnostics)))
    `(("schema" . "elisp-quality-ai.file/v1")
      ("file" . ,file)
      ("summary" . (("definitions" . ,(length definitions))
                    ("diagnostics" . ,(length diagnostics))))
      ("definitions" . ,definitions)
      ("diagnostics" . ,diagnostics)
      ("collectors" . ,(elisp-quality-ai-collector-metadata)))))

(defun elisp-quality-ai-core-analyze-directory (directory)
  "Return an AI-oriented report object for DIRECTORY."
  (elisp-quality-ai-collector-clear-failures)
  (let* ((root (file-name-as-directory (expand-file-name directory)))
         (files (elisp-quality-ai-core-elisp-files root))
         (file-reports (let ((elisp-quality-ai-core--collecting-directory t)
                             (elisp-quality-ai-collector--preserve-failures t))
                         (mapcar #'elisp-quality-ai-core-analyze-file files)))
         (definitions
          (apply #'+ (mapcar (lambda (report)
                               (length (cdr (assoc "definitions" report))))
                             file-reports)))
         (diagnostics
          (apply #'+ (mapcar (lambda (report)
                               (length (cdr (assoc "diagnostics" report))))
                             file-reports)))
         (partial-report
          `(("schema" . "elisp-quality-ai.project/v1")
            ("root" . ,root)
            ("summary" . (("files" . ,(length files))
                          ("definitions" . ,definitions)
                          ("diagnostics" . ,diagnostics)))
            ("files" . ,(vconcat file-reports))
            ("collectors" . ,(elisp-quality-ai-collector-metadata))))
         (tasks (elisp-quality-ai-task-generate partial-report)))
    `(("schema" . "elisp-quality-ai.project/v1")
      ("root" . ,root)
      ("summary" . (("files" . ,(length files))
                    ("definitions" . ,definitions)
                    ("diagnostics" . ,diagnostics)
                    ("tasks" . ,(length tasks))))
      ("files" . ,(vconcat file-reports))
      ("tasks" . ,tasks)
      ("collectors" . ,(elisp-quality-ai-collector-metadata)))))

(provide 'elisp-quality-ai-core)
;;; elisp-quality-ai-core.el ends here

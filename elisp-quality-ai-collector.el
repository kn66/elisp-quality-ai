;;; elisp-quality-ai-collector.el --- Collector API for elisp-quality-ai -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: elisp-quality-ai contributors
;; Maintainer: elisp-quality-ai contributors
;; Package-Requires: ((emacs "29.1"))
;; Keywords: lisp, tools, maint

;;; Commentary:

;; Registration and execution helpers for quality collectors.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup elisp-quality-ai nil
  "AI-friendly quality reports for Emacs Lisp."
  :group 'lisp
  :prefix "elisp-quality-ai-")

(defcustom elisp-quality-ai-disabled-collectors nil
  "Collector names to skip when running `elisp-quality-ai' reports."
  :type '(repeat string)
  :group 'elisp-quality-ai)

(defcustom elisp-quality-ai-collector-failure-message-max-length 1000
  "Maximum length of collector failure messages in diagnostics."
  :type 'natnum
  :group 'elisp-quality-ai)

(cl-defstruct (elisp-quality-ai-collector
               (:constructor elisp-quality-ai-collector--make))
  name function requires description enabled available)

(defvar elisp-quality-ai-collector-registry nil
  "Registered `elisp-quality-ai' collectors.")

(defun elisp-quality-ai-collector--plist-member-p (plist property)
  "Return non-nil when PLIST has PROPERTY."
  (not (null (memq property plist))))

(defun elisp-quality-ai-collector--name-string (name)
  "Return NAME as a collector name string."
  (let ((name-string
         (cond
          ((stringp name) name)
          ((symbolp name) (symbol-name name))
          (t (error "Collector name must be a string or symbol: %S" name)))))
    (when (string-empty-p name-string)
      (error "Collector name must not be empty"))
    name-string))

(defun elisp-quality-ai-collector--string-value (value)
  "Return VALUE as a string suitable for JSON output."
  (cond
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun elisp-quality-ai-collector--truncate-string (string max-length)
  "Return STRING truncated to MAX-LENGTH characters when needed."
  (if (and (natnump max-length)
           (> max-length 0)
           (> (length string) max-length))
      (concat (substring string 0 max-length) "...")
    string))

(defun elisp-quality-ai-collector--string-vector (values)
  "Return VALUES normalized to a vector of strings."
  (let ((items
         (cond
          ((null values) nil)
          ((vectorp values) (append values nil))
          ((listp values) values)
          (t (list values)))))
    (vconcat (mapcar #'elisp-quality-ai-collector--string-value items))))

(defun elisp-quality-ai-collector--json-bool (value)
  "Return VALUE as a JSON-friendly boolean."
  (if value t :json-false))

(defun elisp-quality-ai-collector--predicate-value (value)
  "Return boolean VALUE, calling it when it is a zero-argument function."
  (condition-case nil
      (if (functionp value)
          (funcall value)
        value)
    (error nil)))

(defun elisp-quality-ai-collector--disabled-p (collector)
  "Return non-nil when COLLECTOR is disabled by user configuration."
  (let ((name (elisp-quality-ai-collector-name collector)))
    (cl-some
     (lambda (disabled)
       (ignore-errors
         (string= name (elisp-quality-ai-collector--name-string disabled))))
     elisp-quality-ai-disabled-collectors)))

(defun elisp-quality-ai-collector--enabled-p (collector)
  "Return non-nil when COLLECTOR should be run."
  (and (elisp-quality-ai-collector--predicate-value
        (elisp-quality-ai-collector-enabled collector))
       (not (elisp-quality-ai-collector--disabled-p collector))))

(defun elisp-quality-ai-collector--available-p (collector)
  "Return non-nil when COLLECTOR's dependencies are available."
  (elisp-quality-ai-collector--predicate-value
   (elisp-quality-ai-collector-available collector)))

(defun elisp-quality-ai-register-collector (name function &rest properties)
  "Register collector NAME implemented by FUNCTION.
FUNCTION is called with one argument, the absolute file path to analyze.  It
should return nil, one diagnostic alist, or a list/vector of diagnostic alists.

PROPERTIES may include:

- `:requires' as a string, symbol, list, or vector naming dependencies.
- `:description' as a human-readable collector description.
- `:enabled' as a boolean or zero-argument predicate.  Defaults to t.
- `:available' as a boolean or zero-argument predicate.  Defaults to t."
  (unless (functionp function)
    (error "Collector function must be callable: %S" function))
  (let* ((name-string (elisp-quality-ai-collector--name-string name))
         (requires (elisp-quality-ai-collector--string-vector
                    (plist-get properties :requires)))
         (description (or (plist-get properties :description) ""))
         (enabled (if (elisp-quality-ai-collector--plist-member-p
                       properties :enabled)
                      (plist-get properties :enabled)
                    t))
         (available (if (elisp-quality-ai-collector--plist-member-p
                         properties :available)
                        (plist-get properties :available)
                      t))
         (collector (elisp-quality-ai-collector--make
                     :name name-string
                     :function function
                     :requires requires
                     :description description
                     :enabled enabled
                     :available available)))
    (setq elisp-quality-ai-collector-registry
          (append
           (cl-remove name-string elisp-quality-ai-collector-registry
                      :key #'elisp-quality-ai-collector-name
                      :test #'string=)
           (list collector)))
    collector))

(defun elisp-quality-ai-collector--collectors (collectors)
  "Return COLLECTORS or the global collector registry."
  (or collectors elisp-quality-ai-collector-registry))

(defun elisp-quality-ai-collector--metadata (collector)
  "Return JSON-friendly metadata for COLLECTOR."
  `(("name" . ,(elisp-quality-ai-collector-name collector))
    ("available" . ,(elisp-quality-ai-collector--json-bool
                     (elisp-quality-ai-collector--available-p collector)))
    ("requires" . ,(vconcat (elisp-quality-ai-collector-requires collector)))
    ("enabled" . ,(elisp-quality-ai-collector--json-bool
                   (elisp-quality-ai-collector--enabled-p collector)))
    ("description" . ,(elisp-quality-ai-collector-description collector))))

(defun elisp-quality-ai-collector-metadata (&optional collectors)
  "Return JSON-friendly metadata for registered COLLECTORS."
  (vconcat
   (mapcar #'elisp-quality-ai-collector--metadata
           (elisp-quality-ai-collector--collectors collectors))))

(defun elisp-quality-ai-collector--diagnostic-p (object)
  "Return non-nil when OBJECT is a diagnostic alist."
  (and (consp object)
       (consp (car object))
       (let ((key (caar object)))
         (or (stringp key) (symbolp key)))))

(defun elisp-quality-ai-collector--diagnostic-list (result)
  "Return RESULT normalized to a list of diagnostic alists."
  (cond
   ((null result) nil)
   ((vectorp result) (append result nil))
   ((elisp-quality-ai-collector--diagnostic-p result) (list result))
   ((listp result) result)
   (t (error "Collector returned unsupported result: %S" result))))

(defun elisp-quality-ai-collector--key-string (key)
  "Return KEY as a schema field name."
  (cond
   ((stringp key) key)
   ((symbolp key) (symbol-name key))
   (t (error "Diagnostic key must be a string or symbol: %S" key))))

(defun elisp-quality-ai-collector--normalize-keys (diagnostic)
  "Return DIAGNOSTIC with string keys."
  (mapcar
   (lambda (entry)
     (unless (consp entry)
       (error "Diagnostic entries must be cons cells: %S" entry))
     (cons (elisp-quality-ai-collector--key-string (car entry))
           (cdr entry)))
   diagnostic))

(defun elisp-quality-ai-collector--put-default (key value diagnostic)
  "Set KEY to VALUE in DIAGNOSTIC when KEY is missing or nil."
  (if-let ((entry (assoc key diagnostic)))
      (progn
        (unless (cdr entry)
          (setcdr entry value))
        diagnostic)
    (append diagnostic (list (cons key value)))))

(defun elisp-quality-ai-collector--positive-integer (value &optional default)
  "Return VALUE as a positive integer, or DEFAULT when it cannot be converted."
  (let ((number
         (cond
          ((integerp value) value)
          ((numberp value) (floor value))
          ((and (stringp value)
                (string-match-p "\\`[0-9]+\\'" value))
           (string-to-number value))
          (t nil))))
    (if (and number (> number 0))
        number
      default)))

(defun elisp-quality-ai-collector--normalize-location-field
    (key diagnostic &optional default)
  "Normalize numeric location KEY in DIAGNOSTIC.
When DEFAULT is nil and the value cannot be converted, remove KEY."
  (if-let ((entry (assoc key diagnostic)))
      (let ((number (elisp-quality-ai-collector--positive-integer
                     (cdr entry) default)))
        (if number
            (progn
              (setcdr entry number)
              diagnostic)
          (assoc-delete-all key diagnostic)))
    (if default
        (append diagnostic (list (cons key default)))
      diagnostic)))

(defun elisp-quality-ai-collector--normalize-location-fields (diagnostic)
  "Return DIAGNOSTIC with schema location fields normalized."
  (dolist (key '("line" "column" "end_line" "end_column") diagnostic)
    (setq diagnostic
          (elisp-quality-ai-collector--normalize-location-field
           key diagnostic (and (equal key "line") 1)))))

(defun elisp-quality-ai-collector--normalize-diagnostic
    (collector file diagnostic)
  "Return normalized DIAGNOSTIC from COLLECTOR for FILE."
  (unless (elisp-quality-ai-collector--diagnostic-p diagnostic)
    (error "Collector returned an invalid diagnostic: %S" diagnostic))
  (let* ((name (elisp-quality-ai-collector-name collector))
         (file (expand-file-name file))
         (normalized (elisp-quality-ai-collector--normalize-keys diagnostic)))
    (setq normalized
          (elisp-quality-ai-collector--put-default "source" name normalized))
    (setq normalized
          (elisp-quality-ai-collector--put-default "category" "collector"
                                                   normalized))
    (setq normalized
          (elisp-quality-ai-collector--put-default "severity" "warning"
                                                   normalized))
    (setq normalized
          (elisp-quality-ai-collector--put-default "file" file normalized))
    (setq normalized
          (elisp-quality-ai-collector--put-default "line" 1 normalized))
    (setq normalized
          (elisp-quality-ai-collector--put-default
           "message" (format "Collector %s reported a finding." name)
           normalized))
    (setq normalized
          (elisp-quality-ai-collector--put-default
           "suggestion" "Review the collector finding." normalized))
    (setq normalized
          (elisp-quality-ai-collector--put-default "collector" name normalized))
    (when-let ((file-entry (assoc "file" normalized)))
      (setcdr file-entry (expand-file-name
                          (elisp-quality-ai-collector--string-value
                           (cdr file-entry)))))
    (setq normalized
          (elisp-quality-ai-collector--normalize-location-fields normalized))
    normalized))

(defun elisp-quality-ai-collector--failure-diagnostic (collector file error-data)
  "Return a diagnostic for COLLECTOR failure on FILE with ERROR-DATA."
  (let ((name (elisp-quality-ai-collector-name collector))
        (message
         (elisp-quality-ai-collector--truncate-string
          (error-message-string error-data)
          elisp-quality-ai-collector-failure-message-max-length)))
    `(("source" . ,name)
      ("collector" . ,name)
      ("category" . "collector")
      ("severity" . "warning")
      ("file" . ,(expand-file-name file))
      ("line" . 1)
      ("message" . ,(format "Collector %s failed: %s"
                             name message))
      ("suggestion" . "Review the collector configuration or disable the collector."))))

(defun elisp-quality-ai-collector--run-one (collector file)
  "Run COLLECTOR for FILE and return a list of diagnostics."
  (condition-case error-data
      (mapcar
       (lambda (diagnostic)
         (elisp-quality-ai-collector--normalize-diagnostic
          collector file diagnostic))
       (elisp-quality-ai-collector--diagnostic-list
        (funcall (elisp-quality-ai-collector-function collector)
                 (expand-file-name file))))
    (error
     (list (elisp-quality-ai-collector--failure-diagnostic
            collector file error-data)))))

(defun elisp-quality-ai-run-collectors-for-file (file &optional collectors)
  "Run enabled and available COLLECTORS for FILE.
Return a vector of normalized diagnostics.  When COLLECTORS is nil, use
`elisp-quality-ai-collector-registry'."
  (let ((file (expand-file-name file))
        diagnostics)
    (dolist (collector (elisp-quality-ai-collector--collectors collectors))
      (when (and (elisp-quality-ai-collector--enabled-p collector)
                 (elisp-quality-ai-collector--available-p collector))
        (setq diagnostics
              (append diagnostics
                      (elisp-quality-ai-collector--run-one collector file)))))
    (vconcat diagnostics)))

(defun elisp-quality-ai-collector--excluded-directory-regexp ()
  "Return the active directory exclusion regexp."
  (let ((variable 'elisp-quality-ai-scan-excluded-directory-regexp))
    (if (boundp variable)
      (symbol-value variable)
      (rx (or bos "/") (or ".git" ".eask" ".eldev" ".elsa" "dist" "build")
          (or "/" eos)))))

(defun elisp-quality-ai-collector--excluded-path-p (path root)
  "Return non-nil when PATH under ROOT should be excluded from scanning."
  (string-match-p (elisp-quality-ai-collector--excluded-directory-regexp)
                  (file-relative-name path root)))

(defun elisp-quality-ai-collector--elisp-files (root)
  "Return Emacs Lisp source files under ROOT for collector execution."
  (directory-files-recursively
   root "\\.el\\'" nil
   (lambda (subdirectory)
     (not (elisp-quality-ai-collector--excluded-path-p subdirectory root)))))

(defun elisp-quality-ai-run-collectors-for-directory
    (directory &optional files collectors)
  "Run COLLECTORS for Emacs Lisp FILES in DIRECTORY.
FILES defaults to every `.el' file under DIRECTORY.  Return a vector of objects
with `file' and `diagnostics' fields."
  (let* ((root (file-name-as-directory (expand-file-name directory)))
         (files (or files (elisp-quality-ai-collector--elisp-files root))))
    (vconcat
     (mapcar
      (lambda (file)
        (let ((absolute-file (expand-file-name file root)))
          `(("file" . ,absolute-file)
            ("diagnostics" . ,(elisp-quality-ai-run-collectors-for-file
                               absolute-file collectors)))))
      files))))

(defalias 'elisp-quality-ai-run-collectors-for-project
  #'elisp-quality-ai-run-collectors-for-directory
  "Run collectors for DIRECTORY, treating it as a project root.")

(provide 'elisp-quality-ai-collector)
;;; elisp-quality-ai-collector.el ends here

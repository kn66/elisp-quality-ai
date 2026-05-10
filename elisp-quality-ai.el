;;; elisp-quality-ai.el --- AI-friendly quality reports for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: elisp-quality-ai contributors
;; Maintainer: elisp-quality-ai contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: lisp, tools, maint
;; URL: https://example.com/elisp-quality-ai

;;; Commentary:

;; This package builds AI-friendly quality reports for Emacs Lisp projects.
;; It provides a small core report format and collector API for tools such as
;; checkdoc, byte compilation, package-lint, relint, complexity metrics, Elsa,
;; and coverage tools.

;;; Code:

(require 'json)
(require 'seq)
(require 'subr-x)
(require 'elisp-quality-ai-core)
(require 'elisp-quality-ai-checkdoc)
(require 'elisp-quality-ai-byte-compile)
(require 'elisp-quality-ai-external)
(require 'elisp-quality-ai-report)

(defcustom elisp-quality-ai-default-format 'json
  "Default output format for `elisp-quality-ai-write-report'."
  :type '(choice (const :tag "JSON" json)
                 (const :tag "JSON Lines" jsonl)
                 (const :tag "Markdown" markdown))
  :group 'elisp-quality-ai)

(defcustom elisp-quality-ai-fast-disabled-collectors
  '("byte-compile" "relint" "package-lint" "cognitive-complexity" "elsa")
  "Collectors disabled by `elisp-quality-ai' fast CLI mode."
  :type '(repeat string)
  :group 'elisp-quality-ai)

;;;###autoload
(defun elisp-quality-ai-analyze-file (file)
  "Return an AI-oriented analysis report for FILE."
  (elisp-quality-ai-core-analyze-file file))

;;;###autoload
(defun elisp-quality-ai-analyze-directory (directory)
  "Return an AI-oriented analysis report for DIRECTORY."
  (elisp-quality-ai-core-analyze-directory directory))

;;;###autoload
(defun elisp-quality-ai-analyze-project (&optional directory)
  "Return an AI-oriented analysis report for the project at DIRECTORY.
When DIRECTORY is nil, use `default-directory'."
  (elisp-quality-ai-core-analyze-directory
   (elisp-quality-ai-core-project-root (or directory default-directory))))

;;;###autoload
(defun elisp-quality-ai-tasks (&optional directory limit)
  "Return ranked AI task objects for DIRECTORY.
When DIRECTORY is nil, use `default-directory'.  LIMIT bounds the number of
tasks returned when it is a positive integer."
  (let* ((report (elisp-quality-ai-analyze-directory
                  (or directory default-directory)))
         (tasks (append (cdr (assoc "tasks" report)) nil)))
    (vconcat
     (if (and (integerp limit) (> limit 0))
         (seq-take tasks limit)
       tasks))))

;;;###autoload
(defun elisp-quality-ai-next-task (&optional directory)
  "Return the highest ranked AI task for DIRECTORY, or nil.
When DIRECTORY is nil, use `default-directory'."
  (let ((tasks (elisp-quality-ai-tasks directory 1)))
    (and (> (length tasks) 0)
         (aref tasks 0))))

;;;###autoload
(defun elisp-quality-ai-report-current-project (&optional format)
  "Display a quality report for the current project using FORMAT.
FORMAT defaults to `elisp-quality-ai-default-format'."
  (interactive)
  (let* ((format (or format elisp-quality-ai-default-format))
         (report (elisp-quality-ai-analyze-project default-directory))
         (buffer (get-buffer-create "*elisp-quality-ai*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (elisp-quality-ai-report-to-string report format))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buffer)))

;;;###autoload
(defun elisp-quality-ai-write-report
    (directory output-file &optional format include-definitions)
  "Analyze DIRECTORY and write a quality report to OUTPUT-FILE.
FORMAT defaults to `elisp-quality-ai-default-format'.  Supported formats are
`json', `jsonl', and `markdown'.  When OUTPUT-FILE is nil or \"-\", write the
report to standard output.
When INCLUDE-DEFINITIONS is non-nil, include the full definition inventory."
  (interactive
   (list (read-directory-name "Directory: " nil nil t)
         (read-file-name "Output file: " nil nil nil "quality-report.json")
         elisp-quality-ai-default-format))
  (let* ((format (or format elisp-quality-ai-default-format))
         (report (elisp-quality-ai-analyze-directory directory))
         (content (elisp-quality-ai-report-to-string
                   report format include-definitions
                   (not (null include-definitions)))))
    (if (or (null output-file) (equal output-file "-"))
        (progn
          (princ content)
          nil)
      (with-temp-file output-file
        (insert content))
      output-file)))

(defun elisp-quality-ai--json-string (object)
  "Return OBJECT encoded as pretty JSON."
  (let ((json-encoding-pretty-print t))
    (concat (json-encode object) "\n")))

(defun elisp-quality-ai--jsonl-string (objects)
  "Return OBJECTS encoded as JSON Lines."
  (concat (mapconcat #'json-encode (append objects nil) "\n") "\n"))

(defun elisp-quality-ai--task-markdown (tasks)
  "Return TASKS encoded as a compact Markdown list."
  (if (= 0 (length tasks))
      "No tasks.\n"
    (concat
     (mapconcat
      (lambda (task)
        (format "- `%s` Priority %s at `%s:%s` - %s"
                (cdr (assoc "id" task))
                (cdr (assoc "priority" task))
                (cdr (assoc "file" task))
                (cdr (assoc "line" task))
                (cdr (assoc "title" task))))
      (append tasks nil)
      "\n")
     "\n")))

(defun elisp-quality-ai--object-to-string (object format)
  "Return OBJECT encoded using FORMAT."
  (pcase format
    ('json (elisp-quality-ai--json-string object))
    ('jsonl (elisp-quality-ai--jsonl-string
             (if (vectorp object) object (vector object))))
    ('markdown (if (vectorp object)
                   (elisp-quality-ai--task-markdown object)
                 (elisp-quality-ai--task-markdown (vector object))))
    (_ (error "Unsupported elisp-quality-ai output format: %S" format))))

(defun elisp-quality-ai--parse-format (value)
  "Return output format symbol for VALUE."
  (pcase value
    ((or "json" 'json) 'json)
    ((or "jsonl" 'jsonl) 'jsonl)
    ((or "markdown" "md" 'markdown) 'markdown)
    (_ (error "Unsupported format: %s" value))))

(defun elisp-quality-ai--parse-positive-integer (value option)
  "Return VALUE parsed as a positive integer for OPTION."
  (let ((number (and value (string-to-number value))))
    (unless (and (integerp number) (> number 0))
      (error "%s expects a positive integer, got %S" option value))
    number))

(defun elisp-quality-ai--cli-pop-value (option args)
  "Pop OPTION's value from ARGS."
  (unless args
    (error "%s expects a value" option))
  (cons (car args) (cdr args)))

(defun elisp-quality-ai--cli-options (args)
  "Parse command line ARGS into an alist."
  (when (equal (car args) "--")
    (setq args (cdr args)))
  (let ((options (list (cons 'command (or (car args) "report"))
                       (cons 'root default-directory)
                       (cons 'format elisp-quality-ai-default-format)
                       (cons 'output "-")
                       (cons 'limit nil)
                       (cons 'fast nil)
                       (cons 'load-paths nil)
                       (cons 'load-files nil)
                       (cons 'include-definitions nil)
                       (cons 'fail-on nil)))
        (rest (cdr args)))
    (while rest
      (let ((arg (pop rest)))
        (pcase arg
          ("--root"
           (let ((value (elisp-quality-ai--cli-pop-value arg rest)))
             (setcdr (assoc 'root options) (car value))
             (setq rest (cdr value))))
          ("--format"
           (let ((value (elisp-quality-ai--cli-pop-value arg rest)))
             (setcdr (assoc 'format options)
                     (elisp-quality-ai--parse-format (car value)))
             (setq rest (cdr value))))
          ("--output"
           (let ((value (elisp-quality-ai--cli-pop-value arg rest)))
             (setcdr (assoc 'output options) (car value))
             (setq rest (cdr value))))
          ("--limit"
           (let ((value (elisp-quality-ai--cli-pop-value arg rest)))
             (setcdr (assoc 'limit options)
                     (elisp-quality-ai--parse-positive-integer
                      (car value) arg))
             (setq rest (cdr value))))
          ("--fast"
           (setcdr (assoc 'fast options) t))
          ("--full"
           (setcdr (assoc 'fast options) nil))
          ("--load-path"
           (let ((value (elisp-quality-ai--cli-pop-value arg rest)))
             (push (car value) (cdr (assoc 'load-paths options)))
             (setq rest (cdr value))))
          ("--load"
           (let ((value (elisp-quality-ai--cli-pop-value arg rest)))
             (push (car value) (cdr (assoc 'load-files options)))
             (setq rest (cdr value))))
          ("--include-definitions"
           (setcdr (assoc 'include-definitions options) t))
          ("--fail-on"
           (let ((value (elisp-quality-ai--cli-pop-value arg rest)))
             (setcdr (assoc 'fail-on options) (car value))
             (setq rest (cdr value))))
          (_ (error "Unknown option: %s" arg)))))
    options))

(defun elisp-quality-ai--cli-apply-load-options (load-paths load-files)
  "Apply CLI LOAD-PATHS and LOAD-FILES before running analysis."
  (let ((directories
         (mapcar #'expand-file-name (nreverse (copy-sequence load-paths)))))
    (dolist (directory directories)
      (add-to-list 'load-path directory t))
    (setq elisp-quality-ai-byte-compile-load-path
          (append directories elisp-quality-ai-byte-compile-load-path)))
  (dolist (file (nreverse (copy-sequence load-files)))
    (load (expand-file-name file) nil t t)))

(defun elisp-quality-ai--write-output (content output)
  "Write CONTENT to OUTPUT, or standard output when OUTPUT is nil or \"-\"."
  (if (or (null output) (equal output "-"))
      (princ content)
    (with-temp-file output
      (insert content))))

(defun elisp-quality-ai--diagnostic-severity-rank (severity)
  "Return numeric rank for diagnostic SEVERITY."
  (pcase severity
    ("error" 3)
    ("warning" 2)
    ("info" 1)
    (_ 0)))

(defun elisp-quality-ai--report-has-fail-on-severity-p (report fail-on)
  "Return non-nil when REPORT has a diagnostic at FAIL-ON severity or worse."
  (when fail-on
    (let ((threshold (elisp-quality-ai--diagnostic-severity-rank fail-on)))
      (seq-some
       (lambda (file-report)
         (seq-some
          (lambda (diagnostic)
            (>= (elisp-quality-ai--diagnostic-severity-rank
                 (cdr (assoc "severity" diagnostic)))
                threshold))
          (append (cdr (assoc "diagnostics" file-report)) nil)))
       (append (cdr (assoc "files" report)) nil)))))

(defun elisp-quality-ai-cli-run (&optional args)
  "Run the elisp-quality-ai command line interface with ARGS.
Return the intended process exit code."
  (let* ((options (elisp-quality-ai--cli-options
                   (or args command-line-args-left)))
         (command (cdr (assoc 'command options)))
         (root (cdr (assoc 'root options)))
         (format (cdr (assoc 'format options)))
         (output (cdr (assoc 'output options)))
         (limit (cdr (assoc 'limit options)))
         (fail-on (cdr (assoc 'fail-on options)))
         (include-definitions (cdr (assoc 'include-definitions options)))
         (load-paths (cdr (assoc 'load-paths options)))
         (load-files (cdr (assoc 'load-files options)))
         (elisp-quality-ai-disabled-collectors
          (if (cdr (assoc 'fast options))
              (append elisp-quality-ai-fast-disabled-collectors
                      elisp-quality-ai-disabled-collectors)
            elisp-quality-ai-disabled-collectors))
         content
         report)
    (elisp-quality-ai--cli-apply-load-options load-paths load-files)
    (pcase command
      ("report"
       (setq report (elisp-quality-ai-analyze-directory root))
       (setq content
             (elisp-quality-ai-report-to-string
              report format include-definitions t)))
      ("tasks"
       (setq report (elisp-quality-ai-analyze-directory root))
       (setq content
             (elisp-quality-ai--object-to-string
              (let ((tasks (append (cdr (assoc "tasks" report)) nil)))
                (vconcat (if limit (seq-take tasks limit) tasks)))
              format)))
      ("next-task"
       (setq report (elisp-quality-ai-analyze-directory root))
       (setq content
             (elisp-quality-ai--object-to-string
              (or (and (> (length (cdr (assoc "tasks" report))) 0)
                       (aref (cdr (assoc "tasks" report)) 0))
                  [])
              format)))
      (_
       (error "Unknown command: %s" command)))
    (elisp-quality-ai--write-output content output)
    (if (elisp-quality-ai--report-has-fail-on-severity-p report fail-on) 2 0)))

;;;###autoload
(defun elisp-quality-ai-cli-main (&optional args)
  "Run the command line interface with ARGS and exit Emacs."
  (let ((exit-code
         (condition-case error-data
             (elisp-quality-ai-cli-run args)
           (error
            (princ (format "elisp-quality-ai: %s\n"
                           (error-message-string error-data))
                   #'external-debugging-output)
            1))))
    (kill-emacs exit-code)))

(provide 'elisp-quality-ai)
;;; elisp-quality-ai.el ends here

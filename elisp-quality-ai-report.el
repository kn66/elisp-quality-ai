;;; elisp-quality-ai-report.el --- Report exporters for elisp-quality-ai -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: elisp-quality-ai contributors
;; Maintainer: elisp-quality-ai contributors
;; Package-Requires: ((emacs "29.1"))
;; Keywords: lisp, tools, maint

;;; Commentary:

;; JSON, JSONL, and Markdown exporters for elisp-quality-ai reports.

;;; Code:

(require 'json)
(require 'seq)
(require 'subr-x)

(defcustom elisp-quality-ai-report-top-task-limit 5
  "Maximum number of top tasks to highlight in report views."
  :type 'integer
  :group 'elisp-quality-ai)

(defcustom elisp-quality-ai-report-include-definitions nil
  "Non-nil means report exporters include full definition inventories.
The default report output is optimized for AI context by emitting diagnostics,
tasks, summaries, and collector metadata without every successful definition."
  :type 'boolean
  :group 'elisp-quality-ai)

(defun elisp-quality-ai-report--get (key object)
  "Return KEY from alist OBJECT."
  (cdr (assoc key object)))

(defun elisp-quality-ai-report--schema (report)
  "Return REPORT's schema identifier."
  (elisp-quality-ai-report--get "schema" report))

(defun elisp-quality-ai-report--project-report-p (report)
  "Return non-nil when REPORT is a project report."
  (equal (elisp-quality-ai-report--schema report)
         "elisp-quality-ai.project/v1"))

(defun elisp-quality-ai-report--file-report-p (report)
  "Return non-nil when REPORT is a file report."
  (equal (elisp-quality-ai-report--schema report)
         "elisp-quality-ai.file/v1"))

(defun elisp-quality-ai-report--file-reports (report)
  "Return file reports contained in REPORT as a list."
  (cond
   ((elisp-quality-ai-report--project-report-p report)
    (append (or (elisp-quality-ai-report--get "files" report) []) nil))
   ((elisp-quality-ai-report--file-report-p report)
    (list report))
   (t
    (error "Unsupported elisp-quality-ai report schema: %S"
           (elisp-quality-ai-report--schema report)))))

(defun elisp-quality-ai-report--tasks-list (report)
  "Return task reports contained in REPORT as a list."
  (cond
   ((elisp-quality-ai-report--project-report-p report)
    (append (or (elisp-quality-ai-report--get "tasks" report) []) nil))
   ((elisp-quality-ai-report--file-report-p report)
    nil)
   (t
    (error "Unsupported elisp-quality-ai report schema: %S"
           (elisp-quality-ai-report--schema report)))))

(defun elisp-quality-ai-report--json (report)
  "Return REPORT encoded as pretty JSON."
  (let ((json-encoding-pretty-print t))
    (concat (json-encode report) "\n")))

(defun elisp-quality-ai-report--compact-file (file-report)
  "Return a compact copy of FILE-REPORT for default AI-oriented output."
  (let ((diagnostics (or (elisp-quality-ai-report--get "diagnostics" file-report)
                         [])))
    `(("schema" . ,(elisp-quality-ai-report--get "schema" file-report))
      ("file" . ,(elisp-quality-ai-report--get "file" file-report))
      ("summary" . ,(elisp-quality-ai-report--get "summary" file-report))
      ("diagnostics" . ,diagnostics)
      ("collectors" . ,(or (elisp-quality-ai-report--get "collectors" file-report)
                           [])))))

(defun elisp-quality-ai-report--compact-project (report)
  "Return a compact copy of project REPORT for default AI-oriented output."
  (let* ((files (elisp-quality-ai-report--file-reports report))
         (files-with-diagnostics
          (seq-filter
           (lambda (file-report)
             (> (length (or (elisp-quality-ai-report--get "diagnostics"
                                                          file-report)
                            []))
                0))
           files)))
    `(("schema" . ,(elisp-quality-ai-report--get "schema" report))
      ("root" . ,(elisp-quality-ai-report--get "root" report))
      ("summary" . ,(elisp-quality-ai-report--get "summary" report))
      ("files" . ,(vconcat (mapcar #'elisp-quality-ai-report--compact-file
                                    files-with-diagnostics)))
      ("tasks" . ,(or (elisp-quality-ai-report--get "tasks" report) []))
      ("collectors" . ,(or (elisp-quality-ai-report--get "collectors" report)
                           [])))))

(defun elisp-quality-ai-report--compact (report)
  "Return REPORT without successful definition inventories."
  (cond
   ((elisp-quality-ai-report--project-report-p report)
    (elisp-quality-ai-report--compact-project report))
   ((elisp-quality-ai-report--file-report-p report)
    (elisp-quality-ai-report--compact-file report))
   (t
    (error "Unsupported elisp-quality-ai report schema: %S"
           (elisp-quality-ai-report--schema report)))))

(defun elisp-quality-ai-report--definition-events (report)
  "Return JSONL events for definitions in REPORT."
  (let (events)
    (dolist (file-report (elisp-quality-ai-report--file-reports report))
      (dolist (definition (append (elisp-quality-ai-report--get "definitions" file-report) nil))
        (push `(("type" . "definition")
                ("schema" . "elisp-quality-ai.event/v1")
                ("file" . ,(elisp-quality-ai-report--get "file" definition))
                ("symbol" . ,(elisp-quality-ai-report--get "name" definition))
                ("kind" . ,(elisp-quality-ai-report--get "kind" definition))
                ("line" . ,(elisp-quality-ai-report--get "line" definition))
                ("end_line" . ,(elisp-quality-ai-report--get "end_line" definition))
                ("metrics" . ,(elisp-quality-ai-report--get "metrics" definition)))
              events)))
    (nreverse events)))

(defun elisp-quality-ai-report--diagnostic-events (report)
  "Return JSONL events for diagnostics in REPORT."
  (let (events)
    (dolist (file-report (elisp-quality-ai-report--file-reports report))
      (dolist (diagnostic (append (elisp-quality-ai-report--get "diagnostics" file-report) nil))
        (push (append '(("type" . "diagnostic")
                       ("schema" . "elisp-quality-ai.event/v1"))
                      diagnostic)
              events)))
    (nreverse events)))

(defun elisp-quality-ai-report--task-events (report)
  "Return JSONL events for tasks in REPORT."
  (mapcar
   (lambda (task)
     (append '(("type" . "task")
               ("schema" . "elisp-quality-ai.event/v1"))
	             task))
   (elisp-quality-ai-report--tasks-list report)))

(defun elisp-quality-ai-report--top-tasks (report)
  "Return the top ranked tasks from REPORT."
  (seq-take (elisp-quality-ai-report--tasks-list report)
            elisp-quality-ai-report-top-task-limit))

(defun elisp-quality-ai-report--top-task-events (report)
  "Return JSONL events for top ranked tasks in REPORT."
  (let ((rank 0))
    (mapcar
     (lambda (task)
       (setq rank (1+ rank))
       (append `(("type" . "top_task")
                 ("schema" . "elisp-quality-ai.event/v1")
                 ("rank" . ,rank))
               task))
     (elisp-quality-ai-report--top-tasks report))))

(defun elisp-quality-ai-report--jsonl (report include-definitions)
  "Return REPORT encoded as JSON Lines.
When INCLUDE-DEFINITIONS is non-nil, emit definition events."
  (let ((events (append (and include-definitions
                             (elisp-quality-ai-report--definition-events report))
                        (elisp-quality-ai-report--diagnostic-events report)
                        (elisp-quality-ai-report--top-task-events report)
                        (elisp-quality-ai-report--task-events report))))
    (concat (mapconcat #'json-encode events "\n") "\n")))

(defun elisp-quality-ai-report--markdown-top-issues (report)
  "Return a Markdown section for top ranked tasks in REPORT."
  (let ((top-tasks (elisp-quality-ai-report--top-tasks report))
        (rank 0))
    (concat
     "## Top Issues\n\n"
     (if (null top-tasks)
         "No top issues.\n\n"
       (concat
        (mapconcat
         (lambda (task)
           (setq rank (1+ rank))
           (format "%d. `%s` Priority %s at `%s:%s` - %s"
                   rank
                   (elisp-quality-ai-report--get "id" task)
                   (elisp-quality-ai-report--get "priority" task)
                   (elisp-quality-ai-report--get "file" task)
                   (elisp-quality-ai-report--get "line" task)
                   (elisp-quality-ai-report--get "title" task)))
         top-tasks
         "\n")
        "\n\n")))))

(defun elisp-quality-ai-report--markdown-tasks (tasks)
  "Return a Markdown section for TASKS."
  (concat
   "## Tasks\n\n"
   (if (= 0 (length tasks))
       "No tasks.\n\n"
     (concat
      (mapconcat
       (lambda (task)
         (let ((reasons (append (elisp-quality-ai-report--get "reason" task) nil))
               (steps (append (elisp-quality-ai-report--get "suggested_steps" task) nil)))
           (concat
            (format "### %s\n\n" (elisp-quality-ai-report--get "title" task))
            (format "- ID: `%s`\n" (elisp-quality-ai-report--get "id" task))
            (format "- Priority: %s\n" (elisp-quality-ai-report--get "priority" task))
            (format "- Location: `%s:%s`\n"
                    (elisp-quality-ai-report--get "file" task)
                    (elisp-quality-ai-report--get "line" task))
            (when (elisp-quality-ai-report--get "symbol" task)
              (format "- Symbol: `%s`\n" (elisp-quality-ai-report--get "symbol" task)))
            "\nReason:\n"
            (mapconcat (lambda (reason) (format "- %s" reason)) reasons "\n")
            "\n\nSuggested steps:\n"
            (mapconcat (lambda (step) (format "- %s" step)) steps "\n")
            "\n")))
       (append tasks nil)
       "\n")
      "\n"))))

(defun elisp-quality-ai-report--markdown-diagnostics (files)
  "Return a Markdown section for diagnostics in FILES."
  (concat
   "## Diagnostics\n\n"
   (let ((sections
          (delq
           nil
           (mapcar
            (lambda (file-report)
              (let ((diagnostics (elisp-quality-ai-report--get "diagnostics" file-report)))
                (unless (= 0 (length diagnostics))
                  (concat
                   (format "### `%s`\n\n" (elisp-quality-ai-report--get "file" file-report))
                   (mapconcat
                    (lambda (diagnostic)
                      (format "- `%s:%s` **%s/%s** %s\n  Suggestion: %s"
                              (elisp-quality-ai-report--get "file" diagnostic)
                              (elisp-quality-ai-report--get "line" diagnostic)
                              (elisp-quality-ai-report--get "severity" diagnostic)
                              (elisp-quality-ai-report--get "category" diagnostic)
                              (elisp-quality-ai-report--get "message" diagnostic)
                              (elisp-quality-ai-report--get "suggestion" diagnostic)))
                    (append diagnostics nil)
                    "\n")
                   "\n"))))
            (append files nil)))))
     (if sections
         (mapconcat #'identity sections "\n")
       "No diagnostics.\n"))))

(defun elisp-quality-ai-report--markdown-project (report)
  "Return project REPORT encoded as Markdown."
  (let* ((summary (elisp-quality-ai-report--get "summary" report))
         (files (elisp-quality-ai-report--get "files" report))
         (tasks (elisp-quality-ai-report--get "tasks" report)))
    (concat
     "# elisp-quality-ai report\n\n"
     (format "- Root: `%s`\n" (elisp-quality-ai-report--get "root" report))
     (format "- Files: %s\n" (elisp-quality-ai-report--get "files" summary))
     (format "- Definitions: %s\n" (elisp-quality-ai-report--get "definitions" summary))
     (format "- Diagnostics: %s\n" (elisp-quality-ai-report--get "diagnostics" summary))
     (format "- Tasks: %s\n\n" (elisp-quality-ai-report--get "tasks" summary))
     (elisp-quality-ai-report--markdown-top-issues report)
     (elisp-quality-ai-report--markdown-tasks tasks)
     (elisp-quality-ai-report--markdown-diagnostics files))))

(defun elisp-quality-ai-report--markdown-file (report)
  "Return file REPORT encoded as Markdown."
  (let ((summary (elisp-quality-ai-report--get "summary" report)))
    (concat
     "# elisp-quality-ai file report\n\n"
     (format "- File: `%s`\n" (elisp-quality-ai-report--get "file" report))
     (format "- Definitions: %s\n"
             (elisp-quality-ai-report--get "definitions" summary))
     (format "- Diagnostics: %s\n\n"
             (elisp-quality-ai-report--get "diagnostics" summary))
     (elisp-quality-ai-report--markdown-diagnostics (vector report)))))

(defun elisp-quality-ai-report--markdown (report)
  "Return REPORT encoded as Markdown."
  (cond
   ((elisp-quality-ai-report--project-report-p report)
    (elisp-quality-ai-report--markdown-project report))
   ((elisp-quality-ai-report--file-report-p report)
    (elisp-quality-ai-report--markdown-file report))
   (t
    (error "Unsupported elisp-quality-ai report schema: %S"
           (elisp-quality-ai-report--schema report)))))

(defun elisp-quality-ai-report-to-string
    (report format &optional include-definitions include-definitions-supplied)
  "Return REPORT encoded using FORMAT.
FORMAT must be one of `json', `jsonl', or `markdown'.
When INCLUDE-DEFINITIONS is non-nil, include full definition inventories.
When INCLUDE-DEFINITIONS-SUPPLIED is nil, default to
`elisp-quality-ai-report-include-definitions'."
  (let ((include-definitions
         (if (or include-definitions-supplied include-definitions)
             include-definitions
           elisp-quality-ai-report-include-definitions)))
    (pcase format
      ('json (elisp-quality-ai-report--json
              (if include-definitions
                  report
                (elisp-quality-ai-report--compact report))))
      ('jsonl (elisp-quality-ai-report--jsonl report include-definitions))
      ('markdown (elisp-quality-ai-report--markdown report))
      (_ (error "Unsupported elisp-quality-ai report format: %S" format)))))

(provide 'elisp-quality-ai-report)
;;; elisp-quality-ai-report.el ends here

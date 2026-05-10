;;; elisp-quality-ai-task.el --- Task generation for elisp-quality-ai -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: elisp-quality-ai contributors
;; Maintainer: elisp-quality-ai contributors
;; Package-Requires: ((emacs "29.1"))
;; Keywords: lisp, tools, maint

;;; Commentary:

;; Convert normalized diagnostics into AI-oriented task objects.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst elisp-quality-ai-task-ranking-weights
  '(("severity" . (("error" . 100)
                   ("warning" . 60)
                   ("info" . 30)))
    ("category" . (("compile" . 40)
                   ("type" . 35)
                   ("complexity" . 30)
                   ("size" . 25)
                   ("test" . 25)
                   ("regexp" . 20)
                   ("package" . 15)
                   ("style" . 10)
                   ("documentation" . 0)
                   ("collector" . 5)))
    ("signals" . (("missing_docstring" . 5)
                  ("definition_length_overage" . 1)
                  ("definition_length_overage_cap" . 20)
                  ("complexity_overage" . 2)
                  ("complexity_overage_cap" . 30)
                  ("test_coverage_gap" . 1)
                  ("test_coverage_gap_cap" . 30)
                  ("git_churn" . 1)
                  ("git_churn_cap" . 25)
                  ("related_diagnostic" . 1))))
  "Task priority scoring weights.

The scorer combines severity, category, current size/docstring diagnostics, and
future numeric signals for complexity, test coverage, and git churn.")

(defcustom elisp-quality-ai-task-default-verify-commands nil
  "Commands to include in generated tasks as verification steps.
When nil, infer a small project-local default from files such as Makefile or
Eask.  Set this to a list of strings to override inference."
  :type '(choice (const :tag "Infer from project" nil)
                 (repeat string))
  :group 'elisp-quality-ai)

(defun elisp-quality-ai-task--get (key object)
  "Return KEY from alist OBJECT."
  (cdr (assoc key object)))

(defun elisp-quality-ai-task--weight (section key &optional default)
  "Return ranking weight KEY from SECTION, falling back to DEFAULT."
  (or (cdr (assoc key
                  (cdr (assoc section
                              elisp-quality-ai-task-ranking-weights))))
      default
      0))

(defun elisp-quality-ai-task--numeric-get (keys object)
  "Return the first numeric value for KEYS in OBJECT."
  (seq-some
   (lambda (key)
     (let ((value (elisp-quality-ai-task--get key object)))
       (and (numberp value) value)))
   keys))

(defun elisp-quality-ai-task--integer-get (key object &optional default)
  "Return KEY from OBJECT as an integer, falling back to DEFAULT."
  (let ((value (elisp-quality-ai-task--get key object)))
    (cond
     ((integerp value) value)
     ((numberp value) (floor value))
     ((and (stringp value)
           (string-match-p "\\`[0-9]+\\'" value))
      (string-to-number value))
     (t default))))

(defun elisp-quality-ai-task--metric-match-p (diagnostic patterns)
  "Return non-nil when DIAGNOSTIC metric matches one of PATTERNS."
  (let ((metric (elisp-quality-ai-task--get "metric" diagnostic)))
    (and (stringp metric)
         (seq-some
          (lambda (pattern)
            (string-match-p pattern metric))
          patterns))))

(defun elisp-quality-ai-task--bounded-signal-score
    (amount weight-key cap-key)
  "Return score contribution for AMOUNT using WEIGHT-KEY and CAP-KEY."
  (if (and (numberp amount) (> amount 0))
      (round
       (min (elisp-quality-ai-task--weight "signals" cap-key)
            (* amount
               (elisp-quality-ai-task--weight "signals" weight-key))))
    0))

(defun elisp-quality-ai-task--diagnostics (project-report)
  "Return all diagnostics from PROJECT-REPORT as a list."
  (let (diagnostics)
    (dolist (file-report (append (elisp-quality-ai-task--get "files" project-report) nil))
      (dolist (diagnostic (append (elisp-quality-ai-task--get "diagnostics" file-report) nil))
        (push diagnostic diagnostics)))
    (nreverse diagnostics)))

(defun elisp-quality-ai-task--diagnostic-key (diagnostic)
  "Return a grouping key for DIAGNOSTIC."
  (let ((file (or (elisp-quality-ai-task--get "file" diagnostic) ""))
        (symbol (elisp-quality-ai-task--get "symbol" diagnostic))
        (line (elisp-quality-ai-task--integer-get "line" diagnostic 0)))
    (if symbol
        (format "%s\0%s" file symbol)
      (format "%s\0%s" file line))))

(defun elisp-quality-ai-task--diagnostic-overage (diagnostic)
  "Return how far DIAGNOSTIC value is over its threshold."
  (let ((value (elisp-quality-ai-task--get "value" diagnostic))
        (threshold (elisp-quality-ai-task--get "threshold" diagnostic)))
    (and (numberp value) (numberp threshold)
         (> value threshold)
         (- value threshold))))

(defun elisp-quality-ai-task--missing-docstring-p (category metric)
  "Return non-nil when CATEGORY and METRIC describe a missing docstring."
  (and (equal category "documentation")
       (or (not metric) (equal metric "docstring"))))

(defun elisp-quality-ai-task--definition-size-score
    (category metric overage)
  "Return size score for CATEGORY, METRIC, and OVERAGE."
  (if (or (equal category "size")
          (equal metric "definition-lines"))
      (elisp-quality-ai-task--bounded-signal-score
       overage "definition_length_overage"
       "definition_length_overage_cap")
    0))

(defun elisp-quality-ai-task--complexity-score
    (diagnostic category overage)
  "Return complexity score for DIAGNOSTIC, CATEGORY, and OVERAGE."
  (if (or (equal category "complexity")
          (elisp-quality-ai-task--metric-match-p
           diagnostic '("complexity")))
      (elisp-quality-ai-task--bounded-signal-score
       overage "complexity_overage"
       "complexity_overage_cap")
    0))

(defun elisp-quality-ai-task--coverage-score
    (diagnostic category)
  "Return test coverage score for DIAGNOSTIC and CATEGORY."
  (let ((coverage (elisp-quality-ai-task--numeric-get
                   '("coverage" "test_coverage" "test-coverage")
                   diagnostic))
        (threshold
         (elisp-quality-ai-task--numeric-get
          '("coverage_threshold" "coverage-threshold" "threshold")
          diagnostic)))
    (if (and (or (equal category "test")
                 (elisp-quality-ai-task--metric-match-p
                  diagnostic '("coverage" "test")))
             (numberp coverage)
             (numberp threshold)
             (< coverage threshold))
        (elisp-quality-ai-task--bounded-signal-score
         (- threshold coverage)
         "test_coverage_gap" "test_coverage_gap_cap")
      0)))

(defun elisp-quality-ai-task--git-churn-score (diagnostic)
  "Return git churn score for DIAGNOSTIC."
  (let ((churn (elisp-quality-ai-task--numeric-get
                '("git_churn" "git-churn" "churn")
                diagnostic))
        (value (elisp-quality-ai-task--get "value" diagnostic)))
    (if (or churn
            (elisp-quality-ai-task--metric-match-p
             diagnostic '("git.*churn" "churn")))
        (elisp-quality-ai-task--bounded-signal-score
         (or churn value) "git_churn" "git_churn_cap")
      0)))

(defun elisp-quality-ai-task--diagnostic-score (diagnostic)
  "Return a priority contribution for DIAGNOSTIC."
  (let* ((severity (elisp-quality-ai-task--get "severity" diagnostic))
         (category (elisp-quality-ai-task--get "category" diagnostic))
         (metric (elisp-quality-ai-task--get "metric" diagnostic))
         (base (elisp-quality-ai-task--weight "severity" severity))
         (category-score (elisp-quality-ai-task--weight "category" category))
         (overage (elisp-quality-ai-task--diagnostic-overage diagnostic))
         (missing-docstring
          (elisp-quality-ai-task--missing-docstring-p category metric)))
    (+ base
       category-score
       (if missing-docstring
           (elisp-quality-ai-task--weight "signals" "missing_docstring")
         0)
       (elisp-quality-ai-task--definition-size-score
        category metric overage)
       (elisp-quality-ai-task--complexity-score
        diagnostic category overage)
       (elisp-quality-ai-task--coverage-score diagnostic category)
       (elisp-quality-ai-task--git-churn-score diagnostic))))

(defun elisp-quality-ai-task--group-score (diagnostics)
  "Return the priority score for grouped DIAGNOSTICS."
  (+ (apply #'max 0 (mapcar #'elisp-quality-ai-task--diagnostic-score diagnostics))
     (* (elisp-quality-ai-task--weight "signals" "related_diagnostic")
        (max 0 (1- (length diagnostics))))))

(defun elisp-quality-ai-task--unique-strings (strings)
  "Return STRINGS with duplicates removed, preserving first occurrence."
  (let (seen result)
    (dolist (string strings)
      (when (and string (not (member string seen)))
        (push string seen)
        (push string result)))
    (nreverse result)))

(defun elisp-quality-ai-task--categories (diagnostics)
  "Return unique category strings from DIAGNOSTICS."
  (elisp-quality-ai-task--unique-strings
   (mapcar (lambda (diagnostic)
             (elisp-quality-ai-task--get "category" diagnostic))
           diagnostics)))

(defconst elisp-quality-ai-task--title-rules
  '((("compile") "Fix compile issues for %s" "Fix compile issues")
    (("type") "Fix static analysis issues for %s" "Fix static analysis issues")
    (("complexity" "size")
     "Refactor %s to improve maintainability"
     "Refactor code to improve maintainability")
    (("test") "Improve test coverage for %s" "Improve test coverage")
    (("regexp") "Fix regexp issues for %s" "Fix regexp issues")
    (("package") nil "Fix package metadata issues")
    (("documentation") "Document %s" "Document quality findings"))
  "Rules for mapping diagnostic categories to task titles.")

(defun elisp-quality-ai-task--title-rule (categories)
  "Return the first title rule matching CATEGORIES."
  (seq-find
   (lambda (rule)
     (seq-some (lambda (category)
                 (member category categories))
               (car rule)))
   elisp-quality-ai-task--title-rules))

(defun elisp-quality-ai-task--format-title-rule (rule subject)
  "Return title for RULE and SUBJECT."
  (let ((subject-format (nth 1 rule))
        (fallback-title (nth 2 rule)))
    (if (and subject subject-format)
        (format subject-format subject)
      fallback-title)))

(defun elisp-quality-ai-task--title (symbol categories)
  "Return a task title for SYMBOL and CATEGORIES."
  (let ((subject (and symbol (format "`%s`" symbol)))
        (rule (elisp-quality-ai-task--title-rule categories)))
    (if rule
        (elisp-quality-ai-task--format-title-rule rule subject)
      (elisp-quality-ai-task--format-title-rule
       '(nil "Address quality findings for %s" "Address quality findings")
       subject))))

(defun elisp-quality-ai-task--constraints (symbol)
  "Return task constraints for SYMBOL."
  (vconcat
   (delq
    nil
    (list
     "Preserve public behavior unless a diagnostic explicitly requires a behavior change."
     (and symbol (format "Keep `%s' compatible with existing callers." symbol))
     "Prefer the smallest cohesive change that resolves the diagnostics."
     "Add or update tests when behavior changes."))))

(defun elisp-quality-ai-task--suggested-steps (categories)
  "Return suggested action list for CATEGORIES."
  (vconcat
   (elisp-quality-ai-task--unique-strings
    (append
     (when (or (member "complexity" categories) (member "size" categories))
       '("Identify cohesive sub-operations inside the definition."
         "Extract helper functions without changing the public entry point."))
     (when (member "documentation" categories)
       '("Add a concise docstring describing behavior, arguments, and return value."))
     (when (member "compile" categories)
       '("Resolve byte compiler warnings before changing style-only code."))
     (when (member "regexp" categories)
       '("Rewrite the regexp using clearer `rx' form when that improves maintainability."))
     (when (member "package" categories)
       '("Update package metadata while keeping package headers parseable."))
     (when (member "test" categories)
       '("Add focused regression tests for the affected behavior."))
     '("Run the relevant test and lint commands after editing.")))))

(defun elisp-quality-ai-task--reason (diagnostics)
  "Return reason strings for DIAGNOSTICS."
  (vconcat
   (elisp-quality-ai-task--unique-strings
    (mapcar (lambda (diagnostic)
              (elisp-quality-ai-task--get "message" diagnostic))
            diagnostics))))

(defun elisp-quality-ai-task--relative-file (file root)
  "Return FILE relative to ROOT when possible."
  (if (and file root)
      (file-relative-name file root)
    file))

(defun elisp-quality-ai-task--stable-id (file root symbol line categories)
  "Return a stable task id for FILE under ROOT, SYMBOL, LINE, and CATEGORIES."
  (let ((subject (or symbol (format "line-%s" line)))
        (category (or (car categories) "finding"))
        (relative-file (or (elisp-quality-ai-task--relative-file file root)
                           "unknown-file")))
    (format "task:%s:%s:%s" relative-file subject category)))

(defun elisp-quality-ai-task--verify-commands (root)
  "Return project verification commands for ROOT as a vector."
  (vconcat
   (or elisp-quality-ai-task-default-verify-commands
       (let ((root (and root (file-name-as-directory root))))
         (cond
          ((and root (file-exists-p (expand-file-name "Makefile" root)))
           '("make test" "make compile"))
          ((and root (file-exists-p (expand-file-name "Eask" root)))
           '("eask run script test"))
          (t nil))))))

(defun elisp-quality-ai-task--task< (left right)
  "Return non-nil when task LEFT should sort before task RIGHT."
  (let ((left-priority (elisp-quality-ai-task--get "priority" left))
        (right-priority (elisp-quality-ai-task--get "priority" right))
        (left-file (or (elisp-quality-ai-task--get "file" left) ""))
        (right-file (or (elisp-quality-ai-task--get "file" right) ""))
        (left-line (elisp-quality-ai-task--integer-get "line" left 0))
        (right-line (elisp-quality-ai-task--integer-get "line" right 0))
        (left-symbol (or (elisp-quality-ai-task--get "symbol" left) ""))
        (right-symbol (or (elisp-quality-ai-task--get "symbol" right) "")))
    (cond
     ((/= left-priority right-priority) (> left-priority right-priority))
     ((not (string= left-file right-file)) (string< left-file right-file))
     ((/= left-line right-line) (< left-line right-line))
     (t (string< left-symbol right-symbol)))))

(defun elisp-quality-ai-task--renumber (tasks)
  "Return TASKS with stable report-local ids assigned."
  (let ((index 0))
    (mapcar
     (lambda (task)
       (setq index (1+ index))
       (cons (cons "id" (format "task-%03d" index)) task))
     tasks)))

(defun elisp-quality-ai-task--build (diagnostics root)
  "Build one task from grouped DIAGNOSTICS under ROOT."
  (let* ((first (car diagnostics))
         (file (elisp-quality-ai-task--get "file" first))
         (symbol (elisp-quality-ai-task--get "symbol" first))
         (line (apply #'min (mapcar (lambda (diagnostic)
                                      (elisp-quality-ai-task--integer-get
                                       "line" diagnostic 0))
                                     diagnostics)))
         (categories (elisp-quality-ai-task--categories diagnostics)))
    `(("title" . ,(elisp-quality-ai-task--title symbol categories))
      ("stable_id" . ,(elisp-quality-ai-task--stable-id
                       file root symbol line categories))
      ("file" . ,file)
      ("symbol" . ,symbol)
      ("line" . ,line)
      ("priority" . ,(elisp-quality-ai-task--group-score diagnostics))
      ("reason" . ,(elisp-quality-ai-task--reason diagnostics))
      ("constraints" . ,(elisp-quality-ai-task--constraints symbol))
      ("suggested_steps" . ,(elisp-quality-ai-task--suggested-steps categories))
      ("verify_commands" . ,(elisp-quality-ai-task--verify-commands root))
      ("diagnostics" . ,(vconcat diagnostics)))))

(defun elisp-quality-ai-task-generate (project-report)
  "Return a vector of AI-oriented tasks for PROJECT-REPORT."
  (let ((groups (make-hash-table :test #'equal))
        (root (elisp-quality-ai-task--get "root" project-report)))
    (dolist (diagnostic (elisp-quality-ai-task--diagnostics project-report))
      (let ((key (elisp-quality-ai-task--diagnostic-key diagnostic)))
        (push diagnostic (gethash key groups))))
    (let (tasks)
      (maphash
       (lambda (_key diagnostics)
         (push (elisp-quality-ai-task--build (nreverse diagnostics) root)
               tasks))
       groups)
      (vconcat
       (elisp-quality-ai-task--renumber
        (sort tasks #'elisp-quality-ai-task--task<))))))

(provide 'elisp-quality-ai-task)
;;; elisp-quality-ai-task.el ends here

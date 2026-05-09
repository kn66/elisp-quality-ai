;;; elisp-quality-ai-test.el --- Tests for elisp-quality-ai -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for elisp-quality-ai.

;;; Code:

(require 'ert)
(require 'json)
(require 'seq)
(require 'elisp-quality-ai)

(defconst elisp-quality-ai-test-required-project-keys
  '("schema" "root" "summary" "files" "tasks" "collectors")
  "Required project report keys.")

(defconst elisp-quality-ai-test-required-project-summary-keys
  '("files" "definitions" "diagnostics" "tasks")
  "Required project summary keys.")

(defconst elisp-quality-ai-test-required-file-keys
  '("schema" "file" "summary" "definitions" "diagnostics" "collectors")
  "Required file report keys.")

(defconst elisp-quality-ai-test-required-file-summary-keys
  '("definitions" "diagnostics")
  "Required file summary keys.")

(defconst elisp-quality-ai-test-required-definition-keys
  '("name" "kind" "file" "line" "end_line" "public" "interactive"
    "docstring" "metrics")
  "Required definition keys.")

(defconst elisp-quality-ai-test-required-definition-metric-keys
  '("lines")
  "Required definition metric keys.")

(defconst elisp-quality-ai-test-required-diagnostic-keys
  '("source" "category" "severity" "file" "line" "message" "suggestion")
  "Required diagnostic keys.")

(defconst elisp-quality-ai-test-required-task-keys
  '("id" "stable_id" "title" "file" "symbol" "line" "priority" "reason"
    "constraints" "suggested_steps" "verify_commands" "diagnostics")
  "Required task keys.")

(defconst elisp-quality-ai-test-required-collector-keys
  '("name" "available" "requires" "enabled" "description")
  "Required collector metadata keys.")

(defconst elisp-quality-ai-test-required-jsonl-event-keys
  '("type" "schema")
  "Required JSONL event keys.")

(defconst elisp-quality-ai-test-required-top-task-event-keys
  '("type" "schema" "rank")
  "Required top task JSONL event keys.")

(defconst elisp-quality-ai-test-allowed-severities
  '("error" "warning" "info")
  "Allowed diagnostic severities documented in the schema.")

(defconst elisp-quality-ai-test-allowed-categories
  '("complexity" "size" "documentation" "compile" "style" "package"
    "regexp" "type" "test" "collector")
  "Allowed diagnostic categories documented in the schema.")

(defun elisp-quality-ai-test--get (key object)
  "Return KEY from alist OBJECT using string comparison."
  (cdr (or (assoc key object)
           (and (stringp key) (assoc (intern key) object)))))

(defun elisp-quality-ai-test--should-have-keys (object keys)
  "Assert that OBJECT has every key in KEYS."
  (dolist (key keys)
    (should (or (assoc key object)
                (and (stringp key) (assoc (intern key) object))))))

(defun elisp-quality-ai-test--jsonl-events (jsonl)
  "Parse JSONL into a list of alists."
  (mapcar
   (lambda (line)
     (json-parse-string line :object-type 'alist :array-type 'array))
   (split-string jsonl "\n" t)))

(defmacro elisp-quality-ai-test-with-temp-elisp (contents &rest body)
  "Write CONTENTS to a temporary Elisp file and run BODY.
BODY can use the variables `directory' and `file'."
  (declare (indent 1))
  `(let* ((directory (make-temp-file "elisp-quality-ai-" t))
          (file (expand-file-name "sample.el" directory)))
     (let ((elisp-quality-ai-disabled-collectors
            '("checkdoc" "byte-compile" "relint" "package-lint"
              "cognitive-complexity" "elsa")))
       (unwind-protect
           (progn
             (with-temp-file file
               (insert ,contents))
             ,@body)
         (when (file-directory-p directory)
           (delete-directory directory t))))))

(ert-deftest elisp-quality-ai-analyze-file-collects-definitions ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  \"Return X.\"\n  x)\n\n(defvar sample-value nil\n  \"Sample value.\")\n"
    (let* ((report (elisp-quality-ai-analyze-file file))
           (definitions (cdr (assoc "definitions" report))))
      (should (= 2 (length definitions)))
      (should (equal "sample-public"
                     (cdr (assoc "name" (aref definitions 0))))))))

(ert-deftest elisp-quality-ai-analyze-file-reports-missing-docstring ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  x)\n"
    (let* ((report (elisp-quality-ai-analyze-file file))
           (diagnostics (cdr (assoc "diagnostics" report))))
      (should (= 1 (length diagnostics)))
      (should (equal "documentation"
                     (cdr (assoc "category" (aref diagnostics 0))))))))

(ert-deftest elisp-quality-ai-analyze-file-ignores-malformed-definition-name ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun)\n\n(defun sample-public ()\n  \"Return non-nil.\"\n  t)\n"
    (let* ((report (elisp-quality-ai-analyze-file file))
           (definitions (cdr (assoc "definitions" report))))
      (should (= 1 (length definitions)))
      (should (equal "sample-public"
                     (elisp-quality-ai-test--get "name"
                                                 (aref definitions 0)))))))

(ert-deftest elisp-quality-ai-analyze-file-skips-bare-defvar-declaration ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defvar external-variable)\n\n(defvar sample-value nil)\n"
    (let* ((report (elisp-quality-ai-analyze-file file))
           (definitions (cdr (assoc "definitions" report)))
           (diagnostics (cdr (assoc "diagnostics" report)))
           (external-definition
            (seq-find
             (lambda (definition)
               (equal "external-variable"
                      (elisp-quality-ai-test--get "name" definition)))
             (append definitions nil)))
           (sample-definition
            (seq-find
             (lambda (definition)
               (equal "sample-value"
                      (elisp-quality-ai-test--get "name" definition)))
             (append definitions nil)))
           (diagnostic (aref diagnostics 0)))
      (should external-definition)
      (should sample-definition)
      (should (eq t (elisp-quality-ai-test--get "declaration"
                                                external-definition)))
      (should (eq :json-false
                  (elisp-quality-ai-test--get "declaration"
                                              sample-definition)))
      (should (= 1 (length diagnostics)))
      (should (equal "sample-value"
                     (elisp-quality-ai-test--get "symbol" diagnostic)))
      (should (equal "documentation"
                     (elisp-quality-ai-test--get "category" diagnostic))))))

(ert-deftest elisp-quality-ai-elisp-files-excludes-root-level-directories ()
  (let* ((directory (make-temp-file "elisp-quality-ai-" t))
         (git-directory (expand-file-name ".git" directory))
         (build-directory (expand-file-name "build" directory))
         (src-directory (expand-file-name "src" directory)))
    (unwind-protect
        (progn
          (dolist (subdirectory (list git-directory build-directory
                                      src-directory))
            (make-directory subdirectory))
          (with-temp-file (expand-file-name "hidden.el" git-directory)
            (insert "(defun hidden-git () nil)\n"))
          (with-temp-file (expand-file-name "hidden.el" build-directory)
            (insert "(defun hidden-build () nil)\n"))
          (with-temp-file (expand-file-name "visible.el" src-directory)
            (insert "(defun visible () nil)\n"))
          (should (equal '("src/visible.el")
                         (mapcar
                          (lambda (file)
                            (file-relative-name file directory))
                          (elisp-quality-ai-core-elisp-files directory)))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest elisp-quality-ai-analyze-file-reports-derived-mode-missing-docstring ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(define-derived-mode sample-mode fundamental-mode \"Sample\" nil)\n"
    (let* ((report (elisp-quality-ai-analyze-file file))
           (definitions (cdr (assoc "definitions" report)))
           (definition (aref definitions 0))
           (diagnostics (cdr (assoc "diagnostics" report))))
      (should (= 1 (length definitions)))
      (should (eq :json-false
                  (elisp-quality-ai-test--get "docstring" definition)))
      (should (= 1 (length diagnostics)))
      (should (equal "sample-mode"
                     (elisp-quality-ai-test--get "symbol"
                                                 (aref diagnostics 0)))))))

(ert-deftest elisp-quality-ai-analyze-file-handles-qualified-cl-defmethod ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(cl-defmethod sample-method :around ((x string))\n  \"Return X.\"\n  x)\n"
    (let* ((report (elisp-quality-ai-analyze-file file))
           (definitions (cdr (assoc "definitions" report)))
           (definition (aref definitions 0))
           (diagnostics (cdr (assoc "diagnostics" report))))
      (should (= 1 (length definitions)))
      (should (equal "sample-method"
                     (elisp-quality-ai-test--get "name" definition)))
      (should (eq t (elisp-quality-ai-test--get "docstring" definition)))
      (should (= 0 (length diagnostics))))))

(ert-deftest elisp-quality-ai-analyze-file-handles-extra-cl-defmethod ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(cl-defmethod sample-method :extra \"variant\" :around ((x string))\n  \"Return X.\"\n  x)\n"
    (let* ((report (elisp-quality-ai-analyze-file file))
           (definitions (cdr (assoc "definitions" report)))
           (definition (aref definitions 0))
           (diagnostics (cdr (assoc "diagnostics" report))))
      (should (= 1 (length definitions)))
      (should (equal "sample-method"
                     (elisp-quality-ai-test--get "name" definition)))
      (should (eq t (elisp-quality-ai-test--get "docstring" definition)))
      (should (= 0 (length diagnostics))))))

(ert-deftest elisp-quality-ai-analyze-file-collects-setf-definition-name ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(cl-defmethod (setf sample-value) (new-value object)\n  \"Set sample value.\"\n  new-value)\n"
    (let* ((report (elisp-quality-ai-analyze-file file))
           (definitions (cdr (assoc "definitions" report)))
           (definition (aref definitions 0))
           (diagnostics (cdr (assoc "diagnostics" report))))
      (should (= 1 (length definitions)))
      (should (equal "(setf sample-value)"
                     (elisp-quality-ai-test--get "name" definition)))
      (should (eq t (elisp-quality-ai-test--get "docstring" definition)))
      (should (= 0 (length diagnostics))))))

(ert-deftest elisp-quality-ai-analyze-file-applies-read-symbol-shorthands ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; read-symbol-shorthands: ((\"sample-\" . \"sample-long-prefix-\")); -*-\n\n(defun sample-public ()\n  \"Return non-nil.\"\n  t)\n"
    (let* ((report (elisp-quality-ai-analyze-file file))
           (definitions (cdr (assoc "definitions" report)))
           (definition (aref definitions 0)))
      (should (= 1 (length definitions)))
      (should (equal "sample-long-prefix-public"
                     (elisp-quality-ai-test--get "name" definition))))))

(ert-deftest elisp-quality-ai-analyze-file-reports-read-syntax-errors ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public ()\n  \"Return non-nil.\"\n  t)\n\n(defun sample-broken (\n"
    (let* ((report (elisp-quality-ai-analyze-file file))
           (definitions (cdr (assoc "definitions" report)))
           (diagnostics (cdr (assoc "diagnostics" report)))
           (diagnostic (aref diagnostics 0)))
      (should (= 1 (length definitions)))
      (should (= 1 (length diagnostics)))
      (should (equal "compile"
                     (elisp-quality-ai-test--get "category" diagnostic)))
      (should (equal "error"
                     (elisp-quality-ai-test--get "severity" diagnostic)))
      (should (= 7 (elisp-quality-ai-test--get "line" diagnostic)))
      (should (string-match-p
               "End of file"
               (elisp-quality-ai-test--get "message" diagnostic))))))

(ert-deftest elisp-quality-ai-analyze-file-reports-file-local-variable-errors ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; read-symbol-shorthands: ((\"sample-\" . ; -*-\n\n(defun sample-public ()\n  \"Return non-nil.\"\n  t)\n"
    (let* ((report (elisp-quality-ai-analyze-file file))
           (definitions (cdr (assoc "definitions" report)))
           (diagnostics (cdr (assoc "diagnostics" report)))
           (diagnostic
            (seq-find
             (lambda (candidate)
               (string-match-p
                "File-local variables"
                (elisp-quality-ai-test--get "message" candidate)))
             (append diagnostics nil))))
      (should (= 1 (length definitions)))
      (should diagnostic)
      (should (equal "compile"
                     (elisp-quality-ai-test--get "category" diagnostic)))
      (should (equal "error"
                     (elisp-quality-ai-test--get "severity" diagnostic)))
      (should (= 1 (elisp-quality-ai-test--get "line" diagnostic))))))

(ert-deftest elisp-quality-ai-report-encodes-json ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  \"Return X.\"\n  x)\n"
    (let ((json (elisp-quality-ai-report-to-string
                 (elisp-quality-ai-analyze-directory directory)
                 'json)))
      (should (string-match-p "\"schema\"" json))
      (should (string-match-p "\"summary\"" json))
      (should-not (string-match-p "\"sample-public\"" json)))))

(ert-deftest elisp-quality-ai-report-include-definitions-encodes-json-definitions ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  \"Return X.\"\n  x)\n"
    (let ((json (elisp-quality-ai-report-to-string
                 (elisp-quality-ai-analyze-directory directory)
                 'json
                 t)))
      (should (string-match-p "\"schema\"" json))
      (should (string-match-p "\"sample-public\"" json)))))

(ert-deftest elisp-quality-ai-file-report-jsonl-includes-file-events ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  x)\n"
    (let* ((report (elisp-quality-ai-analyze-file file))
           (jsonl (elisp-quality-ai-report-to-string report 'jsonl t))
           (events (elisp-quality-ai-test--jsonl-events jsonl))
           (definition-event
            (seq-find
             (lambda (event)
               (equal "definition"
                      (elisp-quality-ai-test--get "type" event)))
             events))
           (diagnostic-event
            (seq-find
             (lambda (event)
               (equal "diagnostic"
                      (elisp-quality-ai-test--get "type" event)))
             events)))
      (should definition-event)
      (should diagnostic-event)
      (should (equal "sample-public"
                     (elisp-quality-ai-test--get "symbol"
                                                 definition-event)))
      (should (equal "sample-public has no obvious docstring."
                     (elisp-quality-ai-test--get "message"
                                                 diagnostic-event))))))

(ert-deftest elisp-quality-ai-file-report-jsonl-omits-definitions-by-default ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  \"Return X.\"\n  x)\n"
    (let* ((report (elisp-quality-ai-analyze-file file))
           (jsonl (elisp-quality-ai-report-to-string report 'jsonl))
           (events (elisp-quality-ai-test--jsonl-events jsonl)))
      (should-not
       (seq-find
        (lambda (event)
          (equal "definition" (elisp-quality-ai-test--get "type" event)))
        events)))))

(ert-deftest elisp-quality-ai-file-report-markdown-includes-diagnostics ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  x)\n"
    (let ((markdown (elisp-quality-ai-report-to-string
                     (elisp-quality-ai-analyze-file file)
                     'markdown)))
      (should (string-match-p "# elisp-quality-ai file report" markdown))
      (should (string-match-p "- Definitions: 1" markdown))
      (should (string-match-p "sample-public has no obvious docstring"
                              markdown)))))

(ert-deftest elisp-quality-ai-schema-project-report-has-required-keys ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  x)\n"
    (let* ((report (elisp-quality-ai-analyze-directory directory))
           (summary (elisp-quality-ai-test--get "summary" report))
           (files (elisp-quality-ai-test--get "files" report))
           (tasks (elisp-quality-ai-test--get "tasks" report))
           (collectors (elisp-quality-ai-test--get "collectors" report))
           (file-report (aref files 0)))
      (elisp-quality-ai-test--should-have-keys
       report elisp-quality-ai-test-required-project-keys)
      (elisp-quality-ai-test--should-have-keys
       summary elisp-quality-ai-test-required-project-summary-keys)
      (elisp-quality-ai-test--should-have-keys
       file-report elisp-quality-ai-test-required-file-keys)
      (should (equal "elisp-quality-ai.project/v1"
                     (elisp-quality-ai-test--get "schema" report)))
      (should (vectorp files))
      (should (vectorp tasks))
      (should (vectorp collectors)))))

(ert-deftest elisp-quality-ai-schema-file-report-has-required-keys ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  x)\n"
    (let* ((report (elisp-quality-ai-analyze-file file))
           (summary (elisp-quality-ai-test--get "summary" report))
           (definitions (elisp-quality-ai-test--get "definitions" report))
           (diagnostics (elisp-quality-ai-test--get "diagnostics" report))
           (collectors (elisp-quality-ai-test--get "collectors" report)))
      (elisp-quality-ai-test--should-have-keys
       report elisp-quality-ai-test-required-file-keys)
      (elisp-quality-ai-test--should-have-keys
       summary elisp-quality-ai-test-required-file-summary-keys)
      (should (equal "elisp-quality-ai.file/v1"
                     (elisp-quality-ai-test--get "schema" report)))
      (should (vectorp definitions))
      (should (vectorp diagnostics))
      (should (vectorp collectors)))))

(ert-deftest elisp-quality-ai-collector-registration-exposes-metadata ()
  (let ((elisp-quality-ai-collector-registry nil)
        (elisp-quality-ai-disabled-collectors nil))
    (elisp-quality-ai-register-collector
     'fake #'ignore
     :requires '(checkdoc "package-lint")
     :description "Fake collector"
     :available (lambda () t))
    (let* ((metadata (elisp-quality-ai-collector-metadata))
           (collector (aref metadata 0))
           (requires (elisp-quality-ai-test--get "requires" collector)))
      (should (= 1 (length metadata)))
      (elisp-quality-ai-test--should-have-keys
       collector elisp-quality-ai-test-required-collector-keys)
      (should (equal "fake" (elisp-quality-ai-test--get "name" collector)))
      (should (eq t (elisp-quality-ai-test--get "available" collector)))
      (should (eq t (elisp-quality-ai-test--get "enabled" collector)))
      (should (equal "Fake collector"
                     (elisp-quality-ai-test--get "description" collector)))
      (should (vectorp requires))
      (should (equal '("checkdoc" "package-lint")
                     (append requires nil))))))

(ert-deftest elisp-quality-ai-collector-run-file-normalizes-diagnostics ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  \"Return X.\"\n  x)\n"
    (let ((elisp-quality-ai-collector-registry nil)
          (elisp-quality-ai-disabled-collectors nil))
      (elisp-quality-ai-register-collector
       "fake-style"
       (lambda (_file)
         '(((category . "style")
            (severity . "info")
            (line . 2)
            (message . "Use a clearer name.")
            (suggestion . "Rename the binding.")))))
      (let* ((diagnostics (elisp-quality-ai-run-collectors-for-file file))
             (diagnostic (aref diagnostics 0)))
        (should (= 1 (length diagnostics)))
        (should (equal "fake-style"
                       (elisp-quality-ai-test--get "source" diagnostic)))
        (should (equal "fake-style"
                       (elisp-quality-ai-test--get "collector" diagnostic)))
        (should (equal "style"
                       (elisp-quality-ai-test--get "category" diagnostic)))
        (should (equal file
                       (elisp-quality-ai-test--get "file" diagnostic)))))))

(ert-deftest elisp-quality-ai-collector-normalizes-string-location-fields ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public ()\n  \"Return non-nil.\"\n  t)\n"
    (let ((elisp-quality-ai-collector-registry nil)
          (elisp-quality-ai-disabled-collectors nil))
      (elisp-quality-ai-register-collector
       "fake-location"
       (lambda (_file)
         '(((category . "style")
            (severity . "warning")
            (line . "3")
            (column . "5")
            (end_line . "4")
            (end_column . "9")
            (message . "Location strings.")
            (suggestion . "Normalize locations.")))))
      (let* ((diagnostics (elisp-quality-ai-run-collectors-for-file file))
             (diagnostic (aref diagnostics 0)))
        (should (= 3 (elisp-quality-ai-test--get "line" diagnostic)))
        (should (= 5 (elisp-quality-ai-test--get "column" diagnostic)))
        (should (= 4 (elisp-quality-ai-test--get "end_line" diagnostic)))
        (should (= 9 (elisp-quality-ai-test--get "end_column" diagnostic)))))))

(ert-deftest elisp-quality-ai-task-generation-handles-string-line-diagnostics ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public ()\n  \"Return non-nil.\"\n  t)\n"
    (let ((elisp-quality-ai-collector-registry nil)
          (elisp-quality-ai-disabled-collectors nil))
      (elisp-quality-ai-register-collector
       "fake-location"
       (lambda (_file)
         '(((category . "style")
            (severity . "warning")
            (line . "3")
            (message . "Location string.")
            (suggestion . "Normalize locations.")))))
      (let* ((report (elisp-quality-ai-analyze-directory directory))
             (tasks (elisp-quality-ai-test--get "tasks" report))
             (task (aref tasks 0))
             (diagnostic (aref (elisp-quality-ai-test--get
                                "diagnostics" task)
                               0)))
        (should (= 1 (length tasks)))
        (should (= 3 (elisp-quality-ai-test--get "line" diagnostic)))
        (should (= 3 (elisp-quality-ai-test--get "line" task)))))))

(ert-deftest elisp-quality-ai-analyze-file-includes-collector-diagnostics ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  \"Return X.\"\n  x)\n"
    (let ((elisp-quality-ai-collector-registry nil)
          (elisp-quality-ai-disabled-collectors nil))
      (elisp-quality-ai-register-collector
       'fake-core
       (lambda (_file)
         '((("category" . "style")
            ("severity" . "warning")
            ("line" . 1)
            ("message" . "Collector finding.")
            ("suggestion" . "Review the finding.")))))
      (let* ((report (elisp-quality-ai-analyze-file file))
             (diagnostics (elisp-quality-ai-test--get "diagnostics" report))
             (collectors (elisp-quality-ai-test--get "collectors" report))
             (diagnostic (aref diagnostics 0)))
        (should (= 1 (length diagnostics)))
        (should (= 1 (length collectors)))
        (should (equal "fake-core"
                       (elisp-quality-ai-test--get "source" diagnostic)))
        (should (equal "style"
                       (elisp-quality-ai-test--get "category" diagnostic)))))))

(ert-deftest elisp-quality-ai-collector-failures-become-diagnostics ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  \"Return X.\"\n  x)\n"
    (let ((elisp-quality-ai-collector-registry nil)
          (elisp-quality-ai-disabled-collectors nil))
      (elisp-quality-ai-register-collector
       "broken"
       (lambda (_file)
         (error "Boom")))
      (let* ((diagnostics (elisp-quality-ai-run-collectors-for-file file))
             (diagnostic (aref diagnostics 0)))
        (should (= 1 (length diagnostics)))
        (should (equal "collector"
                       (elisp-quality-ai-test--get "category" diagnostic)))
        (should (equal "warning"
                       (elisp-quality-ai-test--get "severity" diagnostic)))
        (should (string-match-p
                 "Collector broken failed: Boom"
                 (elisp-quality-ai-test--get "message" diagnostic)))))))

(ert-deftest elisp-quality-ai-collector-failure-messages-are-truncated ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public ()\n  \"Return non-nil.\"\n  t)\n"
    (let ((elisp-quality-ai-collector-registry nil)
          (elisp-quality-ai-disabled-collectors nil)
          (elisp-quality-ai-collector-failure-message-max-length 12))
      (elisp-quality-ai-register-collector
       "broken"
       (lambda (_file)
        (error "%s" (make-string 100 ?x))))
      (let* ((diagnostics (elisp-quality-ai-run-collectors-for-file file))
             (diagnostic (aref diagnostics 0))
             (message (elisp-quality-ai-test--get "message" diagnostic)))
        (should (<= (length message) 40))
        (should (string-suffix-p "..." message))))))

(ert-deftest elisp-quality-ai-collector-run-directory-returns-file-results ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  \"Return X.\"\n  x)\n"
    (let ((elisp-quality-ai-collector-registry nil)
          (elisp-quality-ai-disabled-collectors nil))
      (elisp-quality-ai-register-collector
       "fake-directory"
       (lambda (_file)
         '((("category" . "style")
            ("severity" . "info")
            ("line" . 1)
            ("message" . "Directory collector finding.")
            ("suggestion" . "Review the finding.")))))
      (let* ((results (elisp-quality-ai-run-collectors-for-directory
                       directory (list file)))
             (result (aref results 0))
             (diagnostics (elisp-quality-ai-test--get "diagnostics" result)))
        (should (= 1 (length results)))
        (should (equal file (elisp-quality-ai-test--get "file" result)))
        (should (= 1 (length diagnostics)))))))

(ert-deftest elisp-quality-ai-collector-run-directory-excludes-scan-directories ()
  (let* ((directory (make-temp-file "elisp-quality-ai-" t))
         (git-directory (expand-file-name ".git" directory))
         (build-directory (expand-file-name "build" directory))
         (src-directory (expand-file-name "src" directory)))
    (unwind-protect
        (let ((elisp-quality-ai-collector-registry nil)
              (elisp-quality-ai-disabled-collectors nil))
          (dolist (subdirectory (list git-directory build-directory
                                      src-directory))
            (make-directory subdirectory))
          (with-temp-file (expand-file-name "hidden.el" git-directory)
            (insert "(defun hidden-git () nil)\n"))
          (with-temp-file (expand-file-name "hidden.el" build-directory)
            (insert "(defun hidden-build () nil)\n"))
          (with-temp-file (expand-file-name "visible.el" src-directory)
            (insert "(defun visible () nil)\n"))
          (elisp-quality-ai-register-collector "fake-directory" (lambda (_file) nil))
          (should (equal '("src/visible.el")
                         (mapcar
                          (lambda (result)
                            (file-relative-name
                             (elisp-quality-ai-test--get "file" result)
                             directory))
                          (append
                           (elisp-quality-ai-run-collectors-for-directory
                            directory)
                           nil)))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest elisp-quality-ai-checkdoc-collector-appears-in-file-report ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample package -*- lexical-binding: t; -*-\n\n;;; Commentary:\n;; Sample package.\n\n;;; Code:\n\n(defun sample-bad (x)\n  \"Return a value.\"\n  x)\n\n(provide 'sample)\n;;; sample.el ends here\n"
    (let* ((elisp-quality-ai-disabled-collectors nil)
           (report (elisp-quality-ai-analyze-file file))
           (diagnostics (append (elisp-quality-ai-test--get "diagnostics"
                                                            report)
                                nil))
           (diagnostic (seq-find
                        (lambda (candidate)
                          (equal "checkdoc"
                                 (elisp-quality-ai-test--get "source"
                                                             candidate)))
                        diagnostics)))
      (should diagnostic)
      (should (equal "checkdoc"
                     (elisp-quality-ai-test--get "collector" diagnostic)))
      (should (equal "style"
                     (elisp-quality-ai-test--get "category" diagnostic)))
      (should (equal "warning"
                     (elisp-quality-ai-test--get "severity" diagnostic)))
      (should (equal file (elisp-quality-ai-test--get "file" diagnostic)))
      (should (= 9 (elisp-quality-ai-test--get "line" diagnostic)))
      (should (string-match-p
               "Argument"
               (elisp-quality-ai-test--get "message" diagnostic))))))

(ert-deftest elisp-quality-ai-checkdoc-collector-can-be-disabled ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample package -*- lexical-binding: t; -*-\n\n;;; Commentary:\n;; Sample package.\n\n;;; Code:\n\n(defun sample-bad (x)\n  \"Return a value.\"\n  x)\n\n(provide 'sample)\n;;; sample.el ends here\n"
    (let* ((report (elisp-quality-ai-analyze-file file))
           (diagnostics (append (elisp-quality-ai-test--get "diagnostics"
                                                            report)
                                nil)))
      (should-not
       (seq-find
        (lambda (diagnostic)
          (equal "checkdoc" (elisp-quality-ai-test--get "source" diagnostic)))
        diagnostics)))))

(ert-deftest elisp-quality-ai-byte-compile-collector-reports-free-variable ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-free ()\n  \"Return an unknown value.\"\n  unknown-variable)\n"
    (let* ((elisp-quality-ai-disabled-collectors nil)
           (report (elisp-quality-ai-analyze-file file))
           (diagnostics (append (elisp-quality-ai-test--get "diagnostics"
                                                            report)
                                nil))
           (diagnostic (seq-find
                        (lambda (candidate)
                          (equal "byte-compile"
                                 (elisp-quality-ai-test--get "source"
                                                             candidate)))
                        diagnostics)))
      (should diagnostic)
      (should (equal "byte-compile"
                     (elisp-quality-ai-test--get "collector" diagnostic)))
      (should (equal "compile"
                     (elisp-quality-ai-test--get "category" diagnostic)))
      (should (equal "warning"
                     (elisp-quality-ai-test--get "severity" diagnostic)))
      (should (equal file (elisp-quality-ai-test--get "file" diagnostic)))
      (should (= 5 (elisp-quality-ai-test--get "line" diagnostic)))
      (should (= 3 (elisp-quality-ai-test--get "column" diagnostic)))
      (should (string-match-p
               "free variable"
               (elisp-quality-ai-test--get "message" diagnostic)))
      (should-not (file-exists-p (expand-file-name "sample.elc"
                                                   directory))))))

(ert-deftest elisp-quality-ai-byte-compile-collector-uses-load-path ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(require 'sample-helper)\n\n(defun sample-ok ()\n  \"Return non-nil.\"\n  t)\n"
    (let* ((support-directory (expand-file-name "support" directory))
           (helper-file (expand-file-name "sample-helper.el"
                                          support-directory)))
      (make-directory support-directory)
      (with-temp-file helper-file
        (insert ";;; sample-helper.el --- Helper -*- lexical-binding: t; -*-\n\n(provide 'sample-helper)\n"))
      (unwind-protect
          (let* ((missing-load-path-diagnostics
                  (let ((elisp-quality-ai-byte-compile-load-path nil))
                    (elisp-quality-ai-byte-compile-collect-file file)))
                 (configured-load-path-diagnostics
                  (let ((elisp-quality-ai-byte-compile-load-path
                         '("support")))
                    (elisp-quality-ai-byte-compile-collect-file file))))
            (should
             (seq-find
              (lambda (diagnostic)
                (and (equal "error"
                            (elisp-quality-ai-test--get "severity"
                                                        diagnostic))
                     (string-match-p
                      "Cannot open load file"
                      (elisp-quality-ai-test--get "message" diagnostic))))
              (append missing-load-path-diagnostics nil)))
            (should (= 0 (length configured-load-path-diagnostics)))
            (should-not (file-exists-p (expand-file-name "sample.elc"
                                                         directory))))
        (when (featurep 'sample-helper)
          (unload-feature 'sample-helper t))))))

(ert-deftest elisp-quality-ai-external-collectors-report-unavailable-status ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  \"Return X.\"\n  x)\n"
    (let ((elisp-quality-ai-collector-registry nil)
          (elisp-quality-ai-external-use-emacs-packages nil)
          (elisp-quality-ai-external-tool-command-alist
           '(("relint" . "elisp-quality-ai-missing-relint")
             ("package-lint" . "elisp-quality-ai-missing-package-lint")
             ("cognitive-complexity" . "elisp-quality-ai-missing-cognitive-complexity")
             ("elsa" . "elisp-quality-ai-missing-elsa"))))
      (elisp-quality-ai-external-register-collectors)
      (let ((metadata (append (elisp-quality-ai-collector-metadata) nil))
            (diagnostics (elisp-quality-ai-run-collectors-for-file file)))
        (should (= 4 (length metadata)))
        (should (= 0 (length diagnostics)))
        (dolist (name '("relint" "package-lint" "cognitive-complexity" "elsa"))
          (let ((collector (seq-find
                            (lambda (candidate)
                              (equal name
                                     (elisp-quality-ai-test--get "name"
                                                                 candidate)))
                            metadata)))
            (should collector)
            (elisp-quality-ai-test--should-have-keys
             collector elisp-quality-ai-test-required-collector-keys)
            (should (eq :json-false
                        (elisp-quality-ai-test--get "available"
                                                    collector)))))))))

(ert-deftest elisp-quality-ai-external-library-available-detects-loaded-api ()
  (let ((spec '(:name "relint"
                :category "regexp"
                :libraries ("elisp-quality-ai-missing-relint")
                :functions ((elisp-quality-ai-test-loaded-relint-file . file)))))
    (cl-letf (((symbol-function 'elisp-quality-ai-test-loaded-relint-file)
               (lambda (_file) nil)))
      (let ((elisp-quality-ai-external-use-emacs-packages t))
        (should (elisp-quality-ai-external--library-available-p spec))))))

(ert-deftest elisp-quality-ai-package-lint-runs-only-on-entry-files ()
  (let* ((directory (make-temp-file "elisp-quality-ai-" t))
         (package-directory (expand-file-name "sample" directory))
         (test-directory (expand-file-name "test" package-directory))
         (main-file (expand-file-name "sample.el" package-directory))
         (internal-file (expand-file-name "sample-core.el" package-directory))
         (test-file (expand-file-name "sample-test.el" test-directory)))
    (unwind-protect
        (progn
          (make-directory test-directory t)
          (with-temp-file main-file
            (insert ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n")
            (insert ";; Version: 0.1.0\n")
            (insert ";; Package-Requires: ((emacs \"29.1\"))\n")
            (insert ";; URL: https://example.com/sample\n"))
          (with-temp-file internal-file
            (insert ";;; sample-core.el --- Core -*- lexical-binding: t; -*-\n")
            (insert ";; Package-Requires: ((emacs \"29.1\"))\n"))
          (with-temp-file test-file
            (insert ";;; sample-test.el --- Tests -*- lexical-binding: t; -*-\n"))
          (should
           (elisp-quality-ai-external--package-lint-file-p main-file))
          (should-not
           (elisp-quality-ai-external--package-lint-file-p internal-file))
          (should-not
           (elisp-quality-ai-external--package-lint-file-p test-file)))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest elisp-quality-ai-elsa-collector-is-disabled-by-default ()
  (let ((elisp-quality-ai-collector-registry nil)
        (elisp-quality-ai-enable-elsa nil))
    (elisp-quality-ai-external-register-collectors)
    (let* ((metadata (append (elisp-quality-ai-collector-metadata) nil))
           (elsa (seq-find
                  (lambda (collector)
                    (equal "elsa"
                           (elisp-quality-ai-test--get "name" collector)))
                  metadata)))
      (should elsa)
      (should (eq :json-false
                  (elisp-quality-ai-test--get "enabled" elsa))))))

(ert-deftest elisp-quality-ai-relint-command-output-normalizes-diagnostic ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public ()\n  \"Return non-nil.\"\n  t)\n"
    (let* ((script (expand-file-name "fake-relint" directory))
           (elisp-quality-ai-collector-registry nil)
           (elisp-quality-ai-disabled-collectors nil)
           (elisp-quality-ai-external-use-emacs-packages nil)
           (elisp-quality-ai-external-tool-command-alist
            `(("relint" . ,script)
              ("package-lint" . "elisp-quality-ai-missing-package-lint")
              ("cognitive-complexity" . "elisp-quality-ai-missing-cognitive-complexity")
              ("elsa" . "elisp-quality-ai-missing-elsa"))))
      (with-temp-file script
        (insert "#!/bin/sh\n")
        (insert "printf '%s:3:5: warning: Prefer rx notation\\n' \"$1\"\n"))
      (set-file-modes script #o755)
      (elisp-quality-ai-external-register-collectors)
      (let* ((diagnostics (elisp-quality-ai-run-collectors-for-file file))
             (diagnostic (aref diagnostics 0)))
        (should (= 1 (length diagnostics)))
        (should (equal "relint"
                       (elisp-quality-ai-test--get "source" diagnostic)))
        (should (equal "relint"
                       (elisp-quality-ai-test--get "collector" diagnostic)))
        (should (equal "regexp"
                       (elisp-quality-ai-test--get "category" diagnostic)))
        (should (equal "warning"
                       (elisp-quality-ai-test--get "severity" diagnostic)))
        (should (equal file
                       (elisp-quality-ai-test--get "file" diagnostic)))
        (should (= 3 (elisp-quality-ai-test--get "line" diagnostic)))
        (should (= 5 (elisp-quality-ai-test--get "column" diagnostic)))
        (should (equal "Prefer rx notation"
                       (elisp-quality-ai-test--get "message" diagnostic)))))))

(ert-deftest elisp-quality-ai-cognitive-complexity-result-normalizes-diagnostic ()
  (let* ((spec '(:name "cognitive-complexity"
                 :category "complexity"))
         (file "/tmp/sample.el")
         (elisp-quality-ai-cognitive-complexity-threshold 10)
         (diagnostics
          (elisp-quality-ai-external--normalize-library-result
           spec file '(12)))
         (diagnostic (car diagnostics)))
    (should (= 1 (length diagnostics)))
    (should (equal "cognitive-complexity"
                   (elisp-quality-ai-test--get "source" diagnostic)))
    (should (equal "complexity"
                   (elisp-quality-ai-test--get "category" diagnostic)))
    (should (equal "warning"
                   (elisp-quality-ai-test--get "severity" diagnostic)))
      (should (equal "cognitive-complexity"
                   (elisp-quality-ai-test--get "metric" diagnostic)))
    (should (= 12 (elisp-quality-ai-test--get "value" diagnostic)))
    (should (= 10 (elisp-quality-ai-test--get "threshold" diagnostic)))))

(ert-deftest elisp-quality-ai-external-dotted-status-result-is-ignored ()
  (let ((spec '(:name "relint"
                :category "regexp"))
        (file "/tmp/sample.el"))
    (should-not
     (elisp-quality-ai-external--normalize-library-result spec file '(0 . 0)))))

(ert-deftest elisp-quality-ai-elsa-normalizer-filters-common-noise ()
  (let* ((spec '(:name "elsa"
                 :category "type"))
         (file "/tmp/sample.el")
         (diagnostics
          (elisp-quality-ai-external--normalize-library-result
           spec file
           (mapconcat
            #'identity
            '("1: warning: Condition always evaluates to nil."
              "2: warning: Unreachable expression \"(setq --cl-var-- nil)\""
              "3: warning: Function `pcase-let' is missing arglist definition.  Maybe it is called before being declared?"
              "4: warning: Argument 1 accepts type `number' but received `frame-configuration'"
              "5: warning: Reference to free variable `real-problem'.")
            "\n"))))
    (should (= 1 (length diagnostics)))
    (should (equal "Reference to free variable `real-problem'."
                   (elisp-quality-ai-test--get "message"
                                               (car diagnostics))))))

(ert-deftest elisp-quality-ai-elsa-normalizer-filters-declared-free-variable ()
  (let* ((directory (make-temp-file "elisp-quality-ai-" t))
         (file (expand-file-name "sample.el" directory))
         (spec '(:name "elsa"
                 :category "type")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n")
            (insert "(declare-function external-call \"external\")\n")
            (insert "(defface sample-face '((t nil)) \"Sample face.\")\n"))
          (let ((diagnostics
                 (elisp-quality-ai-external--normalize-library-result
                  spec file
                  (mapconcat
                   #'identity
                   '("1: warning: Reference to free variable `external-call'."
                     "2: warning: Reference to free variable `sample-face'."
                     "3: warning: Reference to free variable `real-problem'.")
                   "\n"))))
            (should (= 1 (length diagnostics)))
            (should (equal "Reference to free variable `real-problem'."
                           (elisp-quality-ai-test--get "message"
                                                       (car diagnostics))))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest elisp-quality-ai-cognitive-complexity-current-buffer-creates-parser ()
  (skip-unless (and (fboundp 'treesit-available-p)
                    (treesit-available-p)
                    (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'elisp)))
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public ()\n  \"Return non-nil.\"\n  t)\n"
    (let ((result
           (elisp-quality-ai-external--call-function
            (lambda ()
              (not (null (treesit-parser-list))))
            'current-buffer
            file
            '(:name "cognitive-complexity"))))
      (should result))))

(ert-deftest elisp-quality-ai-cognitive-complexity-current-buffer-is-per-definition ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-simple ()\n  \"Return non-nil.\"\n  t)\n\n(defun sample-complex ()\n  \"Return non-nil.\"\n  (when t t))\n"
    (let ((elisp-quality-ai-cognitive-complexity-threshold 3)
          diagnostics)
      (cl-letf (((symbol-function 'cognitive-complexity-analyze)
                 (lambda (&rest _args)
                   '(4))))
        (setq diagnostics
              (elisp-quality-ai-external--call-function
               'cognitive-complexity-buffer
               'current-buffer
               file
               '(:name "cognitive-complexity"
                 :category "complexity")))
        (should (= 2 (length diagnostics)))
        (should (equal "sample-simple"
                       (elisp-quality-ai-test--get "symbol" (car diagnostics))))
        (should (equal "sample-complex"
                       (elisp-quality-ai-test--get "symbol" (cadr diagnostics))))
        (should (= 3 (elisp-quality-ai-test--get "line" (car diagnostics))))
        (should (= 7 (elisp-quality-ai-test--get "line" (cadr diagnostics))))))))

(ert-deftest elisp-quality-ai-schema-definition-has-required-keys ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  \"Return X.\"\n  x)\n"
    (let* ((report (elisp-quality-ai-analyze-file file))
           (definitions (elisp-quality-ai-test--get "definitions" report))
           (definition (aref definitions 0))
           (metrics (elisp-quality-ai-test--get "metrics" definition)))
      (elisp-quality-ai-test--should-have-keys
       definition elisp-quality-ai-test-required-definition-keys)
      (elisp-quality-ai-test--should-have-keys
       metrics elisp-quality-ai-test-required-definition-metric-keys)
      (should (integerp (elisp-quality-ai-test--get "line" definition)))
      (should (integerp (elisp-quality-ai-test--get "end_line" definition)))
      (should (integerp (elisp-quality-ai-test--get "lines" metrics))))))

(ert-deftest elisp-quality-ai-schema-diagnostic-has-required-keys ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  x)\n"
    (let* ((report (elisp-quality-ai-analyze-file file))
           (diagnostics (elisp-quality-ai-test--get "diagnostics" report))
           (diagnostic (aref diagnostics 0))
           (severity (elisp-quality-ai-test--get "severity" diagnostic))
           (category (elisp-quality-ai-test--get "category" diagnostic)))
      (elisp-quality-ai-test--should-have-keys
       diagnostic elisp-quality-ai-test-required-diagnostic-keys)
      (should (member severity elisp-quality-ai-test-allowed-severities))
      (should (member category elisp-quality-ai-test-allowed-categories))
      (should (integerp (elisp-quality-ai-test--get "line" diagnostic))))))

(ert-deftest elisp-quality-ai-task-generation-groups-diagnostics-by-symbol ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  (let ((value x))\n    (when value\n      value)))\n"
    (let* ((elisp-quality-ai-definition-length-threshold 3)
           (report (elisp-quality-ai-analyze-directory directory))
           (tasks (elisp-quality-ai-test--get "tasks" report))
           (task (aref tasks 0))
           (diagnostics (elisp-quality-ai-test--get "diagnostics" task)))
      (should (= 1 (length tasks)))
      (elisp-quality-ai-test--should-have-keys
       task elisp-quality-ai-test-required-task-keys)
      (should (equal "task-001" (elisp-quality-ai-test--get "id" task)))
      (should (string-match-p
               "sample\\.el:sample-public:size"
               (elisp-quality-ai-test--get "stable_id" task)))
      (should (equal "sample-public" (elisp-quality-ai-test--get "symbol" task)))
      (should (= 2 (length diagnostics)))
      (should (string-match-p "Refactor" (elisp-quality-ai-test--get "title" task))))))

(ert-deftest elisp-quality-ai-task-generation-includes-verify-commands ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  x)\n"
    (with-temp-file (expand-file-name "Makefile" directory)
      (insert "test:\n\t@true\ncompile:\n\t@true\n"))
    (let* ((report (elisp-quality-ai-analyze-directory directory))
           (tasks (elisp-quality-ai-test--get "tasks" report))
           (task (aref tasks 0))
           (commands (elisp-quality-ai-test--get "verify_commands" task)))
      (should (equal '("make test" "make compile")
                     (append commands nil))))))

(ert-deftest elisp-quality-ai-next-task-returns-highest-ranked-task ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  x)\n"
    (let ((task (elisp-quality-ai-next-task directory)))
      (should task)
      (should (equal "task-001" (elisp-quality-ai-test--get "id" task)))
      (should (equal "sample-public"
                     (elisp-quality-ai-test--get "symbol" task))))))

(ert-deftest elisp-quality-ai-cli-run-emits-limited-tasks-json ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  x)\n"
    (let ((output (expand-file-name "tasks.json" directory)))
      (should (= 0 (elisp-quality-ai-cli-run
                    (list "tasks" "--root" directory "--format" "json"
                          "--limit" "1" "--output" output "--fast"))))
      (let* ((json (with-temp-buffer
                     (insert-file-contents output)
                     (buffer-string)))
             (tasks (json-parse-string json :object-type 'alist
                                        :array-type 'array))
             (task (aref tasks 0)))
        (should (= 1 (length tasks)))
        (should (equal "task-001" (elisp-quality-ai-test--get "id" task)))
        (should (elisp-quality-ai-test--get "stable_id" task))))))

(ert-deftest elisp-quality-ai-cli-options-apply-load-path-and-load ()
  (let* ((directory (make-temp-file "elisp-quality-ai-" t))
         (support-directory (expand-file-name "support" directory))
         (load-file (expand-file-name "loaded.el" directory))
         (original-load-path load-path)
         (original-byte-compile-load-path
          elisp-quality-ai-byte-compile-load-path))
    (unwind-protect
        (progn
          (make-directory support-directory)
          (with-temp-file load-file
            (insert "(setq elisp-quality-ai-test-cli-loaded t)\n"))
          (when (boundp 'elisp-quality-ai-test-cli-loaded)
            (makunbound 'elisp-quality-ai-test-cli-loaded))
          (let* ((options
                  (elisp-quality-ai--cli-options
                   (list "report"
                         "--load-path" support-directory
                         "--load" load-file)))
                 (load-paths (cdr (assoc 'load-paths options)))
                 (load-files (cdr (assoc 'load-files options))))
            (should (equal (list support-directory) load-paths))
            (should (equal (list load-file) load-files))
            (let ((elisp-quality-ai-byte-compile-load-path nil))
              (elisp-quality-ai--cli-apply-load-options load-paths load-files)
              (should (member support-directory load-path))
              (should (equal (list support-directory)
                             elisp-quality-ai-byte-compile-load-path))
              (should (bound-and-true-p elisp-quality-ai-test-cli-loaded)))))
      (setq load-path original-load-path)
      (setq elisp-quality-ai-byte-compile-load-path
            original-byte-compile-load-path)
      (when (boundp 'elisp-quality-ai-test-cli-loaded)
        (makunbound 'elisp-quality-ai-test-cli-loaded))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest elisp-quality-ai-cli-report-omits-definitions-by-default ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  \"Return X.\"\n  x)\n"
    (let ((output (expand-file-name "report.json" directory)))
      (should (= 0 (elisp-quality-ai-cli-run
                    (list "report" "--root" directory "--format" "json"
                          "--output" output "--fast"))))
      (let* ((json (with-temp-buffer
                     (insert-file-contents output)
                     (buffer-string)))
             (report (json-parse-string json :object-type 'alist
                                         :array-type 'array))
             (files (elisp-quality-ai-test--get "files" report)))
        (dolist (file-report (append files nil))
          (should-not (assoc "definitions" file-report)))
        (should-not (string-match-p "\"sample-public\"" json))))))

(ert-deftest elisp-quality-ai-cli-report-includes-definitions-when-requested ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  \"Return X.\"\n  x)\n"
    (let ((output (expand-file-name "report-full.json" directory)))
      (should (= 0 (elisp-quality-ai-cli-run
                    (list "report" "--root" directory "--format" "json"
                          "--output" output "--fast"
                          "--include-definitions"))))
      (let ((json (with-temp-buffer
                    (insert-file-contents output)
                    (buffer-string))))
        (should (string-match-p "\"definitions\"" json))
        (should (string-match-p "\"sample-public\"" json))))))

(ert-deftest elisp-quality-ai-task-generation-ranks-higher-priority-first ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-small (x)\n  x)\n\n(defun sample-large (x)\n  \"Return X through several simple steps.\"\n  (let ((a x))\n    (let ((b a))\n      (let ((c b))\n        c))))\n"
    (let* ((elisp-quality-ai-definition-length-threshold 3)
           (report (elisp-quality-ai-analyze-directory directory))
           (tasks (elisp-quality-ai-test--get "tasks" report))
           (first-task (aref tasks 0))
           (second-task (aref tasks 1)))
      (should (= 2 (length tasks)))
      (should (equal "sample-large" (elisp-quality-ai-test--get "symbol" first-task)))
      (should (equal "sample-small" (elisp-quality-ai-test--get "symbol" second-task)))
      (should (> (elisp-quality-ai-test--get "priority" first-task)
                 (elisp-quality-ai-test--get "priority" second-task))))))

(ert-deftest elisp-quality-ai-task-ranking-uses-future-metric-signals ()
  (let* ((base-complexity
          '(("source" . "fake")
            ("category" . "complexity")
            ("severity" . "warning")
            ("file" . "/tmp/sample.el")
            ("line" . 1)
            ("message" . "Complexity finding.")
            ("suggestion" . "Refactor.")))
         (complexity-with-value
          (append base-complexity
                  '(("metric" . "cognitive-complexity")
                    ("value" . 20)
                    ("threshold" . 10))))
         (base-coverage
          '(("source" . "fake")
            ("category" . "test")
            ("severity" . "warning")
            ("file" . "/tmp/sample.el")
            ("line" . 2)
            ("message" . "Coverage finding.")
            ("suggestion" . "Add tests.")))
         (coverage-with-gap
          (append base-coverage
                  '(("metric" . "test-coverage")
                    ("coverage" . 50)
                    ("threshold" . 80))))
         (base-churn
          '(("source" . "fake")
            ("category" . "style")
            ("severity" . "warning")
            ("file" . "/tmp/sample.el")
            ("line" . 3)
            ("message" . "Churn finding.")
            ("suggestion" . "Review churn.")))
         (churn-with-value
          (append base-churn
                  '(("metric" . "git-churn")
                    ("value" . 12)))))
    (should (> (elisp-quality-ai-task--diagnostic-score
                complexity-with-value)
               (elisp-quality-ai-task--diagnostic-score base-complexity)))
    (should (> (elisp-quality-ai-task--diagnostic-score coverage-with-gap)
               (elisp-quality-ai-task--diagnostic-score base-coverage)))
    (should (> (elisp-quality-ai-task--diagnostic-score churn-with-value)
               (elisp-quality-ai-task--diagnostic-score base-churn)))))

(ert-deftest elisp-quality-ai-task-generation-sorts-ties-deterministically ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n"
    (let* ((diagnostics
            (vector
             `(("source" . "fake")
               ("category" . "documentation")
               ("severity" . "info")
               ("file" . ,file)
               ("symbol" . "sample-b")
               ("line" . 3)
               ("message" . "Document sample-b.")
               ("suggestion" . "Add docs."))
             `(("source" . "fake")
               ("category" . "documentation")
               ("severity" . "info")
               ("file" . ,file)
               ("symbol" . "sample-a")
               ("line" . 3)
               ("message" . "Document sample-a.")
               ("suggestion" . "Add docs."))))
           (report
            `(("schema" . "elisp-quality-ai.project/v1")
              ("root" . ,directory)
              ("files" . ,(vector
                            `(("schema" . "elisp-quality-ai.file/v1")
                              ("file" . ,file)
	                              ("summary" . (("definitions" . 0)
	                                            ("diagnostics" . 2)))
	                              ("definitions" . [])
	                              ("diagnostics" . ,diagnostics))))
	              ("tasks" . [])))
           (tasks (elisp-quality-ai-task-generate report)))
      (should (= 2 (length tasks)))
      (should (equal "sample-a"
                     (elisp-quality-ai-test--get "symbol" (aref tasks 0))))
      (should (equal "sample-b"
                     (elisp-quality-ai-test--get "symbol" (aref tasks 1))))
      (should (equal "task-001"
                     (elisp-quality-ai-test--get "id" (aref tasks 0))))
      (should (equal "task-002"
                     (elisp-quality-ai-test--get "id" (aref tasks 1)))))))

(ert-deftest elisp-quality-ai-task-title-omits-duplicate-generic-subject ()
  (should (equal "Address quality findings"
                 (elisp-quality-ai-task--title nil '("style"))))
  (should (equal "Document `sample-public`"
                 (elisp-quality-ai-task--title "sample-public"
                                               '("documentation")))))

(ert-deftest elisp-quality-ai-markdown-includes-task-section ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  x)\n"
    (let ((markdown (elisp-quality-ai-report-to-string
                     (elisp-quality-ai-analyze-directory directory)
                     'markdown)))
      (should (string-match-p "## Top Issues" markdown))
      (should (string-match-p "`task-001` Priority" markdown))
      (should (string-match-p "## Tasks" markdown))
      (should (string-match-p "Document `sample-public`" markdown)))))

(ert-deftest elisp-quality-ai-schema-jsonl-events-have-required-keys ()
  (elisp-quality-ai-test-with-temp-elisp
      ";;; sample.el --- Sample -*- lexical-binding: t; -*-\n\n(defun sample-public (x)\n  x)\n"
    (let* ((report (elisp-quality-ai-analyze-directory directory))
           (jsonl (elisp-quality-ai-report-to-string report 'jsonl t))
           (events (elisp-quality-ai-test--jsonl-events jsonl))
           (definition-event (seq-find
                              (lambda (event)
                                (equal "definition"
                                       (elisp-quality-ai-test--get "type" event)))
                              events))
           (diagnostic-event (seq-find
                              (lambda (event)
                                (equal "diagnostic"
                                       (elisp-quality-ai-test--get "type" event)))
                              events))
           (task-event (seq-find
                        (lambda (event)
                          (equal "task"
                                 (elisp-quality-ai-test--get "type" event)))
                        events))
           (top-task-event (seq-find
                            (lambda (event)
                              (equal "top_task"
                                     (elisp-quality-ai-test--get "type" event)))
                            events)))
      (should definition-event)
      (should diagnostic-event)
      (should top-task-event)
      (should task-event)
      (elisp-quality-ai-test--should-have-keys
       top-task-event elisp-quality-ai-test-required-task-keys)
      (elisp-quality-ai-test--should-have-keys
       top-task-event elisp-quality-ai-test-required-top-task-event-keys)
      (should (= 1 (elisp-quality-ai-test--get "rank" top-task-event)))
      (elisp-quality-ai-test--should-have-keys
       task-event elisp-quality-ai-test-required-task-keys)
      (dolist (event events)
        (elisp-quality-ai-test--should-have-keys
         event elisp-quality-ai-test-required-jsonl-event-keys)
        (should (equal "elisp-quality-ai.event/v1"
                       (elisp-quality-ai-test--get "schema" event)))))))

(provide 'elisp-quality-ai-test)
;;; elisp-quality-ai-test.el ends here

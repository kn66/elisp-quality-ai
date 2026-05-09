;;; elisp-quality-ai-external.el --- External collectors for elisp-quality-ai -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: elisp-quality-ai contributors
;; Maintainer: elisp-quality-ai contributors
;; Package-Requires: ((emacs "29.1"))
;; Keywords: lisp, tools, maint

;;; Commentary:

;; Optional collectors for external Emacs Lisp quality tools.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'lisp-mnt)
(require 'seq)
(require 'subr-x)
(require 'elisp-quality-ai-collector)

(defvar ansi-inhibit-ansi)
(defvar elsa-global-state)
(defvar warning-minimum-level)

(defcustom elisp-quality-ai-external-use-emacs-packages t
  "Whether external collectors may use installed Emacs package APIs.
When nil, external collectors only use configured or discoverable executables."
  :type 'boolean
  :group 'elisp-quality-ai)

(defcustom elisp-quality-ai-external-tool-command-alist nil
  "Alist mapping external collector names to executable paths.
Keys may be strings or symbols.  Values are command names or absolute paths."
  :type '(alist :key-type (choice string symbol)
                :value-type string)
  :group 'elisp-quality-ai)

(defcustom elisp-quality-ai-external-tool-arguments-alist nil
  "Alist mapping external collector names to argument lists.
Each argument is a string.  The substring `%f' is replaced with the source file."
  :type '(alist :key-type (choice string symbol)
                :value-type (repeat string))
  :group 'elisp-quality-ai)

(defcustom elisp-quality-ai-cognitive-complexity-threshold 8.0
  "Score above which `cognitive-complexity' emits a warning diagnostic."
  :type 'number
  :group 'elisp-quality-ai)

(defcustom elisp-quality-ai-enable-elsa nil
  "Whether the Elsa external collector runs by default.
Elsa is useful but currently has heavy process-global side effects in batch
sessions, so it is registered for metadata but disabled unless this option is
non-nil."
  :type 'boolean
  :group 'elisp-quality-ai)

(defcustom elisp-quality-ai-elsa-use-subprocess t
  "Whether the Elsa collector runs in an isolated Emacs subprocess.
Running Elsa out of process keeps its global state, cache setup, and noisy
stderr output away from the main report process."
  :type 'boolean
  :group 'elisp-quality-ai)

(defcustom elisp-quality-ai-elsa-subprocess-timeout 30
  "Seconds to wait for one Elsa subprocess before failing the collector."
  :type 'natnum
  :group 'elisp-quality-ai)

(defcustom elisp-quality-ai-elsa-ignored-message-regexps
  '("\\bframe-configuration\\b"
    "\\b--cl-[[:alnum:]-]+--\\b"
    "\\`Condition always evaluates to \\(?:nil\\|non-nil\\)\\."
    "\\`Unreachable expression "
    "\\`Function `[^']+' is missing arglist definition\\."
    "\\`Useless `progn' around body of then branch\\."
    "\\`Rewrite `if' as `when'"
    "\\`Domain of variable .* is empty\\'"
    " expects `[^']+'[,] got `mixed\\'"
    " received `[^']*mixed"
    "\\`Symbol's function definition is void: nil\\'")
  "Elsa diagnostic message regexps ignored as common analysis noise.
The defaults suppress diagnostics that usually come from macro expansion,
incomplete standard-library declarations, or stale Elsa type state rather than
from actionable project defects."
  :type '(repeat regexp)
  :group 'elisp-quality-ai)

(defconst elisp-quality-ai-external--collector-specs
  '((:name "relint"
     :category "regexp"
     :commands ("relint")
     :libraries ("relint")
     :functions ((relint-file . file)
                 (relint-current-buffer . current-buffer)
                 (relint-buffer . current-buffer))
     :buffers ("*Relint*" "*relint*")
     :description "Run relint regular expression checks.")
    (:name "package-lint"
     :category "package"
     :commands ("package-lint")
     :libraries ("package-lint")
     :functions ((package-lint-file . file)
                 (package-lint-current-buffer . current-buffer)
                 (package-lint-buffer . current-buffer))
     :buffers ("*Package-Lint*" "*package-lint*")
     :description "Run package-lint package metadata checks.")
    (:name "cognitive-complexity"
     :category "complexity"
     :commands ("cognitive-complexity")
     :libraries ("cognitive-complexity")
     :functions ((cognitive-complexity-file . file)
                 (cognitive-complexity-current-buffer . current-buffer)
                 (cognitive-complexity-buffer . current-buffer))
     :buffers ("*cognitive-complexity*" "*Cognitive Complexity*")
     :description "Run cognitive-complexity checks when installed.")
    (:name "elsa"
     :category "type"
     :commands ("elsa")
     :libraries ("elsa")
     :functions ((elsa-analyse-file . elsa-file)
                 (elsa-file . file)
                 (elsa-run-file . file)
                 (elsa-current-buffer . current-buffer))
     :buffers ("*Elsa*" "*elsa*")
     :description "Run Elsa static analysis when installed."))
  "External collector specifications.")

(defun elisp-quality-ai-external--get (property spec)
  "Return PROPERTY from SPEC."
  (plist-get spec property))

(defun elisp-quality-ai-external--name= (left right)
  "Return non-nil when collector names LEFT and RIGHT are equal."
  (string= (elisp-quality-ai-collector--string-value left)
           (elisp-quality-ai-collector--string-value right)))

(defun elisp-quality-ai-external--configured-value (name alist)
  "Return configured value for NAME in ALIST."
  (cdr
   (cl-find-if
    (lambda (entry)
      (elisp-quality-ai-external--name= name (car entry)))
    alist)))

(defun elisp-quality-ai-external--command (spec)
  "Return an executable command for SPEC, or nil."
  (let* ((name (elisp-quality-ai-external--get :name spec))
         (configured (elisp-quality-ai-external--configured-value
                      name elisp-quality-ai-external-tool-command-alist))
         (commands (if configured
                       (list configured)
                     (elisp-quality-ai-external--get :commands spec))))
    (seq-some #'executable-find commands)))

(defun elisp-quality-ai-external--libraries (spec)
  "Return libraries from SPEC."
  (elisp-quality-ai-external--get :libraries spec))

(defun elisp-quality-ai-external--cognitive-complexity-spec-p (spec)
  "Return non-nil when SPEC is the cognitive-complexity collector."
  (equal (elisp-quality-ai-external--get :name spec)
         "cognitive-complexity"))

(defun elisp-quality-ai-external--elsa-spec-p (spec)
  "Return non-nil when SPEC is the Elsa collector."
  (equal (elisp-quality-ai-external--get :name spec) "elsa"))

(defun elisp-quality-ai-external--package-lint-spec-p (spec)
  "Return non-nil when SPEC is the package-lint collector."
  (equal (elisp-quality-ai-external--get :name spec) "package-lint"))

(defun elisp-quality-ai-external--library-available-p (spec)
  "Return non-nil when an Emacs package for SPEC is available."
  (and elisp-quality-ai-external-use-emacs-packages
       (or
        (seq-some
         (lambda (library)
           (locate-library library))
         (elisp-quality-ai-external--libraries spec))
        (seq-some
         (lambda (entry)
           (fboundp (car entry)))
         (elisp-quality-ai-external--get :functions spec)))))

(defun elisp-quality-ai-external--available-p (spec)
  "Return non-nil when SPEC can be run."
  (or (elisp-quality-ai-external--command spec)
      (elisp-quality-ai-external--library-available-p spec)))

(defun elisp-quality-ai-external--library-header (file header)
  "Return HEADER from FILE using `lisp-mnt', or nil."
  (with-temp-buffer
    (insert-file-contents file)
    (lm-header header)))

(defun elisp-quality-ai-external--test-file-p (file)
  "Return non-nil when FILE appears to be test-only source."
  (string-match-p
   (rx (or "/test/" "/tests/" "-test.el" "-tests.el"))
   (expand-file-name file)))

(defun elisp-quality-ai-external--directory-main-file-p (file)
  "Return non-nil when FILE name matches its parent directory name."
  (let* ((file (expand-file-name file))
         (file-base (file-name-base file))
         (directory-base
          (file-name-nondirectory
           (directory-file-name (file-name-directory file)))))
    (string= file-base directory-base)))

(defun elisp-quality-ai-external--package-metadata-file-p (file)
  "Return non-nil when FILE has enough package headers for package-lint."
  (let ((version (or (elisp-quality-ai-external--library-header file "version")
                     (elisp-quality-ai-external--library-header
                      file "package-version")))
        (requires (elisp-quality-ai-external--library-header
                   file "package-requires"))
        (homepage (or (elisp-quality-ai-external--library-header file "url")
                      (elisp-quality-ai-external--library-header
                       file "homepage"))))
    (and version requires homepage)))

(defun elisp-quality-ai-external--package-lint-file-p (file)
  "Return non-nil when package-lint should inspect FILE.
Package-lint is designed for package entry files.  Running it over split
implementation files or tests creates many false package-prefix and metadata
diagnostics, so the collector limits itself to likely package entry points."
  (and (not (elisp-quality-ai-external--test-file-p file))
       (or (elisp-quality-ai-external--package-metadata-file-p file)
           (elisp-quality-ai-external--directory-main-file-p file))))

(defun elisp-quality-ai-external--collect-file-p (spec file)
  "Return non-nil when SPEC should collect diagnostics for FILE."
  (and
   (or (not (elisp-quality-ai-external--package-lint-spec-p spec))
       (elisp-quality-ai-external--package-lint-file-p file))
   (or (not (elisp-quality-ai-external--elsa-spec-p spec))
       (not (elisp-quality-ai-external--test-file-p file)))))

(defun elisp-quality-ai-external--enabled-p (spec)
  "Return non-nil when SPEC should run by default."
  (or (not (elisp-quality-ai-external--elsa-spec-p spec))
      elisp-quality-ai-enable-elsa))

(defun elisp-quality-ai-external--severity (label default)
  "Return normalized severity from LABEL, falling back to DEFAULT."
  (let ((label (and label (downcase label))))
    (cond
     ((not label) default)
     ((string-match-p "\\(?:error\\|fatal\\)" label) "error")
     ((string-match-p "\\(?:warn\\|warning\\)" label) "warning")
     ((string-match-p "\\(?:info\\|note\\)" label) "info")
     (t default))))

(defun elisp-quality-ai-external--strip-ansi (line)
  "Return LINE without simple ANSI color escapes."
  (replace-regexp-in-string "\33\\[[0-9;]*m" "" line))

(defun elisp-quality-ai-external--diagnostic
    (spec file line column severity message)
  "Return a normalized diagnostic.
SPEC, FILE, LINE, COLUMN, SEVERITY, and MESSAGE describe the finding."
  (let ((diagnostic
         `(("source" . ,(elisp-quality-ai-external--get :name spec))
           ("collector" . ,(elisp-quality-ai-external--get :name spec))
           ("category" . ,(elisp-quality-ai-external--get :category spec))
           ("severity" . ,severity)
           ("file" . ,(expand-file-name file))
           ("line" . ,(or line 1))
           ("message" . ,(string-trim message))
           ("suggestion" . ,(format "Review the %s finding and update the affected code."
                                     (elisp-quality-ai-external--get :name spec))))))
    (if column
        (append diagnostic `(("column" . ,column)))
      diagnostic)))

(defun elisp-quality-ai-external--collector-diagnostic
    (spec file severity message)
  "Return a collector diagnostic for SPEC, FILE, SEVERITY, and MESSAGE."
  `(("source" . ,(elisp-quality-ai-external--get :name spec))
    ("collector" . ,(elisp-quality-ai-external--get :name spec))
    ("category" . "collector")
    ("severity" . ,severity)
    ("file" . ,(expand-file-name file))
    ("line" . 1)
    ("message" . ,message)
    ("suggestion" . ,(format "Review the %s collector configuration."
                             (elisp-quality-ai-external--get :name spec)))))

(defun elisp-quality-ai-external--parse-line (spec file line)
  "Parse one external tool diagnostic LINE for SPEC and FILE."
  (let* ((line (string-trim (elisp-quality-ai-external--strip-ansi line)))
         (default-severity "warning")
         parsed)
    (cond
     ((string-match
       "\\`\\(.+\\.el\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?:[ \t]*\\(?:\\([[:alpha:]-]+\\):[ \t]*\\)?\\(.*\\)\\'"
       line)
      (setq parsed
            (list :line (string-to-number (match-string 2 line))
                  :column (and (match-string 3 line)
                               (string-to-number (match-string 3 line)))
                  :severity (elisp-quality-ai-external--severity
                             (match-string 4 line) default-severity)
                  :message (match-string 5 line))))
     ((string-match
       "\\`\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?:[ \t]*\\(?:\\([[:alpha:]-]+\\):[ \t]*\\)?\\(.*\\)\\'"
       line)
      (setq parsed
            (list :line (string-to-number (match-string 1 line))
                  :column (and (match-string 2 line)
                               (string-to-number (match-string 2 line)))
                  :severity (elisp-quality-ai-external--severity
                             (match-string 3 line) default-severity)
                  :message (match-string 4 line))))
     ((string-match "\\`\\([[:alpha:]-]+\\):[ \t]*\\(.+\\)\\'" line)
      (setq parsed
            (list :line 1
                  :severity (elisp-quality-ai-external--severity
                             (match-string 1 line) default-severity)
                  :message (match-string 2 line)))))
    (when (and parsed
               (not (string-empty-p (plist-get parsed :message))))
      (elisp-quality-ai-external--diagnostic
       spec file
       (plist-get parsed :line)
       (plist-get parsed :column)
       (plist-get parsed :severity)
       (plist-get parsed :message)))))

(defun elisp-quality-ai-external--parse-output (spec file output)
  "Parse external tool OUTPUT for SPEC and FILE."
  (delq
   nil
   (mapcar
    (lambda (line)
      (elisp-quality-ai-external--parse-line spec file line))
    (split-string output "\n" t))))

(defun elisp-quality-ai-external--arguments (spec file)
  "Return command arguments for SPEC and FILE."
  (let* ((name (elisp-quality-ai-external--get :name spec))
         (configured (elisp-quality-ai-external--configured-value
                      name elisp-quality-ai-external-tool-arguments-alist))
         (arguments (or configured '("%f"))))
    (mapcar
     (lambda (argument)
       (replace-regexp-in-string "%f" (expand-file-name file) argument t t))
     arguments)))

(defun elisp-quality-ai-external--command-output (command arguments)
  "Run COMMAND with ARGUMENTS and return a cons of status and output."
  (let ((error-file (make-temp-file "elisp-quality-ai-external-stderr-")))
    (unwind-protect
        (with-temp-buffer
          (let ((status (apply #'call-process
                               command nil (list (current-buffer) error-file)
                               nil arguments)))
            (when (file-exists-p error-file)
              (insert-file-contents error-file))
            (cons status (buffer-string))))
      (when (file-exists-p error-file)
        (delete-file error-file)))))

(defun elisp-quality-ai-external--run-command (spec file command)
  "Run external COMMAND for SPEC and FILE."
  (pcase-let* ((`(,status . ,output)
                (elisp-quality-ai-external--command-output
                 command (elisp-quality-ai-external--arguments spec file)))
               (diagnostics
                (elisp-quality-ai-external--parse-output spec file output)))
    (cond
     (diagnostics diagnostics)
     ((and (integerp status) (not (= 0 status)))
      (list
       (elisp-quality-ai-external--collector-diagnostic
        spec file "error"
        (format "%s exited with status %s%s"
                (elisp-quality-ai-external--get :name spec)
                status
                (if (string-empty-p (string-trim output))
                    "."
                  (format ": %s" (string-trim output)))))))
     (t nil))))

(defun elisp-quality-ai-external--alist-get-any (keys object)
  "Return the first value for KEYS in alist OBJECT."
  (seq-some
   (lambda (key)
     (cdr (or (assoc key object)
              (and (stringp key) (assoc (intern key) object))
              (and (symbolp key) (assoc (symbol-name key) object)))))
   keys))

(defun elisp-quality-ai-external--normalize-object (spec file object)
  "Return a diagnostic for arbitrary SPEC, FILE, and OBJECT."
  (cond
   ((stringp object)
    (elisp-quality-ai-external--parse-line spec file object))
   ((elisp-quality-ai-collector--diagnostic-p object)
    (let ((message
           (or (elisp-quality-ai-external--alist-get-any
                '("message" message "description" description "text" text)
                object)
               (format "%S" object)))
          (line (or (elisp-quality-ai-external--alist-get-any
                     '("line" line "line-number" line-number)
                     object)
                    1))
          (column (elisp-quality-ai-external--alist-get-any
                   '("column" column "col" col)
                   object))
          (severity
           (elisp-quality-ai-external--severity
            (elisp-quality-ai-collector--string-value
             (or (elisp-quality-ai-external--alist-get-any
                  '("severity" severity "level" level "type" type)
                  object)
                 "warning"))
            "warning")))
      (elisp-quality-ai-external--diagnostic
       spec file line column severity message)))
   (t
    (elisp-quality-ai-external--parse-line
     spec file (format "%S" object)))))

(defun elisp-quality-ai-external--cognitive-complexity-diagnostic
    (spec file score &optional line symbol)
  "Return a cognitive-complexity diagnostic for SPEC, FILE, and SCORE.
When LINE or SYMBOL are non-nil, attach them to the diagnostic."
  (let* ((threshold elisp-quality-ai-cognitive-complexity-threshold)
         (diagnostic
          (elisp-quality-ai-external--diagnostic
           spec file line nil "warning"
           (if symbol
               (format "%s cognitive complexity score is %s; threshold is %s."
                       symbol score threshold)
             (format "Cognitive complexity score is %s; threshold is %s."
                     score threshold)))))
    (append diagnostic
            `(("metric" . "cognitive-complexity")
              ("value" . ,score)
              ("threshold" . ,threshold))
            (when symbol
              `(("symbol" . ,symbol))))))

(defun elisp-quality-ai-external--normalize-cognitive-complexity-result
    (spec file result)
  "Normalize cognitive-complexity RESULT for SPEC and FILE."
  (cond
   ((and (listp result)
         (seq-every-p #'elisp-quality-ai-collector--diagnostic-p result))
    result)
   ((and (consp result)
         (numberp (car result))
         (> (car result) elisp-quality-ai-cognitive-complexity-threshold))
    (list
     (elisp-quality-ai-external--cognitive-complexity-diagnostic
      spec file (car result))))))

(defun elisp-quality-ai-external--cognitive-complexity-definition-name ()
  "Return the symbol name for the definition at point."
  (save-excursion
    (forward-char 1)
    (forward-sexp 1)
    (skip-chars-forward " \t\n")
    (when-let ((symbol (symbol-at-point)))
      (symbol-name symbol))))

(defun elisp-quality-ai-external--cognitive-complexity-definitions
    (spec file function)
  "Return complexity diagnostics for definitions in the current buffer.
SPEC and FILE identify the collector and file.  FUNCTION is the
`cognitive-complexity' analyzer to use on each definition."
  (let (diagnostics)
    (goto-char (point-min))
    (while (re-search-forward
            "^(def\\(?:un\\|macro\\|subst\\)\\_>"
            nil t)
      (goto-char (match-beginning 0))
      (let ((start (point))
            (line (line-number-at-pos))
            (symbol (elisp-quality-ai-external--cognitive-complexity-definition-name))
            end score)
        (forward-sexp 1)
        (setq end (point))
        (setq score
              (car
               (funcall function
                        (buffer-substring-no-properties start end)
                        major-mode)))
        (when (and (numberp score)
                   (> score elisp-quality-ai-cognitive-complexity-threshold))
          (push
           (elisp-quality-ai-external--cognitive-complexity-diagnostic
            spec file score line symbol)
           diagnostics))))
    (nreverse diagnostics)))

(defun elisp-quality-ai-external--typep (object type)
  "Return non-nil when OBJECT is of EIEIO TYPE."
  (ignore-errors
    (cl-typep object type)))

(defun elisp-quality-ai-external--slot-value (object slot)
  "Return OBJECT's SLOT value, or nil when unavailable."
  (ignore-errors
    (slot-value object slot)))

(defun elisp-quality-ai-external--elsa-message-severity (message)
  "Return normalized severity for Elsa MESSAGE."
  (cond
   ((and (fboundp 'elsa-error-p) (funcall 'elsa-error-p message)) "error")
   ((and (fboundp 'elsa-warning-p) (funcall 'elsa-warning-p message)) "warning")
   (t "info")))

(defun elisp-quality-ai-external--elsa-message-location (message)
  "Return line and column cons for Elsa MESSAGE."
  (let ((expression
         (elisp-quality-ai-external--slot-value message 'expression)))
    (cons
     (or (elisp-quality-ai-external--slot-value message 'line)
         (and expression
              (elisp-quality-ai-external--slot-value expression 'line))
         1)
     (or (elisp-quality-ai-external--slot-value message 'column)
         (and expression
              (elisp-quality-ai-external--slot-value expression 'column))))))

(defun elisp-quality-ai-external--elsa-message-diagnostic
    (spec file message)
  "Return a normalized diagnostic for SPEC, FILE, and Elsa MESSAGE."
  (pcase-let ((`(,line . ,column)
               (elisp-quality-ai-external--elsa-message-location message)))
    (elisp-quality-ai-external--diagnostic
     spec file line column
     (elisp-quality-ai-external--elsa-message-severity message)
     (or (elisp-quality-ai-external--slot-value message 'message)
         (and (fboundp 'elsa-message-format)
              (funcall 'elsa-message-format message))
         (format "%S" message)))))

(defun elisp-quality-ai-external--file-declares-symbol-p (file symbol)
  "Return non-nil when FILE declares SYMBOL."
  (when (and (stringp file)
             (file-readable-p file)
             (symbolp symbol))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (re-search-forward
       (concat
        "^[ \t]*(\\(?:declare-function\\|def\\(?:const\\|custom\\|face\\|macro\\|subst\\|un\\|var\\)\\)[ \t\n]+"
        (regexp-quote (symbol-name symbol))
        "\\(?:[ \t\n)]\\|\\'\\)")
       nil t))))

(defun elisp-quality-ai-external--declared-free-variable-diagnostic-p
    (diagnostic)
  "Return non-nil when DIAGNOSTIC reports a declared symbol as free."
  (let ((message (elisp-quality-ai-external--alist-get-any
                  '("message" message)
                  diagnostic))
        (file (elisp-quality-ai-external--alist-get-any
               '("file" file)
               diagnostic)))
    (and
     (stringp message)
     (string-match "\\`Reference to free variable `\\([^']+\\)'\\." message)
     (elisp-quality-ai-external--file-declares-symbol-p
      file (intern (match-string 1 message))))))

(defun elisp-quality-ai-external--ignored-elsa-diagnostic-p (diagnostic)
  "Return non-nil when DIAGNOSTIC is filtered Elsa noise."
  (let ((message (elisp-quality-ai-external--alist-get-any
                  '("message" message)
                  diagnostic))
        (line (elisp-quality-ai-external--alist-get-any
               '("line" line)
               diagnostic)))
    (and
     (stringp message)
     (or
      (and (equal line 1)
           (not (string-match-p "\\`Reference to free variable " message)))
      (elisp-quality-ai-external--declared-free-variable-diagnostic-p
       diagnostic)
      (seq-some
       (lambda (regexp)
         (string-match-p regexp message))
       elisp-quality-ai-elsa-ignored-message-regexps)))))

(defun elisp-quality-ai-external--filter-elsa-diagnostics (diagnostics)
  "Return DIAGNOSTICS without configured Elsa noise."
  (seq-remove #'elisp-quality-ai-external--ignored-elsa-diagnostic-p
              diagnostics))

(defun elisp-quality-ai-external--normalize-elsa-result (spec file result)
  "Normalize Elsa RESULT for SPEC and FILE."
  (elisp-quality-ai-external--filter-elsa-diagnostics
   (cond
    ((elisp-quality-ai-external--typep result 'elsa-state)
     (delq
      nil
      (mapcar
       (lambda (message)
         (when (elisp-quality-ai-external--typep message 'elsa-message)
           (elisp-quality-ai-external--elsa-message-diagnostic
            spec file message)))
       (elisp-quality-ai-external--slot-value result 'errors))))
    ((elisp-quality-ai-external--typep result 'elsa-message)
     (list (elisp-quality-ai-external--elsa-message-diagnostic
            spec file result)))
    ((stringp result)
     (elisp-quality-ai-external--parse-output spec file result))
    (t nil))))

(defun elisp-quality-ai-external--normalize-library-result
    (spec file result)
  "Normalize library RESULT for SPEC and FILE."
  (cond
   ((elisp-quality-ai-external--cognitive-complexity-spec-p spec)
    (or (elisp-quality-ai-external--normalize-cognitive-complexity-result
         spec file result)
        (and (stringp result)
             (elisp-quality-ai-external--parse-output spec file result))))
   ((elisp-quality-ai-external--elsa-spec-p spec)
    (elisp-quality-ai-external--normalize-elsa-result spec file result))
   ((null result) nil)
   ((stringp result)
    (elisp-quality-ai-external--parse-output spec file result))
   ((vectorp result)
    (delq
     nil
     (mapcar
      (lambda (object)
        (elisp-quality-ai-external--normalize-object spec file object))
      (append result nil))))
   ((elisp-quality-ai-collector--diagnostic-p result)
    (list (elisp-quality-ai-external--normalize-object spec file result)))
   ((and (consp result) (not (proper-list-p result)))
    (delq
     nil
     (list (elisp-quality-ai-external--normalize-object spec file result))))
   ((listp result)
    (delq
     nil
     (mapcar
      (lambda (object)
        (elisp-quality-ai-external--normalize-object spec file object))
      result)))
   (t nil)))

(defun elisp-quality-ai-external--treesit-language-for-file (file)
  "Return the tree-sitter language symbol for FILE, when known."
  (cond
   ((string-match-p "\\.el\\'" file) 'elisp)
   (t nil)))

(defun elisp-quality-ai-external--ensure-treesit-parser (spec file)
  "Create a tree-sitter parser for FILE when SPEC needs one."
  (when (and (elisp-quality-ai-external--cognitive-complexity-spec-p spec)
             (fboundp 'treesit-available-p)
             (treesit-available-p)
             (fboundp 'treesit-parser-list)
             (null (treesit-parser-list)))
    (when-let* ((language (elisp-quality-ai-external--treesit-language-for-file
                           file))
                ((fboundp 'treesit-language-available-p))
                ((treesit-language-available-p language))
                ((fboundp 'treesit-parser-create)))
      (treesit-parser-create language))))

(defun elisp-quality-ai-external--elsa-cache-file
    (cache-directory feature &optional compiled)
  "Return an isolated Elsa cache file under CACHE-DIRECTORY for FEATURE.
When COMPILED is non-nil, return the compiled cache file name."
  (expand-file-name
   (format "%s-elsa-cache.el%s"
           (if (stringp feature) feature (symbol-name feature))
           (if compiled "c" ""))
   cache-directory))

(defun elisp-quality-ai-external--call-function (function mode file &optional spec)
  "Call external tool FUNCTION in MODE for FILE and optional SPEC."
  (pcase mode
    ('file
     (funcall function file))
    ('elsa-file
     (let ((cache-directory (make-temp-file "elisp-quality-ai-elsa-" t)))
       (unwind-protect
           (let* ((default-directory
                    (file-name-directory (expand-file-name file)))
                  (enable-dir-local-variables nil)
                  (ansi-inhibit-ansi t)
                  (byte-compile-verbose nil)
                  (byte-compile-warnings nil)
                  (inhibit-message t)
                  (warning-minimum-level :emergency)
                  (global-state (if (fboundp 'elsa-global-state)
                                    (funcall 'elsa-global-state)
                                  (and (boundp 'elsa-global-state)
                                       (symbol-value 'elsa-global-state)))))
             (when (fboundp 'elsa-load-config)
               (funcall 'elsa-load-config))
             (require 'elsa-startup nil t)
             (let ((elsa-global-state global-state))
               (cl-letf (((symbol-function 'elsa--get-cache-file-name)
                          (lambda (_global-state feature &optional compiled)
                            (elisp-quality-ai-external--elsa-cache-file
                             cache-directory feature compiled))))
                 (if (fboundp 'elsa-log)
                     (cl-letf (((symbol-function 'elsa-log)
                                (lambda (&rest _args) nil)))
                       (funcall function file global-state))
                   (funcall function file global-state)))))
         (when (file-directory-p cache-directory)
           (delete-directory cache-directory t)))))
    ('current-buffer
     (with-temp-buffer
       (insert-file-contents file)
       (setq buffer-file-name file)
       (emacs-lisp-mode)
       (elisp-quality-ai-external--ensure-treesit-parser spec file)
       (if (and (elisp-quality-ai-external--cognitive-complexity-spec-p spec)
                (eq function 'cognitive-complexity-buffer)
                (fboundp 'cognitive-complexity-analyze))
           (elisp-quality-ai-external--cognitive-complexity-definitions
            spec file #'cognitive-complexity-analyze)
         (funcall function))))
    (_
     (error "Unsupported external collector function mode: %S" mode))))

(defun elisp-quality-ai-external--buffer-output (spec)
  "Return combined output from known buffers for SPEC."
  (mapconcat
   #'identity
   (delq
    nil
    (mapcar
     (lambda (buffer-name)
       (when-let ((buffer (get-buffer buffer-name)))
         (with-current-buffer buffer
           (buffer-string))))
     (elisp-quality-ai-external--get :buffers spec)))
   "\n"))

(defun elisp-quality-ai-external--clear-buffers (spec)
  "Clear known output buffers for SPEC."
  (dolist (buffer-name (elisp-quality-ai-external--get :buffers spec))
    (when-let ((buffer (get-buffer buffer-name)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer))))))

(defun elisp-quality-ai-external--package-directory ()
  "Return the directory containing `elisp-quality-ai-external'."
  (file-name-directory
   (or load-file-name
       (locate-library "elisp-quality-ai-external")
       (buffer-file-name)
       default-directory)))

(defun elisp-quality-ai-external--elsa-subprocess-form
    (spec file output-file load-path-value)
  "Return child Emacs form for Elsa SPEC, FILE, OUTPUT-FILE, and LOAD-PATH-VALUE."
  `(let ((load-path ',load-path-value)
         (warning-minimum-level :emergency)
         (inhibit-message t)
         result)
     (condition-case error-data
         (progn
           (require 'package)
           (package-initialize)
           (require 'elisp-quality-ai-external)
           (setq elisp-quality-ai-enable-elsa t)
           (setq elisp-quality-ai-elsa-use-subprocess nil)
           (setq elisp-quality-ai-elsa-ignored-message-regexps
                 ',elisp-quality-ai-elsa-ignored-message-regexps)
           (setq result
                 (elisp-quality-ai-external--run-library ',spec ,file))
           (with-temp-file ,output-file
             (let ((print-length nil)
                   (print-level nil))
               (prin1 (cons 'ok result) (current-buffer)))))
       (error
        (with-temp-file ,output-file
          (let ((print-length nil)
                (print-level nil))
            (prin1 (cons 'error (error-message-string error-data))
                   (current-buffer))))))))

(defun elisp-quality-ai-external--call-with-timeout
    (program arguments timeout stderr-file)
  "Run PROGRAM with ARGUMENTS, waiting up to TIMEOUT seconds.
Write stderr to STDERR-FILE and return the process exit status.  Signal an
error when the process times out."
  (let* ((stderr-buffer (generate-new-buffer " *elisp-quality-ai-stderr*"))
         (process
          (make-process
           :name "elisp-quality-ai-elsa"
           :buffer nil
           :command (cons program arguments)
           :stderr stderr-buffer
           :noquery t))
         (deadline (and (natnump timeout)
                        (> timeout 0)
                        (+ (float-time) timeout))))
    (unwind-protect
        (progn
          (while (and (process-live-p process)
                      (or (not deadline) (< (float-time) deadline)))
            (accept-process-output process 0.1))
          (when (process-live-p process)
            (delete-process process)
            (error "Elsa subprocess timed out after %s seconds" timeout))
          (with-current-buffer stderr-buffer
            (write-region (point-min) (point-max) stderr-file nil 'silent))
          (process-exit-status process))
      (when (buffer-live-p stderr-buffer)
        (kill-buffer stderr-buffer)))))

(defun elisp-quality-ai-external--read-file-string (file)
  "Return FILE contents as a string, or an empty string when FILE is absent."
  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string))
    ""))

(defun elisp-quality-ai-external--elsa-subprocess-status-error
    (status stderr)
  "Signal an Elsa subprocess error for STATUS and STDERR."
  (error "Elsa subprocess exited with status %s%s"
         status
         (if (string-empty-p stderr) "" (format ": %s" stderr))))

(defun elisp-quality-ai-external--read-elsa-subprocess-result
    (output-file stderr-file status)
  "Read Elsa subprocess OUTPUT-FILE and STDERR-FILE for process STATUS."
  (let ((stderr (string-trim
                 (elisp-quality-ai-external--read-file-string stderr-file))))
    (unless (file-exists-p output-file)
      (error "Elsa subprocess produced no result%s"
             (if (string-empty-p stderr) "" (format ": %s" stderr))))
    (with-temp-buffer
      (insert-file-contents output-file)
      (pcase (read (current-buffer))
        (`(ok . ,diagnostics)
         (if (and (integerp status) (not (= 0 status)) (null diagnostics))
             (elisp-quality-ai-external--elsa-subprocess-status-error
              status stderr)
           diagnostics))
        (`(error . ,message)
         (error "%s" message))
        (other
         (error "Elsa subprocess returned unsupported result: %S" other))))))

(defun elisp-quality-ai-external--run-elsa-subprocess (spec file)
  "Run Elsa SPEC for FILE in an isolated Emacs subprocess."
  (let* ((output-file (make-temp-file "elisp-quality-ai-elsa-result-"))
         (stderr-file (make-temp-file "elisp-quality-ai-elsa-stderr-"))
         (package-directory (elisp-quality-ai-external--package-directory))
         (load-path-value
          (delete-dups
           (mapcar #'expand-file-name
                   (cons package-directory load-path))))
         (form
          (elisp-quality-ai-external--elsa-subprocess-form
           spec (expand-file-name file) output-file load-path-value))
         (status nil))
    (unwind-protect
        (progn
          (setq status
                (elisp-quality-ai-external--call-with-timeout
                 invocation-name
                 (list "-Q" "--batch"
                       "-L" package-directory
                       "--eval" (prin1-to-string form))
                 elisp-quality-ai-elsa-subprocess-timeout
                 stderr-file))
          (elisp-quality-ai-external--read-elsa-subprocess-result
           output-file stderr-file status))
      (when (file-exists-p output-file)
        (delete-file output-file))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun elisp-quality-ai-external--run-library (spec file)
  "Run installed Emacs package implementation for SPEC and FILE."
  (dolist (library (elisp-quality-ai-external--libraries spec))
    (require (intern library) nil t))
  (let ((function-entry
         (seq-find
          (lambda (entry)
            (fboundp (car entry)))
          (elisp-quality-ai-external--get :functions spec))))
    (unless function-entry
      (error "No supported Emacs package API found for %s"
             (elisp-quality-ai-external--get :name spec)))
    (elisp-quality-ai-external--clear-buffers spec)
    (let* ((result (elisp-quality-ai-external--call-function
                    (car function-entry) (cdr function-entry)
                    (expand-file-name file) spec))
           (diagnostics
            (elisp-quality-ai-external--normalize-library-result
             spec file result))
           (buffer-diagnostics
            (elisp-quality-ai-external--parse-output
             spec file (elisp-quality-ai-external--buffer-output spec))))
      (append diagnostics buffer-diagnostics))))

(defun elisp-quality-ai-external--collect-file (spec file)
  "Collect diagnostics for FILE using external tool SPEC."
  (when (elisp-quality-ai-external--collect-file-p spec file)
    (let ((command (elisp-quality-ai-external--command spec)))
      (cond
       ((and (elisp-quality-ai-external--elsa-spec-p spec)
             elisp-quality-ai-elsa-use-subprocess
             (elisp-quality-ai-external--library-available-p spec))
        (elisp-quality-ai-external--run-elsa-subprocess spec file))
       (command
        (elisp-quality-ai-external--run-command spec file command))
       (t
        (elisp-quality-ai-external--run-library spec file))))))

(defun elisp-quality-ai-external-register-collectors ()
  "Register all optional external tool collectors."
  (dolist (spec elisp-quality-ai-external--collector-specs)
    (let ((spec spec))
      (elisp-quality-ai-register-collector
       (elisp-quality-ai-external--get :name spec)
       (lambda (file)
         (elisp-quality-ai-external--collect-file spec file))
       :requires (elisp-quality-ai-external--get :commands spec)
       :description (elisp-quality-ai-external--get :description spec)
       :enabled (lambda ()
                  (elisp-quality-ai-external--enabled-p spec))
       :available (lambda ()
                    (elisp-quality-ai-external--available-p spec))))))

(elisp-quality-ai-external-register-collectors)

(provide 'elisp-quality-ai-external)
;;; elisp-quality-ai-external.el ends here

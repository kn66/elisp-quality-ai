# Report Schema

This document defines the stable report shapes emitted by
`elisp-quality-ai`.  The schema is intentionally simple: objects are Emacs Lisp
alists with string keys, and arrays are vectors.  This keeps the data easy to
encode as JSON and easy for AI agents to consume.

The current schema version is experimental, but changes should be deliberate and
covered by tests.

## Encoding Rules

- Object keys are strings.
- Arrays are vectors in Emacs Lisp and JSON arrays when encoded.
- Booleans are `t` and `:json-false` internally, encoded as JSON `true` and
  `false`.
- File paths are absolute paths.
- Line numbers are one-based.
- Optional fields may be absent unless listed as required below.

## Project Report

Schema id: `elisp-quality-ai.project/v1`

Required fields:

| Field | Type | Description |
| --- | --- | --- |
| `schema` | string | Always `elisp-quality-ai.project/v1`. |
| `root` | string | An absolute project or directory root path. |
| `summary` | object | Aggregate counts for the report. |
| `files` | array of file reports | Per-file reports. |
| `tasks` | array of tasks | AI-oriented work items ranked by priority. |
| `collectors` | array of collectors | Registered collector metadata and status. |

Project `summary` fields:

| Field | Type | Description |
| --- | --- | --- |
| `files` | integer | Number of scanned Emacs Lisp source files. |
| `definitions` | integer | Total number of collected definitions. |
| `diagnostics` | integer | Total number of diagnostics. |
| `tasks` | integer | Total number of generated tasks. |

Example:

```json
{
  "schema": "elisp-quality-ai.project/v1",
  "root": "/path/to/project/",
  "summary": {
    "files": 1,
    "definitions": 1,
    "diagnostics": 1,
    "tasks": 1
  },
  "files": [],
  "tasks": [],
  "collectors": []
}
```

## File Report

Schema id: `elisp-quality-ai.file/v1`

Required fields:

| Field | Type | Description |
| --- | --- | --- |
| `schema` | string | Always `elisp-quality-ai.file/v1`. |
| `file` | string | Absolute file path. |
| `summary` | object | File-local counts. |
| `definitions` | array of definitions | Definitions found in the file. |
| `diagnostics` | array of diagnostics | Diagnostics found in the file. |
| `collectors` | array of collectors | Registered collector metadata and status. |

File `summary` fields:

| Field | Type | Description |
| --- | --- | --- |
| `definitions` | integer | Number of collected definitions in the file. |
| `diagnostics` | integer | Number of diagnostics in the file. |

## Collector

Collectors wrap built-in or external quality checks.  Collector metadata appears
in project and file reports so unavailable or disabled integrations can be
represented without aborting report generation.

Required fields:

| Field | Type | Description |
| --- | --- | --- |
| `name` | string | Registered collector name. |
| `available` | boolean | Whether the collector's dependencies are available. |
| `requires` | array of strings | Dependency names or tools used by the collector. |
| `enabled` | boolean | Whether the collector is enabled for execution. |
| `description` | string | Human-readable collector summary. |

Collector functions return diagnostics.  Failures during collector execution are
normalized as diagnostics with category `collector` instead of crashing the
report.

The built-in `checkdoc` collector emits diagnostics with source `checkdoc`,
category `style`, severity `warning`, and the normalized source file and line.

The built-in `byte-compile` collector emits diagnostics with source
`byte-compile`, category `compile`, severity `warning` or `error`, and the
normalized source file, line, and column when available.  It writes compiled
output to a temporary directory rather than the project tree.

Optional external collectors are registered for `relint`, `package-lint`,
`cognitive-complexity`, and `elsa`.  When the matching executable or Emacs
package is unavailable, the collector remains in metadata with `available`
encoded as `false` and is skipped during collection.  Elsa is registered but
disabled by default; its metadata uses `enabled: false` until
`elisp-quality-ai-enable-elsa` is non-nil.

## Definition

Definitions are the primary unit for AI-assisted refactoring.

Required fields:

| Field | Type | Description |
| --- | --- | --- |
| `name` | string | Defined symbol name, without quoting. Setf names are rendered as `(setf NAME)`. |
| `kind` | string | Defining form, such as `defun` or `defcustom`. |
| `file` | string | Absolute source file path. |
| `line` | integer | Start line. |
| `end_line` | integer | End line. |
| `public` | boolean | Whether the symbol looks public. |
| `interactive` | boolean | Whether the definition appears interactive. |
| `docstring` | boolean | Whether an obvious docstring was found. |
| `metrics` | object | Definition-local metrics. |

Definition `metrics` fields:

| Field | Type | Description |
| --- | --- | --- |
| `lines` | integer | Definition line count. |

Optional definition fields:

| Field | Type | Description |
| --- | --- | --- |
| `declaration` | boolean | Whether the form only declares an external binding, such as a bare `(defvar SYMBOL)`. Declaration-only forms are not treated as missing-docstring findings. |

Current definition forms:

- `defun`
- `defmacro`
- `defsubst`
- `cl-defun`
- `cl-defmacro`
- `cl-defmethod`
- `defvar`
- `defconst`
- `defcustom`
- `define-minor-mode`
- `define-globalized-minor-mode`
- `define-derived-mode`

## Diagnostic

Diagnostics represent findings from the core analyzer or from collectors.

Required fields:

| Field | Type | Description |
| --- | --- | --- |
| `source` | string | Tool or collector name. |
| `category` | string | Finding category. |
| `severity` | string | Finding severity. |
| `file` | string | Absolute source file path. |
| `line` | integer | One-based source line. |
| `message` | string | Human-readable finding message. |
| `suggestion` | string | Suggested next action. |

Optional fields:

| Field | Type | Description |
| --- | --- | --- |
| `symbol` | string | Related definition symbol. |
| `column` | integer | One-based source column. |
| `end_line` | integer | End line for range diagnostics. |
| `end_column` | integer | End column for range diagnostics. |
| `metric` | string | Metric name, when applicable. |
| `value` | number | Measured value. |
| `threshold` | number | Configured threshold. |
| `collector` | string | Registered collector name, when produced by a collector. |

Allowed `severity` values:

| Value | Meaning |
| --- | --- |
| `error` | Likely incorrect behavior or invalid package state. |
| `warning` | Maintainability, quality, or likely bug risk. |
| `info` | Useful improvement with lower urgency. |

Allowed `category` values:

| Value | Meaning |
| --- | --- |
| `complexity` | Complexity or understandability issue. |
| `size` | Length or physical size issue. |
| `documentation` | Missing or weak documentation. |
| `compile` | Byte compiler warning or error. |
| `style` | Formatting or convention issue. |
| `package` | Package metadata or packaging issue. |
| `regexp` | Regular expression issue. |
| `type` | Type or static-analysis issue. |
| `test` | Test or coverage issue. |
| `collector` | Collector execution or availability issue. |

Collectors should use the closest existing category before adding a new one.

## Task

Tasks are AI-oriented work items derived from diagnostics and metrics.  They
are generated at project-report level, not inside individual file reports.
Tasks are sorted by descending `priority`.

Required fields:

| Field | Type | Description |
| --- | --- | --- |
| `id` | string | Stable task id within one report. |
| `stable_id` | string | Stable cross-run id derived from file, symbol or line, and category. |
| `title` | string | Short action-oriented title. |
| `file` | string | Primary source file. |
| `symbol` | string or null | Primary symbol, if any. |
| `line` | integer | Primary line. |
| `priority` | integer | Higher means more important. |
| `reason` | array of strings | Why this task exists. |
| `constraints` | array of strings | Safety constraints for AI edits. |
| `suggested_steps` | array of strings | Suggested implementation steps. |
| `verify_commands` | array of strings | Project-local commands an agent can run after editing. |
| `diagnostics` | array of diagnostics | Source diagnostics for the task. |

Tasks should preserve public APIs unless the report explicitly says otherwise.

Task priority is report-local.  Higher values should appear earlier in the
`tasks` array and should generally be handled first by an AI agent.

Task `id` values are report-local and rank based.  AI agents that need to track
the same task across multiple runs should prefer `stable_id`.

Priority combines documented scoring weights for diagnostic severity and
category with extra bounded signals for missing docstrings, definition length
over threshold, future complexity over threshold, future test coverage gaps, and
future git churn.

## JSONL Events

JSONL output emits one event per line.  Each event has:

| Field | Type | Description |
| --- | --- | --- |
| `type` | string | Event type. |
| `schema` | string | Always `elisp-quality-ai.event/v1`. |

Current event types:

- `definition`
- `diagnostic`
- `top_task`
- `task`

Definition event fields:

| Field | Type | Description |
| --- | --- | --- |
| `type` | string | Always `definition`. |
| `schema` | string | Always `elisp-quality-ai.event/v1`. |
| `file` | string | Absolute source file path. |
| `symbol` | string | Definition symbol. |
| `kind` | string | Defining form. |
| `line` | integer | Start line. |
| `end_line` | integer | End line. |
| `metrics` | object | Definition metrics. |

Diagnostic events contain `type`, `schema`, and all diagnostic fields.

Top task events contain `type`, `schema`, `rank`, and all task fields.  They are
emitted for the first ranked tasks highlighted in human-readable reports.

Task events contain `type`, `schema`, and all task fields.

## Minimal JSONL Example

```jsonl
{"type":"definition","schema":"elisp-quality-ai.event/v1","file":"/tmp/sample.el","symbol":"sample-public","kind":"defun","line":3,"end_line":5,"metrics":{"lines":3}}
{"type":"diagnostic","schema":"elisp-quality-ai.event/v1","source":"elisp-quality-ai","category":"documentation","severity":"info","file":"/tmp/sample.el","symbol":"sample-public","line":3,"message":"sample-public has no obvious docstring.","suggestion":"Add a concise docstring that states behavior and important arguments."}
{"type":"top_task","schema":"elisp-quality-ai.event/v1","rank":1,"id":"task-001","title":"Document `sample-public`","file":"/tmp/sample.el","symbol":"sample-public","line":3,"priority":35,"reason":["sample-public has no obvious docstring."],"constraints":["Preserve public behavior unless a diagnostic explicitly requires a behavior change.","Keep `sample-public' compatible with existing callers.","Prefer the smallest cohesive change that resolves the diagnostics.","Add or update tests when behavior changes."],"suggested_steps":["Add a concise docstring describing behavior, arguments, and return value.","Run the relevant test and lint commands after editing."],"diagnostics":[]}
{"type":"task","schema":"elisp-quality-ai.event/v1","id":"task-001","title":"Document `sample-public`","file":"/tmp/sample.el","symbol":"sample-public","line":3,"priority":35,"reason":["sample-public has no obvious docstring."],"constraints":["Preserve public behavior unless a diagnostic explicitly requires a behavior change.","Keep `sample-public' compatible with existing callers.","Prefer the smallest cohesive change that resolves the diagnostics.","Add or update tests when behavior changes."],"suggested_steps":["Add a concise docstring describing behavior, arguments, and return value.","Run the relevant test and lint commands after editing."],"diagnostics":[]}
```

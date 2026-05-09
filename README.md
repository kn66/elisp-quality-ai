# elisp-quality-ai

`elisp-quality-ai` builds AI-friendly Emacs Lisp quality reports.

The package is intended to normalize diagnostics from Emacs Lisp quality tools
into stable, machine-readable reports.  The first implementation focuses on a
small built-in inventory:

- project and directory scanning
- top-level definition collection
- per-definition size metrics
- missing docstring diagnostics for public definitions
- collector registration for additional diagnostics
- built-in checkdoc diagnostics
- built-in byte compiler diagnostics
- optional external tool collectors for package-lint, relint,
  cognitive-complexity, and Elsa
- ranked AI task generation from diagnostics
- top issue highlights in Markdown and JSONL
- JSON, JSONL, and Markdown report output

Future collectors can wrap coverage tools and other project-specific checks.
The current schema is documented in `docs/schema.md`.

## Usage

Add the package directory to `load-path`, then require it:

```elisp
(add-to-list 'load-path "/path/to/elisp-quality-ai")
(require 'elisp-quality-ai)
```

From Emacs, run the current project report command:

```elisp
(elisp-quality-ai-report-current-project)
```

To write reports from Lisp:

```elisp
(elisp-quality-ai-write-report default-directory "quality-report.json" 'json)
(elisp-quality-ai-write-report default-directory "quality-report.jsonl" 'jsonl)
(elisp-quality-ai-write-report default-directory "quality-report.md" 'markdown)
```

From a shell in this repository:

```sh
emacs -Q --batch -L . \
  --eval "(require 'elisp-quality-ai)" \
  --eval "(elisp-quality-ai-write-report default-directory \"quality-report.json\" 'json)"
```

Or use the bundled command line wrapper:

```sh
bin/elisp-quality-ai report --root . --format json --output quality-report.json
bin/elisp-quality-ai tasks --root . --format json --limit 5
bin/elisp-quality-ai next-task --root . --format json
```

Use `--output -` or omit `--output` to write to standard output.  Use `--fast`
to skip slower or environment-sensitive collectors such as byte compilation and
optional external tools.  Use `--fail-on error` or `--fail-on warning` when a CI
job should exit non-zero for findings at that severity or worse.
By default, `report` output is optimized for AI context and omits successful
definition inventories.  It includes summaries, diagnostics, tasks, collector
metadata, and only files with diagnostics.  Add `--include-definitions` when you
need the full definition inventory for debugging or archival reports:

```sh
bin/elisp-quality-ai report --root . --format json --include-definitions
```

Project reports include a `tasks` array sorted by weighted priority.  JSONL
output emits diagnostic, `top_task`, and `task` events by default.  With
`--include-definitions`, JSONL also emits definition events.  Markdown output
includes Top Issues and Tasks sections before raw diagnostics.

To inspect the highest ranked task from Emacs Lisp:

```elisp
(let* ((report (elisp-quality-ai-analyze-project default-directory))
       (tasks (cdr (assoc "tasks" report))))
  (and (> (length tasks) 0)
       (aref tasks 0)))
```

AI agents should prefer `bin/elisp-quality-ai tasks --root . --format json`
when choosing work.  Handle tasks in descending priority, use `stable_id` for
cross-run tracking, preserve public behavior unless diagnostics require a
behavior change, and run commands from `verify_commands` after edits.

## Collector Configuration

The built-in `checkdoc` and `byte-compile` collectors run by default.  Disable
collectors by name when you need a faster or more deterministic report:

```elisp
(setq elisp-quality-ai-disabled-collectors
      '("checkdoc" "byte-compile" "relint" "package-lint"
        "cognitive-complexity" "elsa"))
```

The byte compiler collector writes compiled output only to a temporary
directory.  Add project-specific compile dependencies with:

```elisp
(setq elisp-quality-ai-byte-compile-load-path '("lisp" "vendor"))
```

Optional external collectors are registered even when their tools are not
installed.  Unavailable tools appear in collector metadata with
`available: false` and do not stop report generation.  To point a collector at a
specific executable:

```elisp
(setq elisp-quality-ai-external-tool-command-alist
      '(("relint" . "/path/to/relint")))
```

When optional collectors are installed as Emacs packages, initialize packages
before loading `elisp-quality-ai` in batch sessions:

```elisp
(require 'package)
(package-initialize)
(require 'elisp-quality-ai)
```

The optional external collectors are complementary rather than interchangeable:

- `package-lint` checks package structure and distribution metadata, such as
  headers, dependencies, commentary sections, autoload conventions, and MELPA
  packaging expectations.
- `relint` checks regular expressions embedded in Emacs Lisp code and reports
  obsolete, suspicious, or mechanically improvable regexp patterns.
- `cognitive-complexity` reports maintainability signals for definitions whose
  branching and nesting make them harder to understand or modify.
- `Elsa` performs deeper static analysis around symbol resolution, values, and
  type-like flow.  Its findings are closer to semantic program issues than
  package metadata or style checks.

Some findings can overlap, but each collector is treated as a separate signal.
`elisp-quality-ai` normalizes their output into one report while preserving the
collector source so duplicated or related diagnostics can still be interpreted
in context.

The `cognitive-complexity` collector emits a complexity diagnostic when the
score is greater than `elisp-quality-ai-cognitive-complexity-threshold`.
The `package-lint` collector runs only on likely package entry files, avoiding
false package-prefix and metadata findings for split implementation files and
tests.
The Elsa collector is registered but disabled by default because current Elsa
batch APIs have process-wide side effects.  Enable it explicitly with:

```elisp
(setq elisp-quality-ai-enable-elsa t)
```

Tune the built-in size diagnostic with:

```elisp
(setq elisp-quality-ai-definition-length-threshold 80)
```

Collectors can be registered with:

```elisp
(elisp-quality-ai-register-collector
 'example
 (lambda (file)
   (list `(("source" . "example")
           ("category" . "style")
           ("severity" . "info")
           ("file" . ,file)
           ("line" . 1)
           ("message" . "Example collector finding.")
           ("suggestion" . "Review the example finding."))))
 :description "Example collector")
```

## Examples

Sample input and output files live under `examples/`:

- `examples/sample-package.el`
- `examples/sample-report.json`
- `examples/sample-report.jsonl`
- `examples/sample-report.md`

## Development

Run the full local test suite:

```sh
make test
```

Byte-compile into a disposable build directory:

```sh
make compile BUILD_DIR=/tmp/elisp-quality-ai-byte-compile
```

Run package metadata lint after `package-lint` is installed:

```sh
make package-lint
```

With Eask, install development dependencies and run the test script:

```sh
eask install-deps --dev
eask run script test
```

Without `make`, the equivalent test command is:

```sh
emacs -Q --batch -L . -L test -l test/elisp-quality-ai-test.el -f ert-run-tests-batch-and-exit
```

A GitHub Actions example is provided at `.github/workflows/ci.yml`.

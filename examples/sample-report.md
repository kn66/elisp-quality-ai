# elisp-quality-ai report

- Root: `/tmp/elisp-quality-ai-example/`
- Files: 1
- Definitions: 2
- Diagnostics: 2
- Tasks: 2

## Top Issues

1. `task-001` Priority 88 at `/tmp/elisp-quality-ai-example/sample-package.el:11` - Refactor `sample-long` to improve maintainability
2. `task-002` Priority 35 at `/tmp/elisp-quality-ai-example/sample-package.el:8` - Document `sample-undocumented`

## Tasks

### Refactor `sample-long` to improve maintainability

- ID: `task-001`
- Priority: 88
- Location: `/tmp/elisp-quality-ai-example/sample-package.el:11`
- Symbol: `sample-long`

Reason:
- sample-long has 6 lines; threshold is 3.

Suggested steps:
- Identify cohesive sub-operations inside the definition.
- Extract helper functions without changing the public entry point.
- Run the relevant test and lint commands after editing.

### Document `sample-undocumented`

- ID: `task-002`
- Priority: 35
- Location: `/tmp/elisp-quality-ai-example/sample-package.el:8`
- Symbol: `sample-undocumented`

Reason:
- sample-undocumented has no obvious docstring.

Suggested steps:
- Add a concise docstring describing behavior, arguments, and return value.
- Run the relevant test and lint commands after editing.

## Diagnostics

### `/tmp/elisp-quality-ai-example/sample-package.el`

- `/tmp/elisp-quality-ai-example/sample-package.el:8` **info/documentation** sample-undocumented has no obvious docstring.
  Suggestion: Add a concise docstring that states behavior and important arguments.
- `/tmp/elisp-quality-ai-example/sample-package.el:11` **warning/size** sample-long has 6 lines; threshold is 3.
  Suggestion: Consider extracting smaller helpers while preserving the public interface.

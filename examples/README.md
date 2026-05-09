# Examples

This directory contains a small input file and stable sample outputs for the
three report formats.

- `sample-package.el` is the source file used for the examples.
- `sample-report.json` shows the full project report shape.
- `sample-report.jsonl` shows event-oriented output.
- `sample-report.md` shows the Markdown view.

The sample reports use `/tmp/elisp-quality-ai-example/` as a stable placeholder
root.  Reports generated on your machine will contain the absolute path of the
directory you analyze.

To regenerate equivalent output from this directory:

```sh
emacs -Q --batch -L .. \
  --eval "(require 'elisp-quality-ai)" \
  --eval "(let ((elisp-quality-ai-disabled-collectors '(\"checkdoc\" \"byte-compile\" \"relint\" \"package-lint\" \"cognitive-complexity\" \"elsa\")) (elisp-quality-ai-definition-length-threshold 3)) (elisp-quality-ai-write-report default-directory \"sample-report.json\" 'json))"
```

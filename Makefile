EMACS ?= emacs
BUILD_DIR ?= .build

ELISP_FILES := \
	elisp-quality-ai-collector.el \
	elisp-quality-ai-checkdoc.el \
	elisp-quality-ai-byte-compile.el \
	elisp-quality-ai-external.el \
	elisp-quality-ai-task.el \
	elisp-quality-ai-core.el \
	elisp-quality-ai-report.el \
	elisp-quality-ai.el

PACKAGE_FILE := elisp-quality-ai.el

.PHONY: test compile package-lint check clean

test:
	$(EMACS) -Q --batch -L . -L test \
		-l test/elisp-quality-ai-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	@mkdir -p "$(BUILD_DIR)"
	ELISP_QUALITY_AI_BUILD_DIR="$(abspath $(BUILD_DIR))" \
		$(EMACS) -Q --batch -L . \
		--eval "(setq byte-compile-dest-file-function (lambda (file) (expand-file-name (concat (file-name-nondirectory file) \"c\") (getenv \"ELISP_QUALITY_AI_BUILD_DIR\"))))" \
		-f batch-byte-compile $(ELISP_FILES)

package-lint:
	$(EMACS) -Q --batch -L . \
		--eval "(progn (require 'package) (package-initialize) (require 'package-lint))" \
		-f package-lint-batch-and-exit $(PACKAGE_FILE)

check: test compile package-lint

clean:
	rm -rf "$(BUILD_DIR)"

(package "elisp-quality-ai"
         "0.1.0"
         "AI-friendly quality reports for Emacs Lisp")

(website-url "https://example.com/elisp-quality-ai")
(keywords "lisp" "tools" "maint")
(package-file "elisp-quality-ai.el")

(source "gnu")
(source "melpa")

(depends-on "emacs" "29.1")

(development
  (depends-on "package-lint")
  (depends-on "relint"))

(script "test" "emacs -Q --batch -L . -L test -l test/elisp-quality-ai-test.el -f ert-run-tests-batch-and-exit")

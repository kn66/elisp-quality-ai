;;; sample-package.el --- Example package -*- lexical-binding: t; -*-

;;; Commentary:
;; Example package used by elisp-quality-ai docs.

;;; Code:

(defun sample-undocumented (x)
  "Return X."
  x)

(defun sample-long (value)
  "Return VALUE after simple processing."
  (let ((first value))
    (let ((second first))
      (let ((third second))
        third))))

(provide 'sample-package)
;;; sample-package.el ends here

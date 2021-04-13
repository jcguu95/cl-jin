(in-package :jin-utils)

(defun org-timestamp (str)
  "Convert org-mode time stamp to time."
;; example
;; (org-timestamp "[2020-01-02 19:30]")
  (read-from-string
   (uiop:run-program
    (format nil "~a ~a"
            "emacs -batch -eval"
            (format nil
                    "\"(progn ~a (print (org-timestamp-from-string \\\"~a\\\")))\""
                    "(require 'org-element)" str))
    :output '(:string :stripped t))))

(in-package :jin)

(defun org-timestamp (str)
  "Convert org-mode time stamp to time."
;; example
;; (org-timestamp "[2020-01-02 19:30]")
  (read-from-string
   (uiop:run-program
    (format nil "~a ~a"
            "emacsclient -e"
            (format nil
                    "'(org-timestamp-from-string \"~a\")'" str))
    :output '(:string :stripped t))))

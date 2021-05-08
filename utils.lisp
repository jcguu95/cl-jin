(in-package :recording)

(defun make-prefix ()
  "Make prefix for output files."
  (concatenate 'string
               (sb-ext:posix-getenv "HOME")
               "/"
               (local-time:format-timestring
                t (local-time:now)
                :format '((:YEAR 4) (:MONTH 2) (:DAY 2)
                          #\-
                          (:HOUR 2) (:MIN 2) (:SEC 2)))))

(defun kill-all (&key force)
  (let ((sig (if force 9 15)))
    (mapcar (lambda (x)
              (sb-ext:process-kill x sig))
            *processes*)))

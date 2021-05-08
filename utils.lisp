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

(defun grab-dimension ()
  (string-right-trim
   (format nil "~%")
   (with-output-to-string (s)
     (uiop:run-program
      ;; TODO get rid of bashism
      "xdpyinfo | grep dimensions | awk '{print $2;}'"
      :output s))))

(defun dmenu (candidates &optional prompt)
  (string-right-trim
   '(#\Newline)
   (with-output-to-string
       (out)
     (with-input-from-string
         (in (format nil "~{~a~%~}" candidates))
       (sb-ext:run-program "/usr/local/bin/dmenu" `("-p" ,prompt)
                           :input in :output out)))))

(defun any-alive-p ()
  "Return if any recording process is alive."
  (eval `(or ,@(mapcar #'sb-ext:process-alive-p *processes*))))

(defun kill-all (&key force)
  (let ((sig (if force 9 15)))
    (mapcar (lambda (x)
              (sb-ext:process-kill x sig))
            *processes*)))

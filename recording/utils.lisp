(in-package :jin.recording)

(defparameter *store* (concatenate 'string
                                   (sb-ext:posix-getenv "HOME")
                                   "/data/capture/+media/"))

(defun make-prefix ()
  "Make prefix for output files."
  (concatenate 'string
               *store*
               (local-time:format-timestring
                t (local-time:now)
                :format '((:YEAR 4) (:MONTH 2) (:DAY 2)
                          #\-
                          (:HOUR 2) (:MIN 2) (:SEC 2)))))

;; TODO Use jin.utils:whereis
(defun whereis (target)
  (string-right-trim
   (format nil "~%")
   (with-output-to-string (out)
     (uiop:run-program
      (format nil "whereis ~a | cut -d' ' -f2" target)
      :output out))))

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
       (sb-ext:run-program (whereis "dmenu")
                           `("-p" ,prompt)
                           :input in :output out)))))

(defun any-alive-p ()
  "Return if any recording process is alive."
  (eval `(or ,@(mapcar #'sb-ext:process-alive-p
                       *processes*))))

(defun kill-all (&key force)
  (let ((sig (if force 9 15)))
    (mapcar (lambda (x)
              (sb-ext:process-kill x sig))
            *processes*)))

(in-package :jin.service)

(local-time:enable-read-macros)

(defparameter *root*
  (ensure-directories-exist "~/.local/lisp-service/"))

(defmacro call-with-error-barfed-and-ignored (body)
  "Call the body normally if there is no error. Otherwise, print
the error condition and backtrace to *STANDARD-OUTPUT*, and
leave (non-locally?)."
  `(block try
     (handler-bind
         ((error (lambda (condition)
                   (let ((now (local-time:now)))
                     (cl-user::print-backtrace :stream *standard-output*))
                   (return-from try (list :error condition)))))
       ,body)))

(defclass service ()
  ((name
    :initform
    (error "Must supply a name.")
    :initarg :name
    :accessor name
    :documentation ;; TODO How to force checking?
    "The name for the service. It should be a string that only
   uses +-_0-9a-z." )

   (action
    :initform '(lambda nil)
    :initarg :action
    :accessor action
    :documentation ;; TODO How to force checking?
    "A list that evaluates to a lisp function which takes no
    argument. We do not want an action to be a function
    immediately because we want to log the action into our log
    files. This way, more information is retained, and we believe
    it makes debugging easier.")))

(defgeneric log-dir (s)
  (:documentation
   "Generate the PATH-DIR for the service S. All log files go
  into it."))

(defmethod log-dir ((s service))
    (ensure-directories-exist
     (concatenate 'string *root* (name s) "/")))

(defmethod log-dir ((s string))
    (ensure-directories-exist
     (concatenate 'string *root* s "/")))

(defun latest-launch (service)
  "Return the timestamp (in the sense of the package :LOCAL-TIME)
of latest report of SERVICE."
  (read-from-string
   (file-namestring
    (car (last (uiop:directory-files
                (log-dir service)))))))

(defgeneric print-readably (object stream))
(defmethod print-readably ((object cons) stream)
  (let ((list object))
    (princ "(" stream)
    (print-readably (car list) stream)
    (loop
      :while (consp (cdr list))
      :do (princ " " stream)
          (pop list)
          (print-readably (car list) stream))
    (if (null (cdr list))
        (progn (princ ")" stream) nil)
        (progn (princ " . " stream)
               (print-readably (cdr list) stream)
               (princ ")" stream)
               nil))))

(defmethod print-readably ((object t) stream)
  (handler-case
      (let ((*print-readably* t))
        (prin1-to-string object))
    (:no-error (result)
      (write-string result stream))
    (error ()
      (let ((*print-readably* nil))
        (prin1 (list :unreadably-printable-flag
                     (prin1-to-string object))
               stream)))))

(defgeneric dispatch (s)
  (:documentation
   "Dispatch the SERVICE as a thread. Log everything into the
   LOG-DIR of SERVICE."))

(defmethod dispatch ((service service))
  "Dispatch the service SERVICE by making a thread.
*STANDARD-OUTPUT* is redirected to the log-file of the service.
 Finally, the ACTION of the service is called by
 #'CALL-WITH-ERROR-BARFED-AND-IGNORED. Therefore, if there is an error, the
 ACTION should be ended (by a non-local jump), and the
 error-condition and backtrace should be logged."
  (let* ((now (local-time:now))
         (log-file (concatenate 'string
                                (log-dir service)
                                (prin1-to-string now)))
         (log ""))

    (bt:make-thread
     (lambda ()
       (let (return)
         (let* ((s (make-string-output-stream))
                (*standard-output* s))
           (setf multi-return (multiple-value-list
                         (call-with-error-barfed-and-ignored
                          (funcall (eval (action service))))))
           (setf log (get-output-stream-string s)))

         (with-open-file
             (*standard-output* log-file :direction :output
                                         :if-exists :append
                                         :if-does-not-exist :create)
           (format t "(~%~%~{  ~s~%~%~})"
           ;; (print-readably
            (list :time now
                  :action (action service)
                  :multi-return
                  (with-output-to-string (str)
                    (print-readably multi-return
                                    str))
                  :log log)
            ;; *standard-output*)
           )
           ))))

    (format t "See log file at:")
    log-file))

(defun help ()
  (format t "Make a service instance by~%~%")
  (format t "  ~s"
          '(make-instance
            'service
            :name "example"
            :action '(lambda ()
                      (uiop:run-program
                       "/path/to/example"
                       :output *standard-output*
                       :error-output *standard-output*))))
  (format t "~%~%and dispatch by~%~%")
  (format t "  (dispatch it)."))

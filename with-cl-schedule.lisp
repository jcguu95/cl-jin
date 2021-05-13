(defmacro schedule-service (name action &rest schedule-definition)
  "This macro makes use of Jin's cl-schedule v0.0.5. It wraps #'dispatch
in a #'cl-schedule:schedule-function form."
  ;; Depends on jin's cl-schedule v0.0.3.
  ;; https://github.com/jcguu95/cl-schedule/tree/18707cb325138b177b0d5e3775b4e78cf67583b2
  ;;
  ;; __A minimal example__
  ;;
  ;; (schedule-service
  ;;  "hello"                                ; name
  ;;  '(lambda ()
  ;;    (print "Hello!")
  ;;    (values 1 2 3))
  ;;  :second '(member 0 10 20 30 40 50)
  ;;  :day-of-week '(integer 2 4))
  ;;  
  `(let* ((name ,name)
          (schedule (cl-schedule:make-typed-cron-schedule ,@schedule-definition))
          (service (make-instance 'jin.service:service
                                  :name name
                                  :action ,action)))
     (cl-schedule:schedule-function
      (lambda () (jin.service:dispatch service))
      (cl-schedule:make-scheduler schedule
                           :init-time
                           ;; FIXME This doesn't take care when no log is present.
                           (local-time:timestamp-to-universal
                            (or (getf (car (jin.service:logs
                                            (jin.service::name service)))
                                      :time)
                                (local-time:now))))
      :name name
      :immediate t
      :ignore-skipped t
      :thread t)))


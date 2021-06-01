(in-package :jin.service)

(dispatch-service
 "tmp--toy-example"
 '(lambda ()
   (print "hi")
   (values 1 2 3)))

(dispatch-service
 "tmp--test-example"
 '(lambda ()
   (prin1 "hi") ;; this will show in the :LOG.
   (defclass *test* () (name))
   (let ((*h1* (make-hash-table))
         (*h2* (make-hash-table))
         (*t* (make-instance '*test*)))
     (setf (gethash 'foo *h1*) 'quux) ;; *h1* is readably-printable.
     (setf (gethash 'foo *h2*) *t*) ;; *h2* is not readbly-printable.
     (values 1 2 (+ 1 2) *h1* *h2* *t*))))

(dispatch-service
 "tmp--test-example-fail"
 '(lambda ()
   (uiop:run-program "echo '"
    :output *standard-output*)))

(dispatch-service
 "tmp--audio-capturer"
 '(lambda ()
   (uiop:run-program
    "/home/jin/.scripts/audio_capture 598"
    :output *standard-output*
    :error-output *standard-output*)))

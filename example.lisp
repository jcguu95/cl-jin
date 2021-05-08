(in-package :jin.service)

(defparameter *audio-capture-service*
  (make-instance
   'service
   :name "audio-capturer"
   :action '(lambda ()
             (uiop:run-program
              "/home/jin/.scripts/audio_capture 598"
              :output *standard-output*
              :error-output *standard-output*))))
;; (dispatch *audio-capture-service*)

(defparameter *toy-service*
  (make-instance
   'service
   :name "test"
   :action '(lambda ()
             (prin1 "hi")               ;; this will show in the :LOG.
             (defclass *test* () (name))
             (let ((*h1* (make-hash-table))
                   (*h2* (make-hash-table))
                   (*t* (make-instance '*test*)))
               (setf (gethash 'foo *h1*) 'quux) ;; *h1* is readably-printable.
               (setf (gethash 'foo *h2*) *t*)   ;; *h2* is not readbly-printable.
               (values 1 2 (+ 1 2) *h1* *h2* *t*)))))
;; (dispatch *toy-service*)

(defparameter *toy-service-fail*
  (make-instance
   'service
   :name "test-fail"
   ;; :action '(lambda ()
   ;;           (/ 0))
   :action '(lambda ()
             (uiop:run-program "echo '"
              :output *standard-output*))))
;; (dispatch *toy-service-fail*)

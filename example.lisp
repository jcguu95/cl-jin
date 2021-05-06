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
             (prin1 "hi") (values 1 2 (+ 1 2)))))
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

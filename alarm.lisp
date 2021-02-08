(in-package :jin)

(defparameter *mixer*
  (cl-binaural::create-mixer))

(defun make-a-sound ()
  (cl-binaural::mixer-add-streamer
   *mixer*
   (cl-binaural::make-instance
    'cl-binaural::naive-binaurer
    :angle (/ pi 6) ; angle between ears axis and the source direction
    :radius 20      ; distance to the source, in centimeters
    :streamer (cl-binaural::make-instance 'cl-binaural::test-streamer) ; streamer object to binaurize
    )))

;; FIXME I want this to be a thread, not loop.
;; Otherwise, it freezes stumpwm.
(defun make-sound-for-seconds (sec)
  (let ((tmp-thread
          (sb-thread:make-thread
           (lambda ()
             (loop (make-a-sound)
                   (sleep 1))))))
    ;; the let-form above will have launched the sound maker.

    ;; schedule a timer that terminates the sound maker above
    ;; after SEC seconds.
    (sb-impl::schedule-timer
     (sb-impl::make-timer
      (lambda () (sb-thread:terminate-thread tmp-thread))
      :name "Sound maker killer.")
     sec)))

(defun alarm ()
  ; http://sbcl.org/manual/#Timers
  ;; TODO Want to make it declarative. How to?
  (print "TODO"))

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
  (sb-sys::with-timeout sec
    (loop (make-a-sound)
          (sleep 1))))

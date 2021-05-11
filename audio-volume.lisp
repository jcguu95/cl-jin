(in-package :jin.utils)

(defun alter-audio-volume (n)
  "Alter audio volume by n%."
  ;; TODO Add indication for the current volume.
  (setf n (round n))
  (let* ((pactl-command "/usr/bin/pactl")
         (service
           (make-instance
            'jin.service:service
            :name "audio-volume"
            :action `(lambda ()
                       (sb-ext:run-program
                        ,pactl-command
                        ',(list "set-sink-volume" "0"
                                (if (> n 0)
                                    (format nil "+~d%" n)
                                    (format nil "~d%" n)))
                        :output *standard-output*
                        :error *standard-output*)))))
    (jin.service:dispatch service)))

(defun toggle-mute-audio ()
  (let* ((pactl-command "/usr/bin/pactl")
         (service (make-instance
                   'jin.service:service
                   :name "audio-volume"
                   :action `(lambda ()
                              (sb-ext:run-program
                               ,pactl-command
                               ',(list "set-sink-mute" "0" "toggle")
                               :output *standard-output*
                               :error *standard-output*)))))
    (jin.service:dispatch service)))

(defun normalize-audio ()
  (let* ((pactl-command "/usr/bin/pactl")
         (service (make-instance
                   'jin.service:service
                   :name "audio-volume"
                   :action `(lambda ()
                              (sb-ext:run-program
                               ,pactl-command
                               ',(list "set-sink-volume" "0" "100%")
                               :output *standard-output*
                               :error *standard-output*)))))
    (jin.service:dispatch service)))

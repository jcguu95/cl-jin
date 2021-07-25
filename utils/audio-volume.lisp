(in-package :jin.utils)

(defun get-volume ()
  "Return current audio volume in % as a number."
  (read-from-string
   (with-output-to-string (s)
     (sb-ext:run-program "/usr/bin/pamixer"
                         '("--get-volume")
                         :output s
                         :error *standard-output*))))

(defun muted? ()
  "Return if it is muted."
  (let ((result (with-output-to-string (s)
                  (sb-ext:run-program "/usr/bin/pamixer"
                                      '("--get-volume-human")
                                      :output s
                                      :error *standard-output*))))
    (string= result (format nil "muted~%"))))

(defun alter-audio-volume (n)
  "Alter audio volume by n%."
  (setf n (round n))
  (let* ((pactl-command "/usr/bin/pactl")
         (string (if (> n 0)
                     (format nil "+~d%" n)
                     (format nil "~d%" n)))
         (service
           (make-instance
            'jin.service:service
            :name "audio-volume"
            :action `(lambda ()
                       (sb-ext:run-program
                        ,pactl-command
                        ',(list "set-sink-volume"
                                "0" string)
                        :output *standard-output*
                        :error *standard-output*)))))
    (jin.utils:notify-send "Audio Volume"
                           (format nil ": ~d -> ~d%~a"
                                   string (get-volume)
                                   (if (muted?) " (muted)" ""))
                           1500)
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
    (jin.utils:notify-send "Audio Volume"
                           (format nil ": toggle mute -> ~d%~a"
                                   (get-volume) (if (muted?) " (muted)" " (unmuted)"))
                           1500)
    (jin.service:dispatch service)))

(defun normalize-audio ()
  (let* ((pactl-command "/usr/bin/pactl")
         (service (make-instance
                   'jin.service:service
                   :name "audio-volume"
                   :action `(lambda ()
                              (sb-ext:run-program
                               ,pactl-command
                               ;; FIXME set-sink-volume 0 doesn't
                               ;; work all the time, see
                               ;; https://unix.stackexchange.com/questions/560467/
                               ;; is-there-a-consistent-shell-command-for-adjusting-volume
                               ',(list "set-sink-volume" "0" "100%")
                               :output *standard-output*
                               :error *standard-output*)))))
    (jin.utils:notify-send "Audio Volume"
                           (format nil ": normalize audio -> ~d%~a"
                                   (get-volume)
                                   (if (muted?) " (muted)" " (unmuted)"))
                           1500)
    (jin.service:dispatch service)))

(defun audio-volume ()
  (string-trim '(#\Newline)
               (with-output-to-string (s)
                 (uiop:run-program
                  "amixer get Master | grep 'Front Left' | grep -o '[0-9]*%'"
                  :output s))))
(audio-volume)

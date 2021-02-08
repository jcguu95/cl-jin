; might have to tweak hw:X,Y based on "aplay -l". see more in this thread.
; https://superuser.com/questions/53957/what-do-alsa-devices-like-hw0-0-mean-how-do-i-figure-out-which-to-use
;
; Most codes are taken from Luke Smith.

(in-package :recording)

(defun stop-recording ()
  (let* ((pid-file "/tmp/recordingpid")
         (rec-pid (parse-integer (uiop:read-file-string pid-file))))
    (uiop:run-program (format nil "kill -15 ~a" rec-pid))
    (uiop:run-program (format nil "rm -f ~a" pid-file))
    (uiop:run-program (format nil "sleep 3"))
    (uiop:run-program (format nil "kill -9 ~a" rec-pid))))

(defun screencast ()
  (uiop:run-program
   (format nil "~{~a ~}"
           '("ffmpeg -y" "-f x11grab"
             "-framerate 60"
             "-s \"$(xdpyinfo | grep dimensions | awk '{print $2;}')\" "
             "-i \"$DISPLAY\" "
             "-f alsa -i hw:0,0 "
             "-r 30 "
             "-c:v h264 -crf 0 -preset ultrafast -c:a aac "
             " \"$HOME/$(date '+%Y-%m-%d-%H%M%S')_screencast.mp4\" & "
             "echo $! > /tmp/recordingpid "
             ))))

(defun webcam ()
  ;; TODO How to enable voice recording too?
  (uiop:run-program
  (format nil "~{~a ~}"
          '("ffmpeg"
            "-f v4l2"
            "-i /dev/video0"
            "-video_size 640x480"
            "\"$HOME/$(date '+%Y-%m-%d-%H%M%S')_webcam.mkv\" &"
            "echo $! > /tmp/recordingpid"))))

(defun audio ()
  (uiop:run-program
   (format nil "~{~a ~}"
           '("ffmpeg"
             "-f alsa -i hw:0,0"
             "-ab 50k"
             "-c:a mp3"
             "\"$HOME/$(date '+%Y-%m-%d-%H%M%S')_audio.mp3\" &"
             "echo $! > /tmp/recordingpid"))))

(defun video ()
  (uiop:run-program
   (format nil "~{~a ~}"
           '("ffmpeg"
             "-f x11grab"
             "-s \"$(xdpyinfo | grep dimensions | awk '{print $2;}')\""
             "-i \"$DISPLAY\""
             "-c:v libx264 -qp 0 -r 30"
             "\"$HOME/$(date '+%Y-%m-%d-%H%M%S')_video.mkv\" &"
             "echo $! > /tmp/recordingpid"))))

; might have to tweak hw:X,Y based on "aplay -l". see more in this thread.
; https://superuser.com/questions/53957/what-do-alsa-devices-like-hw0-0-mean-how-do-i-figure-out-which-to-use
;
; Most codes are taken from Luke Smith.

(defparameter *processes* nil)

(in-package :recording)

(defun stop-recording ()
  (let* ((pid-file "/tmp/recordingpid")
         (rec-pid (parse-integer (uiop:read-file-string pid-file))))
    ;; kill it gently
    (uiop:run-program (format nil "kill -15 ~a" rec-pid))
    ;; FIXME not a robust way to detect if the program is running.
    (uiop:run-program (format nil "rm -f ~a" pid-file))
    ;; sleep for a few seconds and then kill it forcefully.
    ;; FIXME this step produces an error if ffmpeg has been successfully killed.
    (uiop:run-program (format nil "sleep 3"))
    (uiop:run-program (format nil "kill -9 ~a" rec-pid))))

(defun record-screencast ()
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

(defun record-webcam ()
  "With :wait being NIL,this will return a process. I can then
terminate that process by sb-ext:process-kill."
  (push
   (sb-ext:run-program
    "/usr/bin/ffmpeg"                   ; TODO make it portable
    `("-f" "v4l2" "-i" "/dev/video0"
           "-video_size" "640x480"
           ,(concatenate 'string (make-prefix) ".mkv"))
    :output *standard-output*
    :error *standard-output*
    :wait nil)
   *processes*))

(defun record-audio ()
  "With :wait being NIL,this will return a process. I can then
terminate that process by sb-ext:process-kill."
  (push
   (sb-ext:run-program
    "/usr/bin/ffmpeg"                   ; TODO make it portable
    `("-f" "alsa" "-i" "hw:0,0"
           "-ab" "50k" "-c:a" "mp3"
           ,(concatenate 'string
                         (sb-ext:posix-getenv "HOME")
                         "/"
                         (local-time:format-timestring
                          t (local-time:now)
                          :format '((:YEAR 4) (:MONTH 2) (:DAY 2)
                                    #\-
                                    (:HOUR 2) (:MIN 2) (:SEC 2)))
                         ".mp3"))
    :output *standard-output*
    :error *standard-output*
    :wait nil)
   *processes*))

(defun record-video ()
  (uiop:run-program
   (format nil "~{~a ~}"
           '("ffmpeg"
             "-f x11grab"
             "-s \"$(xdpyinfo | grep dimensions | awk '{print $2;}')\""
             "-i \"$DISPLAY\""
             "-c:v libx264 -qp 0 -r 30"
             "\"$HOME/$(date '+%Y-%m-%d-%H%M%S')_video.mkv\" &"
             "echo $! > /tmp/recordingpid"))))

(defun prompt-recording ()
  (if
   ;; FIXME not a robust way to tell if the program is running.
   (uiop:file-exists-p "/tmp/recordingpid")

   ;; If "tmp/recordingpid" exists, treat as if the program
   ;; hasn't terminated since last time. Ask the user if they
   ;; want to kill it or not by dmenu. Follow accordingly.
   (alexandria:switch
       ((uiop:run-program
         (format nil
                 "echo -e \"~{~a\\n~}\" | dmenu -p \"~a\""
                 '("YES" "NO")
                 "Previous recording ain't terminated yet. Kill it?")
         :output '(:string :stripped t))
        :test #'string=)
     ("YES" (stop-recording))
     ("NO" nil))

   ;; Use dmenu to let the user choose which recording style
   ;; they want.
   (alexandria:switch
       ((uiop:run-program
         (format nil "echo -e \"~{~a\\n~}\" | dmenu"
                 '("audio" "video" "webcam" "screencast"))
         :output '(:string :stripped t)) :test #'string=)
     ("audio" (record-audio))
     ("video" (record-video))
     ("webcam" (record-webcam))
     ("screencast" (record-screencast)))))

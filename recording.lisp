(in-package :recording)

(defparameter *processes* nil)

(defun record-screencast ()
  "With :wait being NIL,this will return a process. I can then
terminate that process by sb-ext:process-kill."
  (push
   (sb-ext:run-program
    "/usr/bin/ffmpeg"                   ; TODO make it portable
    `("-y" "-f" "x11grab" "-framerate" "60"
           "-s" ,(grab-dimension)
           "-i" ,(sb-ext:posix-getenv "DISPLAY")
           "-f" "alsa" "-i" "hw:0,0"
           "-r" "30"
           "-c:v" "h264" "-crf" "0" "-preset" "ultrafast" "-c:a" "aac"
           ,(concatenate 'string (make-prefix) ".mp4"))
    :output *standard-output*
    :error *standard-output*
    :wait nil)
   *processes*))

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
           ,(concatenate 'string (make-prefix) ".mp3"))
    :output *standard-output*
    :error *standard-output*
    :wait nil)
   *processes*))

(defun record-video ()
  "With :wait being NIL,this will return a process. I can then
terminate that process by sb-ext:process-kill."
  (push
   (sb-ext:run-program
    "/usr/bin/ffmpeg"                   ; TODO make it portable
    `("-f" "x11grab"
           "-s" ,(grab-dimension)
           "-i" ,(sb-ext:posix-getenv "DISPLAY")
           "-c:v" "libx264" "-qp" "0" "-r" "30"
           ,(concatenate 'string (make-prefix) ".mkv"))
    :output *standard-output*
    :error *standard-output*
    :wait nil)
   *processes*))

(defun prompt-recording ()
  (if (any-alive-p)

      (alexandria:switch
          ((uiop:run-program
            (format nil
                    "echo -e \"~{~a\\n~}\" | dmenu -p \"~a\""
                    '("YES" "NO")
                    "Previous recording ain't terminated yet. Kill it?")
            :output '(:string :stripped t))
           :test #'string=)
        ("YES" (kill-all))
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

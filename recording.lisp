(in-package :recording)

(defparameter *processes* nil)

(defun record-screencast ()
  "With :wait being NIL,this will return a process. I can then
terminate that process by sb-ext:process-kill."
  (push
   (sb-ext:run-program
    (whereis "ffmpeg")
    `("-y" "-f" "x11grab"
           "-framerate" "60"
           "-s" ,(grab-dimension)
           "-i" ,(sb-ext:posix-getenv "DISPLAY")
           "-f" "alsa"
           "-i" "hw:0,0"
           "-r" "30"
           "-c:v" "h264"
           "-crf" "0"
           "-preset" "ultrafast"
           "-c:a" "aac"
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
    (whereis "ffmpeg")
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
    (whereis "ffmpeg")
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
    (whereis "ffmpeg")
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
          ((dmenu '("YES" "NO")
                  "Kill previous recordings?")
           :test #'string=)
        ("YES" (kill-all))
        ("NO" nil))
      (alexandria:switch
          ((dmenu '("audio" "video" "webcam" "screencast")
                  "Select an option to start recording.")
           :test #'string=)
        ("audio" (record-audio))
        ("video" (record-video))
        ("webcam" (record-webcam))
        ("screencast" (record-screencast)))))

;; TODO use jin.service to start and log recordings.
;;
;; --  Under Construction --
;;
;;   FIXME The output is obscured..
;;
;; (make-instance 'jin.service::service
;;                :name "testing"
;;                :action
;;                '(lambda ()
;;                  (let ((process (sb-ext:run-program
;;                                  (whereis "ffmpeg")
;;                                  `("-y" "-f" "x11grab"
;;                                         "-framerate" "60"
;;                                         "-s" ,(grab-dimension)
;;                                         "-i" ,(sb-ext:posix-getenv "DISPLAY")
;;                                         "-f" "alsa"
;;                                         "-i" "hw:0,0"
;;                                         "-r" "30"
;;                                         "-c:v" "h264"
;;                                         "-crf" "0"
;;                                         "-preset" "ultrafast"
;;                                         "-c:a" "aac"
;;                                         ,(concatenate 'string (make-prefix) ".mp4"))
;;                                  :output *standard-output*
;;                                  :error *standard-output*
;;                                  :wait nil)))
;;                    (push process *processes*)
;;                    process)))

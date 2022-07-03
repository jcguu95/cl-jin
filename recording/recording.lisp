(in-package :jin.recording)

;; TODO use jin.service to launch recordings.
;; TODO need to rewrite docstrings
;; TODO also notify the user when the recording ends.

(defparameter *processes* nil)

(defun screenshot (&optional select-mode)
  "With :wait being NIL,this will return a process. I can then
terminate that process by sb-ext:process-kill."
  ;; TODO let the user choose if fullscreen or what
  (if select-mode
      (setf select-mode "-s")
      (setf select-mode ""))
  (push (sb-ext:run-program
         (jin.utils:whereis "scrot")
         `(,select-mode "-q" "35" ; q stands for quality
                        ,(concatenate 'string (make-prefix) ".jpg"))
         :output *standard-output*
         :error *standard-output*
         :wait nil)
        *processes*)
  ;; (jin.utils:notify-send
  ;;  "" (format nil "Took a screenshot in ~a!" *store*) 3000)
  )

(defun record-screencast ()
  "With :wait being NIL,this will return a process. I can then
terminate that process by sb-ext:process-kill."
  (push (sb-ext:run-program
         (jin.utils:whereis "ffmpeg")
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

        *processes*)
  (jin.utils:notify-send
   "" (format nil "Screencast recording in ~a!" *store*) 3000))

(defun record-webcam ()
  "With :wait being NIL,this will return a process. I can then
terminate that process by sb-ext:process-kill."
  (push (sb-ext:run-program
         (jin.utils:whereis "ffmpeg")
         `("-f" "v4l2" "-i" "/dev/video0"
                "-video_size" "640x480"
                ,(concatenate 'string (make-prefix) ".mkv"))
         :output *standard-output*
         :error *standard-output*
         :wait nil)

        *processes*)
  (jin.utils:notify-send
   "" (format nil "Webcam recording in ~a!" *store*) 3000))

(defun record-audio ()
  "With :wait being NIL,this will return a process. I can then
terminate that process by sb-ext:process-kill."
  (push (sb-ext:run-program
         (jin.utils:whereis "ffmpeg")
         `("-f" "alsa" "-i" "hw:0,0"
                "-ab" "50k" "-c:a" "mp3"
                ,(concatenate 'string (make-prefix) ".mp3"))
         :output *standard-output*
         :error *standard-output*
         :wait nil)

        *processes*)
  (jin.utils:notify-send
   "" (format nil "Audio recording in ~a!" *store*) 3000))

(defun record-video ()
  "With :wait being NIL,this will return a process. I can then
terminate that process by sb-ext:process-kill."
  (push (sb-ext:run-program
         (jin.utils:whereis "ffmpeg")
         `("-f" "x11grab"
                "-s" ,(grab-dimension)
                "-i" ,(sb-ext:posix-getenv "DISPLAY")
                "-c:v" "libx264" "-qp" "0" "-r" "30"
                ,(concatenate 'string (make-prefix) ".mkv"))
         :output *standard-output*
         :error *standard-output*
         :wait nil)

        *processes*)
  (jin.utils:notify-send
   "" (format nil "Video recording in ~a!" *store*) 3000))

(defun prompt-recording ()
  (if (any-alive-p)
      (alexandria:switch
          ((jin.utils:dmenu '("YES" "NO")
                  "Kill previous recordings?")
           :test #'string=)
        ("YES" (kill-all))
        ("NO" nil))
      (alexandria:switch
          ((jin.utils:dmenu '("audio" "video" "webcam" "screencast")
                            "Select an option to start recording:")
           :test #'string=)
        ("audio" (record-audio))
        ("video" (record-video))
        ("webcam" (record-webcam))
        ("screencast" (record-screencast))
        ("screenshot" (screenshot))
        )))

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
;;                                  (jin.utils:whereis "ffmpeg")
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

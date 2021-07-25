;; Only works for `X11`, with `xcompmgr` running in the
;; background, and with `transset-df` installed.

(in-package :jin.utils)

(defun set-opacity (n &optional select)
  "Set the opacity of the focused/selected window to N, which is
a floating number between 0 and 1. It is a wrapper for
`transset-def -v [-a/-c] $n`."
  (unless (<= 0 n 1)
    (error "Arg must be a number between 0 and 1."))
  (let* ((command (whereis "transset-df"))
         (selecting-method (if select "--click" "--actual"))
         (service (make-instance
                   'jin.service:service
                   :name "window-transparency"
                   :action `(lambda ()
                              (sb-ext:run-program
                               ,command
                               ',(list "--verbose" selecting-method (format nil "~f" n))
                               :output *standard-output*
                               :error *standard-output*)))))
    (jin.utils:notify-send "Opacity"
                           (format nil "~a~%~a"
                                   (format nil ": -> ~f" n)
                                   (opacity-hint-message))
                           1500)
    (jin.service:dispatch service)))

(defun alter-opacity (n &optional select)
  "Alter the opacity of the focused/selected window by increasing
N, which is a floating number between -1 and 1. It is a wrapper
for `transset-def -v [-a/-c] [--inc/--dec] $n`."
  (unless (<= -1 n 1)
    (error "Arg must be a number between -1 and 1."))
  (let* ((command (whereis "transset-df"))
         (selecting-method (if select "--click" "--actual"))
         (inc/dec (if (< n 0) (progn (setf n (- n)) "--dec") "--inc"))
         (service (make-instance
                   'jin.service:service
                   :name "window-transparency"
                   :action `(lambda ()
                              (sb-ext:run-program
                               ,command
                               ',(list "--verbose" selecting-method inc/dec (format nil "~f" n))
                               :output *standard-output*
                               :error *standard-output*)))))
    (jin.utils:notify-send "Opacity"
                           (format nil "~a~%~a"
                                   (format nil ": ~a by ~f" inc/dec n)
                                   (opacity-hint-message))
                           1500)
    (jin.service:dispatch service)))

(defun opacity-hint-message ()
  (format nil "Hint: make sure xcompmgr is running."))

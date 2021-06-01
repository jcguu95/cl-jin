(in-package :jin.utils)

(defun xrandr-output ()
  (string-trim '(#\Newline)
               (with-output-to-string (s)
                 (uiop:run-program
                  "xrandr | grep -w connected | cut -f '1' -d ' '"
                  :output s))))

(defun set-brightness (n)
  "Set monitor brightness. By default, n = 1. When surrounding is
dark, can set n to be 0.5 for example."
  (if (not (and (numberp n) (<= 0.1 n 2)))
      (warn "Input must be a number between 0.1 and 2.")
      (let ((service (make-instance
                      'jin.service:service
                      :name "alter-brightness"
                      :action `(lambda ()
                                 (sb-ext:run-program
                                  ;; TODO Make it more portable by using `whereis`.
                                  "/usr/bin/xrandr"
                                  ',(list "--output" (xrandr-output)
                                          "--brightness" (format nil "~2$" n)))))))
        (jin.utils:notify-send "Brightness" (format nil "set to ~2$" n) 5000)
        (jin.service:dispatch service))))

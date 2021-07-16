(in-package :jin.curfew)

(defun time-within (t0 t1)
  "Return a function: time \\mapsto (time \\in [t0,t1])."
  (lambda (time)
    (if (<= t0 t1)
        (<= t0 time t1)
        (or (<= t0 time)
            (>= t1 time)))))

(defun now-in-int ()
  "Return current time in a 4-digit integer. For example, 12:30pm
is represented by 1230, while 10:30pm is represented as 2230."
  (let ((h (local-time:timestamp-hour   (local-time:now)))
        (m (local-time:timestamp-minute (local-time:now))))
    (+ (* 100 h) m)))

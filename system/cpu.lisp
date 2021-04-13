(in-package :jin-utils)

;;; CPU usage
;;; code taken from alezost

(defvar *last-user-time* 0)
(defvar *last-system-time* 0)
(defvar *last-idle-time* 0)
(defvar *last-iowait-time* 0)
(defvar *last-irq-time* 0)

(defun current-cpu-usage ()
  "Return the average CPU usage since the last call.
1st value is a total percent of CPU usage.
2nd value is percent of CPU time spent in user mode.
3rd value is percent of CPU time spent in system mode.
4th value is percent of CPU time spent waiting for IO to complete.
5th value is percent of CPU time spent servicing interrupts."
  (let ((cpu% 0)
        (user% 0)
        (system% 0)
        (io% 0)
        (irq% 0))
    (with-open-file (stat #P"/proc/stat" :direction :input)
      (read stat)               ; read the first "cpu" word
      (let* ((cur-user-time   (+ (read stat) (read stat)))
             (cur-system-time (read stat))
             (cur-idle-time   (read stat))
             (cur-iowait-time (read stat))
             (cur-irq-time    (+ (read stat) (read stat)))
             (user-time       (- cur-user-time   *last-user-time*))
             (system-time     (- cur-system-time *last-system-time*))
             (idle-time       (- cur-idle-time   *last-idle-time*))
             (iowait-time     (- cur-iowait-time *last-iowait-time*))
             (irq-time        (- cur-irq-time    *last-irq-time*))
             (cpu-time        (+ user-time system-time iowait-time irq-time))
             (total-time      (+ cpu-time idle-time)))
        (unless (zerop total-time)
          (setf cpu%    (/ cpu-time    total-time)
                user%   (/ user-time   total-time)
                system% (/ system-time total-time)
                io%     (/ iowait-time total-time)
                irq%    (/ irq-time    total-time)
                *last-user-time*   cur-user-time
                *last-system-time* cur-system-time
                *last-idle-time*   cur-idle-time
                *last-iowait-time* cur-iowait-time
                *last-irq-time*    cur-irq-time))))
    (values cpu% user% system% io% irq%)))

(defvar *acpi-thermal-zone*
  (let ((proc-dir (stumpwm:list-directory #P"/proc/acpi/thermal_zone/"))
        (sys-dir (sort
                  (remove-if-not
                   (lambda (x)
                     (when (cl-ppcre:scan "^.*/thermal_zone\\d+/" (namestring x))
                       x))
                   (stumpwm:list-directory #P"/sys/class/thermal/"))
                  #'string< :key #'namestring)))
    (cond
      (proc-dir
       (cons :procfs
             (make-pathname :directory (pathname-directory (first proc-dir))
                            :name "temperature")))
      (sys-dir
       (cons :sysfs
             (make-pathname :directory (pathname-directory (first sys-dir))
                            :name "temp"))))))

(defun fmt-cpu-temp ()
  "Returns a string representing the current CPU temperature."
  (format nil "~,1F°C"
          (case (car *acpi-thermal-zone*)
            (:procfs (parse-integer
                      (get-proc-file-field (cdr *acpi-thermal-zone*) "temperature")
                      :junk-allowed t))
            (:sysfs   (with-open-file (f (cdr *acpi-thermal-zone*))
                        (/ (read f) 1000))))))

(defun echo-cpu-state ()
  (format nil "(CPU ~a% ~a)"
          (ceiling (* 100 (current-cpu-usage)))
          (fmt-cpu-temp)))

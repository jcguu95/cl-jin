(in-package :jin.utils)

;; FIXME Currently providing two methods. Each of them has its
;; own problem.

(defun notify-send (title content &optional (expire-time 0))
  "Expire-time in ms."
  (let ((service (make-instance
                  'jin.service:service
                  :name "notify-send"
                  :action `(lambda ()
                             (sb-ext:run-program
                              (jin.utils:whereis "notify-send")
                              ',(list title content "-t"
                                      (format nil "~a" expire-time)))))))
    (jin.service:dispatch service)))

(defun notify-send-using-dbus (title content &key (expire-timeout 0))
  "Send notification using dbus. EXPIRE-TIMEOUT is an INT32 in
milisecond; 0 means infinite while (-1) means default."
  ;; FIXME Doesn't work the fly for my guix system.
  ;; taken from https://github.com/death/dbus/blob/master/examples/notify.lisp
  (dbus:with-open-bus (bus (dbus:session-server-addresses))
    (dbus:with-introspected-object
        (notifications bus "/org/freedesktop/Notifications"
                       "org.freedesktop.Notifications")
      (notifications "org.freedesktop.Notifications" "Notify"
                     "Test" 0 "" title content '() '() expire-timeout))))

;;; Usage
;; (notify-send-using-dbus "A" "B" :expire-timeout (-1))
;; (notify-send-using-dbus "A" "B" :expire-timeout 2000)

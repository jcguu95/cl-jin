(in-package :jin.utils)

;; (defun notify-send (title content &key (expire-time 0))
;;   "Example usage: (notify-send \"A\" \"B\" :expire-time 0.2222)."
;;   ;; FIXME This does not take care of dbus carefully.
;;   ;; FIXME Special characters are hard to escape.. deprecated.
;;   (uiop:run-program
;;    (format nil "notify-send \"~a\" \"~a\" -t ~a"
;;            title content (ceiling (* 1000 expire-time)))))
;; (notify-send "A" "B" :expire-time 2)
;; (notify-send "A" "B" :expire-time 0.55)

(defun notify-send (title content &key (expire-timeout 0))
  "Send notification using dmenu. EXPIRE-TIMEOUT is an INT32 in
milisecond; 0 means infinite while (-1) means default."
  ;; taken from https://github.com/death/dbus/blob/master/examples/notify.lisp
  (dbus:with-open-bus (bus (dbus:session-server-addresses))
    (dbus:with-introspected-object
        (notifications bus "/org/freedesktop/Notifications"
                       "org.freedesktop.Notifications")
      (notifications "org.freedesktop.Notifications" "Notify"
                     "Test" 0 "" title content '() '() expire-timeout))))

;; FIXME it doesn't work on the fly on my guix machine

;;; Usage
;; (notify-send "A" "B" :expire-timeout (-1))
;; (notify-send "A" "B" :expire-timeout 2000)

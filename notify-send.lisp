(in-package :jin)

(defun notify-send (title content &key (expire-time 0))
  "Example usage: (notify-send \"A\" \"B\" :expire-time 0.2222)."
  ;; FIXME This does not take care of dbus carefully.
  (uiop:run-program
   (format nil "notify-send \"~a\" \"~a\" -t ~a"
           title content (ceiling (* 1000 expire-time)))))

;; (notify-send "A" "B" :expire-time 2)
;; (notify-send "A" "B" :expire-time 0.55)



;;;; Deprecated cuz I dunno how to change expire time.
;;
;; (defun notify-send (title content)
;;   "Send notification using dmenu."
;;   ;; TODO Put this to another personal package.
;;   ;; taken from https://github.com/death/dbus/blob/master/examples/notify.lisp
;;   ;; TODO Add argument for controlling how long it shows.
;;   (dbus:with-open-bus (bus (dbus:session-server-addresses))
;;     (dbus:with-introspected-object
;;         (notifications bus "/org/freedesktop/Notifications"
;;                        "org.freedesktop.Notifications")
;;       (notifications "org.freedesktop.Notifications" "Notify"
;;                      "Test" 0 "" title content '() '() -1))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

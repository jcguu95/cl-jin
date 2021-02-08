(in-package :jin)

(defun notify-send (title content)
  "Send notification using dmenu."
  ;; TODO Put this to another personal package.
  ;; taken from https://github.com/death/dbus/blob/master/examples/notify.lisp
  ;; TODO Add argument for controlling how long it shows.
  (dbus:with-open-bus (bus (dbus:session-server-addresses))
    (dbus:with-introspected-object
        (notifications bus "/org/freedesktop/Notifications"
                       "org.freedesktop.Notifications")
      (notifications "org.freedesktop.Notifications" "Notify"
                     "Test" 0 "" title content '() '() -1))))

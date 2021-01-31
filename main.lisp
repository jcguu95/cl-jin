; finally I want to do this for so long
;  --> curfew in common lisp!

(defvar *url-list*
  '("www.youtube.com" "news.ycombinator.com"
    "www.reddit.com" "old.reddit.com"
    "www.facebook.com" "www.messenger.com" "www.instagram.com"))

(defun main ()
  "Main entry point."
  ;; FIXME remember that lock-url has a weird bug.
  (within=>do
   ((1730 1700) (mapcar #'lock-url *url-list*)))
  (within=>do
   ((1700 1730) (mapcar #'unlock-url *url-list*)))
  (within=>do
   ((1700 1900) (slock-all)))
  (within=>do
   ((1930 0500) (slock-all))))

(defmacro within=>do (pair)
  (let ((interval (nth 0 pair))
        (action (nth 1 pair)))
  ;; E.g. (within=>do ((1700 1900) (slock-all)))
  `(when (funcall (time-within ,@interval)
                  (now-in-int))
     ,action)))

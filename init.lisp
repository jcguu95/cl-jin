; finally I want to do this for so long
;  --> curfew in common lisp!

(defvar *url-list*
  '("www.youtube.com" "news.ycombinator.com"
    "www.reddit.com" "old.reddit.com"
    "www.facebook.com" "www.messenger.com"
    "www.instagram.com"))

(defun main ()
  "Main entry point."
  ;; TODO Write a macro that clears this mess up.
  (when (funcall (time-within (1700 1730))
                 (now-in-int))
    (mapcar #'unlock-url *url-list&))
  (when (funcall (time-within (1730 1700))
                 (now-in-int))
    (mapcar #'lock-url *url-list*))
  ;; FIXME remember that lock-url has a weird bug.

  (when (funcall (time-within (1700 1900))
                 (now-in-int))
    (slock-all))
  (when (funcall (time-within (1930 0500))
                 (now-in-int))
    (slock-all)))

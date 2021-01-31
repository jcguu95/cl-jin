(in-package :curfew)

(setf *url-list*
      '(
        "www.youtube.com" "news.ycombinator.com"
        "www.reddit.com" "old.reddit.com"
        "www.facebook.com" "www.messenger.com" "www.instagram.com"
        ))

(setf *instruction*
      '(
        ((1700 1900) (slock-all))
        ((1930 0500) (slock-all))

        ((1730 1700) (mapcar #'lock-url *url-list*))
        ((1700 1730) (mapcar #'unlock-url *url-list*))
        ))



(defun main ()
  "Main entry point."
  (mapcar #'within=>do *instruction*)
  (format t "Function #'main executed."))

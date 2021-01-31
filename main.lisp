; finally I want to do this for so long
;  --> curfew in common lisp!

(defvar *url-list*
  '("www.youtube.com" "news.ycombinator.com"
    "www.reddit.com" "old.reddit.com"
    "www.facebook.com" "www.messenger.com" "www.instagram.com"))

(defvar instruction*)
(setf *instruction*
  '(((1730 1700) (mapcar #'lock-url *url-list*))
    ;; FIXME remember that lock-url has a weird bug.
    ((1700 1730) (mapcar #'unlock-url *url-list*))
    ((1700 1900) (slock-all))
    ((1930 0500) (slock-all))))

(defun main ()
  "Main entry point."
  (mapcar #'within=>do *instruction*))

(main)

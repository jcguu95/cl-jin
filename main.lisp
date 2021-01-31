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
   '((1730 1700) (mapcar #'lock-url *url-list*)))
  (within=>do
   '((1700 1730) (mapcar #'unlock-url *url-list*)))
  (within=>do
   '((1700 1900) (slock-all)))
  (within=>do
   '((1930 0500) (slock-all))))

(defun within=>do (pair)
  (unless (eq (length pair) 2)
    (error "Input should be a list of length 2."))
  (let ((interval (car pair))
        (action (car (cdr pair))))
    (when (funcall (apply #'time-within interval)
                   (now-in-int))
      (eval action))))

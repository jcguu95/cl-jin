; finally I want to do this for so long
;  --> curfew in common lisp!

(ql:quickload :local-time)

;; aux-functions

(defun apply-functions (fs e)
  ;; test: (apply-functions (list #'not #'not) nil) => (t t)
  (loop for f in fs
        collect (funcall f e)))

(defun func-and (&rest funcs)
  "#'AND, but lifted to the functional level."
  (lambda (x) (eval (append '(and)
                            (apply-functions funcs x)))))

(defun func-or (&rest funcs)
  "#'OR, but lifted to the functional level."
  (lambda (x) (eval (append '(or)
                            (apply-functions funcs x)))))

(defun func-not (func)
  "#'NOT, but lifted to the functional level."
  (lambda (x) (not (funcall func x))))

;; time functions

(defun time-within (t0 t1)
  "Return a function: time \\mapsto (time \\in [t0,t1])."
  (lambda (time)
    (if (<= t0 t1)
        (<= t0 time t1)
        (or (<= t0 time)
            (>= t1 time)))))

(defun now-in-int ()
  "Return current time in a 4-digit integer. For example, 12:30pm
is represented by 1230, while 10:30pm is represented as 2230."
  (let ((h (local-time:timestamp-hour   (local-time:now)))
        (m (local-time:timestamp-minute (local-time:now))))
    (+ (* 100 h) m)))

;; file aux functions
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

;; action functions

;; kill app
;; TODO
;; slock user
;; TODO
;;
;; URL (un)lockers ;;
;; DONE

(defvar *dns-path* "/home/jin/hosts")
;(defvar *dns-path* "/etc/hosts")

(defun lock-url (url)
  "In DNS-FILE, do the following. If there is DECO-URL, then
mention that it's locked already by this program. If there is not
DECO-URL but there is DECO#URL, uncomment the first DECO#URL, and
mention the change. If there are no DECO-URL nor DECO#URL, append
DECO-URL to *DNS-PATH*, and mention the change.

_Remark_ This function may require root privilege to run.

_Remark_ This function depends on the global variable *DNS-PATH*."
  (let* ((deco-url (format nil "127.0.0.1 ~a  # curfew.d" url))
         (deco#url (format nil "#127.0.0.1 ~a  # curfew.d" url))
         (dns-file (get-file *dns-path*)))
    (if (find deco-url dns-file :test #'equal)
        (format t "URL locked already by this program: \"~a\"." url)
        (let ((result (if (find deco#url dns-file :test #'equal)
                          (substitute deco-url deco#url
                                      dns-file    :test #'equal)
                          (append dns-file (list deco-url)))))
          ;; mention it, and write result to file.
          (with-open-file (stream *dns-path*
                                  :direction :output
                                  :if-exists :overwrite)
            (format stream "~{~a~^~%~}" result))
          (format t "Locking URL: \"~a\"." url)))))

(defun unlock-url (url)
  "In DNS-FILE, when there is DECO-URL, uncomment the URL and
mention it. Otherwise, mention that it is not locked by this
program.

_Remark_ This function may require root privilege to run.

_Remark_ This function depends on the global variable *DNS-PATH*."
  (let* ((deco-url (format nil "127.0.0.1 ~a  # curfew.d" url))
         (deco#url (format nil "#127.0.0.1 ~a  # curfew.d" url))
         (dns-file (get-file *dns-path*)))
    (if (find deco-url dns-file :test #'equal)
      (let ((result (substitute deco#url deco-url
                                dns-file :test #'equal)))
        ;; write result to file, and mention it.
        (with-open-file (stream *dns-path*
                                :direction :output
                                :if-exists :overwrite)
          (format stream "~{~a~^~%~}" result))

          (format t "Unlocking URL: \"~a\"." url))

      (format t "URL not locked by this program: \"~a\"." url))))

;; ---
;;
;; (pred-1 action-1)

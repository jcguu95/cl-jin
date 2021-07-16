(in-package :jin.curfew)

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

;; file aux functions

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

;; instruction handling

(defun within=>do (pair)
  (unless (eq (length pair) 2)
    (error "Input should be a list of length 2."))
  (let ((interval (car pair))
        (action (car (cdr pair))))
    (when (funcall (apply #'time-within interval)
                   (now-in-int))
      (eval action))))

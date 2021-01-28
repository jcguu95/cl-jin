; finally I want to do this for so long
;  --> curfew in common lisp!

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

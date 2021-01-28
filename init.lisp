; finally I want to do this for so long
;  --> curfew in common lisp!

;; aux-functions

(defun apply-functions (fs e)
  ;; test: (apply-functions (list #'not #'not) nil) => (t t)
  (loop for f in fs
        collect (funcall f e)))

(defun func-and (&rest funcs)
  "#'AND, but lifted to the functional level."
  ;; (funcall (func-and #'id #'id #'id) t)
  ;; (funcall (func-and #'id #'id #'id) nil)
  ;; (funcall (func-and #'id #'id #'not) nil)
  ;; (funcall (func-and #'not #'not #'not) nil)
  (lambda (x) (eval (append '(and) (apply-functions funcs x)))))
;; TODO still wrong, as (apply-functions aren't evaluated upon
;; the macro is called.)

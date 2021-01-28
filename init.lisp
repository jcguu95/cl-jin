; finally I want to do this for so long
;  --> curfew in common lisp!

;; aux-functions

(defun apply-functions (fs e)
  ;; test: (apply-functions (list #'not #'not) nil) => (t t)
  (loop for f in fs
        collect (funcall f e)))

(defmacro apply-macro (m es)
  ;; Try (macroexpand-1 '(apply-macro and (t t nil)))
  (append (list m) es))

(defun func-and (&rest funcs)
  "#'AND, but lifted to the functional level."
  (lambda (x) (apply-macro and (apply-functions funcs x))))
;; TODO still wrong, as (apply-functions aren't evaluated upon
;; the macro is called.)

(funcall (func-and #'not #'not) nil)

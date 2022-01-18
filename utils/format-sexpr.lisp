(in-package :jin.utils)

;;;; Doesn't support hash-table yet. To print hash-table
;;;; readably, refer to:
;;;; https://lispcookbook.github.io/cl-cookbook/data-structures.html

(defun make-spaces (count)
  "Make a string of COUNT many spaces."
  (assert (integerp count))
  (if (zerop count)
      ""
      (concatenate 'string " " (make-spaces (- count 1)))))

(defun indent (n string)
  (format nil "~a~a"
          (make-spaces n)
          (cl-ppcre:regex-replace-all
           "\\n"
           string
           (concatenate 'string (format nil "~%") (make-spaces n)))))

(defun adhoc-lint (s)
  "Removed unnecessary space characters around parentheses."
  (cl-ppcre:regex-replace-all
   "[ \\n]*\\)"
   (cl-ppcre:regex-replace-all
    "\\([ \\n]*" s "(")
   ")"))

(defun pure-cons-p (list)
  (and (listp list)
       (not (null (cdr list)))
       (atom (cdr list))))

;; Main function
(defun format-sexpr (sexpr)
  "Format SEXPR."
  (adhoc-lint (if (or (atom sexpr)
                      (pure-cons-p sexpr))
                  (format nil "~s" sexpr)
                  (eval `(concatenate 'string
                                      (format nil "(~%")
                                      ,@(loop for term in sexpr
                                              collect (format
                                                       nil
                                                       "~a~%"
                                                       (indent 1 (format-sexpr term))))
                                      ")")))))

;;; Tests

(setf fiveam:*run-test-when-defined* t)
(fiveam:test format-sexpr-test
  (fiveam:is
   (equal (make-spaces 3) "   "))
  (fiveam:is
   (equal (format nil "   a~%   b~%   c")
          (indent 3 (format nil "a~%b~%c"))))
  (fiveam:is
   (and (eq t (pure-cons-p '(a . b)))
        (eq nil (pure-cons-p '(a b)))
        (eq nil (pure-cons-p '(a)))
        (eq nil (pure-cons-p 'a))))
  (fiveam:is
   (equal "(((3)))"
          (adhoc-lint (format nil "(    ( ( 3 )~% )  )"))))

  (fiveam:is
   (equal (format nil "~%~a"
                  (format-sexpr '(1 2 (3 4 (5 6 (7 (8 9) 10 (11 12))))
                                  (13 (14 (15 16 (17 :a)))))))
          "
(1
 2
 (3
  4
  (5
   6
   (7
    (8
     9)
    10
    (11
     12))))
 (13
  (14
   (15
    16
    (17
     :A)))))"))
  (fiveam:is
   (equal
    (format-sexpr '(defun fibo (n)
                    (if (< n 2)
                        1
                        (+ (fibo (- n 1))
                           (fibo (- n 2))))))
    "(DEFUN
 FIBO
 (N)
 (IF
  (<
   N
   2)
  1
  (+
   (FIBO
    (-
     N
     1))
   (FIBO
    (-
     N
     2)))))")))

;;;

(defun get-sexpr-from-file (file)
  (ignore-errors
   (read-from-string
    (uiop:read-file-string
     (ensure-directories-exist file)
     :if-does-not-exist :create))))

(defun write-sexpr-to-file (file sexpr)
  (with-open-file
      (o (ensure-directories-exist file)
         :direction :output
         :if-exists :supersede
         :if-does-not-exist :create)
    (format o (format-sexpr sexpr))
    (format o "~%~%")))

(defun write-sexpr (args sexpr)
  (write-sexpr-to-file (format nil "~{~a~^/~}" args) sexpr))

(defun get-sexpr (args)
  (get-sexpr-from-file (format nil "~{~a~^/~}" args)))

(defsetf get-sexpr write-sexpr)

;;; Playground

;; (setf (access:accesses (get-sexpr (list "/tmp/sexpr" 2021 10 31))
;;                        :main-note :content)
;;       "Today is Xiling's birthday :)")

;; (defvar *root* "/tmp/sexpr")
;;
;; (defun date<-ts (ts)
;;   (list (subseq ts 0 4)
;;         (subseq ts 4 6)
;;         (subseq ts 6 8)))
;;
;; (loop for file in (uiop:directory-files "~/data/storage/+org/wiki/fleeting")
;;       do (symbol-macrolet
;;              ((files (access:accesses
;;                       (get-sexpr (eval `(list ,*root*
;;                                               ,@(date<-ts (pathname-name file)))))
;;                       :files)))
;;            (unless (member file files :test #'equalp)
;;              (setf files (cons file files)))))

;; (loop for file in (uiop:directory-files "~/data/storage/+TO-ORGANIZE/scanned")
;;       do (symbol-macrolet
;;              ((files (access:accesses
;;                       (get-sexpr (eval `(list ,*root*
;;                                               ,@(date<-ts (pathname-name file)))))
;;                       :files)))
;;            (unless (member file files :test #'equalp)
;;              (setf files (cons file files)))))

;; FIXME Kinda Leaky: The following gives an error.
;;
;; (setf (get-sexpr (list "/tmp/sexpr" 1)) 3)
;; (setf (access:accesses
;;        (get-sexpr (list "/tmp/sexpr" 1)) :a)
;;       "Whatever.")
;;
;; It seems to be a problem of the function ACCESSES. In
;; particular, the following easier example also gives an error.
;;
;; (setf *X* '((:a 1)))
;; (setf (access:accesses (nth 0 *X*) :a :b) 10) ; => #<error>
;;
;; The function ACCESSES does not seem to expand nested
;; setf-expandable forms well.

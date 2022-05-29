(in-package :mumu-normalizer)

(defconstant +m++ 'm+)
(defconstant +m-+ 'm-)
(defconstant +p++ 'p+)
(defconstant +p-+ 'p-)

(defun pairs (list)
  (loop :for (a b) :on list :while b
        :collect (list a b)))

(defun positions (x list &optional (shift 0))
  (let ((pos (position x list)))
    (if (and list pos)
        (cons (+ pos shift)
              (positions x (subseq list (1+ pos)) (+ 1 pos shift)))
        nil)))

(defun permute-at (n list)
  "Return a list with the nth and the (n+1)th element permuted."
  (if (<= 0 n (- (length list) 2))
      (let (result)
        (setf result
              (concatenate 'list
                           (subseq list 0 n)
                           (list (nth (1+ n) list)
                                 (nth n list))
                           (subseq list (+ n 2)))))
      (error "N is not in the correct range.")))

(defun replace-with (list n x)
  "Return a list with the nth element replaced with x."
  (if (<= 0 n (1- (length list)))
      (let (result)
        (setf result
              (concatenate 'list
                           (subseq list 0 n)
                           (list x)
                           (subseq list (1+ n)))))
      (error "N is not in the correct range.")))

(defun const>= (x y)
  "P+ > P- > M- > M+"
  (let ((list (reverse (list +P++ +P-+ +M-+ +M++))))
    (>= (position x list) (position y list))))

(const>= +P++ +P-+)                     ;; t
(const>= +P-+ +P++)                     ;; nil

(defun const> (x y)
  "P+ > P- > M- > M+"
  (let* ((list (reverse (list +P++ +P-+ +M-+ +M++)))
         (px (position x list)) (py (position y list)))
    (when (and px py)
      (> px py))))

(defun list>= (xs ys)
  (cond ((null ys)
         t)
        ((equal (car xs) (car ys))
         (list>= (cdr xs) (cdr ys)))
        ((const> (car xs) (car ys))
         t)))

(list>= (list +P-+ +P-+) (list +P++ +M++)) ; nil
(list>= (list +P++ +P-+) (list +P++ +M++)) ; t
(list>= (list +P++ +P++) (list +P++ +M++)) ; t

(defun list> (xs ys)
  (cond ((null xs)
         nil)
        ((equal (car xs) (car ys))
         (list> (cdr xs) (cdr ys)))
        ((const> (car xs) (car ys))
         t)))

(list> (list +P++ +P++) (list +P++ +P++)) ; nil

(defun make-basic-summand (n ad-expr)
  (list n ad-expr))

(defun coeff (basic-summand)
  (car basic-summand))

(defun ad-expr (basic-summand)
  (car (cdr basic-summand)))

(defun +- (&rest lcs-ad-exprs)
  "Add up the linear combinations of adjoint expressions."
  (if lcs-ad-exprs
      (concatenate 'list (car lcs-ad-exprs) (apply #'+- (cdr lcs-ad-exprs)))
      (list (make-basic-summand 0 '()))))

(defun *- (n lc-ad-exprs)
  "Scalar product of n and the linear combination of adjoint
expression."
  (loop for basic-summand in lc-ad-exprs
        collect (make-basic-summand (* n (coeff basic-summand))
                                    (ad-expr basic-summand))))

(*- 8 (+- (make-basic-summand 2 (list +m++ +m-+))
          (make-basic-summand 3 (list +m++ +m-+))))

(defun perform-jacobi-identity-at (n ad-expr)
  "Perform Jacobi identity at the nth element of the adjoint
expression, and return the result as a linear combination of
adjoint expressions. Only m+ and m- are supported now."
  (assert (<= 0 n (- (length ad-expr) 2)))
  (let ((this (nth n ad-expr))
        (next (nth (1+ n) ad-expr))
        result)
    (cond ((equal this +m++)
           (cond
             ((equal next +m++)
              (setf result (list (make-basic-summand 1 nil))))
             ((equal next +p++)
              (setf result (list (make-basic-summand 1 (permute-at n ad-expr)))))
             ((equal next +p-+)
              (setf result (list (make-basic-summand -1 (permute-at n ad-expr))
                                 (make-basic-summand 2 (replace-with (replace-with ad-expr n +p++)
                                                                     (1+ n) +p++)))))
             ((equal next +m-+)
              (setf result (list (make-basic-summand -1 (permute-at n ad-expr))
                                 (make-basic-summand -1 (replace-with (replace-with ad-expr n +p++)
                                                                      (1+ n) +p-+))
                                 (make-basic-summand -1 (replace-with (replace-with ad-expr n +p-+)
                                                                      (1+ n) +p++)))))))
          ((equal this +m-+)
           (cond ((equal next +p-+)
                  (setf result (list (make-basic-summand 1 (permute-at n ad-expr)))))
                 ((equal next +p++)
                  (setf result (list (make-basic-summand -1 (permute-at n ad-expr))
                                     (make-basic-summand 2 (replace-with (replace-with ad-expr n +p-+)
                                                                         (1+ n) +p-+)))))
                 ((equal next +m++)
                  (setf result (list (make-basic-summand -1 (permute-at n ad-expr))
                                     (make-basic-summand -1 (replace-with (replace-with ad-expr n +p-+)
                                                                          (1+ n) +p++))
                                     (make-basic-summand -1 (replace-with (replace-with ad-expr n +p++)
                                                                          (1+ n) +p-+)))))
                 ((equal next +m-+)
                  (setf result (list (make-basic-summand 1 nil))))))
          (t (error "Only m+ and m- are supported now.")))
    result))

(perform-jacobi-identity-at 1 (list +m++ +m-+ +m++))
(perform-jacobi-identity-at 0 (list +m-+ +m++ +m++))
(perform-jacobi-identity-at 1 (list +p++ +m-+ +m++))

(mumu-normalization--ad-expr (list +p-+ +m++ +m++))
(mumu-normalization--ad-expr (list +p-+ +m++ +m-+))
;; (perform-jacobi-identity-at 1 (list +p-+ +m++ +m-+))
(mumu-normalization--ad-expr (list +p++ +m-+ +m++))

;; An adjoint expression is something in the form [a[b[c[d[ef]]]]].
;; TODO mumu-normalization should be defined in the doc soon.
(defun mumu-normalization--ad-expr (ad-expr)
  "Take an adjoint expression, mumu-normalize it, and return a
linear combination of adjoint expression."
  (let (result)
    (when (null ad-expr)
      (setf result (list (make-basic-summand 0 nil))))
    (loop for pair in (pairs ad-expr)
          do (when (or (equal pair (list +m++ +m++))
                       (equal pair (list +m-+ +m-+)))
               (setf result (list (list 0 nil)))))
    (unless result
      (let* ((pos-m+ (positions +m++ ad-expr))
             (pos-m- (positions +m-+ ad-expr))
             (len-m+ (length pos-m+))
             (len-m- (length pos-m-)))
        (cond
          ;; Suppose there's no m+ nor m-.
          ((equal (list len-m+ len-m-) (list 0 0))
           (setf result (list (make-basic-summand 1 ad-expr))))
          ;; Suppose there's no m+ but there are some m-.
          ((and (= len-m+ 0) (> len-m- 0))
           (cond
             ;; Suppose there's only one m-.
             ((= len-m- 1)
              (if (equal +m-+ (car (last ad-expr)))
                  (setf result (list (make-basic-summand 1 ad-expr)))
                  (setf result (perform-jacobi-identity-at (car pos-m-) ad-expr))))
             ;; Suppose there are multiple m-.
             ((> len-m- 1)
              (if (equal +m-+ (car (last ad-expr)))
                  (setf result (perform-jacobi-identity-at (cadr (reverse pos-m-)) ad-expr))
                  (setf result (perform-jacobi-identity-at (car (reverse pos-m-)) ad-expr))))))
          ((= len-m+ 1)
           (if
            ;; If the only m+ is at the last position,
            (equal (car (reverse pos-m+)) (1- (length ad-expr)))
            ;; then go to the m- branch:
            (cond
              ;; Suppose there's only one m-:
              ((= len-m- 1)
               (if
                ;; If the m- is at the second last,
                (equal (- (length ad-expr) 2)
                       (car pos-m-))
                ;; then return the original stuff as a linear
                ;; combination.
                (setf result (list (make-basic-summand 1 ad-expr)))
                ;; Otherwise, perform Jacobi at that only m-.
                (setf result (perform-jacobi-identity-at (car pos-m-) ad-expr))))
              ;; Suppose there are more than one m-:
              ((>= len-m- 2)
               (if
                ;; If the second last element is m-,
                (equal +m-+ (nth (- (length ad-expr) 2) ad-expr))
                ;; then perform Jacobi at the second last m-.
                (setf result (perform-jacobi-identity-at (nth 1 (reverse pos-m-)) ad-expr))
                ;; Otherwise, perform Jacobi at the last m-.
                (setf result (perform-jacobi-identity-at (nth 0 (reverse pos-m-)) ad-expr)))))
            ;; Otherwise, then perform Jacobi identity at m+:
            (setf result (perform-jacobi-identity-at (car (reverse pos-m+)) ad-expr))))
          ;; Suppose there're more than one m+.
          ((>= len-m+ 2)
           (if
            ;; If the last one is m+,
            (equal +m++ (car (last ad-expr)))
            ;; then perform Jacobi at the second last m+.
            (setf result (perform-jacobi-identity-at (nth (- len-m+ 2) pos-m+) ad-expr))
            ;; Otherwise, perform Jacobi at the last m+.
            (setf result (perform-jacobi-identity-at (nth (- len-m+ 1) pos-m+) ad-expr)))))))
    result))

;; TODO tests
(mumu-normalization--ad-expr (list +p++ +m++ +m++ +p-+)) ;; test it should be: ((0 NIL))

(mumu-normalization--ad-expr (list +m++ +m++))           ;; test it should be: ((0 NIL))
(mumu-normalization--ad-expr (list +m++ +m-+))           ;; should be: ((-1 (M- M+)) (-1 (P+ P-)) (-1 (P- P+)))
(mumu-normalization--ad-expr (list +m++ +p++))           ;; should be: ((1 (P+ M+)))
(mumu-normalization--ad-expr (list +m++ +p-+))           ;; should be: ((-1 (P- M+)) (2 (P+ P+))

(mumu-normalization--ad-expr (list +m-+ +m-+))           ;; should be: ((0 NIL))
(mumu-normalization--ad-expr (list +m-+ +m++))           ;; should be: ((1 (M- M+)))
(mumu-normalization--ad-expr (list +m-+ +p++))           ;; should be: ((-1 (P+ M+)) (2 (P- P-))
(mumu-normalization--ad-expr (list +m-+ +p-+))           ;; should be: ((1 (P- M-)))

;; A basic summand is a linear combination of adjoint expressions
;; with one summand.
(defun mumu-normalization--basic-summand (basic-summand)
  "Take a basic-summand, mumu-normalize it, and return a linear
combination of adjoint expression."
  (*- (coeff basic-summand)
      (mumu-normalization--ad-expr (ad-expr basic-summand))))

;; test
(mumu-normalization--basic-summand
 (make-basic-summand 3 (list +m++ +p++))) ;; should be ((3 (P+ M+)))

(defun mumu-normalization--lc-ad-expr (lc-ad-exprs)
  "Sort along the adjoint expressions, merge the coefficients if
possible, discard the zero terms. Loop for each basic summand and
normalize. Return the sum."
  (let ((result lc-ad-exprs))
    (setf result (sort result #'list>))
    ;; TODO merge coeff
    ;; TODO Discard zero terms
    (setf result (loop for basic-summand in result
                       ;; do (push (mumu-normalization--basic-summand basic-summand) *test*) ; TODO remove test
                       collect (mumu-normalization--basic-summand basic-summand)))
    (apply #'+- result)))

(defun mumu-normalization (lc-ad-exprs &optional (times 10))
  (let ((result lc-ad-exprs))
    (dotimes (i times)
      (setf result (mumu-normalization--lc-ad-expr result)))
    result))

(defun mumu-normalized-p (ad-expr)
  (let* ((pos-m+ (positions +m++ ad-expr))
         (pos-m- (positions +m-+ ad-expr))
         (len-m+ (length pos-m+))
         (len-m- (length pos-m-)))
    (cond ((= len-m+ 0)
           (cond ((= len-m- 0) t)
                 ((= len-m- 1) (= (car pos-m-) (1- (length ad-expr))))
                 ((> len-m- 1) nil)))
          ((= len-m+ 1)
           (= (car pos-m+) (1- (length ad-expr))))
          ((> len-m+ 1) nil))))

(mumu-normalization--lc-ad-expr (list (make-basic-summand 1 (list +m++ +m-+))))

(mumu-normalization--ad-expr (list +m++ +m-+ +m++)) ; FIXME wrong; should be 0
(mumu-normalization--lc-ad-expr (list (make-basic-summand 1 (list +m++ +m-+ +m++)))) ; FIXME wrong should be 0

(mumu-normalization (list (make-basic-summand 1 (list +m++ +m-+ +m++)))) ; FIXME wrong should be 0
                                        ;
(mumu-normalization--lc-ad-expr (list (make-basic-summand 1 (list +m-+ +m++))))
(mumu-normalization--lc-ad-expr (list (make-basic-summand 1 (list +m++ +p-+))))
(mumu-normalization--lc-ad-expr (list (make-basic-summand 1 (list +m-+ +m++ +m-+ +m++))))

(mumu-normalization--lc-ad-expr
 (list
  (make-basic-summand 1 (list +m-+ +m++ +m-+ +m++))
  (make-basic-summand 1 (list +m-+ +m++ +m-+ +m++))))

(apply #'+-
       (list
        (list
         (make-basic-summand 1 (list +m-+ +m++ +m-+ +m++))
         (make-basic-summand 1 (list +m-+ +m++ +m-+ +m++)))
        (list
         (make-basic-summand 1 (list +m-+ +m++ +m-+ +m++))
         (make-basic-summand 1 (list +m-+ +m++ +m-+ +m++))))
       )

(sort (mumu-normalization--lc-ad-expr (list (make-basic-summand 1 (list +m-+ +m++ +m-+ +m++))))
      #'list>)
(setf *test* nil)
(mumu-normalization (list (make-basic-summand 1 (list +m-+ +m++ +m-+ +m++))) 4)
(mumu-normalization (list (make-basic-summand 1 (list +m-+ +p++ +p-+ +m++))) 2)
(mumu-normalization (list (make-basic-summand 1 (list +m-+ +p-+ +p++ +m++))) 2)

(mumu-normalization (list (make-basic-summand 1 (list +p-+ +p++ +m-+ +m++))) 1)

(mumu-normalization (list
                     (make-basic-summand -1 (list +m-+ +m++ +m-+ +m++))
                     (make-basic-summand -1 (list +m++ +m-+ +m-+ +m++)))
                    5)                  ;GOOD, but this is one of
                                        ;the special relation and
                                        ;should therefore be zero
                                        ;(not yet implemented).

(mumu-normalization (list
                     (make-basic-summand 1 (list +p-+ +m-+ +p-+ +m-+ +m++)))
                    1)

(mumu-normalization (list
                     (make-basic-summand 1 (list +m++ +m-+ +p++ +m-+ +m++)))
                    10)

(mumu-normalization (list
                     (make-basic-summand 1 (list +m++ +m-+ +p++ +m++)))
                    1)

(mumu-normalization (list
                     (make-basic-summand 1 (list +m-+ +p++ +p-+ +m-+ +m++)))
                    3)

(mumu-normalization (list
                     (make-basic-summand 1 (list +m++ +m-+ +p-+ +m++)))
                    4)

(mumu-normalization (list
                     (make-basic-summand 1 (list +p-+ +m++ +p-+ +p++ +p++)))
                    3)

(mumu-normalization (list
                     (make-basic-summand 1 (list +m++ +p-+ +p-+ +m-+ +m++))
                     (make-basic-summand 1 (list +p-+ +m++ +p-+ +m-+ +m++)))
                    10)

;; FIXME seems wrong.. souldn't be zero
(mumu-normalization (list
                     (make-basic-summand 1 (list +p++ +p-+ +p++ +m++)))
                    1)

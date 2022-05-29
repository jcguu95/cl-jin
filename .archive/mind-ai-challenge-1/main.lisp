(defvar *union-find*)

;;; Data structure
(defclass union-find ()
  ((content :initarg :content
            :accessor content)
   (size :initarg :size
         :accessor size)))

(defun make-union-find (size)
  "Make a union-find data structure of positive integers with given
SIZE, with each number pointing to itself."
  (assert (and (integerp size) (> size 0)))
  (make-instance 'union-find
                 :content (apply #'vector (loop for n from 0 to (1- size) collect n))
                 :size size))

(defun -find (n)
  "Return the root of the Nth element of the *UNION-FIND*."
  (let ((pointed (elt (content *union-find*) n)))
    (if (= pointed (elt (content *union-find*) pointed)) ; stable
        pointed
        (-find pointed))))

(defun -union (n m)
  "Union the Nth and the Mth element in the *UNION-FIND*."
  (let ((size (size *union-find*)))
    (setf (elt (content *union-find*) (-find m)) (-find n))
    (setf (elt (content *union-find*) m) (-find n))))

(defun solve (height width content)
  ;; Initialize data.
  (setf *union-find* (make-union-find (* height width)))
  ;; Define local functions for converting the position (a,b) to
  ;; its order (a*width+b), and vice versa.
  (flet ((ord<-pos (a b) (+ (* a width) b))
         (pos<-ord (n) (cons (floor (/ n width)) (mod n width)))
         (safe-1- (n) (if (>= n 1) (1- n) n)))
    ;; Traverse through the content and apply #'union to
    ;; admissible pairs.
    (loop for n from 0 to (1- height) do
      (loop for m from 0 to (1- width) do
        (if (zerop (aref content n m))
            (setf (elt (content *union-find*) (ord<-pos n m)) -1)
            (progn (unless (zerop (aref content (safe-1- n) m))
                     (-union (ord<-pos (safe-1- n) m) (ord<-pos n m)))
                   (unless (zerop (aref content n (safe-1- m)))
                     (-union (ord<-pos n (safe-1- m)) (ord<-pos n m)))))))
    ;; Finally, collect the volume to the group.
    (let ((result (make-hash-table)))
      (dotimes (i (* height width))
        (let ((next (elt (content *union-find*) i))
              (n (car (pos<-ord i)))
              (m (cdr (pos<-ord i))))
          (when (>= next 0)
            (setf (gethash (-find next) result)
                  (+ (or (gethash (-find next) result) 0)
                     (aref content n m))))))
      (loop for value being the hash-values of result
            collect value))))

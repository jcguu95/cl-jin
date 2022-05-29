(defvar +example-1+
  (make-array '(10 10)
              :initial-contents
              '((0 5 5 7 0 0 0 0 0 0)
                (0 1 8 8 0 0 0 0 0 0)
                (0 3 2 2 0 0 2 8 4 0)
                (0 0 0 0 0 0 8 8 3 0)
                (0 0 0 0 0 0 8 7 8 0)
                (8 5 2 0 0 0 0 0 0 0)
                (2 7 3 0 0 0 0 0 0 0)
                (3 5 1 0 0 7 5 8 0 0)
                (0 0 0 0 0 1 1 7 0 0)
                (0 0 0 0 0 8 3 5 0 0))))

(defvar +example-2+
  (make-array '(10 10)
              :initial-contents
              '((0 1 1 1 0 0 0 0 0 0)
                (0 0 0 1 0 0 0 0 0 0)
                (0 0 1 1 0 0 2 2 2 0)
                (0 0 1 0 0 0 2 0 2 0)
                (0 1 1 1 1 0 2 2 2 0)
                (1 1 0 0 0 0 0 2 0 0)
                (0 1 0 2 2 2 0 2 0 0)
                (1 1 0 0 0 2 2 2 0 0)
                (0 1 1 1 0 0 2 0 0 0)
                (0 0 0 0 0 2 2 2 0 0))))

(defvar +example-3+
  (make-array '(10 10)
              :initial-contents
              '((0 1 1 1 0 0 0 0 0 0)
                (0 1 1 1 0 0 0 0 0 0)
                (0 1 1 1 1 1 1 1 1 0)
                (0 0 0 0 0 0 1 1 1 0)
                (0 0 0 0 0 0 1 1 1 0)
                (2 2 2 0 0 0 0 0 0 0)
                (2 2 2 0 0 0 0 0 0 0)
                (2 2 2 0 0 4 4 4 0 0)
                (0 0 0 3 0 4 4 4 0 0)
                (0 0 0 0 4 4 4 4 0 0))))

(defvar +example-4+
  (make-array '(3 5)
              :initial-contents
              '((0 1 0 1 0)
                (1 0 1 0 1)
                (0 1 0 1 0))))

(defvar +example-5+
  (make-array '(2 3)
              :initial-contents
              '((2 0 7)
                (0 5 1))))

(assert (and (equal (solve 10 10 +example-1+) '(41 56 36 45) )
             (equal (solve 10 10 +example-2+) '(19 40))
             (equal (solve 10 10 +example-3+) '(20 18 40 3))
             (equal (solve  3  5 +example-4+) '(1 1 1 1 1 1 1))
             (equal (solve  2  3 +example-5+) '(2 13))))

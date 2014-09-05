(defpackage "utilities"
  (:export "one-of" "n-of" "one-of-with-index" "set-nth"))

(defun random-elt (choices)
  (elt choices (random (length choices))))

(defun one-of (set)
  (random-elt set))

(defun n-of (size set)
  (if (= 0 size)
      nil
    (cons (one-of set) (n-of (- size 1) set))))

(defun one-of-with-index (set)
  (let ((rand-ind (random (length set))))
    (cons (elt set rand-ind) (cons rand-ind nil))))

(defun set-nth (list n value)
  (fill (copy-seq list) value :start n :end (1+ n)))

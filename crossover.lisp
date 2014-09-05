(defpackage "crossover"
  (:export "crossover"))

(load "utilities")

;; Crossover reproduction
(defun rand-code-point (code)
  (if (atom code) 
      (cons nil (cons code nil))
      (if (= 0 (random 10))
	  (cons nil (cons code nil))
	(let* ((next-code-pt-w-ind (one-of-with-index code))
	       (next-code (car next-code-pt-w-ind))
	       (next-index (cadr next-code-pt-w-ind))
	       (rand-code-and-path (rand-code-point next-code))
	       (rand-code (cadr rand-code-and-path))
	       (path-to-code (car rand-code-and-path))
	       (full-path (cons next-index path-to-code)))
	  (cons full-path (cons rand-code nil))))))

;; Slow and inefficient, but will do for now
(defun code-point-with-type (expr-type desired-type code)
  (let* ((prospective-point-and-path (rand-code-point code))
	 (prospective-point (cadr prospective-point-and-path))
	 (prospective-type (funcall expr-type prospective-point)))
    (if (eq desired-type prospective-type)
	prospective-point-and-path
      (code-point-with-type expr-type desired-type code))))

(defun swap-point (parent-path parent new-point)
  (if (null parent-path)
      new-point
    (set-nth parent (car parent-path) (swap-point (cdr parent-path) (nth (car parent-path) parent) new-point))))

(defun swap-code-points (mother-path mother-point mother father-path father-point father)
  (let ((mother-child (swap-point mother-path mother father-point))
	(father-child (swap-point father-path father mother-point)))
    (cons mother-child (cons father-child nil))))

(defun crossover (expr-type contains-type mother father)
  (let* ((mother-point-and-path (rand-code-point mother))
	 (mother-point (cadr mother-point-and-path))
	 (mother-path (car mother-point-and-path))
	 (mother-type (funcall expr-type mother-point)))
    (if (funcall contains-type mother-type father)
	(let* ((father-point-and-path (code-point-with-type expr-type (funcall expr-type mother-point) father))
	       (father-point (cadr father-point-and-path))
	       (father-path (car father-point-and-path)))
	  (swap-code-points mother-path mother-point mother father-path father-point father))
      (crossover expr-type contains-type mother father))))


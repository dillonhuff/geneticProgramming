(defpackage "population"
  (:export "create-generation-0" "next-generation"))

(defstruct generation
  population
  population-fitness
  population-reproduction-info
  building-blocks
  building-block-fitness)

(defun get-population (gen)
  (generation-population gen))

(defun get-pop-fitness (gen)
  (generation-population-fitness gen))

(defun get-pop-reproduction-info (gen)
  (generation-population-reproduction-info gen))

(defun get-building-blocks (gen)
  (generation-building-blocks gen))

(defun get-block-fitness (gen)
  (generation-building-block-fitness gen))

(defstruct reproduction-info
  method
  parents
  parent-code-point-locations)

;; Population generation
(defun init-generation (pop fitness-scores)
  (make-generation :population pop :population-fitness fitness-scores))

(defun new-population (pop-size generating-func)
  (if (= 0 pop-size)
      nil
    (cons (funcall generating-func) (new-population (- pop-size 1) generating-func))))

(defun create-generation-0 (size initial-solution-generator pop-fitness-eval)
  (let* ((gen-0 (new-population size initial-solution-generator))
	 (gen-0-fitness (funcall pop-fitness-eval gen-0)))
    (init-generation gen-0 gen-0-fitness)))

(defun next-generation (cur-gen eval-func build-pop-func)
  (let* ((next-gen-pop (funcall build-pop-func (get-population cur-gen) (get-pop-fitness cur-gen)))
	 (next-gen-fitness (funcall eval-func next-gen-pop)))
    (init-generation next-gen-pop next-gen-fitness)))

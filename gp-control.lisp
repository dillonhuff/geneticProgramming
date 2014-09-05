(defpackage "gp-control"
  (:export "run-gp"))


(defun rec-run-gp (num-iters-left cur-generation eval-func build-pop-func)
  (if (= 0 num-iters-left)
      cur-generation
    (rec-run-gp (- num-iters-left 1) (next-generation cur-generation eval-func build-pop-func) eval-func build-pop-func)))

(defun run-gp (pop-size init-sol-func eval-func build-new-pop-func num-iters)
  (let ((init-pop (create-generation-0 pop-size init-sol-func eval-func)))
    (rec-run-gp num-iters init-pop eval-func build-new-pop-func)))

;;================================================
;; tenjo-sample.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :tenjo)

(tenjo:init-bench "sample.log")

(defun fib (n)
  "Simple recursive Fibonacci number function"
  (declare (fixnum n))
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(defun fact (n)
  (declare (number n))
  (if (= n 1) 1
      (* n (fact (1- n)))))

(tenjo:defbench fib 100
  (fib 32))

(tenjo:defbench fact 100
  (fact 4000))

(tenjo:run-all-bench)

(tenjo:dump-result)

;;================================================
;; tenjo.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(defpackage :tenjo
  (:use :common-lisp :chimi)
  (:export #:defbench
           #:init-bench
           #:run-all-bench
           #:dump-result
           #:show-result
           #:visualize-bench-result))


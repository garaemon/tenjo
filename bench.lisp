;;================================================
;; bench.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :tenjo)

(defvar *benchmarks* nil)
(defvar *benchmark-results* nil)
(defvar *logfile-name* nil)

(defun init-bench (&optional (logfile-name nil))
  (setq *benchmarks* nil)
  (setq *logfile-name* logfile-name)
  t)

(defmacro defbench (name num &rest bodies)
  `(progn
     (if (assoc ',name *benchmarks*)
         (error "SAME BENCH NAME ~A" ',name)
         (push (cons ',name
                     (list
                      (cons :sampling ,num)
                      (cons :function
                            #'(lambda () (progn ,@bodies)))))
               *benchmarks*))))

(defun run-bench (&key
                  (name nil)
                  (func nil)
                  (sampling num))
  (format t "[tenjo-benchmark] running ~A...~%" name)
  (let ((before-user (get-internal-run-time))
        (before-real (get-internal-real-time)))
    (dotimes (i sampling)
      (funcall func))
    (let ((after-user (get-internal-run-time))
          (after-real (get-internal-real-time)))
      (let ((real (/ (- after-real before-real) internal-time-units-per-second))
            (user (/ (- after-user before-user) internal-time-units-per-second)))
        (push (cons name
                    (list (cons :real (/ real sampling))
                          (cons :user (/ user sampling))))
              *benchmark-results*)
        ))))


(defun print-bench-result (r)
  (let ((name (car r))
        (user-result (cdr (assoc :user (cdr r))))
        (real-result (cdr (assoc :real (cdr r)))))
    (format t "~25A  ~8,2f ~8,2f~%" name user-result real-result)))

(defun run-all-bench ()
  ;; clear results
  (setq *benchmark-results* nil)
  (iterate:iter
   (iterate:for b in *benchmarks*)
   (let ((name (car b))
         (func (cdr (assoc :function (cdr b))))
         (sample (cdr (assoc :sampling (cdr b)))))
     (run-bench :name name :func func :sampling sample)))
  ;; print all bench results
  (format t "-----------------------------------------------~%")
  (format t "  BENCHMARK RESULTS~%")
  (format t "-----------------------------------------------~%")
  (format t " BENCH-NAME:                 user[sec]  real[sec]~%")
  (iterate:iter
    (iterate:for r in *benchmark-results*)
    (print-bench-result r))
  t)

(defun dump-result ()
  (if (null *benchmark-results*)
      (error "You have to call defbench and run-all-bench")
      (with-open-file (f *logfile-name*
                         :direction :output
                         :if-exists :append)
        (format f "~s~%" f)
        *logfile-name*)))


;;================================================
;; bench.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :tenjo)

;; global symbols to store test results
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
      (let ((real (/ (- after-real before-real)
                     internal-time-units-per-second))
            (user (/ (- after-user before-user)
                     internal-time-units-per-second)))
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

;; result format
;; results := result result result ...
;; result  := (stamp benchmark-results)
;; stamp   := ((:date . date)
;;             (:lisp-implementation . lisp-implementation))
;; benchmark-results := (a-benchmark-result a-benchmark-result ...)
;; a-benchmark-result := (bench-name
;;                        (:real . real-time)
;;                        (:user . user-time))
;;
(defun dump-result ()
  (if (null *benchmark-results*)
      (error "You have to call defbench and run-all-bench")
      (with-open-file (f *logfile-name*
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
        (let ((stamp (list (cons :date (chimi:local-time-string))
                           (cons :lisp-implementation
                                 (lisp-implementation-type))
                           (cons :version
                                 (lisp-implementation-version)))))
          (format f "~s~%" (list stamp *benchmark-results*)))
        *logfile-name*)))

;; print results like below
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <FIB>
;; DATE                    USER            REAL
;; ---------------------------------------------
;; 2010-01-01-00-00-00      1.0            1.0
;; 2010-01-01-00-00-00      1.0            1.0
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <FACT>
;; DATE                    USER            REAL
;; ---------------------------------------------
;; 2010-01-01-00-00-00      1.0            1.0
;; 2010-01-01-00-00-00      1.0            1.0

(defun show-result (&key (log-file nil))
  ;; error check
  (if (null log-file) (error "You have to specify :log-file keyword"))
  (let ((results (chimi:read-from-file log-file)))
    (let ((all-tests
           (remove-duplicates
            (mapcan #'(lambda (x) (mapcar #'car (cadr x))) results))))
      (dolist (target-test all-tests)
        (show-one-result target-test results)))))

(defun show-one-result (target results)
  (format t ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
  (format t "<~A>~%" target)
  (format t "      DATE              USER     REAL~%")
  (format t "-----------------------------------------------~%")
  (let ((date-user-reals
         (mapcar #'(lambda (r)
                     (if (member target (mapcar #'car (cadr r)))
                         (list (assoc-ref :date (car r))
                               (assoc-ref
                                :user
                                (cdr (find-if #'(lambda (x) (eq (car x) target))
                                              (cadr r))))
                               (assoc-ref
                                :real
                                (cdr (find-if #'(lambda (x) (eq (car x) target))
                                              (cadr r)))))
                         nil))
                     results)))
    (dolist (r date-user-reals)
      (format t "~A ~8,2f ~8,2f~%" (car r) (cadr r) (caddr r)))
    (format t "~%")
    ))

;; it requires gnuplot
(defun visualize-bench-result (&key
                               (max-length 10)
                               (log-file-name *logfile-name*)
                               (target-implementations
                                (list (lisp-implementation-type))))
  (let ((datum nil))
    ;; read log
    (with-open-file (f log-file-name :direction :input)
      (let ((r nil))
        (while (setq r (read f nil nil))
          (push r datum))))
    ;; reverse
    (setq datum (reverse datum))
    ;; draw graph
    )
  )


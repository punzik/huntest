;; -*- geiser-scheme-implementation: guile -*-

(define-module (huntest iverilog))

(export
 test-body-simple)

(import
 (huntest)
 (srfi srfi-1)                          ; Lists
 (srfi srfi-9)                          ; Records
 (srfi srfi-11)                         ; let-values
 (srfi srfi-13)                         ; String library
 (srfi srfi-26)                         ; Currying with cut
 (srfi srfi-28)                         ; Simple format
 (srfi srfi-37)                         ; args-fold
 (srfi srfi-39))                        ; Parameters

;;;
;;; Icarus diagnostics use "warning:" for compiler and runtime warnings.
;;;
(define (warning-output? output)
  (any (lambda (line)
         (string-contains (string-downcase line) "warning:"))
       (string-split output #\newline)))

;;;
;;; Simple iverilog testbench test body function
;;;
(define* (test-body-simple #:key
                           sources
                           top
                           (compile-flags '())
                           (runtime-flags '())
                           (include-paths '())
                           (parameters '())
                           (defines '())
                           (init (lambda args #t))
                           (finish (lambda args #t))
                           (fail-on-warnings? #f))
  (lambda (plusargs base-path tb-path test-path)
    (let ((vvp-file (string-append top ".vvp"))
          (includes
           (map (cut string-append "-I" <>)
                (cons (base-path)
                      (map base-path include-paths))))
          (defines
           (map (cut string-append "-D" <>)
                (cons*
                 (string-append "HUNTEST_BASE_DIR="
                                (verilog-string-literal (base-path)))
                 (string-append "HUNTEST_TB_DIR="
                                (verilog-string-literal (tb-path)))
                 "HUNTEST_TESTBENCH"
                 (map
                  (lambda (def)
                    (if (list? def)
                        (format "~a=~a" (car def) (cadr def))
                        def))
                  defines))))
          (parameters
           (map (lambda (p)
                  (format "-P~a.~a=~a" top (first p) (second p)))
                parameters))
          (sources
           (base-path (if (procedure? sources)
                          (sources plusargs base-path tb-path test-path)
                          sources))))

      (define (run-command command args)
        (let-values (((retval output)
                      (system%-capture-argv command args #:base (test-path))))
          (let ((warning? (warning-output? output)))
            (display output)
            (when (and fail-on-warnings? warning?)
              (println 'fail "Icarus warning treated as failure"))
            (and (zero? retval)
                 (or (not fail-on-warnings?)
                     (not warning?))))))

      (let-values (((ext-flags reg-flags)
                    (partition
                     (lambda (flag)
                       (any (cut string-prefix? <> flag)
                            '("-vcd" "-lxt" "-lx2" "-fst" "-none" "-sdf" "-compatible")))
                     runtime-flags)))

        (and (init plusargs base-path tb-path test-path)
             (let ((retval
                    (and
                     (run-command "iverilog"
                                  (append (list "-o" vvp-file "-s" top)
                                          compile-flags parameters defines includes sources))
                     (run-command "vvp"
                                  (append reg-flags
                                          (list (test-path vvp-file))
                                          ext-flags plusargs)))))
               (and (finish plusargs base-path tb-path test-path)
                    retval)))))))

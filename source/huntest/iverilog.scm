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
                           (finish (lambda args #t)))
  (lambda (plusargs base-path tb-path test-path)
    (let ((vvp-file (string-append top ".vvp"))
          (includes
           (map (cut string-append "-I" <>)
                (cons (base-path)
                      (map base-path include-paths))))
          (defines
            (map (cut string-append "-D" <>)
                 (cons*
                  (string-append "HUNTEST_BASE_DIR='\"" (base-path) "\"'")
                  (string-append "HUNTEST_TB_DIR='\"" (tb-path) "\"'")
                  "HUNTEST_TESTBENCH"
                  (map
                   (lambda (def)
                     (if (list? def)
                         (format "~a=~a" (car def) (cadr def))
                         def))
                   defines))))
          (parameters
           ;; (map (cut string-append (format "-P~a." top) <>) parameters)
           (map (lambda (p)
                  (format "-P~a.~a=~a" top (first p) (second p)))
                parameters)))

      (let-values (((ext-flags reg-flags)
                    (partition
                     (lambda (flag)
                       (any (cut string-prefix? <> flag)
                            '("-vcd" "-lxt" "-lx2" "-fst" "-none" "-sdf" "-compatible")))
                     runtime-flags)))

        (and (init plusargs base-path tb-path test-path)
             (let ((retval
                    (and (zero?
                          (system% (string-append-sep*
                                    " "
                                    "iverilog" "-o" vvp-file
                                    "-s" top
                                    compile-flags parameters defines includes
                                    (base-path (if (procedure? sources)
                                                   (sources plusargs base-path tb-path test-path)
                                                   sources)))
                                   #:base (test-path)))
                         (zero?
                          (system% (string-append-sep*
                                    " "
                                    "vvp"
                                    reg-flags
                                    (test-path vvp-file)
                                    ext-flags
                                    plusargs)
                                   #:base (test-path))))))

               (and (finish plusargs base-path tb-path test-path)
                    retval)))))))

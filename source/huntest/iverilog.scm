;; -*- geiser-scheme-implementation: guile -*-

(define-module (huntest iverilog))

(export
 simple-testbench)

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
;;; Simple iverilog testbench with one test
;;;
(define* (simple-testbench #:key
                           sources
                           top
                           (name top)
                           (compile-flags '())
                           (runtime-flags '())
                           (include-paths '())
                           (parameters '())
                           (defines '()))

  (let ((vvp-file (string-append
                   (string->filename name)
                   ".vvp")))
    (make-testbench
     #:name name
     #:init (lambda (plusargs base-path tb-path)
              (let ((includes
                     (map (cut string-append "-I" <>)
                          (cons (base-path) include-paths)))
                    (defines
                      (map (cut string-append "-D" <>)
                           (cons*
                            (string-append "HUNTEST_BASE_DIR='\"" (base-path) "\"'")
                            (string-append "HUNTEST_TB_DIR='\"" (tb-path) "\"'")
                            "HUNTEST_TESTBENCH"
                            defines)))
                    (parameters
                     (map (cut string-append (format "-P~a." top) <>) parameters)))
                (zero?
                 (system% (string-append-sep*
                           " "
                           "iverilog" "-o" vvp-file
                           "-s" top
                           compile-flags parameters defines includes
                           (base-path sources))
                          #:base (tb-path)))))

     #:tests (make-test
              #:name top
              #:body
              (lambda (plusargs base-path tb-path test-path)
                (let-values (((ext-flags reg-flags)
                              (partition
                               (lambda (flag)
                                 (any (cut string-prefix? <> flag)
                                      '("-vcd" "-lxt" "-lx2" "-fst" "-none" "-sdf" "-compatible")))
                               runtime-flags)))
                  (zero?
                   (system% (string-append-sep*
                             " "
                             "vvp"
                             reg-flags
                             (tb-path vvp-file)
                             ext-flags
                             plusargs)
                            #:base (test-path)))))))))

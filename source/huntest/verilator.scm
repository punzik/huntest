;; -*- geiser-scheme-implementation: guile -*-

(define-module (huntest verilator))

(export
 test-body-simple)

(import
 (huntest)
 (srfi srfi-1)                          ; Lists
 (srfi srfi-11)                         ; let-values
 (srfi srfi-13)                         ; String library
 (srfi srfi-26)                         ; Currying with cut
 (srfi srfi-28))                        ; Simple format

;;;
;;; Verilator warnings start with %Warning-; C++ compiler warnings normally
;;; contain warning:.  Match both when strict warning handling is enabled.
;;;
(define (warning-output? output)
  (let ((output (string-downcase output)))
    (or (string-contains output "%warning-")
        (string-contains output "warning:"))))

;;;
;;; Simple Verilator testbench test body function.
;;; Builds a self-contained SystemVerilog testbench with --binary and runs it.
;;;
(define* (test-body-simple #:key
                           sources
                           top
                           (compile-flags '())
                           (runtime-flags '())
                           (include-paths '())
                           (parameters '())
                           (defines '())
                           (timing? #t)
                           (build-jobs 1)
                           (init (lambda args #t))
                           (finish (lambda args #t))
                           (fail-on-warnings? #f))
  (lambda (plusargs base-path tb-path test-path)
    (let* ((obj-dir (test-path "obj_dir"))
           (binary-name "simulation")
           (binary (append-path obj-dir binary-name))
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
                   (format "-G~a=~a" (first p) (second p)))
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
              (println 'fail "Verilator warning treated as failure"))
            (and (zero? retval)
                 (or (not fail-on-warnings?)
                     (not warning?))))))

      (and (init plusargs base-path tb-path test-path)
           (let ((retval
                  (and
                   (run-command
                    "verilator"
                    (append
                     (list "--binary"
                           (if timing? "--timing" "--no-timing")
                           "--top-module" top
                           "--Mdir" obj-dir
                           "-o" binary-name
                           "--build-jobs" (number->string build-jobs))
                     compile-flags parameters defines includes sources))
                   (run-command binary (append runtime-flags plusargs)))))
             (and (finish plusargs base-path tb-path test-path)
                  retval))))))

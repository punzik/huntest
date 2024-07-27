;; -*- geiser-scheme-implementation: guile -*-

(define-module (huntest sby))

(export
 test-body-simple
 make-sby-config)

(import
 (prefix (huntest) hut::)
 (srfi srfi-1)                          ; Lists
 (srfi srfi-9)                          ; Records
 (srfi srfi-11)                         ; let-values
 (srfi srfi-13)                         ; String library
 (srfi srfi-26)                         ; Currying with cut
 (srfi srfi-28)                         ; Simple format
 (srfi srfi-37)                         ; args-fold
 (srfi srfi-39)                         ; Parameters
 (ice-9 string-fun))

(define* (test-body-simple #:key
                           sources
                           top
                           (parameters '())
                           (defines '())
                           (mode 'bmc)
                           (depth 20)
                           (append 0)
                           (engine 'yices)
                           (frontend 'yosys-verilog)
                           (init (lambda args #t))
                           (finish (lambda args #t))
                           (strip-output #f))
  (lambda (plusargs base-path tb-path test-path)
    (and (init plusargs base-path tb-path test-path)
         (let ((sources
                (map (lambda (file)
                       (if (absolute-file-name? file)
                           file
                           (base-path file)))
                     (if (procedure? sources)
                         (sources plusargs base-path tb-path test-path)
                         sources)))
               (sby-file (test-path (format "~a.sby" top))))
           (with-output-to-file sby-file
             (lambda ()
               (for-each
                hut::println
                (make-sby-config #:sources sources
                                 #:top top
                                 #:parameters parameters
                                 #:defines defines
                                 #:mode mode
                                 #:depth depth
                                 #:append append
                                 #:engine engine
                                 #:frontend frontend))))
           (let ((retval
                  (zero?
                   (let-values
                       (((retval output)
                         (hut::system%-capture
                          (format "sby -f ~a" sby-file)
                          #:base (test-path))))
                     (display
                      (if strip-output
                          (string-replace-substring output (string-append "" (base-path) "/")  "")
                          output))
                     retval))))

             (and (finish plusargs base-path tb-path test-path)
                  retval))))))

(define (list-of-strings . args)
  (map (cut format "~a" <>)
       (hut::list-flat args)))

(define* (make-sby-config #:key
                          sources
                          top
                          (parameters '())
                          (defines '())
                          (mode 'bmc)
                          (depth 20)
                          (append 20)
                          (engine 'yices)
                          (frontend 'yosys-verilog))
  (define * format)
  (list-of-strings
   ;; -- Options and targets
   "[options]"
   (* "mode ~a"
      (or (and (memq mode '(bmc prove cover))
               (symbol->string mode))
          (raise-exception
           (format "Error: unknown sby mode: '~a'\n" mode))))
   (* "depth ~a" depth)
   (if (zero? append)
       ""
       (* "append ~a" append))

   ;; -- Engines
   "[engines]"
   (* "smtbmc ~a"
      (or (and (memq engine '(boolector yices))
               (symbol->string engine))
          (raise-exception
           (format "Error: unknown sby engine: '~a'\n" engine))))

   ;; -- Script
   "[script]"
   (case frontend
     ((yosys-verilog yosys-systemverilog)
      (* "read -noverific"))
     ((verific)
      (* "read -verific"))
     ((synlig)
      (* "plugin -i systemverilog"))
     (else
      (raise-exception
       (format "Error: unknown sby frontend: '~a'\n" frontend))))
   ;; Defines
   (map (lambda (param)
          (let ((name (car param))
                (value (if (null? (cdr param)) #f
                           (let ((v (cadr param)))
                             (if (number? v) v (* "\"~a\"" v))))))
            (* "read -define ~a"
               (if value (* "~a=~a" name value) name))))
        defines)
   ;; Read files
   (map (lambda (file)
          (let ((file (basename file)))
            (case frontend
              ((yosys-verilog)
               (* "read -formal ~a" file))
              ((yosys-systemverilog)
               (* "read -formal -sv ~a" file))
              ((verific)
               (* "read -formal ~a" file))
              ((synlig)
               (* "read_systemverilog -formal -defer ~a" file))
              (else
               (* "read -formal ~a" file)))))
        sources)
   ;; Link sources when synlig uses
   (if (eq? frontend 'synlig)
       "read_systemverilog -link"
       '())
   ;; Parameters
   (map (lambda (p)
          (* "chparam -set ~a ~a ~a" (first p) (second p) top))
        parameters)
   ;; Prepare sources
   (* "prep -top ~a" top)

   ;; -- Files list
   "[files]"
   (map (cut * "~a" <>) sources)))

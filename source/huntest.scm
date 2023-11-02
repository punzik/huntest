;; -*- geiser-scheme-implementation: guile -*-

(define-module (huntest) #:declarative? #f)

(export
 ;; Main
 run
 make-test
 make-testbench
 main

 ;; Auxiliary
 append-path
 delete-recursive
 find-files
 find-plusarg
 list-add-separator
 list-dir
 list-flatten
 printf
 sort-uniq
 string->filename
 string-append*
 string-append-sep*
 system%
 system%-capture)

(import
 (srfi srfi-1)                          ; Lists
 (srfi srfi-9)                          ; Records
 (srfi srfi-11)                         ; let-values
 (srfi srfi-13)                         ; String library
 (srfi srfi-26)                         ; Currying with cut
 (srfi srfi-28)                         ; Simple format
 (srfi srfi-37)                         ; args-fold
 (srfi srfi-39)                         ; Parameters
 (ice-9 popen)
 (ice-9 textual-ports)
 (ice-9 threads)
 (ice-9 futures)
 (ice-9 atomic)
 (ice-9 regex))

(define APP_VERSION          "0.1.0")

(define WORK_DIR_PREFIX      "")
(define TEST_DIR_PREFIX      "")
(define MAX_LEN_OF_FILENAME  32)
(define LOG_FILE_NAME        "log.txt")
(define TEST_SCRIPT_REGEX    "\\.hut$")
(define UNNAMED_TEST_NAME    "unnamed")
(define UNNAMED_TB_NAME      "unnamed")

;;; '(tag-symbol . tag-name)
(define output-tags
  '((info . "INFO#")
    (warn . "WARN#")
    (fail . "FAIL#")))

;;; '(tag-symbol color-code prefix)
(define output-tag-format
  '((info 15  "   | ")
    (warn 226 "   + ")
    (fail 196 "   ! ")
    (#f   244 "   : ")))

;;; Fail tags list
(define fail-tags '(fail))

(define LOG_HEAD_COLOR 14)
(define LOG_INFO_COLOR 6)
(define LOG_SUCC_COLOR 47)
(define LOG_FAIL_COLOR 196)

;;;
;;; Colorize text
;;;
(define* (string-colorize text fg #:optional (bg 'default))
  (format "~a~a~a~a[0m"
          ;; Foreground
          (if (number? fg)
              (format "~a[38;5;~am" #\esc fg)
              (format "~a[~am" #\esc (case fg
                                       ((black)   "30")
                                       ((red)     "31")
                                       ((green)   "32")
                                       ((yellow)  "33")
                                       ((blue)    "34")
                                       ((magenta) "35")
                                       ((cyan)    "36")
                                       ((white)   "37")
                                       ((default) "39"))))
          ;; Background
          (if (number? bg)
              (format "~a[48;5;~am" #\esc bg)
              (format "~a[~am" #\esc (case bg
                                       ((black)   "40")
                                       ((red)     "41")
                                       ((green)   "42")
                                       ((yellow)  "43")
                                       ((blue)    "44")
                                       ((magenta) "45")
                                       ((cyan)    "46")
                                       ((white)   "47")
                                       ((default) "49"))))
          text #\esc))

;;;
;;; Split string by tag and rest of string
;;; E.g: "INFO#message text" -> (values 'info "message text")
;;;
(define (tagged-message-split str)
  (let ((tag
         (find
          (lambda (t)
            (string-prefix? (cdr t) str))
          output-tags)))
    (if tag
        (values (car tag)
                (substring str (string-length (cdr tag))))
        (values #f str))))

;;;
;;; Function removes a tag from string, adds prefix that matches the tag, and
;;; optionally colorize output string.
;;; E.g: "INFO#message text" -> "  |" + (colorize "message text")
;;;
(define* (string-untag str #:optional (colorize? #t))
  (let-values (((tag str)
                (tagged-message-split str)))
    (let* ((tag-format (assq tag output-tag-format))
           (color (second tag-format))
           (prefix (third tag-format)))
      (string-append
       prefix
       (if colorize? (string-colorize str color) str)))))

;;;
;;; String or list of string has fail tag
;;;
(define (tag-fail? strs)
  (let ((fail-tag-prefixes
         (map (lambda (t) (cdr (assq t output-tags))) fail-tags)))
    (any (lambda (s)
           (find (cut string-prefix? <> s) fail-tag-prefixes))
         (if (list? strs) strs (list strs)))))

;;;
;;; String has no tag
;;;
(define (tag-none? str)
  (not (find (lambda (t) (string-prefix? (cdr t) str)) output-tags)))

;;;
;;; Add separator between list items
;;; E.g: (list-add-separator 0 '(1 2 3)) -> '(1 0 2 0 3)
;;;
(define (list-add-separator sep lst)
  (if (or (null? lst)
          (null? (cdr lst)))
      lst
      (cons* (car lst) sep (list-add-separator sep (cdr lst)))))

;;;
;;; Flatten nested lists
;;;
(define (list-flatten lst)
  (if (null? lst) '()
      (fold-right
       (lambda (x out)
         (if (list? x)
             (append (list-flatten x) out)
             (cons x out)))
       '() lst)))

;;;
;;; Recursively append strings and list of strings
;;;
(define (string-append* . strings)
  (string-concatenate
   (list-flatten strings)))

;;;
;;; Recursively append strings and list of strings with separator
;;;
(define (string-append-sep* sep . strings)
  (string-concatenate
   (list-add-separator
    sep
    (list-flatten strings))))

;;;
;;; Random string
;;;
(define* (string-random len #:optional (template "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"))
  (list->string
   (map (lambda (x) (string-ref template (random (string-length template))))
        (iota len))))

;;;
;;; Check for equal elements in list
;;;
(define* (set? lst #:optional (less? <))
  (not
   (call/cc
    (lambda (ret)
      (fold (lambda (x y)
              (if (and y
                       (not (less? x y))
                       (not (less? y x)))
                  (ret x)
                  x))
            #f (sort lst less?))
      (ret #f)))))

;;;
;;; Sort and remove duplicates
;;;
(define (sort-uniq items less)
  (let ((items (sort items less)))
    (reverse
     (fold
      (lambda (item items)
        (if (or  (null? items)
                 (less item (car items))
                 (less (car items) item))
            (cons item items)
            items))
      '() items))))

;;;
;;; Append path to each string
;;; E.g:
;;; 1. (appent-path "/root" '("a" "b")) -> '("/root/a" "/root/b")
;;; 2. (appent-path "/root" "a") -> "/root/a"
;;;
(define (append-path path items)
  (if (list? items)
      (map (cut string-append path file-name-separator-string <>) items)
      (string-append path file-name-separator-string items)))

;;;
;;; Plusarg exists?
;;;
(define (find-plusarg plusargs arg)
  (find (lambda (p)
          (let ((p-arg (substring p 1)))
            (if (string-null? arg)
                (string-null? p-arg)
                (string-prefix? arg p-arg))))
        plusargs))

;;;
;;; As system% but returns values of retval and output as string
;;;
(define* (system%-capture cmd #:key (base #f))
  ;; Echo command
  (printf "RUN: ~a\n" cmd)
  ;; Execute
  (let* ((cmd (string-append
               (if base (format "cd ~a; " base) "")
               cmd " 2>&1"))
         (p (open-input-pipe cmd)))
    (let ((output (get-string-all p)))
      (values (close-pipe p)
              output))))

;;;
;;; Overridden function with output redirection capability
;;;
(define* (system% cmd #:key (base #f))
  (let-values (((retval output)
                (system%-capture cmd #:base base)))
    (display output)
    retval))

;;;
;;; Futures with completed flag
;;;
(define (make-future% thunk)
  (let* ((completed (make-atomic-box #f))
         (f (future
             (dynamic-wind
               (lambda () #f)
               thunk
               (lambda () (atomic-box-set! completed #t))))))
    (lambda (action)
      (cond
       ((eq? action 'future%?) #t)
       ((eq? action 'touch) (touch f))
       ((eq? action 'completed?) (atomic-box-ref completed))
       (else (raise-exception (format "Unknown future% action '~a'" action)))))))

(define-syntax-rule
  (future% body ...)
  (make-future% (lambda () body ...)))

(define (future%? f) (f 'future%?))
(define (future%-touch f) (f 'touch))
(define (future%-completed? f) (f 'completed?))

;;;
;;; Prarallel execute of the thunks list and call
;;; func on the result of thunk as it complete
;;;
(define (map-select func thunks)
  (let ((fs (map make-future% thunks)))
    (let loop ((fs fs) (ret '()))
      (if (null? fs)
          ret
          (let-values (((completed fs)
                        (partition future%-completed? fs)))
            (loop
             fs
             (if (null? completed)
                 (begin
                   (usleep 10000)
                   (yield)
                   ret)
                 (fold
                  (lambda (f out)
                    (cons (func (future%-touch f)) out))
                  ret completed))))))))

;;;
;;; Return directory list
;;;
(define (list-dir path)
  (if (file-exists? path)
      (let ((dir (opendir path)))
        (let loop ((ls '()))
          (let ((item (readdir dir)))
            (if (eof-object? item)
                (begin
                  (closedir dir)
                  ls)
                (if (or (string=? item ".")
                        (string=? item ".."))
                    (loop ls)
                    (loop (cons (string-append path "/" item) ls)))))))
      '()))

;;;
;;; Recursive find path items for which the function f returns true
;;; (fn fullpath type) : (-> (string symbol) boolean)
;;; Returns empty list if files not found
;;;
(define* (find-files fn base #:key (recursive #f))
  (let ((ls (list-dir base)))
    (let ((files.dirs
           (fold (lambda (name f.d)
                   (with-exception-handler
                       (lambda (e) f.d)
                     (lambda ()
                       (let* ((files (car f.d))
                              (dirs  (cdr f.d))
                              (t (stat:type (stat name)))
                              (f (if (fn name t) (cons name files) files)))
                         (if (eq? t 'directory)
                             (cons f (cons name dirs))
                             (cons f dirs))))
                     #:unwind? #t))
                 '(()) ls)))
      (let ((files (car files.dirs))
            (dirs (cdr files.dirs)))
        (if recursive
            (fold (lambda (dir files)
                    (append files (find-files fn dir #:recursive #t)))
                  files dirs)
            files)))))

;;;
;;; Recursive delete directory
;;;
(define (delete-recursive path)
  (let ((path (canonicalize-path path)))
    (if (eq? 'directory (stat:type (stat path)))
        (begin
          (for-each delete-recursive (list-dir path))
          (rmdir path))
        (delete-file path))))

;;;
;;; Useful print function
;;;
(define (printf tag . rest)
  (if (symbol? tag)
      (let ((output-tag (assq tag output-tags)))
        (if output-tag
            (begin
              (display (cdr output-tag))
              (display (apply format rest)))
            (raise-exception (format "Tag '~a' not found" tag))))
      (display (apply format (cons tag rest)))))

(define (eprintf tag . rest)
  (with-output-to-port (current-error-port)
    (lambda () (apply printf (cons tag rest)))))

;;;
;;; Make correct file name from arbitrary string
;;;
(define* (string->filename str #:optional (max-len MAX_LEN_OF_FILENAME))
  (let ((str
         (string-downcase
          (string-map
           (lambda (c)
             (if (or (char-alphabetic? c)
                     (char-numeric? c)) c #\_))
           str))))
    (if (> (string-length str) max-len)
        (substring str 0 max-len)
        str)))

;;;
;;; Renames duplicate strings by adding a decimal suffix
;;;
(define (rename-duplicates string-list eq)
  (let ((counter 1))
    (reverse
     (fold
      (lambda (item out)
        (cons
         (let loop ((i item))
           (if (member i out eq)
               (begin
                 (let ((suffix (number->string counter)))
                   (set! counter (+ counter 1))
                   (loop (string-append item suffix))))
               i))
         out))
      '() string-list))))

;;;
;;; Test struct
;;;
(define-record-type <test>
  (test-new name func pass output path)
  test?
  (name test-name test-set-name!)
  (func test-func)
  (pass test-pass? test-pass!)
  (output test-output test-set-output!)
  (path test-path test-set-path!))

;;;
;;; Testbench struct
;;;
(define-record-type <testbench>
  (testbench-new name init finish tests init-pass init-output fini-pass fini-output base-path work-path filename)
  testbench?
  (name tb-name)
  (init tb-init)
  (finish tb-finish)
  (tests tb-tests tb-set-tests!)
  (init-pass tb-init-pass? tb-init-pass!)
  (init-output tb-init-output tb-init-set-output!)
  (fini-pass tb-fini-pass? tb-fini-pass!)
  (fini-output tb-fini-output tb-fini-set-output!)
  (base-path tb-base-path tb-set-base-path!)
  (work-path tb-work-path tb-set-work-path!)
  (filename tb-filename tb-set-filename!))

;;;
;;; Test constructor
;;;
(define* (make-test #:key (name "") (body (lambda args #f)))
  (let* ((name (string-trim-both name))
         (name (if (string-null? name) UNNAMED_TEST_NAME name)))
    (test-new name body #f '() #f)))

;;;
;;; Testbench constructor
;;;
(define* (make-testbench #:key
                         (name "")
                         (init (lambda args #t))
                         (finish (lambda args #t))
                         (tests '()))

  (let* ((name (string-trim-both name))
         (name (if (string-null? name) UNNAMED_TB_NAME name))
         (tests (if (list? tests) tests (list tests))))

    ;; Check for identical test names
    (map (lambda (test name)
           (test-set-name! test name))
         tests
         (rename-duplicates (map test-name tests) string=?))

    (testbench-new name init finish tests
                   #f '() #f '() #f #f #f)))

;;;
;;; Filter testbenches by regexp query
;;; Query is a string like a "testbench-regexp::test-regexp"
;;; Example:
;;;   "::"                   -> select all tests in all testbenches
;;;   "^tb0$::"              -> select testbenches with name exactly "tb0"
;;;   "testbench::test [13]" -> select testbenches with name contains "testbench"
;;;                             and tests with name contains "test 1" or "test 3"
;;;   "::test 1"             -> setect tests with name contains "test 1"
;;;   "testbench 1"          -> select testbenches witn name contains "testbench 1"
;;;
(define (filter-testbenches tbs query)
  (let-values
      (((tb-regex test-regex)
        (let ((index-of-:: (string-contains query "::")))
          (if index-of-::
              (let ((l (substring query 0 index-of-::))
                    (r (substring query (+ index-of-:: 2))))
                (values (if (string-null? l) #f l)
                        (if (string-null? r) #f r)))
              (values (if (string-null? query) #f query) #f)))))

    ;; Filter testbenches by testbench name
    (let ((tbs
           (if tb-regex
               (let ((tb-regex (make-regexp tb-regex)))
                 (filter (lambda (tb) (regexp-exec tb-regex (tb-name tb)))
                         tbs))
               tbs)))

      ;; Filter tests by test name
      (let ((tbs
             (if test-regex
                 (let ((test-regex (make-regexp test-regex)))
                   (map
                    (lambda (tb)
                      (tb-set-tests! tb
                                     (filter
                                      (lambda (test) (regexp-exec test-regex (test-name test)))
                                      (tb-tests tb)))
                      tb)
                    tbs))
                 tbs)))

        ;; Remove testbenches without tests
        (filter (lambda (tb) (not (null? (tb-tests tb)))) tbs)))))

;;;
;;; Execute function with intercept of output
;;;
(define (execute-phase func set-output! set-pass!)
  (let* ((pass #f)
         (output (string-split
                  (string-trim-both
                   (with-output-to-string
                     (lambda ()
                       (with-exception-handler
                           (lambda (e)
                             (newline)
                             (printf 'fail "~a\n" e))
                         (lambda () (set! pass (func)))
                         #:unwind? #t))))
                  #\newline))
         (pass (and pass (not (tag-fail? output)))))

    (set-output! output)
    (set-pass! pass)
    pass))

;;;
;;; Execute test
;;;
(define (test-execute! tb plusargs test)
  (execute-phase
   (lambda () ((test-func test)
          plusargs
          (tb-base-path tb)
          (tb-work-path tb)
          (test-path test)))
   (lambda (o) (test-set-output! test o))
   (lambda (p) (test-pass! test p))))

;;;
;;; Execute testbench init
;;;
(define* (tb-init-execute! tb plusargs)
  (execute-phase
   (lambda () ((tb-init tb)
          plusargs
          (tb-base-path tb)
          (tb-work-path tb)))
   (lambda (o) (tb-init-set-output! tb o))
   (lambda (p) (tb-init-pass! tb p))))

;;;
;;; Execute testbench destructor
;;;
(define* (tb-fini-execute! tb plusargs)
  (execute-phase
   (lambda () ((tb-finish tb)
          plusargs
          (tb-base-path tb)
          (tb-work-path tb)))
   (lambda (o) (tb-fini-set-output! tb o))
   (lambda (p) (tb-fini-pass! tb p))))

;;;
;;; Format testbench output log
;;;
(define (tb-print-output tb verbosity colorize?)
  (define (print-output output verbose? colorize?)
    (when output
      (for-each
       (lambda (s)
         (when (or verbose? (not (tag-none? s)))
           (display
            (format "~a\n" (string-untag s colorize?)))))
       output)))

  (let ((string-colorize (if colorize? string-colorize (lambda (s c) s)))
        (verbose (eq? verbosity 'verbose))
        (quiet (eq? verbosity 'quiet))
        (testbench-pass (and (tb-init-pass? tb)
                             (tb-fini-pass? tb)
                             (every test-pass? (tb-tests tb)))))

    (when (not (and quiet testbench-pass))
      (let-values (((s c) (if testbench-pass
                              (values "PASS" LOG_HEAD_COLOR)
                              (values "FAIL" LOG_FAIL_COLOR))))
        (display (string-colorize (format "TESTBENCH ~a ~a : ~a\n"
                                          s (tb-name tb) (tb-work-path tb))
                                  c)))

      ;; Print init output
      (print-output (tb-init-output tb)
                    (or (not (tb-init-pass? tb)) verbose)
                    colorize?)

      ;; Print tests
      (when (tb-init-pass? tb)
        (for-each
         (lambda (test)
           (when (not (and quiet (test-pass? test)))
             (let ((testname (format "~a::~a" (tb-name tb) (test-name test))))
               ;; Test header
               (display (string-colorize (format "   TEST ~a : ~a\n"
                                                 testname
                                                 (basename (test-path test)))
                                         LOG_HEAD_COLOR))

               ;; Test output
               (print-output (test-output test)
                             (or (not (test-pass? test)) verbose)
                             colorize?)

               ;; Test status
               (display
                (if (test-pass? test)
                    (string-colorize (format "   PASS ~a\n" testname) LOG_SUCC_COLOR)
                    (string-colorize (format "   FAIL ~a\n" testname) LOG_FAIL_COLOR))))))
         (tb-tests tb)))

      ;; Print fini output
      (print-output (tb-fini-output tb)
                    (or (not (tb-fini-pass? tb)) verbose)
                    colorize?)

      ;; Print testbench status
      (display
       (if testbench-pass
           (string-colorize (format "PASS ~a\n" (tb-name tb)) LOG_SUCC_COLOR)
           (string-colorize (format "FAIL ~a\n" (tb-name tb)) LOG_FAIL_COLOR)))
      (newline))))

;;;
;;; Print testbenches and tests
;;;
(define (tb-print-only tb colorize?)
  (let ((string-colorize (if colorize? string-colorize (lambda (s c) s))))
    (display (string-colorize (format "TESTBENCH ~a : ~a\n"
                                      (tb-name tb)
                                      (append-path
                                       (tb-base-path tb)
                                       (tb-filename tb)))
                              LOG_HEAD_COLOR))
    (for-each
     (lambda (test)
       (display (string-colorize (format "   TEST ~a::~a\n"
                                         (tb-name tb)
                                         (test-name test))
                                 LOG_INFO_COLOR)))
     (tb-tests tb))))

;;;
;;; Clear testbench output dir
;;;
(define (clear-testbench-output tb)
  (let ((tb-pass (and (tb-init-pass? tb)
                      (tb-fini-pass? tb)
                      (every test-pass? (tb-tests tb)))))
    (if tb-pass
        (delete-recursive (tb-work-path tb))
        (for-each
         (lambda (test)
           (when (test-pass? test)
             (delete-recursive (test-path test))))
         (tb-tests tb)))))

;;;
;;; Print pass/fail statistics
;;;
(define (print-summary testbenches colorize?)
  (let ((colorize (if colorize? string-colorize (lambda (s c) s)))
        (tb-count (length testbenches))
        (test-count (apply
                     +
                     (map (lambda (tb) (length (tb-tests tb))) testbenches)))
        (tb-succ (length
                  (filter
                   (lambda (tb)
                     (and (tb-init-pass? tb)
                          (tb-fini-pass? tb)
                          (every test-pass? (tb-tests tb))))
                   testbenches)))
        (test-succ (apply
                    +
                    (map (lambda (tb) (length (filter test-pass? (tb-tests tb))))
                         testbenches))))

    (display (colorize (format "## ALL  ~a (~a)\n" tb-count test-count) LOG_HEAD_COLOR))
    (display (colorize (format "## PASS ~a (~a)\n" tb-succ test-succ) LOG_SUCC_COLOR))
    (display (colorize (format "## FAIL ~a (~a)\n"
                               (- tb-count tb-succ)
                               (- test-count test-succ))
                       LOG_FAIL_COLOR))))

;;;
;;; Make dir reqursive
;;;
(define (mkdir-rec path)
  (when (not (file-exists? path))
    (let ((base (dirname path))
          (dir (basename path)))
      (when (not
             (or (file-exists? base)
                 (equal? base ".")
                 (equal? base "/")))
        (mkdir-rec base))
      (mkdir path))))

;;;
;;; Run testbenches
;;;
(define* (run-testbenches! testbenches
                           #:key
                           (plusargs       '())
                           (base-path      #f)
                           (verbosity      #f)
                           (keep-output?   #f)
                           (colorize?      #f)
                           (static-output? #f)
                           (parallel?      #f))

  ;; Make testbench work directory
  (define (prepare-tb-path tb)
    (let ((base-path (or base-path (tb-base-path tb)))
          (script-name (tb-filename tb)))
      (mkdir-rec base-path)

      (if static-output?
          ;; static work path
          (let ((work-path
                 (append-path base-path
                              (format "~a~a-~a" WORK_DIR_PREFIX
                                      script-name (string->filename (tb-name tb))))))
            (when (file-exists? work-path)
              (delete-recursive work-path))
            (mkdir work-path)
            work-path)
          ;; dynamic work path
          (mkdtemp
           (append-path base-path
                        (format "~a~a-~a-~a-XXXXXX"
                                WORK_DIR_PREFIX
                                script-name
                                (string->filename (tb-name tb))
                                (current-time)))))))

  ;; Make test directory
  (define (prepare-test-path test work-path)
    (let ((test-path
           (append-path work-path
                        (format "~a~a" TEST_DIR_PREFIX
                                (string->filename (test-name test))))))
      (if (file-exists? test-path)
          (raise-exception
           (format "Fatal error: test path exists: '~a'\n" test-path))
          (mkdir test-path))
      ;; (canonicalize-path)
      test-path))

  (if parallel?
      ;; Parallel execution
      (let loop ((inits
                  ;; Execute all inits
                  (map
                   (lambda (tb) (cons
                                 tb
                                 (begin
                                   (tb-set-work-path! tb (prepare-tb-path tb))
                                   (future% (tb-init-execute! tb plusargs)))))
                   testbenches))
                 (tests '())
                 (finis '()))

        (when (or (not (null? inits))
                  (not (null? tests))
                  (not (null? finis)))

          (let-values (((inits-completed inits)
                        (partition
                         (lambda (i) (future%-completed? (cdr i)))
                         inits))

                       ((tests-completed tests)
                        (partition
                         (lambda (t) (every future%-completed? (cdr t)))
                         tests))

                       ((finis-completed finis)
                        (partition
                         (lambda (f) (future%-completed? (cdr f)))
                         finis)))

            (let-values (((inits-completed-pass inits-completed-fail)
                          (partition
                           (lambda (i) (future%-touch (cdr i)))
                           inits-completed)))

              (let ((tests
                     (append
                      tests
                      ;; Execute initied testbench tests
                      (map
                       (lambda (i)
                         (let ((tb (car i)))
                           (cons tb
                                 (map
                                  (lambda (test)
                                    (test-set-path! test (prepare-test-path test (tb-work-path tb)))
                                    (future% (test-execute! tb plusargs test)))
                                  (tb-tests tb)))))
                       inits-completed-pass)))

                    (finis
                     (append
                      finis
                      ;; Execute fini of completed testbenches
                      (map (lambda (t)
                             (let ((tb (car t)))
                               (cons tb (future% (tb-fini-execute! tb plusargs)))))
                           (append tests-completed
                                   inits-completed-fail)))))

                ;; Final processing
                (for-each
                 (lambda (f)
                   (let ((tb (car f)))
                     (tb-print-output tb verbosity colorize?)

                     (with-output-to-file (append-path (tb-work-path tb) LOG_FILE_NAME)
                       (lambda () (tb-print-output tb 'verbose #f)))

                     (when (not keep-output?)
                       (clear-testbench-output tb))))
                 finis-completed)

                ;; Wait
                (if (and (null? inits-completed)
                         (null? tests-completed)
                         (null? finis-completed))
                    (usleep 10000)
                    (yield))

                (loop inits tests finis))))))

      ;; Sequential execution
      (for-each
       (lambda (tb)
         (tb-set-work-path! tb (prepare-tb-path tb))

         (when (tb-init-execute! tb plusargs)
           (for-each
            (lambda (test)
              (test-set-path! test (prepare-test-path test (tb-work-path tb)))
              (test-execute! tb plusargs test))
            (tb-tests tb))
           (tb-fini-execute! tb plusargs))

         (tb-print-output tb verbosity colorize?)

         (with-output-to-file (append-path (tb-work-path tb) LOG_FILE_NAME)
           (lambda () (tb-print-output tb 'verbose #f)))

         (when (not keep-output?)
           (clear-testbench-output tb)))

       testbenches))

  ;; Print summary
  (print-summary testbenches colorize?))

;;;
;;; Force clear all testbench outputs
;;;
(define (force-delete-outputs tb-list work-path-base)
  (for-each
   (lambda (tb)
     (let* ((base (or work-path-base (tb-base-path tb)))
            (prefix (format "~a~a-~a"
                            WORK_DIR_PREFIX (tb-filename tb)
                            (string->filename (tb-name tb))))
            (folders (map
                      basename
                      (filter
                       (lambda (f) (eq? 'directory (stat:type (stat f))))
                       (list-dir base)))))

       (for-each
        (lambda (folder)
          (let ((folder (append-path base folder)))
            (printf "Delete '~a'\n" folder)
            (delete-recursive folder)))
        (filter (cut string-prefix? prefix <>) folders))))
   tb-list))

;;;
;;; Set base path and script file name for testbenches
;;;
(define (testbenches-link-to-file! tb-list filename)
  (let ((path (canonicalize-path (dirname filename)))
        (name (basename filename)))
    (for-each (lambda (tb)
                (tb-set-base-path! tb path)
                (tb-set-filename! tb name))
              tb-list)))

;;;
;;; Check pass of testbenches
;;;
(define (testbenches-pass? tb-list)
  (every
   (lambda (tb)
     (and (tb-init-pass? tb)
          (tb-fini-pass? tb)
          (every test-pass? (tb-tests tb))))
   tb-list))

;;;
;;; Parse command line options with SRFI-37
;;;
(define (opt-get opts name)
  (let ((opt (assq name opts)))
    (if opt
        (cdr opt)
        #f)))

(define (parse-opts args . opt-spec)
  (define (opt-set opts name value)
    (if (assq name opts)
        (map (lambda (opt)
               (if (eq? (car opt) name)
                   (cons name value)
                   opt))
             opts)
        (alist-cons name value opts)))

  (define (opt-add opts name value)
    (if (assq name opts)
        (opt-set opts name
                 (cons value
                       (opt-get opts name)))
        (alist-cons name `(,value) opts)))

  (args-fold
   ;; args
   args
   ;; options
   (map (lambda (spec)
          (let* ((names (first spec))
                 (type (second spec))
                 (name (first names))
                 (char (second names))
                 (req? (eq? type 'required))
                 (opt? (eq? type 'optional))
                 (many? (eq? type 'multiple)))
            (option (list (symbol->string name) char)
                    (or many? req?)
                    opt?
                    (if many?
                        (lambda (opt nm arg opts rest error)
                          (values (if arg
                                      (opt-add opts name arg)
                                      opts)
                                  rest
                                  error))
                        (lambda (opt nm arg opts rest error)
                          (values (opt-set opts name (if arg arg #t)) rest error))))))
        opt-spec)
   ;; unrecognized options
   (lambda (opt name arg opts rest error)
     (values opts rest name))
   ;; operands
   (lambda (operand opts rest error)
     (values opts (cons operand rest) error))
   ;; seeds
   '() '() #f))

;;;
;;; Print log level verilog defines
;;;
(define (print-verilog-defines)
  (define (* . fmt) (apply printf fmt) (newline))
  (let ((tags
         (map (lambda (tag)
                (cons
                 (string-upcase
                  (list->string
                   (map (lambda (c)
                          (if (or (char-alphabetic? c)
                                  (char-numeric? c))
                              c #\_))
                        (string->list
                         (symbol->string (car tag))))))
                 (cdr tag)))
              (filter car output-tags))))
    (when (not (null? tags))
      (* "`ifndef HUNTEST_VERILOG_DEFINES")
      (* " `define HUNTEST_VERILOG_DEFINES")
      (* "")
      (* "// Log level string prefixes for use with $display function.")
      (* "// Example usage: $display(\"%s~a message\", `LOG_~a);"
         (string-capitalize (caar tags)) (caar tags))
      (for-each
       (lambda (tag)
         (* " `define LOG_~a \"~a\"" (car tag) (cdr tag)))
       tags)
      (* "")
      (* "// Dirty hacked redefine of $display function. Must be used with two parentheses.")
      (* "// Example usage: `log_~a((\"~a message\"));"
         (string-downcase (caar tags))
         (string-capitalize (caar tags)))
      (for-each
       (lambda (tag)
         (* " `define log_~a(msg)  begin $display({`LOG_~a, $sformatf msg}); end"
            (string-downcase (car tag)) (car tag)))
       tags)
      (* "`endif"))))

;;;
;;; Parameters for running testbench from upper level code
;;;
(define %run-standalone% (make-parameter #t))

;;;
;;; Print help
;;;
(define (print-help app-name)
  (define (* . fmt) (apply printf fmt) (newline))
  (* "Usage: ~a [OPTION]... [PLUSARGS]" app-name)
  (* "Run testbenches")
  (* "")
  (* "Options:")
  (* "  -Q, --query <QUERY>  Regexp query string.")
  (* "  -k, --keep           Do not delete work directory if test is pass.")
  (* "  -s, --static         Use static work dir for initial debug purposes.")
  (* "                       This option also enable keep option.")
  (* "  -w, --work <PATH>    Work path. Default: base path of the script.")
  (* "  -c, --color          Colorize output.")
  (* "  -i, --nopar          Sequential execution.")
  (* "  -C, --clean          Delete work folders.")
  (* "  -f, --defines        Print useful Verilog defines.")
  (when (not (%run-standalone%))
    (* "  -r, --recursive      Recursive search for script files.")
    (* "  -x, --regex <REGEX>  Regular expression for searching script files. Default: '~a'" TEST_SCRIPT_REGEX))
  (* "  -l, --list           List testbenches.")
  (* "  -v, --verbose        Verbose output.")
  (* "  -q, --quiet          Quiet output.")
  (* "  -V, --version        Print version.")
  (* "  -h, --help           Print this message and exit.")
  (* "")
  (* "PLUSARGS:")
  (* "  A plussarg is any argument beginning with +.")
  (* "  For example: +arg, +var=val, +define+MACRO=123")
  (* "")
  (* "QUERY:")
  (* "  QUERY is a string like a 'testbench-regexp::test-regexp'.")
  (* "  Query used to filter testbenches and tests by their names.")
  (* "  Each part of the query is a regular expression or an empty string.")
  (* "")
  (* "  For example:")
  (* "    '::'                   -> select all tests in all testbenches.")
  (* "    '^tb0$::'              -> select testbenches with name exactly 'tb0'.")
  (* "    'testbench::test [13]' -> select testbenches with name contains 'testbench'")
  (* "                              and tests with name contains 'test 1' or 'test 3'.")
  (* "    '::test 1'             -> setect tests with name contains 'test 1'.")
  (* "    'testbench 1'          -> select testbenches witn name contains 'testbench 1'.")
  (* "")
  (* "Source code and issue tracker: <https://github.com/punzik/huntest>"))

;;;
;;; Parse application options
;;;
(define (app-options cmdl)
  (let ((opt-list
         (append
          (if (%run-standalone%)
              '()
              '(((recursive #\r) none)
                ((regex #\x) required)))
          '(((query #\Q) required)
            ((verbose #\v) none)
            ((quiet #\q) none)
            ((keep #\k) none)
            ((color #\c) none)
            ((static #\s) none)
            ((work #\w) required)
            ((nopar #\i) none)
            ((clean #\C) none)
            ((list #\l) none)
            ((defines #\f) none)
            ((version #\V) none)
            ((help #\h) none)))))
    (let-values (((opts rest err)
                  (apply parse-opts (cons (cdr cmdl) opt-list))))
      (cond
       ;; Unknown options
       (err
        (eprintf "ERROR: Unknown option '~a'\n\n" err)
        (print-help (car cmdl))
        (exit -1))
       ;; Help
       ((opt-get opts 'help)
        (print-help (car cmdl))
        (exit 0))
       ;; Print version
       ((opt-get opts 'version)
        (printf "~a, Version ~a\n" (car cmdl) APP_VERSION)
        (exit 0))
       ;; Print defines
       ((opt-get opts 'defines)
        (print-verilog-defines)
        (exit 0))
       ;; OK
       (else
        (let ((opts (alist-cons
                     'plusargs
                     (filter (cut string-prefix? "+" <>) rest)
                     (alist-cons
                      'rest
                      (remove (cut string-prefix? "+" <>) rest)
                      (alist-cons
                       'parallel
                       (not (opt-get opts 'nopar))
                       opts)))))
          (cut opt-get opts <>)))))))

;;;
;;; Run testbench
;;;
(define* (run . testbenches)
  (let ((testbenches (list-flatten testbenches)))
    (if (%run-standalone%)
        ;; For standalone run
        (let* ((cmdl (command-line))
               (opt (app-options cmdl))
               (testbenches
                (if (opt 'query)
                    (filter-testbenches testbenches (opt 'query))
                    testbenches)))

          (testbenches-link-to-file! testbenches (car cmdl))

          (cond
           ;; dry run
           ((opt 'list)
            (for-each
             (cut tb-print-only <> (opt 'colorize))
             testbenches))

           ;; clean outputs
           ((opt 'clean)
            (force-delete-outputs testbenches (opt 'work-base)))

           ;; run testbenches
           (else
            (run-testbenches! testbenches
                              #:base-path      (opt 'work-base)
                              #:plusargs       (opt 'plusargs)
                              #:verbosity      (cond
                                                ((opt 'verbose) 'verbose)
                                                ((opt 'quiet) 'quiet)
                                                (else #f))
                              #:keep-output?   (or (opt 'keep-output) (opt 'static-output))
                              #:colorize?      (opt 'colorize)
                              #:static-output? (opt 'static-output)
                              #:parallel?      (opt 'parallel))
            (when (not (testbenches-pass? testbenches))
              (exit -1))))

          (exit 0))

        ;; For run from upper level code
        testbenches)))

;;;
;;; Load testbenches from files
;;;
(define (load-testbenches files)
  (fold
   (lambda (file tbs)
     (with-exception-handler
         (lambda (e)
           (eprintf "ERROR: load testbench script ~a\n" file)
           (eprintf "~a\n\n" e)
           tbs)
       (lambda ()
         (let ((t (list-flatten (load file))))
           (if (or (testbench? t)
                   (and (list? t)
                        (every testbench? t)))
               (begin
                 (testbenches-link-to-file! t file)
                 (append tbs t))
               (begin
                 (eprintf "ERROR: no testbench loaded from ~a\n" file)
                 tbs))))
       #:unwind? #t))
   '() files))

;;;
;;; MAIN
;;;
(define (main cmdl)
  ;; Find files and execute testbenches
  (parameterize ((%run-standalone% #f))
    (let* ((opt (app-options cmdl))

           (file-filter
            (let ((regex (make-regexp (or (opt 'regex) TEST_SCRIPT_REGEX))))
              (lambda (file type)
                (and (eq? type 'regular)
                     (regexp-exec regex file)))))
           (files
            (sort-uniq
             (map (cut canonicalize-path <>)
                  (if (null? (opt 'rest))
                      (find-files file-filter (getcwd) #:recursive (opt 'recursive))
                      (fold
                       (lambda (f files)
                         (with-exception-handler
                             (lambda (e)
                               (eprintf "ERROR: can't stat file ~a\n" f)
                               files)
                           (lambda ()
                             (let ((t (stat:type (stat f))))
                               (cond
                                ((eq? t 'regular)
                                 (cons f files))
                                ((eq? t 'directory)
                                 (append files (find-files file-filter f #:recursive (opt 'recursive))))
                                (else files))))
                           #:unwind? #t))
                       '() (opt 'rest))))
             string<)))

      (let ((testbenches
             ((if (opt 'query)
                  (cut filter-testbenches <> (opt 'query))
                  (lambda (x) x))
              (load-testbenches files))))
        (cond
         ;; dry run
         ((opt 'list)
          (for-each
           (cut tb-print-only <> (opt 'colorize))
           testbenches))

         ;; clean outputs
         ((opt 'clean)
          (force-delete-outputs testbenches (opt 'work-base)))

         ;; run testbenches
         (else
          (run-testbenches! testbenches
                            #:base-path      (opt 'work-base)
                            #:plusargs       (opt 'plusargs)
                            #:verbosity      (cond
                                              ((opt 'verbose) 'verbose)
                                              ((opt 'quiet) 'quiet)
                                              (else #f))
                            #:keep-output?   (or (opt 'keep-output) (opt 'static-output))
                            #:colorize?      (opt 'colorize)
                            #:static-output? (opt 'static-output)
                            #:parallel?      (opt 'parallel))
          (when (not (testbenches-pass? testbenches))
            (exit -1))))

        (exit 0)))))

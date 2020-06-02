#lang racket

(require json)
(require "../src/programs.rkt")


(define op-map
  (make-hash
   `((expt . pow)
     (sqr . exp2)
     (abs . fabs))))

(define version-10-var-map
  (make-hash
   `((pi . PI)
     (e . E))))

(define (format-op expr)
  (if (equal? (first expr) 'cube)
      (cons 'pow (append (rest expr) (list 3)))
      (cons (hash-ref op-map (first expr) (first expr)) (rest expr))))


(define (format-expr is-version-10 expr)
  (match expr
    [(? string?)
     (let ([parsed (string->number expr)])
       (if parsed
           parsed
           (raise (error "string that is not a num"))))]
    [(list op args ...)
     (format-op (cons op (map (curry format-expr is-version-10) args)))]
    [(? symbol?)
     (if is-version-10
         (hash-ref version-10-var-map expr expr)
         expr)]
    [else
     expr]))


(define (read-expr expr-string is-version-10)
  (format-expr is-version-10 (call-with-input-string expr-string read)))

(define (make-fpcore expr)
  (let ([vars (free-variables expr)])
    (format "(FPCore (~a) ~a)"
            (string-join (map symbol->string vars) " ")
            expr)))

(define (convert-file file-name existing-set is-version-10)
  (define file-port (open-input-file (build-path (current-directory) file-name)))
  (define tests (hash-ref (read-json file-port) 'tests))
  (define exprs-unfiltered
    (for/list ([test tests])
      (read-expr (hash-ref test 'input) is-version-10)))
  (define exprs
    (for/set ([expr exprs-unfiltered]
              #:when (not (set-member? existing-set expr)))
      expr))
  exprs)
  

(define (output-exprs exprs output-file)
  (for ([expr exprs])
    (fprintf output-file "~a\n" (make-fpcore expr))))

(define (get-expr-set json-files expr-set is-version-10)
  (cond
    [(empty? json-files)
     expr-set]
    [else
     (begin
       (define new-expr-set
         (set-union expr-set (convert-file (first json-files) expr-set is-version-10)))
       (get-expr-set (rest json-files) new-expr-set #f))]))

(define (string-less expr1 expr2)
  (string>? (~a expr1) (~a expr2)))

(define (sort-exprs exprs)
  (sort (set->list exprs) string-less))
  
(module+ main
  (define rebuilding? #f)
  (command-line 
   #:program "convert-demo"
   #:args (bench-folder . json-files)
   (define output-file
     (open-output-file (build-path
                        (current-directory)
                        bench-folder
                        "submitted.fpcore")
                        #:exists 'replace))
   (define exprs (get-expr-set json-files (set) #t))
   (define sorted (sort-exprs exprs))
   (output-exprs sorted output-file)))


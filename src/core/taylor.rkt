#lang racket

(require math/number-theory)
(require "../common.rkt")
(require "../function-definitions.rkt")
(require "../programs.rkt")
(require "reduce.rkt")

(provide approximate)

(module+ test (require rackunit))

(define (approximate expr vars #:transform [tforms #f]
                     #:terms [terms 3] #:iters [iters 5])
  "Take a Taylor expansion in multiple variables, with at most `terms` terms."

  (when (not tforms)
    (set! tforms (map (const (cons identity identity)) vars)))
  (set! expr
        (for/fold ([expr expr]) ([var vars] [tform tforms])
          (replace-expression expr var ((car tform) var))))
  (debug #:from 'approximate "Taking taylor expansion of" expr "in" vars "around" 0)

  ; This is a very complex routine, with multiple parts.
  ; Some of the difficulty is due to the use of bounded Laurent series and their encoding.

  ; The first step is to determine the minimum exponent of each variable.
  ; We do this by taking a taylor expansion in each variable and considering the minimal term.

  (define offsets (for/list ([var (reverse vars)]) (car (taylor var expr))))

  ; We construct a multivariable Taylor expansion by taking a Taylor expansion in one variable,
  ; then expanding each coefficient in the second variable, and so on.
  ; We cache the computation of any expansion to speed this process up.

  (define taylor-cache (make-hash))
  (hash-set! taylor-cache '() (taylor (car vars) expr))

  ; This is the memoized expansion-taking.
  ; The argument, `coeffs`, is the "uncorrected" degrees of the terms--`offsets` is not subtracted.

  (define/contract (get-taylor coeffs)
    (-> (listof exact-nonnegative-integer?) any/c)
    (hash-ref! taylor-cache coeffs
               (λ ()
                  (let* ([oc (get-taylor (cdr coeffs))]
                         [expr* ((cdr oc) (car coeffs))])
                    (if (= (length coeffs) (length vars))
                      (simplify expr*)
                      (let ([var (list-ref vars (length coeffs))])
                        (taylor var expr*)))))))

  ; Given some uncorrected degrees, this gets you an offset to apply.
  ; The corrected degrees for uncorrected `coeffs` are (map - coeffs (get-offset coeffs))

  (define/contract (get-offset coeffs)
    (-> (listof exact-nonnegative-integer?) any/c)
    (if (null? coeffs)
      (car (get-taylor '()))
      (cons (car (get-taylor (cdr coeffs))) (get-offset (cdr coeffs)))))

  ; Given some corrected degrees, this gets you the uncorrected degrees, or #f
  (define get-coeffs-hash (make-hash))

  (define (get-coeffs expts)
    (-> (listof exact-nonnegative-integer?) any/c)
    (hash-ref! get-coeffs-hash expts
               (λ ()
                 (if (null? expts)
                     '()
                     ; Find the true coordinate of our tail
                     (let ([etail (get-coeffs (cdr expts))])
                       (if etail
                           ; Get the offset from our head
                           (let ([offset-head (car (get-taylor etail))])
                             ; Sometimes, our head exponent is too small
                             (if (< (car expts) (- offset-head))
                                 #f
                                 (cons (+ (car expts) offset-head) etail)))
                           #f))))))

  ; We must now iterate through the coefficients in `corrected` order.
  (simplify
   (apply
    make-sum
    ; We'll track how many non-trivial zeros we've seen
    ; and all the useful terms we've seen so far
    (let loop ([empty 0] [res '()] [i 0])
      ; We stop once we've seen too many non-trivial zeros in a row or we have enough terms
      (if (or (> empty iters) (>= (length res) terms))
          res
          ; `expts` is the corrected degrees, `coeffs` is the uncorrected degrees
          (let* ([expts (map - (iterate-diagonal (length vars) i) offsets)]
                 [coeffs (get-coeffs expts)])
            (if (not coeffs)
                (loop empty res (+ 1 i))
                (let ([coeff (for/fold ([coeff (get-taylor coeffs)]) ([var vars] [tform tforms])
                               (replace-expression coeff var ((cdr tform) var)))])
                  (if (equal? coeff 0)
                      (loop (+ empty 1) res (+ 1 i))
                      (loop 0 (cons (make-term coeff
                                               (reverse
                                                (for/list ([var vars] [tform tforms])
                                                  ((cdr tform) var)))
                                               expts) res) (+ 1 i)))))))))))

(define (variary->binary op identity args)
  (match args
    ['() identity]
    [(list x) x]
    [(list x xs ...)
     (list op x (variary->binary op identity xs))]))

(define (make-sum . terms)
  (define-values (consts vars)
    (partition (and/c number? exact?) terms))
  (variary->binary
   '+ 0
   (match (apply + consts) [0 vars] [n (cons n vars)])))

(module+ test
  (check-equal? (make-sum 0 1 2 3 'x) '(+ 6 x))
  (check-equal? (make-sum 0 'x) 'x)
  (check-equal? (make-sum 0 'x 'y) '(+ x y))
  (check-equal? (make-sum 0) 0)
  (check-equal? (make-sum -1 1) 0))

(define (make-neg term)
  (match term
    [(? number?) (- term)]
    [`(- ,term2) term2]
    [_ term]))

(define (make-sub head . terms)
  (match head
    [0 (make-neg (apply make-sum terms))]
    [_ (make-sum head (make-neg (apply make-sum terms)))]))

(define (make-prod . terms)
  (define-values (consts vars)
    (partition (and/c number? exact?) terms))
  (variary->binary
   '* 1
   (match (apply * consts) [0 '(0)] [1 vars] [n (cons n vars)])))

(module+ test
  (check-equal? (make-prod 1 2 3 'x) '(* 6 x))
  (check-equal? (make-prod 1 'x) 'x)
  (check-equal? (make-prod 1 'x 'y) '(* x y))
  (check-equal? (make-prod 0 'x 'y) 0)
  (check-equal? (make-prod 1) 1)
  (check-equal? (make-prod 6 1/6) 1))

(define (make-quot num dem)
  (cond
   [(and (number? num) (number? dem) (exact? num) (exact? dem) (not (= dem 0))) (/ num dem)]
   [(equal? dem 1) num]
   [else `(/ ,num ,dem)]))

(define (make-monomial var power)
  (cond
   [(and (number? var) (exact? var)) (expt var power)]
   [(equal? power 0)   1]
   [(equal? power 1)   var]
   [(equal? power -1) `(/ 1 ,var)]
   [(positive? power) `(pow ,var ,power)]
   [(negative? power) `(pow ,var ,power)]))

(define (make-term head vars expts)
  ; We prefer (pow (* x y) 3) over (* (pow x 3) (pow y 3)),
  ; so we first extract the GCD of the exponents and put that exponentiation outside
  (define outside-expt (apply gcd expts))
  (if (equal? outside-expt 0)
      head ; Only if all expts are 0
      (make-prod
       head
       (make-monomial
        (apply make-prod (map make-monomial vars (map (curryr / outside-expt) expts)))
        outside-expt))))

(define n-sum-to-cache (make-hash))

(define (n-sum-to n k)
  (hash-ref! n-sum-to-cache (cons n k)
             (λ ()
                (cond
                 [(= k 0) (list (build-list n (const 0)))]
                 [(= n 1) (list (list k))]
                 [(= n 0) '()]
                 [else
                  (apply append
                         (for/list ([i (in-range 0 (+ k 1))])
                           (map (curry cons i) (n-sum-to (- n 1) (- k i)))))]))))

(define (iterate-diagonal dim i)
  (let loop ([i i] [sum 0])
    (let ([seg (n-sum-to dim sum)])
      (if ((length seg) . <= . i)
          (loop (- i (length seg)) (+ sum 1))
          (list-ref seg i)))))

(define taylor-expansion-known
  '(+ - * / sqr sqrt exp sin cos log pow))

(define (taylor var expr*)
  "Return a pair (e, n), such that expr ~= e var^n"
  (debug #:from 'taylor "Taking taylor expansion of" expr* "in" var)
  (define expr
    (if (and (list? expr*) (not (set-member? taylor-expansion-known (car expr*))))
        ((get-expander taylor-expansion-known) expr*)
        expr*))
  (unless (equal? expr expr*)
    (debug #:from 'taylor "Rewrote expression to" expr))
  (match expr
    [(? (curry equal? var))
     (taylor-exact 0 1)]
    [(? constant?)
     (taylor-exact expr)]
    [(? variable?)
     (taylor-exact expr)]
    [`(+ ,args ...)
     (apply taylor-add (map (curry taylor var) args))]
    [`(- ,arg)
     (taylor-negate ((curry taylor var) arg))]
    [`(- ,arg ,args ...)
     (apply taylor-add ((curry taylor var) arg) (map (compose taylor-negate (curry taylor var)) args))]
    [`(* ,left ,right)
     (taylor-mult (taylor var left) (taylor var right))]
    [`(/ 1 ,arg)
     (taylor-invert (taylor var arg))]
    [`(/ ,num ,den)
     (taylor-quotient (taylor var num) (taylor var den))]
    [`(sqr ,a)
     (let ([ta (taylor var a)])
       (taylor-mult ta ta))]
    [`(sqrt ,arg)
     (taylor-sqrt (taylor var arg))]
    [`(exp ,arg)
     (let ([arg* (normalize-series (taylor var arg))])
       (if (positive? (car arg*))
           (taylor-exact expr)
           (taylor-exp (zero-series arg*))))]
    [`(sin ,arg)
     (let ([arg* (normalize-series (taylor var arg))])
       (cond
        [(positive? (car arg*))
         (taylor-exact expr)]
        [(= (car arg*) 0)
         ; Our taylor-sin function assumes that a0 is 0,
         ; because that way it is especially simple. We correct for this here
         ; We use the identity sin (x + y) = sin x cos y + cos x sin y
         (taylor-add
          (taylor-mult (taylor-exact `(sin ,((cdr arg*) 0))) (taylor-cos (zero-series arg*)))
          (taylor-mult (taylor-exact `(cos ,((cdr arg*) 0))) (taylor-sin (zero-series arg*))))]
        [else
         (taylor-sin (zero-series arg*))]))]
    [`(cos ,arg)
     (let ([arg* (normalize-series (taylor var arg))])
       (cond
        [(positive? (car arg*))
         (taylor-exact expr)]
        [(= (car arg*) 0)
         ; Our taylor-cos function assumes that a0 is 0,
         ; because that way it is especially simple. We correct for this here
         ; We use the identity cos (x + y) = cos x cos y - sin x sin y
         (taylor-add
          (taylor-mult (taylor-exact `(cos ,((cdr arg*) 0))) (taylor-cos (zero-series arg*)))
          (taylor-negate
           (taylor-mult (taylor-exact `(sin ,((cdr arg*) 0))) (taylor-sin (zero-series arg*)))))]
        [else
         (taylor-cos (zero-series arg*))]))]
    [`(log ,arg)
     (let* ([arg* (normalize-series (taylor var arg))]
            [rest (taylor-log (cdr arg*))])
       (if (zero? (car arg*))
           rest
           (cons 0
                 (λ (n)
                    (if (= n 0)
                        (simplify (make-sum (make-prod (- (car arg*)) `(log ,var))
                                            ((cdr rest) 0)))
                        ((cdr rest) n))))))]
    [`(pow ,(? (curry equal? var)) ,(? exact-integer? power))
     (cons (- power) (λ (n) (if (= n 0) 1 0)))]
    [`(pow ,base ,power)
     (taylor var `(exp (* ,power (log ,base))))]
    [_
     (taylor-exact expr)]))

; A taylor series is represented by a function f : nat -> expr,
; representing the coefficients (the 1 / n! terms not included),
; and an integer offset to the exponent

(define (taylor-exact . terms)
  (define items (list->vector (map simplify terms)))
  (cons 0
        (λ (n)
           (if (<= (length terms) n)
               0
               (vector-ref items n)))))

(define (first-nonzero-exp f)
  "Returns n, where (series n) != 0, but (series n) = 0 for all smaller n"
  (let loop ([n 0])
    (if (and (equal? (f n) 0) (< n 20))
        (loop (+ n 1))
        n)))

(define (align-series . serieses)
  (if (or (<= (length serieses) 1) (apply = (map car serieses)))
      serieses
      (let ([offset* (car (argmax car serieses))])
        (for/list ([series serieses])
          (let ([offset (car series)])
            (cons offset* (λ (n)
                             (if (< (+ n (- offset offset*)) 0)
                                 0
                                 ((cdr series) (+ n (- offset offset*)))))))))))

(define (taylor-add . terms)
  (match-define (list (cons offset serieses) ...) (apply align-series terms))
  (let ([hash (make-hash)])
    (cons (car offset)
          (λ (n)
            (hash-ref! hash n
                       (λ () (simplify (apply make-sum (for/list ([series serieses]) (series n))))))))))

(define (taylor-negate term)
  (cons (car term) (λ (n) (simplify (make-neg ((cdr term) n))))))

(define (taylor-mult left right)
  (cons (+ (car left) (car right))
        (let ([hash (make-hash)])
          (lambda (n)
            (hash-ref! hash n
                       (λ ()
                         (simplify
                          (apply make-sum
                           (for/list ([i (range (+ n 1))])
                             (make-prod ((cdr left) i) ((cdr right) (- n i))))))))))))

(define (normalize-series series)
  "Fixes up the series to have a non-zero zeroth term,
   allowing a possibly negative offset"
  (match series
    [(cons offset coeffs)
     (let ([slack (first-nonzero-exp coeffs)])
       (cons (- offset slack) (compose coeffs (curry + slack))))]))

(define ((zero-series series) n)
  (if (< n (- (car series))) 0 ((cdr series) (+ n (car series)))))

(define (taylor-invert term)
  "This gets tricky, because the function might have a pole at 0.
   This happens if the inverted series doesn't have a constant term,
   so we extract that case out."
  (match (normalize-series term)
    [(cons offset b)
     (let ([hash (make-hash)])
       (hash-set! hash 0 (simplify (make-quot 1 (b 0))))
       (letrec ([f (λ (n)
                      (hash-ref! hash n
                                 (λ ()
                                    (simplify
                                     (make-neg
                                      (apply
                                       make-sum
                                       (for/list ([i (range n)])
                                         (make-prod (f i) (make-quot (b (- n i)) (b 0))))))))))])
         (cons (- offset) f)))]))

(define (taylor-quotient num denom)
  "This gets tricky, because the function might have a pole at 0.
   This happens if the inverted series doesn't have a constant term,
   so we extract that case out."
  (match (cons (normalize-series num) (normalize-series denom))
    [(cons (cons noff a) (cons doff b))
     (let ([hash (make-hash)])
       (hash-set! hash 0 (simplify (make-quot (a 0) (b 0))))
       (letrec ([f (λ (n)
                      (hash-ref! hash n
                                 (λ ()
                                    (simplify
                                     (make-sub
                                      (make-quot (a n) (b 0))
                                      (apply
                                       make-sum
                                       (for/list ([i (range n)])
                                         (make-prod (f i) (make-quot (b (- n i)) (b 0))))))))))]
                [offset (- noff doff)])
         (cons offset f)))]))

(define (taylor-sqrt num)
  (let* ([num* (normalize-series num)]
         [offset (car num*)]
         [offset* (if (even? offset) offset (+ offset 1))]
         [coeffs (cdr num*)]
         [coeffs* (if (even? offset) coeffs (λ (n) (if (= n 0) 0 (coeffs (- n 1)))))]
         [hash (make-hash)])
    (define sc0
      (match (coeffs* 0)
        [(? (and/c number? exact?) n)
         (if (exact? (sqrt n)) (sqrt n) `(sqrt ,n))]
        [expr `(sqrt ,expr)]))
    (hash-set! hash 0 (simplify sc0))
    (hash-set! hash 1 (simplify (make-quot (coeffs* 1) (make-prod 2 sc0))))
    (letrec ([f (λ (n)
                   (hash-ref! hash n
                              (λ ()
                                 (simplify
                                  (cond
                                   [(even? n)
                                    (make-quot
                                     (make-sub
                                      (coeffs* n) (make-monomial (f (/ n 2)) 2)
                                      (apply
                                       make-sum
                                       (for/list ([k (in-naturals 1)] #:break (>= k (- n k)))
                                         (make-prod 2 (f k) (f (- n k))))))
                                     (make-prod 2 (f 0)))]
                                   [(odd? n)
                                    (make-quot
                                     (make-sub
                                      (coeffs* n)
                                      (apply
                                       make-sum
                                       (for/list ([k (in-naturals 1)] #:break (>= k (- n k)))
                                         (make-prod 2 (f k) (f (- n k))))))
                                     (make-prod 2 (f 0)))])))))])
      (cons (/ offset* 2) f))))

(define (rle l)
  (for/list ([run (group-by identity l)])
    (cons (length run) (car run))))

(define (partition-list n)
  (define (aux n k)
    (cond
     [(= n 0) '(())]
     [(< n k) '()]
     [else
      (append (map (curry cons k) (aux (- n k) k))
              (aux n (+ k 1)))]))
  (map rle (aux n 1)))

(define (taylor-exp coeffs)
  (let* ([hash (make-hash)])
    (define c0 (simplify (match (coeffs 0) [0 1] [n `(exp ,n)])))
    (hash-set! hash 0 c0)
    (cons 0
          (λ (n)
            (hash-ref! hash n
                       (λ ()
                         (simplify
                          (make-prod
                           c0
                           (apply make-sum
                                  (for/list ([p (partition-list n)])
                                    (apply
                                     make-prod
                                     (for/list ([factor p])
                                       (make-quot
                                        (make-monomial (coeffs (cdr factor)) (car factor))
                                        (factorial (car factor)))))))))))))))

(define (taylor-sin coeffs)
  (let ([hash (make-hash)])
    (hash-set! hash 0 0)
    (cons 0
          (λ (n)
            (hash-ref! hash n
                       (λ ()
                         (simplify
                          (apply
                           make-sum
                           (for/list ([p (partition-list n)])
                             (if (= (modulo (apply + (map car p)) 2) 1)
                                 (apply
                                  make-prod
                                  (if (= (modulo (apply + (map car p)) 4) 1) 1 -1)
                                  (for/list ([factor p])
                                    (make-quot (make-monomial (coeffs (cdr factor)) (car factor))
                                               (factorial (car factor)))))
                                 0))))))))))

(define (taylor-cos coeffs)
  (let ([hash (make-hash)])
    (hash-set! hash 0 1)
    (cons 0
          (λ (n)
            (hash-ref! hash n
                       (λ ()
                         (simplify
                          (apply
                           make-sum
                           (for/list ([p (partition-list n)])
                             (if (= (modulo (apply + (map car p)) 2) 0)
                                 (apply
                                  make-prod
                                  (if (= (modulo (apply + (map car p)) 4) 0) 1 -1)
                                  (for/list ([factor p])
                                    (make-quot (make-monomial (coeffs (cdr factor)) (car factor))
                                               (factorial (car factor)))))
                                 0))))))))))

;; This is a hyper-specialized symbolic differentiator for log(f(x))

(define initial-logtable '((1 -1 1)))

(define (list-setinc l i)
  (let loop ([i i] [l l] [rest '()])
    (if (= i 0)
        (if (null? (cdr l))
            (append (reverse rest) (list (- (car l) 1) 1))
            (append (reverse rest) (list* (- (car l) 1) (+ (cadr l) 1) (cddr l))))
        (loop (- i 1) (cdr l) (cons (car l) rest)))))

(define (loggenerate table)
  (apply append
         (for/list ([term table])
           (match term
             [`(,coeff ,ps ...)
              (filter identity
                      (for/list ([i (in-naturals)] [p ps])
                        (if (zero? p)
                            #f
                            `(,(* coeff p) ,@(list-setinc ps i)))))]))))

(define (lognormalize table)
  (filter (λ (entry) (not (= (car entry) 0)))
          (for/list ([entry (group-by cdr table)])
            (cons (apply + (map car entry))
                  (cdar entry)))))

(define (logstep table)
  (lognormalize (loggenerate table)))

(define logcache (make-hash (list (cons 1 '((1 -1 1))))))
(define logbiggest 1)

(define (logcompute i)
  (hash-ref! logcache i
             (λ ()
                (logstep (logcompute (- i 1))))))

(define (taylor-log coeffs)
  "coeffs is assumed to start with a nonzero term"
  (let ([hash (make-hash)])
    (define c0 (simplify (match (coeffs 0) [1 0] [n `(log ,n)])))
    (hash-set! hash 0 c0)
    (cons 0
          (λ (n)
            (hash-ref! hash n
                       (λ ()
                         (let* ([tmpl (logcompute n)])
                           (simplify
                            (make-quot
                             (apply
                              make-sum
                              (for/list ([term tmpl])
                                (match term
                                  [`(,coeff ,k ,ps ...)
                                   (make-prod coeff 
                                              (make-quot
                                                (apply
                                                 make-prod
                                                 (for/list ([i (in-naturals 1)] [p ps])
                                                   (if (= p 0)
                                                       1
                                                       (make-monomial (make-prod (factorial i) (coeffs i)) p))))
                                                 (make-monomial (coeffs 0) (- k))))])))
                              (factorial n))))))))))

(module+ test
  (check-pred exact-integer? (car (taylor 'x '(pow x 1.0)))))

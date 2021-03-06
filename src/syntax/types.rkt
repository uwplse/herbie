#lang racket

(require math/bigfloat)

(provide (struct-out type) get-type type-name? bigvalue? value-of bigvalue-of)
(module+ internals (provide define-type))

(struct type (name exact? inexact? exact->inexact inexact->exact))

(define type-dict (make-hash))
(define-syntax-rule (define-type name (exact? inexact?) e->i i->e)
  (hash-set! type-dict 'name (type 'name exact? inexact? e->i i->e)))

(define (type-name? x) (hash-has-key? type-dict x))
(define (get-type x) (hash-ref type-dict x))

(define-type real (real? bigfloat?)
  bf bigfloat->flonum)

(define-type bool (boolean? boolean?)
  identity identity)

(define (value-of type) (type-exact? (hash-ref type-dict type))) ; used once (eval-application in src/programs.rkt)
(define (bigvalue-of type) (type-inexact? (hash-ref type-dict type))) ; not used

; used 3 times (src/float.rkt and src/syntax/syntax.rkt)
; counterpart value? is in src/interface.rkt since the definition of what is a value is
; now repr specific
(define (bigvalue? x) (for/or ([(type rec) (in-hash type-dict)]) ((type-inexact? rec) x)))

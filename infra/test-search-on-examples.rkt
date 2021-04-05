#lang racket
(require "../src/sampling.rkt" "../src/interface.rkt" "../src/syntax/sugar.rkt")

(define (perform-test lines)
  (define total 0)
  (define search-saved 0)
  (define groups (group-by (lambda (ele) (second ele)) lines))
  (for ([group groups])
       (define prog-unsugar (second (first group)))
       (*var-reprs* (for/list ([var (second prog-unsugar)])
                              (cons var (get-representation 'binary64))))
       (define prog `(lambda ,(second prog-unsugar)
                        ,(desugar-program (third prog-unsugar)
                                          (get-representation 'binary64) (*var-reprs*))))
       
       (println prog)
       (flush-output)
       (define sampler (make-sampler (get-representation 'binary64) `(lambda ,(second prog) TRUE)
                                     (list prog) empty #t))
       (for ([line group])
            (when (and (sampler (third line)) (fourth (fourth line))) ;; only domain errors
                  (set! search-saved (add1 search-saved)))
            (set! total (add1 total))
            (println (list search-saved total)))))
  

(module+ main
  (command-line #:program "test-search"
    #:args (example-file)
    (perform-test (for/list ([line (in-port read (open-input-file example-file))])
                            line))))
    

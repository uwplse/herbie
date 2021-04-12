#lang racket
(require "../src/sampling.rkt" "../src/interface.rkt" "../src/syntax/sugar.rkt")

(define (perform-test lines)
  (define total 0)
  (define search-saved 0)
  (define search-saved-unsamplable 0)
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
            (define sampler-saved (sampler (third line)))
            (when (and sampler-saved (fourth (fourth line))) ;; only domain errors
                  (set! search-saved (add1 search-saved)))
            (when (and sampler-saved (sixth (fourth line)))
                  (set! search-saved-unsamplable (add1 search-saved-unsamplable)))
            
            (set! total (add1 total))
            (println (list total search-saved search-saved-unsamplable)))))
  

(module+ main
  (command-line #:program "test-search"
    #:args (example-file)
    (perform-test (for/list ([line (in-port read (open-input-file example-file))])
                            line))))
    

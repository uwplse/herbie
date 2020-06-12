#lang racket

(define (read-lines port)
  (define line (read port))
  (if (equal? line eof)
      empty
      (cons line (read-lines port))))


(define (fpcore-less-than fpcore fpcore2)
  (string>? (~a (rest (rest fpcore))) (~a (rest (rest fpcore2)))))

(define (sort-fpcores fpcores)
  (sort fpcores fpcore-less-than))

(module+ main
  (command-line 
   #:program "sort"
   #:args (json-file)
   (for ([line (sort-fpcores (read-lines (open-input-file json-file)))])
     (displayln line))))

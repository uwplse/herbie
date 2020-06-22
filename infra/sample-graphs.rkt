#lang racket

(require plot/no-gui json)

(define (sample-type? entry)
  (equal? (hash-ref entry 'type "nothing") "sample"))

(define (by-precision entry entry2)
  (< (hash-ref entry 'precision) (hash-ref entry2 'precision)))

(define (filter-outcome category outcomes)
  (filter (compose (curry equal? category) (curryr hash-ref 'category)) outcomes))

;; example row: {"count":796,"program":"body","category":"valid","precision":160,"time":57.04443359375}
(define (draw-outcomes-for title category yaxis ylabel divby outcomes timeline-dir)
  (define filtered (filter-outcome category outcomes))
  (define sorted (sort filtered by-precision))
  (define histogram-data
    (map (lambda (entry) (vector (hash-ref entry 'precision) (/ (hash-ref entry yaxis) divby))) sorted))

  (define y-max (if (empty? histogram-data) 1
                    (* 1.25 (apply max (map (curryr vector-ref 1) histogram-data)))))
  (define output-file
    (open-output-file (build-path timeline-dir (string-append category "-" (symbol->string yaxis) ".png"))
                      #:exists 'replace))
  
  (plot-file (discrete-histogram histogram-data)
             output-file
             'png
             #:x-label "Precision" #:y-label ylabel
             #:y-max y-max #:y-min 0 #:x-min 0 #:x-max (length histogram-data)
             #:title title))

(define (get-counts entries precisions)
  (for/list ([precision precisions])
      (define found (findf (lambda (entry) (equal? (hash-ref entry 'precision) precision)) entries))
      (vector precision (if found (hash-ref found 'count) 0))))
  

(define (draw-movability-chart outcomes timeline-dir)
  (define overflowed (filter-outcome "overflowed" outcomes))
  (define exited (filter-outcome 'exit outcomes))
  (define precisions (set-union (list->set (map (curryr hash-ref 'precision) overflowed))
                                (list->set (map (curryr hash-ref 'precision) exited))))
  
  (define overflowed-data
    (get-counts overflowed precisions))
  (define exited-data (get-counts exited precisions))
  (define output-file
    (open-output-file (build-path timeline-dir (string-append "movability.png"))
                      #:exists 'replace))

  (plot-file (list (discrete-histogram
                    overflowed-data
                    #:skip 2.5 #:x-min 0
                    #:label "Detected")
                   (discrete-histogram
                    exited-data
                    #:skip 2.5 #:x-min 1
                    #:label "Undetected" #:color 2 #:line-color 2))
             output-file
             'png
             #:x-label "Precision"
             #:y-label "Count"
             #:title "Unsamplable Points Detected at Varying Precision"))


(define (draw-graphs data timeline-dir)
  (define sample-types (filter sample-type? data))
  (when (not (equal? (length sample-types) 1))
    (error "got more than one sample phase"))
  (define outcomes (hash-ref (first sample-types) 'outcomes))
  (draw-outcomes-for "Time Spent Finding Valid Points"
                     "valid" 'time "Time (seconds)" 1000 outcomes timeline-dir)
  (draw-outcomes-for "Time Spent Finding Invalid Points"
                     "invalid" 'time "Time (seconds)" 1000 outcomes timeline-dir)
  (draw-outcomes-for "Number of Valid Points" "valid" 'count "Count" 1 outcomes timeline-dir)
  (draw-outcomes-for "Number of Invalid Points" "invalid" 'count "Count" 1 outcomes timeline-dir)

  (draw-movability-chart outcomes timeline-dir))
  

(module+ main
  (command-line
   #:program "sample-graphs"
   #:args (timeline-json-file output-folder)
   (when (not (directory-exists? output-folder))
     (make-directory output-folder))
   (draw-graphs (read-json (open-input-file timeline-json-file)) output-folder)))

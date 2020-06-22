#lang racket

(require plot/no-gui json)

(define (sample-type? entry)
  (equal? (hash-ref entry 'type "nothing") "sample"))

(define (by-precision entry entry2)
  (< (hash-ref entry 'precision) (hash-ref entry2 'precision)))

;; example row: {"count":796,"program":"body","category":"valid","precision":160,"time":57.04443359375}
(define (draw-outcomes-for category yaxis ylabel divby outcomes timeline-dir)
  (define filtered (filter (compose (curry equal? category) (curryr hash-ref 'category)) outcomes))
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
             #:y-max y-max #:y-min 0 #:x-min 0 #:x-max (length histogram-data)))

(define (draw-graphs data timeline-dir)
  (define sample-types (filter sample-type? data))
  (when (not (equal? (length sample-types) 1))
    (error "got more than one sample phase"))
  (define outcomes (hash-ref (first sample-types) 'outcomes))
  (draw-outcomes-for "valid" 'time "Time (seconds)" 1000 outcomes timeline-dir)
  (draw-outcomes-for "invalid" 'time "Time (seconds)" 1000 outcomes timeline-dir)
  (draw-outcomes-for "valid" 'count "Count" 1 outcomes timeline-dir)
  (draw-outcomes-for "invalid" 'count "Count" 1 outcomes timeline-dir))
  

(module+ main
  (command-line
   #:program "sample-graphs"
   #:args (timeline-json-file)
   (define-values (timeline-dir timeline-file is-dir) (split-path (string->path timeline-json-file)))
   (draw-graphs (read-json (open-input-file timeline-json-file)) timeline-dir)))

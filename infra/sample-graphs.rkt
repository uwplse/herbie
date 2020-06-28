#lang racket

(require plot/no-gui json)

(define (sample-type? entry)
  (equal? (hash-ref entry 'type "nothing") "sample"))
(define (analyze-type? entry)
  (equal? (hash-ref entry 'type "nothing") "analyze"))

(define (by-precision entry entry2)
  (< (hash-ref entry 'precision) (hash-ref entry2 'precision)))

(define (filter-outcomes categories outcomes)
  (filter (compose (curryr member categories) (curryr hash-ref 'category)) outcomes))
(define (filter-outcome category outcomes)
  (filter-outcomes (list category) outcomes))

(define (get-counts entries precisions field)
  (for/list ([precision precisions])
    (define all-with-precision (filter (lambda (entry) (equal? (hash-ref entry 'precision) precision)) entries))
    (vector precision (apply + (map (curryr hash-ref field) all-with-precision)))))


(define (precision-data outcomes-list data-field divby)
  (define precisions
   (sort
    (set->list
     (apply set-union
            (for/list ([outcomes outcomes-list])
              (list->set (map (curryr hash-ref 'precision) outcomes)))))
    <))

  (for/list ([precision precisions])
    (vector precision
            (for/list ([outcomes outcomes-list])
              (define all-with-precision (filter (lambda (entry) (equal? (hash-ref entry 'precision) precision)) outcomes))
              (/ (apply + (map (curryr hash-ref data-field) all-with-precision)) divby)))))

;; example row: {"count":796,"program":"body","category":"valid","precision":160,"time":57.04443359375}
(define (draw-outcomes-for filename title yaxis ylabel divby timeline-dir outcomes-list)
  (define histogram-data
    (precision-data
     outcomes-list
     'time
     divby))
  
  (define y-max (if (empty? histogram-data) 1
                    (* 1.25 (apply max (map (compose (curry apply +) (curryr vector-ref 1))
                                            histogram-data)))))
  
  (define output-file
    (open-output-file (build-path timeline-dir (string-append filename ".png"))
                      #:exists 'replace))
  
  (plot-file
   (stacked-histogram histogram-data #:colors `(2 1) #:line-colors `(2 1))
   output-file
   'png
   #:x-label "Precision" #:y-label ylabel
   #:y-max y-max #:y-min 0 #:x-min 0 #:x-max (length histogram-data)
   #:title title))

(define (get-total-points-tagged outcomes tags)
  (apply + (map (curryr hash-ref 'count)
                (filter (lambda (outcome)
                          (member (hash-ref outcome 'category) tags)) outcomes))))

(define (draw-search-chart data timeline-dir outcomes)
  (define analyze-outcome (first (filter analyze-type? data)))
  (define rows (hash-ref analyze-outcome 'sampling))
  (define histogram-data
    (for/list ([row rows])
      (define total (apply + (rest row)))
      (vector (first row) (map (lambda (n) (/ n total))
                               (rest row)))))

  (define actual-valid
    (get-total-points-tagged outcomes (list "valid")))
  (define actual-invalid
    (get-total-points-tagged outcomes (list "false" "invalid" "nan" "overflowed" "exit")))
  (define actual-total (+ actual-valid actual-invalid))
  (define height-actual-sampled
    (+ (first (vector-ref (last histogram-data) 1))
       (second (vector-ref (last histogram-data) 1))))
  
  
  (define actual-data
    (list
     (vector "*"
             (list (* height-actual-sampled (/ actual-valid actual-total))
                   0
                   (* height-actual-sampled (/ actual-invalid actual-total))))))

  
  (define output-file
    (open-output-file (build-path timeline-dir  "search-by-iter.png")
                      #:exists 'replace))
  (parameterize ([plot-width (* 40 (+ 2 (length histogram-data)))])
    (plot-file
     (list (stacked-histogram histogram-data)
           (stacked-histogram actual-data
                              #:alphas (list 0.8 0.8 0.8)
                              #:x-min (+ 1 (length histogram-data))))
     output-file
     'png
     #:x-label "Iteration"
     #:y-label "Proportion of Input Space"
     #:y-max 1.25
     #:title "Analysis of Input Space by Search Over Iterations")))

(define (draw-movability-chart outcomes timeline-dir)
  (define overflowed (filter-outcome "overflowed" outcomes))
  (define exited (filter-outcome "exit" outcomes))
  (define precisions (sort (map (curryr hash-ref 'precision) overflowed) <))
                                                 

  (when (not (equal? (length exited) 1))
    (error "multiple exited rows"))
  (define overflowed-data
    (get-counts overflowed precisions 'count))
  (define exited-data (list (vector 'Undetected (hash-ref (first exited) 'count))))
  (define output-file
    (open-output-file (build-path timeline-dir (string-append "movability.png"))
                      #:exists 'replace))
  
  (parameterize ([plot-width (* 80 (length overflowed-data))])
    (plot-file (list (discrete-histogram
                      overflowed-data
                      #:skip 1.5 #:x-min 0
                      #:color 3 #:line-color 3)
                     (discrete-histogram
                      exited-data
                      #:x-min (+ (* 1.5 (length overflowed-data)) 1.5)
                      #:color 1 #:line-color 1))
               output-file
               'png
               #:x-label "Precision"
               #:y-label "Count"
               #:title "Problematic Points Detected at Varying Precision")))


(define (draw-overall-graphs data timeline-dir)
  (define sample-types (filter sample-type? data))
  (when (not (equal? (length sample-types) 1))
    (error "got more than one sample phase"))
  (define outcomes (hash-ref (first sample-types) 'outcomes))
  (draw-outcomes-for "valid-invalid" "Time Spent Finding Valid and Invalid Points"
                     'time "Time (minutes)" 60000 timeline-dir
                     (list (filter-outcomes (list "valid") outcomes)
                           (filter-outcomes (list "nan" "exit" "false" "overflowed" "invalid") outcomes)))

  (draw-outcomes-for "false-found" "Points Which Fail Precondition"
                     'count "Count" 1 timeline-dir
                     (list (filter-outcome "false" outcomes)))
  
  (draw-outcomes-for "nan-found" "Points with Domain Error Detected"
                     'count "Count" 1 timeline-dir
                     (list (filter-outcome "nan" outcomes)))

  (draw-outcomes-for "infinite-found" "Infinite Points Detected"
                     'count "Count" 1 timeline-dir
                     (list (filter-outcome "invalid" outcomes)))
  
  
  (draw-movability-chart outcomes timeline-dir)
  (draw-search-chart data timeline-dir outcomes))


(define (find-timeline-files timeline-file)
  (define-values (timeline-folder extension isdir) (split-path timeline-file))
  (define dir (map (curry build-path timeline-folder) (directory-list timeline-folder)))
  
  (define folders (filter directory-exists? dir))
  (filter file-exists? (map (lambda (folder) (build-path folder "timeline.json")) folders)))

(define (last-dir-name file)
  (define-values (folder extension isdir) (split-path file))
  (define-values (root folder-name isdir2) (split-path folder))
  (path->string folder-name))

(define (->percent num)
  (string-append "$" (~r (* num 100) #:precision 1) "\\%$"))

(define (make-suite-table timeline-file output-folder)
  (define sub-timeline-files (find-timeline-files timeline-file))
  (define bench-names (map last-dir-name sub-timeline-files))
  (define bench-datas
    (for/list ([timeline sub-timeline-files])
      (define types (filter analyze-type? (read-json (open-input-file timeline))))
      (when (not (equal? (length types) 1))
        (error "didn't find just one analyze timeline step"))
      (define analyze-data (hash-ref (first types) 'sampling))
      ;; iter true other false
      (define final-iter (last analyze-data))
      (define total (apply + (rest final-iter)))
      (map (lambda (n) (->percent (/ n total))) (rest final-iter))))

  (define output-file
    (open-output-file (build-path output-folder "search-by-suite.txt") #:exists 'replace))
  (displayln "Benchmark Suite & $T$ & $O$ & $F$ \\\\" output-file)
  (for ([bench-name bench-names] [bench-data bench-datas])
    (displayln (string-append (string-join (cons bench-name bench-data) " & ") " \\\\") output-file)))

(module+ main
  (command-line
   #:program "sample-graphs"
   #:args (timeline-json-file output-folder)
   (when (not (directory-exists? output-folder))
     (make-directory output-folder))
   (draw-overall-graphs (read-json (open-input-file timeline-json-file)) output-folder)
   (make-suite-table timeline-json-file output-folder)))

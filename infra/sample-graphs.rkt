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

(define (get-counts entries precisions)
  (for/list ([precision precisions])
      (define found (findf (lambda (entry) (equal? (hash-ref entry 'precision) precision)) entries))
      (vector precision (if found (hash-ref found 'count) 0))))


(define (precision-data outcomes-list data-field)
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
              (apply + (map (curryr hash-ref data-field) all-with-precision))))))

;; example row: {"count":796,"program":"body","category":"valid","precision":160,"time":57.04443359375}
(define (draw-outcomes-for filename title yaxis ylabel divby timeline-dir outcomes-list outcomes-names)
  (define histogram-data
    (precision-data
     outcomes-list
     'time))
  
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

  

(define (draw-movability-chart outcomes timeline-dir)
  (define overflowed (filter-outcome "overflowed" outcomes))
  (define exited (filter-outcome "exit" outcomes))
  (define precisions (sort (set->list (set-union (list->set (map (curryr hash-ref 'precision) overflowed))
                                                 (list->set (map (curryr hash-ref 'precision) exited))))
                           <))
  
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


(define (draw-overall-graphs data timeline-dir)
  (define sample-types (filter sample-type? data))
  (when (not (equal? (length sample-types) 1))
    (error "got more than one sample phase"))
  (define outcomes (hash-ref (first sample-types) 'outcomes))
  (draw-outcomes-for "valid-invalid" "Time Spent Finding Valid and Invalid Points"
                     'time "Time (minutes)" 60000 timeline-dir
                     (list (filter-outcomes (list "valid") outcomes)
                           (filter-outcomes (list "nan" "exit" "false" "overflowed" "invalid") outcomes))
                     (list "Valid" "Invalid"))

  (draw-outcomes-for "types-invalid" "Types Of Invalid Points With Search Disabled"
                     'count "Count" 1 timeline-dir
                     (list (filter-outcome "false" outcomes)
                           (filter-outcome "nan" outcomes)
                           (filter-outcome "invalid" outcomes))
                     (list "Failed Precondition"
                           "Domain Error"
                           "Infinite"))
  
  (draw-movability-chart outcomes timeline-dir))


(define (find-timeline-files timeline-file)
  (define-values (timeline-folder extension isdir) (split-path timeline-file))
  (define dir (map (curry build-path timeline-folder) (directory-list timeline-folder)))
  
  (define folders (filter directory-exists? dir))
  (filter file-exists? (map (lambda (folder) (build-path folder "timeline.json")) folders)))

(define (last-dir-name file)
  (define-values (folder extension isdir) (split-path file))
  (define-values (root folder-name isdir2) (split-path folder))
  (path->string folder-name))

(define (average . values)
  (/ (apply + values) (length values)))

(define (with-bench-names bench-names data)
  (for/list ([bench-name bench-names] [point data])
    (vector bench-name (* 100 point))))


(define (draw-suite-graphs timeline-file output-folder)
  (define sub-timeline-files (find-timeline-files timeline-file))
  (define bench-names (map last-dir-name sub-timeline-files))
  
  (define bench-data
    (for/list ([timeline sub-timeline-files])
      (define types (filter analyze-type? (read-json (open-input-file timeline))))
      (when (not (equal? (length types) 1))
        (error "didn't find just one analyze timeline step"))
      (define analyze-data (hash-ref (first types) 'sampling))
      (apply map average analyze-data)))

  (define range-analysis-saved
    (with-bench-names bench-names (map (lambda (data) (- 1 (first data))) bench-data)))
  (define search-saved
    (with-bench-names bench-names (map (lambda (data) (- 1 (second data))) bench-data)))
  (define guaranteed-chance
    (with-bench-names bench-names (map third bench-data)))

  (define output-file
    (open-output-file (build-path output-folder "search-by-suite.png") #:exists 'replace))
  (parameterize ([plot-width (* 120 (length bench-names))])
    (plot-file (list (discrete-histogram
                      range-analysis-saved
                      #:skip 3.5 #:x-min 0
                      #:label "Average Space Saved By Range Analysis")
                     (discrete-histogram
                      search-saved
                      #:skip 3.5 #:x-min 1
                      #:label "Average Space Saved By Search and Range Analysis" #:color 2 #:line-color 2)
                     (discrete-histogram
                      guaranteed-chance
                      #:skip 3.5 #:x-min 2
                      #:label "Average Guaranteed Chance to Sample Valid Point" #:color 4 #:line-color 4))
               #:y-max 150
               output-file
               'png
               #:x-label "Benchmark Suite"
               #:y-label "Percentage"
               #:title "Statistics on Rival's Sampler By Benchmark Suite")))


(module+ main
  (command-line
   #:program "sample-graphs"
   #:args (timeline-json-file output-folder)
   (when (not (directory-exists? output-folder))
     (make-directory output-folder))
   (draw-overall-graphs (read-json (open-input-file timeline-json-file)) output-folder)
   #;(draw-suite-graphs timeline-json-file output-folder)))

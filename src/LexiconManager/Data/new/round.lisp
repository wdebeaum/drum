;;;;
;;;; W::ROUND
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::ROUND W::TRIP)
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("round_trip%1:04:00"))
     (LF-PARENT ONT::round-TRIP)
     (example "he made a round trip in eight hours")
     (preference .98) ;; prefer the adj sense
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ((w::round w::trip)
   (SENSES
    ((meta-data :origin plow :entry-date 20060531 :change-date 20090818 :comments pq405)
     (example "a round trip ticket")
     (LF-PARENT ONT::route-topology-val)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::ROUND
   (wordfeats (W::MORPH (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("round%3:00:00"))
     (LF-PARENT ONT::round-val)
     )
    )
   )
))


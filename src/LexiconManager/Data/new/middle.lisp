;;;;
;;;; W::MIDDLE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::MIDDLE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::center)
     (example "the middle of the road")
;     (TEMPL PART-OF-RELN-TEMPL)
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    #|
    ((LF-PARENT ONT::TIME-POINT)
     (example "the middle of the meeting/lesson")
     (meta-data :origin cardiac :entry-date 20081005 :change-date nil :comments nil :wn ("middle%1:28:00"))
     (TEMPL GEN-PART-OF-RELN-ACTION-TEMPL)
     )
    ((LF-PARENT ONT::TIME-POINT)
     (example "the middle of the year")
     (meta-data :origin cardiac :entry-date 20081005 :change-date nil :comments nil :wn ("middle%1:28:00"))
     (TEMPL GEN-PART-OF-RELN-INTERVAL-TEMPL)
     )
    |#
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((w::middle w::school)
   (SENSES
    ((LF-PARENT ONT::academic-institution)
     (EXAMPLE "send email from school")
     (meta-data :origin asma :entry-date 20111003 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::middle
   (SENSES
    ((meta-data :origin step :entry-date 20080528 :change-date nil :comments nil)
     (LF-PARENT ONT::middle-val)
     (preference .98) ;; prefer relational noun sense
     )
    (
     (LF-PARENT ONT::MIDDLE-LOCATION-VAL)
     )    
    )
   )
))

(define-words :pos W::adj 
 :tags (:base500)
 :words (
  (W::middle
   (wordfeats (W::ALLOW-POST-N1-SUBCAT +))
   (SENSES
    ((meta-data :origin step :entry-date 20080528 :change-date nil :comments nil)
     (LF-PARENT ONT::MIDDLE-LOCATION-VAL)
     (TEMPL ADJ-CO-THEME-TEMPL (XP (% W::PP (W::PTYPE (? p W::in W::of)))))
     )
    ))))

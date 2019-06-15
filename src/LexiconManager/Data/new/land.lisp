;;;;
;;;; W::LAND
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::LAND
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("land%1:17:00"))
     (LF-PARENT ONT::land)
     )
    )
   )
))

(define-words :pos W::v 
 :tags (:base500)
 :words (
  (W::land
    (wordfeats (W::morph (:forms (-vb) :nom w::landing)))
   (SENSES
        ;;;; the helicopter landed
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("pocket-9.10-1"))
     (LF-PARENT ONT::ARRIVE)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-TEMPL)
     )
    ;; he landed the helicopter
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("pocket-9.10-1"))
     (LF-PARENT ONT::ARRIVE)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))


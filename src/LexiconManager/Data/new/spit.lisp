;;;;
;;;; W::spit
;;;;

(define-words :pos W::V 
 :words (
  (W::spit
   (wordfeats (W::morph (:forms (-vb) :past W::spat :ing W::spitting)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("spit%2:29:00"))
     (LF-PARENT ONT::bodily-process)
     (TEMPL agent-affected-xp-templ) ; like vomit
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("spit%2:29:00"))
     (LF-PARENT ONT::bodily-process)
     (TEMPL agent-templ) ; like bleed
     )
    )
   )
))


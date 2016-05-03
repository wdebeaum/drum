;;;;
;;;; W::rage
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
   (W::rage
     (wordfeats (W::morph (:forms (-vb) :nom w::rage)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("marvel-31.3-2"))
     (LF-PARENT ONT::disliking)
     (TEMPL agent-neutral-xp-templ (xp (% w::pp (w::ptype (? p w::about w::against))))) ; like mind,worry
     )
    ((example "He raged")
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab :vn ("entity_specific_cos-45.5"))
     (LF-PARENT ONT::bodily-process)
     (TEMPL agent-templ) 
     )
    )
   )
))


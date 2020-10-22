;;;;
;;;; W::THROUGH
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::THROUGH
   (SENSES
    ((LF-PARENT ONT::through)
     ;(TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     )
    ((LF-PARENT ONT::DIRECTION)
     (TEMPL PRED-S-POST-TEMPL)
     )
    #||((LF-PARENT ONT::REASON)
     (TEMPL binary-constraint-s-templ )
     (Example "to let the head fall forward through drowsiness")
     (meta-data :origin gloss-training :entry-date 20100217 :change-date nil :comments nil)
     (preference .98)
     )||#
    
   ((LF-PARENT ONT::BY-MEANS-OF)
     (TEMPL BINARY-CONSTRAINT-S-subjcontrol-TEMPL)
     (EXAMPLE "he killed it through immersing it in water")
    )

    ((LF-PARENT ONT::BY-MEANS-OF)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (EXAMPLE "through this method")
     )

    ((LF-PARENT ONT::COMPLETELY)
     ;(TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (TEMPL PARTICLE-TEMPL)
     (EXAMPLE "I looked through the files")
     )

   )
   )))


;;;;
;;;; W::report
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
 (W::report
   (SENSES
    ((meta-data :origin obtw :entry-date 20111011)
     (LF-PARENT ONT::document)
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  ;They reported fifty people to be killed.
  (W::report
   (wordfeats (W::morph (:forms (-vb) :nom W::report)))
   (SENSES
    (;;(LF-parent ONT::announce)
     (lf-parent ont::assert)
     (Example "He reported that they solved the problem")
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::cp (W::ctype W::s-that))))
     (meta-data :origin "verbnet-1.5-corrected" :entry-date 20060214 :change-date 20090506 :comments nil :vn ("say-37.7")) 
     )
    (;;(LF-parent ONT::announce)
     (lf-parent ont::assert)
     (Example "He reported the results [to him]")
     (TEMPL AGENT-FORMAL-AGENT1-OPTIONAL-TEMPL)
     (meta-data :origin "verbnet-1.5-corrected" :entry-date 20060214 :change-date 20090506 :comments nil :vn ("say-37.7")) 
     )
     (;;(LF-parent ONT::announce)
      (lf-parent ont::assert)
     (Example "He reported them ready")
     (TEMPL AGENT-NEUTRAL-FORMAL-OBJCONTROL-TEMPL)
     (meta-data :origin trips :entry-date 20090406 :change-date 20090506 :comments nil :vn ("say-37.7")) 
     )
     ;; he reported them killed
    (;;(LF-parent ONT::announce)
     (lf-parent ont::assert)
     (Example "He reported them to be ready")
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL (xp (% W::cp (W::ctype W::s-to))))
     (meta-data :origin trips :entry-date 20090406 :change-date 20090506 :comments nil :vn ("say-37.7")) 
     )	   
    )
   )
))


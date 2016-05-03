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

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  ;They reported fifty people to be killed.
  (W::report
   (wordfeats (W::morph (:forms (-vb) :nom W::report)))
   (SENSES
    (;;(LF-parent ONT::announce)
     (lf-parent ont::assert)
     (Example "He reported that they solved the problem")
     (TEMPL agent-theme-xp-templ (xp (% W::cp (W::ctype W::s-that))))
     (meta-data :origin "verbnet-1.5-corrected" :entry-date 20060214 :change-date 20090506 :comments nil :vn ("say-37.7")) 
     )
    (;;(LF-parent ONT::announce)
     (lf-parent ont::assert)
     (Example "He reported the results [to him]")
     (TEMPL agent-theme-to-addressee-optional-templ)
     (meta-data :origin "verbnet-1.5-corrected" :entry-date 20060214 :change-date 20090506 :comments nil :vn ("say-37.7")) 
     )
     (;;(LF-parent ONT::announce)
      (lf-parent ont::assert)
     (Example "He reported them ready")
     (TEMPL agent-neutral-complex-templ)
     (meta-data :origin trips :entry-date 20090406 :change-date 20090506 :comments nil :vn ("say-37.7")) 
     )
     ;; he reported them killed
    (;;(LF-parent ONT::announce)
     (lf-parent ont::assert)
     (Example "He reported them to be ready")
     (TEMPL agent-theme-objcontrol-templ (xp (% W::cp (W::ctype W::s-to))))
     (meta-data :origin trips :entry-date 20090406 :change-date 20090506 :comments nil :vn ("say-37.7")) 
     )	   
    )
   )
))


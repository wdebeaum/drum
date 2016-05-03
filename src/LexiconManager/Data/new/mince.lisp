;;;;
;;;; W::mince
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::mince
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("carve-21.2-1"))
     (LF-PARENT ONT::crush)
 ; like mash,squash,squish,crush,bruise
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("carve-21.2-1"))
     (LF-PARENT ONT::cut)
 ; like grate
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("mince%2:38:00"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))


;;;;
;;;; w::spray
;;;;

(define-words :pos W::n
 :words (
  (w::spray
  (senses
   ((LF-PARENT ONT::substance)
    (TEMPL count-PRED-TEMPL)
    (meta-data :origin cardiac :entry-date 20090402 :change-date nil :comments nil)
    )
   )
)
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
;   )
 (W::SPRAY
   (SENSES
   
    ;; alternative considerations for roles here: co-theme-theme; actor-theme
    ((meta-data :origin monroe :entry-date 20031223 :change-date 20040617 :comments s7)
     (lf-parent ont::push-liquid) ;; used to be ont::emit 
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     (templ agent-affected-xp-templ)
     (example "the fire hydrant sprayed water")
     )
    ((meta-data :origin caet :entry-date 20110114 :change-date nil :comments nil)
     (lf-parent ont::push-liquid)
     (templ affected-templ)
     (example "the water sprayed across the sidewalk")
     )
    )
   )
))


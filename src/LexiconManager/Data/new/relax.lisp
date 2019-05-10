;;;;
;;;; W::relax
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(W::relax
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("amuse-31.1") :wn ("relax%2:29:01"))
     (LF-PARENT ont::experiencer-obj)
     (example "the muscles relaxed")
     (TEMPL affected-templ) 
     (preference .98) ;; prefer ont::subduing 
     )
;    (;(LF-PARENT ont::wait)
;     (LF-PARENT ONT::evoke-calm)
;     (example "relax")
;     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
;     (TEMPL affected-TEMPL)
;     ;(preference .96) ;; prefer transitive
;     (meta-data :origin plow :entry-date 20050922 :change-date nil :comments nil)
;     )
    ((LF-PARENT ont::evoke-comfort)
     (example "a massage will relax him")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-affected-xp-TEMPL)
     (meta-data :origin plow :entry-date 20050922 :change-date nil :comments nil)
     )
    ;; need another sense for "relax the constraints"
    )
   )
))


;;;;
;;;; W::miss
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::miss
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("admire-31.2") :wn ("miss%2:37:00"))
     (LF-PARENT ONT::misses)
      ;(TEMPL experiencer-action-objcontrol-templ) ; like suffer
     (TEMPL EXPERIENCER-FORMAL-SUBJCONTROL-TEMPL  (xp (% W::VP (W::vform W::ing))))
     (example "I miss eating oranges")
     (PREFERENCE 0.96)
     )
    
    ((LF-PARENT ONT::miss)
     (TEMPL agent-neutral-XP-TEMPL)
     (meta-data :origin fruitcarts :entry-date 20050427 :change-date nil :comments nil)
     (example "miss the target")
     )
    ((LF-PARENT ONT::misses)
     (SEM (F::Aspect F::indiv-level))
     (example "I miss oranges")
     (TEMPL experiencer-neutral-templ)
     )
    )
   )
))


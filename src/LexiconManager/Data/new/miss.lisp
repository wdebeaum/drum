;;;;
;;;; W::miss
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::miss
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("admire-31.2") :wn ("miss%2:37:00"))
     (LF-PARENT ONT::misses)
     (TEMPL experiencer-action-objcontrol-templ) ; like suffer
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


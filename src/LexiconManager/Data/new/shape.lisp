;;;;
;;;; W::SHAPE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::SHAPE
   (SENSES
    ((meta-data :origin calo :entry-date 20031229 :change-date nil :comments html-purchasing-corpus :wn ("shape%1:07:00" "shape%1:03:00"))
     (LF-PARENT ONT::shape)
     (TEMPL OTHER-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::shape
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090504 :comments nil :vn ("build-26.1-1"))
     (LF-PARENT ONT::shape-change)
     (TEMPL agent-affected-result-templ (xp (% w::pp (w::ptype w::into)))) ; like carve
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::in-relation)
     (example "a thing shaped like a box")
     (meta-data :origin fruitcarts :entry-date 20050418 :change-date nil :comments nil)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-theme-xp-templ (xp (% W::PP (W::ptype W::like)))))
     )
    )
   ))


;;;;
;;;; W::mix
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::mix (W::up))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("mix-22.1-1-1"))
     (LF-PARENT ONT::combine-objects)
     (TEMPL agent-affected2-optional-templ (xp (% w::pp (w::ptype w::with)))) ; like combine
     (PREFERENCE 0.96)
     )
    ((EXAMPLE "He mixes them up ")
     (LF-PARENT ONT::Confuse)
     (TEMPL agent-theme-xp-templ)
     )
    )
   )
))

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::mix
 (wordfeats (W::morph (:forms (-vb) :nom w::mix)))
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil :vn ("mix-22.1-1-1"))
   (LF-PARENT ONT::combine-objects)
   (example "mix the ingredients")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
  )
 )
))

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
((w::mix (w::in))
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::combine-objects)
   (example "mix in the remaining ingredients")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
  )
 )
))


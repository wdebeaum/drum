;;;;
;;;; W::mix
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::mix (W::up))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("mix-22.1-1-1"))
     (LF-PARENT ONT::combine-objects)
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype w::with)))) ; like combine
     (PREFERENCE 0.96)
     )
    ((EXAMPLE "He mixes them up ")
     (LF-PARENT ONT::Confuse)
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     )
    )
   )
))

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
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
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
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


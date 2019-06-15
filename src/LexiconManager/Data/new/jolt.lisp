;;;;
;;;; W::jolt
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::jolt
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("amuse-31.1") :wn ("jolt%2:37:00"))
     (LF-PARENT ont::provoke)
     (example "the news jolted him [into action]")
     (TEMPL AGENT-AFFECTED-FORMAL-XP-OPTIONAL-B-TEMPL (xp (% w::pp (w::ptype (? pt w::into w::to))))) ; like annoy,bother,concern,hurt
     )
    )
   )
))


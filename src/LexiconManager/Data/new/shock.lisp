;;;;
;;;; W::shock
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::shock
   (wordfeats (W::morph (:forms (-vb) :nom w::shock)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("amuse-31.1") :wn ("shock%2:37:00" "shock%2:37:01" "shock%2:37:02"))
     (LF-PARENT ont::provoke)
     (example "it shocked him into silence")
     (TEMPL agent-affected-effect-optional-templ (xp (% w::pp (w::ptype (? pt w::into w::to))))) ; like annoy,bother,concern,hurt
       )
    ; it shocked him into running for office
    )
   )
))


;;;;
;;;; W::agitate
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::agitate
  (wordfeats (W::morph (:forms (-vb) :nom w::agitation)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("agitate%2:37:00"))
     (LF-PARENT ONT::evoke-excitement)
     (TEMPL agent-affected-xp-templ)
     )
;    ((meta-data :origin "wordnet-3.0" :entry-date 20090515 :change-date nil :comments nil)
;     (LF-PARENT ONT::evoke-hurt)
;     (TEMPL agent-affected-xp-templ)
;     )
    )
   )
))

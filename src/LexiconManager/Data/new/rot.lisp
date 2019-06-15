;;;;
;;;; W::rot
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::rot
   (wordfeats (W::morph (:forms (-vb) :past W::rotten :pastpart w::rotted :ing W::rotting)))
   (SENSES
    #|
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5") :wn ("rot%2:30:00"))
      (templ agent-affected-xp-templ)
     (LF-PARENT ONT::deteriorate)
 ; like ferment
     (syntax (w::resultative +))
     )
    |#
    ((meta-data :origin step :entry-date 20080623 :change-date 20090504 :comments nil)
     (LF-PARENT ONT::deteriorate)
     (example "the bridge rotted")
     (templ affected-templ)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))


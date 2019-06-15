;;;;
;;;; W::consume
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(W::consume
   (wordfeats (W::morph (:forms (-vb) :nom w::consumption)))
   (SENSES
    #||((meta-data :origin calo-ontology :entry-date 20051205 :change-date nil :comments nil)
     (LF-PARENT ONT::USE)
     (example "this process consumes all the resources")
     (SEM (F::ASPECT F::DYNAMIC))
     )||#
    ((meta-data :origin calo-ontology :entry-date 20051205 :change-date nil :comments nil)
     (LF-PARENT ONT::consume)
     (example "how much bread does he consume")
     (SEM (F::ASPECT F::DYNAMIC))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))


;;;;
;;;; w::arrest
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (w::arrest
     (wordfeats (W::morph (:forms (-vb) :nom w::arrest)))
     (senses
      ((lf-parent ont::stop)
       (example "arrest the process")
       (templ agent-affected-xp-templ)	     
       (meta-data :origin calo-ontology :entry-date 20060713 :change-date 20090506 :comments caloy3)
       )
      )
     )
))


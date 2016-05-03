;;;;
;;;; w::commit
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(w::commit
 (senses
  ((LF-parent ont::promise)
   (Example "He committed to the deadline")
   (TEMPL AGENT-THEME-optional-TEMPL(xp (% W::PP (W::ptype W::to))))
   (meta-data :origin calo-ontology :entry-date 20060125 :change-date nil :comments meeting-understanding) 
   )
  )
 )
))


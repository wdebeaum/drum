;;;;
;;;; w::consent
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(w::consent
 (senses
  ((LF-parent ont::promise)
   (Example "He consented to the terms")
   (TEMPL AGENT-THEME-optional-TEMPL (xp (% W::PP (W::ptype W::to))))
   (meta-data :origin calo-ontology :entry-date 20060125 :change-date nil :comments meeting-understanding) 
   )
  )
 )
))


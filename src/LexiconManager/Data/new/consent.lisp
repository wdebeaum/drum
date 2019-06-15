;;;;
;;;; w::consent
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
(w::consent
 (senses
  ((LF-parent ont::promise)
   (Example "He consented to the terms")
   (TEMPL AGENT-FORMAL-XP-PP-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))))
   (meta-data :origin calo-ontology :entry-date 20060125 :change-date nil :comments meeting-understanding) 
   )
  )
 )
))


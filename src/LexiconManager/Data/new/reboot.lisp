;;;;
;;;; W::REBOOT
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::REBOOT
   (wordfeats (W::morph (:forms (-vb) :past W::rebooted :ing W::rebooting)))
   (SENSES
    ((LF-PARENT ONT::boot-up) ;reboot)
     (example "reboot the computer")
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040210 :CHANGE-DATE NIL
		:COMMENTS HTML-PURCHASING-CORPUS)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))


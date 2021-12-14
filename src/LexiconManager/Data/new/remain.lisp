;;;;
;;;; W::remain
;;;;

(define-words :pos W::v 
 :words (
  (W::remain
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::STAY)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-TEMPL)
     )
    #|
    ((LF-PARENT ONT::STAY)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-TEMPL))
    |#
    ((example "the bulbs remain on")
     ;(LF-PARENT ONT::stay)
     ;(TEMPL AGENT-FORMAL-PRED-SUBJCONTROL-TEMPL)
     (LF-PARENT ONT::MAINTAIN-KEEP)
     (TEMPL AFFECTED-FORMAL-XP-PRED-TEMPL)
     (meta-data :origin bee :entry-date 20090220 :change-date nil :comments beetle-pilots)
     )
    )
   )
))


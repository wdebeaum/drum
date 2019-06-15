;;;;
;;;; w::boot
;;;;

(define-words :pos W::n
 :words (
  (w::boot
  (senses
   ((LF-PARENT ONT::attire)
    (meta-data :origin caloy3 :entry-date 20070330 :change-date nil :comments nil)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   ((W::BOOT (w::up))
    (wordfeats (W::morph (:forms (-vb) :past W::booted :ing W::booting)))
    (SENSES
     ((LF-PARENT ONT::boot-up)
      (META-DATA :ORIGIN CALO :ENTRY-DATE 20040210 :CHANGE-DATE NIL
		 :COMMENTS HTML-PURCHASING-CORPUS)
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
      )
     )
    )
))


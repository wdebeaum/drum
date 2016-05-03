;;;;
;;;; w::originate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (w::originate
    (wordfeats (W::morph (:forms (-vb) :nom w::origination)))
    (SENSES
     ((LF-PARENT ONT::START)
      (EXAMPLE "the idea originated in a remote province")
      (META-DATA :ORIGIN calo-ontology :ENTRY-DATE 20060426 :CHANGE-DATE NIL
		 :COMMENTS nil)
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (templ affected-create-templ)
      )
     ((LF-PARENT ONT::START)
      (EXAMPLE "he originated the idea")
      (META-DATA :ORIGIN calo-ontology :ENTRY-DATE 20060426 :CHANGE-DATE NIL
		 :COMMENTS nil)
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (templ agent-affected-create-templ)
      )
    
    )
)))


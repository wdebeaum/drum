;;;;
;;;; w::found
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (w::found
    (wordfeats (W::morph (:forms (-vb) :nom w::foundation)))
    (SENSES
     ((LF-PARENT ONT::establish)
      (EXAMPLE "he founded the program on accepted principles")
      (META-DATA :ORIGIN calo-ontology :ENTRY-DATE 20060426 :CHANGE-DATE NIL :COMMENTS nil)
      (TEMPL AGENT-AFFECTEDR-MANNER-2-XP-3-XP2-OPTIONAL-TEMPL (xp2 (% w::pp (w::ptype (? pt w::on)))))
      )
     )
    )
))


;;;;
;;;; w::found
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (w::found
    (wordfeats (W::morph (:forms (-vb) :nom w::foundation)))
    (SENSES
     ((LF-PARENT ONT::establish)
      (EXAMPLE "he founded the program on accepted principles")
      (META-DATA :ORIGIN calo-ontology :ENTRY-DATE 20060426 :CHANGE-DATE NIL :COMMENTS nil)
      (TEMPL agent-affected-create-manner-optional-templ (xp2 (% w::pp (w::ptype (? pt w::on)))))
      )
     )
    )
))


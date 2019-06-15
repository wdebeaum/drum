;;;;
;;;; W::influence
;;;;

(define-words :pos W::v 
 :words (
 (W::influence
  (wordfeats (W::morph (:forms (-vb) :nom W::influence :nomsubjpreps (w::of) :nomobjpreps (w::on))))
   (SENSES
    ((EXAMPLE "influence the voters" "this decision influences our plan")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date 20081107 :comments nil)
     (LF-PARENT ONT::objective-influence)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
     )
    )
   )
))


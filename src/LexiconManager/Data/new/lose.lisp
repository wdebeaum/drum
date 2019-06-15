;;;;
;;;; W::lose
;;;;

(define-words :pos W::v 
 :words (
  (W::lose
   (wordfeats (W::morph (:forms (-vb) :past W::lost :nom w::loss)))
   (SENSES
    ((EXAMPLE "He lost the book")
     (LF-PARENT ONT::lose)
     (syntax (w::resultative +))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060412 :change-date 20090507 :comments nil)
     )
    ((EXAMPLE "He lost.")
     (LF-PARENT ONT::lose-compete)
     (TEMPL AGENT-TEMPL)
     )
    )
   )
))


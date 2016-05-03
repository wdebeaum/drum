;;;;
;;;; w::addition
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (w::addition
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060126 :change-date nil :comments meeting-understanding)
     (LF-PARENT ONT::add-include)
     (templ other-reln-theme-templ  (xp (% W::pp (W::ptype (? pt w::to)))))
     (example "the deck is a recent addition")
     )
    )
   )
))


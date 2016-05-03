;;;;
;;;; W::cannot
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
   (W::cannot
   (SENSES
    ((LF-PARENT ONT::ABILITY)
     (lf-form w::can)
     (meta-data :origin calo-ontology :entry-date 20060201 :change-date nil :comments meeting-understanding)
     (example "it cannot fail")
     (TEMPL AUX-MODAL-TEMPL)
     (SYNTAX (W::VFORM W::PRES) (w::neg +))
     )
    ((LF-PARENT ONT::ABILITY)
     (lf-form w::can)
     (meta-data :origin calo-ontology :entry-date 20060201 :change-date nil :comments meeting-understanding)
     (example "it cannot")
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     (SYNTAX (W::VFORM W::PRES) (w::neg +) (W::changesem +))
     )
    )
   )
))


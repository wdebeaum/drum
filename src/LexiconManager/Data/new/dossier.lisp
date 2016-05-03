;;;;
;;;; W::dossier
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::dossier
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20031217 :change-date nil :wn ("dossier%1:10:00") :comments nil)
     (LF-PARENT ONT::chronicle)
     (example "prepare a dossier on him")
     (templ other-reln-templ (xp (% W::pp (W::ptype (? ptp W::for W::on)))))
     )
    )
   )
))


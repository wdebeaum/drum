;;;;
;;;; w::can
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (w::can
   (SENSES
    ((LF-PARENT ONT::small-container)
     
     (example "throw it in the trash can")
     (meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :wn ("can%1:06:00") :comments Office)
     )
    )
   )
))

(define-words :pos W::v :boost-word t 
 :tags (:base500)
 :words (
  (W::can
   (SENSES
    ;;;; I can drive a truck
    ((LF-PARENT ONT::ABILITY)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::can)
     (TEMPL AUX-MODAL-TEMPL)
     (SYNTAX (W::VFORM W::PRES))
     )
    ((LF-PARENT ONT::ABILITY)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::can)
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     (SYNTAX (W::VFORM W::PRES) (W::changesem +))
     )
    ((LF-PARENT ONT::FIRE-DISMISS)
     (preference .96)
     (TEMPL agent-affected-xp-templ)
     )
    ((LF-PARENT ONT::TRANSFORM-TO-PRESERVE)
     (preference .96)
     (TEMPL agent-affected-xp-templ)
     )
    
   )
   )))


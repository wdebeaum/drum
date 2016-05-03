;;;;
;;;; W::quote
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::quote
    (wordfeats (W::morph (:forms (-vb) :nom w::quote))) ; quotation
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060424 :change-date nil :comments nil :vn ("transfer_mesg-37.1"))
     (LF-PARENT ONT::scripted-say)
     (TEMPL agent-affected-iobj-theme-templ) ; like relay
     )
    )
   )
))


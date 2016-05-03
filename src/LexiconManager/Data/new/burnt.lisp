;;;;
;;;; W::burnt
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   ;; alternate pastpart form
  (W::burnt
   (wordfeats (W::morph (:forms NIL)) (W::vform W::pastpart))
   (SENSES
    ((meta-data :origin calo :entry-date 20040504 :change-date nil :comments html-purchasing-corpus)
     (EXAMPLE "he had already  burnt the cd")
     (LF-PARENT ONT::write)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (preference .95)
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("entity_specific_cos-45.5") :wn ("burn%2:43:01"))
     (LF-PARENT ONT::transformation)
     (TEMPL agent-affected-xp-templ) ; like ferment
     )
    )
   )
))


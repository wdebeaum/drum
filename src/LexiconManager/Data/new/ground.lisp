;;;;
;;;; W::GROUND
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
;   )
  (W::GROUND
   (SENSES
     ((meta-data :origin monroe :entry-date 20031219 :change-date nil :wn ("ground%1:17:00" "ground%1:27:00" "ground%1:17:01") :comments s3)
      (LF-PARENT ONT::land)
      )
     )
   )
))

(define-words :pos W::n
 :words (
  ((W::GROUND W::BEEF)
  (senses
	   ((LF-PARENT ONT::BEEF)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (w::ground
   (wordfeats (W::morph (:forms (-vb)))) ;; :nom w::grounding)))
    (SENSES
     (;(LF-PARENT ONT::establish)
      (LF-PARENT ONT::attribute-impute)
      (EXAMPLE "he grounded his beliefs on faith")
      (META-DATA :ORIGIN calo-ontology :ENTRY-DATE 20060620 :CHANGE-DATE NIL :COMMENTS nil)
      ;(TEMPL AGENT-AFFECTEDR-MANNER-2-XP-3-XP2-OPTIONAL-TEMPL (xp (% w::pp (w::ptype (? pt w::in w::on)))))
      (TEMPL AGENT-NEUTRAL-NEUTRAL1-2-XP-3-XP2-TEMPL (xp2 (% w::pp (w::ptype w::on))))
      )
     )
    )
))


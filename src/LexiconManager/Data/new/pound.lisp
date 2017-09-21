;;;;
;;;; W::POUND
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::POUND
 ;  (abbrev w::lb)
   (SENSES
    ((meta-data :origin calo :entry-date 20060803 :change-date nil :comments nil :wn ("pound%1:23:09"))
     (LF-PARENT ONT::weight-unit)
     (TEMPL SUBSTANCE-UNIT-TEMPL)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::POUND W::CAKE)
  (senses
	   ((LF-PARENT ONT::CAKE-PIE)
	    (TEMPL count-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
   (W::pound
   (wordfeats (W::morph (:forms (-vb))))
   (SENSES
    ((LF-PARENT ONT::pulse)
     (example "his heart pounds after exercise")
     (TEMPL affected-templ)
     (preference .96) ;; prefer noun sense
     (meta-data :origin cardiac :entry-date 20081215 :change-date nil :comments LM-vocab)
     )
    )
   )
))


;;;;
;;;; W::TOP
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::TOP
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("top%1:15:00" "top%1:15:01" "top%1:15:02"))
     (LF-PARENT ONT::TOP-location)
;     (TEMPL GEN-PART-OF-RELN-TEMPL)
     (TEMPL GEN-PART-OF-RELN-BARE-TEMPL)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::TOP W::ROUND)
  (senses
	   ((LF-PARENT ONT::BEEF)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::TOP W::SIRLOIN)
  (senses
	   ((LF-PARENT ONT::BEEF)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::TOP W::BLADE)
  (senses
	   ((LF-PARENT ONT::BEEF)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::TOP
   (wordfeats (W::ALLOW-POST-N1-SUBCAT +))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("top%3:00:00") :comlex (ADJECTIVE))
     (LF-PARENT ONT::TOP-LOCATION-VAL)
     (TEMPL ADJ-CO-THEME-TEMPL (XP (% W::PP (W::PTYPE (? p W::in W::of)))))
     )
;;;    ((example "I can pay top dollar for a computer")
;;;     (meta-data :origin calo :entry-date 20040505 :change-date nil :comments calo-y1variants)
;;;     (LF-PARENT ONT::ACCEPTABILITY-VAL)
;;;     )
;;;    ((EXAMPLE "a drug suitable for cancer")
;;;     (meta-data :origin calo :entry-date 20040505 :change-date nil :comments calo-y1variants)
;;;     (LF-PARENT ONT::ACCEPTABILITY-VAL)
;;;    )
;;;    ((EXAMPLE "a solution good for him")
;;;     (meta-data :origin calo :entry-date 20040505 :change-date nil :comments calo-y1variants)
;;;     (LF-PARENT ONT::ACCEPTABILITY-VAL)
;;;    )
    )
   )
))


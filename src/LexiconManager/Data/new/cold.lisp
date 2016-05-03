;;;;
;;;; W::cold
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::cold
   (SENSES
    ((LF-PARENT ONT::TEMPERATURE)
     (example "severe cold is forecast for today")
     (meta-data :origin plow :entry-date 20060802 :change-date nil :comments weather :wn ("cold%1:07:00" "cold%1:09:00"))
     (TEMPL OTHER-RELN-TEMPL)
     (templ mass-pred-templ)
     )
    )
   )
))

(define-words :pos W::n
 :tags (:base500)
 :words (
  (w::cold
  (senses;;;;; names of diseases/conditions that are count nouns and cannot appear without an article
   ((LF-PARENT ONT::illness)
    (TEMPL count-pred-TEMPL)
    )
   )
)
))

(define-words :pos W::n
 :words (
  ((W::COLD W::CUT)
  (senses
	   ((LF-PARENT ONT::MEAT)
	    (TEMPL count-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
   (W::COLD
    (wordfeats (W::MORPH (:FORMS (-ER -LY))))
    (SENSES
     ((meta-data :origin calo :entry-date 20031223 :change-date 20090731 :wn ("cold%3:00:01") :comments html-purchasing-corpus)
      (LF-PARENT ONT::COLD)
      (TEMPL LESS-ADJ-TEMPL)
      )
     )
    )
))


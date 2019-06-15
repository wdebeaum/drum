;;;;
;;;; W::AFFILIATE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::AFFILIATE
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::affiliate)
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::affiliate
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("amalgamate-22.2-2"))
     (LF-PARENT ONT::associate)
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-TEMPL (xp (% w::pp (w::ptype w::with)))) ; like associate
     )
    )
   )
))


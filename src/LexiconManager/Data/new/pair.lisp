;;;;
;;;; W::PAIR
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::PAIR
   (SENSES
    ((LF-PARENT ONT::quantity)
     (TEMPL classifier-count-pl-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE 20090520 :wn ("pair%1:14:00") :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::pair
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("amalgamate-22.2-2"))
     (LF-PARENT ONT::associate)
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-TEMPL (xp (% w::pp (w::ptype w::with)))) ; like associate,associate
     )
    )
   )
))


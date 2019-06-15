;;;;
;;;; W::bank
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::bank
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :wn ("bank%1:14:00") :comments Office)
     (LF-PARENT ONT::financial-institution)
     )					
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("bank%1:17:01"))
     (LF-PARENT ONT::shore)
     (example "the people are on the river bank")
     (TEMPL OTHER-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::bank
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("rely-70"))
     (LF-PARENT ont::rely)
     (TEMPL agent-neutral-xp-templ (xp (% w::pp (w::ptype w::on)))) ; like rely,depend,count
     )
    )
   )
))


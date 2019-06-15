;;;;
;;;; W::waste
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::waste
   (SENSES
    ((LF-PARENT ONT::waste)
     (EXAMPLE "it's waste")
     (meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :wn ("waste%1:27:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::waste
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("consume-65-1"))
     (LF-PARENT ONT::spend-time)
     (TEMPL AGENT-EXTENT-FORMAL-XP-TEMPL (xp (% w::pp (w::ptype (? pt w::for w::on))))) ; like spend
     (PREFERENCE 0.96)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("consume-65-1"))
     (LF-PARENT ONT::spend-time)
     (TEMPL NEUTRAL-EXTENT-TEMPL) ; like spend
     (PREFERENCE 0.96)
     )
    ;;;; we wasted an opportunity
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090504 :comments nil :vn ("destroy-44") :wn ("waste%2:30:00" "waste%2:35:00"))
     (LF-PARENT ONT::destroy)
     )
    )
   )
))


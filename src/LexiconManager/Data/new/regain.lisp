;;;;
;;;; W::regain
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   (W::regain
   (SENSES
    ((meta-data :origin "wordnet-3.0" :entry-date 20090504 :change-date nil :comments nil)
     (LF-PARENT ONT::acquire)
     
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  (W::regain
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("obtain-13.5.2") :wn ("regain%2:40:00" "regain%2:40:01"))
     (LF-PARENT ONT::acquire)
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype w::from)))) ; like recover
     )
    )
   )
))


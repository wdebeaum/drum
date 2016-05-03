;;;;
;;;; W::PART
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::PART
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("part%1:24:00"))
     (LF-PARENT ONT::part)
     (TEMPL other-reln-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :tags (:base500)
 :words (
  (W::part
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("separate-23.1-2"))
     (LF-PARENT ONT::separation)
     (TEMPL agent-affected-source-optional-templ (xp (% w::pp (w::ptype (? t w::from))))) ; like separate
     )
    )
   )
))


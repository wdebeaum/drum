;;;;
;;;; W::scout
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::scout
   (SENSES
    ((meta-data :origin obtw :entry-date 20111006)
     ;; 20111006 added for obtw demo
     (LF-PARENT ONT::scout)
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::scout
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20061005 :comments nil :vn ("search-35.2") :wn ("scout%2:39:00"))
     (LF-PARENT ONT::physical-scrutiny)
     (TEMPL agent-theme-xp-templ) ; like check,search
     )
    )
   )
))


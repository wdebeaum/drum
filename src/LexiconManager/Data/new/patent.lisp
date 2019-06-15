;;;;
;;;; W::patent
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (W::patent
   (SENSES
    ((EXAMPLE "he obtained a patent for the invention")
     (meta-data :origin mobius :entry-date 20080729 :change-date 20090504 :comments nil)
     (LF-PARENT ONT::official-document)
     (templ other-reln-theme-templ)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::patent
   (SENSES
    ((LF-PARENT ONT::official-document)
     (meta-data :origin task-learning :entry-date 20050926 :change-date nil :wn ("patent%1:10:01") :comments nil)
     (example "don't infringe on my patent")
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::patent
   (SENSES
    ((EXAMPLE "he patented the invention")
     (meta-data :origin mobius :entry-date 20080729 :change-date 20090501 :comments nil)
     (LF-PARENT ONT::reserve)
     (SEM (F::Cause F::agentive))
     )
    )
   )
))


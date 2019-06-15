;;;;
;;;; W::FORMAT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::FORMAT
   (SENSES
    (
     (LF-PARENT ONT::info-holder)
     (meta-data :origin calo :entry-date 20050404 :change-date nil :wn ("format%1:10:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::format
   (SENSES
    ((LF-PARENT ONT::version)
     (EXAMPLE "choose the desired format")
     (meta-data :origin task-learning :entry-date 20050812 :change-date nil :wn ("format%1:10:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::format
   (SENSES
    ((lf-parent ont::arrange-text)
     (example "format the text")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     (meta-data :origin task-learning :entry-date 20050812 :change-date 20090504 :comments nil)
     )
    )
   )
))


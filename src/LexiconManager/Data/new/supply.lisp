;;;;
;;;; W::SUPPLY
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::SUPPLY
   (SENSES
    ((LF-PARENT ONT::source) (TEMPL other-reln-templ)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("supply%1:23:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::supply
   (SENSES
    ((lf-parent ont::supply)
     (TEMPL AGENT-AFFECTED-TEMPL)
     (example "supply him a job")
     (meta-data :origin task-learning :entry-date 20050822 :change-date 20090501 :comments nil :vn ("fulfilling-13.4.1-1"))
     )
    ((meta-data :origin calo-ontology :entry-date 20060315 :change-date 20090501 :comments nil)
     (LF-PARENT ONT::supply)
     (example "supply him with the goods")
     (TEMPL AGENT-AFFECTEDR-AFFECTED-XP-NP-TEMPL (xp (% w::pp (w::ptype w::with))))
     )
    ((lf-parent ont::supply)
     (templ agent-affected-goal-optional-templ)
     (example "supply a job to him")
     (meta-data :origin step :entry-date 20080528 :change-date 20090501 :comments nil :vn ("fulfilling-13.4.1-1"))
     )
    #||((meta-data :origin step :entry-date 20080528 :change-date 20090501 :comments nil)
     (LF-PARENT ONT::supply)
     (example "the engine supplied him with energy")
     (TEMPL agent-recipient-affected-templ (xp (% w::pp (w::ptype w::with))))
     )||#
    )
   )
))


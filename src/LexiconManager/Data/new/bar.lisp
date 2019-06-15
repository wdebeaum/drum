;;;;
;;;; W::bar
;;;;

(define-words :pos w::N :templ count-pred-templ
 :words (
   (W::bar
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :wn ("bar%1:06:04") :comment caloy3)
     (LF-PARENT ONT::drinking-establishment)
     (example "let's meet at the bar")
     )
    ((LF-PARENT ONT::shape-object)
     (EXAMPLE "To see the status bar, choose Status Bar from the View menu")
     (meta-data :origin task-learning :entry-date 20050812 :change-date nil :comments nil)
     )

    ((LF-PARENT ONT::BAR-MEASURE)
    ;; musical notation for a repeating pattern of musical beats
    (meta-data :wn ("bar%1:10:00"))
    (EXAMPLE "the orchestra omitted the last twelve bars of the song")
    (TEMPL OTHER-RELN-TEMPL)
    )
    )
   )
))

(define-words :pos W::n
 :words (
;; also type of located event, but with an associated ceremony
  ((w::bar w::mitzvah)
  (senses
   ((LF-PARENT ONT::ceremony)
    (meta-data :origin chf :entry-date 20070809 :change-date nil :comments nil)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::bar
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("forbid-67"))
     (LF-PARENT ONT::hindering)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
 ; like block
     )
    ((meta-data :origin "gloss-training" :entry-date 20100217 :change-date nil :comments nil)
     (LF-PARENT ONT::hindering)
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-from-ing) (w::ptype w::from))))
     (example "It bars him from doing something")
     )
    )
   )
))


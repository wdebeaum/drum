;;;;
;;;; W::experience
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::experience
   (wordfeats (W::morph (:forms (-vb) :nom W::experience)))
   (SENSES
    ((LF-PARENT ONT::have-experience)
     (example "he is experiencing a headache")
     (meta-data :origin cardiac :entry-date 20080217 :change-date nil :comments nil)
     (TEMPL EXPERIENCER-FORMAL-PRED-SUBJCONTROL-TEMPL)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::experience
  (senses
   ((LF-PARENT ONT::knowledge-experience-scale)
    (TEMPL mass-pred-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))


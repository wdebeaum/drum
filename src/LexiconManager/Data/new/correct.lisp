;;;;
;;;; W::correct
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::correct
   (wordfeats (W::morph (:forms (-vb) :nom W::correction)))
   (SENSES
    ((LF-PARENT ONT::REPAIR)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (EXAMPLE "correct the email address")
     (meta-data :origin task-learning :entry-date 20050826 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::CORRECT
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("correct%3:00:00"))
     (LF-PARENT ONT::correct)
     )
    )
   )
))


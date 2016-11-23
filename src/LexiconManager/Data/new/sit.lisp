;;;;
;;;; w::sit
;;;;

(define-words :pos W::n
 :words (
  ((w::sit w::up)
  (senses
   ((LF-PARENT ONT::working-out)
    (meta-data :origin cardiac :entry-date 20090129 :change-date nil :comments nil)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::v :templ AGENT-neutral-XP-TEMPL
 :words (
  ((W::sit (w::out))
   (SENSES
    ((meta-data :origin asma :entry-date 20111004)
     (LF-PARENT ONT::avoiding)
     (example "sitting out gym class")
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::sit
   (wordfeats (W::morph (:forms (-vb) :past W::sat :ing W::sitting)))
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("put_spatial-9.2-1"))
     (EXAMPLE "the cup is sitting on the table")
     (LF-PARENT ONT::BE-AT-LOC)
     (SEM (F::Aspect F::Stage-level) (F::Time-span F::Extended))
     (TEMPL neutral-templ)
     )
    ((EXAMPLE "he sat at the desk" )
     (LF-PARENT ONT::BODY-MOVEMENT-place)
     (TEMPL AGENT-TEMPL)
     )
    ((EXAMPLE "Sit the box on the desk" )
     ;;(LF-PARENT ONT::BODY-MOVEMENT)
     (lf-parent ont::ppace-in-position ) ;; 20120523 GUM change new parent
      (TEMPL AGENT-AFFECTED-goal-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  ((W::sit (W::down))
   (wordfeats (W::morph (:forms (-vb) :past W::sat :ing W::sitting)))
   (SENSES
    ((EXAMPLE "You had better sit down")
     (LF-PARENT ONT::BODY-MOVEMENT)
     (TEMPL AGENT-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   ((W::sit (W::up))
   (wordfeats (W::morph (:forms (-vb) :past W::sat :ing W::sitting)))
   (SENSES
    ((EXAMPLE "You had better sit down")
     (LF-PARENT ONT::BODY-MOVEMENT)
      (TEMPL AGENT-TEMPL)
     )
    )
   )
))


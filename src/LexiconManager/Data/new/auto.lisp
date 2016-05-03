;;;;
;;;; w::auto
;;;;

(define-words :pos W::adj
 :words (
  ((w::auto w::immune)
  (senses
   ((LF-PARENT ONT::body-system-val)
    (TEMPL central-adj-templ)
    (meta-data :origin cardiac :entry-date 20090408 :change-date nil :comments nil)
    )
   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::auto
   (SENSES
    ((LF-PARENT ONT::AUTOMATIC)
     )
    )
   )
))

(define-words :pos W::adv 
 :words (
  (W::auto-
   (SENSES
    ((LF-PARENT ONT::MANNER-REFL)
    (example "autophosphorylate")
    (templ PRED-VP-PRE-templ)
     )
    )
   )
))

(define-words :pos W::adj
 :words (
  (W::auto-
   (SENSES
    ((LF-PARENT ONT::MANNER-REFL)
    (example "autophosphorylation; autobiography")
    (templ CENTRAL-ADJ-TEMPL)
     )
    )
   )
))

(define-words :pos w::adv
 :words (
  (w::auto-
  (senses
   ((lf-parent ont::MANNER-REFL)
    (example "autohypnotic")
    (templ ADJ-OPERATOR-TEMPL)
    )
   )
  )
))

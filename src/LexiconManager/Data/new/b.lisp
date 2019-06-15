;;;;
;;;; w::b
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    ((w::b W::c w::c)
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060425 :change-date nil :comments iris)
     (example "what is the cc address")
     (LF-PARENT ONT::copy)
     )
    )
   )
))

(define-words :pos w::N :templ count-pred-templ
 :words (
    ((W::b w::and w::b)
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060503 :change-date nil :comment pq387)
     (LF-PARENT ont::bedandbreakfast)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   ((w::b W::c w::c)
    (SENSES
     ((lf-parent ont::sendcopy)
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     (TEMPL AGENT-AFFECTEDR-AFFECTED-XP-NP-TEMPL (xp (% W::PP (W::ptype w::on))))
     (example "bcc him on that")
     (meta-data :origin calo-ontology :entry-date 20060425 :change-date nil :comments iris)
     )
     )
    )
))

(define-words :pos W::value :boost-word t
 :words (
  ((w::b w::c)
  (senses
   ((LF-PARENT ONT::era)
    (TEMPL value-templ) (PREFERENCE 0.92)
    )
   )
)
))

(define-words :pos W::value :boost-word t
 :words (
  ((w::b. w::c.)
  (senses
   ((LF-PARENT ONT::era)
    (TEMPL value-templ) (PREFERENCE 0.92)
    )
   )
)
))

(define-words :pos W::value :boost-word t
 :words (
  ((w::b w::c w::e)
  (senses
   ((LF-PARENT ONT::era)
    (TEMPL value-templ) (PREFERENCE 0.92)
    )
   )
)
))

(define-words :pos W::value :boost-word t
 :words (
  (W::B
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))


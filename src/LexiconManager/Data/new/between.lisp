;;;;
;;;; W::BETWEEN
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::BETWEEN
   (SENSES
    ((LF-PARENT ONT::between)
     (example "it is between a rock and a hard place")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (preference .98) ;; prefer np attachment
     )
     ((LF-PARENT ONT::between)
     (example "find flights between two cities")
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL (xp (% W::NP (W::agr W::3p))))
     (meta-data :origin plow :entry-date 20060526 :change-date nil :comments pq0405)
     )
     #|
     ((LF-PARENT ONT::SPATIAL-LOC)
      (TEMPL BINARY-CONSTRAINT-OF-STATE-NP-TEMPL)
      (meta-data :origin beetle2 :entry-date 20082002 :change-date nil :comments pilot1)
      (example "voltage between terminals")
      (preference 0.96) ;; don't choose if other options are available
      )
     |#
     ((LF-PARENT ONT::time-clock-rel)
      (meta-data :origin ralf :entry-date 20040709 :change-date nil :comments nil)
      (example "between 4 and 5 pm")
      (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL  (xp (% W::NP (W::agr W::3p))))
      )
     ((LF-PARENT ONT::SCALE-RELATION)
      (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL  (xp (% W::NP (W::agr W::3p))))
      (example "purchases between five and ten dollars")
      (SYNTAX (W::ALLOW-DELETED-COMP +) (w::degree-adv +))
      (meta-data :origin calo :entry-date 20050418 :change-date nil :comments projector-purchasing)
      )
     )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::BETWEEN
   (SENSES
    ((LF (W::BETWEEN))
     (non-hierarchy-lf t))
    )
   )
))


;;;;
;;;; W::partition
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::partition
   (SENSES
  ((meta-data :origin calo-ontology :entry-date 20060214 :change-date nil :comments nil)
     (LF-PARENT ONT::separation) ;; like divide
     (example "he partitioned the data")
     (templ agent-affected-xp-templ)
     )
  ((meta-data :origin beetle :entry-date 20080716 :change-date nil :comments nil :vn ("separate-23.1-2"))
   (LF-PARENT ont::separation)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-SOURCE-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? t W::from)))))
     (EXAMPLE "a divider partitions the room")
     )
    )
   )
))


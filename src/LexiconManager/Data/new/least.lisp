;;;;
;;;; W::least
;;;;

(define-words :pos W::adv :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::least
   (wordfeats (W::COMPARATIVE W::SUPERL)); (W::FUNCTN ONT::COMPARE-VAL))
   (SENSES
    ((LF-PARENT ONT::MIN-VAL)
     (TEMPL SUPERL-TEMPL (xp (% W::pp (W::ptype W::of))))
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "the least fun I've had in years")
     (SEM (F::SCALE ONT::COMPARE-VAL))
     )
    )
  )
))


;;;;
;;;; W::pride
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::pride
   (SENSES
    ((LF-PARENT ONT::discrete-property-val)
     (meta-data :origin chf :entry-date 20070810 :change-date nil :comments chf-dialogues)
     (TEMPL OTHER-RELN-TEMPL (xp (% W::pp (W::ptype (? pt W::in)))))
     (example "I take pride in it")
     )
    )
   )
))


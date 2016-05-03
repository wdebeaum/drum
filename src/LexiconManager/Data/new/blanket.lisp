;;;;
;;;; w::blanket
;;;;

(define-words :pos W::n
 :words (
  (w::blanket
  (senses
   ((LF-PARENT ONT::bedding)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::blanket
   (wordfeats (W::morph (:forms (-vb) :past W::blanketed :ing W::blanketing)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090911 :comments nil :vn ("contiguous_location-47.8") :wn ("blanket%2:35:00" "blanket%2:42:00"))
     (LF-PARENT ONT::cover)
     (TEMPL neutral-neutral-templ)
     (example "snow blankets the ground")
     )
    ((LF-PARENT ONT::cover)
     (TEMPL AGENT-affected2-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? pt W::with W::in)))))
;     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "blanket the ground with snow")
     (meta-data :origin trips :entry-date 20090911 :change-date nil :comments nil)
     )
    )
   )
))


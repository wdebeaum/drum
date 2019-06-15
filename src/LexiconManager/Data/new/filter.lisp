;;;;
;;;; W::filter
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::filter
   (SENSES
    ((LF-PARENT ONT::device)
     (SEM (F::mobility F::non-self-moving))
     (meta-data :origin calo-ontology :entry-date 20060609 :change-date nil :wn ("filter%1:06:00") :comments plow-req)
     (example "put the water through the filter")
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::filter
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060608 :change-date 20090601 :comments nil :vn ("wipe_instr-10.4.2") :wn ("filter%2:35:00"))
     (EXAMPLE "filter the dirt from the water")
     (LF-PARENT ONT::remove-parts)
     (TEMPL AGENT-AFFECTED-SOURCE-XP-TEMPL (xp (% w::pp (w::ptype (? ptp w::from)))))
     (PREFERENCE 0.96)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060608 :change-date nil :comments nil :vn ("wipe_instr-10.4.2") :wn ("filter%2:35:00"))
     (example "filter the water")
     (LF-PARENT ONT::clean)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    )
   )
))


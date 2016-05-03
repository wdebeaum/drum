;;;;
;;;; W::RESULT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::RESULT
   (SENSES
    ((meta-data :origin calo :entry-date 20031229 :change-date 20070521 :comments html-purchasing-corpus)
     (LF-PARENT ONT::result)
     (example "here is the list of results")
     (TEMPL OTHER-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  (W::result
   (SENSES
    ((EXAMPLE "The stimulation results in the activation")
     (lf-parent ont::cause-produce-reproduce) 
     (TEMPL AGENT-AFFECTED-as-comp-TEMPL (xp (% W::pp (W::ptype W::in))))
     )

    ((EXAMPLE "Side effects may result.")
     (lf-parent ont::cause-produce-reproduce) 
     (TEMPL AFFECTED-TEMPL)
     )

    ((EXAMPLE "The stimulation results from the activation")
     (lf-parent ont::cause-produce-reproduce) 
     (TEMPL AFFECTED-AGENT-as-comp-TEMPL (xp (% W::pp (W::ptype W::from))))
     )

    )
   )
))

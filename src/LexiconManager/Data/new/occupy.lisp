;;;;
;;;; W::occupy
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::occupy
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090512 :comments nil :vn ("amuse-31.1"))
     (LF-PARENT ONT::evoke-attention)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    ((LF-PARENT ONT::control-manage)
     (example "occupy the country")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
    )
   ))
))


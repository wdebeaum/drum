;;;;
;;;; W::prescribe
;;;;

(define-words :pos W::v 
 :words (
 (W::prescribe
   (SENSES
    ((meta-data :origin cernl :entry-date 20100607 :change-date nil :comments hpi-acn-1)
     (LF-PARENT ONT::prescribing)
     (example "he prescribed the medication" "she prescribed me Lexapro")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (templ agent-affected-recipient-alternation-templ)
     )
    #||((lf-parent ont::prescribing)
     (templ agent-recipient-affected-alternation-templ)
     (example "he was prescribed the medication by the doctor")
     (meta-data :origin cernl :entry-date 20100607)
     )
    ((lf-parent ont::prescribing)
     (templ agent-theme-recipient-alternation-templ)
     (example "the medication was prescribed by the doctor")
     (meta-data :origin cernl :entry-date 20100607)
     )||#
    )
   )
))


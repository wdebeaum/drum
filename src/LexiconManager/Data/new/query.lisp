;;;;
;;;; W::query
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::query
    (wordfeats (W::morph (:forms (-vb) :nom W::query)))
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060315 :change-date nil :comments nil)
     ;;(LF-PARENT ONT::QUESTIONING)
     ;;(lf-parent ont::ask-query-question) ;; 20120524 GUM change new parent
     (LF-PARENT ONT::ASK-QUESTION)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     )
    )
   )
))


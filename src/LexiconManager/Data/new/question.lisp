;;;;
;;;; W::question
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
 (W::question
    ;(wordfeats (W::morph (:forms (-vb) :nom W::question)))
   (SENSES
    ((EXAMPLE "question authority")
     (meta-data :origin calo-ontology :entry-date 20060315 :change-date nil :comments nil)
     (lf-parent ont::contest) ;; 20120524 GUM change new parent
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     )
    )
   )
))

(define-words :pos W::n :templ count-pred-templ
 :tags (:base500)
 :words (
 (W::question
   (SENSES
    ((LF-PARENT ONT::QUESTION)
     )
    )
   )
))


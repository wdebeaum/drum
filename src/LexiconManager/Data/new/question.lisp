;;;;
;;;; W::question
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
 (W::question
    ;(wordfeats (W::morph (:forms (-vb) :nom W::question)))
   (SENSES
    ((EXAMPLE "question authority")
     (meta-data :origin calo-ontology :entry-date 20060315 :change-date nil :comments nil)
     ;;(LF-PARENT ONT::QUESTIONING)
     (lf-parent ont::contest) ;; 20120524 GUM change new parent
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-theme-xp-TEMPL)
     )
    )
   )
))

(define-words :pos W::n :templ other-reln-theme-TEMPL
 :tags (:base500)
 :words (
 (W::question
   (SENSES
    (
     ;;(LF-PARENT ONT::QUESTIONING)
;     (lf-parent ont::ask-query-question) ;; 20120524 GUM change new parent
     (LF-PARENT ONT::ASK-QUESTION)
     )
    )
   )
))


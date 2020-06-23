;;;;
;;;; W::validate
;;;;

(define-words :pos W::v 
 :words (
 (W::validate
    (wordfeats (W::morph (:forms (-vb) :nom w::validation)))
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::evaluate) ;scrutiny)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-xp-templ)
     )
    )
   )
))


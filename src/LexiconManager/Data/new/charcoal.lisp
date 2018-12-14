;;;;
;;;; W::charcoal
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  ((W::charcoal w::broil)
      (wordfeats (W::morph (:forms (-vb) :past (W::charcoal w::broiled) :ing( W::charcoal w::broiling) )))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("cooking-45.3"))
     (LF-PARENT ONT::roast) ; like bake,blanch,boil,braise,cook,fry
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
    )
   )
  )
))


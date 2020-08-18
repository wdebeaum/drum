;;;;
;;;; W::WHEREVER
;;;;

(define-words :pos W::adv 
  :words (
  (W::WHEREVER
   (SENSES
    ((meta-data :origin monroe :entry-date 20031217 :change-date nil :comments s15)
     (LF-PARENT ONT::AT-LOC)
     (SYNTAX (W::IMPRO-CLASS LOCATION))
     (SEM (F::information F::information-content))
     (TEMPL binary-constraint-S-decl-TEMPL)
     (example "I'll find it wherever you put it")
     )

    ((LF-PARENT ONT::AT-LOC) 
     (SYNTAX (W::IMPRO-CLASS ONT::LOCATION))
     (example "Wherever did you put the cake?")
     (TEMPL ppword-question-adv-pred-templ)
     (SEM (F::information F::information-content))
    
    )
    )
   ))
  )


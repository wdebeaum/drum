;;;;
;;;; W::BUDGET
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::BUDGET
   (SENSES
    ;; note that this subcat is restricted to abstract objects like projects -- budgets for people or organizations
    ;; are considered more general and get assoc-with interpretations
    ((meta-data :origin calo :entry-date 20030605 :change-date nil :comments calo-y1script)
     (LF-PARENT ONT::BUDGET)
     (example "the budget of the project")
     (TEMPL OTHER-RELN-TEMPL )
     )
    ((meta-data :origin calo :entry-date 20030605 :change-date nil :wn ("budget%1:21:02") :comments calo-y1script)
     (LF-PARENT ONT::BUDGET)
     (example "our budget of 500 dollars")
     (TEMPL other-reln-TEMPL (xp (% W::pp (W::ptype (? pt W::for w::of)))))
     (preference .98)
     )
    )
   )
))


;;;;
;;;; W::kind
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::kind
   (wordfeats (W::MORPH (:forms (-S-3P))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("kind%1:09:00"))
     (EXAMPLE "What kind of surgery is it?")
     (LF-PARENT ONT::KIND)
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::kind W::of)
   (SENSES
    ((LF-PARENT ONT::degree-MODIFIER-MED)
     (LF-FORM W::KIND-OF)
     (TEMPL ADJ-OPERATOR-TEMPL)
     )
    ((LF-PARENT ONT::degree-modifier-med)
     (LF-FORM W::KIND-OF)
     (TEMPL PRED-VP-PRE-TEMPL)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::KIND
    (wordfeats (W::MORPH (:FORMS (-ER -LY))))
    (SENSES
     ((LF-PARENT ONT::COURTEOUS-val)
      (EXAMPLE "he is a kind person")
      )
     )
    )
))



(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::NEXT
   (wordfeats (W::COMP-OP W::LESS))
   (SENSES
    (;(lf-parent ont::sequence-val)
     (lf-parent ont::sequence-val-next)
     (example "let's meet next monday")
     (meta-data :origin calo-ontology :entry-date 20060419 :change-date nil :wn ("next%5:00:00:succeeding(a):00") :comments nil)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::NEXT
   (SENSES
    ((EXAMPLE "Next send a truck to Abyss" "do it next")
     (meta-data :origin trips :entry-date nil :change-date 20070414 :comments lam-evaluation)
     (LF-PARENT ONT::SEQUENCE-POSITION)
     (TEMPL disc-templ)
     )
    ((LF-PARENT ONT::SEQUENCE-POSITION)
     (example "we next open the package")
     (TEMPL PRED-S-VP-TEMPL)
     )
    )
   
   )
))


(define-words :pos W::ADV
  :tags (:base500)
 :words (
  ((W::NEXT W::TO)
   (SENSES
    ((LF-PARENT ONT::adjacent)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::ORDINAL :boost-word t :templ ORDINAL-TEMPL
 :tags (:base500)
 :words (
  (W::NEXT
   (SENSES
    ((LF (W::NEXT))
     (non-hierarchy-lf t)(SYNTAX (W::agr ?a))
     )
    )
   )
))


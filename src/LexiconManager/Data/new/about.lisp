;;;;
;;;; W::ABOUT
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::ABOUT
   (SENSES
    ;; collapse this sense w/ either qmodifier
    ((LF-PARENT ONT::time-clock-rel)
     (LF-FORM W::APPROXIMATE)
     (example "it's about midnight")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
    ((LF-PARENT ONT::QMODIFIER)
     (LF-FORM W::APPROXIMATE)
     (example "about 5 pounds")
     (TEMPL NUMBER-OPERATOR-TEMPL)
     )
    ((LF-PARENT ONT::MODIFIER)
     (LF-FORM W::APPROXIMATE)
     (example "he about cried")
     (preference 0.98) 
     (TEMPL PRED-VP-PRE-TEMPL)
     )
    ((EXAMPLE "About the book, can we change it")
     (LF-PARENT ONT::TOPIC)
     (LF-FORM W::TOPIC)
     (TEMPL topic-templ)
     (PREFERENCE 0.96)  
     )
    ((EXAMPLE "a question about the book" "a discussion about the problem")
     (LF-PARENT ONT::Associated-information)
     (TEMPL binary-constraint-s-or-np-templ)
     )
    ((LF-PARENT ONT::degree-modifier-med)
     (example "it's about there")
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (meta-data :origin fruitcarts :entry-date 20050331 :change-date nil :comments fruitcarts-11-1)
     )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::ABOUT
   (SENSES
    ((LF (W::ABOUT))
     (non-hierarchy-lf t))
    )
   )
))


;;;;
;;;; W::instead
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::instead
   (wordfeats (W::ALLOW-DELETED-COMP +))
   (SENSES
    ((LF-PARENT ONT::CHOICE-OPTION)
     (LF-FORM W::INSTEAD-OF)
     (TEMPL binary-constraint-S-templ)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::INSTEAD W::of)
   (SENSES
    ((LF-PARENT ONT::CHOICE-OPTION)
     (LF-FORM W::INSTEAD-OF)
     (TEMPL binary-constraint-S-templ)
     )
    ((LF-PARENT ONT::CHOICE-OPTION)
     (LF-FORM W::INSTEAD-OF)
     (TEMPL binary-constraint-s-ing-templ)
     )
    )
   )
))


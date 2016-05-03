;;;;
;;;; W::JUST
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::JUST
   (SENSES
    ((LF-PARENT ONT::MODIFIER)
     (LF-FORM W::JUST)
     (example "it's just ready")
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     )
    ((LF-PARENT ONT::MODIFIER)
     (LF-FORM W::JUST)
     (example "just five dollars")
     (TEMPL NUMBER-OPERATOR-TEMPL)
     )
    ((LF-PARENT ONT::MODIFIER)
     (LF-FORM W::JUST)
     (TEMPL PRED-S-VP-TEMPL)
     )
    ((LF-PARENT ONT::MODIFIER)
     (LF-FORM W::JUST)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::just W::in W::case)
   (SENSES
    ((LF-PARENT ONT::situated-in)
     (TEMPL binary-constraint-s-decl-templ)
     (meta-data :origin trips :entry-date nil :change-date 20070609 :comments sentential-conjunction-cleanup)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
;   )
  ((W::just W::a W::second)
   (SENSES
    ((LF (W::WAIT))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_DISCOURSE-MANAGE))
     (preference .98)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::just W::a W::sec)
   (SENSES
    ((LF (W::WAIT))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_DISCOURSE-MANAGE))
     (preference .98)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::just W::a W::minute)
   (SENSES
    ((LF (W::WAIT))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_DISCOURSE-MANAGE))
     (preference .98)
     )
    )
   )
))


;;;;
;;;; w::no
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
   (w::no
    (SENSES
    ((LF-PARENT ont::degree-modifier)
     (TEMPL NEG-ADJ-OPERATOR-TEMPL)
     (example "it has no green in it")
     (meta-data :origin cardiac :entry-date 20080429 :change-date nil :comments nil)
     )
    ((LF-PARENT ont::degree-modifier)
     (TEMPL comparative-adj-ADV-OPERATOR-TEMPL)
     (example "no more")
     (meta-data :origin cardiac :entry-date 20080429 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::NO W::LONGER)
   (SENSES
    ((LF-PARENT ONT::FREQUENCY)
     (TEMPL PRED-VP-PRE-TEMPL)
     (SYNTAX (W::NEG +))
     )
    ((LF-PARENT ONT::NEG)
     (example "It makes the path no longer closed")
     (TEMPL NEG-ADJ-ADV-OPERATOR-TEMPL)
     (preference 0.97)
     )
    ((LF-PARENT ONT::FREQUENCY)
     (TEMPL PRED-S-POST-TEMPL)
     (example "he can wait no longer")
     (preference .98)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::NO W::MORE)
   (SENSES
    ((LF-PARENT ONT::FREQUENCY)
     (TEMPL PRED-VP-PRE-TEMPL)
     (SYNTAX (W::NEG +))
     )
    ((LF-PARENT ONT::FREQUENCY)
     (TEMPL PRED-S-POST-TEMPL)
     (SYNTAX (W::NEG +))
     (example "he can wait no more")
     (preference .98)
     )
    )
   )
))

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
  (W::NO
   (wordfeats (W::NEG +) (W::AGR ?agr) (W::MASS ?m) (W::status W::quantifier))
   (SENSES
    ((LF ONT::NONE)
     (non-hierarchy-lf t)(TEMPL quan-no-bare-TEMPL)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::NO
   (SENSES
    ((LF (ONT::NEG))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::no W::way)
   (SENSES
    ((LF (ONT::NEG))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::no W::thanks)
   (SENSES
    ((LF (ONT::NEG))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     )
    )
   )
))


;;;;
;;;; W::NOT
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::NOT
   (SENSES
    #||((TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     (LF-PARENT ONT::BUT-EXCEPTION)
     (preference .98) ;; prefer the ADV operator when possible
     )||#
    ((LF-PARENT ONT::NEG)
     (example "not too bad")
     (TEMPL NEG-ADJ-ADV-OPERATOR-TEMPL)
     )    
    )
   )
))

;; not can be a conjunction "apples not pears keep", he was "happy, not sad"
(define-words :pos W::conj :boost-word t
 :tags (:base500)
 :words (
  (w::not
   (wordfeats (w::but-not +))
   (SENSES
    ((LF ONT::BUT-EXCEPTION)
     (non-hierarchy-lf t)
     (TEMPL SUBCAT-ANY-TEMPL)
     )
    )
   )
  
  ))


(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
    ((W::NOT w::at w::all)
   (SENSES
    ((LF-PARENT ONT::degree-modifier-verylow)
     (example "not at all bad")
     (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil)
     (TEMPL NEG-ADJ-ADV-OPERATOR-TEMPL)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
    ((W::NOT w::all w::that)
   (SENSES
    ((LF-PARENT ONT::degree-modifier-low)
     (example "not at all bad")
     (meta-data :origin cardiac :entry-date 20081015 :change-date nil :comments nil)
     (TEMPL NEG-ADJ-ADV-OPERATOR-TEMPL)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
;   )
   ((w::not W::real)
   (SENSES
    ((LF-PARENT ONT::degree-MODIFIER-LOW)
     (LF-FORM W::not-REALLY)
     (TEMPL ADJ-OPERATOR-TEMPL)
     )
    ((LF-PARENT ONT::DEGREE-MODIFIER-LOW)
     (LF-FORM W::not-REALLY)
     (TEMPL PRED-VP-PRE-TEMPL)
     )
    )
   )
))

(define-words 
 :pos W::adv :templ pred-vp-templ
 :words  
 (
;; added for lam
  ((w::not w::remotely)
   (senses
    ((LF-PARENT ONT::DEGREE-MODIFIER-LOW)
     (example "not remotely cold")
     (TEMPL ADJ-OPERATOR-TEMPL)
     )
    ))
))


(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::NOT W::MORE W::THAN)
   (SENSES
    ((LF-PARENT ONT::QMODIFIER)
     (LF-FORM W::MAX)
     (TEMPL NUMBER-OPERATOR-TEMPL)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::NOT W::LESS W::THAN)
   (SENSES
    ((LF-PARENT ONT::QMODIFIER)
     (LF-FORM W::MIN)
     (TEMPL NUMBER-OPERATOR-TEMPL)
     )
    )
   )
))

(define-words :pos W::neg
 :tags (:base500)
 :words (
;; VP negation e.g. he isn't going to the party
  (W::NOT
  (senses((LF ONT::NEG)
    (non-hierarchy-lf t)(TEMPL no-features-templ)
    )
   )
)
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::not W::on W::your W::life)
   (SENSES
    ((LF (W::NEG))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   ((W::not w::that w::I W::know W::of)
   (SENSES
    ((LF (W::UNSURE-NEG))
     (meta-data :origin cardiac :entry-date 20080814 :change-date nil :comments nil)
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     (preference .97)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   ((W::not w::that w::I w::^m W::aware W::of)
   (SENSES
    ((LF (W::UNSURE-NEG))
     (meta-data :origin cardiac :entry-date 20080814 :change-date nil :comments nil)
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     (preference .97)
     )
    )
   )
))


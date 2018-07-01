;;;;
;;;; W::THERE
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::THERE
   (wordfeats ;(W::CASE W::SUB)  ; OBJ in "Let there be light"
    (W::agr (? ag W::3s W::3p)) ;(W::sing-lf-only +)
    (W::expletive +))
   (SENSES
    ((LF ONT::EXPLETIVE)
     (non-hierarchy-lf t))
    )
   )
))

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::THERE
   (SENSES
    ((LF-PARENT ONT::pos-wrt-speaker-reln)
     (SYNTAX (W::IMPRO-CLASS ONT::location)
     ))
    ))
  ))

(define-words :pos W::n :templ PPWORD-N-TEMPL
 :tags (:base500)
 :words (
  ;; put it in there
  (W::THERE
   (SENSES
    ((LF-PARENT ONT::LOCATION)
     (SYNTAX (W::case W::obj))
     (PREFERENCE 0.97)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   ((W::there w::you w::go)
   (SENSES
    ((LF (W::ATTENTION))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_EXPRESSIVE))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::there w::we w::go)
   (SENSES
    ((LF (W::ATTENTION))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_EXPRESSIVE))
     )
    )
   )
))


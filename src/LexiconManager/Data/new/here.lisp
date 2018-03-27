;;;;
;;;; W::HERE
;;;;

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::HERE
   ;(wordfeats (W::ATYPE (? atype W::pre-vp W::post w::pre-vp)))
   (SENSES
    ((LF-PARENT ONT::pos-wrt-speaker-reln)
     (SYNTAX (W::IMPRO-CLASS ONT::location))
     ;(TEMPL pred-s-vp-templ)
     )
     )
   )
))


(define-words :pos W::n :templ PPWORD-N-TEMPL
 :tags (:base500)
 :words (
  (W::HERE
   (SENSES
    ((LF-PARENT ONT::location)
     (PREFERENCE 0.98)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   ;; added for CAET
   ((W::here w::goes)
   (SENSES
    ((LF (W::ATTENTION))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_EXPRESSIVE))
     )
     ))
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::here w::we w::go)
   (SENSES
    ((LF (W::ATTENTION))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_EXPRESSIVE))
     )
    ))
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   ((W::here w::you w::go)
   (SENSES
    ((LF (W::ATTENTION))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_EXPRESSIVE))
     )
    )
   )
))


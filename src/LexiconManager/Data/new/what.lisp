;;;
;;;; W::WHAT
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::WHAT ; separate entries for 3s and 3p so we can figure out the spec
   (SENSES
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     (SEM (F::origin (? !n F::human)))
     (TEMPL pronoun-wh-templ)
     (SYNTAX (W::agr W::3s) (W::wh W::Q)
	     (w::case (? c w::sub w::obj))) ;(W::case W::obj) (W::sing-lf-only +))
     )
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     (SEM (F::origin (? !n F::human)))
     (TEMPL pronoun-wh-templ)
     (SYNTAX (W::agr W::3p) (W::wh W::Q)
	     (w::case (? c w::sub w::obj))) ;(W::case W::obj) (W::sing-lf-only +))
     )
    #|
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     (SEM (F::origin (? !n F::human)))
     (TEMPL pronoun-wh-templ)
     (SYNTAX (W::agr W::3s) (W::wh W::Q) (W::case W::sub))
     )
    |#
    )
   )
))

(define-words :pos W::art :boost-word t
 :tags (:base500)
 :words (
  (W::WHAT
   (SENSES
    ((LF ONT::WH-TERM)
     (non-hierarchy-lf t)(TEMPL wh-qtype-TEMPL)
     (SYNTAX (W::agr W::3s))
     )
    ((LF ONT::WH-plural)
     (non-hierarchy-lf t)
     (TEMPL wh-qtype-TEMPL)
     (SYNTAX (W::agr W::3p))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::what W::the W::hell)
   (SENSES
    ((LF (W::WHAT-THE-HELL))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_EXPRESSIVE))
     (preference .97) ;; don't compete with 'what'
     )
    )
   )
))


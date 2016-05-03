;;;;
;;;; W::WHERE
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::WHERE
   (SENSES
    ;;;;;(sem (information f_information-content))
    ((LF-PARENT ONT::LOCATION)
      (TEMPL PRONOUN-WH-TEMPL)
     (SYNTAX (W::wh W::r) (W::case W::obj) (W::sing-lf-only +))
     )
     #||((LF-PARENT ONT::LOCATION)
      (TEMPL PRONOUN-WH-TEMPL)
     (SYNTAX (W::wh W::q) (W::case W::obj) (W::sing-lf-only +))
     )||#
    ;;;;; (sem (information f_information-content))
;    ((LF-PARENT ONT::PATH)
;     (TEMPL PRONOUN-WH-TEMPL)
;     (SYNTAX (W::wh W::r) (W::case W::obj) (W::sing-lf-only +))
;     )
    )
   )
))

(define-words :pos W::adv :templ PPWORD-QUESTION-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::WHERE
   (SENSES
    ((LF-PARENT ONT::WH-LOCATION)
     (SYNTAX (W::IMPRO-CLASS ONT::LOCATION))
     (TEMPL ppword-question-adv-pred-templ)
     (SEM (F::information F::information-content))
     )
    ((LF-PARENT ONT::TO-LOC)
     (SYNTAX (W::IMPRO-CLASS ONT::LOCATION))
     (SEM (F::information F::information-content))
     )
    )
   )
))

(define-words :pos W::n :templ PPWORD-N-TEMPL
 :tags (:base500)
 :words (
  (W::WHERE
   (wordfeats (W::WH W::Q))
   (SENSES
    ((LF-PARENT ONT::LOCATION)
     (PREFERENCE 0.9) ;; really prefer adv
     )
    )
   )
))


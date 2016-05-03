;;;;
;;;; W::WHEN
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::WHEN
   (SENSES
    ((LF-PARENT ONT::EVENT-TIME-REL)
     (TEMPL PRONOUN-WH-TEMPL)
     (SYNTAX (W::wh W::r) (W::case W::obj) (W::sing-lf-only +))
     )
    )
   )
))

(define-words :pos W::adv :templ PPWORD-QUESTION-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::WHEN
   (SENSES
    ((LF-PARENT ONT::EVENT-TIME-REL)
     (SYNTAX (W::IMPRO-CLASS ONT::TIME-POINT))
     (TEMPL ppword-question-adv-pred-templ)
     (SEM (F::information F::information-content))
     )
    )
   )
))

(define-words :pos W::n :templ PPWORD-N-TEMPL
 :tags (:base500)
 :words (
  (W::WHEN
   (wordfeats (W::WH W::Q))
   (SENSES
    ((LF-PARENT ONT::Time-point)
     (PREFERENCE 0.9) ;; really prefer adv
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
   (W::when
   (SENSES
    ((LF-PARENT ONT::event-time-rel)
     (TEMPL binary-constraint-S-decl-TEMPL)
     (example "I saw him when he left")
     )
    ((LF-PARENT ONT::event-time-rel)
     (meta-data :origin calo :entry-date 20040809 :change-date nil :comments caloy2)
     (example "buy me a monitor when buying a computer")
     (TEMPL binary-constraint-S-ing-TEMPL)
     )
    ((LF-PARENT ONT::temporal-location)
     (meta-data :origin calo :entry-date 20040809 :change-date nil :comments caloy2)
     (example "when hungry I like steak" "I like steak when hungry" "The man, when hungry, likes steak")
     (TEMPL binary-constraint-s-or-np-pred-TEMPL)
     )
    )
   )
))


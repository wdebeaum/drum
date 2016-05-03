;;;;
;;;; w::alot
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (w::alot
   (SENSES
    ((LF-PARENT ont::degree-modifier-high)
     (meta-data :origin calo :entry-date 20050214 :change-date nil :comments caloy2)
     (example "there are alot lighter ones but they are more expensive")
     (TEMPL comparative-adj-adv-operator-templ)
     )
    ((LF-PARENT ont::degree-modifier-high)
     (meta-data :origin calo :entry-date 20050214 :change-date nil :comments caloy2)
     (example "do you run alot?")
     (TEMPL pred-vp-templ)
     )
;    ((LF-PARENT ont::degree-modifier)
;     (TEMPL comparative-ADV-OPERATOR-TEMPL)
;     (example "alot more")
;     (meta-data :origin calo :entry-date 20050427 :change-date nil :comments caloy2)
;     )
    )
   )
))

(define-words :pos W::quan :boost-word t
 :words (
   (w::alot
    (wordfeats (W::negatable +) (W::NOsimple +))
    (SENSES
     ((LF W::PLENTY)
      (example "alot of water")
      (non-hierarchy-lf t)(TEMPL quan-mass-TEMPL)
      (SYNTAX (W::agr (? agr W::3s)) (w::status w::indefinite))
      )
     ((LF W::PLENTY)
      (example "alot of people")
      (non-hierarchy-lf t)(TEMPL quan-cardinality-pl-TEMPL)
      (SYNTAX (W::agr W::3p) (w::status w::indefinite-plural))
      )
     )
    )
))


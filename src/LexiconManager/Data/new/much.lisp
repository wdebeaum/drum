;;;;
;;;; W::much
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::much
   (SENSES
    ((LF-PARENT ont::degree-modifier-high)
     (meta-data :origin calo :entry-date 20050214 :change-date nil :comments caloy2)
     (example "there are much lighter ones but they are more expensive")
     (TEMPL comparative-adj-adv-operator-templ)
     )
    ((LF-PARENT ont::degree-modifier-high)
     (meta-data :origin calo :entry-date 20050214 :change-date nil :comments caloy2)
     (example "do you run much?")
     (TEMPL pred-vp-templ)
     )
;    ((LF-PARENT ont::degree-modifier)
;     (TEMPL comparative-ADV-OPERATOR-TEMPL)
;     (example "much more")
;     (meta-data :origin calo :entry-date 20050427 :change-date nil :comments caloy2)
;     )
    )
   )
))

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
  (W::MUCH
   (wordfeats (W::status w::SM) (W::negatable +) (W::AGR W::3s))
   (SENSES
    ((LF ONT::MUCH)
     (non-hierarchy-lf t)
     (example "there isn't much water")
     (TEMPL quan-mass-TEMPL)
     )
   #|| ((LF ONT::MUCH)
     (non-hierarchy-lf t)
     (example "there isn't much pain")
     (TEMPL quan-bare-TEMPL)
     )||#
    ((LF ONT::MUCH)
     (non-hierarchy-lf t)
     (example "there isn't much of the truck left")
     (syntax (w::mass w::mass))
     (TEMPL quan-sing-sing-TEMPL)
     )
    )
   )
))


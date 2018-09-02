;;;;
;;;; W::ABILITY
;;;;

#|
(define-words :pos W::v :templ COUNT-PRED-TEMPL
 :words (
   (W::ability-verb-doesnotexist
     (wordfeats (W::morph (:forms (-vb) :nom W::ability :nomsubjpreps (w::of) :nomobjpreps (-))))
     (SENSES
      (;(LF-PARENT ONT::ABLE)  
       (LF-PARENT ONT::ABILITY-STATE)  
       (TEMPL neutral-theme-subjcontrol-templ )
       )))))
|#

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::ability
   (SENSES
    ((meta-data :origin domain-reorganization :entry-date 20170810 :change-date nil :wn ("ability%1:07:00") :comments caloy3)
     (LF-PARENT ONT::able-scale)
     ;(TEMPL MASS-PRED-TEMPL)
     (templ other-reln-templ (XP (% W::CP (W::CTYPE W::s-to))))
     )
    )
   )
))

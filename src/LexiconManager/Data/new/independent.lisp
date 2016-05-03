;;;;
;;;; W::independent
;;;;

(define-words :pos W::adj 
 :words (
   (W::independent
    (wordfeats (W::morph (:FORMS (-LY))))
    (SENSES
     ((meta-data :origin monroe :entry-date 20031219 :change-date 20090818 :wn ("independent%3:00:00") :comments s11)
      (LF-PARENT ONT::independent)
;      (TEMPL CENTRAL-ADJ-XP-POSSIBLE-PRE-TEMPL (xp (% W::PP (w::ptype w::of))))
;      (TEMPL ADJ-CO-THEME-TEMPL (XP (% W::PP (W::PTYPE (? ptp W::of)))))
      (TEMPL ADJ-NEUTRAL-NEUTRAL-TEMPL (XP (% W::PP (W::PTYPE (? ptp W::of)))))
      )
     )
    )
   ))


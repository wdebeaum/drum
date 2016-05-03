;;;;
;;;; W::dependent
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::dependent
    (SENSES
     ((meta-data :origin monroe :entry-date 20031219 :change-date 20090731 :wn ("dependent%3:00:00") :comments s11)
      (LF-PARENT ONT::DEPENDENT)
;      (TEMPL ADJ-CO-THEME-TEMPL (XP (% W::PP (W::PTYPE (? ptp w::on w::upon)))))
      (TEMPL ADJ-NEUTRAL-NEUTRAL-TEMPL (XP (% W::PP (W::PTYPE (? ptp w::on w::upon)))))
      )
     )
    )
))



(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::characteristic
   (SENSES
    ((meta-data :origin calo :entry-date 20031229 :change-date nil :comments caloy2 :wn ("characteristic%1:07:00" "characteristic%1:07:01"))
     (LF-PARENT ont::attribute)
     (example "compare the characteristics of this computer")
      )
    )
   )
))

(define-words :pos W::adj
 :words (
   (W::characteristic
   (SENSES
    (
     (lf-parent ont::stereotypical-val)
     (TEMPL central-adj-optional-xp-templ (xp (% W::PP (W::ptype W::of))))
     (example "footprints characteristic of the penguin")
      )
    )
   )
))

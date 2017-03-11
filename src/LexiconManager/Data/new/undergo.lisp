;;;;
;;;; W::undergo
;;;;

(define-words :pos W::v :templ Affected-effect-XP-TEMPL
 :words (
   (W::undergo
   (wordfeats (W::morph (:forms (-vb) :3s W::undergoes :past W::underwent :pastpart W::undergone)))
   (SENSES
    ((lf-parent ont::undergo-action)
     (example "The mixture undergoes an explosion")
     (TEMPL affected-effect-xp-TEMPL)
     (meta-data :origin LbR :entry-date 20080922 :change-date nil :comments nil)
     )
    )
)))


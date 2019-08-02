;;;;
;;;; W::enthusiastic
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
    (W::enthusiastic
   (wordfeats (W::morph (:FORMS ( -LY))))
   (SENSES
    ((meta-data :origin adj-devel :entry-date 20080926 :change-date nil :comments nil :wn ("happy%3:00:00"))
     (example "an enthusiactic message")
     (lf-parent ont::eager-val)
     (templ central-adj-templ)
;     (templ central-adj-content-templ)
;     (preference .98)
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "I am enthusiastic about her")
     (lf-parent ont::eager-val)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype (? pt w::about)))))
     )
    
   ((example "I am enthusiastic to go")
     (lf-parent ont::eager-val)
     (TEMPL adj-subjcontrol-action-templ)
     )
    )
   )
))


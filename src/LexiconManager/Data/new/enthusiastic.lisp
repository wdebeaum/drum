;;;;
;;;; W::enthusiastic
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
    (W::enthusiastic
   (wordfeats (W::morph (:FORMS ( -LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "I am enthusiastic / an enthusiastic person")
     (LF-PARENT ONT::EMOTIONAL-VAL)
     (templ central-adj-experiencer-templ)
     )
    ((meta-data :origin adj-devel :entry-date 20080926 :change-date nil :comments nil :wn ("happy%3:00:00"))
     (example "an enthusiactic message")
     (LF-PARENT ONT::EMOTIONAL-VAL)
     (templ central-adj-content-templ)
     (preference .98)
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "I am enthusiastic about her")
     (LF-PARENT ONT::EMOTIONAL-VAL)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype (? pt w::about)))))
     )
    )
   )
))


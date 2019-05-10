;;;;
;;;; W::gloomy
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::gloomy
   (wordfeats (W::morph (:FORMS (-er -LY))))
   (SENSES
;    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
;     (example "I am gloomy / a gloomy person")
;     (LF-PARENT ONT::unhappy)
;     (templ central-adj-experiencer-templ)
;     )
    ((meta-data :origin adj-devel :entry-date 20080926 :change-date nil :comments nil :wn ("happy%3:00:00"))
     (example "a gloomy outlook")
     (LF-PARENT ONT::unhappy)
     (templ central-adj-templ)
;     (templ central-adj-content-templ)
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "I am gloomy about it")
     (LF-PARENT ONT::unhappy)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype (? pt w::about)))))
     )
    )
   )
))


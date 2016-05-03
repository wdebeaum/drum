;;;;
;;;; W::cranky
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::cranky
   (wordfeats (W::morph (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "I am cranky / a cranky person")
     (LF-PARENT ONT::agitated)
     (templ central-adj-experiencer-templ)
     )
    ((meta-data :origin adj-devel :entry-date 20080926 :change-date nil :comments nil :wn ("happy%3:00:00"))
     (example "a cranky response") ;; not a stimulus!
     (LF-PARENT ONT::agitated)
     (templ central-adj-content-templ)
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "I am cranky about it")
     (LF-PARENT ONT::agitated)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype (? pt w::about)))))
     )
    )
   )
))


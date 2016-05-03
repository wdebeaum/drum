;;;;
;;;; W::grumpy
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::grumpy
   (wordfeats (W::morph (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "I am grumpy / a grumpy person")
     (LF-PARENT ONT::unpleasant)
     (templ central-adj-experiencer-templ)
     )
    ((meta-data :origin adj-devel :entry-date 20080926 :change-date nil :comments nil :wn ("happy%3:00:00"))
     (example "a grumpy disposition") ;; not a stimulus!
     (LF-PARENT ONT::unpleasant)
     (templ central-adj-content-templ)
     (preference .98)
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "I am angry about it")
     (LF-PARENT ONT::unpleasant)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype (? pt w::about)))))
     )
    #||((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "I am grumpy that she does that")
     (LF-PARENT ONT::unpleasant)
     (TEMPL ADJ-OF-CONTENT-XP-TEMPL)
     )   ||# 
    )
   )
))


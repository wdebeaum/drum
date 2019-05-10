;;;;
;;;; W::tense
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::tense
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ;((meta-data :origin cardiac :entry-date 20080926 :change-date nil :comments LM-vocab)
    ; (example "he is tense / a tense person")
    ; (LF-PARENT ONT::uneasy)
    ; (templ central-adj-experiencer-templ)
    ; )
    ((meta-data :origin adj-devel :entry-date 20080926 :change-date nil :comments nil :wn ("happy%3:00:00"))
     (example "a tense situation")
     (LF-PARENT ONT::uneasy)
     (templ central-adj-templ)
;     (templ central-adj-content-templ)
     )
    )
   )
))


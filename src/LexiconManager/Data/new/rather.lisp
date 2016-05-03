;;;;
;;;; W::rather
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::rather
   (SENSES
    ((LF-PARENT ONT::DEGREE-MODIFIER-MED)
     (example "degree modifiers are rather more difficult than certain others")
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     )
     ((LF-PARENT ONT::DEGREE-MODIFIER-MED)
      (meta-data :origin cardiac :entry-date 20090121 :change-date nil :comments speechtest-transcripts)
     (example "he would rather sleep than get up")
     (TEMPL pred-vp-TEMPL)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::rather W::than)
   (SENSES
    ((LF-PARENT ONT::CHOICE-OPTION)
     (LF-FORM W::INSTEAD-OF)
     (TEMPL binary-constraint-S-templ)
     )
    ((LF-PARENT ONT::CHOICE-OPTION)
     (LF-FORM W::INSTEAD-OF)
     (TEMPL binary-constraint-s-ing-templ)
     )
    )
   )
))


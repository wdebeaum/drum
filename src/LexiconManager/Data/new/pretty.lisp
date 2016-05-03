;;;;
;;;; W::PRETTY
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::PRETTY
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("pretty%5:00:00:beautiful:00") :comlex (ADJECTIVE))
     (LF-PARENT ONT::BEAUTIFUL)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::PRETTY
   (SENSES
    ((LF-PARENT ONT::degree-MODIFIER-MED)
     (example "it's pretty green")
     (LF-FORM W::PRETTY)
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     )
;    ((LF-PARENT ONT::degree-MODIFIER)
;     (example "he got pretty tired")
;     (meta-data :origin chf :entry-date 20070904 :change-date nil :comments nil)
;     (TEMPL PRED-VP-TEMPL)
;     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
   ((W::PRETTY W::MUCH)
   (SENSES
;    ((meta-data :origin monroe :entry-date 20031217 :change-date nil :comments s3)
;     (LF-PARENT ONT::DEGREE-MODIFIER)
;     (LF-FORM W::pretty-much)
;     (TEMPL PRED-VP-TEMPL)
;     )
    ((meta-data :origin monroe :entry-date 20031217 :change-date nil :comments s3)
     (LF-PARENT ONT::degree-MODIFIER-MED)
     (example "it's pretty much over")
     (LF-FORM W::pretty-much)
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     )
    )
   )
))


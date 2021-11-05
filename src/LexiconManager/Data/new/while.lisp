;;;;
;;;; W::while
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::while
   (SENSES
    ((meta-data :origin monroe :entry-date 20031219 :change-date nil :wn ("while%1:28:00") :comments s11)
     (LF-PARENT ONT::time-interval)
     (example "they waited a long while")
     (TEMPL count-pred-templ)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::WHILE
   (SENSES
    ((LF-PARENT ONT::Qualification )
     ;; FIXME -- we might want syntax to be pre-only ;; not true.  We can say "A is true while B is not"
     (TEMPL binary-constraint-s-decl-templ)
     (Example "while A is true, B is not")     
     (meta-data :origin beetle2 :entry-date 20070609 :change-date nil :comments sentential-conjunction-cleanup)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  (W::WHILE
   (SENSES
    ((LF-PARENT ONT::when-while)
     (TEMPL binary-constraint-s-decl-templ)
     )
    ((LF-PARENT ONT::when-while)
     (TEMPL binary-constraint-S-while-loc-templ)
     (example "He ate the pizza while in the house")
     )
    ((LF-PARENT ONT::when-while)
     (TEMPL binary-constraint-S-ing-TEMPL)
     (example "He ate the pizza while drinking coffee.")
     )
    )
   )))


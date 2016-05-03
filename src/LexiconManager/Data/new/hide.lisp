;;;;
;;;; W::HIDE
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 (W::HIDE
   (SENSES
    ((LF-PARENT ONT::HIDE)
     (example "hide the benign objects")
     (meta-data :origin lou :entry-date 20040311 :change-date nil :comments lou-sent-entry)
     )
    ((LF-PARENT ONT::HIDE)
     (example "he hid from me")
     (templ agent-templ)
	      
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::HIDE W::AND W::SEEK)
   (SENSES
    (
     (LF-PARENT ONT::GAME)
     (TEMPL MASS-PRED-TEMPL)
     (syntax (W::morph (:forms (-none))))
     )
    )
   )
))


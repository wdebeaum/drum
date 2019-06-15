;;;;
;;;; W::retract
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::retract
   (wordfeats (W::morph (:forms (-vb) :nom w::retraction)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090602 :comments nil :vn ("remove-10.1"))
     (LF-PARENT ONT::pull-out-of)
     (example "He retracted his bid from the auction")
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090602 :change-date nil :comments nil)
     (LF-PARENT ONT::pull-out-of)
     (TEMPL AFFECTED-SOURCE-XP-OPTIONAL-TEMPL)
     (example "The CD tray retracted")
     )
    )
   )
))


;;;;
;;;; W::augment
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::augment
   (wordfeats (W::morph (:forms (-vb) :nom w::augmentation)))
   (SENSES
    ((example "We augmented the solution with potassium")
     (LF-PARENT ONT::add-include)
     (TEMPL AGENT-AFFECTED-TEMPL)
     )
    ;; need this template to go through the adjectival passive rule
    ((meta-data :origin calo :entry-date 20031230 :change-date 20090908 :comments html-purchasing-corpus)
     (LF-PARENT ONT::add-include)
     (example "the augmented version")
     (TEMPL AGENT-RESULT-XP-NP-TEMPL)
     )
    )
   )
))


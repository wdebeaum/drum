;;;;
;;;; W::augment
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::augment
   (wordfeats (W::morph (:forms (-vb) :nom w::augmentation)))
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date 20090908 :comments html-purchasing-corpus)
     (LF-PARENT ONT::add-include)
     (TEMPL AGENT-GOAL-THEME-TEMPL)
     )
    ;; need this template to go through the adjectival passive rule
    ((meta-data :origin calo :entry-date 20031230 :change-date 20090908 :comments html-purchasing-corpus)
     (LF-PARENT ONT::add-include)
     (example "the augmented version")
     (TEMPL AGENT-GOAL-xp-TEMPL)
     )
    )
   )
))


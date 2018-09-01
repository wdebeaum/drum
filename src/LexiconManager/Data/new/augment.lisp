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
    ((meta-data :origin cause-result-relations :entry-date 20180802 :change-date nil :comments nil)
     (LF-PARENT ONT::intensify)
     (example "the pressure augmented(intensified).")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::intensity-scale))
     (TEMPL affected-unaccusative-TEMPL)
     )   
   )
))


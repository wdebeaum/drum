;;;;
;;;; W::taste
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::taste
   (wordfeats (W::morph (:forms (-vb) :nom W::taste)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("stimulus_subject-30.4") :wn ("taste%2:39:02" "taste%2:39:12"))
     (LF-PARENT ONT::appears-to-have-property)
     (example "it tastes good")
     (TEMPL neutral-pred-xp-templ) ; like look
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::active-perception)
     (TEMPL experiencer-neutral-templ)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::taste
  (senses
   ((LF-PARENT ONT::ability-to-taste)
    (TEMPL MASS-PRED-TEMPL)
    )
   )
)
))

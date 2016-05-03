;;;;
;;;; W::ensue
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::ensue
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("occurrence-48.3") :wn ("ensue%2:42:00"))
     (LF-PARENT ONT::occurring)
     (TEMPL neutral-templ) ; like occur,happen
     (preference .98)
     )
    )
   )
))


(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::ensuing
   (SENSES
    ((meta-data :origin step :entry-date 20080728 :change-date nil :comments nil)
     (LF-PARENT ONT::outcome-val)
     (example "the resulting explosion")
     )
    )
   )
))

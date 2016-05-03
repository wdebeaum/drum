;;;;
;;;; W::deplore
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::deplore
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("admire-31.2") :wn ("deplore%2:32:00" "deplore%2:32:01"))
     (LF-PARENT ONT::disliking)
     (TEMPL experiencer-action-objcontrol-templ) ; like suffer
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090508 :comments nil :vn ("admire-31.2") :wn ("deplore%2:32:00" "deplore%2:32:01"))
     (LF-PARENT ONT::disliking)
     (TEMPL experiencer-neutral-xp-templ) ; like admire,adore,appreciate,despise,detest,dislike,loathe,miss
     )
    )
   )
))


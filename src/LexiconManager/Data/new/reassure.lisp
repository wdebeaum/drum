;;;;
;;;; W::reassure
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::reassure
       (wordfeats (W::morph (:forms (-vb) :nom W::reassurance)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("amuse-31.1") :wn ("reassure%2:32:00" "reassure%2:37:00"))
     (LF-PARENT ont::cause-body-effect)
     (TEMPL agent-affected-xp-templ) ; like annoy,bother,concern,hurt
     )
    )
   )
))


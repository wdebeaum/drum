;;;;
;;;; w::pronounce
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (w::pronounce
   (wordfeats (W::morph (:forms (-vb) :nom w::pronunciation)))
   (senses
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090501 :comments nil :vn ("dub-29.3") :wn ("pronounce%2:32:00" "pronounce%2:32:01"))
     (LF-PARENT ONT::declare-performative)
     (TEMPL agent-neutral-name-templ) ; like call
     (PREFERENCE 0.96)
     )
    ((lf-parent ont::locution)
     (example "pronounce the word for me")
     (templ agent-neutral-xp-templ)
     (meta-data :origin task-learning :entry-date 20050815 :change-date nil :comments nil)
    )
   ))
))


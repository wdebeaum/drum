;;;;
;;;; W::stimulate
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::stimulate
   (wordfeats (W::morph (:forms (-vb) :nom w::stimulation)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("amuse-31.1") :wn ("stimulate%2:29:00" "stimulate%2:37:00" "stimulate%2:39:00"))
     (LF-PARENT ont::cause-stimulate)
     (example "the speech stimulated the crowd (in)to action" )
     (TEMPL agent-affected-effect-optional-templ (xp (% w::pp (w::ptype (? pt w::into))))) ; like annoy,bother,concern,hurt
      )
    ((LF-PARENT ont::cause-stimulate)
     (TEMPL agent-affected-xp-templ)
     (example "the lecture stimulated conversation")
     )
    )
   )
))


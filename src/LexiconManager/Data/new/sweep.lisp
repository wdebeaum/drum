;;;;
;;;; W::sweep
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::sweep
   (wordfeats (W::morph (:forms (-vb) :past W::swept :ing W::sweeping)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("funnel-9.3-2-1"))
     (LF-PARENT ONT::move)
     ;;(TEMPL agent-affected-source-templ (xp (% w::pp (w::ptype (? t w::off w::from))))) ; like dump
     (TEMPL agent-affected-xp-templ)
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("sweep%2:35:03" "sweep%2:38:00"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))


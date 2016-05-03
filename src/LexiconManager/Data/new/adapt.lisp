;;;;
;;;; W::ADAPT
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::ADAPT
   (SENSES
    ((EXAMPLE "this projector is specifically adapted for home theater")
     ;;(LF-PARENT ONT::ACCOMMODATE)
     (lf-parent ont::adapt) ;; 20120524 GUM change new parent
     (TEMPL agent-affected-theme-TEMPL (xp (% W::pp (W::ptype (? pt W::for w::to)))))
     (meta-data :origin calo :entry-date 20050527 :change-date nil :comments projector-purchasing)
     )
    ))
))


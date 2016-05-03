;;;;
;;;; W::handle
;;;;

(define-words :pos W::v 
 :words (
  (W::handle
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("hold-15.1-1"))
     (LF-PARENT ONT::body-manipulation)
     (TEMPL agent-affected-xp-templ) ; like clutch,grip,clasp,hold,wield,grasp
     (PREFERENCE 0.96)
     )
    ((meta-data :origin monroe :entry-date 20031217 :change-date nil :comments s7)
     ;;(LF-PARENT ONT::managing)
     (lf-parent  ont::manage) ;; 20120521 GUM change new parent 
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-affected-XP-TEMPL)
     (example "handle the situation")
     )
    )
   )
))


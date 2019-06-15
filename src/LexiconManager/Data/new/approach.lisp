;;;;
;;;; w::approach
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (w::approach
   (senses
    ((lf-parent ont::procedure)
     (templ other-reln-templ (xp (% W::pp (W::ptype (? pt W::for)))))
     (example "the approach for solving this problem")
     (meta-data :origin integrated-learning :entry-date 20050817 :change-date nil :wn ("approach%1:04:02") :comments lf-restructuring)
     )	   	   	   	   
    ))
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::approach
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("escape-51.1-2"))
     (LF-PARENT ONT::move-toward)
     (TEMPL affected-templ) ; like go,fall
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::MOVE-toward)
     (meta-data :origin ralf :entry-date 20040621 :change-date nil :comments ralf.txt)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "the truck/I approached rochester")
     (TEMPL agent-neutral-TEMPL)
     )
    )
   )
))


;;;;
;;;; W::induce
;;;;

(define-words :pos W::v 
 :words (
  (W::induce
   (wordfeats (W::morph (:forms (-vb) :nom w::induction :agentnom w::inducer)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("59-force"))
     ;(LF-PARENT ont::provoke)
     (LF-PARENT ont::cause-effect)   
     ;(TEMPL agent-affected-theme-objcontrol-templ)  ; like dare
     (TEMPL agent-effect-affected-objcontrol-templ)  ; like dare
     (example "He induced him to run for office")  
     )
    (;(LF-PARENT ont::cause-stimulate)
     (LF-PARENT ont::cause-produce-reproduce)   
     (example "the chemical induces phosphorylization")
     (templ agent-affected-xp-templ))
    
    #||((LF-PARENT ont::provoke)
     (TEMPL agent-effect-affected-optional-templ (xp (% w::pp (w::ptype (? pt w::from w::in w::among)))))
     (example "His manner induced anger [in them]")  
     )
    )||#
   ))
))


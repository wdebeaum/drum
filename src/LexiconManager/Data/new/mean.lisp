;;;;
;;;; W::mean
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::mean
   (wordfeats (W::morph (:forms (-vb) :ing W::meaning :past W::meant)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("wish-62"))
     (LF-PARENT ONT::intention)
     (TEMPL experiencer-theme-xp-templ (xp (% w::cp (w::ctype w::s-to)))) ; like plan,intend
     (PREFERENCE 0.96)
     )
    ;;;; I mean that ...
    ((LF-PARENT ONT::intention)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (example "I mean that ...")
     (TEMPL experiencer-theme-xp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    ((LF-PARENT ONT::intention)
     (example "I mean it/yes")
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL experiencer-theme-xp-templ (xp (% w::utt)))
     )
    ;; can't push a w::utt through the wh gap rules so we need this -- should be fixed
    ((LF-PARENT ONT::intention)
     (example "what do you mean")
     (meta-data :origin calo :entry-date 20050505 :change-date nil :comments plow)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL experiencer-theme-xp-templ)
     )    
    )
   )
))

(define-words :pos W::V 
  :templ agent-theme-xp-templ
 :tags (:base500)
 :words (
	  (w::mean
	   (senses 
	    ((lf-parent ont::correlation)
	     (templ neutral-theme-xp-templ)
	     (example "an alarm means trobule")
	     )	    
	    ((lf-parent ont::correlation) 
	     (templ neutral-theme-xp-templ (xp (% w::cp (w::ctype w::s-that))))
	     (example "the exmpalation means that ...")
	     )	    	    
	    )
	   )
))


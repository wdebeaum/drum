;;;;
;;;; W::accept
;;;;

(define-words :pos W::v 
 :words (
  (W::accept
     (wordfeats (W::morph (:forms (-vb) :nom w::acceptance)))
   (SENSES
    ((lf-parent ont::accept-agree) 
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (templ agent-affected-xp-np-templ)
     (example "he accepted the cookie")
     )
    ((lf-parent ont::accept-agree) 
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-FORMAL-XP-PP-TEMPL (xp (% W::cp (W::ctype W::s-finite))))    
     (example "he accepted that the cookie was wet")
     )
    ((LF-PARENT ONT::is-compatible-with)
     (SEM (F::Time-span F::extended) (f::trajectory -))
     (example "that projector accepts european voltage")
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     (meta-data :origin "wordnet-3.0" :entry-date 20090501 :change-date nil :comments nil)
     )
    )
   )
))

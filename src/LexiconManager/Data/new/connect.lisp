;;;;
;;;; W::connect
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::connect
   (wordfeats (W::morph (:forms (-vb) :nom W::connection)))
   (SENSES
    ;;;; well, the usage sounds weird to me, but maybe...
    ;;;; a truck connected with a train
    ((LF-PARENT ONT::attach)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL (xp (% W::PP (W::ptype W::with))))
     )
    ;;;; connect the truck to the train
    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-OPTIONAL-TEMPL (xp (% W::pp (W::ptype W::to))))
     )
    ;;;; connect the battery with the bulb
    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-TEMPL (xp (% W::pp (W::ptype W::with))))
     (preference 0.99)
     )
    ;;;; the path connects one city to/with another
    ((LF-PARENT ONT::CONNECTED)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-NEUTRAL2-XP-TEMPL  (xp (% W::pp (W::ptype (? ptype W::to W::with)))))
     )
    ;;;; the path links 2 cities
    ((LF-PARENT ONT::CONNECTED)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-NP-PLURAL-TEMPL)
     )
    ;;;; one city connects to/with another
    ((LF-PARENT ONT::CONNECTED)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% W::pp (W::ptype (? ptype W::to W::with)))))
     )

    ;;;; Myrosia 12/31/00 2 streets connect/intersect/meet
    ((LF-PARENT ONT::CONNECTED)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NP-PLURAL-TEMPL)
     )
        
    )
   )
  ))

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-AFFECTED1-XP-OPTIONAL-TEMPL
 :words (
	  (w::connect
	   (senses
	    ((lf-parent ont::attach)
	     (meta-data :origin bee :entry-date 20040407 :change-date nil :comments test-s40) 
	     (example "Connect the device - the intransitive usage found in bee data"))
	    ))
))


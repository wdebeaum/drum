;;;;
;;;; W::connect
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::connect
   (wordfeats (W::morph (:forms (-vb) :nom W::connection)))
   (SENSES
    ;;;; well, the usage sounds weird to me, but maybe...
    ;;;; a truck connected with a train
    ((LF-PARENT ONT::attach)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-XP-TEMPL (xp (% W::PP (W::ptype W::with))))
     )
    ;;;; connect the truck to the train
    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-affected2-optional-TEMPL (xp (% W::pp (W::ptype W::to))))
     )
    ;;;; connect the battery with the bulb
    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-affected2-TEMPL (xp (% W::pp (W::ptype W::with))))
     (preference 0.99)
     )
    ;;;; the path connects one city to/with another
    ((LF-PARENT ONT::CONNECTED)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-neutral-neutral-xp-templ  (xp (% W::pp (W::ptype (? ptype W::to W::with)))))
     )
    ;;;; the path links 2 cities
    ((LF-PARENT ONT::CONNECTED)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-neutral-plural-templ)
     )
    ;;;; one city connects to/with another
    ((LF-PARENT ONT::CONNECTED)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-neutral-templ (xp (% W::pp (W::ptype (? ptype W::to W::with)))))
     )

    ;;;; Myrosia 12/31/00 2 streets connect/intersect/meet
    ((LF-PARENT ONT::CONNECTED)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-plural-templ)
     )
        
    )
   )
  ))

(define-words :pos W::V 
  :templ agent-affected2-optional-templ
 :words (
	  (w::connect
	   (senses
	    ((lf-parent ont::attach)
	     (meta-data :origin bee :entry-date 20040407 :change-date nil :comments test-s40) 
	     (example "Connect the device - the intransitive usage found in bee data"))
	    ))
))


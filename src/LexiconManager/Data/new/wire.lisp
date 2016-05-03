;;;;
;;;; w::wire
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
	 ;; defined separately from the other components because we need extra spatial abstraction features
	 (w::wire
	  (senses ((lf-parent ont::Device-component) 
		   (sem (f::spatial-abstraction (? saa f::line f::spatial-point)))
		   (meta-data :origin bee :entry-date 20040407 :change-date nil :comments test-s)
		   )))
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 (W::wire
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("send-11.1-1"))
     (LF-PARENT ONT::send)
     (TEMPL agent-affected-recipient-alternation-templ) ; like mail,send,forward,transmit
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::ATTACH)
     (example "wire the computer to the network")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-affected2-optional-TEMPL (xp (% W::pp (W::ptype W::to))))
     )
    )
   )
))


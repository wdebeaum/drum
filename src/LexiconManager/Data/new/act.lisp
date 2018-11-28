;;;;
;;;; W::ACT
;;;;

#|
(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
;; adding (reinstating) this as ont::act for gloss
 (W::ACT
   (SENSES
    ((meta-data :origin jr :entry-date 20120426 :comment gloss-variant)
     (LF-PARENT ONT::ACT)
     (templ other-reln-templ)
     )
    )
   )
))
|#

(define-words :pos W::V 
  :templ agent-theme-xp-templ
 :tags (:base500)
 :words (
	  (w::act
;; 20120426 :origin jr removing nominalization for gloss to improve control over the key word "act"
;; BOB: put the noun "act" back here; let's see what happens
	   (wordfeats (W::morph (:forms (-vb) :nom W::act)))
	   (senses
	    ((lf-parent ont::act-behave)
	      ;; the like modification is a function of the adverbial
;	     (templ agent-theme-optional-templ (xp (% w::pp (w::ptype (? pt w::like w::as)))))
	     (templ agent-templ)
	     ;;(templ agent-formal-templ (xp (% w::pp (w::ptype (? pt w::like w::as)))))
	     (example "he acts as/like a judge/ in the play")
	     (meta-data :origin bee :entry-date 20040609 :change-date nil :comments portability-experiment)
	     )
	    ;; this should be a different sense
;	    ((lf-parent ont::acting)
;	     (templ agent-effect-xp-templ)
;	     (example "he acted the part")
;	     )
	    ((LF-PARENT ONT::EXECUTE)
	     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
;	     (TEMPL agent-effect-xp-templ (xp (% w::pp (w::ptype w::on))))
;	     (TEMPL AGENT-THEME-OPTIONAL-TEMPL (xp (% w::pp (w::ptype w::on))))
	     (TEMPL AGENT-NEUTRAL-xp-TEMPL (xp (% w::pp (w::ptype w::on))))
	     (example "he acted on the plan")
	     )
	    ))
))


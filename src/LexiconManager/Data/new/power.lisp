;;;;
;;;; W::POWER
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::POWER w::supply)
    (wordfeats (W::morph (:forms (-S-3P) :plur (W::power W::supplies))))
   (SENSES
    ((meta-data :origin calo :entry-date 20050308 :change-date nil :comments projector-purchasing)
     (LF-PARENT ONT::device)
     (TEMPL count-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::POWER
   (SENSES
    (;; changed from ont::substance to newly created ont::power for AKRL in OBTW demo
     (meta-data :origin trips :entry-date 20060803 :change-date 20110926 :comments nil)
     (LF-PARENT ONT::power)
     (TEMPL MASS-PRED-TEMPL)
     )
    (
     (LF-PARENT ONT::able-scale)
     (templ other-reln-templ (XP (% W::CP (W::CTYPE W::s-to))))
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
 (w::power
  (senses
   ((LF-PARENT ONT::control-manage)
     (example "the piston powers the cylinder")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (meta-data :origin LbR :entry-date 20080922 :change-date nil :comments nil)
     )
   )
  )
))


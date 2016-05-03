;;;;
;;;; W::UH
;;;;

(define-words :pos W::fp :boost-word t
 :tags (:fp)
 :words (
  (W::UH
  (senses
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;
   ;;;; myrosia and swift 03/20&21/02
   ;;;; move these from grammar (fnword-lex.lisp) to lexicon
   ;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ((LF ONT::FILLED-PAUSE)
    (non-hierarchy-lf t)(TEMPL fp-templ)
    )
   )
)
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::uh W::huh)
   (SENSES
    ((LF (W::uhhuh))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_ACK))
     )
    )
   )
))


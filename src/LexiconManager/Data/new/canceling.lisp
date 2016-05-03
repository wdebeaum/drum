;;;;
;;;; W::canceling
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
;; alternate ing form
  (W::canceling
   (wordfeats (W::morph (:forms NIL)) (W::AGR ?agr) (W::vform W::ing))
   (SENSES
    ((LF-PARENT ONT::CANCEL)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-neutral-XP-TEMPL)
     (example "cancel the meeting")
     )
        )
   )
))


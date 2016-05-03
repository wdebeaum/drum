;;;;
;;;; W::live
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
   (W::live
   (SENSES
    
     ((LF-PARENT ONT::live)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL affected-TEMPL)
     (example "he lived to a ripe old age")
     )
    )
   )
))


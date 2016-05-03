;;;;
;;;; W::REQUEST
;;;;

#|
(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::REQUEST
   (SENSES
     ((LF-PARENT ONT::request)
      (example "this request is a book order")
      (meta-data :origin task-learning :entry-date 20050901 :change-date nil :wn ("request%1:10:00" "request%1:10:01") :comments nil)
      (TEMPL OTHER-RELN-theme-TEMPL)
     )
    )
   )
))
|#

(define-words :pos W::v 
 :words (
  (W::REQUEST
   (wordfeats (W::morph (:forms (-vb) :nom W::request)))
   (SENSES
    ((EXAMPLE "request something")
     (LF-PARENT ONT::REQUEST)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-XP-TEMPL)
     )
    ((EXAMPLE "request that he do something")
     (LF-PARENT ONT::REQUEST)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-effect-XP-TEMPL (xp (% W::cp (W::ctype (? c W::s-that s-that-subjunctive)))))
     )
    ((EXAMPLE "request him to do something")
     (LF-PARENT ONT::request)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-ADDRESSEE-effect-OBJCONTROL-TEMPL)
     )
    )
   )
))


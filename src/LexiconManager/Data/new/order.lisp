;;;;
;;;; W::ORDER
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
    (W::ORDER
   (SENSES
    ((meta-data :origin calo :entry-date 20030605 :change-date nil :wn ("order%1:10:01") :comments calo-y1script)
     (LF-PARENT ONT::ORDER)
     (TEMPL OTHER-RELN-TEMPL (xp (% W::pp (W::ptype W::for))))
     (example "send the order to purchasing")
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
(W::order
 (wordfeats (W::morph (:forms (-vb) :past W::ordered :ing w::ordering)))
 (SENSES
  ((lf-parent ont::reserve)
   (example "order the book from amazon dot com")
   (SEM (F::Aspect F::bounded) (F::Time-span F::atomic) (F::trajectory -))
   (TEMPL agent-affected-xp-templ)
   )
  ))
))


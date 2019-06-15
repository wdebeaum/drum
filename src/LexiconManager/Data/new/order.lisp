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

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
(W::order
 (wordfeats (W::morph (:forms (-vb) :past W::ordered :ing w::ordering)))
 (SENSES
  ((lf-parent ont::reserve)
   (example "order the book from amazon dot com")
   (SEM (F::Aspect F::bounded) (F::Time-span F::atomic) (F::trajectory -))
   (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
   )

    ((lf-parent ont::arranging)
     (example "reorder them")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (meta-data :origin task-learning :entry-date 20050826 :change-date nil :comments nil)
     )
    ((lf-parent ont::arranging)
     (example "reorder them by departure date")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-FORMAL-XP-PP-WITH-TEMPL (xp (% W::PP (W::ptype W::by))))
     (meta-data :origin plow :entry-date 20070928 :change-date nil :comments plow-travel)
     )
  
  ))
))


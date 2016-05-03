;;;;
;;;; w::click
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;    )
    (w::click
    (senses
;     ((lf-parent ont::event)
;      (templ count-pred-templ)
;      (meta-data :origin plow :entry-date 20070808 :change-date nil :comments nil)
;      (example "he heard a click")
;      )
     ((lf-parent ont::click)
      (templ other-reln-theme-templ  (xp (% W::pp (W::ptype (? pt w::on)))))
      (meta-data :origin plow :entry-date 20070808 :change-date nil :comments nil)
      (example "there was a click on that link")
      )
     ))
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::click
   (wordfeats (W::morph (:forms (-vb) :nom W::click)))
   (SENSES
    ((EXAMPLE "click on the url")
     (LF-PARENT ONT::click)
     (meta-data :origin plow :entry-date 20050315 :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-affected-XP-TEMPL (xp (% w::pp (w::ptype (? ptp w::on)))))
     )
    ((EXAMPLE "click the link in your browser")
     (LF-PARENT ONT::click)
     (meta-data :origin plow :entry-date 20050321 :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    ((EXAMPLE "click in the window") 
     (LF-PARENT ONT::click)
     (meta-data :origin plow :entry-date 20060808 :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     (preference .98) ;; prefer transitive sense
     )
    )
   )
))


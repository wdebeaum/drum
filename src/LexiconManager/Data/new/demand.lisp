;;;;
;;;; W::demand
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::demand
   (wordfeats (W::morph (:forms (-vb) :nom W::demand)))
   (SENSES
    ((EXAMPLE "demand something")
     (LF-PARENT ONT::REQUEST)
     ;;(lf-parent ont::appeal-apply-demand) ;; 20120523 GUM change new parent
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (meta-data :origin "verbnet-2.0" :entry-date 20060505 :change-date nil :comments nil :vn ("order-60"))
     )
    ((EXAMPLE "demand that he do something")
     (LF-PARENT ONT::REQUEST)
     ;;(lf-parent ont::appeal-apply-demand) ;; 20120523 GUM change new parent
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-NP-TEMPL (xp (% W::cp (W::ctype (? c W::s-that w::s-that-subjunctive)))))
     (meta-data :origin "verbnet-2.0" :entry-date 20060505 :change-date nil :comments nil :vn ("order-60"))
     )
    )
   )
))


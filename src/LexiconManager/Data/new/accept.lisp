;;;;
;;;; W::accept
;;;;

(define-words :pos W::v :TEMPL AFFECTED-AFFECTED1-XP-NP-TEMPL
 :words (
  (W::accept
     (wordfeats (W::morph (:forms (-vb) :nom w::acceptance)))
   (SENSES
    ((lf-parent ont::incur-inherit-receive) ;; 20120523 GUM change new parent
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "accept the cookie")
     )
    ((LF-PARENT ONT::is-compatible-with)
     (SEM (F::Time-span F::extended) (f::trajectory -))
     (example "that projector accepts european voltage")
     (TEMPL NEUTRAL-FORMAL-XP-NP-2-TEMPL)
     (meta-data :origin "wordnet-3.0" :entry-date 20090501 :change-date nil :comments nil)
     )
    )
   )
))

;;;;
;;;; w::observe
;;;;

(define-words :pos W::V 
  :words (
    (w::observe
     (senses
      (
       (lf-parent ont::active-perception) 
       (templ agent-neutral-templ)
       )

    (
     (LF-PARENT ONT::becoming-aware)
     (TEMPL agent-theme-xp-templ (xp (% w::cp (w::ctype w::s-finite))))
     (PREFERENCE 0.96)
     )
    (
     (LF-PARENT ONT::becoming-aware)
     (TEMPL agent-templ) 
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::becoming-aware)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-neutral-xp-templ)
     (example "he observed the damage")
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Perceive :vn ("see-30.1-1"))
     )

    ))
))


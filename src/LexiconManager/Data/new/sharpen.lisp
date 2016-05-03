;;;;
;;;; w::sharpen
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (w::sharpen
   (senses
    ((meta-data :origin "wordnet-3.0" :entry-date 20090505 :change-date nil :comments nil)
     (LF-PARENT ONT::sharpen-soft) ;; GUM change : new parent
     (example "sharpen the pencil")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090505 :change-date nil :comments nil)
     (LF-PARENT ONT::visual-adjust)
     (example "sharpen the image")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))


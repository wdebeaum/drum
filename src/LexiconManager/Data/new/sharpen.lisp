;;;;
;;;; w::sharpen
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (w::sharpen
   (senses
    ((meta-data :origin "wordnet-3.0" :entry-date 20090505 :change-date nil :comments nil)
     (LF-PARENT ONT::sharpen) ;; GUM change : new parent
     (example "sharpen the pencil")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended) (f::scale ont::sharp-texture-scale))
     )
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "the lava rocks sharpened")
     (LF-PARENT ONT::sharpen)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::sharp-texture-scale))
     (TEMPL affected-unaccusative-templ)
     )

    ((meta-data :origin "wordnet-3.0" :entry-date 20090505 :change-date nil :comments nil)
     (LF-PARENT ONT::visual-adjust)
     (example "sharpen the image")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))


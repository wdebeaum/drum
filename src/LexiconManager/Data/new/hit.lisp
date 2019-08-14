;;;;
;;;; W::hit
;;;;

;; ;; 20121212 GUM change delete type and associated words

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::hit
   (wordfeats (W::morph (:forms (-vb) :past W::hit :nom w::hit)))
   (SENSES
    ((LF-PARENT ONT::hitting)
     (example "a truck hit a train")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (meta-data :origin monroe :entry-date 20031216)
     )
    #|
    (
     ;(LF-PARENT ONT::type)
     (lf-parent ont::author-write-burn-print_reprint_type_retype_mistype)
     (example "hit the key")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin plot :entry-date 20080411 :change-date nil :comments nil)
     (TEMPL agent-affected-XP-TEMPL)
     )
    |#

    #|
    ((LF-PARENT ONT::hitting)
     (example "2 trucks hit")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-PLURAL-TEMPL)
     (meta-data :origin monroe :entry-date 20031216)
     )
    |#
    
    ((LF-PARENT ONT::INTERSECT)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     (example "street a hits street b at 490")
     (meta-data :origin monroe :entry-date 20031216)
     )
    )
   )
))


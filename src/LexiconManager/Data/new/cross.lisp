;;;;
;;;; W::cross
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::cross
   (SENSES
    ((meta-data :origin newbeegle :entry-date 20050211 :change-date nil :wn ("cross%1:07:00") :comments nil)
     (example "the red cross on a battery means it is in a short circuit")
     (LF-PARENT ONT::SHAPE-OBJECT)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::cross
   (wordfeats (W::morph (:forms (-vb) :3s W::crosses)))
   (SENSES
    ;;;; swier -- I-90 and I-490 cross
    ((LF-PARENT ONT::INTERSECT)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NP-PLURAL-TEMPL)
     )
    ;;;; street a crosses/meets street b
    ((LF-PARENT ONT::INTERSECT)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     )
    ;;  cross a river
    ((LF-PARENT ONT::PASS-ACROSS)
     (TEMPL AGENT-NEUTRAL-XP-TEMPL)
     )
    )
   )
))


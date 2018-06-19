;;;;
;;;; W::mark
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::mark
   (SENSES
    ((LF-PARENT ONT::graphic-symbol)
     (example "it's a question mark")
     (meta-data :origin plow :entry-date 20050922 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::mark
   (wordfeats (W::morph (:forms (-vb) :ing W::marking)))
   (SENSES
    ((EXAMPLE "Mark those points red")
     (LF-PARENT ONT::classify)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-theme-templ)
     (preference .97) ;; prefer the highlight sense
     )
    ((EXAMPLE "Mark the search criteria")
     ;(LF-PARENT ONT::select) ;; like select
     (LF-PARENT ONT::classify) ;; like select
     (TEMPL agent-neutral-xp-templ)
     (meta-data :origin plot :entry-date 20081024 :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::mark (W::off))
   (SENSES
    ((EXAMPLE "Mark off those points")
     (LF-PARENT ONT::highlight)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    )
   )
))


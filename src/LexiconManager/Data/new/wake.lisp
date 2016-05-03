;;;;
;;;; w::wake
;;;;

(define-words :pos W::n
 :words (
;; also type of located event, but with an associated ceremony
  (w::wake
  (senses
   ((LF-PARENT ONT::ceremony)
    (meta-data :origin chf :entry-date 20070809 :change-date nil :comments nil)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::v 
 :words (
  ((W::wake (W::up))
   (wordfeats (W::morph (:forms (-vb) :past W::woke :pastpart W::woken :ing W::waking)))
   (SENSES
    ((EXAMPLE "As soon as I wake up")
     (LF-PARENT ONT::BODILY-PROCESS)
     (LF-FORM W::wake-up)
     (SEM (F::trajectory -))
     (TEMPL agent-TEMPL)
     )
    ((EXAMPLE "he woke up happy")
     (LF-PARENT ONT::BODILY-PROCESS)
     (SEM (F::trajectory -))
     (TEMPL agent-PRED-TEMPL)
     )
    ((EXAMPLE "he/it woke him up")
     (LF-PARENT ONT::objective-influence)
     (SEM (F::trajectory -))
     (preference .98)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  (W::wake
   (wordfeats (W::morph (:forms (-vb) :past W::woke :pastpart W::woken :ing W::waking)))
   (SENSES
    ((EXAMPLE "he woke at dawn")
     (LF-PARENT ONT::BODILY-PROCESS)
     (LF-FORM W::wake-up)
     (SEM (F::trajectory -))
     (TEMPL agent-TEMPL)
     )
    ((EXAMPLE "he woke happy")
     (LF-PARENT ONT::BODILY-PROCESS)
     (SEM (F::trajectory -))
     (TEMPL agent-PRED-TEMPL)
     )
    ((EXAMPLE "he/it woke him")
     (LF-PARENT ONT::objective-influence)
     (SEM (F::trajectory -))
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))


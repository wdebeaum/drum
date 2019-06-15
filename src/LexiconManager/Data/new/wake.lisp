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
  (W::wake
   (wordfeats (W::morph (:forms (-vb) :past W::woke :pastpart W::woken :ing W::waking)))
   (SENSES
    ((EXAMPLE "he woke at dawn")
     (LF-PARENT ONT::awake)
     (LF-FORM W::wake-up)
     (SEM (F::trajectory -))
     (TEMPL affected-TEMPL)
     )
    ((EXAMPLE "he/it woke him")
     (LF-PARENT ONT::objective-influence)
     (SEM (F::trajectory -))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))


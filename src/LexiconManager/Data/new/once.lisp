;;;;
;;;; W::once
;;;;

(define-words :pos W::ADV
 :words (
   ((W::once w::in w::a w::while)
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090319 :change-date nil :comments nil)
     (LF-PARENT ONT::FREQUENCY)
     (lf-form w::sometimes)
     (TEMPL pred-s-post-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::once w::in w::awhile)
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090319 :change-date nil :comments nil)
     (LF-PARENT ONT::FREQUENCY)
     (lf-form w::sometimes)
     (TEMPL pred-s-post-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::ONCE
   (SENSES
    ((LF-PARENT ONT::event-time-rel)
     (example "once he was a child")
     (TEMPL pred-s-vp-TEMPL)
     (preference .98)
     )
    ((LF-PARENT ONT::event-time-rel)
     (TEMPL binary-constraint-S-decl-TEMPL)
     )
    ((LF-PARENT ONT::repetition)
     (example "take the medication once")
     (TEMPL PRED-S-post-TEMPL)
     )
    )
   )
))


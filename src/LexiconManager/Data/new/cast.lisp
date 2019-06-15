;;;;
;;;; w::cast
;;;;

(define-words :pos W::n
 :words (
  (w::cast
  (senses
   ((LF-PARENT ONT::medical-instrument)
    (meta-data :origin cardiac :entry-date 20090129 :change-date nil :comments LM-vocab)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::cast
   (wordfeats (W::morph (:forms (-vb) :past W::cast :ing W::casting)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090501 :comments nil :vn ("characterize-29.2") :wn ("cast%2:32:00" "cast%2:36:01"))
     (LF-PARENT ONT::classify)
     (TEMPL AGENT-NEUTRAL-FORMAL-2-XP-3-XP2-PP-TEMPL) ; like interpret,classify
     )
    )
   )
))


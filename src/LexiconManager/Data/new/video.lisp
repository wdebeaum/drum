;;;;
;;;; w::video
;;;;

(define-words :pos W::n
 :words (
  ((w::video w::game)
  (senses
   ((LF-PARENT ONT::game)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((w::VIDEO W::CAMERA)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::video W::cameras))))
   (SENSES
    ((meta-data :origin boudreaux :entry-date 20041116)
     (LF-PARENT ONT::RECORDING-DEVICE)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::VIDEO
   (SENSES
    ((LF-PARENT ONT::IMAGE)
     (TEMPL OTHER-RELN-TEMPL)
     (meta-data :origin boudreaux :entry-date 20031025)
     )
    )
   )
))


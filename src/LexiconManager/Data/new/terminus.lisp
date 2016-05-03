;;;;
;;;; W::terminus
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::terminus
   (SENSES
    ((LF-PARENT ONT::TERMINUS)
     (meta-data :origin BOB :entry-date 20150109 :change-date nil)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((w::N W::PUNC-MINUS W::terminus)
   (SENSES
    ((LF-PARENT ONT::TERMINUS)
     (meta-data :origin BOB :entry-date 20150109 :change-date nil)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((w::C W::PUNC-MINUS W::terminus)
   (SENSES
    ((LF-PARENT ONT::TERMINUS)
     (meta-data :origin BOB :entry-date 20150109 :change-date nil)
     )
    )
   )
))

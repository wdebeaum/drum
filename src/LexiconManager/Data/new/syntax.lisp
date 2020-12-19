;;;;
;;;; W::syntax
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::syntax
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((LF-PARENT ONT::linguistic-component) ;linguistic-object)
;     (TEMPL OTHER-RELN-TEMPL)
;     (SYNTAX (W::AGR W::3S))
     (EXAMPLE "this is the syntax of the language")
     (meta-data :origin task-learning :entry-date 20050926 :change-date nil :wn ("syntax%1:09:01") :comments nil)
     )
    )
   )
))


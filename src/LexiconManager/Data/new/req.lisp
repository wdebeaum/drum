;;;;
;;;; W::req
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::req
   (SENSES
    ((meta-data :origin calo :entry-date 20031211 :change-date nil :comments calo-y1script)
     (LF-PARENT ONT::order)
     (TEMPL OTHER-RELN-TEMPL (xp (% W::pp (W::ptype W::for))))
     (example "send the req to purchasing")
     )
    )
   )
))


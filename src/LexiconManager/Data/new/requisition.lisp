;;;;
;;;; W::requisition
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::requisition
   (SENSES
    ((meta-data :origin calo :entry-date 20031211 :change-date nil :wn ("requisition%1:10:01") :comments calo-y1script)
     (LF-PARENT ONT::order)
     (TEMPL OTHER-RELN-TEMPL (xp (% W::pp (W::ptype W::for))))
     (example "send the requisition to purchasing")
     )
    )
   )
))


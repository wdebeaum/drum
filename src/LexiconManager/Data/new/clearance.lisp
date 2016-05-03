;;;;
;;;; W::clearance
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::clearance
   (SENSES
    ((EXAMPLE "get clearance for the event")
     (LF-PARENT ONT::allow) 
      (TEMPL subcat-mass-effect-TEMPL (xp (% W::pp (W::ptype (? ptp W::for)))))
     (meta-data :origin caloy3 :entry-date 20070330 :change-date nil :comments y3-test-data)
     )
    #||((EXAMPLE "get clearance")
     (LF-PARENT ONT::allow)
     (TEMPL other-reln-effect-TEMPL)     
     (meta-data :origin caloy3 :entry-date 20070330 :change-date nil :comments y3-test-data)
     )||#
    )
   )
))


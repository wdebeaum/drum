;;;;
;;;; W::permission
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;   )
  (W::permission
   (SENSES
    ((EXAMPLE "can I have permission to move it")
     (LF-PARENT ONT::allow)
     (TEMPL SUBCAT-mass-EFFECT-TEMPL)
     (meta-data :origin lou :entry-date 20031103)
     )
    ((EXAMPLE "get permission for the purchase")
     (LF-PARENT ONT::allow) 
     (TEMPL subcat-mass-effect-TEMPL (xp (% W::pp (W::ptype (? ptp W::for)))))
     (meta-data :origin calo :entry-date 20040128 :comments calo-y1script )
     )
    #||((EXAMPLE "get permission")
     (LF-PARENT ONT::allow) 
     (TEMPL other-reln-effect-TEMPL)     
     (meta-data :origin calo :entry-date 20040128 :comments nil )
     )||#
    )
   )
))


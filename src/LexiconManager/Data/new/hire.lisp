;;;;
;;;; W::hire
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::hire
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090429 :comments nil :vn ("get-13.5.1") :wn ("hire%2:40:00" "hire%2:41:00" "hire%2:41:01"))
     (LF-PARENT ONT::lease-hire)    
 ; like procure,order,buy
     (example "he hired a car for a week")
     (preference .96) 
     )
    #||((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("get-13.5.1") :wn ("hire%2:40:00" "hire%2:41:00" "hire%2:41:01"))
     (LF-PARENT ONT::lease-hire)
     (example "he hired him a car for a week")
     (TEMPL agent-recipient-affected-templ) ; like buy,order
     (preference .98)
     )||#
     ((LF-PARENT ONT::employ)
     (example "Abrams hired this employee")
     (meta-data :origin csli-ts :entry-date 20070313 :change-date 20090508 :comments nil :wn nil)
     )
    #||((LF-PARENT ONT::employ)  ;; this is purpose
     (example "Abrams hired him to do the job")
     (meta-data :origin csli-ts :entry-date 20070313 :change-date 20090508 :comments nil :wn nil)
     (templ agent-affected-effect-objcontrol-templ)
     )||#
    )
   )
))


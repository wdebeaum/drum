;;;;
;;;; W::procure
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::procure
   (wordfeats (W::morph (:forms (-vb) :nom W::procurement)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("get-13.5.1") :wn ("procure%2:34:00" "procure%2:40:00"))
     (LF-PARENT ONT::acquire)
     (TEMPL agent-affected-xp-templ )
     )
   #|| ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("get-13.5.1") :wn ("procure%2:34:00" "procure%2:40:00"))
     (LF-PARENT ONT::acquire)  ;; like buy, order
     (TEMPL AGENT-RECIPIENT-affected-TEMPL) 
     )||#
    
    )
   )
))


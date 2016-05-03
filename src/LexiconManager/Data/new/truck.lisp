;;;;
;;;; W::TRUCK
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::TRUCK
;;   (wordfeats (W::MORPH (:forms (-S-3P -load))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date 20111006 :comments nil :wn ("truck%1:06:00"))
     ;; 20111006 changed from ont::vehicle to ont::truck for obtw demo
     (LF-PARENT ONT::truck)
     )
    )
   )
))

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::truck
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("drive-11.5-1"))
     (LF-PARENT ONT::transport)
 ; like shuttle
     )
    )
   )
))


;;;;
;;;; W::CONDITION
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CONDITION
   (SENSES
    ((meta-data :origin calo :entry-date 20060803 :change-date nil :comments nil :wn ("condition%1:10:02"))
     (LF-PARENT ONT::requirements)
     (example "one condition for the purchase is approval")
     )
    ((meta-data :wn ("condition%1:26:05"))
     (LF-PARENT ONT::medical-condition)
     (example "he has a medical condition.")
     )
    ((meta-data :wn ("condition%1:26:00"))
     (LF-PARENT ONT::status)
     (example "a condition of disrepair")
     )
    ((LF-PARENT ONT::condition)
     (example "what are the weather conditions for tomorrow" "what is the patient's condition")
     (meta-data :origin plow :entry-date 20050425 :change-date nil :wn ("condition%1:26:00") :comments plow-req)
     )
    )
   )
))


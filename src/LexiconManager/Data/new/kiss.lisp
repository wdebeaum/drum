;;;;
;;;; W::kiss
;;;;

(define-words :pos W::v 
 :words (
  (W::kiss
   (wordfeats (W::morph (:forms (-vb) :nom w::kiss)))
   (SENSES
    ((LF-PARENT ONT::kissing)
     (EXAMPLE "John kissed Marsha")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("marry-36.2" "touch-20") :wn ("kiss%2:35:00" "kiss%2:35:01"))
     )
    ((LF-PARENT ONT::kissing)
     (EXAMPLE "John and Marsha kissed")
     (TEMPL AGENT-NP-PLURAL-TEMPL)
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("marry-36.2" "touch-20") :wn ("kiss%2:35:00" "kiss%2:35:01"))
     )
    )
   )
))


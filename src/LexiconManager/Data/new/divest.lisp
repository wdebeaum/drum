;;;;
;;;; W::divest
;;;;

(define-words :pos W::v 
 :words (
  (W::divest
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090601 :comments nil :vn ("cheat-10.6") :wn ("divest%2:40:00" "divest%2:40:01" "divest%2:41:00"))
     (LF-PARENT ONT::remove-from)
     (TEMPL agent-source-affected-optional-templ)
     (example "divest the company of stocks")
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20090605 :change-date 20090605 :comments nil)
     (LF-PARENT ONT::remove-from)
     (TEMPL agent-affected-source-optional-templ)
     (example "divest stocks from the portfolio")
     )
    )
   )
))


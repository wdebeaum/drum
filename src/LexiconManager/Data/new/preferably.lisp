;;;;
;;;; W::preferably
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::preferably
   (SENSES
    ((LF-PARENT ONT::qualification)
     (LF-FORM W::preferable)
     (TEMPL DISC-TEMPL)
     )
    ((LF-PARENT ONT::preferred-val)
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (example "preferably green")
     )
    )
   )
))


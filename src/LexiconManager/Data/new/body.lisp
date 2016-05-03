;;;;
;;;; W::body
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::body
   (SENSES
    ((meta-data :origin medadvisor :entry-date 20060803 :change-date nil :comments nil :wn ("body%1:08:00"))
     (LF-PARENT ONT::body-part)
     (templ body-part-reln-templ)
     )
    ((LF-PARENT ONT::info-medium)
     (EXAMPLE "put the body of the message here")
     (meta-data :origin plot :entry-date 20080225 :change-date nil :comments nil)
     (templ other-reln-templ)
     )
    )
   )
))


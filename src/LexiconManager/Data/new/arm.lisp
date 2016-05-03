;;;;
;;;; W::ARM
;;;;

(define-words :pos W::n
 :words (
;; physical systems, digestive, reproductive,. ...
;; those are adjectives
;; external
  (W::ARM
  (senses((LF-PARENT ONT::external-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))

(define-words :pos W::n
 :words (
  ((W::ARM W::POT W::ROAST)
  (senses
	   ((LF-PARENT ONT::BEEF)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))


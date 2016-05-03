;;;;
;;;; W::rear
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::rear
   (SENSES
    ((LF-PARENT ONT::object-dependent-location)
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060609 :change-date nil :wn ("rear%1:15:00" "rear%1:15:01" "rear%1:06:00") :comments plow-req)
     )
    )
   )
))

(define-words :pos W::n
 :words (
;; physical systems, digestive, reproductive,. ...
;; those are adjectives
;; external
  (w::rear
  (senses((LF-PARENT ONT::external-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))

(define-words :pos W::n
 :words (
;; physical systems, digestive, reproductive,. ...
;; those are adjectives
;; external
  ((w::rear w::end)
  (senses((LF-PARENT ONT::external-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))


;;;;
;;;; W::hand
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::hand w::held)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::hand W::helds))))
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :comments Office)
     (LF-PARENT ONT::device)
     (example "Create a mobile lifestyle with your Palm hand held")
     )
    )
   )
))

(define-words :pos W::n
 :tags (:base500)
 :words (
;; physical systems, digestive, reproductive,. ...
;; those are adjectives
;; external
  (W::HAND
  (senses((LF-PARENT ONT::external-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))

(define-words :pos W::v :templ agent-affected-xp-templ
 :tags (:base500)
 :words (
  (W::hand
   (SENSES
    #||((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date nil :comments nil :vn ("send-11.1-1"))
     (LF-PARENT ONT::giving) ;; was ont::send
     (example "hand him the package")
     (TEMPL agent-affected-recipient-alternation-templ) ; like mail,send,forward,transmit
     )||#
    ((LF-PARENT ONT::giving)
     (example "hand a gift to him")
     (TEMPL agent-affected-recipient-alternation-templ) ; like grant,offer
     )
    )
   )
))


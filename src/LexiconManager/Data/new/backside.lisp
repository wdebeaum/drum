;;;;
;;;; W::BACKSIDE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::BACKSIDE
   (SENSES
    ((LF-PARENT ONT::object-dependent-location) (TEMPL gen-part-of-reln-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("backside%1:15:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::n
 :words (
;; physical systems, digestive, reproductive,. ...
;; those are adjectives
;; external
  (w::backside
  (senses((LF-PARENT ONT::external-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))


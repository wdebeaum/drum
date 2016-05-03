;;;;
;;;; W::MODEL
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::MODEL
   (SENSES
    ((LF-PARENT ONT::product-model) (TEMPL gen-part-of-reln-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("model%1:09:03")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::model
   (SENSES
    ((meta-data :origin integrated-learning :entry-date 20050817  :change-date 20050817 :wn ("model%1:06:00") :comments nil)
     (LF-PARENT ONT::representation)
     (TEMPL OTHER-RELN-TEMPL)
     (example "an illustration of the process")
     )
    ((LF-PARENT ONT::product-model)
     (TEMPL gen-part-of-reln-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("model%1:09:03") :COMMENTS HTML-PURCHASING-CORPUS)
     )
    )
   )
))

(define-words :pos W::V :templ agent-affected-create-templ
 :words (
  (W::model
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("create-26.4") :wn ("model%2:36:00" "model%2:36:02"))
     (LF-PARENT ONT::create)
 ; like produce
     )
    )
   )
))


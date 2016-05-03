;;;;
;;;; W::area
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
 (W::area
   (SENSES
  ;; 20120716 base500 removing competing sense of area
  ;  ((meta-data :origin lou :entry-date 20060803 :change-date nil :comments nil :wn ("area%1:15:01"))
  ;   (LF-PARENT ONT::LOCATION)
  ;   (example "search this area for mines")
  ;   (TEMPL OTHER-RELN-TEMPL)
  ;   )
    ((LF-PARENT ONT::loc-as-area)
     (example "the house is in a nice area")
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
;; jr 20120716 base500 removing non-physical senses of area 
;; :tags (:base500)
 :words (
  (W::AREA
   (SENSES  
    ((LF-PARENT ONT::discipline)
     (example "that's not my area of expertise")
     (meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :wn ("area%1:09:00") :comments nil)
     (preference .98)
     )
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("area%1:07:00"))
     (LF-PARENT ONT::area)
     (example "the area of the room")
     (TEMPL OTHER-RELN-TEMPL)
     )
    ((meta-data :origin sense :entry-date 20091027 :change-date nil :comments nil :wn ("area%1:07:00"))
     (LF-PARENT ONT::area)
     (example "the area of 200 sq meters")
     (TEMPL reln-subcat-of-units-TEMPL)
     )
    )
   )
))


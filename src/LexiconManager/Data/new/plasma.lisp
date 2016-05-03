;;;;
;;;; w::plasma
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (w::plasma
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060213 :change-date nil :comment projector-purchasing :wn ("plasma%1:08:00" "plasma%1:27:01"))
     (LF-PARENT ONT::substance)
     (example "plasma technology consists of pixels that allow electric pulses to excite natural gases to produce light")
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::plasma
  (senses
   ((LF-PARENT ONT::bodily-fluid)
    (TEMPL mass-PRED-TEMPL)
    (meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
     )
   )
)
))

;(define-words :pos W::n
; :words (
;  ((w::plasma w::membrane)
;  (senses
;   ((LF-PARENT ONT::PLASMA-MEMBRANE)
;    (TEMPL count-PRED-TEMPL)
;    (meta-data :origin BOB :entry-date 2041209 :change-date nil :comments nil)
;    )
;   )
;)
;))

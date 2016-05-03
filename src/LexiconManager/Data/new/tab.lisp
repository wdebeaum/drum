;;;;
;;;; W::TAB
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (W::TAB
     (SENSES
      ((meta-data :origin calo :entry-date 20040505 :change-date nil :wn ("tab%1:10:00") :comments calo-y1variants)
       (example "put it on my tab")
       (LF-PARENT ONT::account-payable)
       (templ other-reln-templ)
       )
      )
     )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::TAB
   (SENSES
    ((meta-data :origin plow :entry-date 20050318 :change-date nil :comments nil)
     (example "click on the tab in the corner of the browser")
     (LF-PARENT ONT::icon)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::tab
  (senses
   ((LF-PARENT ont::substance-delivery-unit) 
    (meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("pill%1:06:00"))
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos w::N 
 :words (
;; Various electrical parts
  (w::tab
  (senses((LF-parent ONT::Device-component) 
	    (templ part-of-reln-templ)
	    (meta-data :origin bee :entry-date 20040407 :change-date 20040607 :comments (test-s portability-experiment))
	    ))
  ;; Myrosia added terminal and holder for portability experiment
)
))


;;;;
;;;; w::open
;;;;

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :tags (:base500)
 :words (
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Defined by Myrosia for beetle
;;
	  (w::open
	   (senses
	    ((LF-parent ont::open)
	     (example "open the door")
	     )
	    ((lf-parent ont::open)
	     (templ affected-templ)
	     (example "after the piston moves downward the valve opens")
	     (meta-data :origin mobius :entry-date 20080702 :change-date 20091008 :comments nil)
	     )
	  	    ))
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::OPEN
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin step :entry-date 20080624 :change-date nil :comments nil :wn ("open%3:00:01"))
     (EXAMPLE "it's open") ;; basic use should not generate a purpose impro
     (LF-PARENT ONT::active-open)
     )
    ;; changed adj-purpose-optional to adj-purpose
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("open%3:00:01"))
     (EXAMPLE "that's open for business")
     (LF-PARENT ONT::active-open)
     (TEMPL ADJ-PURPOSE-TEMPL)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("open%3:00:01"))
     (LF-PARENT ONT::active-open)
     (example "the stores open are ...")
     (TEMPL postpositive-ADJ-TEMPL)
     )
    )
   )
))


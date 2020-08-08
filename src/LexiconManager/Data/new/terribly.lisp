;;;;
;;;; W::terribly
;;;;

(define-words :pos w::adv
 :words (
  (w::terribly
  (senses((TEMPL ADJ-OR-ADV-OPERATOR-TEMPL)
     (lf-parent ont::degree-modifier-high)
            (meta-data :origin adjective-reorganization :entry-date 20170413 :change-date nil :comments nil)
            )
           )
)
))

(define-words :pos W::adv :templ PRED-VP-TEMPL 
 :words (
	  (W::terribly
	  (SENSES
	   ;((LF-PARENT ONT::acceptability-val)
	   ; (TEMPL ADJ-OPERATOR-TEMPL)	    
	   ; (example "his ankles are terribly swollen" "he is terribly ill")
	   ; (SEM (f::gradability +) (f::orientation f::neg) (f::intensity ONT::hi))
	   ; (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	   ; )
	   ((LF-PARENT ONT::awful-val)
	    (TEMPL PRED-VP-TEMPL)	    
	    (example "he performed terribly on the exam")
	    (SEM (f::gradability +) (f::orientation f::neg) (f::intensity ONT::hi))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))


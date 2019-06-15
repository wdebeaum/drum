;;;;
;;;; w::multiply
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	  (w::multiply
	   (senses ((lf-parent ont::calc-multiply)
		    (example "multiply x [by 5]" "multiply x and y")
		    (TEMPL AGENT-FORMAL-FORMAL1-XP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype w::by))))
		    (meta-data :origin lam :entry-date 20050420 :change-date 20090522 :comments lam-initial)
		    )
		   ((lf-parent ont::calc-multiply)   ;; this should be handled by general mechansims

		    (example "multiply by 5")
		    (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::pp (w::ptype w::by))))
		    (meta-data :origin lam :entry-date 20050420 :change-date 20090522 :comments lam-initial)
		    )
		   ; james and wdebeaum think this example is bogus
		   ;((lf-parent ont::function-calculation)
		    ;(example "6 multiplies x")
		    ;(templ theme-source-xp-templ (xp (% w::NP)))
		    ;(meta-data :origin lam :entry-date 20050420 :change-date nil :comments lam-initial)
		   ; )
		   ((lf-parent ONT::increase-number)
		    (example "the flies multiplied")
		    (templ affected-templ)
		    (meta-data :origin "wordnet-3.0" :entry-date 20090608 :change-date nil :comments nil)
		    )
		   ))
))


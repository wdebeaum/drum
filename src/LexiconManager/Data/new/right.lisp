;;;;
;;;; W::RIGHT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  ;; ambiguity 
  (W::RIGHT
   (SENSES
    ((meta-data :origin fruitcarts :entry-date 20060803 :change-date nil :comments nil :wn ("right%1:15:00"))
     (LF-PARENT ont::right-loc);ONT::object-dependent-location)
     (example "to the right of the building")
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     ;; enforced subcat to reduce ambiguity, but prevents "on the right" unless we add another grammar rule
;;     (TEMPL other-reln-subcat-required-templ)
     )
   ((LF-PARENT ont::social-contract)
     (example "the right to sing")
     (TEMPL SUBCAT-INF-TEMPL)
    ))
   
  )))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::RIGHT
   (SENSES
    ;; This first sense seems redundant with the second
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("right%3:00:02"))
     (LF-PARENT ONT::correctness-VAL)
     (example "that's right/the right choice")
     (templ central-adj-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("right%5:00:00:appropriate:00"))
     (EXAMPLE "that's right for him ")
     (LF-PARENT ONT::correctness-VAL)
     (TEMPL ADJ-CO-THEME-TEMPL (XP (% W::PP (W::PTYPE W::For))))
     (SYNTAX (W::allow-deleted-comp -))
     )
    )
   )
))

#||(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
	  (w::right
	   (senses
	    ((lf-parent ONT::RIGHT)
	     (templ attributive-only-adj-templ)
	     (meta-data :origin bee :entry-date 20040408 :change-date 20090731 :wn ("right%3:00:00") :comments test-s)
	     )
	    ))	  
))||#

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::RIGHT
   (SENSES
    ((LF-PARENT ONT::DIRECTION)
     (example "turn right")
     (TEMPL PRED-S-POST-TEMPL)
     )
    ((LF-PARENT ONT::degree-modifier-med)
     (example "right there")
     (TEMPL ADV-OPERATOR-TEMPL)
     (preference .96) ;; prefer direction
     (meta-data :origin fruitcarts :entry-date 20050331 :change-date nil :comments fruitcarts-11-1)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::right W::away)
   (SENSES
    ((LF-PARENT ONT::event-time-rel)
     (TEMPL PRED-S-POST-templ)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
    ((W::right W::now)
   (SENSES
    ((LF-PARENT ONT::event-time-rel)
     (TEMPL PRED-S-POST-templ)
     )
    )
   )
))

(define-words :pos W::name
 :words (
  ((W::right W::honorable)
  (senses((LF-PARENT ONT::title)
	    (syntax (w::title +))
	    (templ nname-templ)
	    )
	   )
)
))


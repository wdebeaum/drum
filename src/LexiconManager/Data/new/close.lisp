;;;;
;;;; w::close
;;;;

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :tags (:base500)
 :words (
	  (w::close
	   (senses
	    ((LF-parent ont::close)
	     (example "close the door")
	     (templ agent-affected-xp-templ)
	     )
	    ((lf-parent ont::close)
	     (templ affected-templ)
	     (example "after the piston moves upward the valve closes")
	     (meta-data :origin mobius :entry-date 20080702 :change-date 20091008 :comments engine-text01)
	     )
	    ))
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::CLOSE
   (wordfeats (W::MORPH (:FORMS (-ER))) (W::COMP-OP W::LESS) (W::ALLOW-POST-N1-SUBCAT +))
   (SENSES
    ;; 06/02/2010 reinstating this sense for PTB tests at jfa's request. It had been commented out because it interfered with interpretations for PLOW, e.g. a restaurant close to a hotel, which had the ADV sense similar to near.
    ((EXAMPLE "They are close/ the church is close to the house" "The session losses left municipal dollar bonds close to where they were before the 190.58-point drop ")
;     (LF-PARENT ONT::DISTANCE-VAL)
     (LF-PARENT ONT::near)
    (TEMPL ADJ-CO-THEME-TEMPL (XP (% W::PP (W::Ptype W::to))))
    )
    #|
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("close%3:00:05"))
     (example "it was a close call")
     (LF-PARENT ONT::near)
     (templ adj-theme-templ)
     (SEM (f::orientation ont::less) (f::intensity ont::hi))
     )
    |#
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ((W::CLOSE W::BY)
   (wordfeats (W::MORPH (:FORMS (-ER))) (W::COMP-OP W::LESS))
   (SENSES
    ((meta-data :origin monroe :entry-date 20031217 :change-date nil :comments nil)
     (LF-PARENT ONT::near)
     (example "the houses are close by")
     (TEMPL ADJ-THEME-TEMPL)
     (SEM (f::orientation ont::less) (f::intensity ont::hi))
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::close
   (wordfeats (W::morph (:FORMS (-ER))))
   (SENSES
    ((LF-PARENT ont::near-reln)
     (example "move it close [to that]")
     (TEMPL PRED-S-POST-subcat-optional-TEMPL  (xp (% w::pp (w::ptype w::to))))
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::close w::to)
   (wordfeats (W::morph (:FORMS (-ER))))
   (SENSES
    ((LF-PARENT ONT::time-clock-rel)
     (LF-FORM W::APPROXIMATE)
     (example "it is close to five")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
    ;; removing these senses to favor the adjective senses instead at jfa's request 06/02/2010
;    ((LF-PARENT ont::near-reln ) ;ONT::proximity
;     (example "find a hotel close to a zipcode" "he is close to the party")
;     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
;     )
    ;; handle this as subcat to adv w::close
;     ((LF-PARENT ont::near-reln ) ;ONT::proximity
;     (example "move it close to there")
;     (preference .98)
;     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
;     )
    )
   )
))


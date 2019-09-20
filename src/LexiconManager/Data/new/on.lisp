;;;;
;;;; w::on
;;;; 

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ((W::on w::time)
   (SENSES
    ((meta-data :origin plow :entry-date 20060629 :change-date 20090818 :wn ("on_time%5:00:00:punctual:00") :comments pq)
     (example "flights with on time arrival")
     (LF-PARENT ONT::scheduled-time-modifier)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
    ((W::on w::site)
   (SENSES
    ((meta-data :origin plow :entry-date 20060530 :change-date nil :comments pq0404)
     (example "on site laundry facilities")
     (LF-PARENT ONT::on-site-val)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ((W::on w::hand)
   (SENSES
    ((example "use what you have on hand")
     (meta-data :origin monroe :entry-date 20031217 :change-date nil :wn ("on_hand%5:00:00:available:00") :comments s15)
     (lf-parent ont::available)
     (SEM (F::GRADABILITY F::-))
     (TEMPL postpositive-adj-templ)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ((W::on w::sale)
   (SENSES
    ((example "this model is on sale")
     (meta-data :origin calo :entry-date 20050324 :change-date nil :comments caloy2)
     (LF-PARENT ONT::commerce-val)
     (SEM (F::GRADABILITY F::-))
     (SYNTAX (W::atype W::predicative-only))
     )
    ((example "the computers on sale are the g3 and g4")
     (meta-data :origin calo :entry-date 20050324 :change-date nil :comments caloy2)
     (LF-PARENT ONT::commerce-val)
     (SEM (F::GRADABILITY F::-))
     (TEMPL postpositive-adj-templ)
     )
    )
   )
))

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
	  (w::on
	   (senses
	    (;(lf-parent ont::artifact-property-val)
	     (lf-parent ont::active-on)
	     ;(templ central-adj-templ)
	     (templ predicative-only-adj-templ)
	     (Example "The switch is on -- predicative only, because 'the on switch' is not at all the same;  The off position/state")
	     (meta-data :origin bee :entry-date 20040408 :change-date nil :wn ("on%3:00:00") :comments test-s)
	     )
	    )
	   )))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
	      :words (
		      ((W::ON W::PURPOSE)
		       (SENSES
			((LF-PARENT ONT::intentional-val)
			 (TEMPL PRED-S-POST-templ)
			 )
			)
		       )
		      ))

(define-words :pos W::adv :templ PRED-S-VP-TEMPL
	      :words (
		      ((W::on W::an W::empty W::stomach)
		       (wordfeats (W::morph NIL))
		       (SENSES
			(;(LF-PARENT ONT::assoc-with)
					;(LF-FORM W::with_meal)
					;(TEMPL ppword-adv-templ (xp (% W::s)))
					;(SYNTAX (W::atype W::post))
			 (LF-PARENT ONT::associated-with-food-val)
			 (TEMPL PRED-S-POST-templ)
			 )
			)
		       )
		      ))

(define-words :pos W::adv :templ PRED-S-VP-TEMPL
 :words (
  ((W::on W::a W::full W::stomach)
   (wordfeats (W::morph NIL))
   (SENSES
    (;(LF-PARENT ONT::assoc-with)
     ;(LF-FORM W::without_meal)
     ;(TEMPL ppword-adv-templ (xp (% W::s)))
     ;(SYNTAX (W::atype W::post))
     (LF-PARENT ONT::associated-with-food-val)
     (TEMPL PRED-S-POST-templ)
     )
    )
   )
  ))

(define-words :pos W::ADV
  :tags (:base500)
  :words (
	  (W::ON
	   (SENSES
	    ((LF-PARENT ONT::TIME-on-rel)
	     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
	     (example "he departed on Monday")
	     (preference .985)
	     )
	    #|
	    ((LF-PARENT ONT::time-on-rel)
	    (TEMPL BINARY-CONSTRAINT-adj-postpos-TEMPL)
	    (meta-data :origin cernl :entry-date 20110114 :change-date nil :comments hpi-acn-3)
	    (example "the device placed on that day")
	    (preference .98)
	    )
	    |#
	    ((LF-PARENT ONT::ON)
	     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
	     (example "it is on the corner")
	     )
    ;;;;; on drugs, on antibiotics
	    ((LF-PARENT ONT::on-medication)
	     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
	     (preference .98)
	     )
	    ((LF-PARENT ONT::ASSOC-WITH)
	     (example "get me a quote on the ibm thinkpad")
	     (meta-data :origin calo :entry-date 20041130 :change-date nil :comments calo-y2)
	     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
	     (PREFERENCE 0.975)
	     )
	    )
	   )
))

(define-words :pos W::ADV
 :words (
    ((w::on w::behalf w::of)
     (senses
     ((LF-PARENT ONT::BENEFICIARY)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (example "he is working on behalf of the agency")
     (meta-data :origin calo-ontology :entry-date 20060426 :change-date nil :comments nil)
     )
    )
  )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::ON
   (SENSES
    ((LF (W::ON))
     (non-hierarchy-lf t))
    )
   )
))
#||
(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
	      :words (
  ((w::on W::which)
   (wordfeats (W::sing-lf-only +)  (W::case (? cas W::sub W::obj -)))
   (SENSES
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     (SYNTAX (W::wh W::R))
     (TEMPL PRONOUN-WH-TEMPL)
     )
    
   )
))
)||#

(define-words :pos W::ADV
 :words (
    ((w::on w::the w::other w::hand)
     (senses
     ((LF-PARENT ONT::CONTRASTIVE)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (example "on the other hand, Kim volunteered")
     )
    )
  )
))

(in-package :om)

;;; All these are predicates with arguments
(define-type ONT::predicate
  :parent ONT::abstract-object
  :comment "predications that require a subcat to form a modifier, typically adverbials (e.g., on, as well as"
  :sem (F::ABSTR-OBJ (:default (F::GRADABILITY +) (F::scale -) (f::intensity -) (f::orientation -)  (F::CONTAINER -) (f::intentional -)))
  :arguments (;(:ESSENTIAL ONT::OF)
	      ;(:ESSENTIAL ONT::VAL)
	      (:ESSENTIAL ONT::FIGURE)
	      (:ESSENTIAL ONT::GROUND)
	      
	      )
  )

(define-type ONT::modifier
 :parent ONT::PREDICATE
 :sem (F::ABSTR-OBJ  (:required (F::CONTAINER -) (f::intentional -))
		     (:default (F::GRADABILITY +) (F::scale -) (f::intensity -) (f::orientation -)))
 :arguments ((:OPTIONAL ONT::GROUND)
             )
 )

;;; Modifiers of quantifiers
(define-type ONT::Qmodifier
 :parent ONT::MODIFIER
 )

;;; for more, less, most, least
(define-type ONT::grade-modifier
 :parent ONT::MODIFIER
 :arguments ((:REQUIRED ONT::FIGURE (F::abstr-obj (F::gradability +)))
	     (:ESSENTIAL ONT::GROUND (F::abstr-obj (f::scale ?!sc)))
             )
 )

(define-type ONT::situation-object-modifier
 :parent ONT::PREDICATE
 :sem (F::ABSTR-OBJ (:default (F::GRADABILITY +) (F::scale ont::other-scale) (f::intensity ont::hi)))
 :arguments ((:ESSENTIAL ONT::FIGURE ((? tt F::PHYS-OBJ F::situation)))
             )
 )

(define-type ONT::phys-modifier
 :parent ONT::SITUATION-OBJECT-MODIFIER
 :arguments ((:ESSENTIAL ONT::FIGURE (F::PHYS-OBJ))
             )
 )

;;; Myrosia 01/08/03 this actually has phys-obj in the restriction
;;; because most frequently adjectives that tend to modify situations
;;; will also modify physical objects involved in those situations:
;;; medical emergency / medical instrument
(define-type ONT::situation-modifier
 :parent ONT::SITUATION-OBJECT-MODIFIER
 :arguments ((:ESSENTIAL ONT::FIGURE (F::situation))
             (:OPTIONAL ONT::GROUND)
	     (:optional ont::norole)	     
             )
 )

(define-type ONT::CONJUNCT
 :parent ONT::PREDICATE
 )

(define-type ONT::CLAUSE-CONDITION
 :parent ONT::CONJUNCT
 )

(define-type ONT::BUT-EXCEPTION
 :parent ONT::CONJUNCT
 )

;;; swift 04/14/02 added to support sentence-initial uses of yes & no
(define-type ONT::ASSESS
 :parent ONT::PREDICATE
 )

;; very, really
;; split into five sublevels
(define-type ONT::degree-modifier
 :parent ONT::modifier
  :arguments ((:REQUIRED ONT::FIGURE ((? ss F::situation F::abstr-obj))
 )))

(define-type ONT::DEGREE-MODIFIER-VERYHIGH
 :parent ONT::DEGREE-MODIFIER
 )

(define-type ONT::DEGREE-MODIFIER-HIGH
 :parent ONT::DEGREE-MODIFIER
 )

(define-type ONT::DEGREE-MODIFIER-MED
 :parent ONT::DEGREE-MODIFIER
 )

(define-type ONT::DEGREE-MODIFIER-LOW
 :parent ONT::DEGREE-MODIFIER
 )

(define-type ONT::DEGREE-MODIFIER-VERYLOW
 :parent ONT::DEGREE-MODIFIER
 )

;; so (very good)
(define-type ONT::intensifier
 :parent ONT::modifier
 )

;; out, over
(define-type ONT::location-distance-modifier
    :arguments ((:required ONT::FIGURE (F::SITUATION (F::TYPE ONT::EVENT-OF-ACTION))))
 :parent ONT::PREDICATE
 )


(define-type ONT::NEG
 :parent ONT::PREDICATE
 )

(define-type ONT::PRIORITY
 :parent ONT::PREDICATE
 )

(define-type ONT::QUALIFICATION
 :parent ONT::PREDICATE
 )

(define-type ONT::RESTRICTION
 :parent ONT::PREDICATE
 )

(define-type ONT::TOPIC-SIGNAL
 :parent ONT::PREDICATE
 )

(define-type ONT::TOPIC
 :parent ONT::PREDICATE
 :arguments ((:OPTIONAL ONT::GROUND)
             )
 )

(define-type ONT::DEGREE
 :parent ONT::PREDICATE
 :arguments ((:OPTIONAL ONT::GROUND)
             )
 )

(define-type ONT::DEGREE-OF-BELIEF
 :parent ONT::SITUATION-MODIFIER
 :arguments ((:ESSENTIAL ONT::FIGURE (F::situation))
             )
 )

;;; for you know, you see, etc.
(define-type ONT::INTERJECTION
 :parent ONT::SITUATION-MODIFIER
 :arguments ((:ESSENTIAL ONT::FIGURE (F::situation))
             )
 )

;;;; anything can be a reason -- do we really need to keep it here???
;; because (of), as, since
(define-type ONT::reason
 :parent ONT::SITUATION-MODIFIER
 )

;; cause, 'cause
(define-type ONT::reason-informal
 :parent ONT::reason
 )

;; due to
(define-type ONT::due-to
 :parent ONT::reason
 )

;; therefore, thus
(define-type ONT::therefore
; :parent ONT::reason
 :parent ONT::situation-modifier
 )

(define-type ONT::PURPOSE
 :parent ONT::SITUATION-MODIFIER
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Situation (F::type ont::event-of-action))); maybe takes statives: This suffices for...
;;             (:REQUIRED ONT::VAL (F::Situation (F::aspect F::dynamic)))
	     ;; purposes don't have to be dynamic -- e.g. to store something, to remember, etc.
;	     (:REQUIRED ONT::GROUND ((? xx F::Situation f::abstr-obj f::phys-obj) (F::scale (? !sc ont::duration-scale))))
	     (:REQUIRED ONT::GROUND ((? xx F::Situation f::abstr-obj f::phys-obj) (f::type (? !t ONT::ORGANISM)) (F::scale -)))
	     ;; a separate role because it will be lower priority
;	     (:required ont::obj-val (f::abstr-obj)) ;; needed for non-situation ont::vals -- e.g., hit return for more results
	    ;; (:required ont::REASON (f::abstr-obj)) ;; needed for non-situation ont::vals -- e.g., hit return for more results
             )
 )

; against
(define-type ONT::CONTRA
 :parent ONT::SITUATION-MODIFIER
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Situation))
;;             (:REQUIRED ONT::VAL (F::Situation (F::aspect F::dynamic)))
	     ;; purposes don't have to be dynamic -- e.g. to store something, to remember, etc.
	     (:REQUIRED ONT::GROUND (F::Situation))
             )
 )

;;; swift 11/26/01 -- added the restriction (intentional +) for benefactive lf_val to prevent body parts from
;;; being parsed as beneficiaries
(define-type ONT::BENEFICIARY
 :parent ONT::SITUATION-MODIFIER
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Situation (F::aspect F::dynamic) (F::Cause F::agentive)))
             (:REQUIRED ONT::GROUND (F::Phys-obj (F::origin F::human) (F::intentional +)))
             )
 )

;;; swift 11/27/01 -- 'why is he on synthroid'
(define-type ONT::on-medication
 :parent ONT::PREDICATE
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Phys-obj (F::origin F::living) (F::intentional +)))
             (:REQUIRED ONT::GROUND (F::Phys-obj (F::form F::substance)))
             )
 )

;; I have a disability [with maps], I want a computer [with 500 mb of ram]
;; get the weather report [for tomorrow]; the day for the meeting
(define-type ONT::ASSOC-WITH
 :parent ONT::PREDICATE
 :arguments ((:ESSENTIAL ONT::FIGURE ((? s F::Phys-obj F::Situation f::abstr-obj f::time)))
             (:REQUIRED ONT::GROUND ((? vl F::Phys-obj f::situation f::abstr-obj f::time)))
             )
 )

;; book by an author
(define-type ONT::ORIGINATOR
 :parent ONT::PREDICATE
 :arguments ((:ESSENTIAL ONT::FIGURE ((? s F::Phys-obj f::abstr-obj)
				  (f::intentional -)
				  (f::information (? inf f::information-content))
				 ))
             (:REQUIRED ONT::GROUND (F::Phys-obj (f::intentional +)))
             )
 )

;; I cleared the roads with Charlie
(define-type ONT::ACCOMPANIMENT
 :parent ONT::PREDICATE
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Situation  (f::type ont::event-of-action)
						    (f::aspect f::dynamic)))
             (:REQUIRED ONT::GROUND (F::Phys-obj (F::origin F::living) (F::intentional +)))
             )
 )

(define-type ONT::WITH-INSTRUMENT
 :parent ONT::PREDICATE
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Situation (f::type ont::event-of-action)
						   (f::aspect f::dynamic)))
             (:REQUIRED ONT::GROUND (F::Phys-obj (F::origin F::artifact) (F::intentional -) (F::OBJECT-FUNCTION F::INSTRUMENT)))
             )
 )

;; for 'without' adverbials
(define-type ONT::without
 :parent ONT::PREDICATE
 :arguments ((:ESSENTIAL ONT::FIGURE ((? o F::Phys-obj F::Situation)))
             (:REQUIRED ONT::GROUND ((? v F::Phys-obj f::situation f::abstr-obj)))
             )
 )

;; under/over/above/below 5 pounds/dollars/feet etc.
(define-type ONT::SCALE-RELATION
 :parent ONT::PREDICATE
 :arguments ((:ESSENTIAL ONT::FIGURE) ;(F::abstr-obj (f::type ont::domain)))  ; I am.../The height is...
             (:REQUIRED ONT::GROUND (F::abstr-obj (f::scale (? !t ont::duration-scale)))) ;(f::scale ?!sc) )) ; to exclude time related senses e.g., TIME-CULMINATION-REL and TIME-DEADLINE-REL for "within"
             )
 )

(define-type ONT::less-than-rel
 :parent ONT::scale-relation
 )

(define-type ONT::more-than-rel
 :parent ONT::scale-relation
 )

;; used in "he is nine years old"   --  he has value on scale AGE-SCALE of 9 years
(define-type ONT::has-value-on-scale
 :parent ONT::scale-relation
 )


;; 5 (feet) by 10 (feet)
(define-type ONT::dimension
 :parent ONT::PREDICATE
 :arguments ((:ESSENTIAL ONT::FIGURE ((? s F::Phys-obj F::situation)))
             (:REQUIRED ONT::GROUND (F::abstr-obj (f::scale ?!sc)))
	     (:OPTIONAL ONT::GROUND1 (F::abstr-obj (f::scale ?!sc2)))
             )
 )

(define-type ONT::instrument
 :parent ONT::SITUATION-MODIFIER
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Situation (F::Aspect F::dynamic)))
             (:REQUIRED ONT::GROUND (F::Phys-obj (F::intentional -) (F::origin (? x F::non-living F::Artifact))))
             )
 )

(define-type ONT::choice-option
 :parent ONT::PREDICATE
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Situation))
             (:REQUIRED ONT::GROUND)
             )
 )

(define-type ONT::attributed-information
 :parent ONT::PREDICATE
 :arguments ((:ESSENTIAL ONT::FIGURE (F::situation))
             (:REQUIRED ONT::GROUND ((? s F::Phys-obj F::abstr-obj)))
             )
 )

(define-type ONT::manner
 :parent ONT::SITUATION-MODIFIER
 :arguments ((:ESSENTIAL ONT::FIGURE (F::situation (f::type ont::event-of-action)))
             (:REQUIRED ONT::GROUND ((? at F::abstr-obj F::situation f::phys-obj) (f::intentional -))) ;; don't want times to work here
             )
 )

(define-type ONT::by-means-of
 :parent ONT::SITUATION-MODIFIER
 :arguments ((:ESSENTIAL ONT::FIGURE (F::situation (F::type ont::event-of-change))) ;(f::aspect (? asp f::dynamic f::stage-level))))
             (:REQUIRED ONT::GROUND (F::situation))
             )
 )

;; This is for "by itself" tec. Unlike typical manner, it can apply to all situations, e.g.
;; the battery is in a closed path by itself
;; we are assuming that "by itself" modifies "is" because it's too complicated to figure out sensible rules to make it modify the subject NP
;; See emails with subject "what to do about "by itself"" from December 2008
;;
;; 2015/12: This is not "by itself" anymore.  "by itself" has moved to EXCLUSIVE
(define-type ONT::manner-refl
 :parent ONT::MANNER
 :arguments ((:ESSENTIAL ONT::FIGURE (F::situation))
             (:REQUIRED ONT::GROUND (f::phys-obj))				 
             )
 )

(define-type ONT::manner-undo
 :parent ONT::MANNER
 :arguments ((:ESSENTIAL ONT::FIGURE (F::situation))
             (:REQUIRED ONT::GROUND (f::phys-obj))				 
             )
 )

(define-type ONT::exclusive   ; alone, myself
 :parent ONT::MANNER
 )

(define-type ONT::inclusive  ; co-, together
 :parent ONT::MANNER
 )

;; in that event
(define-type ONT::situated-in
 :parent ONT::SITUATION-OBJECT-MODIFIER
 :arguments ((:ESSENTIAL ONT::FIGURE ((? xxx F::Situation 
					 F::PHYS-OBJ)))
	   	     ;; SITUATED-IN shouldn't be used for scales!
	     (:REQUIRED ONT::GROUND (F::situation (F::type (? t ONT::SITUATION-ROOT))))
             )
 )

;; out of the meeting
(define-type ONT::situated-out
 :parent ONT::SITUATION-MODIFIER
 :arguments ((:ESSENTIAL ONT::FIGURE ((? xxx F::Situation F::PHYS-OBJ)))
             (:REQUIRED ONT::GROUND (F::situation))
             )
 )

;; for if-then sentences -- ont::conditional is currently in use for would and should
(define-type ONT::CONDITION
 :parent ONT::SITUATION-MODIFIER
 )

;; if, whether, as long as
(define-type ONT::POS-CONDITION
 :parent ONT::CONDITION
 )

;; unless
(define-type ONT::NEG-CONDITION
 :parent ONT::CONDITION
 )

;; when, whenever
(define-type ONT::TIME-CONDITION
 :parent ONT::CONDITION
 )

;; for like, in the sense of similar to
(define-type ONT::similarity
 :parent ONT::PREDICATE
 :arguments ((:ESSENTIAL ONT::FIGURE ((? at F::Phys-obj F::Situation)))
             (:REQUIRED ONT::GROUND)
             )
 )

(define-type ONT::such
 :parent ONT::PREDICATE
 :arguments ((:ESSENTIAL ONT::FIGURE)
             (:optional ONT::GROUND)
             )
 )



(define-type ONT::reason-for
 :parent ONT::SITUATION-MODIFIER
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Situation))
             (:REQUIRED ONT::GROUND)
             )
 )

(define-type ONT::sequence-position
 :parent ONT::PREDICATE
 :arguments ((:REQUIRED ONT::FIGURE)
	     (:optional ont::GROUND) ;; list members before/after the specified item
             )
 )

;;; Added by myrosia to handle about
(define-type ONT::association-predicate
 :parent ONT::PREDICATE
 :arguments ((:REQUIRED ONT::FIGURE)
             (:REQUIRED ONT::GROUND)
             )
 )

;; regarding, as to
;; e.g. the incident regarding the man; the meeting regarding the party
(define-type ONT::associated-information
 :parent ONT::ASSOCIATION-PREDICATE
 :arguments ((:REQUIRED ONT::FIGURE) ;(?argtype (F::information F::information-content))
	     (:required ont::GROUND) ;(?argtyp2 (f::information f::information-content)))
             )
 )

;;; swift 04/14/02 added for too, also
(define-type ONT::additive
 :parent ONT::PREDICATE
 :arguments ((:OPTIONAL ONT::GROUND)
             )
 )

(define-type ont::parenthetical
    :parent ONT::PREDICATE
    :arguments ((:REQUIRED ONT::FIGURE )
		(:REQUIRED ONT::GROUND )			   
		)
    )

;;; etcetera, and so on...
(define-type ONT::etcetera
 :parent ONT::PREDICATE
 )

;;; swift 03/20/02 defined to handle 'vertical' and 'horizontal'
(define-type ONT::orientation
 :parent ONT::PREDICATE
 :arguments ((:ESSENTIAL ONT::FIGURE (F::PHYS-OBJ))
             )
 )

(define-type ONT::SpeakerStatus
 :parent ONT::PREDICATE
 :arguments ((:ESSENTIAL ONT::FIGURE (F::SITUATION))
             )
 )

(define-type ONT::Politeness
 :parent ONT::PREDICATE
 :arguments ((:ESSENTIAL ONT::FIGURE (F::SITUATION))
             )
 )

;; breathlessly, sickly, tiredly
(define-type ONT::physical-symptom-manner
 :parent ONT::PREDICATE
 :arguments ((:ESSENTIAL ONT::FIGURE (F::SITUATION))
             )
 )


;; e.g., use it as a lever
(define-type ONT::as-role
 :parent ONT::PREDICATE
 :arguments ((:essential  ONT::GROUND ((? roleval F::PHYS-OBJ F::ABSTR-OBJ)
			  ))
	     ))
 
(define-type ONT::COMPLETELY
  :parent ONT::PREDICATE
  :arguments (;(:ESSENTIAL ONT::OF (F::SITUATION (f::type ont::event-of-change)))
	      (:ESSENTIAL ONT::FIGURE (F::SITUATION (f::type ont::event-of-change) (F::trajectory -)))
	      (:ESSENTIAL ONT::GROUND ((? gd F::SITUATION F::abstr-obj F::phys-obj)))
  ))

; rotate around/about this axis/origin, flip along this axis
; invert the notes around G4 (musica)
(define-type ONT::pivot
  :parent ONT::PREDICATE
  :arguments ((:ESSENTIAL ONT::FIGURE ((? fig F::SITUATION) (f::type (? t ONT::MOTION ONT::ROTATE))))
	      (:ESSENTIAL ONT::GROUND ((? gd F::abstr-obj)))
  ))

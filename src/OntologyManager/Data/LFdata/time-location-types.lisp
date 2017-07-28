(in-package :om)

;;; Spatial predicate here
;;;

; ****************************************************************
; 12 / 2009
; new organization for spatial prepositions / adverbials
;
; ****************************************************************

; high-level type for spatial locations
; a relation between an object (figure) to another object (ground) by a spatial relation, possible abstract
(define-type ont::position-reln
 :parent ont::abstract-object
 :comment "Spatial relations that locate one object (the figure) in terms of another object (the ground)"
 ;; situations can be spatially located, e.g. meetings, riots, parties
 ;; so can abstr-obj: the idea in the document; the name on the envelope; the man at the party
 :arguments (;(:ESSENTIAL ONT::OF ((? of F::Phys-obj F::Situation f::abstr-obj)))
	     ;(:ESSENTIAL ONT::val ((? val F::Phys-obj F::Situation f::abstr-obj)))
	     (:ESSENTIAL ONT::FIGURE ((? fig F::Phys-obj F::Situation f::abstr-obj)))
	     (:ESSENTIAL ONT::GROUND ((? grd F::Phys-obj F::Situation f::abstr-obj)) (f::type (? !t ONT::TIME-MEASURE-SCALE)))
             )
 )

(define-type ont::at-scale-value
    :comment "The main predicate for mapping an object to a value on a scale"
    :parent ont::position-reln)

; *********************************************
;
; ont::position-as-point-reln subtree
;
; *********************************************

; figure is viewed as a point
(define-type ont::position-as-point-reln
 :parent ont::position-reln
 )

; figure is viewed as a point and related to ground by (abstract) containment
(define-type ont::pos-as-containment-reln
  :parent ont::position-as-point-reln
  )

; ground is within a container, group or area
;(define-type ont::within-reln
;  :parent ont::pos-as-containment-reln
;  )

; in, within, inside (of)
(define-type ont::in-loc
  :parent ont::pos-as-containment-reln
  :arguments ((:ESSENTIAL ONT::GROUND ((? val f::phys-obj f::abstr-obj) ; measure (music)
					(f::type (? t ont::phys-object ont::information-function-object))
				       (f::intentional -) (f::container +)
				   )))
  )

(define-type ont::contain-reln
    :comment "a kind of Inverse of IN-LOC, but can't be used as a result location"
    :parent ont::predicate
    :arguments ((:ESSENTIAL ONT::FIGURE ((? val f::phys-obj) (f::intentional -)
					 (f::container +)
				   )))
    )

; figure is outside a container, group or area
(define-type ont::outside
  :parent ont::pos-as-containment-reln
  :arguments ((:ESSENTIAL ONT::GROUND ((? val f::phys-obj) (f::intentional -)
				       (f::container +)  ;; having container + causes problems with things like "pull the plug out of the wall"
				       )))
  )

; out (of), outside (of)
(define-type ont::out-loc
  :parent ont::outside
  )

; figure is abstract value and delimited by some range of values (ground)
; in, within
; examples: the price is within five dollars of the estimate
; ?? the house is within five miles of the starbucks
(define-type ont::delimit-reln
  :parent ont::pos-as-containment-reln
  :arguments ((:ESSENTIAL ONT::FIGURE ((? of f::abstr-obj)))
	      (:ESSENTIAL ONT::GROUND ((? val f::abstr-obj)))
	      )
  )

; figure is related to the ground by a distance scale
(define-type ont::pos-distance
  :parent ont::position-as-point-reln
  )

; figure is small distance on the scale
(define-type ont::proximate-reln
  :parent ont::pos-distance
  )

; close (to), nearby, by, thereabouts, near, around
(define-type ont::near-reln
  :parent ont::proximate-reln
  )

; figure is adjacent to ground
; adjacent (to), next (to), alongside (of), beside
(define-type ont::adjacent
  :wordnet-sense-keys ("adjacent%5:00:00:close:01")
  :parent ont::near-reln
  :arguments ((:essential ONT::FIGURE ) ;((? of1  f::phys-obj f::abstr-obj)))
	      (:ESSENTIAL ONT::GROUND ((? grd F::Phys-obj)))
	     
	      )
  )

; figure is large distance on the scale
(define-type ont::distal-reln
  :parent ont::pos-distance
 )

; far
(define-type ont::far-reln
  :parent ont::distal-reln
  )

; figure has same location as ground
;(define-type ont::same-as-loc
;  :parent ont::position-as-point-reln
;  )

; at
(define-type ont::at-loc
  :parent ont::position-as-point-reln
  )

; figure is related to perspective of speaker
(define-type ont::pos-wrt-speaker-reln
  :parent ont::position-as-point-reln
  )

; figure associated w/ loc of speaker
; here
(define-type ont::here
  :parent ont::pos-wrt-speaker-reln
  )

; figure not associated w/ loc of speaker
; there
(define-type ont::there
  :parent ont::pos-wrt-speaker-reln
  )

; figure related by directon from ground
(define-type ont::pos-directional-reln
  :parent ont::position-as-point-reln
  )

; direction is a verticality reln
(define-type ont::directional-vert
  :parent ont::pos-directional-reln
  )

; figure is below ground (in some way)
; below, under, underneath
(define-type ont::below
  :parent ont::directional-vert
  )

; figure is below (and translated) from ground
; down (from/of)
(define-type ont::down
  :parent ont::below
  )

; figure is above ground (in some way)
; above, over
(define-type ont::above
  :parent ont::directional-vert
  )

; figure is above (and translated) from ground
; up (from/of/to)
(define-type ont::up
  :parent ont::above
  )

; figure is related by a navigational reln to ground
(define-type ont::navigational-reln
    :comment "Figure is related by a cardinal directional reln to ground"
    :parent ont::pos-directional-reln
    )

; north (of), northward
(define-type ont::north-reln
  :parent ont::navigational-reln
  )

; east (of/from), eastward
(define-type ont::east-reln
  :parent ont::navigational-reln
  )

; south (of/from), southward
(define-type ont::south-reln
  :parent ont::navigational-reln
  )

; west (of/from), westward
(define-type ont::west-reln
  :parent ont::navigational-reln
  )

; figure is related by inherent orientation of ground
(define-type ont::oriented-loc-reln
  :parent ont::position-as-point-reln
  )

;;
(define-type ont::orients-to
  :parent ont::oriented-loc-reln
  :arguments ((:essential ONT::GROUND ((? of1  f::phys-obj f::abstr-obj) (f::type (? t ONT::CARDINAL-POINT ONT::LOC-WRT-ORIENTATION)))))
  )

; figure is on an object
(define-type ont::on
  :parent ont::oriented-loc-reln
  )

; figure is not on an object
(define-type ont::off
  :parent ont::oriented-loc-reln
  )

; figure is in front of the ground
; in front (of), ahead (of)
(define-type ont::front
  :parent ont::oriented-loc-reln
  )

; figure is behind the ground
; in back (of), behind
(define-type ont::back
  :parent ont::oriented-loc-reln
  )

; figure is right of the ground
; right of
(define-type ont::right-of
  :parent ont::oriented-loc-reln
  )

; figure is left of the ground
; left of
(define-type ont::left-of
  :parent ont::oriented-loc-reln
  )

; figure is spatially related to a group of objects (the ground)
(define-type ont::complex-ground-reln
  :parent ont::position-as-point-reln
  )

; figure is between objects comprising the ground
; between, in between
(define-type ont::between
  :parent ont::complex-ground-reln
  )

; figure is within objects comprising the ground
; among
(define-type ont::among
  :parent ont::complex-ground-reln
  )

; *********************************************
;
; ont::position-as-extent-reln subtree
;
; *********************************************

; figure is a linear object
(define-type ont::position-as-extent-reln
 :parent ont::position-reln
; :arguments ((:ESSENTIAL ONT::of ((? of f::phys-obj (F::spatial-abstraction (? sa F::line F::strip)))))
;             )
 )

; figure is distributed over ground
; over
(define-type ont::pos-as-over
 :parent ont::position-as-extent-reln
 )

; figure is distributed throughout ground
; throughout, through
(define-type ont::distributed-pos
 :parent ont::pos-as-over
 )


; *********************************************
;
; > ont::position-w-trajectory-reln subtree
;
; *********************************************

; relation involves a trajectory (figure, ground or other)
; ?? how do these relate to the ont::path subtree?
(define-type ont::position-w-trajectory-reln
 :parent ont::position-reln
 :arguments (;(:ESSENTIAL ONT::FIGURE ((? t F::Phys-obj F::Situation) (F::trajectory +)))
	     (:ESSENTIAL ONT::FIGURE ((? t F::Phys-obj F::Situation)))
             (:ESSENTIAL ONT::GROUND (F::Phys-obj))
             )
 )

; figure is linear and crosses ground
; across
(define-type ont::pos-as-opposite
 :parent ont::position-w-trajectory-reln
 )

(define-type ont::pos-as-around
 :parent ont::position-w-trajectory-reln
 )

; figure is linear and adjacent to ground
; along, alongside (of)
(define-type ont::linear-extent
 :parent ont::position-w-trajectory-reln
 )

; ground is in the trajectory
(define-type ont::pos-relative-wrt-trajectory
 :parent ont::position-w-trajectory-reln
 )

; before, future
(define-type ont::pos-before-in-trajectory
 :parent ont::pos-relative-wrt-trajectory
 )

; after, past
(define-type ont::pos-after-in-trajectory
 :parent ont::pos-relative-wrt-trajectory
 )

#|
; ground is the trajectory
(define-type ont::pos-located-in-trajectory
 :parent ont::position-w-trajectory-reln
 )

; ?? these are nouns -- belong elsewhere??
; start, beginning
(define-type ont::pos-start-of-trajectory
 :parent ont::pos-located-in-trajectory
 )

; end, finish
(define-type ont::pos-end-of-trajectory
 :parent ont::pos-located-in-trajectory
 )

; mid, midway
(define-type ont::pos-midway
 :parent ont::pos-located-in-trajectory
 )

|#

 ; <

; *********************************************
;
; > ont::conventional-position-reln subtree
;
; *********************************************

; figure is defined in terms of relation to a built environment
(define-type ont::conventional-position-reln
 :parent ont::position-reln
 )

; figure related to floors in a building
(define-type ont::floor-rel
 :parent ont::conventional-position-reln
 )

; upstairs
(define-type ont::floor-above
 :parent ont::floor-rel
 )

; downstairs
(define-type ont::floor-below
 :parent ont::floor-rel
 )

; figure relates to proximity to city center
(define-type ont::city-rel
 :parent ont::conventional-position-reln
 )

; uptown
(define-type ont::uptown
 :parent ont::city-rel
 )

; downtown
(define-type ont::downtown
 :parent ont::city-rel
 )

#||
(define-type ont::dwelling
 :parent ont::conventional-position-reln
 )||#

; <

; *********************************************
;
; ont::trajectory subtree
;
; *********************************************
; high-level type for adverbials describing movement along trajectories
(define-type ont::path ; for now to avoid conflict w/ old ont::trajectory
 :parent ont::predicate
 :sem (F::abstr-obj)
 ;; situations can be spatially located, e.g. meetings, riots, parties
 ;; so can abstr-obj: the idea in the document; the name on the envelope; the man at the party
 :arguments ((:ESSENTIAL ONT::FIGURE ((? of F::Phys-obj F::Situation f::abstr-obj)))
	     (:ESSENTIAL ONT::GROUND ((? val F::Phys-obj F::Situation f::abstr-obj)))
             )
 )

; relates a trajectory/event to its end/goal
(define-type ont::goal-reln
 :parent ont::path
 :arguments ((:ESSENTIAL ONT::FIGURE)); ((? of F::Situation) (F::trajectory +))))  ; FIGURE should point to the argument, not the verb
 )

;;  This should modify the event - as the object arises from the event (e.g., It melted in a soft pile)

(define-type ONT::resulting-object
 :parent ONT::predicate
 :arguments ((:ESSENTIAL ONT::FIGURE
			 ;(F::Situation (f::aspect f::dynamic) (f::type (? t ont::event-of-creation ont::change)))) ; e.g., make, cut
	                 (F::Phys-obj ))
             (:REQUIRED ONT::GROUND (F::Phys-obj ))
             )
 )

(define-type ONT::original-material
 :parent ONT::predicate
 :arguments ((:ESSENTIAL ONT::FIGURE
			 (F::Situation (f::aspect f::dynamic) (f::type ont::event-of-creation)))
             (:REQUIRED ONT::GROUND (F::Phys-obj (f::type ont::material)))
             )
 )

(define-type ONT::resulting-state
 :parent ONT::goal-reln
 :arguments ((:ESSENTIAL ONT::FIGURE )
			 ;(F::Situation (f::aspect f::dynamic) (f::type ont::change)))
             (:REQUIRED ONT::GROUND ((? t F::Abstr-obj F::situation)))
             )
 )

; trajectory ends at the ground
; to
(define-type ont::to
 :parent ont::goal-reln
 )


; trajectory ends on the ground
; onto
(define-type ont::goal-as-on
 :parent ont::goal-reln
 )

; trajectory ends in the ground
; into
(define-type ont::goal-as-containment
    :parent ONT::goal-reln
    :arguments ((:ESSENTIAL ONT::FIGURE); (F::Situation (f::aspect f::dynamic) (f::type ont::motion)))
		(:REQUIRED ONT::GROUND (F::Phys-obj  (F::container +)
			   )))
    )

; relates a trajectory/evet to its beginning/source
(define-type ont::source-reln
 :parent ont::path
 )

; trajectory starts at the ground
; from

(define-type ont::from
  :parent ont::source-reln
  )

(define-type ont::source-as-loc
 :parent ont::from
 :arguments ((:ESSENTIAL ONT::FIGURE (F::phys-obj)))
 )

#| ; now this is :RESULT
; trajectory starts on the ground
; off
(define-type ont::source-as-on
 :parent ont::from
 )
|#

#| ; now this is :RESULT
; trajectory starts in the ground
; out of
(define-type ont::source-as-containment
 :parent ont::from
 )
|#

; constrains direction of motion
(define-type ont::direction-reln
 :parent ont::path
 :arguments (;(:ESSENTIAL ONT::val ((? val F::Phys-obj)))
	     (:ESSENTIAL ONT::GROUND ((? grd F::Phys-obj)))
	     )
 )

; constrains direction of rotation
(define-type ont::dir-rotation
 :parent ont::direction-reln
 )

; clockwise
(define-type ont::clockwise
 :parent ont::dir-rotation
 )

; counterclockwise
(define-type ont::counterclockwise
 :parent ont::dir-rotation
 )

(define-type ont::dir-in-terms-of-obj
 :parent ont::direction-reln
 :arguments (;(:ESSENTIAL ONT::OF (F::situation (F::type ont::motion)))
	     (:ESSENTIAL ONT::FIGURE (F::phys-obj (F::mobility f::movable)))
	     )
 )

; towards
(define-type ont::towards
 :parent ont::dir-in-terms-of-obj
 )

; away
(define-type ont::away
 :parent ont::dir-in-terms-of-obj
 )

; figure is trajectory, ground is within the trajectory
; via, by way of
(define-type ont::obj-in-path
    :arguments (;(:ESSENTIAL ONT::FIGURE ((? type F::Situation F::phys-obj) (F::type (? path-type ont::motion ont::apply-force ont::route)) (F::trajectory +)))
		;(:ESSENTIAL ONT::FIGURE ((? type F::phys-obj) ))
		(:ESSENTIAL ONT::FIGURE ((? type F::Situation F::phys-obj) (F::type (? t F::route F::event-of-change)) )) ; I go to the post office via ...; The route to Avon via...
		(:essential ONT::GROUND  (F::Phys-obj (F::form F::object)))) 
    :parent ont::path
    )


;; ***************************************************************

#|
;; the idea in the document?
(define-type ONT::spatial-loc
 :parent ONT::PREDICATE
 ;; situations can be spatially located, e.g. meetings, riots, parties
 :arguments ((:ESSENTIAL ONT::FIGURE ((? t F::Phys-obj F::Situation)))
	     ;; should really allow a f::situation as ont::val but limited by system here since we specify features
             (:ESSENTIAL ONT::GROUND (F::Phys-obj (F::form (? ff F::object f::solid f::liquid f::gas))))
	     ;; this is for "the voltage at terminals"
	     ;; BEETLE
;	     (:ESSENTIAL ONT::OF-STATE (F::Abstr-obj))
             )
 )

(define-type ONT::spatial-line-loc
 :parent ONT::SPATIAL-LOC
 :arguments ((:ESSENTIAL ONT::GROUND (F::Phys-obj (F::form F::object) (F::spatial-abstraction (? spa F::line F::strip)))
             )
             )
 )
|#

;; among, next to, adjacent to, nearby
;; the house/party around the corner, he walked around the party/the house
;; perhaps the situation cases should be handled with coercion, but for now...
(define-type ONT::approximate-at-loc
 :parent ONT::predicate ;; can't make this child of ont::spatial-loc since ont::val can be f::situation
 :arguments ((:ESSENTIAL ONT::FIGURE ((? lof F::Phys-obj F::Situation)))
             (:ESSENTIAL ONT::GROUND ((? lv f::phys-obj f::situation)))
             )
 )

;; close (to)/near a house, a party, a zipcode

(define-type ONT::trajectory
; :parent ONT::PREDICATE
 :parent ONT::PATH
 :arguments (;(:ESSENTIAL ONT::FIGURE ((? t F::Phys-obj F::Situation) (F::trajectory +)))
	     (:ESSENTIAL ONT::FIGURE ((? t F::Phys-obj F::Situation)))
             (:ESSENTIAL ONT::GROUND (F::Phys-obj))
             )
 )

(define-type ONT::from-loc
 :parent ONT::source-reln
 :arguments ((:ESSENTIAL ONT::FIGURE ( F::situation (F::type ont::motion)))
	     (:ESSENTIAL ONT::GROUND (F::Phys-obj (f::spatial-abstraction (? sa f::spatial-point))))
	     )
 )

(define-type ONT::to-loc
    :comment "the generic goal role: might be a physical object (as possessor) or a resulting state"
 :parent ONT::goal-reln
 :arguments (;(:ESSENTIAL ONT::OF (F::situation (f::type ont::event-of-change)))
	     ;(:ESSENTIAL ONT::VAL ((? t F::Phys-obj F::abstr-obj)))
	     (:ESSENTIAL ONT::FIGURE (F::PHYS-OBJ)); (F::situation (f::type ont::event-of-change)))   ; "I walked to the store" FIGURE should point to "I", not "walked"
	     (:ESSENTIAL ONT::GROUND ((? t F::Phys-obj F::abstr-obj) (f::spatial-abstraction ?!sa) (F::form F::geographical-object)) )  ; spatial-abstraction is not enough: many things have spatial-abstraction, e.g., a frog.  Another possibility is (F::object-function F::spatial-object)
	     )
 )

;; 4 2010 -- no longer needed?
;; pan camera to 45 degrees
;(define-type ONT::to-loc-degrees
; :parent ONT::predicate
; :arguments ((:ESSENTIAL ONT::OF (F::Situation (F::trajectory +)))
;             (:ESSENTIAL ONT::VAL (F::Abstr-obj (f::measure-function f::value)))
;             )
; )

;; for to-phrases that modify vehicles, e.g. the plane to rochester
(define-type ONT::destination-loc
 :parent ONT::predicate
 :arguments ((:ESSENTIAL ONT::GROUND (F::Phys-obj (f::spatial-abstraction (? sa f::spatial-point))))
	     (:ESSENTIAL ont::FIGURE  (f::phys-obj (F::Object-Function F::vehicle) (F::MOBILITY F::Self-moving) (F::container +)))
	     )
 )

#|
;; for from phrases that modify nouns, like "the girl from california" "the plane from rochester"
(define-type ONT::source-loc
 :parent ONT::predicate
 :arguments ((:ESSENTIAL ONT::GROUND (F::Phys-obj (f::spatial-abstraction (? sa f::spatial-point))))
	     (:ESSENTIAL ont::FIGURE  (f::phys-obj ))
	     )
 )
|#

(define-type ONT::through
 :parent ONT::TRAJECTORY
 :arguments ((:ESSENTIAL ONT::GROUND (F::Phys-obj (F::spatial-abstraction (? sa F::spatial-point F::spatial-region))))
             )
 )

; trans-
(define-type ont::across
  :parent ont::trajectory
  )

(define-type ont::over
  :parent ont::trajectory
  )

(define-type ont::around
  :parent ont::trajectory
  )

(define-type ONT::direction
 :parent ONT::position-reln
 :arguments (;(:ESSENTIAL ONT::FIGURE ((? t F::Phys-obj F::Situation) (F::trajectory +) 
		;		      (f::type (? tt ONT::MOTION ONt::APPLY-FORCE ONT::PUT))))
	     (:ESSENTIAL ONT::FIGURE ((? t F::Phys-obj
					 F::abstr-obj ; "move the plan up the agenda"?
					 F::situation))) ; meeting is SITUATION (move the meeting up the stairs)
	     (:ESSENTIAL ONT::GROUND (F::Phys-obj))
            )
 )

(define-type ont::direction-down
    :parent ONT::DIRECTION)

(define-type ont::direction-down-ground
    :comment "this is the transitive 'down' that has a GROUND that describes a physical object and locations objects or events"
    :arguments ( (:ESSENTIAL ONT::FIGURE ((? t F::Phys-obj 
					 F::situation)))
		 (:ESSENTIAL ONT::GROUND (F::Phys-obj)))
    :parent ONT::DIRECTION)

(define-type ont::direction-up
    :comment "This is the intransitive 'up' and is relative to some scale/domain: e.g., the temperature is up, move the ball up"
    :parent ONT::DIRECTION)

(define-type ont::direction-up-ground
    :comment "this is the transitive 'up' that has a GROUND that describes a physical object and locations objects or events"
    :arguments ( (:ESSENTIAL ONT::FIGURE ((? t F::Phys-obj 
					 F::situation)))
		 (:ESSENTIAL ONT::GROUND (F::Phys-obj)))
    :parent ONT::DIRECTION)

;; north, south, east, west
(define-type ont::cardinal-direction
  :parent ont::direction
  )

;;; swift 04/14/02 added to handle adverbs further/father
(define-type ONT::extension
 :parent ONT::PREDICATE
 :arguments ((:ESSENTIAL ONT::FIGURE ((? t F::Phys-obj F::Situation)))
             )
 )

;; along/up/down the street
;; along the river/coast: not F::line/strip
(define-type ONT::ALONG
 :parent ONT::TRAJECTORY
; :arguments ((:ESSENTIAL ONT::GROUND (F::Phys-obj (F::spatial-abstraction (? sa F::line F::strip))))
;             )
 )

(define-type ONT::extent-predicate
 :parent ONT::PREDICATE
 :sem  (F::abstr-obj)
 :arguments (;(:ESSENTIAL ONT::FIGURE (F::Situation))
	     (:ESSENTIAL ONT::FIGURE (F::Situation (f::type ont::change-magnitude)))
;	     (:ESSENTIAL ONT::GROUND (F::abstr-obj (F::scale ont::length)))
	     (:ESSENTIAL ONT::GROUND (F::abstr-obj))  ; no scale (e.g., increase by three dogs)
             )
 )

;;; he ran for five miles
(define-type ONT::spatial-distance-rel
 :parent ONT::extent-predicate
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Situation (f::trajectory +) (F::aspect (? asp F::unbounded F::stage-level)) (F::time-span F::extended)))
	      (:ESSENTIAL ONT::GROUND (F::abstr-obj (F::scale ont::length)))
             )
 )


;;;
;;; > Time predicates here
;;;
;;; The general parent class for all time relationships

(define-type ONT::temporal-predicate
 :parent ONT::PREDICATE   ;; if we change this to extent-predicate, we need to generalize the restrition
 :sem  (F::abstr-obj (F::Scale Ont::time-measure-scale))
 )

;;; A class for core temporal properties of events - aspect, tense, ...
(define-type ONT::time-rel
 :parent ONT::TEMPORAL-PREDICATE
 :arguments ((:REQUIRED ONT::FIGURE (F::situation))
             )
 )

;;; phase (of the moon, of the project)
(define-type ONT::time-span
 :parent ONT::TEMPORAL-PREDICATE
 :arguments ((:REQUIRED ONT::FIGURE)
	     )
 )

;;; A class for temporal modifiers introduced by adjectives or adverbials
(define-type ONT::temporal-modifier
    :parent ONT::TEMPORAL-PREDICATE
    :arguments ((:ESSENTIAL ONT::FIGURE ((? of F::Phys-obj f::situation f::abstr-obj f::time)))
		(:essential ont::scale)
		(:essential ont::standard)
		)
    )

;; the delayed cargo, a scheduled meeting
(define-type ONT::scheduled-time-modifier
 :parent ONT::TEMPORAL-MODIFIER
 :arguments ((:ESSENTIAL ONT::FIGURE ((? of f::phys-obj f::situation)))
             )
 )

;; temporal locations of events, things
(define-type ONT::temporal-location
 :parent ONT::TEMPORAL-MODIFIER
 :arguments ((:ESSENTIAL ONT::FIGURE ((? of F::abstr-obj f::situation f::time)))
             )
 )

;; durations
(define-type ONT::event-duration-modifier
 :parent ONT::TEMPORAL-MODIFIER
 :arguments ((:ESSENTIAL ONT::FIGURE ((? of f::situation f::time)))
;             (:essential ont::GROUND (f::abstr-obj (F::Scale Ont::duration-scale) (F::type ont::time-unit)))
;             (:essential ont::GROUND ((? gd F::abstr-obj F::time) (F::time-scale f::interval)))
	    
             ;(:essential ont::GROUND (F::abstr-obj (F::Scale Ont::duration-scale) (F::type ont::time-unit)))
             (:essential ont::GROUND ((? gd F::abstr-obj F::time) (F::Scale Ont::duration-scale) (F::type ont::time-unit ont::time-interval)))
  ))

#|
(define-type ONT::LONG
 :parent ONT::event-duration-modifier
 :wordnet-sense-keys ("permanent%3:00:00" "lasting%5:00:00:long:02")
 :arguments ((:ESSENTIAL ONT::FIGURE ((? of f::situation f::time)))
             (:essential ont::GROUND (f::abstr-obj (F::Scale F::duration-scale)))
 ))

(define-type ONT::SHORT
 :parent ONT::event-duration-modifier
 :wordnet-sense-keys ("transient%5:00:00:impermanent:00" "impermanent%3:00:00" "impermanent%5:00:00:finite:00")
 :arguments ((:ESSENTIAL ONT::FIGURE ((? of f::situation f::time)))
             (:essential ont::GROUND (f::abstr-obj (F::Scale F::duration-scale)))
 ))
|#

;; how long did it take / did he run
(define-type ont::duration
  :parent ont::temporal-predicate
  :sem (F::abstr-obj (F::Scale Ont::duration-scale))
  :arguments ((:ESSENTIAL ONT::FIGURE ((? t f::situation F::abstr-obj)))
             )
  )

;; frequencies
(define-type ONT::frequency
 :parent ONT::temporal-modifier
 :arguments ((:ESSENTIAL ONT::GROUND (F::abstr-obj (F::scale ont::duration-scale)))
             )
 )

;;; this is a fixed frequency - e .g. do it 3 times
(define-type ONT::repetition
 :parent ONT::FREQUENCY
 )

(define-type ONT::always
 :parent ONT::FREQUENCY
 )

(define-type ONT::often
 :parent ONT::FREQUENCY
 )

(define-type ONT::sometimes
 :parent ONT::FREQUENCY
 )

(define-type ONT::seldom
 :parent ONT::FREQUENCY
 )

(define-type ONT::never
 :parent ONT::FREQUENCY
 )

;;; for things like per day, a day - must apply to bounded events (no quite stong enough, but as close as we can get with the features we have)
(define-type ONT::iteration-period
 :parent ONT::temporal-location
 :arguments ((:ESSENTIAL ONT::FIGURE (F::situation (F::aspect F::bounded))))
 )

;; the population in the 1920s; the shortage in the 1920s
;;; in June, in 1990, in two days
;; This should only be used for locating events in a time span, not for durations
;; so I uncommented the first defn of ONT::VAL
(define-type ONT::time-span-rel
 :parent ONT::temporal-location
 :arguments ((:ESSENTIAL ONT::GROUND (F::time (F::time-scale f::interval)
			 (F::time-function (? funcn F::month-name F::year-name F::day-period))))

;	     (:ESSENTIAL ONT::SIT-VAL (F::situation))
	     ;;(:ESSENTIAL ONT::VAL (F::time ((? vl F::abstr-obj f::time) (F::scale ont::duration-scale))) ;; five minutes/days/hours
	     ;; now used shared (f::scale ont::duration feature on the ont::val instead of ont::time-val
;	     (:optional ont::time-val (f::time)) ;; in June, in 1990
             )
 )

;; the meeting next week; he arrives next week
(define-type ONT::event-time-rel
 :parent ONT::temporal-location
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Situation (F::aspect (? asp F::dynamic F::stage-level))))
             (:ESSENTIAL ONT::GROUND ((? vl F::time f::situation)))
	     ; 3/2011 conflating time and situation in the val role to reduce search space
;             (:ESSENTIAL ONT::SIT-VAL (F::situation)) ;; swift 04/14/02 added this to handle when/before/as soon as/etc. + S, e.g. when I go
             )
 )

;; still, yet, so far, ....
(define-type ONT::time-rel-so-far
; :parent ONT::event-time-rel
  :parent ONT::EVENT-DURATION-MODIFIER
  :arguments ((:ESSENTIAL ONT::FIGURE (F::Situation))
             )
 )

(define-type ONT::event-time-rel-now
 :parent ONT::event-time-rel
 )

(define-type ONT::implicit-overlap
    :comment "this is the implicit relation between the events in sentences like He walked down the street whistling a tune"
 :parent ONT::event-time-rel
 )

(define-type ONT::event-time-rel-culmination
 :parent ONT::event-time-rel
 )

;; event times not including situations
(define-type ONT::event-time
 :parent ONT::event-time-rel
 :arguments (;(:ESSENTIAL ONT::VAL ((? vl F::time)))
	     (:ESSENTIAL ONT::GROUND ((? grd F::time)))
	     )
 )

;; event temporal not including times (e.g., when, upon)
(define-type ONT::event-event-time
 :parent ONT::event-time-rel
 :arguments ((:ESSENTIAL ONT::GROUND ((? vl F::situation)))
	     ))

; unused 3/2011
;;; some things apply only if there's a point of time specified which itself is a day or time
;(define-type ONT::time-day-rel
; :parent ONT::EVENT-TIME-REL
; :arguments ((:ESSENTIAL ONT::VAL (F::time (F::time-function (? funcn F::day-of-week F::time-of-day))))
;             )
; )

;;; some things apply only to day names
;; on Monday, on the next day
(define-type ONT::time-weekday-rel
 :parent ONT::temporal-location
 :arguments ((:ESSENTIAL ONT::GROUND (F::time (F::time-function F::day-of-week)))
             )
 )

;;; some things apply only to time specifications like 5am/noon
(define-type ONT::time-clock-rel
 :parent ONT::temporal-location
 :arguments ((:ESSENTIAL ONT::GROUND (F::time (F::time-function (? fn F::clock-time F::day-point)) (f::time-scale f::point)))
;	     (:ESSENTIAL ONT::SIT-VAL (F::situation))
             )
 )

;;; these modify time objects
;; short term parking; a long day
(define-type ONT::interval-duration-modifier
 :parent ONT::event-duration-modifier
 :arguments ((:essential ONT::FIGURE (F::time (f::time-function f::time-frame) (f::time-scale f::interval)))
             )
 )

;;; for five minutes
(define-type ONT::time-duration-rel
 :parent ONT::event-duration-modifier
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Situation (F::aspect (? asp F::unbounded F::stage-level)) (F::time-span F::extended)))
	      (:ESSENTIAL ONT::GROUND (F::abstr-obj (F::scale ont::duration-scale)))
             )
 )

;;; in five minutes -- a measure of how long it takes to complete s.t.: 'he runs 5 miles in 5 minutes'
(define-type ONT::time-culmination-rel
 :parent ONT::event-duration-modifier
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Situation (F::Aspect F::bounded) (F::Time-span F::extended)))
;             (:ESSENTIAL ONT::VAL (F::time (F::time-function f::time-unit)))
	     (:ESSENTIAL ONT::GROUND (F::abstr-obj (F::scale ont::duration-scale)))
;	     (:optional ont::result-val (f::abstr-obj)) ; until recently / ready
;	     (:optional ont::time-val  (f::abstr-obj (f::scale ont::time-measure-scale)))
             )
 )

;;; in five minutes as a deadline rather than a measure of how long something takes to achieve
;;; so, 'he will run in 2 hours'; he will arrive in 2 hours
(define-type ONT::time-deadline-rel
 :parent ONT::event-duration-modifier
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Situation))
	     (:ESSENTIAL ONT::GROUND (F::abstr-obj (F::scale ont::duration-scale)))
             )
 )


(define-type ONT::while
 :parent ONT::SITUATION-MODIFIER
 )

;;; this is used only for "that/it". Most normal time object denote intervals or units
(define-type ONT::Any-Time-object
 :parent ONT::ANY-SEM
 :sem (F::time (F::time-function F::Any-time-function) (F::time-scale (? sc F::interval F::point)))
 )

(define-type ONT::Time-object
 :parent ONT::ANY-TIME-OBJECT
 :sem (F::time (F::time-scale (? sc F::point F::interval)))
 )

;; these are intervals such as "duration", which cannot generally be counted
;; or serve as time units
(define-type ONT::TIme-interval
 :wordnet-sense-keys ("interval%1:28:00" "time_interval%1:28:00" "time%1:28:03" "clock_time%1:28:00" "time%1:28:00" "time%1:28:05" "time_period%1:28:00")
 :parent ONT::TIME-OBJECT
 :sem (F::time (F::time-scale (? sc F::interval)) (F::Scale Ont::duration-scale))
 :arguments ((:OPTIONAL ONT::GROUND (F::time (f::time-function f::time-frame) (f::time-scale f::interval) (f::scale ont::duration-scale)))
             ;;; a time of two hours
             (:OPTIONAL ONT::FIGURE ((? t f::situation f::abstr-obj)))
	     (:OPTIONAL ONT::EXTENT (F::Abstr-obj (f::time-scale f::interval) (f::scale ont::duration-scale)))
             )
 )

;;  this type is constructed by the grammar for dates, times of day, etc.
(define-type ONT::TIME-LOC
 :parent ONT::TIME-interval)

(define-type ONT::season
 :parent ONT::time-interval
 )

(define-type ONT::winter
 :parent ONT::season
 )

(define-type ONT::spring
 :parent ONT::season
 )

(define-type ONT::summer
 :parent ONT::season
 )

(define-type ONT::autumn
 :parent ONT::season
 )


;; ont::time-unit has been moved under ont::measure-unit (with other units pounds, ghz, etc.)
;;; Covers all explicit things with date and time counting
;; Hours, minutes etc.
;(define-type ONT::time-unit
;    :parent ONT::TIME-OBJECT
;    :sem (f::time (f::time-function f::time-unit))
;    :arguments ((:OPTIONAL ONT::OF ((? t f::situation f::abstr-obj)))
;             )
; )

;; deadline (for doing something)
;; end/beginning/middle (of the year)
(define-type ONT::Time-point
 :wordnet-sense-keys ("time%1:28:06" "clip%1:11:00" "time%1:11:00" "point%1:28:00" "point_in_time%1:28:00")
 :parent ONT::TIME-OBJECT
 :sem (F::time (f::time-scale f::point))
 :arguments ((:OPTIONAL ONT::FIGURE ((? lof f::situation F::time)))
	     ;; this is because we may have things "the end of an event". In reality, there should be a coercion rule, but we are not doing it yet
	     ;; middle of the meeting
;	     (:OPTIONAL ONT::ACTION (F::situation (f::aspect f::unbounded)))
	     ;; middle of the year
;	     (:OPTIONAL ONT::INTERVAL (F::time))
             )
 )

(define-type ONT::date-object
 :wordnet-sense-keys ("date%1:28:03" "time%1:03:00")
 :parent ONT::TIME-Object
 :sem (F::time (F::time-function F::time-of-year))
 )

(define-type ONT::day-name
 :wordnet-sense-keys ("day_of_the_week%1:28:00")
 :parent ONT::DATE-OBJECT
 :sem (F::time (F::time-function F::day-of-week))
   :arguments ((:OPTIONAL ONT::FIGURE ((? t f::situation f::abstr-obj)))
	       (:optional ont::GROUND)
             )
 )

(define-type ONT::month-name
 :wordnet-sense-keys ("calendar_month%1:28:00" "month%1:28:01")
 :parent ONT::DATE-OBJECT
 :sem (F::time (F::time-function F::month-name))
 )

(define-type ONT::era
 :parent ONT::DATE-OBJECT
 :sem (F::time (f::time-function f::era))
 )

;; move forward at seven meters per second; find a hotel at gsa rates
(define-type ont::rate-rel
  :parent ont::predicate
  :arguments (;(:optional ont::of ((? t f::phys-obj f::situation)))
	      ;(:essential ont::val (f::abstr-obj (f::measure-function f::value) (f::scale (? sc f::rate-scale ont::money-scale))))
	      (:optional ont::figure ((? t2 f::phys-obj f::situation)))
	      (:essential ont::ground (f::abstr-obj (f::measure-function f::value) (f::scale (? sc2 ont::rate-scale ont::money-scale))))
	      )
  )

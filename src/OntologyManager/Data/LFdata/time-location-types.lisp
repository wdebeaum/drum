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
 :comment "Spatial relations that locate one object (the figure) in terms of another object (the ground), which often is implicit"
 ;; situations can be spatially located, e.g. meetings, riots, parties
 ;; so can abstr-obj: the idea in the document; the name on the envelope; the man at the party
 :arguments (;(:ESSENTIAL ONT::OF ((? of F::Phys-obj F::Situation f::abstr-obj)))
	     ;(:ESSENTIAL ONT::val ((? val F::Phys-obj F::Situation f::abstr-obj)))
	     (:ESSENTIAL ONT::FIGURE ((? fig F::Phys-obj F::Situation f::abstr-obj)))
	     (:ESSENTIAL ONT::GROUND ((? grd F::Phys-obj F::Situation f::abstr-obj)
				      (f::scale (? !sc ONT::TIME-MEASURE-SCALE)))
             ))
 )

(define-type ont::at-scale-value
    :comment "The main predicate for associating an object with a value on a scale"
    :parent ont::position-reln)

; *********************************************
;
; ont::position-as-point-reln subtree
;
; *********************************************

; figure is viewed as a point
(define-type ont::position-as-point-reln
    :comment "locating an object (FIGURE) wrt a point-like object (GROUND)"
    :parent ont::position-reln
    )


(define-type ont::at-loc
    :comment "prototypical locating of a FIGURE wrt a point-like GROUND"
    :parent ont::position-as-point-reln
    :arguments ((:ESSENTIAL ONT::GROUND ((? val f::phys-obj f::abstr-obj f::situation) (f::tangible +)
					 (f::scale (? !t ONT::TIME-MEASURE-SCALE ONT::RATE-SCALE ONT::MONEY-SCALE ONT::NUMBER-SCALE)) ; excludes "at four"
				       )))
    )

; figure is viewed as a point and related to ground by (abstract) containment
(define-type ont::pos-wrt-containment-reln
    :comment "locating an object (typically the FIGURE) within an extended object (typically the GROUND)"
    :parent ont::position-reln
    )

; in, within, inside (of)
(define-type ont::in-loc
    :parent ont::pos-wrt-containment-reln
    :comment "FIGURE is within or inside the GROUND"
  :arguments ((:ESSENTIAL ONT::GROUND ((? val f::phys-obj f::abstr-obj) ; measure (music)
				       (f::intentional -) (f::container +) ; containers include corner and pathway
				       )))
  )

(define-type ont::contain-reln
    :comment "a kind of Inverse of IN-LOC, but can't be used as a result location"
    :parent ont::pos-wrt-containment-reln
    :arguments ((:ESSENTIAL ONT::FIGURE ((? val f::phys-obj) (f::intentional -)
					 (f::container +)
				   )))
    )

; figure is outside a container, group or area
(define-type ont::outside
  :parent ont::pos-wrt-containment-reln
  :arguments ((:ESSENTIAL ONT::GROUND ((? val f::phys-obj) (f::intentional -)
				       (f::container +)  ;; having container + causes problems with things like "pull the plug out of the wall"
				       )))
  )
#||
; out (of), outside (of)
(define-type ont::out-loc
  :parent ont::outside
  )||#

#|
(define-type ont::delimit-reln
    :comment "the FIGURE has a value in a rnage that is delimited by the GROUND, e.g., within five dollars of the estimate, within five miles of starbucks"
  :parent ont::pos-wrt-containment-reln
  :arguments ((:ESSENTIAL ONT::FIGURE ((? of F::phys-obj f::abstr-obj)))
	      (:ESSENTIAL ONT::GROUND ((? val F::phys-obj f::abstr-obj)))
	      )
  )
|#

					; figure is spatially related to a group of objects (the ground)
(define-type ont::complex-ground-reln
    :comment "FIGURE is located wrt a set of objects that comprise the GROUND. e.g., between X and Y, among the participants"
    :parent ont::position-reln
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
#||
; figure related by directon from ground
(define-type ont::pos-directional-reln
    :comment "FIGURE is related via a directional orientation to the GROUND"
  :parent ont::position-as-point-reln
  )
||#


; figure is related by inherent orientation of ground
(define-type ont::oriented-loc-reln
    :comment "FIGURE is located by a directional relationship with the GROUND"
    :arguments ((:ESSENTIAL ONT::GROUND ((? grd F::Phys-obj f::abstr-obj)
					 (f::type (? !t ONT::TIME-MEASURE-SCALE)))))
    :parent ont::position-as-point-reln
    )

; direction is a verticality reln
(define-type ont::directional-vert
    :comment "FIGURE is related via a vertical orientation to the GROUND"
  :parent ont::oriented-loc-reln
  :arguments ((:ESSENTIAL ONT::GROUND ((? val f::phys-obj f::abstr-obj )
				   )))
  )

; figure is below ground (in some way)
; below, under, underneath
(define-type ont::below
    :wordnet-sense-keys ("below%4:02:01")
    :comment "FIGURE is lower on some vertical scale than the GROUND"
    :parent ont::directional-vert
    )

; figure is below (and translated) from ground
; down (from/of)
(define-type ont::down
    :wordnet-sense-keys ("down%4:02:00" "down%4:02:05")
    :comment "FIGURE is lower on a scale"
    :parent ont::directional-vert
    )

; figure is above ground (in some way)
; above, over
(define-type ont::above
     :comment "FIGURE is higher on some vertical scale than the GROUND"
     :parent ont::directional-vert
  )

; figure is above (and translated) from ground
; up (from/of/to)
(define-type ont::up
    :comment "FIGURE is higher on some vertical scale than the GROUND"
    :parent ont::directional-vert
    )

(define-type ont::orients-to
    :parent ont::oriented-loc-reln
    :comment "FIGURE is located by an object defined by an orientation wrt an object. e.g., to the east, to the back"
  :arguments ((:essential ONT::GROUND ((? of1  f::phys-obj f::abstr-obj) (f::type (? t ONT::CARDINAL-POINT ONT::object-dependent-location)))))
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

; figure is related by a navigational reln to ground
(define-type ont::navigational-reln
    :comment "Figure is related by a cardinal directional reln to ground"
    :parent ont::oriented-loc-reln
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
; *********************************************
;
; ont::position-as-extent-reln subtree
;
; *********************************************

(define-type ont::position-wrt-area-reln
    :comment "Position of FIGURE defined in terms or a relationship to an area (the GROUND)"
    :parent ont::position-reln
    :arguments ((:ESSENTIAL ONT::FIGURE ((? t F::Phys-obj F::Situation)))
		(:ESSENTIAL ONT::GROUND (F::Phys-obj (F::type ont::geo-object)
						     (F::spatial-abstraction (? !xx F::spatial-point))))
		))
    
;; mapped to ONT::ABOVE and ONT::DISTRIBUTED-POS
#||(define-type ont::pos-as-over
    :comment "FIGURE is above GROUND"
    :parent ont::position-wrt-area-reln
    )||#

; figure is distributed throughout ground
; throughout, through
(define-type ont::distributed-pos
    :comment "FIGURE is distributed over the GROUND"
    :parent ont::position-wrt-area-reln
    )

(define-type ont::across
    :comment "FIGURE is a slice through the GROUND from one side to the other. Conceptually on the GROUND"
    :parent ont::position-wrt-area-reln
    )

(define-type ont::pos-as-opposite
    :comment "FIGURE is on an opposite side of an area wrt some reference object: e.g., it is across the street"
    :parent ont::position-wrt-area-reln
    )

(define-type ont::around
    :comment "FIGURE is an area surrounding the boundary of the GROUND, or distrubuted over the GROUND"
    :parent ont::position-wrt-area-reln
    )

(define-type ONT::through
    :parent ONT::position-wrt-area-reln
    :comment "FIGURE crosses the GROUND, conceptually IN the ground" 
    )




; *********************************************
;
; > ont::position-wrt-linear-area-reln subtree
;
; *********************************************

(define-type ont::position-wrt-linear-area-reln
    :comment "FIGURE is defined wrt an area (the GROUND) that has a linear orientation"
    :parent ont::position-reln
    :arguments ((:ESSENTIAL ONT::FIGURE ((? t F::Phys-obj F::Situation)))
		(:ESSENTIAL ONT::GROUND (F::Phys-obj (F::type ont::geo-object)
						     (F::spatial-abstraction (? !ss F::spatial-point)))))
    )

; figure is linear and adjacent to ground
; along, alongside (of)
(define-type ont::pos-extended-along-linear-area
    :comment "FIGURE is located within the linear area (GROUND). e.g., I found it along the street. We walked along the street"
    :parent ont::position-wrt-linear-area-reln
    )

(define-type ont::pos-relative-wrt-trajectory
    :comment "FIGURE is located along a linear area wrt some other object (GROUND)"
 :parent ont::position-wrt-linear-area-reln
 )

(define-type ont::pos-before-in-trajectory
    :comment "FIGURE is before the GROUND wrt a linear area"
    :parent ont::pos-relative-wrt-trajectory
    )

(define-type ont::pos-after-in-trajectory
    :comment "FIGURE is after the GROUND wrt a linear area"
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
 :comment "constrains the location of an object undergoing motion"
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
             (:REQUIRED ONT::GROUND (F::Phys-obj (f::type (? !t ont::cardinal-point ont::loc-wrt-orientation))))
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


; figure is trajectory, ground is within the trajectory
; via, by way of
(define-type ont::obj-in-path
    :arguments (;(:ESSENTIAL ONT::FIGURE ((? type F::Situation F::phys-obj) (F::type (? path-type ont::motion ont::apply-force ont::route)) (F::trajectory +)))
		;(:ESSENTIAL ONT::FIGURE ((? type F::phys-obj) ))
		(:ESSENTIAL ONT::FIGURE ((? type F::Situation F::phys-obj) (F::type (? t ont::route ont::event-of-change)) )) ; I go to the post office via ...; The route to Avon via...
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
#||   ;; all subtypes moved to 
;; close (to)/near a house, a party, a zipcode

(define-type ONT::trajectory
; :parent ONT::PREDICATE
 :parent ONT::PATH
 :arguments (;(:ESSENTIAL ONT::FIGURE ((? t F::Phys-obj F::Situation) (F::trajectory +)))
	     (:ESSENTIAL ONT::FIGURE ((? t F::Phys-obj F::Situation)))
             (:ESSENTIAL ONT::GROUND (F::Phys-obj))
             )
 )||#

(define-type ONT::from-loc
 :parent ONT::source-reln
 :arguments (;(:ESSENTIAL ONT::FIGURE ( F::situation (F::type ont::motion)))
	     ;(:ESSENTIAL ONT::GROUND (F::Phys-obj (f::spatial-abstraction (? sa f::spatial-point))))

	     ; copied from to-loc
	     (:ESSENTIAL ONT::FIGURE ((? f F::PHYS-OBJ F::abstr-obj))); (F::situation (f::type ont::event-of-change)))   ; "I walked to the store" FIGURE should point to "I", not "walked"
	     (:ESSENTIAL ONT::GROUND ((? t F::Phys-obj F::abstr-obj) (f::spatial-abstraction ?!sa)
					;(F::form F::geographical-object)
				      ) )  ; spatial-abstraction is not enough: many things have spatial-abstraction, e.g., a frog.  Another possibility is (F::object-function F::spatial-object)

	     )
 )

; I moved from the chair to the sofa.  not geographic-object (gound)
; transmit the signal: signal is abstr-obj (figure)
(define-type ONT::to-loc
    :comment "the generic goal role: might be a physical object (as possessor) or a resulting state"
 :parent ONT::goal-reln
 :arguments (;(:ESSENTIAL ONT::OF (F::situation (f::type ont::event-of-change)))
	     ;(:ESSENTIAL ONT::VAL ((? t F::Phys-obj F::abstr-obj)))
	     (:ESSENTIAL ONT::FIGURE ((? f F::PHYS-OBJ F::abstr-obj))); (F::situation (f::type ont::event-of-change)))   ; "I walked to the store" FIGURE should point to "I", not "walked"
	     (:ESSENTIAL ONT::GROUND ((? t F::Phys-obj F::abstr-obj) (f::spatial-abstraction ?!sa)
					;(F::form F::geographical-object)
				      ) )  ; spatial-abstraction is not enough: many things have spatial-abstraction, e.g., a frog.  Another possibility is (F::object-function F::spatial-object)
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



(define-type ONT::direction
    :parent ONT::position-reln
    :wordnet-sense-keys ("direction%1:15:00")
    :comment "a direction = a spatial relation between the location (FIGURE) of an object and its previous location"
 :arguments (;(:ESSENTIAL ONT::FIGURE ((? t F::Phys-obj
					 ;F::abstr-obj ; "move the plan up the agenda"?
					; F::situation) ; meeting is SITUATION (move the meeting up the stairs)
				      ;(F::mobility f::movable) (F::ASPECT F::DYNAMIC)))
	     (:ESSENTIAL ONT::FIGURE (F::SITUATION (f::aspect f::dynamic) (f::trajectory +)))
	     (:ESSENTIAL ONT::GROUND (F::Phys-obj))
	     (:OPTIONAL ONT::NOROLE)
            )
 )

(define-type ont::direction-wrt-entity
    :parent ont::direction
    :comment "direction wrt respect to a given entity (the GROUND), i.e., the relation between the FIGUREs orginal and final position is defined with respect to the GROUND"
    )

(define-type ont::direction-forward
    :wordnet-sense-keys ("forward%4:02:00")
    :parent ont::direction-wrt-entity
    :comment "FIGURE moves forward from its original position, where forward is defined by the orientation of the GROUND"
    )

(define-type ont::direction-backward
    :parent ont::direction-wrt-entity
    :wordnet-sense-keys ("back%4:02:00")
    :comment "FIGURE moves backward from its original position, where backward is defined by the orientation of the GROUND"
    )

(define-type ont::direction-rightward
    :parent ont::direction-wrt-entity
     :wordnet-sense-keys ("right%4:02:03")
    :comment "direction rightward wrt respect to a given entity (the GROUND)"
    )

(define-type ont::direction-leftward
    :wordnet-sense-keys ("left%4:02:03")
    :parent ont::direction-wrt-entity
    :comment "direction leftward wrt respect to a given entity (the GROUND)"
    )

(define-type ont::towards
    :comment "direction towards from a given entity (the GROUND)"
    :parent ont::direction-wrt-entity
    )

(define-type ont::away
    :comment "direction away a given entity (the GROUND)"
    :parent ont::direction-wrt-entity
    )

(define-type ont::direction-rotation
    :comment "rotational direction of the FIGURE"
    :parent ont::direction
    )

(define-type ont::clockwise
    :parent ont::direction-rotation
    )

; counterclockwise
(define-type ont::counterclockwise
    :parent ont::direction-rotation
    )

(define-type ont::direction-wrt-verticality
    :parent ont::direction
    :comment "direction wrt respect to verticality (e.g., gravity or a 'vertical' scale, e.g., temperature), possibly also related to a extended object, e.g., a street, a body, which is the GROUND"
    )

(define-type ont::direction-down
     :comment "This is the particle 'down' and is relative to some scale/domain: e.g., the temperature is down, move the ball down"
    :parent ONT::DIRECTION-wrt-verticality)

(define-type ont::direction-down-ground
    :comment "this is the transitive 'down' that has a GROUND that describes a physical object and locations objects or events"
    :arguments ( (:ESSENTIAL ONT::FIGURE ((? t F::Phys-obj 
					 F::situation)))
		 (:ESSENTIAL ONT::GROUND (F::Phys-obj)))
    :parent ONT::DIRECTION-wrt-verticality)

(define-type ont::direction-up
    :comment "This is the particle 'up' and is relative to some scale/domain: e.g., the temperature is up, move the ball up"
    :parent ONT::DIRECTION-wrt-verticality)

(define-type ont::direction-up-ground
    :comment "this is the transitive 'up' that has a GROUND that describes a physical object and locations objects or events"
    :arguments ( (:ESSENTIAL ONT::FIGURE ((? t F::Phys-obj 
					 F::situation)))
		 (:ESSENTIAL ONT::GROUND (F::Phys-obj)))
    :parent ONT::DIRECTION-wrt-verticality)

(define-type ont::direction-wrt-containment
    :parent ont::direction
    :comment "direction relative to containment in some object (the GROUND)"
    )

(define-type ont::direction-in
    :comment "direction involving moving into some object (the GROUND)"
    :parent ont::direction-wrt-containment
    )

(define-type ont::direction-out
    :comment "direction involving moving into some object (the GROUND)"
    :parent ont::direction-wrt-containment
    )

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

(define-type ONT::extent-predicate
 :parent ONT::PREDICATE
 :sem  (F::abstr-obj)
 :arguments (;(:ESSENTIAL ONT::FIGURE (F::Situation))
	     (:ESSENTIAL ONT::FIGURE (F::Situation (f::type ont::change-magnitude)))
;	     (:ESSENTIAL ONT::GROUND (F::abstr-obj (F::scale ont::length-scale)))
;	     (:ESSENTIAL ONT::GROUND (F::abstr-obj))  ; no scale (e.g., increase by three dogs)
	     (:ESSENTIAL ONT::GROUND (F::abstr-obj (F::scale ?!sc)))  ; put scale back for now
             )
 )

;;; he ran for five miles
(define-type ONT::spatial-distance-rel
 :parent ONT::extent-predicate
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Situation (f::trajectory +) (F::aspect (? asp F::unbounded F::stage-level)) (F::time-span F::extended)))
	      (:ESSENTIAL ONT::GROUND (F::abstr-obj (F::scale ont::length-scale)))
             )
 )


;;;
;;; > Time predicates here
;;;
;;; The general parent class for all time relationships

(define-type ONT::temporal-predicate
 :parent ONT::PREDICATE   ;; if we change this to extent-predicate, we need to generalize the restrition
 :sem  (F::abstr-obj) ;; (F::Scale Ont::time-measure-scale))
 )

;;; A class for core temporal properties of events - aspect, tense, ...
(define-type ONT::time-rel
 :parent ONT::TEMPORAL-PREDICATE
 :arguments ((:REQUIRED ONT::FIGURE (F::situation))
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
 :arguments ((:ESSENTIAL ONT::FIGURE ((? of F::phys-obj F::abstr-obj f::situation f::time))) ; abstr-obj: price; phys-obj: plant
             )
 )

;; durations
(define-type ONT::event-duration-modifier
    :sem (F::ABSTR-OBJ  (F::Scale Ont::duration-scale))
  :parent ONT::TEMPORAL-MODIFIER
  :arguments ((:ESSENTIAL ONT::FIGURE ((? of f::situation f::time)))
					;             (:essential ont::GROUND (f::abstr-obj (F::Scale Ont::duration-scale) (F::type ont::time-unit)))
					;             (:essential ont::GROUND ((? gd F::abstr-obj F::time) (F::time-scale f::interval)))
	      
					;(:essential ont::GROUND (F::abstr-obj (F::Scale Ont::duration-scale) (F::type ont::time-unit)))
	      (:essential ont::GROUND ((? gd F::abstr-obj F::time) (F::Scale Ont::duration-scale) ))
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

#||;; how long did it take / did he run         Already have ONT::DURARION-SCALE
(define-type ont::duration
  :parent ont::measure-scale
  :sem (F::abstr-obj (F::Scale Ont::duration-scale))
  :arguments ((:ESSENTIAL ONT::FIGURE ((? t f::situation F::abstr-obj)))
             )
  )
||#
;; frequencies
(define-type ONT::frequency
 :parent ONT::temporal-modifier
 :arguments ((:ESSENTIAL ONT::GROUND (F::abstr-obj (F::scale ont::duration-scale)))
             )
 )

;;; this is a fixed frequency - e .g. do it 3 times
(define-type ONT::repetition
 :wordnet-sense-keys ("repeatedly%4:02:00" "over_and_over%4:02:00")
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
 :parent ONT::predicate  ;; this has nothing to do with time per se
 :arguments ((:ESSENTIAL ONT::FIGURE (F::abstr-object (F::type (? ttt ONT::QUANTITY)))))
 )

;; the population in the 1920s; the shortage in the 1920s
;;; in June, in 1990, in two days
;; This should only be used for locating events in a time span, not for durations
;; so I uncommented the first defn of ONT::VAL
(define-type ONT::time-span-rel
 :parent ONT::temporal-location
 :arguments ((:ESSENTIAL ONT::GROUND (F::time ;(F::time-scale f::interval)
					      ;(F::time-function (? funcn F::month-name F::year-name F::day-period))
					      ))

;	     (:ESSENTIAL ONT::SIT-VAL (F::situation))
	     ;;(:ESSENTIAL ONT::VAL (F::time ((? vl F::abstr-obj f::time) (F::scale ont::duration-scale))) ;; five minutes/days/hours
	     ;; now used shared (f::scale ont::duration feature on the ont::val instead of ont::time-val
;	     (:optional ont::time-val (f::time)) ;; in June, in 1990
             )
 )

;; the meeting next week; he arrives next week
(define-type ONT::event-time-rel
 :parent ONT::temporal-location
 :arguments ((:ESSENTIAL ONT::FIGURE ((? f F::Situation F::time))) 
             (:ESSENTIAL ONT::GROUND ((? vl F::time f::situation)))
	     )
 )

(define-type ont::start-time
    :parent ont::event-time-rel
    )

(define-type ont::before
    :parent ont::event-time-rel
    :wordnet-sense-keys ("before%4:02:03")
    )

(define-type ont::after
    :parent ont::event-time-rel
    :wordnet-sense-keys ("after%4:02:00" "after%4:02:01")
    )

(define-type ont::during
    :parent ont::event-time-rel
    :comment "DURING, STARTS or ENDS in ITL"
    )

(define-type ont::simultaneous
    :parent ont::event-time-rel
    :comment "EQUAL in ITL"
    )

(define-type ont::immediate
    :parent ont::event-time-rel
    :wordnet-sense-keys ("immediately%4:02:00" "immediately%4:02:05")
    )

(define-type ont::when-while
    :parent ont::event-time-rel
    
    )

(define-type ont::until
    :parent ont::event-time-rel
   
    )

;; still, yet, so far, ....
(define-type ONT::time-rel-so-far
; :parent ONT::event-time-rel
    :parent ONT::EVENT-DURATION-MODIFIER
    :wordnet-sense-keys ("so_far%4:02:00")
    :arguments ((:ESSENTIAL ONT::FIGURE (F::Situation))
             )
 )

(define-type ONT::event-time-wrt-now
    :parent ONT::event-time-rel
     )

(define-type ONT::now
     :wordnet-sense-keys ("now%4:02:05" "presently%4:02:00" "current%3:00:00" "present%3:00:01")
     :parent ONT::event-time-wrt-now
     )

(define-type ONT::recent
     :wordnet-sense-keys ("new%3:00:00")
     :parent ONT::event-time-wrt-now
     )

(define-type ONT::in-future
     :wordnet-sense-keys ("future%3:00:00")
     :parent ONT::event-time-wrt-now
     )

(define-type ONT::in-past
     :wordnet-sense-keys ("past%3:00:00")
     :parent ONT::event-time-wrt-now
     )

(define-type ONT::soon
     :wordnet-sense-keys ("soon%4:02:00""imminent%3:00:00")
     :parent ONT::event-time-wrt-now
     )

(define-type ont::occuring-now
     :wordnet-sense-keys ("current%3:00:00")
     :parent ONT::event-time-wrt-now
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

;;; some things apply only to day names, etc
;; on Monday, on the next day
(define-type ONT::time-on-rel
 ;:parent ONT::temporal-location
 :parent ONT::time-span-rel
 :arguments ((:ESSENTIAL ONT::GROUND (F::time (f::type ont::date-object-on))
             )
 ))

;;; some things apply only to month names, etc
;; in June
(define-type ONT::time-in-rel
 ;:parent ONT::temporal-location
 :parent ONT::time-span-rel
 :arguments ((:ESSENTIAL ONT::GROUND (F::time (f::type ont::date-object-in))
             )
 ))

;;; some things apply only to time specifications like 5am/noon
(define-type ONT::time-clock-rel
 :parent ONT::temporal-location
 :arguments ((:ESSENTIAL ONT::GROUND (F::time (F::time-function (? fn F::clock-time F::day-point)))) ;(f::time-scale f::point)))
;	     (:ESSENTIAL ONT::SIT-VAL (F::situation))
             )
 )

;;; these modify time objects
;; short term parking; a long day
(define-type ONT::interval-duration-modifier
 :parent ONT::event-duration-modifier
 :arguments ((:essential ONT::FIGURE (F::time (f::time-function f::time-frame))) ;(f::time-scale f::interval)))
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
    :sem (F::time (F::time-function F::Any-time-function) (F::scale ONT::TIME-LOC-SCALE)) ;(F::time-scale (? sc F::interval F::point)))
    )

(define-type ONT::Time-object
    :comment "objects that refer to temporal locations in some way"
    :parent ONT::ANY-TIME-OBJECT
    :arguments ((:OPTIONAL ONT::FIGURE))
    ;:sem (F::time (F::time-scale (? sc F::point F::interval)))
    )

;; these are intervals such as "duration", which cannot generally be counted
;; or serve as time units
(define-type ONT::Time-interval
 :wordnet-sense-keys ("interval%1:28:00" "time_interval%1:28:00" "time%1:28:03" "clock_time%1:28:00" "time%1:28:00" "time%1:28:05" "time_period%1:28:00")
 :parent ONT::TIME-OBJECT
 ;:sem (F::time (F::time-scale (? sc F::interval)) (F::Scale -) (F::Tangible +)) ;;Ont::duration-scale))
 :sem (F::time (F::Tangible +)) ;;Ont::duration-scale))
 :arguments ((:OPTIONAL ONT::GROUND (F::time (f::time-function f::time-frame) ;(f::time-scale f::interval)
					     (f::scale ont::duration-scale)))
             ;;; a time of two hours
             (:OPTIONAL ONT::FIGURE ((? t f::situation f::abstr-obj f::time)))
	     (:OPTIONAL ONT::EXTENT (F::Abstr-obj ;(f::time-scale f::interval)
						  (f::scale ont::duration-scale)))
             )
 )



;;; phase, stage (e.g. phases of the moon) 
;;; not strictly bound to time, but there currently is no better place to place this type. once abstract sequence is defined
;;; this type can be moved under it. 
(define-type ONT::phase
 :parent ont::time-interval
 :arguments ((:REQUIRED ONT::FIGURE)
	     )
 :comment "e.g., phases of the moon, stage of the project. This type represents stages of a sequence that is more abstract than time."
)

;;  direct reference to times (e.g. now, then, ...)
;;  this type is also constructed by the grammar for dates, times of day, etc.
(define-type ONT::TIME-LOC
    :wordnet-sense-keys ("date%1:28:03")
    :parent ONT::time-object)

(define-type ONT::recurring-time-of-day
    :comment "recurring moments of the day, defined by some event"
    :sem (F::time (F::time-function F::day-point))
    :parent ont::time-interval
    )

(define-type ONT::time-defined-by-event
    :comment "times defined by events"
    :sem (F::time (F::time-function F::day-point))
    :parent ont::time-interval
    )

(define-type ONT::time-defined-by-duration
    :comment "times defined by events"
    :parent ont::time-interval
    )

(define-type ONT::month
    :comment "time interval of a named month"
    :parent ont::time-interval
    )

(define-type ONT::week
    :comment "time interval of a week"
    :parent ont::time-interval
    )

(define-type ONT::day
    :comment "time interval of a day"
    :parent ont::time-interval
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
 ;:sem (F::time (f::time-scale f::point))
 :arguments ((:OPTIONAL ONT::FIGURE ((? lof f::situation F::time)))
	     ;; this is because we may have things "the end of an event". In reality, there should be a coercion rule, but we are not doing it yet
	     ;; middle of the meeting
;	     (:OPTIONAL ONT::ACTION (F::situation (f::aspect f::unbounded)))
	     ;; middle of the year
;	     (:OPTIONAL ONT::INTERVAL (F::time))
             )
 )

#||
(define-type ONT::date-object
 :wordnet-sense-keys ("date%1:28:03" "time%1:03:00")
 :comment "classification of time intervals with respect to some conceptual organization (e.g., calendar)"
 :parent ONT::TIME-Object
 :sem (F::time (F::time-function F::time-of-year)) ;(f::time-scale f::interval))
 )||#

(define-type ont::date-object-on
    :comment "date objects that use ON - e.g., on Monday, on my birthday"
    :parent ONT::TIME-Object
    )

; if considering moving date-object-in to under date-object, check -dt-dow> ("June" is a NAME but should not go through -dt-dow>)
(define-type ont::date-object-in
    :comment "temporal objects that use IN - e.g., in June"
    :parent ONT::TIME-Object
    )

(define-type ONT::today
     :wordnet-sense-keys ("today%4:02:00" "today%1:28:01")
     :parent ONT::time-loc
         )

(define-type ONT::yesterday
     :wordnet-sense-keys ("yesterday%4:02:00" "yesterday%1:28:01")
     :parent ONT::time-loc
     )

(define-type ONT::tomorrow
     :wordnet-sense-keys ("tomorrow%4:02:00" "tomorrow%1:28:01")
     :parent ONT::time-loc
     )

;;; future, past
(define-type ONT::time-span
; :parent ONT::TEMPORAL-PREDICATE
 :parent ont::date-object-in
 :arguments ((:REQUIRED ONT::FIGURE))
 )

(define-type ONT::day-name
 :wordnet-sense-keys ("calendar_day%1:28:00")
 :parent ONT::DATE-OBJECT-on
 :sem (F::time (F::time-function F::day-of-week))
   :arguments ((:OPTIONAL ONT::FIGURE ((? t f::situation f::abstr-obj)))
	       (:optional ont::GROUND)
             )
   )

(define-type ont::recurring-event
    :comment "events that recur every year (or some time interval)"
    :wordnet-sense-keys ("day%1:28:01" "season%1:28:01" "season%1:28:02")
    :parent ONT::date-object-on)



(define-type ONT::holiday
    :wordnet-sense-keys ("leisure%1:28:00")
    :comment "recurring events based on religious or social activities"
    :parent ont::recurring-event
  )


(define-type ONT::era
    :wordnet-sense-keys ("era%1:28:00" "era%1:28:01")
    :parent ONT::DATE-OBJECT-IN
    :sem (F::time (f::time-function f::era))
    )

(define-type ONT::year
    :parent ONT::DATE-OBJECT-IN
    :wordnet-sense-keys ("year%1:28:00" "year%1:28:01" "year%1:28:02")
 :sem (F::time (f::time-function f::year-name))
 )

(define-type ONT::century
    :parent ONT::DATE-OBJECT-IN
    :wordnet-sense-keys ("century%1:28:00")
    :sem (F::time (f::time-function f::time-of-year))
    )

(define-type ONT::day-stage
    :parent ONT::DATE-OBJECT-IN
    :comment "a regular part of the day"
     :wordnet-sense-keys ("morning%1:28:00" "evening%1:28:00" "night%1:28:00" "twilight%1:28:00" "afternoon%1:28:00"  )
    :sem (F::time (f::time-function f::day-period))
 )

(define-type ONT::year-stage
    :parent ONT::DATE-OBJECT-IN
    :sem (F::time (f::time-function f::time-of-year))
 )

(define-type ONT::month-name
 :wordnet-sense-keys ("calendar_month%1:28:00" "month%1:28:01")
 :parent ONT::year-stage
 :sem (F::time (F::time-function F::month-name))
 )

(define-type ONT::week-object
    :parent ONT::year-stage
    :wordnet-sense-keys ("calendar_week%1:28:00" )
    :sem (F::time (F::time-function F::time-of-year))
    )

(define-type ONT::season
    :parent ONT::year-stage
    :wordnet-sense-keys ("season%1:28:00")
    )

(define-type ONT::winter
    :wordnet-sense-keys ("winter%1:28:00")
    :parent ONT::season
 )

(define-type ONT::spring
    :wordnet-sense-keys ("spring%1:28:00")
    :parent ONT::season
 )

(define-type ONT::summer
    :wordnet-sense-keys ("summer%1:28:00")
 :parent ONT::season
 )

(define-type ONT::autumn
    :wordnet-sense-keys ("autumn%1:28:00")
 :parent ONT::season
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

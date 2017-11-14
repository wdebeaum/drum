(in-package :om)

;;;this is aspect tree, basically
;;;)
(define-feature F::Aspect
 ;;;(F_ANY-aspect
 :values (;;; these are all events
          (F::Dynamic
           (F::Bounded)
           (F::Unbounded)
           
           )
          (F::Static
           ;;;(F_Property) ;; x is great
           ;;;(F_Relation) ;; x is bigger than y, x contains/holds y etc
           ;;; swift 01/12/01 -- replace F_State with F_Stage-level; this is straightforward...it is less clear how
           ;;; F_Property (have & be) & F_Relation should be handled.
           ;;;(F_State) ;; these are states: X happened, a knos b (is it a relation???)
           ;;; swift 01/12/01 -- add subfeatures F_Indiv-level and F_Stage-level for a distinction between
           ;;; individual vs. stage level state predicates
           ;;; don't take progressive aspect, e.g. believe in Santa Clause
           (F::Indiv-Level)
           ;;; do take progressive aspect, e.g. feel(ing) happy
           (F::Stage-Level)
           
           )
          )
 )

(define-feature-rule static-aspect
 :feature (f::aspect f::static)
 :implies (F::situation (F::time-span F::extended))
 )

;;;;(define-feature-arguments
;;;; :feature (Aspect F_Dynamic)
;;;; ;; Myrosia 06/09/02 relaxed the
;;;; ;; restriction to account for the fact that event times can be
;;;; ;; eitehr time or situation objects. In the future, we will have to
;;;; ;; figure out a better way to do that via uniform type coercion for
;;;; ;; events
;;;; :arguments ((lf_event-time-rel ((? ftype ft_time ft_situation)))
;;;;		))
;;; agents etc
(define-feature F::Cause
 :values ((-)
          ;;; we get a "cause" here, further extended to Agent
          (F::Force
           ;;; weather etc, no agents - activity???
           (F::Phenomenal)
           ;;; all of these can have agents
           (F::Agentive)
           
           )
          ;;; experiencer
          (F::Stimulating)
          ;;; All kinds of mental things do not really have agents, but something else
          (F::Mental)
          )
 )

;;;;(define-feature-arguments
;;;; :feature (Cause F_Force)
;;;; :arguments ((cause))
;;;;)
;;;;
;;;;(define-feature-arguments
;;;; :feature (Cause F_Stimulating)
;;;; :arguments ((experiencer))
;;;;)
;;;;
;;;;(define-feature-arguments
;;;; :feature (Cause F_Agentive)
;;;; :ARGUMENTS (
;;;;		;; !!! for now ft_Phys-obj until the bug in the parser is fixed
;;;;		;;(agent ((Order ?order) (Intentional +)) force)
;;;;		(agent (ft_Phys-obj (Intentional +)) cause)
;;;;		)
;;;;)
;;; these pertain to adverbials
;;;)
(define-feature F::Time-span
 ;;;(F_Any-Time)
 :values (;;; occurs "instantly" - e.g. stop
          (F::Atomic)
          (F::Extended)
          )
 )

;;;;(define-feature-arguments
;;;; :feature (Time-span F_Atomic)
;;;; ;; Myrosia 06/09/02 relaxed the
;;;; ;; restriction to account for the fact that event times can be
;;;; ;; eitehr time or situation objects. In the future, we will have to
;;;; ;; figure out a better way to do that via uniform type coercion for
;;;; ;; events
;;;; :arguments ((lf_event-time-rel ((? ftype ft_time ft_situation)))
;;;;		)
;;;;)
;;; Myrosia 03/20/00 changed the name to LOCATIVE to avoid confusion iwth LOCATION in nonverbhier
;;;)
(define-feature F::Locative
 ;;;(F_Any-Locative
 :values (;;; they occur at a specific place - e.g. stop, build a house
          (F::Located)
          (-)
          )
 )

(define-feature F::iobj
    :values ((F::recipient)
	     (F::beneficiary)
	     (-)
          )
    )

#||  REMOVING AS I DON'T THINK THIS IS USED ANY MORE -- AND IT CAUSES TROUBLE FOR GLOSS JFA 3/16
(define-feature-arguments
 :feature (f::Locative f::located)
 :arguments(
	    ;; Myrosia 2005/02/21 updated the definition from (f::object-function f::place) to f::spatial-object
	    ;; this allows for interpretations like "it is on route to Avon"
	    ;; which otherwise would be thrown out
	    ;; 2005/08/06 further relaxed to spatial object or representation to cover "on the map"
	    (ONT::spatial-loc (f::Phys-obj (F::object-function (? of f::spatial-object f::representation))))
	    )
 )||#
 

;;;swier added gradability for adjectives
(define-feature F::gradability
 :values ((+)
          (-)
          )
 )

;;;swift adding orientation and intensity for adjectives
(define-feature F::orientation
 :values ((f::pos)
	  (f::neg)
	  (f::neutral)
	  (-)
          )
 )

(define-feature F::intensity
 :values ((f::hi)
          (f::med)
	  (f::lo)
	  (-)
          )
 )

(define-feature F::Trajectory
 :values ((+)
          (-)
          )
 )

;;;(define-feature Usage)) ;; these all can have instruments
;;;)
;; this division doesn't give us a straightforward way to group sentient entities -- for instance, the noise annoyed the man/dog/*tree.
(define-feature F::Origin
 ;;;(F_Any-Origin
 :values ((F::Natural
           (F::Living
            (F::Human)
            (F::Natural-non-human
             (F::Plant)
             (F::Creature)
             (F::non-human-Animal) ;; umls             
             )
            
            )
           (F::non-living
	    (F::Artifact)
	    )
           )
          
          )
 )

(define-feature-rule human-origin
 :feature (f::origin f::human)
 :implies (F::Phys-obj (F::intentional +) (F::form F::solid-object))
 )

;;;)
(define-feature F::Form
 ;;;(F_Any-Form
 :values ((F::Substance
           (F::Solid)
           (F::Liquid)
           (F::Gas)
           (F::Wave)
           )
          (F::Object
           ;;; spatial objects can have extra forms
           (F::solid-object)
           (F::hole)
           (F::enclosure)
           (F::geographical-object)
           
           )
          )
 )

;;;)
(define-feature F::SPATIAL-ABSTRACTION
 ;;;(F_Any-Spatial-Abstraction
 :values ((F::spatial-point)
          (F::line)
          (F::strip)
          (F::spatial-region)
          )
 )

;;;(define-feature Group))
;;; this was blindly copied form EuroWordNet
(define-feature F::object-function
 ;;; I think it's nice, but I haven't used it anywhere
 ;;;(F_Any-Functn
 :values (
	  (F::Instrument
	   (F::provides-service
	    (F::Vehicle)
	    (f::provides-service-on-off) ;; things that can be on/off
	    (f::provides-service-open-closed) ;; things that can be open/closed
	    (F::provides-service-up-down))   ;; thing that are up/down
	   )
          (F::Representation) 
          (F::software)
	  (f::currency)
	  (F::occupation)
          (F::Garment)
          (F::Furniture)
          (F::Covering)
	  (f::weather)
          (F::Container-object)
          (F::Comestible)
	  (f::support)
	  (f::body-part)
          ;;; possible 2nd-order functions
          (F::Linguistic-object
           (F::letter)
           (F::word)
           (F::sentence)
           
           )
          (F::spatial-object
           (F::place
            (F::Building)
            
            )
           ;;;	)
           ;;; (SEQUENCE
           ;;; (TRAJECTORY
           (F::PATH)
           
           )
          )
 )

(define-feature-rule function-vehicle
 :feature (f::object-function f::vehicle)
 :implies (F::Phys-obj (F::intentional -) (F::mobility F::movable))
 )

(define-feature-rule function-covering
 :feature (f::object-function f::covering)
 :implies (F::Phys-obj (F::intentional -) (F::form F::object))
 )

(define-feature-rule function-place
 :feature (f::object-function f::place)
 :implies (F::Phys-obj (F::intentional -) (F::form F::object))
 )

;;;	)))))
;;;;	 (F_Rate)
;;;;	 (F_Source) ;; semantics for things like "letter from you"
;;;;	 ;; adverbials
;;;;	 ;; myrosia o3/20/00 This is directly out of the nonverbhier.lisp
;;;;	 ;; Will need to be rethought later on
;;;;	 (F_ADV-SORT
;;;;	 (F_DISC) ;; discourse adverbials e.g., now, anyways, ...
;;;;	 ;;(SEQUENCE) duplicate?? sequence adverbials e.g., next, before
;;;;	 ;;(F_FREQUENCY)
;;;;	 (F_DEGREE-OF-BELIEF)
;;;;	 (F_COMPARATIVE)
;;;;	 (F_QUANT)
;;;;	 (F_ADJMOD) ;; adjective modifiers
;;;;	 (F_PREDSORT ;; adverbials that can be used as predications (e.g., "He is AT THE STORE")
;;;;		(F_MANNER)
;;;;		(F_SETTING)
;;;;		(F_QUALITY)
;;;;		)
;;;;	)
;;;;
;;;;	 (F_CONSTRAINT
;;;;	 (F_TIME-CONSTRAINT)
;;;;	 (F_OTHER-CONSTRAINT)
;;;;	 (F_POSSESSION)
;;;;	 (F_SUBPART)
;;;;	 (F_REASON)
;;;;	 (F_PURPOSE)
;;;;	 (F_METHOD)
;;;;	)
;;;;
;;;	 (F_measure-function) ;; any quantitative evaluation
;;;	 (F_second-order) ;; anything second order without a specific function
;;; these are the objects that can participate in date/time phrases for time semantics
(define-feature F::time-function
 :values ((F::time-of-day
           (F::clock-time)
           ;;; need to similify - don't need subvalues
           (F::day-part
            ;;; morning, afternoon, evening
            (F::day-period)
            ;;; this is am, pm
            (F::day-point)
            
            )
           
           )
          (F::time-of-year
           (F::day-of-week)
           (F::month-name)
           (F::date)
           (F::year-name)
	   (f::era)
           )
          (F::time-period
	   	   ;; time frames cannot be counted - *5 time spans
	   (F::time-frame)

	   ;; these are abstr-obj under ont::measure-unit
	   ;; time units can be counted - 5 hours
;           (F::time-unit ;; !!!! Myrosia: clean up everything below - never used anywhere!
;	    )
;;;            (F::calendar-unit
;;;             (F::day)
;;;             ;;; week month year
;;;             (F::calendar-period)
;;;             
;;;             )
;;;            (F::clock-unit)
;;;            
;;;            )
           
           )
          (F::frequency)
          )
 )

;;;)
(define-feature F::Mobility
 ;;;(F_Any-Mobility
 :values ((F::fixed)
          (F::movable
           ;;; Trucks and people are marked this way
           (F::self-moving
            (F::air-movable)
            (F::water-movable)
            (F::land-movable)
            
            )
           (F::non-self-moving)
           
           )
          )
 )

(define-feature F::Intentional
 :values ((+)
          (-)
          )
 )

(define-feature F::tangible
 :values ((+)
          (-)
          )
 )
 

(define-feature F::scale
 ; (F::any-scale
   :values ((-)
          ;;; ??? Taken to be "amount" from old hierarchy
  ;        (F::Quantity) not used?
          ;;; likely examples are "2 trains" etc
          ;;; Myrosia 03/20/00 0 changed "time" to "time-measure" to avoid "duplicate values" warning
          ;;;(F_Time-duration)
          ;;; (F_Time-date)
	    ;; Note: I move age-scale and time-measure-scale under linear-scale so that we can parse things like "a 30 year old woman"	    
	    (F::linear-scale   ;; height, width, length
	     (f::age-scale)
	     (f::time-measure-scale 
		(f::duration-scale))  ;; e.g., five minutes/hours/years...) 
	     (f::length-scale)   ;; e.g., three miles
	     )
	    (f::area-scale)  ;; area, sq feet, ...
	    (f::size-scale) ;; size, small, large ...
	    (f::number-scale)
	            ;;;(F_Speed)
	    (F::Volume-scale)
	    (f::sound-scale)
	    (F::Weight-scale)
	    (F::Temperature-scale)
	    (F::Money-scale)
	    (F::Rate-scale)
	    (F::luminosity-scale)
	    (f::humidity-scale)
	    (f::percent-scale)
	    (F::ratio-scale)
	  ;; this scale is used in unit calculations and shared by both times and abstract-objects; it is orthogonal to f::time-scale which is unique to f::time objects
	    
	  
	    (f::color-scale)
	    (F::Other-scale)
	    )
;   )
 )

(define-feature F::measure-function
 :values ((-)
          (F::Value
           (F::Unit)
           
           )
          (F::Term)
          )
 )

;;; this is a hack for now to handle times etc. We will need to fix it
;;;)
(define-feature F::Time-Scale
 ;;;(F_Any-Scale
 :values ((F::POINT)
          (F::INTERVAL)
          )
 )

;; here the feature should be time-frame, not time-period because time-units can be points
(define-feature-rule time-period-rule
    :feature (f::time-function f::time-frame)
    :implies (f::time (f::time-scale f::interval)))


; Myrosia 2005
;F::Information-content is the top-level value for feature f::information, to 
;contrast with "-". Currently it is further split into two subcategories:
;f::data and f::mental-construct. Generally speaking, "f::data" should be set 
;on things like "book" or "prescription", which are data or contain it. 
;"F::mental-construct" should be for something like "formula" which 
;corresponds to something that contains information but less directly. Before 
;there used to be another value, "f::event", which corresponded to events and 
;states. This was because of the sense that events contain information in some 
;sense (they can be explained or summarized), but not the same as data 
;objects. I now see that all events have been switched to "information 
;f::mental-construct", I don't remember when it happened and why. 
;"f::information-content" is a supertype over both "data" and 
;"mental-construct" when a better type could not be assigned.

;;;(define-feature Mental-object)
;;; :VALUES(
;;; (Linguistic-object)
;;; (Display)
;;; (Path)
;;;))
(define-feature F::Information
 ;;;(F_Any-Information
 :values ((-)
          (F::Information-content
           ;;;(F_Problem)
           ;;; (F_Plan)
           ;;;(F_Goal)
           ;;;	 (F_Solution)
           (F::Data)
           (F::mental-construct)
           
           )
          )
 )

;;;)
(define-feature F::Container
 ;;;(F_Any-Container
 :values ((+)
          (-)
          )
 )

;;; This is a feature "type" which is a catch-all for odds and ends
;;; that didn't fit anywhere else
;;; it isn't well developed yet
;;;	)
;;;)
(define-feature F::TYPE
    :name-only t)
 ;;;(F_Any-Type
#|| :values (;;;	 (F_dummy)
          ;;; (F_OTHER
          ;;; (F_Travel-mode) ;; for travel mode special rules
          ;;; (F_that) ;; for this and that determiners
          ;;; (F_this)
          ;;;)
          ;;; (F_SITUATION-RELATED
          (F::BE)
          ;;; (F_EVENT
          ;;;	(F_GO)
          ;;;	)
          (F::EVENTUALITY)
          )
 )||#

;;;)
(define-feature F::group
 ;;;(F_Any-group
 :values ((+)
          (-)
          )
 )

(define-feature F::kr-type
 :name-only t 
 )


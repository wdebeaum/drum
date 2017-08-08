(in-package :om)

; > starts a block
; < ends the block
; use `[code]` to use lisp syntax in code (optional, but makes eliminating commented code easier)


;;; ===== ONT::DOMAIN =====

;;; A domain is a single-valued function
(define-type ONT::DOMAIN
 :parent ONT::ABSTRACT-object
 :wordnet-sense-keys ("attribute%1:03:00")
 :comment "Nouns that name domain/scales, and can serve as relational nouns (e.g., the COLOR of the box)"
 :arguments ((:REQUIRED ONT::FIGURE)
	     (:optional ont::GROUND)
	     (:optional ont::EXTENT)
             )
 )

(define-type ONT::ORDERED-DOMAIN
; :sem (F::Abstr-obj (F::Scale ?!sc))
 :parent ONT::DOMAIN
 )

(define-type ONT::NON-MEASURE-ORDERED-DOMAIN
 :parent ONT::ORDERED-DOMAIN
 )

(define-type ONT::range
 :parent  ONT::NON-MEASURE-ORDERED-DOMAIN
 )

(define-type ont::status
  :parent ont::non-measure-ordered-domain
  :wordnet-sense-keys ("condition%1:26:00" "status%1:26:01" "state%1:26:02" "state_of_matter%1:26:00" "state%1:03:00")
  )

(define-type ont::sleepiness
  :parent ont::status
  :wordnet-sense-keys ("sleepiness%1:26:00" "drowsiness%1:26:00")
  )

;; comfort, discomfort
(define-type ONT::comfortableness
 :parent ONT::non-measure-ordered-domain
 :wordnet-sense-keys ("discomfort%1:26:00" "discomfort%1:12:00")
 )

;; security, privacy
(define-type ONT::confidentiality
    :wordnet-sense-keys ("privacy%1:07:00" "privacy%1:26:02")
    :parent  ONT::NON-MEASURE-ORDERED-DOMAIN
 )

;; protection, insurance
(define-type ONT::protection
 :parent  ONT::NON-MEASURE-ORDERED-DOMAIN
 )

;; severity, intensity
(define-type ONT::intensity
    :wordnet-sense-keys ("intensity%1:07:00" "intensity%1:07:03")
    :parent  ONT::NON-MEASURE-ORDERED-DOMAIN
 )

;; priority
(define-type ONT::importance
    :wordnet-sense-keys ("importance%1:26:00" "importance%1:07:00")
    :parent  ONT::NON-MEASURE-ORDERED-DOMAIN
    )

;; custom, habit, practice, tradition
(define-type ONT::practice
 :parent  ONT::NON-MEASURE-ORDERED-DOMAIN
 )

(define-type ONT::DISCRETE-DOMAIN
 :parent ONT::DOMAIN
 )

(define-type ONT::measure-domain
 :parent ONT::ORDERED-DOMAIN
 :sem (F::Abstr-obj (F::Measure-function F::term))
 :arguments ((:REQUIRED ONT::FIGURE)
             (:ESSENTIAL ONT::GROUND (F::Abstr-obj (F::Measure-function F::Value)))
             )
 )

;; cheap, (in)expensive, pricy
(define-type ONT::cost-val
 :parent  ONT::measure-domain
 :sem (F::Abstr-obj (F::Scale Ont::Money-scale))
 )

(define-type ONT::phys-measure-domain
 :parent ONT::MEASURE-DOMAIN
 :arguments ((:REQUIRED ONT::FIGURE (F::phys-obj (:default (f::form f::object) (f::container +))))
	     (:optional ont::extent))
 )

(define-type ONT::WEIGHT
 :wordnet-sense-keys ("weight%1:07:00" "heaviness%1:07:00" "weightiness%1:07:00")
 :parent ONT::PHYS-MEASURE-DOMAIN
 :sem (F::Abstr-obj (F::Scale Ont::Weight-scale))
 :arguments ((:ESSENTIAL ONT::GROUND (F::Abstr-obj (F::Scale Ont::Weight-scale)))
             )
 )

(define-type ONT::area
 :parent ONT::PHYS-MEASURE-DOMAIN
 :sem (F::Abstr-obj (F::Scale Ont::area-scale))
 :arguments ((:ESSENTIAL ONT::GROUND (F::Abstr-obj (F::Scale Ont::area-scale)))
             )
 )

;; what is linear-s and linear-d?
(define-type ONT::LINEAR-D
 :wordnet-sense-keys ("dimension%1:07:00")
  :parent ONT::PHYS-MEASURE-DOMAIN
 :sem (F::Abstr-obj (F::Scale Ont::Linear-scale))
 :arguments (;;(:ESSENTIAL ONT::val (F::Abstr-obj (F::Scale Ont::Linear-scale) (F::measure-function F::value)))
             (:ESSENTIAL ONT::EXTENT (F::abstr-obj (F::scale ont::linear-scale) (F::measure-function F::value))))
 )

;; length
(define-type ONT::length
;; :sem (F::Abstr-obj (F::Scale Ont::length))
 :parent ONT::linear-d
 )

;; width
(define-type ONT::width-scale
 :wordnet-sense-keys ("width%1:07:00" "diameter%1:07:00")
 :sem (F::Abstr-obj (F::Scale Ont::width-scale))
 :parent ONT::linear-d
 )

;; height
(define-type ONT::height-scale
    :sem (F::Abstr-obj (F::Scale Ont::height-scale))
 :parent ONT::linear-d
 )

;; depth
(define-type ONT::depth-scale
    :sem (F::Abstr-obj (F::Scale Ont::depth-scale))
 :parent ONT::linear-d
 )

;; thickness
(define-type ONT::thickness
 :sem (F::Abstr-obj (F::Scale Ont::thickness))
 :parent ONT::linear-d
 )

;; why isn't this a phys-measure-domain, like ont::weight?
;; Myrosia: because there are many more arguments, distance from A to B is ...
;; 2009 -- moving this under ont::linear-d
(define-type ONT::DISTANCE
 :parent ONT::linear-d
 ;; need this sem specification to get "a short/long distance"
;; :sem (f::abstr-obj (F::Scale Ont::distance))
 :arguments ((:REQUIRED ONT::neutral (F::phys-obj))
             (:OPTIONAL ONT::neutral1 (F::phys-obj))
	     (:OPTIONAL ONT::FIGURE (F::phys-obj))
             )
 )

;; bright(ness)
(define-type ONT::luminosity-val
 :wordnet-sense-keys ("brightness%1:07:00")
 :parent ONT::PHYS-MEASURE-DOMAIN
 :arguments ((:ESSENTIAL ONT::GROUND (F::Abstr-obj (F::Scale Ont::luminosity-scale)))
             )
 )

(define-type ONT::TEMPERATURE
 :wordnet-sense-keys ("temperature%1:07:00" "temperature%1:09:00")
 :parent ONT::PHYS-MEASURE-DOMAIN
 :sem (F::Abstr-obj (F::Scale ONT::Temperature-scale))
 :arguments ((:ESSENTIAL ONT::GROUND (F::Abstr-obj (F::Scale Ont::Temperature-scale)))
             )
 )

(define-type ONT::VOLUME
 :parent ONT::PHYS-MEASURE-DOMAIN
 :sem (F::Abstr-obj (F::Scale Ont::Volume-scale))
 :arguments ((:ESSENTIAL ONT::GROUND (F::Abstr-obj (F::Scale Ont::Volume-scale)))
             )
 )

(define-type ONT::humidity-scale
 :parent ONT::PHYS-MEASURE-DOMAIN
 :sem (F::Abstr-obj) ;;(F::Scale Ont::humidity-scale))
 :arguments ((:ESSENTIAL ONT::GROUND (F::Abstr-obj (F::Scale Ont::humidity-scale)))
             )
 )

;; beginning, end, threshhold
(define-type ONT::BOUND
     :wordnet-sense-keys ("extremity%1:15:00" "extreme%1:07:00" "extreme%1:15:00")
     :parent ONT::ORDERED-DOMAIN
     :arguments ((:REQUIRED ONT::FIGURE ((? fot F::phys-obj F::abstr-obj f::situation)))
		 (:ESSENTIAL ONT::GROUND)
		 )
     )

#|
(define-type ONT::END
 :parent ONT::BOUND
  :wordnet-sense-keys ("end%1:28:00" "end%1:15:01")
 )
|#

#|
(define-type ONT::BEGINNING
 :parent ONT::BOUND
  :wordnet-sense-keys ("beginning%1:28:00")
 )
|#

;; the resolution of an image
(define-type ONT::RESOLUTION
 :parent ONT::phys-measure-domain
 :arguments ((:ESSENTIAL ONT::GROUND (f::abstr-obj (f::scale ont::other-scale)))
             )
 )

; complex scales
(define-type ONT::RATE
; :parent ONT::MEASURE-DOMAIN
 :parent ONT::ORDERED-DOMAIN
  :sem (F::Abstr-obj (F::Scale Ont::Rate-scale))
 :arguments ((:REQUIRED ONT::FIGURE ((? fot F::phys-obj F::situation)))
             (:ESSENTIAL ONT::EXTENT (F::abstr-obj (F::measure-function F::value) (F::scale ont::rate-scale)))
;	     (:essential ont::FORMAL (F::SITUATION (f::type ont::event-of-change)))
             )
 )

(define-type ONT::DENSITY
 :wordnet-sense-keys ("density%1:07:00" "concentration%1:07:02" "concentration%1:07:03")
 :parent ONT::RATE
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj))
             )
 )

(define-type ONT::TIME-RATE
 :parent ONT::RATE
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj))
             )
 )

(define-type ONT::CLOCK-SPEED
 :parent ONT::TIME-RATE
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj (F::origin F::artifact)))
             )
 )

(define-type ONT::SPEED-SCALE
    :parent ONT::TIME-RATE
    )

(define-type ONT::Physical-discrete-domain
 :wordnet-sense-keys ("form%1:07:01" "shape%1:03:00" "form%1:03:00")
 :parent ONT::DISCRETE-DOMAIN
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj))
             )
 )

;; flavor, scent
(define-type ONT::sensory-property
 :parent ONT::PHYSICAL-DISCRETE-DOMAIN
 :sem (F::abstr-obj (F::scale ont::other-scale)) ;; why scale?
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj))
             )
 )

(define-type ONT::size
 :parent ONT::PHYSICAL-DISCRETE-DOMAIN
 ;; making it f::size, to match w/ small & large
 :sem (F::abstr-obj (F::scale ont::size-scale)) ; what scale should this be? weight? length? does it need a scale?
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj))
	     (:OPTIONAL ONT::EXTENT)
             )
 )

(define-type ONT::texture
 :parent ONT::PHYSICAL-DISCRETE-DOMAIN
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj))
             )
 )

(define-type ONT::gender
 :parent ONT::PHYSICAL-DISCRETE-DOMAIN
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj))
             )
 )

(define-type ONT::Color-scale
 :wordnet-sense-keys ("colouring%1:07:00" "coloring%1:07:00" "colour%1:07:00" "color%1:07:00" "color%1:09:01" "colour%1:09:01")
 :parent ONT::physical-discrete-domain
  :sem (F::abstr-obj (F::scale ont::color-scale))
 :arguments ((:REQUIRED ONT::GROUND (F::abstr-obj (F::scale ont::color-scale))) ;?? what's this? the car's color of red?
             )
 )

;; confidence, authority, trust
(define-type ont::assurance
    :wordnet-sense-keys ("trust%1:26:00")
    :parent ont::non-measure-ordered-domain
  )

;; gist, essence, substance
(define-type ont::gist
 :wordnet-sense-keys ("kernel%1:09:00" "substance%1:09:01" "core%1:09:00" "center%1:09:00" "centre%1:09:00" "essence%1:09:00" "gist%1:09:00" "heart%1:09:01" "heart_and_soul%1:09:00" "inwardness%1:09:02" "marrow%1:09:00" "meat%1:09:00" "nub%1:09:00" "pith%1:09:00" "sum%1:09:00" "nitty-gritty%1:09:00")
  :parent ont::non-measure-ordered-domain
  )

(define-type ONT::POPULATION
 :parent ONT::ORDERED-DOMAIN
 :sem (F::Abstr-obj (F::Measure-function F::term))
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj (F::form F::geographical-object)))
             )
 )

(define-type ONT::BUDGET
 :parent ONT::MEASURE-DOMAIN
 :sem (F::Abstr-obj (f::scale ont::money-scale))
 :arguments (;; only projects have budgets -- the budgets for people and objects are more general relations (JFA 9/1)
	     (:essential ONT::FIGURE (f::abstr-obj (f::information f::information-content)))
             ;;; a project's budget of 5 dollars
             (:ESSENTIAL ONT::GROUND (F::Abstr-obj (F::Scale Ont::money-scale)))
             )
 )

;; surplus, excess
(define-type ONT::surplus
 :parent ONT::ORDERED-DOMAIN
 :arguments ((:REQUIRED ONT::FIGURE)
             )
 )

;; spec, requirement
(define-type ONT::REQUIREMENTS
 :parent ONT::ORDERED-DOMAIN
 :arguments ((:REQUIRED ONT::FIGURE)
             )
 )

;; resource
(define-type ONT::resource
 :parent ONT::requirements
 )

#|
(define-type ONT::RESPONSIBILITY
 :parent ONT::ORDERED-DOMAIN
 :arguments ((:OPTIONAL ONT::FIGURE)
             )
 )
|#

(define-type ont::voltage
 :wordnet-sense-keys ("voltage%1:19:02" "electromotive_force%1:19:00" "emf%1:19:00")
    :parent ont::phys-measure-domain)

(define-type ONT::age
    :parent ONT::PHYS-MEASURE-DOMAIN
    :sem (F::Abstr-obj (F::measure-function F::term))
 )

(define-type ONT::likelihood
 :parent ONT::ORDERED-DOMAIN
 :sem (F::Abstr-obj (F::measure-function F::term))
 :arguments ((:REQUIRED ONT::FIGURE (f::situation))
             )
 )

;; hearing, sight, vision
(define-type ONT::physical-sense
 :parent ONT::non-measure-ordered-domain
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj (F::container -) (F::form F::solid-object) (F::origin F::human)))
             )
 )

(define-type ONT::inexpensive
 :parent ONT::COST-VAL
 ; Words: (W::CHEAP W::INEXPENSIVE)
:wordnet-sense-keys ("cheap%3:00:00" "cheap%3:00:00")
 ; Antonym: ONT::expensive (W::EXPENSIVE W::PRICEY W::PRICY)
 )

(define-type ONT::expensive
 :parent ONT::COST-VAL
 ; Words: (W::EXPENSIVE W::PRICEY W::PRICY)
 :wordnet-sense-keys ("costly%5:00:01:expensive:00" "expensive%3:00:00" "expensive%3:00:00")
 ; Antonym: ONT::inexpensive (W::CHEAP W::INEXPENSIVE)
 )

(define-type ont::rate-scale
  :parent ont::measure-domain
  )

(define-type ont::ratio-scale
  :parent ont::measure-domain
  )

(define-type ont::percent-scale
    :wordnet-sense-keys ("percentage%1:24:00" )
    :parent ont::measure-domain)



;;;==== ONT::ATTRIBUTE ====

(define-type ONT::attribute
 :wordnet-sense-keys ("dimension%1:09:00" "attribute%1:09:00" "property%1:09:00" "property%1:07:00" "holding%1:21:00" "belongings%1:21:00" "property%1:21:00")
 :parent ont::abstract-object-nontemporal
 :arguments ((:OPTIONAL ONT::FIGURE ((? lo f::phys-obj f::abstr-obj)))
             )
 )

(define-type ONT::body-property
 :parent ont::attribute
 :arguments ((:OPTIONAL ONT::FIGURE (f::phys-obj (f::origin f::living)))
             )
 )
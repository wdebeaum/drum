(in-package :om)

; > starts a block
; < ends the block
; use `[code]` to use lisp syntax in code (optional, but makes eliminating commented code easier)

;;; A special type for pronouns etc that can refer to arbitrary abstract objects
;;; declares all features as arbibtary vars to override default - features
(define-type ONT::FACT
 :parent ONT::ABSTRACT-OBJECT-nontemporal
 :arguments ((:optional ONT::formal)
	     )
 )

(define-type ONT::KIND
 :parent ONT::ABSTRACT-OBJECT-nontemporal
 :sem (F::abstr-obj (F::SCALE -))
 :arguments ((:REQUIRED ONT::FIGURE)
             )
 )

;; some kind of replication of the same thing
;; version, edition, variant
(define-type ONT::Version
 :parent ONT::KIND
 )

;; example, illustration, instance
(define-type ONT::example
 :parent ONT::kind
 )

(define-type ONT::representation
 :parent ONT::mental-construction
 :arguments ((:OPTIONAL ONT::FIGURE) ;(f::situation (f::information f::mental-construct) (f::cause f::mental)))
		)
 )

(define-type ONT::grouping
    :comment "a  classification, category, variety of things. Not a set of objects!"
    :parent ONT::version
    )

(define-type ONT::FUNCTION-OBJECT
; :parent ONT::domain-property
 :parent ONT::abstract-object-nontemporal
 :sem (F::Abstr-obj)
 )

;; purpose, function
(define-type ont::utility
 :parent ont::function-object
 :wordnet-sense-keys ("utility%1:07:00")
  :arguments ((:REQUIRED ONT::FIGURE)
             )
 )

;; the relation type for comparatives
;; more, better
(define-type ONT::More-val
    :parent ONT::domain-property
    :sem (F::ABSTR-OBJ (:required (F::CONTAINER -) (F::INFORMATION -) (F::INTENTIONAL -))
		       (:default (F::GRADABILITY +) (F::scale ?!sc)))
    )
;; less, worse
(define-type ONT::less-val
    :parent ONT::domain-property
    :sem (F::ABSTR-OBJ (:required (F::CONTAINER -) (F::INFORMATION -) (F::INTENTIONAL -))
		       (:default (F::GRADABILITY +) (F::scale ?!sc)))
    )

;; and for superlatives
;; best, most
(define-type ONT::MAX-val
  :parent ONT::domain-property
  :sem (F::ABSTR-OBJ (:required (F::CONTAINER -) (F::INFORMATION -) (F::INTENTIONAL -))
		      (:default (F::GRADABILITY +) (F::scale ?!sc)))
  :arguments ((:REQUIRED ONT::FIGURE)
	      (:REQUIRED ONT::GROUND)))

;; worst, least
(define-type ONT::MIN-val
  :parent ONT::domain-property
  :sem (F::ABSTR-OBJ (:required (F::CONTAINER -) (F::INFORMATION -) (F::INTENTIONAL -))
		      (:default (F::GRADABILITY +) (F::scale ?!sc)))
  )

;; as hot as it can be
(define-type ONT::as-much-as
  :parent ONT::domain-property
  :sem (F::ABSTR-OBJ (:required (F::CONTAINER -) (F::INFORMATION -) (F::INTENTIONAL -))
		      (:default (F::GRADABILITY +) (F::scale ?!sc)))
  )

(define-type ONT::TOO-MUCH
  :parent ONT::domain-property
  :sem (F::ABSTR-OBJ (:required (F::CONTAINER -) (F::INFORMATION -) (F::INTENTIONAL -))
		      (:default (F::GRADABILITY +) (F::scale ?!sc)))
  )

;; so hungry I could cry
(define-type ONT::SO-MUCH-THAT
  :parent ONT::domain-property
  :sem (F::ABSTR-OBJ (:required (F::CONTAINER -) (F::INFORMATION -) (F::INTENTIONAL -))
		      (:default (F::GRADABILITY +) (F::scale ?!sc)))
  )

;; for adjective scale values to be translated properly to the akrl, the intensity values (hi, med, lo) need to be defined in the ontology
(define-type ONT::hi
  :parent ONT::max-val
 )

(define-type ONT::med
  :parent ONT::max-val ; ??
 )

(define-type ONT::lo
  :parent ONT::min-val
 )

(define-type ont::group-object-abstr
 :wordnet-sense-keys ("mathematical_group%1:09:00" "group%1:09:00" "chemical_group%1:27:00" "radical%1:27:00" "group%1:27:00" "group%1:03:00" "grouping%1:03:00")
  :parent ont::abstract-object-nontemporal
;  :sem (F::Abstr-obj (f::group +)) ; group feature not defined for abstract objects
  :arguments ((:OPTIONAL ONT::FIGURE)
              )
  )

(define-type ONT::system-abstr
  :wordnet-sense-keys ("system%1:06:00" "system%1:14:00")
  :comment "An interconnected group of objects, abstract or physical"
 :parent ONT::group-object-abstr
 )

(define-type ONT::formation-abstr
 :parent ONT::group-object-abstr
 )

(define-type ONT::row-formation-abstr
 :wordnet-sense-keys ("row%1:14:00" "row%1:17:00")
 :parent ONT::formation-abstr
 :arguments ((:OPTIONAL ONT::FIGURE (F::phys-obj))  ; to distinguish between steps as steps in a plan and steps in a staircase
             )
 )

(define-type ONT::column-formation-abstr
 :wordnet-sense-keys ("pile%1:14:00" "column%1:14:00" "column%1:25:02")
 :parent ONT::formation-abstr
 :arguments ((:OPTIONAL ONT::FIGURE (F::phys-obj))  ; to distinguish between steps as steps in a plan and steps in a staircase
             )
 )

;; crowd, audience
(define-type ont::social-group-abstr
 :wordnet-sense-keys ("social_group%1:14:00")
  :parent ont::group-object-abstr
  :sem (F::Abstr-obj (F::information F::information-content) (f::intentional +) (F::Object-Function F::Occupation) (F::Container -))
  :arguments ((:OPTIONAL ONT::FIGURE ((? lof f::phys-obj f::abstr-obj))))
  )

;; swift 20110928 crew defined for obtw demo
(define-type ont::crew-abstr
    :parent ont::social-group-abstr
    )

(define-type ONT::organization-abstr
 :wordnet-sense-keys ("organization%1:14:00" "organisation%1:14:00")
 :parent ONT::social-group-abstr
 )

;; these subtypes came about because of generation issues
;; commerce, finance, business, marketing
(define-type ONT::enterprise-abstr
 :parent ONT::organization-abstr
 )

;; institution
(define-type ONT::institution-abstr
 :parent ONT::organization-abstr
 )

;; an institution created for conduction business
;; company
(define-type ONT::company-abstr
 :parent ONT::institution-abstr
 )

;; google, amazon, isp
(define-type ONT::internet-organization-abstr
 :parent ONT::organization-abstr
 )

;; bank
(define-type ONT::financial-institution-abstr
 :parent ONT::institution-abstr
 )

;; apple, ibm, hp
(define-type ONT::electronics-company-abstr
 :parent ONT::company-abstr
 )

;; officemax, officedepot
(define-type ONT::office-supply-company-abstr
 :parent ONT::company-abstr
 )

;; fetch, gnu
(define-type ONT::software-company-abstr
 :parent ONT::company-abstr
 )

;; court
(define-type ONT::legal-organization-abstr
 :parent ONT::organization-abstr
 )

;; market
(define-type ONT::financial-organization-abstr
 :parent ONT::organization-abstr
 )

;; government, gsa, darpa
(define-type ONT::federal-organization-abstr
 :wordnet-sense-keys ("government%1:14:00" "authorities%1:14:00" "regime%1:14:00")
 :parent ONT::organization-abstr
 )

;; ieee
(define-type ONT::professional-organization-abstr
 :parent ONT::organization-abstr
 )

;; ansi
(define-type ONT::regulatory-organization-abstr
 :parent ONT::organization-abstr
 )

(define-type ONT::airline-abstr
 :parent ONT::enterprise-abstr
 )

;; affiliate, partner, subsidiary
(define-type ONT::affiliate-abstr
 :parent ONT::company-abstr
 )

;; affiliate, partner, subsidiary
(define-type ONT::supplier-abstr
 :parent ONT::company-abstr
 )

;; sri
(define-type ONT::research-institution-abstr
 :parent ONT::company-abstr
 )

;; university, college
(define-type ONT::academic-institution-abstr
    :parent ONT::research-institution-abstr
 )

;; fedex, ups
(define-type ONT::shipping-company-abstr
 :parent ONT::company-abstr
 )

(define-type ONT::military-group-abstr
 :wordnet-sense-keys ("military_unit%1:14:00" "military_force%1:14:00" "military_group%1:14:00" "force%1:14:01")
 :parent ONT::social-group-abstr
 )

(define-type ONT::collection-abstr
 :wordnet-sense-keys ("collection%1:14:00" "aggregation%1:14:00" "accumulation%1:14:00" "assemblage%1:14:01")
 :parent ONT::group-object-abstr
 )

(define-type ONT::sequence-abstr
 :wordnet-sense-keys ("ordering%1:14:00" "order%1:14:00" "ordination%1:14:00")
 :parent ONT::group-object-abstr
 )

(define-type ONT::linear-grouping-abstr
 :wordnet-sense-keys ("line%1:14:01")
 :parent ONT::sequence-abstr
 )

(define-type ONT::combination-abstr
 :wordnet-sense-keys ("combination%1:14:00")
 :parent ONT::group-object-abstr
 )

;;; This is a catch-all for things that are relations between multiple
;;; objects: identity, distance, whatever. Will need better sorting at
;;; a future data
(define-type ONT::relation
 :wordnet-sense-keys ("relation%1:03:00" "amount%2:42:03" "bear_on%2:42:00")
 :parent ONT::abstract-object
 :arguments ((:REQUIRED ONT::FIGURE)
	     (:REQUIRED ONT::GROUND)
	     (:optional ont::neutral)
	     (:optional ont::neutral1))  ;; some relations based on verbs use this
 :sem (F::abstr-obj (:required)
		    (:default (f::intensity ont::hi)))
 )

;; own: his own truck
(define-type ONT::own
  :parent ONT::relation
  )

(define-type ONT::SIMILARITY-val
 :parent ONT::RELATION
 :arguments ((:ESSENTIAL ONT::neutral)
	     (:ESSENTIAL ONT::neutral1)
	     (:ESSENTIAL ONT::FIGURE)
	     (:optional ont::formal)
	     (:optional ont::GROUND) ;; for backwards compat
	                  )
 )

;; such as, as in. These should have the same representation as produced by the grammar rule such-X-as-Y>
;; using roles :of and :val b.c. the :formal :formal1 roles are going to be replaced real soon now
(define-type ont::exemplifies
  :parent ont::similarity-val
    )

(define-type ONT::IDENTITY-val
 :parent ONT::similarity-val
  )

#||;;; ownership, property, etc.
(define-type ONT::possession
 :wordnet-sense-keys ("possession%1:03:00")
 :parent ONT::RELATION
 :arguments ((:ESSENTIAL ONT::neutral (F::phys-obj (F::origin F::human) (F::intentional +)))
             (:ESSENTIAL ONT::neutral1 ((? cth f::phys-obj f::abstr-obj)))
             )
 )||#

;; how does this relate to ont::truth-val??
;;; e.g., wrong, problematic, right,
;; the wrong day, the right time, the right number
(define-type ONT::EVALUATION-VAL
 :parent ONT::RELATION
 :arguments ((:ESSENTIAL ONT::neutral ((? tp f::time f::abstr-obj F::phys-obj F::situation)))
	     (:OPTIONAL  ONT::neutral1 ((? tp1 f::time f::abstr-obj F::phys-obj F::situation)))
             )
 )

#|
;;; Function terms have one or more arguments and have a value
(define-type ONT::abstract-function
 :parent ONT::ABSTRACT-OBJECT
 :sem (F::ABSTR-OBJ (:required (F::CONTAINER -) (F::INTENTIONAL -) (F::measure-function
                  F::term))(:default (F::GRADABILITY -) (f::information -)))
 :arguments ((:ESSENTIAL ONT::val (F::abstr-obj (:default (F::measure-function F::value))))
             )
 )
|#

(define-type ONT::Mathematical-term
    :parent ONT::abstract-object-nontemporal
    :sem (f::abstr-obj (:required (f::gradability -)) (:default (f::information f::data)) )
    :arguments ((:ESSENTIAL ONT::FIGURE (f::abstr-obj (F::measure-function f::term)))
		(:ESSENTIAL ONT::GROUND (F::abstr-obj (:default (F::measure-function F::value))))))

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

(define-type ont::number
;  :parent ont::ordered-domain
  :parent ONT::MATHEMATICAL-TERM
  :sem (F::abstr-obj ;;(F::measure-function F::value)
       (F::CONTAINER -) (F::INFORMATION f::information-content) (F::INTENTIONAL -)
       )
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

;; layer (of ozone, chocolate), sheet (of ice, paper), slice
(define-type ont::sheet-abstr
;  :parent ont::non-measure-ordered-domain
  :parent ONT::GROUP-OBJECT-abstr
  )

(define-type ONT::MEASURE-UNIT
 :wordnet-sense-keys ("unit_of_measurement%1:23:00" "unit%1:23:00")
 :parent ONT::unit
 :sem (F::abstr-obj (F::measure-function F::value) (F::CONTAINER -) (F::INFORMATION -)
       (F::INTENTIONAL -))
 ;;; We define an argument here because we want to express selectional restrictions on what this unit can measure
 :arguments ((:ESSENTIAL ONT::FIGURE)
             )
 )

(define-type ONT::formal-unit
 :parent ONT::measure-unit
 )

;; a number/amount/quantity of X
(define-type ONT::QUANTITY-abstr
 :wordnet-sense-keys ("measure%1:03:00" "quantity%1:03:00" "amount%1:03:00")
; :parent ONT::DOMAIN-PROPERTY
 :parent ONT::GROUP-OBJECT-abstr
 :arguments ((:ESSENTIAL ONT::FIGURE)
             )
 )

(define-type ONT::tangible-unit
 :parent ONT::MEASURE-UNIT
 :arguments ((:ESSENTIAL ONT::FIGURE (F::phys-obj))
             )
 )

;; 5 seconds, minutes, hours, days, weeks, months, years...
(define-type ont::time-unit
 :wordnet-sense-keys ("time_unit%1:28:00" "unit_of_time%1:28:00")
  :parent ont::measure-unit
  :sem (F::abstr-obj (F::Scale Ont::duration-scale))
  :arguments ((:ESSENTIAL ONT::FIGURE ((? t f::situation F::abstr-obj)))
             )
  )

(define-type ont::Hour-duration
    :parent ont::time-unit)

(define-type ont::minute-duration
    :parent ont::time-unit)

(define-type ont::day-duration
    :parent ont::time-unit)

(define-type ont::year-duration
    :parent ont::time-unit)

(define-type ont::week-duration
    :parent ont::time-unit)

(define-type ont::second-duration
    :parent ont::time-unit)

(define-type ont::power-unit
 :wordnet-sense-keys ("power_unit%1:23:00" "electromagnetic_unit%1:23:00" "emu%1:23:00")
    :parent ont::formal-unit
    ;; Right now items like electricity are tagged as substance, so for now we assume that power units measure them
    :arguments ((:essential ont::FIGURE (f::phys-obj (f::form f::substance))))
    )

(define-type ont::energy-unit
 :wordnet-sense-keys ("energy_unit%1:23:00")
    :parent ont::formal-unit
    ;; Right now items like electricity are tagged as substance, so for now we assume that power units measure them
    :arguments ((:essential ont::FIGURE (f::phys-obj (f::form f::substance))))
    )

;; lumen
(define-type ont::luminosity-unit
 :wordnet-sense-keys ("light_unit%1:23:00" "luminous_flux_unit%1:23:00")
    :parent ont::formal-unit
    :sem (F::abstr-obj (F::SCALE ONT::luminosity-scale))
    :arguments ((:essential ont::FIGURE (f::phys-obj)))
    )

(define-type ONT::DISCRETE-DOMAIN
 :parent ONT::DOMAIN
 )

;; dozen, hundred, thousand...
(define-type ONT::NUMBER-UNIT
 :parent ONT::unit
 :comment "words that name measurement units in scales: foot, mile, ..."
 :sem (F::Abstr-obj (F::information F::data))
 :arguments ((:ESSENTIAL ONT::FIGURE))
 )

(define-type ONT::WEIGHT-UNIT
 :wordnet-sense-keys ("mass_unit%1:23:00" "weight_unit%1:23:00" "weight%1:23:00" "gram%1:23:00")
 :parent ONT::tangible-unit
 :sem (F::abstr-obj (F::Scale Ont::WEIGHT-scale))
 ;; 5 ounces of water
 :arguments ((:ESSENTIAL ONT::FIGURE (F::phys-obj))) ;;(F::FORM F::solid))))
 )

(define-type ONT::acoustic-UNIT
 :wordnet-sense-keys ("sound_unit%1:23:00")
 :parent ONT::formal-UNIT
 :sem (F::abstr-obj (F::Scale Ont::sound-scale))
 )

(define-type ONT::LENGTH-UNIT
 :wordnet-sense-keys ("linear_measure%1:23:00" "linear_unit%1:23:00" "week%1:28:00" "hebdomad%1:28:00")
 :parent ONT::tangible-unit
 :sem (F::Abstr-obj (F::Scale ONT::LINEAR-D)) ; Ont::length))  ; e.g., km: not just length but could also be width, height, etc
 )

;; acre, sqare feet
(define-type ONT::AREA-UNIT
 :wordnet-sense-keys ("area_unit%1:23:00" "square_measure%1:23:00")
 :parent ONT::tangible-unit
 :sem (F::Abstr-obj (F::Scale Ont::area-scale))
 )

(define-type ONT::Volume-UNIT
 :wordnet-sense-keys ("volume_unit%1:23:00" "capacity_unit%1:23:00" "capacity_measure%1:23:00" "cubage_unit%1:23:00" "cubic_measure%1:23:00" "cubic_content_unit%1:23:00" "displacement_unit%1:23:00" "cubature_unit%1:23:00")
 :parent ONT::tangible-unit
 :sem (F::Abstr-obj (F::Scale Ont::Volume-scale))
 :arguments ((:ESSENTIAL ONT::FIGURE (F::phys-obj (F::FORM F::substance))))
 )

(define-type ONT::CONTAINER-LOAD
 :parent ONT::VOLUME-UNIT
 )

;; cup, teaspoon, etc.
(define-type ont::volume-measure-unit
 :wordnet-sense-keys ("liquid_unit%1:23:00" "liquid_measure%1:23:00")
 :parent ont::volume-unit
 :arguments ((:essential ont::FIGURE (f::phys-obj (f::form f::substance))))
 )

;; portion, serving
(define-type ont::food-measure-unit
 :parent ont::measure-unit
 :arguments ((:essential ont::FIGURE (f::phys-obj (f::form f::substance))))
 )

(define-type ONT::dose
 :wordnet-sense-keys ("dose%1:06:00" "dosage%1:06:00")
 :parent ONT::volume-unit
 )

;; pill, tablet
(define-type ont::substance-delivery-unit
 :wordnet-sense-keys ("pill%1:06:00")
    :parent ont::dose
    :arguments ((:essential ont::FIGURE (f::phys-obj (f::form f::substance))))
    )

(define-type ONT::rate-unit
 :wordnet-sense-keys ("rate%1:28:00")
 :parent ONT::formal-unit
 :sem (F::Abstr-obj (F::Scale Ont::Rate-scale))
 :arguments ((:ESSENTIAL ONT::FIGURE ((? type F::phys-obj F::situation)))
             )
 )

(define-type ONT::speed-unit
 :wordnet-sense-keys ("mph%1:28:01" "miles_per_hour%1:28:01" "kilometers_per_hour%1:28:00" "kilometres_per_hour%1:28:00" "kph%1:28:00" "km/h%1:28:00")
 :parent ONT::rate-unit
 )

;; bit, byte
(define-type ONT::memory-UNIT
 :wordnet-sense-keys ("computer_memory_unit%1:23:00")
 :parent ONT::formal-UNIT
 :sem (F::Abstr-obj (F::Scale Ont::Other-scale))
 :arguments ((:ESSENTIAL ONT::FIGURE (F::phys-obj (f::origin f::artifact)))
             )
 )

(define-type ONT::Temperature-UNIT
 :wordnet-sense-keys ("temperature_unit%1:23:00")
 :parent ONT::formal-UNIT
 :sem (F::Abstr-obj (F::Scale Ont::Temperature-scale))
 :arguments ((:ESSENTIAL ONT::FIGURE ((? type F::phys-obj F::situation)))
             )
 )

(define-type ONT::angle-UNIT
 :wordnet-sense-keys ("angular_unit%1:23:00")
 :parent ONT::formal-UNIT
 :sem (F::Abstr-obj (F::Scale Ont::Linear-scale))
 :arguments ((:ESSENTIAL ONT::FIGURE (F::situation (f::trajectory +)))
             )
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

(define-type ONT::number-measure-domain
 ;; :parent ONT::MEASURE-DOMAIN
 :parent ONT::MATHEMATICAL-TERM
 :arguments ((:REQUIRED ONT::FIGURE (F::Abstr-obj (F::Measure-function F::Term)))
             )
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

(define-type ONT::LEVEL
  :comment "words that act as predicates that return the value on a scale/domain: What is the X on this scale?  Note: We exclude words that are identical to the names of the scales they pertain to (e.g., What is the height on the height scale?)"
 :wordnet-sense-keys ("level%1:26:00")
 :sem (F::Abstr-obj (F::Scale Ont::LINEAR-SCALE))
; :parent ONT::ordered-DOMAIN
 :parent ONT::ABSTRACT-OBJECT
 :arguments ((:ESSENTIAL ONT::FIGURE ((? of f::phys-obj F::Abstr-obj))) ;; noise, water
	     (:essential ont::GROUND (f::abstr-obj   (F::INFORMATION F::INFORMATION-CONTENT)))
             )
 )

;; ratio, proportion, percent(age)
(define-type ont::quantitative-relation
 :wordnet-sense-keys ("magnitude_relation%1:24:00" "quantitative_relation%1:24:00")
; :parent ONT::QUANTITY
 :parent ONT::MATHEMATICAL-TERM
 :arguments ((:REQUIRED ONT::FIGURE)
	     (:optional ont::formal)
	     ))

;; percent
(define-type ONT::multiple
 :parent ONT::MATHEMATICAL-TERM
 :sem (F::Abstr-obj )
 )

;; percent
(define-type ONT::percent
 :wordnet-sense-keys ("percent%1:24:00" "per_centum%1:24:00" "pct%1:24:00")
; :parent ONT::quantitative-relation
 :parent ONT::FORMAL-UNIT
 :sem (F::Abstr-obj (F::Scale Ont::percent-scale))
 )

(define-type ONT::ratio
 :wordnet-sense-keys ("ratio%1:24:01" "proportion%1:24:00" "ratio%1:24:00")
 :parent ONT::quantitative-relation
 :arguments ((:REQUIRED ONT::FIGURE1))
 :sem (F::Abstr-obj (F::Scale Ont::ratio-scale))
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

(define-type ONT::ASSETS
 :wordnet-sense-keys ("assets%1:21:00")
; :parent ONT::MEASURE-DOMAIN
 :parent ONT::FUNCTION-OBJECT
 :sem (F::Abstr-obj (F::Scale Ont::money-scale))
 :arguments ((:REQUIRED ONT::FIGURE ((? fot F::phys-obj F::situation)))
             (:ESSENTIAL ONT::GROUND (F::abstr-obj (F::measure-function F::value) (F::scale ont::money-scale)))
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

;; dollars -- need a unit definition for '5 dollars'
(define-type ONT::MONEY-UNIT
 :wordnet-sense-keys ("monetary_unit%1:23:00")
 :parent ONT::formal-UNIT
 :sem (F::Abstr-obj (F::Scale Ont::Money-scale))
 )

;; currency
(define-type ONT::currency
 :parent ONT::FUNCTION-OBJECT
 :sem (F::Abstr-obj (f::scale ont::money-scale))
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

(define-type ONT::source
 :parent ONT::function-OBJECT
 :sem (F::Abstr-obj)
 :arguments ((:essential ONT::FIGURE)
	     )
 )

(define-type ONT::prize
 :parent ONT::function-OBJECT
 :sem (F::Abstr-obj)
 :arguments ((:essential ONT::FIGURE)
	     )
 )

(define-type ONT::discipline
  :wordnet-sense-keys ("subject%1:09:00::" "subject_area%1:09:00::" "subject_field%1:09:00::" "field%1:09:00::" "field_of_study%1:09:00::" "study%1:09:02::" "technology%1:09:00")
  :parent ONT::function-OBJECT
  :sem (F::Abstr-obj)
  :arguments ((:essential ONT::FIGURE)
	      )
 )

(define-type ONT::information-function-object
 :parent ONT::FUNCTION-OBJECT
 :wordnet-sense-keys ("communication%1:03:00" "criminal_record%1:10:00" )
 :sem (F::Abstr-obj (F::information F::information-content) (F::intentional -) (F::container +))
 :arguments (
;	     (:optional ONT::Associated-information)
	     )
 )

(define-type ONT::message
     :wordnet-sense-keys ("message%1:10:01")
     :parent ont::information-function-object
     :arguments ((:optional ONT::formal (F::prop)))
	     
    )

(define-type ONT::composition
  :comment "composition, e.g., result of event-of-creation"
  :wordnet-sense-keys ("composition%1:07:01" "composition%1:07:02" "composition%1:04:01")
 :parent ONT::information-function-object
 )

;; information
(define-type ONT::information
 :wordnet-sense-keys ("information%1:10:00" "info%1:10:00" "indication%1:10:00")
 :parent ONT::information-function-object
 )

;; create an ont::communication-object
;; subject, topic
(define-type ONT::content
 :wordnet-sense-keys ("content%1:14:00" "substance%1:10:00" "subject_matter%1:10:00" "content%1:10:00" "message%1:10:00" "meaning%1:09:00" "substance%1:09:00")
 :parent ONT::information
 )

;; success, failure
(define-type ONT::outcome
 :parent ONT::information-function-object
 :arguments ((:essential ONT::FIGURE)
	     )
 )

(define-type ONT::result
 :parent ONT::outcome
 :arguments ((:essential ONT::FIGURE)
	     )
 )

(define-type ONT::clinical-finding
 :parent ONT::result
 :arguments ((:essential ONT::FIGURE)
	     )
 )

;; identification
(define-type ONT::identification
 :parent ONT::information-FUNCTION-OBJECT
  :arguments ((:essential ONT::FIGURE ((? lof F::Phys-obj f::abstr-obj)))
	     )
 )

;; pin, isbn
(define-type ont::id-number
  :parent ont::identification)

;; ssn
(define-type ONT::ssn
 :parent ONT::id-number
  :arguments ((:essential ONT::FIGURE (F::Phys-obj)))
 )

;; username, login
(define-type ONT::username
 :parent ONT::identification
  :arguments ((:essential ONT::FIGURE ((? lof F::Phys-obj f::abstr-obj)))
	     )
 )

;; password
(define-type ONT::password
 :wordnet-sense-keys ("password%1:10:00" "watchword%1:10:00" "word%1:10:05" "parole%1:10:01" "countersign%1:10:01")
 :parent ONT::identification
 )

;; address
(define-type ONT::location-id
 :wordnet-sense-keys ("address%1:15:00")
 :parent ONT::identification
 :sem (F::Abstr-obj (F::object-function f::spatial-object))
 :arguments ((:essential ONT::FIGURE (F::Phys-obj)))
 )

(define-type ont::zipcode
  :parent ont::location-id
  )

;; baseline, guideline
(define-type ont::standard
 :parent ont::information-function-object
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

(define-type  ONT::event-defined-by-activity
 :wordnet-sense-keys ("event%1:03:00" "time_period%1:28:00" "period_of_time%1:28:00" "period%1:28:00")
 :parent ONT::EVENT-TYPE
 :sem (F::Situation (F::aspect F::dynamic))
 :arguments ((:OPTIONAL ONT::FIGURE)
;	     (:optional ont::content)
             )
 )

(define-type ONT::caused-event
 :parent ONT::EVENT-TYPE
 :sem (F::situation (F::cause (? cause F::agentive F::force)))
 )

;;; The difference between actions and events is that actions have agents
;;; not used JFA except as LF for word ACTION
(define-type ONT::Action
 :parent ONT::EVENT-TYPE
 :sem (F::Situation (F::cause F::Agentive) (F::aspect F::dynamic))
 :arguments ((:OPTIONAL ONT::FIGURE)
             )
 )

(define-type ONT::commercial-activity
 :wordnet-sense-keys ("finance%1:04:00" "commerce%1:04:00" "base%1:06:06")
 :parent ONT::activity
 :sem (F::situation (F::cause (? cause F::agentive F::force)))
 )

(define-type ONT::Situation
 :wordnet-sense-keys ("phenomenon%1:03:00")
 :parent ONT::EVENT-TYPE
 :sem (F::Situation)
 )

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

(define-type ONT::trouble
 :wordnet-sense-keys ("impairment%1:11:00" "harm%1:11:01" "damage%1:11:00" "problem%1:09:00" "trouble%1:09:00" "trouble%1:11:00")
 :parent ont::situation
 :arguments ((:OPTIONAL ONT::assoc-with)
             )
 )

;; tour
; can tour a house, a museum; doesn't have to be travel
(define-type ont::tour
  :parent ont::event-defined-by-activity
  )

;; travel
(define-type ONT::travel
 :parent ONT::event-defined-by-activity
 :sem (F::situation (F::trajectory +))
 )

;; trip, journey
(define-type ONT::trip
 :wordnet-sense-keys ("journey%1:04:00" "journeying%1:04:00")
 :parent ONT::travel
 :sem (F::situation (F::trajectory +))
 )

;; jaunt
(define-type ont::jaunt
  :parent ont::trip
  )

;; round trip
(define-type ONT::round-trip
 :parent ONT::trip
 :sem (F::situation (F::trajectory +))
 )

(define-type ont::air-travel
  :parent ont::trip
  )

;; flight
(define-type ONT::flight
 :parent ONT::air-travel
 )

(define-type ont::water-travel
  :parent ont::trip
  )

(define-type ont::land-travel
  :parent ont::trip
  )

;; voyage
(define-type ont::voyage
  :parent ont::water-travel
  )

;; drive
(define-type ont::driving-trip
  :parent ont::water-travel
  )

;; migration
(define-type ont::migration
  :parent ont::trip
  )

(define-type ont::located-event
  :parent ont::event-type
  :sem (f::situation (f::trajectory -) (f::locative f::located))
  :comment "events that are located in time/space - e.g., riot, war, ..."
   :arguments ((:optional ont::formal)
	       (:optional ont::neutral)
	       )
   )

;; meeting, party, conference
(define-type ont::gathering-event
  :parent ont::located-event
  )

;; 20111005 fire type added for obtw demo
(define-type ont::fire
  :parent ont::located-event
  )

(define-type ont::ceremony
  :parent ont::gathering-event
  )

(define-type ont::performance-play
  :parent ont::gathering-event
  )

;; class, course
(define-type ont::instruction-event
  :parent ont::located-event
  )

;; talk, lecture, demo, presentation
(define-type ont::presentation
  :parent ont::gathering-event
  )

;; idea
(define-type ONT::mental-object
 :wordnet-sense-keys ("cognition%1:03:00" "noesis%1:03:00" "grounds%1:10:00" "reason%1:10:00")
 :parent ONT::mental-construction
;; :sem (F::Abstr-obj (F::container +))
 :arguments ((:OPTIONAL ONT::FIGURE) ;(f::situation (f::information f::mental-construct) (f::cause f::mental)))
	     (:optional ont::GROUND)
	     (:optional ont::FORMAL (f::situation))
             )
 )

(define-type ONT::knowledge-belief
    :wordnet-sense-keys ("knowledge%1:03:00")
    :parent ONT::mental-construction
    :arguments ((:OPTIONAL ONT::FIGURE) ;(f::situation (f::information f::mental-construct) (f::cause f::mental)))
		))

(define-type ONT::FEELING
    :wordnet-sense-keys ("feeling%1:03:00" "bother%1:09:00" "worry%1:09:00" "sorrow%1:09:00" "distress%1:12:02" "restlessness%1:12:00")
    :parent ONT::mental-construction
    :arguments ((:OPTIONAL ONT::FIGURE) ;(f::situation (f::information f::mental-construct) (f::cause f::mental)))
		))

;; reason, motivation
(define-type ONT::motive
  :wordnet-sense-keys ("motivation%1:03:00")
 :parent ONT::mental-object
 :arguments (
;	     (:optional ONT::Associated-information)
;	     (:optional ont::purpose) ;; reason for something
;	     (:optional ont::REASON) ;; reason for something
	     )
 )

;; problem-solving object (formerly ont::plan-object)
;; high-level for problem solving objects
(define-type ONT::ps-object
 :parent ONT::mental-construction
 ;;:sem (F::Abstr-obj (F::information F::information-content))
 :arguments ((:optional ont::FIGURE)
	     (:optional ont::formal)
	     (:optional ont::GROUND)
	     )
)

;; ways of doing things, accomplishing tasks
(define-type ONT::procedure
 :parent ONT::ps-object
 ;; CERNL hack:: adding verb roles for verbs tagged as ont::procedure
 )

(define-type ONT::method
     :wordnet-sense-keys ("manner%1:07:01" "method%1:09:00")
     :parent ONT::ps-object
     )

;; recipe, procedure, plan
(define-type ONT::recipe
 :parent ONT::procedure
 )

;; http, ssl, imap, etc.
(define-type ont::protocol
 :parent ont::procedure ;; ont::standard
 )

;; policy
(define-type ONT::policy
 :parent ONT::procedure
 )

;; algorithm, program, keystone correction
;; note that there is ont::computer-program for specific software programs like VM
(define-type ONT::algorithm
 :parent ONT::procedure
 )

;; process
(define-type ONT::process
 :wordnet-sense-keys ("procedure%1:04:00" "process%1:04:00")
 :parent ONT::procedure
 )

;; DRUM
(define-type ONT::SIGNALING-PATHWAY
    :parent ONT::process
    )

;; goals, objectives
(define-type ONT::goal
 :wordnet-sense-keys ("goal%1:09:00" "end%1:09:02" "want%1:17:00" "demand%1:26:00")
 :parent ONT::ps-object
 )

;; proposed things
;; proposal, advice, assignment, recommendation
(define-type ONT::proposal
 :parent ONT::ps-object
 :arguments ((:essential ONT::FIGURE))
 )

;; task-related things that one commits to
;; appointment, schedule, agenda, timeline, campaign
(define-type ONT::commitment
 :parent ONT::ps-object
 )

(define-type ONT::problem
 :parent ONT::ps-object
 )

;; lottery, contest
(define-type ont::competition
  :wordnet-sense-keys ("competition%1:11:00")
  :parent ont::event-defined-by-activity
  )

;; game
(define-type ONT::game
 :wordnet-sense-keys ("game%1:04:00")
 :parent ONT::competition
 )

(define-type ONT::sport
 :parent ONT::game
 )

;; football, soccer...
(define-type ONT::athletic-game
 :wordnet-sense-keys ("athletics%1:04:00" "sport%1:04:00")
 :parent ONT::sport
 )

(define-type ONT::court-game
 :parent ONT::athletic-game
 )

;; chess
(define-type ONT::board-game
 :wordnet-sense-keys ("board_game%1:04:00")
 :parent ONT::game
 )

;; preference, comparison, contrast
(define-type ONT::comparison
 :parent ONT::ps-object
 )

;; this involves a selection among alternatives
;; selection, choice, option, alternative
(define-type ONT::Option
 :wordnet-sense-keys ("option%1:09:00" "alternative%1:09:00" "choice%1:09:02")
 :parent ONT::comparison
  )

;; favorite
(define-type ONT::favorite
 :wordnet-sense-keys ("favorite%1:09:00" "favourite%3:00:00:popular:00" "favourite%3:00:00:loved:00")
 :parent ONT::relation
  )

;; system, agent
(define-type ont::agent
  :parent ont::phys-object
  :sem (F::phys-obj (F::information F::information-content) (f::intentional +))
  )

;; (health care) assistant
(define-type ont::assistant
 :wordnet-sense-keys ("assistant%1:18:00" "helper%1:18:01" "help%1:18:00" "supporter%1:18:01")
  :parent ont::agent
  )

(define-type ont::harmful-agency
  :parent ont::agent
  )

(define-type ONT::computer-software
  :parent ONT::ps-object
  :sem (F::Abstr-obj (F::information F::information-content))
  )

;; autofill, autocomplete, colorsync, etc.
(define-type ONT::software-feature
 :wordnet-sense-keys ("expose%1:10:00" "unmasking%1:10:00")
  :parent ONT::computer-software
  )

;; specific programs like VM
(define-type ONT::computer-program
 :wordnet-sense-keys ("applications_programme%1:10:00" "application_program%1:10:00" "application%1:10:01" "program%1:10:02" "programme%1:10:02" "computer_program%1:10:00" "computer_programme%1:10:00")
  :parent ONT::computer-software
   :sem (F::Abstr-obj (F::information F::information-content) (f::origin f::artifact))
  )

(define-type ONT::web-browser
  :parent ONT::computer-software
  )

(define-type ONT::Linguistic-object
 :wordnet-sense-keys ("language_unit%1:10:00" "linguistic_unit%1:10:00")
 :parent ONT::mental-construction
; :sem (F::Abstr-obj (F::information F::data))
 )

;; digit
(define-type ONT::number-object
 :parent ONT::number
 :sem (F::Abstr-obj (F::information F::data))
 )

(define-type ont::language
    :wordnet-sense-keys ("language%1:10:00" "dialect%1:10:00")
 :parent ont::linguistic-object
 )

(define-type ont::question
    :wordnet-sense-keys ("question%1:10:00")
    :parent ont::linguistic-object
    )

;; prefix, suffix
(define-type ont::linguistic-component
 :parent ont::linguistic-object
 :arguments ((:REQUIRED ONT::FIGURE (F::abstr-obj (f::information f::information-content))))
 )

;; html, java, etc.
(define-type ont::computer-language
 :parent ont::language
 )

;; mbox, jpeg, doc, txt, etc.
(define-type ont::file-format
 :parent ont::computer-language
 )

;;; there is ont::graphic-symbol; this should be related somehow
(define-type ONT::punctuation
    :wordnet-sense-keys ("punctuation%1:10:00")
    :parent ONT::linguistic-object
 )

;; letters of the alphabet
(define-type ONT::letter-symbol
    :wordnet-sense-keys ("letter%1:10:01")
    :parent ONT::linguistic-object
; :sem (F::Abstr-obj (F::information F::data))
 )

;; alpha, beta, ...
(define-type ONT::greek-letter-symbol
 :parent ONT::letter-symbol
 )

(define-type ONT::POPULATION
 :parent ONT::ORDERED-DOMAIN
 :sem (F::Abstr-obj (F::Measure-function F::term))
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj (F::form F::geographical-object)))
             )
 )

(define-type ONT::COST-RELATION
 :parent ONT::predicate
 :sem (F::Abstr-obj (f::scale ont::money-scale) (f::information f::information-content))
 :arguments ((:REQUIRED ont::FIGURE)
	     (:REQUIRED ONT::GROUND (F::Abstr-obj (F::Scale Ont::money-scale) (f::object-function f::currency)))
             )
 )

;; I want to buy a computer for under 1000 dollars
;; we want vp attachment here, but not to static verbs
(define-type ONT::PURCHASE-COST
  :parent ONT::COST-RELATION
  :arguments ((:REQUIRED ont::FIGURE (f::situation (f::aspect f::dynamic)))
	      (:REQUIRED ONT::GROUND (F::Abstr-obj (F::Scale Ont::money-scale) (f::object-function f::currency)))
             )
 )

;;  
(define-type ONT::VALUE-COST
 :wordnet-sense-keys ("change%1:21:02" "return%1:21:00" "issue%1:21:00" "take%1:21:00" "takings%1:21:00" "proceeds%1:21:00" "yield%1:21:00" "payoff%1:21:02")
 :wordnet-sense-keys ("change%1:21:02" "return%1:21:00" "issue%1:21:00" "take%1:21:00" "takings%1:21:00" "proceeds%1:21:00" "yield%1:21:00" "payoff%1:21:02")
 :parent ONT::abstract-object-nontemporal
  :arguments ((:REQUIRED ONT::FIGURE (F::Abstr-obj (F::Scale Ont::money-scale) (f::object-function f::currency)))
	      (:REQUIRED ONT::GROUND (F::Abstr-obj (F::Scale Ont::money-scale) (f::object-function f::currency)))
	      )
  )

;; unique lf for price
(define-type ONT::PRICE
 :parent ONT::VALUE-COST
  :arguments ((:REQUIRED ont::FIGURE ((? lo f::phys-obj f::abstr-obj)))
	      (:REQUIRED ONT::GROUND (F::Abstr-obj (F::Scale Ont::money-scale) (f::object-function f::currency)))
             )
 )

;; unique lf for rate, as in gsa rate
(define-type ont::charge-per-unit
  :parent ont::value-cost
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

;; for account, grant, credit card
;; can we find a way to distinguish between bill (put it on my bill) and grant (put it on my grant)?
(define-type ONT::ACCOUNT
 :parent ONT::ABSTRACT-OBJECT-nontemporal
 :sem (F::Abstr-obj (F::Measure-function F::term)  (f::object-function f::currency) (f::scale ont::money-scale))
  :arguments (
 ;; accounts can belong to individuals, organizations or projects
 (:essential ont::FIGURE ((? lof f::phys-obj f::abstr-obj))))
 )

;; bill, tab
(define-type ONT::ACCOUNT-PAYABLE
 :wordnet-sense-keys ("bill%1:10:01" "account%1:10:02" "invoice%1:10:00")
 :wordnet-sense-keys ("bill%1:10:01" "account%1:10:02" "invoice%1:10:00")
 :parent ONT::ACCOUNT
 :sem (F::Abstr-obj (f::scale ont::money-scale))
 )

;; lack, shortage
(define-type ONT::LACK
  :parent ont::situation
 :arguments ((:REQUIRED ONT::FIGURE)
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
(define-type ONT::sense
;; :parent ONT::ANY-SEM
    :parent ont::abstract-function
;; F::proposition is true or false, this does not apply to sense
;; :sem (F::proposition (F::information F::information-content))
 )
|#

;;; (health) care, treatment
(define-type ONT::treatment
 :wordnet-sense-keys ("care%1:04:01" "attention%1:04:01" "aid%1:04:01" "tending%1:04:00" "regimen%1:09:00::" "regime%1:09:00::")
 :parent ONT::procedure
 )

;; diagnostics
(define-type ont::medical-test
  :parent ont::procedure
  )

;; can be either treatment or test -- not sure where to classify but adding WN mappings
(define-type ont::medical-procedure
  :wordnet-sense-keys  ("medical_procedure%1:04:00"  "incision%1:04:00" "section%1:04:00" "surgical_incision%1:04:00")
  :parent ont::procedure
  )

(define-type ont::medical-diagnostic
  :wordnet-sense-keys ("diagnostic_procedure%1:04:00" "diagnostic_technique%1:04:00")
  :parent ont::medical-test
  )

(define-type ONT::CONSTRAINT
    :wordnet-sense-keys ( "restriction%1:09:00" "limitation%1:09:00")
    :parent ONT::SITUATION
    :arguments ((:OPTIONAL ONT::FIGURE)
		(:optional ont::formal)
		)
    )

(define-type ONT::NAME
 :parent ONT::IDENTIFICATION
 :sem (F::Abstr-obj (F::information F::information-content))
 :arguments ((:ESSENTIAL ONT::FIGURE )
             )
 )

(define-type ONT::FIRSTNAME
 :parent ONT::NAME
 :sem (F::Abstr-obj (F::information F::information-content))
 :arguments ((:ESSENTIAL ONT::FIGURE )
             )
 )

(define-type ONT::LASTNAME
 :parent ONT::NAME
 :sem (F::Abstr-obj (F::information F::information-content))
 :arguments ((:ESSENTIAL ONT::FIGURE )
             )
 )

(define-type ONT::FULLNAME
 :parent ONT::NAME
 :sem (F::Abstr-obj (F::information F::information-content))
 :arguments ((:ESSENTIAL ONT::FIGURE )
             )
 )

;; for books, cd's, etc.
(define-type ONT::TITLE
 :parent ONT::NAME
 :sem (F::Abstr-obj (F::information F::information-content))
 :arguments ((:ESSENTIAL ONT::FIGURE  (F::phys-obj (F::origin F::artifact) (F::information f::information-content)))
             )
 )

(define-type ONT::SIGNATURE
 :wordnet-sense-keys ("signature%1:10:00")
 :wordnet-sense-keys ("signature%1:10:00")
 :parent ONT::name
 )

#|
(define-type ONT::RESPONSIBILITY
 :parent ONT::ORDERED-DOMAIN
 :arguments ((:OPTIONAL ONT::FIGURE)
             )
 )
|#

#|
;;; LF type for movie, theater
(define-type ONT::ENTERTAINMENT
 :parent ONT::ABSTRACT-OBJECT
 :sem (F::ABSTR-OBJ (F::CONTAINER -) (F::INFORMATION f::information-content) (F::INTENTIONAL -))
 )
|#

;; song, rhapsody
(define-type ONT::music
 :wordnet-sense-keys ("music%1:10:00")
 :parent ONT::composition
 )
;; For epa and bee

(define-type ont::voltage
 :wordnet-sense-keys ("voltage%1:19:02" "electromotive_force%1:19:00" "emf%1:19:00")
    :parent ont::phys-measure-domain)

(define-type ont::definition
    :parent ont::information-function-object
    :arguments ((:essential ont::FIGURE))
    )

;; changed parent to ont::discipline (from abstract-object)
;; 2005.04/20 Added by Myrosia to handle words like algebra, mathematics etc.
(define-type ONT::science-discipline
 :parent ONT::discipline
 :sem (F::Abstr-obj (F::container +)) ;; why is this container +?
 )

;; acid wash
(define-type ONT::food-prep-process
 :parent ONT::process
 )

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

(define-type ONT::eating-plan
 :parent ONT::RECIPE
 )

;; hearing, sight, vision
(define-type ONT::physical-sense
 :parent ONT::non-measure-ordered-domain
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj (F::container -) (F::form F::solid-object) (F::origin F::human)))
             )
 )

;; abnormality, irregularity, anomaly
(define-type ONT::abnormality
 :wordnet-sense-keys ("abnormality%1:04:00" "irregularity%1:04:00")
 :parent ONT::event-type
 :sem (F::Situation (F::aspect F::dynamic))
 :arguments ((:OPTIONAL ONT::FIGURE)
             )
 )

;; We need f::situation counterparts for medical words classified as ont::treatment or ont::diagnostic for i2b2 because we don't have multiple inheritance
;; surgery
(define-type ONT::medical-action
 :parent ONT::action
 :sem (F::Situation (F::aspect F::dynamic))
 :arguments ((:OPTIONAL ONT::FIGURE)
             )
 )

#|
(define-type ONT::QUICK
 :parent ONT::EVENT-DURATION-MODIFIER
 ; Words: (W::QUICK W::FAST W::INSTANT W::INSTANTANEOUS)
 :wordnet-sense-keys ("fast%3:00:01" "instantaneous%5:00:00:fast:01" "quick%5:00:00:fast:01")
 ; Antonym: NIL (W::SLOW)
 )
|#

(define-type ONT::SIMILAR
 :wordnet-sense-keys ("like%5:00:00:same:00" "comparable%5:00:00:same:00" "like%3:00:04" "similar%3:00:04" "alike%3:00:00" "same%3:00:04" "like%3:00:02" "like%3:00:00" "similar%3:00:02" "corresponding%5:00:00")
 :wordnet-sense-keys ("like%5:00:00:same:00" "comparable%5:00:00:same:00" "like%3:00:04" "similar%3:00:04" "alike%3:00:00" "same%3:00:04" "like%3:00:02" "like%3:00:00" "similar%3:00:02" "corresponding%5:00:00")
 :parent ONT::SIMILARITY-VAL
 ; Words: (W::SIMILAR W::LIKE W::ANALOGOUS W::KINDRED)
 :wordnet-sense-keys ("like%3:00:00" "like%3:00:00" "analogous%5:00:00:similar:00" "similar%3:00:00" "akin%5:00:00")
 ; Antonym: ONT::DIFFERENT (W::DIFFERENT W::SEPARATE W::DISTINCT)
 )

(define-type ONT::DIFFERENT
 :parent ONT::SIMILARITY-VAL
 ; Words: (W::DIFFERENT W::SEPARATE W::DISTINCT)
 :wordnet-sense-keys ("unlike%3:00:00" "unlike%3:00:00" "discrete%5:00:00:separate:00" "different%3:00:00" "separate%3:00:00" "distinct%5:00:00")
 ; Antonym: ONT::SIMILAR (W::SIMILAR W::LIKE W::ANALOGOUS W::KINDRED)
 )

(define-type ONT::EQUAL
 :parent ONT::SIMILARITY-VAL
 ; Words: (W::EQUAL W::EQUIVALENT)
 :wordnet-sense-keys ("equal%3:00:00" "equivalent%5:00:00:equal:00" "equal%3:00:00")
 ; Antonym: NIL (W::UNEQUAL)
 )

(define-type ONT::SAME
 :parent ONT::IDENTITY-VAL
 :wordnet-sense-keys ("same%3:00:02" "same%3:00:02" "identical%5:00:00:same:02")
 ; Antonym: NIL (W::OTHER)
 )

(define-type ONT::other
 :parent ONT::IDENTITY-VAL
 :wordnet-sense-keys ("other%3:00:00" "another%3:00:00")
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

(define-type ont::scale
  :sem (F::Abstr-obj (F::Scale ?!sc))
  :parent ont::abstract-object
  :arguments ((:ESSENTIAL ONT::figure)
		)
  )

(define-type ont::any-scale
  :parent ont::scale
  )

(define-type ont::number-scale
  :parent ont::scale
  )

(define-type ont::size-scale
  :parent ont::scale
  )

(define-type ont::sound-scale
  :parent ont::scale
  )

(define-type ont::weight-scale
;  :parent ont::size-scale
  :parent ont::scale
  )

(define-type ont::linear-scale
;  :parent ont::size-scale
  :parent ont::scale
  )

(define-type ont::area-scale
;  :parent ont::size-scale
  :parent ont::scale
  )

(define-type ont::volume-scale
;  :parent ont::size-scale
  :parent ont::scale
  )

(define-type ont::temperature-scale
  :parent ont::scale
  )

(define-type ont::money-scale
;  :parent ont::size-scale
  :parent ont::scale
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

(define-type ont::luminosity-scale
  :parent ont::scale
  )

(define-type ont::time-measure-scale
  :sem (F::Abstr-obj (F::Scale Ont::time-measure-scale))
;  :parent ont::scale
  :parent ont::linear-scale
  )

(define-type ont::duration-scale
  :parent ont::time-measure-scale
  )

(define-type ont::age-scale
;  :parent ont::scale
;  :parent ont::linear-scale
  :parent ont::duration-scale
  )

(define-type ont::other-scale
  :parent ont::scale
  )


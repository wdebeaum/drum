(in-package :om)

; > starts a block
; < ends the block
; use `[code]` to use lisp syntax in code (optional, but makes eliminating commented code easier)

;;; A special type for pronouns etc that can refer to arbitrary abstract objects
;;; declares all features as arbibtary vars to override default - features
(define-type ONT::FACT
     :wordnet-sense-keys ("fact%1:09:01" "fact%1:09:02" "fact%1:10:01")
 :parent ONT::ABSTRACT-OBJECT-nontemporal
 :sem (F::Abstr-obj (f::tangible +)) ; facts shouldn't be tangible, but we have it here so that we can add/remove facts (from a graph)
 :arguments ((:optional ONT::formal)
	     )
 )

(define-type ONT::KIND
    :parent ONT::ABSTRACT-OBJECT-nontemporal
    :wordnet-sense-keys ("kind%1:09:00")
    :sem (F::abstr-obj (F::SCALE -))
    :arguments ((:REQUIRED ONT::FIGURE)
		)
 )

;; some kind of replication of the same thing
;; version, edition, variant
(define-type ONT::Version
    :wordnet-sense-keys ("version%1:09:01" "edition%1:14:00" "draft%1:10:00")
    :parent ONT::KIND
 )

;; example, illustration, instance
(define-type ONT::example
 :wordnet-sense-keys ("example%1:09:00" "example%1:09:02")
 :parent ONT::kind
 )

(define-type ONT::representation
 :wordnet-sense-keys ("representation%1:09:00" "figure%1:25:00")
 :parent ONT::mental-construction
 :sem (F::Abstr-obj (f::container +))
 :arguments ((:OPTIONAL ONT::FIGURE) ;(f::situation (f::information f::mental-construct) (f::cause f::mental)))
		)
 )

; social practice
(define-type ONT::social-practice
 :parent ONT::mental-construction
 :wordnet-sense-keys ("custom%1:09:00" "practice%1:09:00" "habit%1:09:00" "routine%1:04:00"
				      "ethos%1:07:00" )
 )


(define-type ont::desire
 :parent ont::mental-construction
 :wordnet-sense-keys ("desire%1:07:00" "appetite%1:12:00")
)

; perceptual-construction
(define-type ont::perceptual-construction
 :parent ont::mental-construction
 :arguments ((:OPTIONAL ONT::FIGURE))
)

; ability to perceive
(define-type ont::ability-to-perceive
 :parent ont::perceptual-construction
 :wordnet-sense-keys ("perception%1:09:01" "sense%1:09:02")
)

(define-type ont::ability-to-see
 :parent ont::ability-to-perceive
 :wordnet-sense-keys ("vision%1:09:01")
)

(define-type ont::ability-to-hear
 :parent ont::ability-to-perceive
 :wordnet-sense-keys ("hearing%1:09:00")
)

(define-type ont::ability-to-taste
 :parent ont::ability-to-perceive
 :wordnet-sense-keys ("taste%1:09:02")
)

(define-type ont::ability-to-touch
 :parent ont::ability-to-perceive
 :wordnet-sense-keys ("touch%1:09:01")
)

(define-type ont::ability-to-smell
 :parent ont::ability-to-perceive
 :wordnet-sense-keys ("smell%1:09:01")
)

; perceivable property
(define-type ont::perceivable-property
 :parent ont::perceptual-construction
)

(define-type ont::perceivable-smell-property
 :parent ont::perceivable-property
 :wordnet-sense-keys ("smell%1:09:02" "aroma%1:09:01")
)

(define-type ont::perceivable-taste-property
 :parent ont::perceivable-property
 :wordnet-sense-keys ("flavor%1:09:00")
)

(define-type ont::perceivable-sound-property
 :parent ont::perceivable-property
 :wordnet-sense-keys ("sound%1:07:00" "sound%1:09:00" "sound%1:19:00" "sound%1:11:00")
)

(define-type ONT::grouping
    :comment "a  classification, category, variety of things. Not a set of objects!"
    ;:parent ONT::version
    :parent ont::kind
    )

(define-type ONT::FUNCTION-OBJECT
; :parent ONT::domain-property
 :parent ONT::tangible-abstract-object
 :sem (F::Abstr-obj)
 )

(define-type ONT::SOCIAL-SYSTEM
    :parent ONT::tangible-abstract-object
    :wordnet-sense-keys ("political_system%1:14:00")
    :sem (F::Abstr-obj)
    )


;; purpose, function
(define-type ont::utility
 :parent ont::function-object
 :wordnet-sense-keys ("role%1:07:00")
  :arguments ((:REQUIRED ONT::FIGURE)
             )
 )

;; the relation type for comparatives
;; more, better
(define-type ONT::More-val
    :parent ONT::domain-property
  :wordnet-sense-keys ("better%3:00:00::" "better%3:00:02::" "greater%3:00:00::")
    :sem (F::ABSTR-OBJ (:required (F::CONTAINER -) (F::INFORMATION -) (F::INTENTIONAL -))
		       (:default (F::GRADABILITY +) (F::scale ?!sc)))
    )
;; less, worse
(define-type ONT::less-val
    :parent ONT::domain-property
  :wordnet-sense-keys ("worse%3:00:00::" "fewer%3:00:00::" "worse%3:00:02::" "worsened%3:00:00::" "less%3:00:00::")
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
	      (:REQUIRED ONT::GROUND))
 :wordnet-sense-keys ("best%3:00:00::" "most%3:00:01::" "most%3:00:02::" "ultimate%3:00:00" "maximum%3:00:00" "peak%1:23:00")
)

;; worst, least
(define-type ONT::MIN-val
  :parent ONT::domain-property
  :sem (F::ABSTR-OBJ (:required (F::CONTAINER -) (F::INFORMATION -) (F::INTENTIONAL -))
		      (:default (F::GRADABILITY +) (F::scale ?!sc)))
 :wordnet-sense-keys ("worst%3:00:00::" "minimum%3:00:00")
  )

;; as hot as it can be
(define-type ONT::as-much-as
  :parent ONT::domain-property
  :sem (F::ABSTR-OBJ (:required (F::CONTAINER -) (F::INFORMATION -) (F::INTENTIONAL -))
		      (:default (F::GRADABILITY +) (F::scale ?!sc)))
  )

(define-type ONT::TOO-MUCH
  :wordnet-sense-keys ("too_much%4:02:00")
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
    :wordnet-sense-keys ("mathematical_group%1:09:00")
    :comment "a formal concept of a group of objects, e.g., mathematical"
    :parent ont::abstract-object-nontemporal
					;  :sem (F::Abstr-obj (f::group +)) ; group feature not defined for abstract objects
    :arguments ((:OPTIONAL ONT::FIGURE)
		)
  )

(define-type ONT::system-abstr
  :wordnet-sense-keys ( "system%1:14:00")
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

;; google, amazon, isp315

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
; following int ont::military-group
; :wordnet-sense-keys ("military_unit%1:14:00" "military_force%1:14:00" "military_group%1:14:00" "force%1:14:01")
 :parent ONT::social-group-abstr
 )

(define-type ONT::collection-abstr
 :wordnet-sense-keys ("collection%1:14:00" "aggregation%1:14:00" "accumulation%1:14:00" "assemblage%1:14:01")
 :parent ONT::group-object-abstr
 )

(define-type ONT::sequence-abstr
 :wordnet-sense-keys ("ordering%1:14:00" "order%1:14:00" "ordination%1:14:00" "sequence%1:07:00")
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

;; own: his own truck
(define-type ONT::own
  :wordnet-sense-keys ("own%5:00:00:personal:00")
  :parent ONT::relation
  )

;;; NOTE: SIMILARITY-val moved to property-val (june 2019)

#||;;; ownership, property, etc.
(define-type ONT::possession
 :wordnet-sense-keys ();"possession%1:03:00")
 :parent ONT::RELATION
 :arguments ((:ESSENTIAL ONT::neutral (F::phys-obj (F::origin F::human) (F::intentional +)))
             (:ESSENTIAL ONT::neutral1 ((? cth f::phys-obj f::abstr-obj)))
             )
 )||#
#|
;; how does this relate to ont::truth-val??
;;; e.g., wrong, problematic, right,
;; the wrong day, the right time, the right number
(define-type ONT::EVALUATION-VAL
 :parent ONT::RELATION
 :arguments ((:ESSENTIAL ONT::neutral ((? tp f::time f::abstr-obj F::phys-obj F::situation)))
	     (:OPTIONAL  ONT::neutral1 ((? tp1 f::time f::abstr-obj F::phys-obj F::situation)))
             )
 )
|#
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
    :wordnet-sense-keys ("constant%1:23:00" "differential_coefficient%1:09:00" "divisor%1:23:00" "equation%1:10:00" "term%1:09:00")
    :parent ONT::abstract-object-nontemporal
    :sem (f::abstr-obj (:required (f::gradability -)) (:default (f::information f::data)) )
    :arguments ((:ESSENTIAL ONT::FIGURE (f::abstr-obj (F::measure-function f::term)))
		(:ESSENTIAL ONT::GROUND (F::abstr-obj (:default (F::measure-function F::value))))))

(define-type ont::data-structure
  :parent ont::mathematical-term
  :wordnet-sense-keys ("matrix%1:14:00")
  )

(define-type ont::parameter
  :parent ont::mathematical-term
  :wordnet-sense-keys ("parameter%1:10:00" "variable%1:10:00" "variable%1:09:00")
)

(define-type ont::number
  :wordnet-sense-keys ("number%1:23:00")
;  :parent ont::ordered-domain
  :parent ONT::MATHEMATICAL-TERM
  :sem (F::abstr-obj ;;(F::measure-function F::value)
       (F::CONTAINER -) (F::INFORMATION f::information-content) (F::INTENTIONAL -)
       )
  )

(define-type ont::number-result
 :parent ont::number
 :wordnet-sense-keys("sum%1:09:01" "quotient%1:23:00" "product%1:09:00")
)

;; layer (of ozone, chocolate), sheet (of ice, paper), slice
;(define-type ont::sheet-abstr
;  :parent ont::non-measure-ordered-domain
;  :parent ONT::GROUP-OBJECT-abstr
;  )

(define-type ONT::MEASURE-UNIT
 :wordnet-sense-keys ("unit_of_measurement%1:23:00" "unit%1:23:00")
 :parent ONT::unit
 :sem (F::abstr-obj (F::measure-function F::value) (F::CONTAINER -) (F::INFORMATION -)
       (F::INTENTIONAL -) (F::SCALE ONT::MEASURE-SCALE))
 ;;; We define an argument here because we want to express selectional restrictions on what this unit can measure
 :arguments ((:ESSENTIAL ONT::FIGURE)
             )
 )

(define-type ONT::formal-unit
 :parent ONT::measure-unit
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
    :wordnet-sense-keys ("hour%1:28:00")
    :parent ont::time-unit)

(define-type ont::minute-duration
    :wordnet-sense-keys ("minute%1:28:00")
    :parent ont::time-unit)

(define-type ont::day-duration
    :wordnet-sense-keys ("day%1:28:00")
    :parent ont::time-unit)

(define-type ont::year-duration
    :wordnet-sense-keys ("year%1:28:01") ;"year%1:28:00")
    :parent ont::time-unit)

(define-type ont::week-duration
    :wordnet-sense-keys ("week%1:28:00")
    :parent ont::time-unit)

(define-type ont::second-duration
    :wordnet-sense-keys ("second%1:28:00")
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

;; dozen, hundred, thousand...
(define-type ONT::NUMBER-UNIT
 :wordnet-sense-keys ("billion%1:23:00" "billion%1:23:01" "billion%5:00:00:cardinal:00" "billion%5:00:01:cardinal:00" "dozen%1:23:00" "dozen%5:00:00:cardinal:00" "hundred%1:23:00" "hundred%5:00:00:cardinal:00" "thousand%1:23:00" "thousand%5:00:00:cardinal:00" "million%1:23:00" "million%5:00:00:cardinal:00" "trillion%1:23:01" "trillion%5:00:01:cardinal:00" "trillion%5:00:00:cardinal:00")
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

(define-type ONT::kg
 :parent ONT::weight-unit
 :sem (F::abstr-obj (F::Scale Ont::WEIGHT-scale))
 ;; 5 ounces of water
 :arguments ((:ESSENTIAL ONT::FIGURE (F::phys-obj))) ;;(F::FORM F::solid))))
 )

(define-type ONT::g
 :parent ONT::weight-unit
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
 :wordnet-sense-keys ("linear_measure%1:23:00" "linear_unit%1:23:00")
 :parent ONT::tangible-unit
 :sem (F::Abstr-obj (F::Scale ONT::LINEAR-EXTENT-SCALE)) ;ONT::LINEAR-D)) ; Ont::length))  ; e.g., km: not just length but could also be width, height, etc
 )

(define-type ONT::km
 :parent ONT::length-unit
 :sem (F::Abstr-obj (F::Scale ONT::LINEAR-EXTENT-SCALE)) ;ONT::LINEAR-D)) ; Ont::length))  ; e.g., km: not just length but could also be width, height, etc
 )

(define-type ONT::m
 :parent ONT::length-unit
 :sem (F::Abstr-obj (F::Scale ONT::LINEAR-EXTENT-SCALE)) ;ONT::LINEAR-D)) ; Ont::length))  ; e.g., km: not just length but could also be width, height, etc
 )

(define-type ONT::cm
 :parent ONT::length-unit
 :sem (F::Abstr-obj (F::Scale ONT::LINEAR-EXTENT-SCALE)) ;ONT::LINEAR-D)) ; Ont::length))  ; e.g., km: not just length but could also be width, height, etc
 )

(define-type ONT::mm
 :parent ONT::length-unit
 :sem (F::Abstr-obj (F::Scale ONT::LINEAR-EXTENT-SCALE)) ;ONT::LINEAR-D)) ; Ont::length))  ; e.g., km: not just length but could also be width, height, etc
 )

;; acre, sqare feet
(define-type ONT::AREA-UNIT
 :wordnet-sense-keys ("area_unit%1:23:00" "square_measure%1:23:00")
 :parent ONT::tangible-unit
 :sem (F::Abstr-obj (F::Scale Ont::area-scale))
 )

(define-type ONT::ha
 :parent ONT::area-unit
 :sem (F::Abstr-obj (F::Scale Ont::area-scale))
 )

(define-type ONT::m2
 :parent ONT::area-unit
 :sem (F::Abstr-obj (F::Scale Ont::area-scale))
 )

(define-type ONT::Volume-UNIT
 :wordnet-sense-keys ("volume_unit%1:23:00" "capacity_unit%1:23:00" "capacity_measure%1:23:00" "cubage_unit%1:23:00" "cubic_measure%1:23:00" "cubic_content_unit%1:23:00" "displacement_unit%1:23:00" "cubature_unit%1:23:00")
 :parent ONT::tangible-unit
 :sem (F::Abstr-obj (F::Scale Ont::Volume-scale))
 :arguments ((:ESSENTIAL ONT::FIGURE (F::phys-obj (F::FORM F::substance))))
 )

(define-type ONT::CONTAINER-LOAD
 :wordnet-sense-keys ("load%1:23:00")
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
    :wordnet-sense-keys ("serving%1:13:00")
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
 :wordnet-sense-keys ("miles_per_gallon%1:23:00" "revolutions_per_minute%1:28:00" "words_per_minute%1:28:00")
 :parent ONT::formal-unit
 :sem (F::Abstr-obj (F::Scale Ont::Rate-scale))
 :arguments ((:ESSENTIAL ONT::FIGURE ((? type F::phys-obj F::situation)))
             )
 )

(define-type ONT::speed-unit
    :wordnet-sense-keys ("mph%1:28:00" "miles_per_hour%1:28:01" "kilometers_per_hour%1:28:00" "kilometres_per_hour%1:28:00" "kph%1:28:00" "km/h%1:28:00")
    :sem (F::Abstr-obj (F::Scale Ont::speed-scale))
    :parent ONT::rate-unit
    )

(define-type ONT::frequency-unit
    :wordnet-sense-keys ("Hz%1:28:00" "khz%1:28:00" "mhz%1:28:00" "ghz%1:28:00" "thz%1:28:00")
    :sem (F::Abstr-obj (F::Scale Ont::frequency-scale))
    :parent ONT::rate-unit
    )

(define-type ONT::bandwidth-unit
    :wordnet-sense-keys ("bps%1:28:00" "baud%1:23:00")
    :sem (F::Abstr-obj (F::Scale Ont::bit-rate-scale))
    :parent ONT::rate-unit
    )

;; bit, byte
(define-type ONT::memory-UNIT
 :wordnet-sense-keys ("computer_memory_unit%1:23:00")
 :parent ONT::formal-UNIT
 :sem (F::Abstr-obj (F::Scale Ont::measure-scale))
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
 :sem (F::Abstr-obj (F::Scale Ont::Linear-extent-scale))
 :arguments ((:ESSENTIAL ONT::FIGURE (F::situation (f::trajectory +)))
             )
 )

(define-type ONT::number-measure-domain
 ;; :parent ONT::MEASURE-DOMAIN
 :wordnet-sense-keys ("root%1:23:00" "cosine%1:24:00" "sine%1:24:00" "exponent%1:10:00")
 :parent ONT::MATHEMATICAL-TERM
 :arguments ((:REQUIRED ONT::FIGURE (F::Abstr-obj (F::Measure-function F::Term)))
             )
 )

(define-type ONT::SCALE-VALUE-FUNCTION
  :comment "words that act as map objects to values on a scale/domain: What is the X on this scale?  Note: We exclude words that are identical to the names of the scales they pertain to (e.g., What is the height on the height scale?)"
 :wordnet-sense-keys ("level%1:26:00" "level%1:07:00")
 :sem (F::Abstr-obj (F::Scale Ont::DOMAIN))
; :parent ONT::ordered-DOMAIN
 :parent ONT::ABSTRACT-OBJECT
 :arguments ((:ESSENTIAL ONT::FIGURE ((? of f::phys-obj F::Abstr-obj))) ;; noise, water
	     (:essential ont::GROUND (f::abstr-obj   (F::INFORMATION F::INFORMATION-CONTENT))) ; a level of 5
             )
 )

(define-type ONT::LEVEL
  :comment "words that act as map objects to values on a scale/domain: What is the X on this scale?  Note: We exclude words that are identical to the names of the scales they pertain to (e.g., What is the height on the height scale?)"
 :wordnet-sense-keys ("level%1:26:00" "level%1:07:00")
 :sem (F::Abstr-obj (F::Scale ont::domain))
; :parent ONT::ordered-DOMAIN
 :parent ONT::SCALE-VALUE-FUNCTION
 :arguments ((:ESSENTIAL ONT::FIGURE ((? of f::phys-obj F::Abstr-obj))) ;; noise, water
	     (:essential ont::GROUND (f::abstr-obj   (F::INFORMATION F::INFORMATION-CONTENT))) ; a level of 5
             )
 )

(define-type ont::value
    :wordnet-sense-keys ("value%1:07:00")
    :comment "a function from an object to a value on some scale of worth"
 :parent ONT::scale-value-function
  :arguments (;(:REQUIRED ONT::FIGURE (F::Abstr-obj (F::Scale Ont::money-scale) (f::object-function f::currency)))
	      (:REQUIRED ont::FIGURE ((? lo f::phys-obj f::abstr-obj)))
              (:REQUIRED ONT::GROUND (F::Abstr-obj (F::Scale Ont::money-scale) (f::object-function f::currency)))
              )
  )

(define-type ONT::range
 :wordnet-sense-keys ("range%1:07:00" "bracket%1:14:00")
 :parent ont::level
)

;; a number/amount/quantity of X
(define-type ONT::QUANTITY-abstr
    :wordnet-sense-keys ("measure%1:03:00" "quantity%1:03:00" "amount%1:03:00" "quantity%1:09:01"
					   "amount%1:21:00" "amount%1:07:00")
; :parent ONT::DOMAIN-PROPERTY
 ;:parent ONT::GROUP-OBJECT-abstr
 :parent ONT::LEVEL
 :sem (F::abstr-obj (F::tangible +)) ; if we are keeping this we should move this up to ONT::LEVEL
 :arguments ((:ESSENTIAL ONT::FIGURE)
             )
 )

(define-type ONT::dynamics
 :wordnet-sense-keys ("dynamics%1:09:00")
 :parent ONT::ABSTRACT-OBJECT
 :arguments ((:ESSENTIAL ONT::FIGURE )
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

;; three fold
(define-type ONT::multiple
 :wordnet-sense-keys ("multiple%1:09:00" "factor%1:23:00" "factor%1:23:01")
 :parent ONT::MATHEMATICAL-TERM
 :sem (F::Abstr-obj (F::Scale ont::DOMAIN))  ; "by three fold" needs scale
 )

;; percent
(define-type ONT::percent
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

(define-type ONT::ASSETS
    :wordnet-sense-keys ("assets%1:21:00" "resource%1:21:00" "reserve%1:21:00" "fund%1:21:00"
					  "kitty%1:21:01")
; :parent ONT::MEASURE-DOMAIN
 :parent ONT::FUNCTION-OBJECT
 :sem (F::Abstr-obj (F::Scale Ont::money-scale))
 :arguments ((:REQUIRED ONT::FIGURE ((? fot F::phys-obj F::situation)))
             (:ESSENTIAL ONT::GROUND (F::abstr-obj (F::measure-function F::value) (F::scale ont::money-scale)))
             )
 )

(define-type ONT::DEBT
 :wordnet-sense-keys ("liabilities%1:21:00" "obligation%1:26:00")
; :parent ONT::MEASURE-DOMAIN
 :parent ONT::FUNCTION-OBJECT
 :sem (F::Abstr-obj (F::Scale Ont::money-scale))
 :arguments ((:REQUIRED ONT::FIGURE ((? fot F::phys-obj F::situation)))
             (:ESSENTIAL ONT::GROUND (F::abstr-obj (F::measure-function F::value) (F::scale ont::money-scale)))
             )
 )

;; dollars -- need a unit definition for '5 dollars'
(define-type ONT::MONEY-UNIT
 :wordnet-sense-keys ("monetary_unit%1:23:00" "currency%1:21:00")
 :parent ONT::formal-UNIT
 :sem (F::Abstr-obj (F::Scale Ont::Money-scale))
 )

;; currency
(define-type ONT::currency
 :wordnet-sense-keys ("medium_of_exchange%1:21:00")
 :parent ONT::FUNCTION-OBJECT
 :sem (F::Abstr-obj (f::scale ont::money-scale))
 )

(define-type ONT::source
 :wordnet-sense-keys ("supply%1:23:00")
 :parent ONT::function-OBJECT
 :sem (F::Abstr-obj)
 :arguments ((:essential ONT::FIGURE)
	     )
 )

(define-type ONT::prize
  :wordnet-sense-keys ("prize%1:21:00" "prize%1:06:00")
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
 :wordnet-sense-keys ("communication%1:03:00" "criminal_record%1:10:00" "calculation%1:09:00" "history%1:28:02")
 :sem (F::Abstr-obj (F::information F::information-content) (F::intentional -) (F::container +) (F::mobility f::movable)) ; movable: spread the news
 :arguments (
;	     (:optional ONT::Associated-information)
	     )
 )

(define-type ONT::message
     :wordnet-sense-keys ("message%1:10:01")
     :parent ont::information-function-object
     ;:arguments ((:optional ONT::formal (F::prop)))
     :arguments ((:optional ONT::formal (F::situation)))
)

(define-type ONT::composition
  :comment "composition, e.g., result of event-of-creation"
  :wordnet-sense-keys ("composition%1:07:01" "composition%1:07:02" "composition%1:04:01")
 :parent ONT::information-function-object
 )

;; information
(define-type ONT::information
 :wordnet-sense-keys ("information%1:09:00" "information%1:10:00" "info%1:10:00" "vital_sign%1:26:00" "indicator%1:10:01")
 :parent ONT::information-function-object
 ;:arguments ((:optional ONT::formal (F::prop))) ; copied from ONT::MESSAGE
 :arguments ((:optional ONT::FIGURE)
	     (:optional ONT::formal (F::situation))) ; copied from ONT::MESSAGE
 )

(define-type ONT::measure-metric
 :wordnet-sense-keys ("metric%1:23:00" "indicator%1:10:00")
 :parent ONT::information
 )

(define-type ONT::evidence
    :wordnet-sense-keys ("basis%1:09:00" "foundation%1:24:00" "evidence%1:09:00" "indication%1:10:00" "evidence%1:10:00")
    :parent ONT::information
    )

;; create an ont::communication-object
;; subject, topic
(define-type ONT::content
 :wordnet-sense-keys ("content%1:14:00" "substance%1:10:00" "subject_matter%1:10:00" "content%1:10:00" "message%1:10:00" "meaning%1:09:00" "substance%1:09:00")
 :parent ONT::information
 )

#|
;; success, failure
(define-type ONT::outcome
    :wordnet-sense-keys ("result%1:11:00" "consequence%1:19:00")
    :parent ONT::information-function-object
    :arguments ((:essential ONT::FIGURE)
		)
    )
|#

#|
(define-type ONT::clinical-finding
 :parent ONT::outcome
 :arguments ((:essential ONT::FIGURE)
	     )
 )
|#

;; identification
(define-type ONT::identification
  :wordnet-sense-keys ("identification%1:10:01" "identifier%1:10:00")
  :parent ONT::information-FUNCTION-OBJECT
  :arguments ((:essential ONT::FIGURE ((? lof F::Phys-obj f::abstr-obj)))
	     )
 )

;; pin, isbn
(define-type ont::id-number
  :wordnet-sense-keys ("identification_number%1:10:00")
  :parent ont::identification)

;; ssn
(define-type ONT::ssn
 :wordnet-sense-keys ("social_security_number%1:10:00")
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
  :wordnet-sense-keys ("zip_code%1:10:00")
  :parent ont::location-id
  )

(define-type  ONT::event-defined-by-activity
 :wordnet-sense-keys ("event%1:03:00" "play%1:04:05" "group_action%1:04:00")
 :parent ONT::EVENT-TYPE
 :sem (F::Situation (F::aspect F::dynamic))
 :arguments ((:OPTIONAL ONT::FIGURE)
;	     (:optional ont::content)
             )
 )

(define-type ONT::outcome ;caused-event
 :wordnet-sense-keys ("consequence%1:19:00" "result%1:11:00" "side_effect%1:19:00")
 :parent ONT::EVENT-TYPE
 :sem (F::situation (F::cause (? cause F::agentive F::force)))
 :arguments ((:OPTIONAL ONT::FIGURE)
	     (:OPTIONAL ONT::FORMAL)
             )
 )


;;; The difference between actions and events is that actions have agents
;;; not used JFA except as LF for word ACTION
(define-type ONT::CAMPAIGN ;Action
 :wordnet-sense-keys ("campaign%1:04:02" "campaign%1:11:00" "expedition%1:04:00" "military_operation%1:04:00")
 :parent ONT::EVENT-TYPE
 :sem (F::Situation (F::cause F::Agentive) (F::aspect F::dynamic))
 :arguments ((:OPTIONAL ONT::FIGURE)
             )
 )

(define-type ONT::commercial-activity
 :wordnet-sense-keys ("commerce%1:04:00" "deal%1:04:02" "finance%1:04:00" "shop%2:41:00")
 :parent ONT::activity
 :sem (F::situation (F::cause (? cause F::agentive F::force)))
 )

(define-type ONT::agriculture
 :wordnet-sense-keys ("agriculture%1:04:01")
 :parent ONT::activity
 :sem (F::situation (F::cause (? cause F::agentive F::force)))
 )

(define-type ONT::Situation
 :wordnet-sense-keys ("phenomenon%1:03:00" "world%1:17:02")
 :parent ONT::EVENT-TYPE
 :sem (F::Situation)
 )

(define-type ONT::order-tranquility
 :wordnet-sense-keys ("order%1:26:00")
 :parent ont::situation
 :arguments ((:OPTIONAL ONT::assoc-with)
             )
 )

(define-type ONT::trouble
 :wordnet-sense-keys ("impairment%1:11:00" "harm%1:11:01" "damage%1:11:00" "problem%1:09:00" "trouble%1:09:00" "trouble%1:11:00" "disorder%1:26:00" "danger%1:26:01")
 :parent ont::situation
 :arguments ((:OPTIONAL ONT::assoc-with)
             )
 )

;; recession
(define-type ont::economic-event
:wordnet-sense-keys ("deflation%1:22:00" "depression%1:26:02" "disinflation%1:22:00" "economic_growth%1:22:00" "globalization%1:22:00" "inflation%1:22:00" "market_forces%1:22:00" "recession%1:26:00" "spiral%1:22:00" "supply%1:22:00")
:parent ONT::event-defined-by-activity
)

;; travel
(define-type ONT::travel
 :wordnet-sense-keys ("travel%1:04:00" )
 :parent ONT::event-defined-by-activity
 :sem (F::situation (F::trajectory +))
 )

;; tour
; can tour a house, a museum; doesn't have to be travel
(define-type ont::tour
  :wordnet-sense-keys ("tour%1:04:00" "tour%2:38:00")
  :parent ont::travel ;event-defined-by-activity
  )


;; trip, journey
(define-type ONT::trip
 :wordnet-sense-keys ("journey%1:04:00" "journeying%1:04:00")
 :parent ONT::travel
 :sem (F::situation (F::trajectory +))
 )

;; jaunt
(define-type ont::jaunt
  :wordnet-sense-keys ("jaunt%1:04:00")
  :parent ont::trip
  )

;; round trip
(define-type ONT::round-trip
 :wordnet-sense-keys ("round_trip%1:04:00")
 :parent ONT::trip
 :sem (F::situation (F::trajectory +))
 )

(define-type ont::air-travel
  :wordnet-sense-keys ("air_travel%1:04:00")
  :parent ont::trip
  )

;; flight
(define-type ONT::flight
 :wordnet-sense-keys ("flight%1:04:00")
 :parent ONT::air-travel
 )

(define-type ont::water-travel
  :wordnet-sense-keys ("water_travel%1:04:00")
  :parent ont::trip
  )

(define-type ont::land-travel
  :parent ont::trip
  )

;; voyage
(define-type ont::voyage
  :wordnet-sense-keys ("voyage%1:04:01" "voyage%2:38:00")
  :parent ont::water-travel
  )

;; drive
(define-type ont::driving-trip
  :wordnet-sense-keys ("ride%1:04:00")
  :parent ont::land-travel ;water-travel
  )

;; migration
;(define-type ont::migration
;  :parent ont::trip
;  )

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
    :wordnet-sense-keys("gathering%1:14:00"); "visit%1:04:02")
    :parent ont::located-event
  )

;; 20111005 fire type added for obtw demo
(define-type ont::fire
  :wordnet-sense-keys("fire%1:11:00")
  :parent ont::located-event
  )

(define-type ont::ceremony
  :wordnet-sense-keys("ceremony%1:04:00" "ceremony%1:04:01" "ceremony%1:11:00" "christening%1:04:00" "commencement%1:11:02" "convocation%1:04:00" "funeral%1:11:00");hypernym for "bar_mitzvah%1:11:00" is already here: "ceremony%1:11:00", similarly  inauguration%1:04:00, coronation%1:11:00, initiation%1:11:00, wake%1:04:00, wedding%1:11:00
  :parent ont::gathering-event
  )

(define-type ont::performance-play
  :wordnet-sense-keys("show%1:10:00")
  :parent ont::gathering-event
  )

;; class, course
(define-type ont::instruction-event
  :wordnet-sense-keys("class%1:04:00")
  :parent ont::located-event
  )

;; talk, lecture, demo, presentation
(define-type ont::presentation
    :wordnet-sense-keys("presentation%1:10:00" "presentation%1:10:02" "show%1:04:00") ;"show%1:10:00")
  :parent ont::gathering-event
  )

; talk, lecture, demo, presentation
(define-type ont::exhibition
    :wordnet-sense-keys("exhibition%1:14:00")
    :parent ont::gathering-event
  )

#|
;; idea
(define-type ONT::mental-object
 :parent ONT::mental-construction
;; :sem (F::Abstr-obj (F::container +))
 :arguments ((:OPTIONAL ONT::FIGURE) ;(f::situation (f::information f::mental-construct) (f::cause f::mental)))
	     (:optional ont::GROUND)
	     (:optional ont::FORMAL (f::situation))
             )
 )
|#

(define-type ont::mental-attitude
 :wordnet-sense-keys("mentality%1:09:01")
 :parent ONT::mental-construction
)

(define-type ONT::knowledge-belief
    :wordnet-sense-keys ("knowledge%1:03:00" "know-how%1:09:00")
    :parent ONT::mental-construction
    :arguments ((:OPTIONAL ONT::FIGURE) ;(f::situation (f::information f::mental-construct) (f::cause f::mental)))
		(:optional ont::FORMAL (f::situation)))
    )

(define-type ONT::doctrine
 :wordnet-sense-keys("doctrine%1:09:00" "faith%1:09:00" "ideology%1:09:01")
 :parent ONT::knowledge-belief
 :comment "A system of beliefs"
)

(define-type ONT::illusion
 :wordnet-sense-keys("illusion%1:09:01" "misconception%1:09:00")
 :parent ONT::knowledge-belief
 :comment "misconception"
)

(define-type ONT::understanding
 :wordnet-sense-keys("understanding%1:09:01" "comprehension%1:09:00")
 :parent ONT::knowledge-belief
)

(define-type ont::opinion
 :parent ONT::knowledge-belief
 :wordnet-sense-keys ("opinion%1:10:01" "viewpoint%1:09:00" "belief%1:09:00" "opinion%1:09:00" )
)

(define-type ont::concept-notion
    :wordnet-sense-keys ("thought%1:09:01" "notion%1:09:00" "concept%1:09:00" "conceptualization%1:09:00" "proposition%1:10:00")
    :parent ONT::mental-construction
    :arguments ((:OPTIONAL ONT::FIGURE)
                )
)

(define-type ONT::gist
 ; :wordnet-sense-keys ("kernel%1:09:00" "substance%1:09:01" "core%1:09:00" "center%1:09:00" "centre%1:09:00" 
 ;		       "essence%1:09:00" "gist%1:09:00" "heart%1:09:01" "heart_and_soul%1:09:00" "inwardness%1:09:02" 
 ;		       "marrow%1:09:00" "meat%1:09:00" "nub%1:09:00" "pith%1:09:00" "sum%1:09:00" "nitty-gritty%1:09:00")
 :wordnet-sense-keys ("gist%1:09:00")
 :parent ONT::concept-notion
)

(define-type ONT::FEELING
    :wordnet-sense-keys ("feeling%1:03:00" "mercy%1:07:00" "psychological_state%1:26:00")
    :parent ONT::mental-construction
    :arguments ((:OPTIONAL ONT::FIGURE) ;(f::situation (f::information f::mental-construct) (f::cause f::mental)))
		)
)


;; reason, motivation
(define-type ONT::motive
  :wordnet-sense-keys ("motivation%1:03:00" "reason%1:10:00")
 :parent ONT::mental-construction ;mental-object
 :arguments ((:OPTIONAL ONT::FORMAL) 
;	     (:optional ONT::Associated-information)
;	     (:optional ont::purpose) ;; reason for something
;	     (:optional ont::REASON) ;; reason for something
	     )
 )

;; problem-solving object (formerly ont::plan-object)
;; high-level for problem solving objects
(define-type ONT::ps-object
 :parent ONT::mental-construction
 :sem (F::Abstr-obj (F::container +))
 :arguments ((:optional ont::FIGURE)
	     (:optional ont::formal)
	     (:optional ont::GROUND)
	     )
)

(define-type ONT::requirements
 :parent ont::ps-object
 :wordnet-sense-keys ("term%1:10:02" "provision%1:10:00" "condition%1:10:01" "requirement%1:09:00" "standard%1:09:00" "criterion%1:09:00")
 :comment "reference or rules that must be met to satisfy evaluation"
)

(define-type ont::mental-plan
 :parent ont::ps-object
 :wordnet-sense-keys("plan%1:09:00" "plan_of_action%1:09:00")
 )

(define-type ont::budget
 :parent ont::mental-plan
 :wordnet-sense-keys("budget%1:21:03")
 )

(define-type ont::design-plan
 :parent ont::mental-plan
 :wordnet-sense-keys("design%1:09:01")
 )

;; baseline, guideline
;(define-type ont::standard
; :parent ont::information-function-object
; )
(define-type ONT::standard
 :parent ont::ps-object
 :wordnet-sense-keys ("criterion%1:10:00" "standard%1:10:00")
 :comment "the ideal in terms of which something can be judged"
)

;; ways of doing things, accomplishing tasks
(define-type ONT::procedure
 :parent ONT::ps-object
 ;; CERNL hack:: adding verb roles for verbs tagged as ont::procedure
 :sem (F::Abstr-obj (F::intentional -) (F::container +))
 :wordnet-sense-keys ("procedure%1:10:00" "procedure%1:04:00" "procedure%1:04:02")
 )

(define-type ONT::method
     :wordnet-sense-keys ("manner%1:07:01" "means%1:06:00" "method%1:09:00" "path%1:04:00" "path%1:06:00" "way%1:04:01")
     :parent ONT::ps-object
     )

;; recipe, procedure, plan
(define-type ONT::recipe
 :parent ONT::procedure
 :wordnet-sense-keys ("instruction%1:10:04")
 )

;; http, ssl, imap, etc.
(define-type ont::protocol
 :parent ont::procedure ;; ont::standard
 :wordnet-sense-keys ("protocol%1:10:01")
 )

;; policy
(define-type ONT::policy
 :parent ONT::procedure
 :wordnet-sense-keys ("policy%1:09:00")
 )

;; algorithm, program, keystone correction
;; note that there is ont::computer-program for specific software programs like VM
(define-type ONT::algorithm
 :parent ONT::procedure
 :wordnet-sense-keys ("algorithm%1:09:00")
)

;; process
(define-type ONT::process
 ;:wordnet-sense-keys ("procedure%1:04:00" "process%1:04:00")
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
 :wordnet-sense-keys ("proposal%1:10:00")
 )

;; task-related things that one commits to
;; appointment, schedule, agenda, timeline, campaign
(define-type ONT::commitment
    :wordnet-sense-keys ("commitment%1:04:02")
 :parent ONT::ps-object
 )

(define-type ONT::problem
    :wordnet-sense-keys ("obstacle%1:09:00" "obstacle%1:06:00" "problem%1:10:00" "problem%1:26:00" "challenge%1:26:00" "woe%1:26:00");"problem%1:09:00"
    :parent ONT::ps-object
    )

(define-type ONT::action-defined-by-game
 :wordnet-sense-keys ("turn%1:04:06" "move%1:04:02")
 :parent ONT::event-defined-by-activity
 )

;; lottery, contest
(define-type ont::competition
  :wordnet-sense-keys ("competition%1:11:00")
  :parent ont::event-defined-by-activity
  )

;; game
(define-type ONT::game
 :wordnet-sense-keys ("game%1:04:00" "game%1:04:01")
 :parent ONT::competition
 )

(define-type ONT::sport
 :wordnet-sense-keys ("athletic_game%1:04:00" "athletics%1:04:00" "sport%1:04:00")
 :parent ONT::game
 )

#|
;; football, soccer...
(define-type ONT::athletic-game
 :wordnet-sense-keys ("athletics%1:04:00" "sport%1:04:00")
 :parent ONT::sport
 )
|#

(define-type ONT::court-game
 :parent ONT::sport ;athletic-game
 :wordnet-sense-keys ("court_game%1:04:00")
 )

;; chess
(define-type ONT::board-game
 :wordnet-sense-keys ("board_game%1:04:00")
 :parent ONT::game
 )

;; preference, comparison, contrast
(define-type ONT::comparison
 :parent ONT::ps-object
 :wordnet-sense-keys ("comparison%1:24:00")
 )

;; this involves a selection among alternatives
;; selection, choice, option, alternative
(define-type ONT::Option
 :wordnet-sense-keys ("option%1:09:00" "alternative%1:09:00" "choice%1:09:02")
 :parent ONT::comparison
  )

;; favorite
(define-type ONT::favorite
 :wordnet-sense-keys ("favorite%1:09:00")
 :parent ONT::relation
  )

#|
;; system, agent
(define-type ont::agent
  :parent ont::phys-object
  :sem (F::phys-obj (F::information F::information-content) (f::intentional +))
  )
|#

;; (health care) assistant
(define-type ont::assistant
 :wordnet-sense-keys ("assistant%1:18:00" "helper%1:18:01" "help%1:18:00" "supporter%1:18:01")
  :parent ont::PERSON-DEFINED-BY-ACTIVITY ;professional ;agent
  )

#|
(define-type ont::harmful-agency
  :parent ont::agent
  )
|#

(define-type ONT::computer-software-related ;computer-software
  :parent ONT::mental-construction ;representation
;  :sem (F::Abstr-obj (F::information F::information-content))
  :sem (F::Abstr-obj (f::origin f::artifact) (F::information F::mental-construct))
  :wordnet-sense-keys ("computer_code%1:10:00") 
  )

;; autofill, autocomplete, colorsync, etc.
(define-type ONT::software-feature
 :wordnet-sense-keys ("expose%1:10:00" "unmasking%1:10:00")
  :parent ONT::computer-software-related ;computer-software
  )

;; specific programs like VM
(define-type ONT::computer-software ;computer-program
 :wordnet-sense-keys ("applications_programme%1:10:00" "application_program%1:10:00" "application%1:10:01" "program%1:10:02" "programme%1:10:02" "computer_program%1:10:00" "computer_programme%1:10:00")
  :parent ONT::computer-software-related
  )

(define-type ONT::web-browser
  :parent ONT::computer-system ;computer-software
  :wordnet-sense-keys ("browser%1:10:00")
)

(define-type ONT::Linguistic-object
 ;:wordnet-sense-keys ("language_unit%1:10:00" "linguistic_unit%1:10:00")
 :parent ONT::mental-construction
; :sem (F::Abstr-obj (F::information F::data))
 )

;; digit
(define-type ONT::number-object
 :parent ONT::number
 :sem (F::Abstr-obj (F::information F::data))
 :wordnet-sense-keys ("digit%1:23:00")
 )

(define-type ont::language
    :wordnet-sense-keys ("language%1:10:00" "dialect%1:10:00")
 :parent ont::linguistic-object
 )

#|
(define-type ont::question
    :wordnet-sense-keys ("question%1:10:00")
    :parent ont::linguistic-object
    )
|#

;; prefix, suffix
(define-type ont::linguistic-component
 :parent ont::linguistic-object
 :arguments ((:REQUIRED ONT::FIGURE (F::abstr-obj (f::information f::information-content))))
 :wordnet-sense-keys ("conjugation%1:14:01" "declension%1:14:00" "grammatical_category%1:10:00" "lexeme%1:10:00" "linguistic_relation%1:24:00" "linguistic_unit%1:10:00" "morpheme%1:10:00" "phone%1:10:00" "syllable%1:10:00")
 )

(define-type ont::question
    :wordnet-sense-keys ("question%1:10:00")
    :parent ont::linguistic-component ;linguistic-object
    )

;; html, java, etc.
(define-type ont::computer-language
 :parent ont::language
 :wordnet-sense-keys ("markup_language%1:10:00" "programming_language%1:10:00")
)

;; mbox, jpeg, doc, txt, etc.
(define-type ont::file-format
 :parent ont::computer-language
 :wordnet-sense-keys ("mpeg%1:04:00")
)

;;; there is ont::graphic-symbol; this should be related somehow
(define-type ONT::punctuation
    :wordnet-sense-keys ("punctuation%1:10:00")
    :parent ONT::text-representation ;:parent ONT::linguistic-object
 )

;; letters of the alphabet
(define-type ONT::letter-symbol
    :wordnet-sense-keys ("letter%1:10:01")
    :parent ONT::text-representation ;:parent ONT::linguistic-object
; :sem (F::Abstr-obj (F::information F::data))
    :comment "a textual symbol to represent information"
    )

#|
;; letters of the alphabet
(define-type ONT::grammatical-category
    :wordnet-sense-keys ("grammatical_category%1:10:00" "declension%1:14:00" "conjugation%1:14:01"
							"linguistic_relation%1:24:00")
    :parent ONT::linguistic-object
    )
|#

;; alpha, beta, ...
(define-type ONT::greek-letter-symbol
 :parent ONT::letter-symbol
 :wordnet-sense-keys ("alpha%1:10:00" "beta%1:10:00" "chi%1:10:00" "delta%1:10:00" "epsilon%1:10:00" "eta%1:10:00" "gamma%1:10:00" "iota%1:10:00" "kappa%1:10:00" "lambda%1:10:00" "mu%1:10:00" "nu%1:10:00" "omega%1:10:00" "omicron%1:10:00" "phi%1:10:00" "pi%1:10:00" "psi%1:10:00" "rho%1:10:00" "sigma%1:10:00" "tau%1:10:00" "theta%1:10:00" "upsilon%1:10:00" "xi%1:10:00" "zeta%1:10:00")
)

#|
(define-type ONT::COST-RELATION
 :parent ONT::predicate
 :sem (F::Abstr-obj (f::scale ont::money-scale) (f::information f::information-content))
 :arguments ((:REQUIRED ont::FIGURE)
	     (:REQUIRED ONT::GROUND (F::Abstr-obj (F::Scale Ont::money-scale) (f::object-function f::currency)))
             )
 )
|#

;; I want to buy a computer for under 1000 dollars
;; we want vp attachment here, but not to static verbs
(define-type ONT::PURCHASE-COST
  :parent ONT::predicate ;COST-RELATION
  :arguments ((:REQUIRED ont::FIGURE (f::situation (f::aspect f::dynamic)))
	      (:REQUIRED ONT::GROUND (F::Abstr-obj (F::Scale Ont::money-scale) (f::object-function f::currency)))
             )
 )

(define-type ONT::TRAJECTORY
    :parent ONT::tangible-abstract-object
    :comment "the history of some value over time"
    :wordnet-sense-keys ("trajectory%1:19:00")
    :arguments ((:REQUIRED ont::FIGURE ((? lo f::phys-obj f::abstr-obj))))
    )




;;
(define-type ONT::VALUE-COST
 :wordnet-sense-keys ("change%1:21:02")
 :parent ONT::tangible-abstract-object
 :arguments (
	     ;(:REQUIRED ONT::FIGURE (F::Abstr-obj (F::Scale Ont::money-scale) (f::object-function f::currency)))
	     (:REQUIRED ont::FIGURE ((? lo f::phys-obj f::abstr-obj f::situation))) ; situation: the cost of building a house
	     (:REQUIRED ONT::EXTENT (F::Abstr-obj (F::Scale Ont::money-scale) (f::object-function f::currency)))
	     )
 )

(define-type ont::expense
 :wordnet-sense-keys ("cost%1:21:00")
  :parent ont::value-cost
  )

(define-type ont::revenue
 :wordnet-sense-keys ("financial_gain%1:21:00" "return%1:21:00" "bonus%1:21:00" "salary%1:21:00")
  :parent ont::value-cost
  )

;; unique lf for price
(define-type ONT::PRICE
 :parent ONT::VALUE-COST
 :arguments ((:REQUIRED ont::FIGURE ((? lo f::phys-obj f::abstr-obj)))
	      (:REQUIRED ONT::EXTENT (F::Abstr-obj (F::Scale Ont::money-scale) (f::object-function f::currency)))
            )
 :wordnet-sense-keys ("charge%1:21:02" "price%1:07:00" "price%1:07:01" "price%1:07:02" "price%1:21:00" "price%1:21:02")
 )

#|
;MOVING TO domain-and-attribute-types.lisp
;; unique lf for rate, as in gsa rate
(define-type ont::charge-per-unit
  :parent ont::ratio-scale ;value-cost
  )
|#

;; for account, grant, credit card
;; can we find a way to distinguish between bill (put it on my bill) and grant (put it on my grant)?
(define-type ONT::ACCOUNT
 :parent ont::function-object ;value-cost
; :parent ONT::ABSTRACT-OBJECT-nontemporal
 :sem (F::Abstr-obj (F::Measure-function F::term)  (f::object-function f::currency) (f::scale ont::money-scale))
  :arguments (
 ;; accounts can belong to individuals, organizations or projects
 (:essential ont::FIGURE ((? lof f::phys-obj f::abstr-obj))))
 :wordnet-sense-keys ("account%1:26:00" "financial_aid%1:21:00" "fund%1:21:01" "funding%1:21:00" "grant%1:10:00") 
)

;; bill, tab
(define-type ONT::ACCOUNT-PAYABLE
 :wordnet-sense-keys ("bill%1:10:01" "account%1:10:02" "invoice%1:10:00")
 :parent ONT::ACCOUNT
 :sem (F::Abstr-obj (f::scale ont::money-scale))
 )

;; lack, shortage
(define-type ONT::LACK
 :wordnet-sense-keys ("deficiency%1:07:00" "shortage%1:07:00")
 :parent ont::situation
 :arguments ((:REQUIRED ONT::FIGURE)
             )
 )

#|
(define-type ONT::sense
;; :parent ONT::ANY-SEM
    :parent ont::abstract-function
;; F::proposition is true or false, this does not apply to sense
;; :sem (F::proposition (F::information F::information-content))
 )
|#

#| ; merged with medical-event
;;; (health) care, treatment
(define-type ONT::treatment
 :wordnet-sense-keys ("care%1:04:01" "attention%1:04:01" "aid%1:04:01" "tending%1:04:00" "regimen%1:09:00::" "regime%1:09:00::")
 :parent ONT::procedure
 )
|#

#|
;; diagnostics
(define-type ont::medical-test
  :parent ont::procedure
  )
|#

;; can be either treatment or test -- not sure where to classify but adding WN mappings
(define-type ont::medical-procedure
  :wordnet-sense-keys  ("checkup%1:04:00" "incision%1:04:00" "medical_procedure%1:04:00" "section%1:04:00" "surgical_incision%1:04:00")
  :parent ont::procedure
  )

(define-type ont::medical-diagnostic
  :wordnet-sense-keys ("diagnostic_procedure%1:04:00" "diagnostic_technique%1:04:00")
  :parent ont::medical-procedure ;medical-test
  )

(define-type ONT::CONSTRAINT
    :wordnet-sense-keys ( "restriction%1:09:00" "limitation%1:09:00" "constraint%1:06:00"
"limit%1:23:00")
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
 :wordnet-sense-keys ("name%1:10:00")
 )

(define-type ONT::FIRSTNAME
 :parent ONT::NAME
 :sem (F::Abstr-obj (F::information F::information-content))
 :arguments ((:ESSENTIAL ONT::FIGURE )
            )
 :wordnet-sense-keys ("first_name%1:10:00")
 )

(define-type ONT::LASTNAME
 :parent ONT::NAME
 :sem (F::Abstr-obj (F::information F::information-content))
 :arguments ((:ESSENTIAL ONT::FIGURE )
             )
 :wordnet-sense-keys ("surname%1:10:00")
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
 :wordnet-sense-keys ("designation%1:10:00" "highness%1:18:00" "sir%1:18:01" "sir%1:18:00")
 )

(define-type ONT::SIGNATURE
 :wordnet-sense-keys ("signature%1:10:00")
 :wordnet-sense-keys ("signature%1:10:00")
 :parent ONT::name
 )

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

(define-type ont::definition
 :parent ont::information-function-object
 :arguments ((:essential ont::FIGURE))
 :wordnet-sense-keys ("definition%1:10:00")
)

;; changed parent to ont::discipline (from abstract-object)
;; 2005.04/20 Added by Myrosia to handle words like algebra, mathematics etc.
(define-type ONT::science-discipline
    :wordnet-sense-keys ("science%1:09:00")
    :parent ONT::discipline
    :sem (F::Abstr-obj (F::container +)) ;; why is this container +?
    )

#|
;; acid wash
(define-type ONT::food-prep-process
 :parent ONT::process
 )
|#

(define-type ONT::eating-plan
 :parent ONT::RECIPE
 :wordnet-sense-keys ("diet%1:04:00") 
)

;; abnormality, irregularity, anomaly
;; moved under domain. jena Aug 2017 see not-typical-scale
#|
(define-type ONT::abnormality
 :wordnet-sense-keys ("abnormality%1:04:00" "irregularity%1:04:00")
 :parent ONT::event-type
 :sem (F::Situation (F::aspect F::dynamic))
 :arguments ((:OPTIONAL ONT::FIGURE)
             )
 )
|#

;; We need f::situation counterparts for medical words classified as ont::treatment or ont::diagnostic for i2b2 because we don't have multiple inheritance
;; surgery
(define-type ONT::medical-event
 :wordnet-sense-keys ("care%1:04:01" "attention%1:04:01" "aid%1:04:01" "tending%1:04:00" "regimen%1:09:00" "regime%1:09:00" "immunization%1:04:00")
 :parent ONT::event-defined-by-activity
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


;;;==== ONT::ATTRIBUTE ====                                                                                         

(define-type ONT::attribute
 :wordnet-sense-keys ("dimension%1:09:00" "attribute%1:09:00" "property%1:09:00" "property%1:07:00" "quality%1:07:00")
 :parent ont::abstract-object-nontemporal
 :arguments ((:OPTIONAL ONT::FIGURE ((? lo f::phys-obj f::abstr-obj)))
             )
 )

(define-type ont::condition-favorability
 :parent ont::attribute
)

(define-type ont::favorable-condition
 :parent ont::condition-favorability
 :wordnet-sense-keys ("advantage%1:07:00" "advantage%1:07:01" "asset%1:07:00" "strength%1:07:01")
 ;asset, plus, strength advantage
)

(define-type ont::not-favorable-condition
 :parent ont::condition-favorability
 :wordnet-sense-keys ("weak_point%1:07:00" "liability%1:07:00" "disadvantage%1:07:00" "detriment%1:11:00" "weakness%1:26:00")
 ; detriment, liability, weakness, minus, disadvantage
)

(define-type ONT::body-property
 :parent ont::attribute
 :arguments ((:OPTIONAL ONT::FIGURE (f::phys-obj (f::origin f::living)))
             )
 )


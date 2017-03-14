(in-package :om)

; > starts a block
; < ends the block
; use `[code]` to use lisp syntax in code (optional, but makes eliminating commented code easier)

(define-type ONT::property-val
 :parent ONT::domain-property
 :sem (F::ABSTR-OBJ (:required (F::CONTAINER -) (F::INFORMATION -) (f::intentional -))
		    (:default ;(f::scale -)
		     (f::intensity -) (f::orientation -)))
 :arguments ((:REQUIRED ONT::FIGURE)
	     (:optional ONT::FORMAL  (f::situation))
	     (:optional ONT::NEUTRAL1)
             (:optional ONT::NEUTRAL ((? pvt F::Phys-obj f::abstr-obj f::situation)))
					;	     (:optional ont::Purpose (f::situation (f::aspect f::dynamic)))
	     (:optional ONT::Affected ((? aff f::phys-obj f::abstr-obj f::situation)))
					;	     (:optional ONT::Purpose-implicit ((? pi f::phys-obj f::abstr-obj f::situation)))
	     (:optional ONT::REASON ((? pi f::phys-obj f::abstr-obj f::situation)))
	     (:OPTIONAL ONT::GROUND)
	     (:optional ont::standard)
	     (:optional ont::norole)
             )
 )

(define-type ONT::living-val
 :parent ONT::property-val
 :wordnet-sense-keys  ("live%3:00:00" "dead%3:00:01"  "dead%3:00:02")
 :arguments ((:ESSENTIAL ONT::FIGURE (F::phys-obj (f::intentional +) (f::type ont::organism))
	     ))
 )

(define-type ONT::dead
 :parent ONT::living-val
 :wordnet-sense-keys  ("dead%3:00:01"  "dead%3:00:02")
 :arguments ((:ESSENTIAL ONT::FIGURE (F::phys-obj (f::intentional +) (f::type ont::organism))
	     ))
 )

(define-type ONT::alive
 :parent ONT::living-val
 :wordnet-sense-keys  ("live%3:00:00")
 :arguments ((:ESSENTIAL ONT::FIGURE (F::phys-obj (f::intentional +) (f::type ont::organism))
	     ))
 )

(define-type ONT::behavioral-property
 :parent ONT::property-val
 :arguments ((:ESSENTIAL ONT::FIGURE (F::phys-obj (f::intentional +) (f::type ont::organism))
	     ))
 )

;; Experiencer is used to formal
(define-type ONT::habituated
 :parent ONT::property-val
 :arguments ((:ESSENTIAL ONT::affected (F::phys-obj))
             (:ESSENTIAL ONT::Formal)
             )
 )

(define-type ONT::physical-property-val
 :parent ONT::property-val
; many physical adjectives can be used on non-physical objects
; :arguments ((:REQUIRED ONT::OF (F::phys-obj))
;             )
 )

;;; big/large/small
(define-type ONT::Size-val
 :parent ONT::physical-property-val
 :sem (F::abstr-obj (F::scale ont::size-scale))
 )

;;; process-related adjectives
(define-type ONT::process-val
 :parent ONT::property-val
 )

;;; secure, safe
(define-type ONT::safety-val
 :parent ONT::process-val
 )

;; classified, unclassified
(define-type ONT::classification-val
 :parent ONT::property-val
 )

;; mid, middle, midway, halfway
(define-type ONT::stage-val
 :parent ONT::process-val
 )

(define-type ONT::substantial-property-val
    :comment "properties having to do with physical substance"
    :parent ONT::physical-property-val
    )

;;; noisy (data, signal)
(define-type ONT::interference-val
 :parent ONT::substantial-property-val
 )

(define-type ont::configuration-property-val
 :parent ONT::physical-property-val
 )

;; 20111017 added for obtw demo (word for type)
(define-type ont::electrical
 :parent ONT::substantial-property-val
 )

;; This is for "open" and "closed" as adjectives
;; In all senses - we cannot disambiguate between availability and anything else
;; Myrosia 2004/04/27
(define-type ont::openness-val
    :parent ont::configuration-property-val
    :arguments (
;		(:optional ONT::Purpose))
		(:optional ONT::REASON))
    )

;;; This is for speed values - fast, slow, etc
(define-type ONT::Speed-val
 :parent ONT::process-val
 :sem  (F::abstr-obj (F::scale ont::speed-scale))
 :arguments ((:REQUIRED ONT::FIGURE ((? type F::phys-obj F::situation F::abstr-obj)))   ;; e.g., "rate" is an abstract object
             )
 )

(define-type ONT::quantity-related-property-val
 :parent ONT::property-val
 :arguments ((:optional ONT::GROUND)
	     (:optional ONT::STANDARD)
             )
 )

(define-type ONT::relative-quantity-val
 :parent ONT::quantity-related-property-val
 )

(define-type ONT::measure-related-property-val
 :parent ONT::quantity-related-property-val
 )

(define-type ONT::number-related-property-val
 :parent ONT::quantity-related-property-val
 )

;; single, dual, lone, twin, only
(define-type ONT::CARDINALITY-VAL
 :parent ONT::number-related-property-val
 )

;; urban, rural
(define-type ONT::urban-VAL
 :parent ONT::property-val
 )

;; friendly, affectionate, kind, mean, considerate
;; no experiencer role; currently no distinction between human and non-human ont::of
(define-type ONT::social-interaction-val
 :parent ONT::property-val
 :arguments ((:required ont::FIGURE ((? lof f::abstr-obj f::phys-obj f::situation))
	     ))
 )

(define-type ONT::automatic
 :parent ONT::property-val
 :arguments ((:required ont::FIGURE ((? lof f::abstr-obj f::phys-obj f::situation))
	     ))
 )

;; responsible, irresponsible
(define-type ONT::responsibility-val
 :parent ONT::social-interaction-val
 :arguments ((:required ont::FIGURE ((? lof f::abstr-obj f::phys-obj f::situation)))
	     (:optional ont::GROUND))
 )

;; interesting, boring
(define-type ONT::FASCINATION-VAL
    :parent ONT::property-val
    )

(define-type ont::body-related-property-val
 :parent ont::physical-property-val
 )

;; digestive, immune,                                                                                                                                                                                                                       
(define-type ONT::body-system-val
 :parent ONT::body-related-property-val ;; of arguments can be more than just human; cardiac care; intestinal disturbance                                                                                                                   
 )


(define-type ONT::body-part-val
 :parent ONT::body-system-val ;; of arguments can be more than just human; cardiac care; intestinal disturbance                                                                                                                             
 )


(define-type ont::animal-property-val
    :parent ont::body-related-property-val
    :arguments ((:optional ont::neutral (f::phys-obj (f::origin (? og f::non-living))))
		(:optional ont::experiencer (f::phys-obj (f::origin (? og2 f::human f::non-human-animal))))
;                (:optional ont::content ((? cont f::abstr-obj f::situation)))
                (:optional ont::formal ((? cont f::abstr-obj f::situation)))
                )
    )

;; hungry, sleepy
(define-type ONT::body-property-val
 :parent ONT::animal-PROPERTY-VAL
 )

;; confused, surprised, happy
(define-type ont::psychological-property-val
    :parent ont::animal-property-val
    :arguments ( ;; stimulus is what provokes the emotion - I am afraid of dogs / storms
		;;(:optional ont::stimulus ((? stm f::phys-obj f::situation f::abstr-obj)))
		;; the object that is involved in a situation, but which is not a stimulus directly
		;; for example, I am afraid for her, for the project
		;;(:optional ont::formal (f::situation f::phys-obj f::abstr-obj)))
		)
    )

;; aware (of x)
(define-type ont::awareness-val
  :parent ont::psychological-property-val
  )

;; happy, sad, gloomy...
(define-type ONT::emotional-val
  :parent ONT::psychological-property-val
 )

;; smart, (un)intelligent
(define-type ONT::INTELLIGENCE-VAL
 :parent ONT::psychological-property-val
 )

;; (un)ambitious, bold
(define-type ONT::boldness-VAL
 :parent ONT::psychological-PROPERTY-VAL
 )

;; careful, cautious, reckless
(define-type ONT::attention-VAL
 :parent ONT::psychological-property-val
 )

(define-type ont::spatial
; :parent ont::abstract-object
 :parent ont::property-val
 :arguments ((:OPTIONAL ONT::FIGURE ((? of F::Phys-obj F::Situation f::abstr-obj)))
	     (:OPTIONAL ONT::GROUND ((? val F::Phys-obj F::Situation f::abstr-obj)))
	     ;(:OPTIONAL ONT::FIGURE)
	     ;(:OPTIONAL ONT::GROUND)
             )
 )

;; circular, direct
(define-type ONT::ROUTE-TOPOLOGY-VAL
  :wordnet-sense-keys ("straight%3:00:01")
 :parent ONT::spatial
 :sem (F::abstr-obj (:required)(:default (F::gradability -)))
 :arguments ((:REQUIRED ONT::FIGURE (F::phys-obj (F::spatial-abstraction (? sab F::line F::strip))))
             )
 )

;; blocked, congested, impassable -- for routes
(define-type ONT::FLOW-VAL
 :parent ONT::configuration-PROPERTY-VAL
 :sem (F::abstr-obj (:required)(:default (F::gradability -)))
 :arguments ((:REQUIRED ONT::FIGURE (F::phys-obj (F::spatial-abstraction (? sab F::line F::strip))))
             )
 )

(define-type ONT::obstructed
 :parent ONT::flow-val
 )

(define-type ONT::unobstructed
 :parent ONT::flow-val
 )

(define-type ont::temporal
 :parent ONT::property-val
 )

;; old, young
(define-type ONT::age-VAL
  :parent ONT::temporal
  :sem (F::abstr-obj (F::scale ont::age-scale))
;  :sem (F::abstr-obj (F::scale ont::duration-scale))
 )

;; for adjectives concerning a linear dimension: tall, fat, short, thick
;; compare also linear-d: length, width, height
(define-type ONT::linear-val
; :parent ONT::spatial
 :parent ONT::property-val
 :sem (F::abstr-obj (F::scale ont::linear-scale))
 :arguments ((:OPTIONAL ONT::FIGURE ((? of F::Phys-obj F::Situation f::abstr-obj)))
	     (:OPTIONAL ONT::GROUND ((? val F::Phys-obj F::Situation f::abstr-obj)))
             )
 )

;; physical properties which humans (or animals?) have sensory receptors for
(define-type ONT::sensory-property-val
 :parent ONT::physical-property-val
 )

;; sight
(define-type ONT::visible-property-val
 :parent ONT::sensory-property-val
 )

;; sound
(define-type ONT::audible-property-val
 :parent ONT::sensory-property-val
 )

;; smell
(define-type ONT::smellable-property-val
 :parent ONT::sensory-property-val
 )

;; taste
(define-type ONT::tastable-property-val
 :parent ONT::sensory-property-val
 )

;; touch
(define-type ONT::tangible-property-val
 :parent ONT::sensory-property-val
 )

;; hot, cold
(define-type ONT::temperature-val
 :parent ONT::tangible-PROPERTY-VAL
 :sem (F::abstr-obj (F::scale ont::temperature-scale))
 )

;; sunny, windy, cloudy, breezy
(define-type ONT::atmospheric-val
 :parent ONT::substantial-PROPERTY-VAL
 :sem (F::abstr-obj)
 )

;; heavy
(define-type ONT::weight-val
 :parent ONT::physical-PROPERTY-VAL
 :sem (F::abstr-obj (F::scale ont::weight-scale))
 )

(define-type ONT::heavy
 :parent ONT::weight-val
 :sem (F::abstr-obj (F::scale ont::weight-scale))
 )

(define-type ONT::overweight
 :parent ONT::heavy
 )

;; dim, light, dark
(define-type ONT::light-val
 :parent ONT::visible-PROPERTY-VAL
 :sem (F::abstr-obj (F::scale ont::luminosity-scale))
 )

(define-type ont::physical-discrete-property-val
  :parent ont::physical-property-val
  )

(define-type ONT::orientation-val
 :parent ONT::spatial
  :arguments ((:OPTIONAL ONT::GROUND (F::phys-obj))
             )
 )

(define-type ONT::COLOR-VAL
 :parent ONT::visible-PROPERTY-VAL
 :sem (F::Abstr-obj (F::MEasure-function F::VALUE) (f::scale ont::color-scale))
 :arguments ((:OPTIONAL ONT::FIGURE (F::phys-obj))  ; to distinguish between steps as steps in a plan and steps in a staircase
             )
 )

(define-type ONT::red
 :parent ONT::color-VAL
 )

(define-type ONT::blue
 :parent ONT::color-VAL
 )

(define-type ONT::green
 :parent ONT::color-VAL
 :sem (F::Abstr-obj (F::scale ONT::GREEN*1--07--00))
 )

(define-type ONT::yellow
 :parent ONT::color-VAL
 :sem (F::Abstr-obj (F::scale ONT::YELLOW*1--07--00))
 )

(define-type ONT::orange
 :parent ONT::color-VAL
  :sem (F::Abstr-obj (F::scale ONT::ORANGE*1--07--00))
  )

(define-type ONT::purple
 :parent ONT::color-VAL
  :sem (F::Abstr-obj (F::scale ONT::PURPLE*1--07--00))
 )

(define-type ONT::black
 :parent ONT::color-VAL
  :sem (F::Abstr-obj (F::scale ONT::BLACKNESS*1--07--00))
 )

(define-type ONT::brown
 :parent ONT::color-VAL
  :sem (F::Abstr-obj (F::scale ONT::BROWNNESS*1--07--00))
 )

(define-type ONT::white
 :parent ONT::color-VAL
  :sem (F::Abstr-obj (F::scale ONT::white*1--07--00))
 )

(define-type ONT::gold
 :parent ONT::color-VAL
  :sem (F::Abstr-obj (F::scale ONT::gold*1--07--00))
 )

(define-type ONT::silver
 :parent ONT::color-VAL
  :sem (F::Abstr-obj (F::scale ONT::silver*1--07--00))
 )

(define-type ONT::magenta
 :parent ONT::color-VAL
  :sem (F::Abstr-obj (F::scale ONT::magenta*1--07--00))
 )

(define-type ONT::pink
 :parent ONT::color-VAL
  :sem (F::Abstr-obj (F::scale ONT::pink*1--07--00))
 )

(define-type ONT::tan
 :parent ONT::color-VAL
 )

(define-type ONT::gray
 :parent ONT::color-VAL
  :sem (F::Abstr-obj (F::scale ONT::greyness*1--07--00))
 )

(define-type ONT::SHAPE-VAL
  :parent ONT::spatial
 :sem (F::Abstr-obj (F::Measure-function F::VALUE))
 )

(define-type ONT::round-val
 :parent ONT::shape-VAL
  :sem (F::Abstr-obj (F::scale ONT::roundness*1--07--00))
 )

(define-type ONT::square-val
 :parent ONT::shape-VAL
  :sem (F::Abstr-obj (F::scale ONT::squareness*1--07--00))
 )

;; smooth, rough, soft, hard
(define-type ONT::TEXTURE-VAL
 :parent ONT::tangible-PROPERTY-VAL
 :sem (F::Abstr-obj (F::MEasure-function F::VALUE))
 )

(define-type ONT::soft-VAL
 :parent ONT::texture-VAL
 :sem (F::Abstr-obj (F::scale ONT::softness*1--07--00))
 )

(define-type ONT::hard-VAL
 :parent ONT::texture-VAL
 :sem (F::Abstr-obj (F::scale ONT::hardness*1--07--00))
 )

(define-type ONT::smooth-VAL
 :parent ONT::texture-VAL
 :sem (F::Abstr-obj (F::scale ONT::smoothness*1--07--00))
 )

;; loud, soft, quiet
(define-type ONT::loudness-VAL
 :parent ONT::audible-PROPERTY-VAL
 :sem (F::Abstr-obj (F::MEasure-function F::VALUE))
 :arguments ((:required ont::FIGURE ((? lof f::phys-obj f::situation)))) ;; an event can be loud, e.g. barking
 )

;; imported, domestic
(define-type ONT::origin-VAL
 :parent ONT::PHYSICAL-DISCRETE-PROPERTY-VAL
 :sem (F::Abstr-obj (F::MEasure-function F::VALUE))
 )

(define-type ont::persistence-val
 :parent ont::process-val
 :sem (F::Abstr-obj (F::scale ONT::TIME-MEASURE-SCALE) (F::TIME-SCALE F::INTERVAL))
 )

(define-type ont::persistent
 :wordnet-sense-keys ("persistent%5:00:00:continual:00" "permanent%3:00:00" "lasting%5:00:00:long:02" "lasting%5:00:00:stable:00")
 :parent ont::persistence-val
 )

(define-type ont::temporary
 :wordnet-sense-keys ("temporary%3:00:00" "transient%5:00:00:impermanent:00" "impermanent%5:00:00:finite:00")
 :parent ont::persistence-val
 )

;; continuous, uninterrupted, can be either time or space dimensionality
(define-type ont::continuous-val
 :parent ont::process-val
 )

(define-type ont::continuous
 :wordnet-sense-keys ("continuous%3:00:01")
 :parent ont::continuous-val
 )

(define-type ont::discontinuous
 :wordnet-sense-keys ("discontinuous%3:00:01")
 :parent ont::continuous-val
 )

;; wet, dry
(define-type ONT::dampness-VAL
 :parent ONT::substantial-property-val
 :arguments ((:REQUIRED ONT::FIGURE ((? lof f::phys-obj f::situation)))) ;; dry towel, a dry cough
 )

;; consecutive, sequential, groups of ordered items
;; didn't use ordered-domain here because these words describe the ordered
;; nature of the objects, but not the domain itself
(define-type ont::ordered-val
 :parent ont::process-val
 )

;; homogeneous, uniform
(define-type ont::homogeneous-val
 :parent ont::configuration-property-val
 )

;; consistent in the logical sense
;; consistent, inconsistent
(define-type ont::consistent-val
 :parent ont::property-val
 :arguments ((:REQUIRED ONT::FIGURE ((? lof f::abstr-obj f::situation)))
	     (:REQUIRED ONT::GROUND ((? vl f::abstr-obj f::situation)))
	     )
 )

(define-type ont::consistent
 :wordnet-sense-keys ("consistent%3:00:00" "consistent%3:00:01")
 :parent ont::consistent-val
 )

(define-type ont::inconsistent
 :wordnet-sense-keys ("inconsistent%3:00:00" "inconsistent%5:00:00:irreconcilable:00" )
 :parent ont::consistent-val
 )

;; still, motionless, static, dynamic
(define-type ont::motion-val
 :parent ont::process-val
 :arguments ((:REQUIRED ONT::FIGURE (F::phys-obj))
             )
 )

(define-type ONT::GEO-FEATURE-VAL
 :parent ONT::spatial
 )

;; internal, external, inner, outer, onsite facilities
(define-type ONT::LOCATION-VAL
 :parent ONT::spatial
 :arguments ((:REQUIRED ONT::FIGURE ((? lof f::phys-obj f::situation f::abstr-obj))))
 )

(define-type ONT::MIDDLE-LOCATION-VAL
  :wordnet-sense-keys ("middle%5:00:00:intermediate:00" "middle%5:00:01:central:01" "middle%3:00:00")
 :parent ONT::LOCATION-VAL
 )

(define-type ONT::TOP-LOCATION-VAL
  :wordnet-sense-keys ("top%3:00:00")
 :parent ONT::LOCATION-VAL
 )

(define-type ONT::BOTTOM-LOCATION-VAL
  :wordnet-sense-keys ("bottom%3:00:00")
 :parent ONT::LOCATION-VAL
 )

(define-type ONT::SIDE-LOCATION-VAL
  :wordnet-sense-keys ("side%3:00:00")
 :parent ONT::LOCATION-VAL
 )

;; these are cardinal directions: northern, northeastern
(define-type ONT::MAP-LOCATION-VAL
 :parent ONT::spatial
 )

(define-type ONT::Discrete-property-val
 :parent ONT::PROPERTY-VAL
 :sem (F::Abstr-obj (F::gradability -))
 )

;; inbound, outbound
(define-type ONT::DIRECTION-VAL
 :parent ONT::spatial
 )

;; next, previous, last, penultimate, etc.
(define-type ONT::Sequence-val
 :parent ONT::process-val
 :sem (F::Abstr-obj (F::gradability -))
 :arguments ((:optional ONT::GROUND)
            )
 )

(define-type ONT::sequence-val-next
 :parent ONT::sequence-val
 )

;;; adjectives like remaining/left over/etc
(define-type ONT::PART-WHOLE-VAL
 :parent ONT::physical-PROPERTY-VAL
 :sem (F::Abstr-obj)
 :arguments ((:optional ONT::GROUND)
             )
 )

(define-type ONt::whole-complete
    :wordnet-sense-keys  ("whole%3:00:00")
    :parent  ONT::PART-WHOLE-VAL
)

(define-type ONt::partial-incomplete
    :wordnet-sense-keys ("incomplete%3:00:00")
    :parent  ONT::PART-WHOLE-VAL
)

;;; deliberate, on purpose
(define-type ONT::INTENTIONALITY-VAL
 :parent ONT::DISCRETE-PROPERTY-VAL
 )

;; incremental, step by step
(define-type ont::incremental-val
  :parent ont::process-val
  )

;; dependable, reliable, unpredictable
(define-type ONT::CERTAINTY-VAL
 :parent ONT::property-val
 )

;; retail, wholesale
(define-type ONT::COMMERCE-VAL
 :parent ONT::property-val
 )

(define-type ONT::actuality-VAL
 :parent ONT::physical-property-val
 :arguments ((:REQUIRED ONT::FIGURE (F::proposition (F::information F::mental-construct)))
             )
 )

;; real, actual
(define-type ONT::actual
 :parent ONT::actuality-val
 )

;; fake, imaginary, hypothetical
;; see also ont::evaluation-val?
(define-type ONT::nonactual
 :parent ONT::actuality-val
 )

;; for frequency adjectives that modify event nouns: regular, frequent, daily, monthly, weekly
(define-type ONT::frequency-val
 :parent ONT::PROCESS-VAL
 :sem (F::abstr-obj)
 ;; f::situation restriction is too strong
 :arguments ((:REQUIRED ONT::FIGURE) ;;(F::situation)) ;; weekly meeting; daily vitamin; daily routine/practice
             )
 )

;;; Myrosia 09/23/03 - a very bad name, really. This is for adjectives like "confused", "mixed up", etc - changed LF_Mistaken to LF_Correctness
(define-type ONT::Correctness-VAL
 :parent ONT::PROPERTY-VAL
 )

(define-type ONT::precision-VAL
 :parent ONT::PROPERTY-VAL
 )

;; familiar, known
(define-type ont::familiarity-val
  :parent ont::property-val
  ;;:arguments ((:optional ont::stimulus))
  )

;; unfamiliar, unknown
(define-type ont::unfamiliarity-val
  :parent ont::property-val
  ;;:arguments ((:optional ont::stimulus))
  )

;; typical, normal, usual
(define-type ONT::typicality-VAL
 :parent ONT::property-VAL
 )

;; atypical, unusual, weird, funky, strange
(define-type ONT::atypicality-VAL
 :parent ONT::property-VAL
 )

;; unique
(define-type ONT::uniqueness-VAL
 :parent ONT::number-related-PROPERTY-VAL
 )

;; new,fresh, innovative
(define-type ONT::novelty-VAL
 :parent ONT::PROPERTY-VAL
 )

;; conventional, traditional
(define-type ONT::conventionality-VAL
 :parent ONT::PROPERTY-VAL
 )

;; fancy, special
(define-type ONT::specialness-VAL
 :parent ONT::PROPERTY-VAL
 )

;; inferior, low end
(define-type ONT::substandard-VAL
 :parent ONT::PROPERTY-VAL
 )

;; stereotypical, formulaic
(define-type ONT::stereotypicality-VAL
 :parent ONT::typicality-VAL
 )

;; resulting
(define-type ONT::outcome-VAL
 :parent ONT::PROCESS-VAL
 )

;; broadband
(define-type ONT::BANDWIDTH-VAL
 :parent ONT::substantial-PROPERTY-VAL
 )

;; done, finished
(define-type ONT::COMPLETION-VAL
 :parent ONT::PROCESS-VAL
 :sem (F::Abstr-obj (F::gradability -))
 :arguments ((:optional ONT::GROUND)
             )
 )

;; higher-level type for evaluation
(define-type ont::evaluation-attribute-val
  :parent ont::property-val
  )

; good, bad, terrible, great
(define-type ONT::ACCEPTABILITY-VAL
    :parent ONT::evaluation-attribute-val
    )

; fun, enjoyable
(define-type ONT::entertainment-VAL
    :parent ONT::evaluation-attribute-val
    )

; noticeable, noteworthy
(define-type ONT::attention-worthy-val
    :parent ONT::evaluation-attribute-val
    )

; obvious, obscure inobtrusive, obtrusive
(define-type ONT::clarity-val
    :parent ONT::evaluation-attribute-val
    )

; scary, frightening, fearsome
(define-type ONT::frightening-val
    :parent ONT::evaluation-attribute-val
    )

; (in)appropriate, suitable
(define-type ONT::APPROPRIATENESS-VAL
    :parent ONT::evaluation-attribute-val
    )

;; delicious, zesty, spicy, salty...
(define-type ONT::TASTE-VAL
 :parent ONT::tastable-property-val
 :sem (F::Abstr-obj (F::scale ONT::tastefulness*1--07--00xs))
 )

(define-type ONT::SWEET-VAL
 :parent ONT::taste-val
 :sem (F::Abstr-obj (F::scale ONT::sweetness*1--07--00))
 )

(define-type ONT::bitter-VAL
 :parent ONT::taste-val
 :sem (F::Abstr-obj (F::scale ONT::bitter*1--07--00))
 )

(define-type ONT::tart-VAL
 :parent ONT::taste-val
 :sem (F::Abstr-obj (F::scale ONT::tartness*1--07--00))
 )

(define-type ONT::sour-VAL
 :parent ONT::taste-val
 :sem (F::Abstr-obj (F::scale ONT::sourness*1--07--00))
 )

;; beautiful, ugly
(define-type ONT::BEAUTY-VAL
    :parent ONT::evaluation-attribute-val
    )

; (un)successful
(define-type ONT::SUCCESS-VAL
    :parent ONT::process-val
    )

;; (un)steady, off balance
(define-type ONT::steadiness-VAL
    :parent ONT::process-val
    )

;; darn
(define-type ONT::expletive-VAL
    :parent ONT::property-val
    )

;; superior, inferior
(define-type ONT::quality-VAL
    :parent ONT::evaluation-attribute-val
    )

; (un)welcome, (un)pleasant
(define-type ONT::pleasant-VAL
    :parent ONT::evaluation-attribute-val
    )

; (un)lucky, (un)fortunate
(define-type ONT::luckiness-VAL
    :parent ONT::property-val
    )

;; (un)important, (un)necessary, (in)significant, central, critical, principal
(define-type ONT::IMPORTANCE-VAL
 :parent ONT::evaluation-attribute-val
 )

;; basic, fundamental, inherent
(define-type ont::fundamental-val
  :parent ont::evaluation-attribute-val
  )

;; rich, poor
(define-type ONT::ECONOMIC-VAL
 :parent ONT::property-val
 )

;; particular, specific
(define-type ONT::SPECIFICITY-VAL
 :parent ONT::property-val
 )

;; preferable
(define-type ONT::PREFERENCE-VAL
 :parent ONT::evaluation-attribute-val
 )

(define-type ONT::GRANULARITY-VAL
 :parent ONT::physical-property-val
 )

;; intense, powerful, weak
(define-type ONT::intensity-VAL
 :parent ONT::physical-property-val
 )

;; monitor resolution: xga, sxga...
(define-type ONT::resolution-VAL
 :parent ONT::physical-property-val
 )

;; positive, negative
(define-type ONT::POLARITY-VAL
 :parent ONT::physical-property-val
 )

(define-type ONT::POLARITY-VAL-POSITIVE
 :wordnet-sense-keys ("positive%3:00:05" "positively%4:02:02")
 :parent ONT::POLARITY-VAL
 )

(define-type ONT::POLARITY-VAL-NEGATIVE
 :wordnet-sense-keys ("negative%3:00:05" "negatively%4:02:00")
 :parent ONT::POLARITY-VAL
 )

;; tight, strict, rigorous
(define-type ONT::BINDING-VAL
 :parent ONT::configuration-property-val
 )

;;; the severity of the problem/situation/crisis/emergency
;; severe, mild
(define-type ONT::SEVERITY-VAL
 :parent  ONT::evaluation-attribute-val
 :arguments ((:required ont::FIGURE ((? of f::situation f::abstr-obj)))) ;; adding restriction to prevent "acute stomach"
 )

;; physical, bodily
(define-type ONT::physical-VAL
 :parent ONT::property-val
 :arguments ((:optional ONT::GROUND))
 )

(define-type ont::has-medical-condition
  :parent ont::physical-val
  :arguments ((:essential ONT::FIGURE (F::phys-obj (F::origin F::human) (F::intentional +))))
  )

;; lightheaded, tired
(define-type ONT::physical-symptom-val
 :parent ONT::has-medical-condition
 )

;; mental, cerebral
(define-type ONT::mental-VAL
 :parent ONT::property-val
 :arguments ((:optional ONT::GROUND))
 )

(define-type ONT::STATUS-VAL
 :parent ONT::PROPERTY-VAL
 :arguments ((:optional ONT::GROUND))
 )

;; american, european, asian...
(define-type ONT::nationality-VAL
 :parent ONT::property-val
 )

;; adjectives meaning "can [not] be verb'd" for some verb
(define-type ONT::can-be-done-val
 :parent ONT::process-val
 :arguments ((:optional ONT::GROUND))
 )

;; adjectives meaning "has [not] been verb'd" for some verb
(define-type ONT::has-been-done-val
 :parent ONT::process-val
 )

;; adjectives meaning "[does not] involve[s] verb'ing" for some verb
(define-type ONT::involves-doing-val
 :parent ONT::process-val
 )

;; adjectives meaning "is [not] verb'ing" for some verb
(define-type ONT::is-doing-val
 :parent ONT::process-val
 )

;; adjectives meaning "[does not] allow[s] verb'ing" for some verb
(define-type ONT::allows-doing-val
 :parent ONT::process-val
 )

;; readable, writable, burnable
(define-type ONT::RW-STATUS-VAL
 :parent ONT::can-be-done-val
 )

;; for the adverbial counterparts of 'tough' words, e.g. easily, straightforwardly, simply
;; the adjective lf is task-complexity-val which defines roles of formal, cause and action. So
;; far we have no adverbs that take a formal argument so I'm not using that lf
(define-type ONT::complexity-val
 :parent ONT::property-val
 )

;; alone, individual
(define-type ONT::Singularity-VAL
 :parent ONT::number-related-property-val
 )

;; the proposal is close to done; the hotel is close to an address; the reporter got close to the riot;
;; close to, near
(define-type ONT::distance-val
 :parent ONT::spatial
 :sem (F::abstr-obj (:required (f::scale ont::linear-scale))(:default (F::gradability +)))
 :arguments ((:REQUIRED ONT::neutral ((? th f::situation f::phys-obj f::abstr-obj)))
             (:ESSENTIAL ONT::neutral1 ((? cth f::situation f::phys-obj f::abstr-obj)))
;	     (:OPTIONAL ONT::PROPERTY)
             )
 )

;; "tough" adjectives, e.g. hard, easy, possible
;; the task is easy for him to do/understand
(define-type ONT::TASK-COMPLEXITY-VAL
 :parent ONT::process-val
 :arguments (
	     (:essential ONT::Affected) ;; him, the storm
;	     (:essential ont::content ((? ct f::phys-obj f::abstr-obj f::situation))) ;; the action (these can also be stative)
	     (:essential ont::FORMAL ((? ct f::phys-obj f::abstr-obj f::situation))) ;; the action (these can also be stative)
             )
 )

#||
;; adjacent, contiguous  -- moved to ADJACENT
(define-type ont::connected-val
 :parent ont::spatial
 :sem (F::abstr-obj (:required)(:default (F::gradability +)))
 :arguments ((:REQUIRED ONT::neutral)
             (:ESSENTIAL ONT::neutral1)
;	     (:OPTIONAL ONT::PROPERTY)
             )
 )||#

;; sure, certain, confident
(define-type ONT::confidence-VAL
 :parent  ONT::psychological-property-val
 :arguments ((:optional ONT::GROUND)
             )
 )

;; perceptible by the mind or senses
;; visibility, audibility
(define-type ONT::perceptibility
 :parent ONT::physical-property-val
 :arguments ((:optional ONT::GROUND)
             )
 )

;; (un)available
(define-type ONT::AVAILABILITY-VAL
 :parent  ONT::can-be-done-val
 :arguments ((:optional ONT::GROUND ((? tp F::phys-obj F::situation)))
	     ;; available in 4 MW capacity
;	     (:optional ont::property (f::abstr-obj))
             )
 )

;; able, capable, competent
(define-type ONT::ABILITY-VAL
 :parent  ONT::property-val
 )

;; (un)manageable, (un)controllable
(define-type ONT::manageability-VAL
 :parent  ONT::can-be-done-val
 )

;; (un)reasonable, (ir)rational, (in)sane
(define-type ONT::reasonable-VAL
 :parent  ONT::psychological-property-val
 :arguments ((:optional ONT::GROUND ((? tp F::phys-obj F::situation)))
             )
 )

;(un)likely, unexpected
(define-type ONT::EXPECTATION-VAL
 :parent  ONT::property-val
 :arguments ((:optional ONT::GROUND ((? tp F::phys-obj F::situation)))
             )
 )

;; (in)accessible
(define-type ONT::ACCESSIBILITY-VAL
 :parent  ONT::can-be-done-val
 :arguments ((:REQUIRED ONT::FIGURE (F::phys-obj))
             (:ESSENTIAL ONT::GROUND (F::phys-obj))
             )
 )

(define-type ONT::ENOUGH-VAL
 :parent  ONT::quantity-related-property-val
 :arguments ((:ESSENTIAL ONT::GROUND)
             )
 )

;; Added by Myrosia for terms like numerical, linear
(define-type ONT::Numerical-property-val
    :parent ONT::discrete-property-val
    :sem (f::abstr-obj (f::gradability -))
    :arguments ((:REQUIRED ONT::FIGURE (f::abstr-obj (F::measure-function f::term)))))

;; optical, magnetic, holographic
(define-type ONT::MEDIUM
 :parent ONT::substantial-property-val
 :sem (F::Abstr-obj (F::gradability -))
 :arguments ((:OPTIONAL ONT::FIGURE)
             )
 )

;; e.g., graphical, tactile, vocal
(define-type ONT::MODE
 :parent ONT::substantial-property-val
 :sem (F::Abstr-obj (F::gradability -))
 :arguments ((:OPTIONAL ONT::FIGURE)
             )
 )

;; specific type defined for obtw demo
(define-type ont::medical
    :parent ont::property-val
    )

                                                                                                                                                                                                  
(define-type ONT::cardiac
 :parent ONT::MEDICAL  ;; of argument can be events (cardiac care, cardiac arrest) or phys-obj (cardiac muscle)                                                                                                                             
 )

(define-type ont::science-discipline-property-val
    :parent ont::discrete-property-val
    )

;; monroe
(define-type ONT::POLITICAL
 :parent ONT::DISCRETE-PROPERTY-VAL
 )

;; for boudreaux
(define-type ONT::BIOLOGICAL
 :parent ONT::science-discipline-PROPERTY-VAL
 )

#|
;; We have a lot of things pertaining to artifacts only: lamps being lit, switches on and off etc.
(define-type ont::artifact-property-val
    :parent ont::configuration-property-val
    :arguments ((:required ont::of (f::phys-obj (f::origin f::artifact))))
    )
|#

;; To put in adjectives which are difficult to find a spot somewhere
;; else. They have something to do with the information (technical
;; solution, for example) but there's no clear way to characterize
;; them, and there are no clear synonyms/antonyms
(define-type ont::abstract-information-property-val
    :parent ont::property-val
    :sem (f::abstr-obj (:default (f::gradability -)))
    :arguments ((:required ont::FIGURE (?ft (f::information f::information-content))))
    )

;; neat, tidy, (dis)orderly
(define-type ONT::orderliness-VAL
 :parent ONT::property-val
 )

;; clean, dirty
(define-type ONT::cleanliness-VAL
 :parent ONT::substantial-property-val
 )

;; (un)comfortable
(define-type ONT::comfort-VAL
 :parent ONT::process-val
 )

;; modern, ancient
(define-type ONT::modernity-VAL
 :parent ONT::temporal
 )

;; national, federal
(define-type ONT::national-VAL
 :parent ONT::property-val
 )

;; for foodkb
;; lean, nonfat, lowfat
(define-type ONT::fat-content
 :parent ONT::substantial-property-val
 )

;; boneless, skinless
(define-type ONT::food-preparation
 :parent ONT::substantial-property-val
 )

;; congenital disease
(define-type ONT::congenital
 :parent ONT::body-related-property-val
 )

;; genetic, hereditary
(define-type ONT::hereditary
 :parent ONT::body-related-property-val
 )

;; natural, unnatural
(define-type ONT::natural-val
 :parent ONT::physical-property-val
 :arguments ((:REQUIRED ONT::FIGURE ((? lof f::phys-obj f::abstr-obj f::situation)))
             )
 )

(define-type ONT::large
 :parent ONT::SIZE-VAL
 ; Words: (W::LARGE W::BIG W::HUGE W::BROAD W::DOUBLE W::VAST W::MASSIVE W::ENORMOUS W::EXTENSIVE W::GIANT W::EXTENDED W::UNLIMITED W::SPACIOUS W::WHOPPING)
 :wordnet-sense-keys ("unlimited%3:00:00" "large%3:00:00" "humongous%5:00:00:large:00" "huge%5:00:01:large:00" "elephantine%5:00:00:large:00" "enormous%5:00:00:large:00" "massive%5:00:00:large:00" "extensive%5:00:00:large:00" "double%5:00:00:large:00" "broad%5:00:00:large:00")
 ; Antonym: ONT::small (W::SMALL W::LITTLE W::LIMITED W::TINY W::TEENY)
 )

(define-type ONT::small
 :parent ONT::SIZE-VAL
 ; Words: (W::SMALL W::LITTLE W::LIMITED W::TINY W::TEENY)
 :wordnet-sense-keys ("bantam%5:00:00:small:00" "small%3:00:00" "limited%3:00:00" "bitty%5:00:00:small:00" "minor%5:00:00:limited:00")
 ; Antonym: ONT::large (W::LARGE W::BIG W::HUGE W::BROAD W::DOUBLE W::VAST W::MASSIVE W::ENORMOUS W::EXTENSIVE W::GIANT W::EXTENDED W::UNLIMITED W::SPACIOUS W::WHOPPING)
 )

;; teeny, tiny
(define-type ont::tiny
  :parent ont::small
  )

;; limited
(define-type ont::limited
  :parent ont::small
  )

;; little
(define-type ont::little
  :parent ont::small
  )

(define-type ONT::SAFE
 :parent ONT::SAFETY-VAL
 ; Words: (W::SAFE W::SECURE)
  :wordnet-sense-keys ("safe%3:00:01" "secure%3:00:02" "safe%3:00:01")
 ; Antonym: ONT::DANGEROUS (W::DANGEROUS W::UNSAFE)
 )

(define-type ONT::DANGEROUS
 :parent ONT::SAFETY-VAL
 ; Words: (W::DANGEROUS W::UNSAFE)
  :wordnet-sense-keys ("dangerous%3:00:00" "insecure%3:00:02" "dangerous%3:00:00")
 ; Antonym: ONT::SAFE (W::SAFE W::SECURE)
 )

(define-type ONT::SPEEDY
 :parent ONT::SPEED-VAL
 ; Words: (W::QUICK W::FAST W::RAPID W::SWIFT W::SPEEDY)
  :wordnet-sense-keys  ("quick%5:00:00:fast:01" "quick%5:00:02:fast:01" "fast%3:00:01" "fleet%5:00:00:fast:01" "rapid%5:00:00:fast:01" "rapid%5:00:02:fast:01" "instantaneous%5:00:00:fast:01")
 ; Antonym: NIL (W::SLOW)
 )

(define-type ONT::boring
 :parent ONT::FASCINATION-VAL
 ; Words: (W::DULL W::BORING W::UNINTERESTING)
 :wordnet-sense-keys ("boring%5:00:00:uninteresting:00" "uninteresting%3:00:00" "uninteresting%3:00:00")
 ; Antonym: NIL (W::INTERESTING)
 )

(define-type ONT::HUNGRY
 :wordnet-sense-keys ("hungry%3:00:00" "famished%5:00:00:hungry:00" "hungry%3:00:00" "hungry%3:00:00" "peckish%5:00:00:hungry:00")
 :parent ONT::BODY-PROPERTY-VAL
 ; Words: (W::HUNGRY W::FAMISHED W::PECKISH)
 :wordnet-sense-keys ("hungry%3:00:00" "famished%5:00:00:hungry:00" "hungry%3:00:00" "hungry%3:00:00" "peckish%5:00:00:hungry:00")
 ; Antonym: NIL (W::THIRSTY)
 )

(define-type ONT::AILING
 :parent ONT::PHYSICAL-SYMPTOM-VAL
 :wordnet-sense-keys ("clammy%3:00:00" "ailing%5:00:00:ill:01" "nauseated%5:00:00:ill:01" "ill%3:00:01" "lightheaded%3:00:00:ill:01" "feverish%5:00:00:ill:01" "dizzy%5:00:00:ill:01" "upset%5:00:00:ill:01" "faint%5:00:00:ill:01")
 ; Words: (W::SICK W::ILL W::UPSET W::GIDDY W::WOOZY W::LIGHTHEADED W::DIZZY W::UNWELL W::FEVERISH W::NAUSEOUS)
 ; Antonym: NIL (W::WELL)
 )

(define-type ONT::DAZED
 :wordnet-sense-keys ("lethargic%3:00:00" "dazed%5:00:00:lethargic:00")
 :parent ONT::PHYSICAL-SYMPTOM-VAL
  )

;;;;;
(define-type ONT::pos-emotional-val
  :parent ONT::emotional-val
 )

(define-type ONT::neg-emotional-val
  :parent ONT::emotional-val
 )

(define-type ONT::neutral-emotional-val
  :parent ONT::emotional-val
 )

(define-type ONT::surprised
  :parent ONT::emotional-val
 )

;; ecstatic
(define-type ONT::pos-intense-emotional-val
  :parent ONT::pos-emotional-val
 )

;; glad
(define-type ONT::pos-soft-emotional-val
  :parent ONT::pos-emotional-val
 )

;; angry
(define-type ONT::neg-intense-emotional-val
  :parent ONT::neg-emotional-val
 )

;; sad
(define-type ONT::neg-soft-emotional-val
  :parent ONT::neg-emotional-val
 )

;;;;;
(define-type ONT::EUPHORIC
 :parent ONT::pos-intense-emotional-val
 ; Words: (W::HAPPY W::EUPHORIC)
 :wordnet-sense-keys ("euphoric%3:00:00" "happy%3:00:00" "cheerful%3:00:00" "beaming%5:00:00:cheerful:00")
 ; Antonym: ONT::UNHAPPY (W::UNHAPPY W::MISERABLE)
 )

(define-type ONT::excited
 :parent ONT::pos-intense-emotional-val
 :wordnet-sense-keys ("excited%3:00:00" "excited%3:00:00:wild:02" "enthusiastic%3:00:00")
 )

(define-type ONT::desirous
 :parent ONT::pos-intense-emotional-val
 :wordnet-sense-keys ("desirous%3:00:00")
 )

;;;;;
(define-type ONT::GRATEFUL
 :parent ONT::pos-soft-emotional-val
 ; Words: (W::GLAD W::GRATEFUL W::CHEERFUL W::THANKFUL)
 :wordnet-sense-keys ("grateful%3:00:00" "glad%3:00:00" "glad%5:00:00:grateful:00")
 ; Antonym: ONT::UNGRATEFUL (W::SAD W::MELANCHOLY W::UNGRATEFUL)
 )

(define-type ONT::amused
 :parent ONT::pos-soft-emotional-val
 :wordnet-sense-keys ("amused%3:00:00:pleased:00")
 )

(define-type ONT::calm
 :parent ONT::pos-soft-emotional-val
 :wordnet-sense-keys ("calm%3:00:00:composed:00")
 )

(define-type ONT::PLEASANT
 :parent ONT::pos-soft-emotional-val
 :wordnet-sense-keys ("pleasant%3:00:00")
 )

;;;;;
(define-type ONT::afraid
 :parent ONT::neg-intense-emotional-val
 :wordnet-sense-keys ("afraid%3:00:00")
 )

(define-type ONT::angry
 :parent ONT::neg-intense-emotional-val
 :wordnet-sense-keys ("afraid%3:00:00")
 )

(define-type ONT::agitated
 :parent ONT::neg-intense-emotional-val
 :wordnet-sense-keys ("agitated%3:00:00")
 )

(define-type ONT::envious
 :parent ONT::neg-intense-emotional-val
 :wordnet-sense-keys ("envious%3:00:00:desirous:00")
 )

(define-type ONT::disgusted
 :parent ONT::neg-intense-emotional-val
 :wordnet-sense-keys ("disgusted%3:00:00:displeased:00")
 )

;;;;;
(define-type ONT::uneasy
 :parent ONT::neg-soft-emotional-val
 :wordnet-sense-keys ("anxious%3:00:00:troubled:00" "uneasy%3:00:00")
 )

(define-type ONT::unpleasant
 :parent ONT::neg-soft-emotional-val
 :wordnet-sense-keys ("grumpy%3:00:00:ill-natured:00" "disagreeable%3:00:00:ill-natured:00" "unpleasant%3:00:00")
 )

(define-type ONT::UNHAPPY
 :parent ONT::neg-soft-emotional-val
 ; Words: (W::UNHAPPY W::MISERABLE)
 :wordnet-sense-keys ("dysphoric%3:00:00" "unhappy%3:00:00" "sad%3:00:00" "gloomy%3:00:00:dejected:00" "melancholy%5:00:00:sad:00" "miserable%5:00:00:unhappy:00")
 ; Antonym: ONT::EUPHORIC (W::HAPPY W::EUPHORIC)
 )

(define-type ONT::sorry
 :parent ONT::neg-soft-emotional-val
 ; Words: (W::sorry)
 :wordnet-sense-keys ("sorry%3:00:02")
 )

(define-type ONT::bored
 :parent ONT::neg-soft-emotional-val
 :wordnet-sense-keys ("bored%3:00:00:tired:00" "bored%3:00:00:uninterested:00")
 )

(define-type ONT::smart
 :parent ONT::INTELLIGENCE-VAL
 ; Words: (W::CLEVER W::INTELLIGENT W::SMART)
 :wordnet-sense-keys ("intelligent%3:00:00" "smart%3:00:00" "bright%5:00:00:intelligent:00" "cagey%5:00:00:smart:00")
 ; Antonym: ONT::STUPID (W::STUPID W::DUMB)
 )

(define-type ONT::STUPID
 :parent ONT::INTELLIGENCE-VAL
 ; Words: (W::STUPID W::DUMB)
 :wordnet-sense-keys ("stupid%3:00:00" "unintelligent%3:00:00" "dense%5:00:00:stupid:00")
 ; Antonym: ONT::smart (W::CLEVER W::INTELLIGENT W::SMART)
 )

(define-type ONT::HEEDLESS
 :parent ONT::ATTENTION-VAL
 ; Words: (W::HEEDLESS W::INATTENTIVE)
 ; WordNet synsets: ("heedless%3:00:00" "inattentive%3:00:00")
 ; Antonym: NIL (W::ATTENTIVE)
 )

(define-type ONT::linear-dimension
 :parent ONT::LINEAR-VAL
; :arguments ((:OPTIONAL ONT::FIGURE (F::Phys-obj (F::type ont::phys-object))))
 :wordnet-sense-keys ("deep%5:00:00" "shallow%3:00:01"   "deep%3:00:01" )
 )

(define-type ONT::HEIGHT-VAL
    :sem (F::Abstr-obj (F::Scale Ont::height-scale))
    :wordnet-sense-keys ("high%3:00:02" "high%3:00:01" "tall%3:00:00")
    :parent ONT::linear-dimension
    )

#||(define-type ONT::HIGH-VAL
 :parent ONT::linear-dimension
 :wordnet-sense-keys ("high%3:00:02" "high%3:00:01" "tall%3:00:00")
)||#

(define-type ONT::LOW-VAL
 :parent ONT::height-val
 :wordnet-sense-keys ("low%3:00:02" "low%3:00:01")
)

(define-type ONT::LENGTH-VAL
 :parent ONT::linear-dimension
)

(define-type ONT::LONG
 :parent ONT::LENGTH-VAL
 :wordnet-sense-keys ("long%3:00:02" "long%3:00:01")
)

(define-type ONT::SHORT
 :parent ONT::LENGTH-VAL
 :wordnet-sense-keys ("short%3:00:02" "short%3:00:01")
)

(define-type ONT::BROAD
 :parent ONT::LINEAR-VAL
 ; Words: (W::LOW W::SHORT W::WIDE W::DEEP W::THICK W::FAT W::PLUMP)
 :wordnet-sense-keys ("wide%3:00:00" "fat%3:00:01" "thick%3:00:01" "chubby%5:00:00:fat:01" "fat%3:00:01" "compact%5:00:00")
)

(define-type ONT::SLIGHT
 :parent ONT::LINEAR-VAL
 ; Words: (W::HIGH W::LONG W::THIN W::NARROW W::TALL W::FLAT W::SHALLOW W::SLIM W::SLENDER W::SKINNY)
:wordnet-sense-keys ("slender%5:00:00" "thin%3:00:03" "scraggy%5:00:00"  "compressed%5:00:00" "narrow%3:00:00" "thin%3:00:01")
)

(define-type ONT::COLD
 :parent ONT::TEMPERATURE-VAL
 ; Words: (W::COLD W::COOL)
 :wordnet-sense-keys ("cool%3:00:01" "cool%3:00:03" "cold%5:00:00:cool:03")
 ; Antonym: ONT::WARM (W::HOT W::WARM)
 )

(define-type ONT::WARM
 :parent ONT::TEMPERATURE-VAL
 ; Words: (W::HOT W::WARM)
 :wordnet-sense-keys ("warm%3:00:01" "warm%3:00:03" "hot%5:00:00:warm:03")
 ; Antonym: ONT::COLD (W::COLD W::COOL)
 )

(define-type ONT::CLEAR-WEATHER
 :parent ONT::ATMOSPHERIC-VAL
 ; Words: (W::CLEAR W::FAIR)
 :wordnet-sense-keys ("clear%3:00:03" "clear%3:00:03" "fair%5:00:00:clear:03")
 ; Antonym: ONT::CLOUDY (W::SMOGGY W::OVERCAST W::FOGGY W::HAZY W::CLOUDY)
 )

(define-type ONT::CLOUDY
 :parent ONT::ATMOSPHERIC-VAL
 ; Words: (W::SMOGGY W::OVERCAST W::FOGGY W::HAZY W::CLOUDY)
 :wordnet-sense-keys ("cloudy%3:00:00" "cloud-covered%5:00:00:cloudy:00" "brumous%5:00:00:cloudy:00" "smoggy%5:00:00:cloudy:00")
 ; Antonym: ONT::CLEAR-WEATHER (W::CLEAR W::FAIR)
 )

(define-type ONT::LIGHTWEIGHT
 :parent ONT::WEIGHT-VAL
 ; Words: (W::LIGHT W::LIGHTWEIGHT)
 :wordnet-sense-keys ("light%3:00:01" "lightweight%5:00:00:light:01")
 ; Antonym: NIL (W::HEAVY)
 )

(define-type ONT::underweight
 :parent ONT::lightweight
 )

(define-type ONT::VERTICAL
 :parent ONT::ORIENTATION-VAL
 ; Words: (W::STRAIGHT W::VERTICAL W::PERPENDICULAR)
:wordnet-sense-keys ("erect%3:00:00" "vertical%3:00:00" "vertical%3:00:00" "perpendicular%3:00:00")
 ; Antonym: ONT::HORIZONTAL (W::PARALLEL W::HORIZONTAL)
 )

(define-type ONT::HORIZONTAL
 :parent ONT::ORIENTATION-VAL
 ; Words: (W::PARALLEL W::HORIZONTAL)
:wordnet-sense-keys ("horizontal%3:00:00" "horizontal%3:00:00" "parallel%3:00:00")
 ; Antonym: ONT::VERTICAL (W::STRAIGHT W::VERTICAL W::PERPENDICULAR)
 )

(define-type ONT::NOISY
 :parent ONT::loudness-VAL
 ; Words: (W::LOUD W::NOISY)
:wordnet-sense-keys ("noisy%3:00:00" "loud%3:00:00")
 ; Antonym: ONT::QUIET (W::SOFT W::QUIET W::SILENT W::STILL)
 )

(define-type ONT::QUIET
 :parent ONT::loudness-VAL
 ; Words: (W::SOFT W::QUIET W::SILENT W::STILL)
 :wordnet-sense-keys ("hushed%5:00:00:soft:04" "soft%3:00:04" "quiet%3:00:01" "silent%5:00:00:quiet:01")
 ; Antonym: ONT::NOISY (W::LOUD W::NOISY)
 )

(define-type ONT::LEFT
 :parent ONT::LOCATION-VAL
 ; Words: (W::LEFT W::LEFTMOST)
 :wordnet-sense-keys ("left%3:00:00" "center%3:00:00" "leftmost%5:00:00:left:00")
 ; Antonym: ONT::RIGHT (W::RIGHT W::RIGHTMOST)
 )

(define-type ONT::RIGHT
 :parent ONT::LOCATION-VAL
 ; Words: (W::RIGHT W::RIGHTMOST)
 :wordnet-sense-keys ("right%3:00:03" "right%3:00:00" "rightmost%5:00:00:right:00")
 ; Antonym: ONT::LEFT (W::LEFT W::LEFTMOST)
 )

(define-type ONT::INTERNAL
 :parent ONT::LOCATION-VAL
 ; Words: (W::INTERNAL W::INNER)
 :wordnet-sense-keys ("internal%3:00:00" "inner%5:00:00:internal:00")
 ; Antonym: ONT::EXTERNAL (W::EXTERNAL W::OUTER)
 )

(define-type ONT::EXTERNAL
 :parent ONT::LOCATION-VAL
 ; Words: (W::EXTERNAL W::OUTER)
 :wordnet-sense-keys ("external%3:00:00" "outer%5:00:00:external:00")
 ; Antonym: ONT::INTERNAL (W::INTERNAL W::INNER)
 )

(define-type ONT::NORTH
 :parent ONT::MAP-LOCATION-VAL
 ; Words: (W::NORTHERN W::NORTH)
 :wordnet-sense-keys ("north%3:00:00" "northerly%5:00:02:north:00" "northeastern%5:00:00:north:00" "northwestern%5:00:00:north:00")
 ; Antonym: ONT::SOUTH (W::SOUTHERN W::SOUTH)
 )

(define-type ONT::SOUTH
 :parent ONT::MAP-LOCATION-VAL
 ; Words: (W::SOUTHERN W::SOUTH)
 :wordnet-sense-keys ("southeasterly%5:00:02:south:00" "southerly%5:00:02:south:00" "southwesterly%5:00:02:south:00" "south%3:00:00")
 ; Antonym: ONT::NORTH (W::NORTHERN W::NORTH)
 )

(define-type ONT::EAST
 :parent ONT::MAP-LOCATION-VAL
 ; Words: (W::EASTERN W::EAST)
 :wordnet-sense-keys ("east%3:00:00" "eastern%5:00:00:east:00")
 ; Antonym: ONT::WEST (W::WESTERN W::WEST)
 )

(define-type ONT::WEST
 :parent ONT::MAP-LOCATION-VAL
 ; Words: (W::WESTERN W::WEST)
 :wordnet-sense-keys ("west%3:00:00" "western%5:00:00:west:00")
 ; Antonym: ONT::EAST (W::EASTERN W::EAST)
 )

(define-type ONT::INCOMING
 :parent ONT::DIRECTION-VAL
 ; Words: (W::INCOMING W::INBOUND)
 :wordnet-sense-keys ("incoming%3:00:00" "inbound%5:00:00:incoming:00" "incoming%3:00:00")
 ; Antonym: ONT::OUTGOING (W::OUTGOING W::OUTBOUND)
 )

(define-type ONT::OUTGOING
 :parent ONT::DIRECTION-VAL
 ; Words: (W::OUTGOING W::OUTBOUND)
 :wordnet-sense-keys ("outgoing%3:00:00" "outbound%5:00:00:outgoing:00" "outgoing%3:00:00")
 ; Antonym: ONT::INCOMING (W::INCOMING W::INBOUND)
 )

(define-type ONT::RELIABLE
 :parent ONT::CERTAINTY-VAL
 ; Words: (W::RELIABLE W::TRUSTWORTHY W::DEPENDABLE)
 :wordnet-sense-keys ("reliable%3:00:00" "reliable%3:00:00" "trustworthy%3:00:00" "dependable%5:00:00:trustworthy:00")
 ; Antonym: ONT::UNRELIABLE (W::UNCERTAIN W::UNRELIABLE)
 )

(define-type ONT::UNRELIABLE
 :parent ONT::CERTAINTY-VAL
 ; Words: (W::UNCERTAIN W::UNRELIABLE)
 :wordnet-sense-keys ("unreliable%3:00:00" "unreliable%3:00:00" "unreliable%3:00:00" "uncertain%5:00:00:unreliable:00")
 ; Antonym: ONT::RELIABLE (W::RELIABLE W::TRUSTWORTHY W::DEPENDABLE)
 )

(define-type ONT::OCCASIONAL
 :parent ONT::FREQUENCY-VAL
 ; Words: (W::RARE W::OCCASIONAL W::INFREQUENT)
:wordnet-sense-keys ("infrequent%3:00:00" "rare%5:00:00:infrequent:00" "occasional%5:00:00:infrequent:00")
 )

(define-type ONT::FREQUENT
 :parent ONT::FREQUENCY-VAL
 ; Words: (W::FREQUENT)
:wordnet-sense-keys ("frequent%3:00:00")
 )

(define-type ONT::REGULAR
 :parent ONT::FREQUENCY-VAL
 ; Words: (W::REGULAR)
:wordnet-sense-keys ("regular%5:00:00:steady:00" )
 )

(define-type ONT::IRREGULAR
 :parent ONT::FREQUENCY-VAL
 ; Words: (W::IRREGULAR)
:wordnet-sense-keys ("irregular%5:00:00:sporadic:00" "casual%5:00:00:irregular:00" "sporadic%3:00:00")
 )

(define-type ONT::correct
 :parent ONT::CORRECTNESS-VAL
 ; Words: (W::PROPER W::CORRECT W::ACCURATE)
 :wordnet-sense-keys ("accurate%5:00:00:correct:00" "correct%3:00:00" "correct%5:00:00:proper:00" "proper%3:00:00")
 ; Antonym: ONT::incorrect (W::MISTAKEN W::INACCURATE W::INCORRECT)
 )

(define-type ONT::incorrect
 :parent ONT::CORRECTNESS-VAL
 ; Words: (W::MISTAKEN W::INACCURATE W::INCORRECT)
 :wordnet-sense-keys ("incorrect%3:00:00" "inaccurate%3:00:00" "faulty%5:00:00:inaccurate:00" "false%5:00:00:incorrect:00")
 ; Antonym: ONT::correct (W::PROPER W::CORRECT W::ACCURATE)
 )

(define-type ONT::LOGICAL
 :parent ONT::CORRECTNESS-VAL
 ; Words: (W::VALID W::LEGITIMATE)
 :wordnet-sense-keys ("valid%3:00:00" "legitimate%5:00:00:valid:00")
 ; Antonym: NIL (W::INVALID)
 )

(define-type ONT::PRECISE
 :parent ONT::PRECISION-VAL
 ; Words: (W::DEAD W::PRECISE)
 :wordnet-sense-keys ("precise%3:00:00" "precise%3:00:00" "dead%5:00:00:precise:00")
 ; Antonym: NIL (W::IMPRECISE)
 )

(define-type ONT::COMMON
 :parent ONT::TYPICALITY-VAL
 ; Words: (W::COMMON W::NORMAL W::USUAL W::REGULAR W::ORDINARY W::STANDARD W::FAMILIAR W::TYPICAL W::CONVENTIONAL W::ORTHODOX W::UNREMARKABLE W::UNEXCEPTIONAL W::ROUTINE)
 :wordnet-sense-keys ("common%3:00:01" "typical%3:00:00" "regular%3:00:00" "conventional%3:00:00" "ordinary%3:00:00" "normal%3:00:01" "run-of-the-mill%5:00:00:ordinary:00" "familiar%3:00:02" "orthodox%3:00:00" "usual%3:00:00" "regular%5:00:00" "everyday%5:00:00" "common%5:00:00" "conventional%5:00:00" "standard%5:00:00" "common%5:00:00" "standard%5:00:00" "regular%5:00:00" "typical%5:00:00")
 ; Antonym: ONT::STRANGE (W::STRANGE W::ODD W::UNUSUAL W::REMARKABLE W::EXTRAORDINARY W::EXCEPTIONAL W::PECULIAR W::WEIRD W::BIZARRE W::UNFAMILIAR W::ABNORMAL W::FUNKY W::UNCONVENTIONAL W::UNCOMMON W::SINGULAR W::FREAKY W::ATYPICAL W::OUTLANDISH W::UNORTHODOX)
 )

(define-type ONT::STRANGE
 :parent ONT::TYPICALITY-VAL
 ; Words: (W::STRANGE W::ODD W::UNUSUAL W::REMARKABLE W::EXTRAORDINARY W::EXCEPTIONAL W::PECULIAR W::WEIRD W::BIZARRE W::UNFAMILIAR W::ABNORMAL W::FUNKY W::UNCONVENTIONAL W::UNCOMMON W::SINGULAR W::FREAKY W::ATYPICAL W::OUTLANDISH W::UNORTHODOX)
 :wordnet-sense-keys ("strange%3:00:00" "abnormal%3:00:00" "bizarre%5:00:00:unconventional:01" "bizarre%5:00:00:unconventional:01" "bizarre%5:00:00:unconventional:01" "especial%5:00:00" "atypical%5:00:00:abnormal:00" "unconventional%3:00:01" "unconventional%3:00:01" "unconventional%3:00:01" "unconventional%3:00:01" "uncommon%3:00:00" "unusual%3:00:00" "unconventional%3:00:00" "strange%5:00:01:unfamiliar:00" "atypical%3:00:00" "extraordinary%3:00:00" "extraordinary%3:00:00" "unfamiliar%3:00:00" "uncommon%3:00:00" "unusual%5:00:00:uncommon:00" "irregular%5:00:00" "curious%5:00:00" "remarkable%5:00:00" "remarkable%5:00:00" "funky%5:00:00" "odd%5:00:00" "weird%5:00:00" "freaky%5:00:00")
 ; Antonym: ONT::COMMON (W::COMMON W::NORMAL W::USUAL W::REGULAR W::ORDINARY W::STANDARD W::FAMILIAR W::TYPICAL W::CONVENTIONAL W::ORTHODOX W::UNREMARKABLE W::UNEXCEPTIONAL W::ROUTINE)
 )

(define-type ONT::FINISHED
 :parent ONT::COMPLETION-VAL
 ; Words: (W::COMPLETE W::FINISHED W::UTTER W::COMPLETED W::DONE)
 :wordnet-sense-keys ("finished%3:00:01" "finished%3:00:01" "complete%3:00:00" "dead%5:00:00" "complete%3:00:00" "done%5:00:00:finished:01" "complete%5:00:00:finished:01" "accomplished%5:00:00:complete:00")
 ; Antonym: ONT::INCOMPLETE (W::UNFINISHED W::INCOMPLETE)
 )

(define-type ONT::INCOMPLETE
 :parent ONT::COMPLETION-VAL
 ; Words: (W::UNFINISHED W::INCOMPLETE)
 :wordnet-sense-keys ("incomplete%3:00:00" "unfinished%3:00:01" "incomplete%5:00:00:unfinished:01")
 ; Antonym: ONT::FINISHED (W::COMPLETE W::FINISHED W::UTTER W::COMPLETED W::DONE)
 )

(define-type ONT::good
 :parent ONT::ACCEPTABILITY-VAL
 ; Words: (W::GOOD W::GREAT W::FINE W::NICE W::ACCEPTABLE W::ALRIGHT W::SATISFACTORY W::SUPERB W::OKAY W::OK W::PEACHY W::FAVORABLE W::BEARABLE W::TOLERABLE W::SUPPORTABLE ALL_RIGHT)
 :wordnet-sense-keys ("all_right%5:00:00:satisfactory:00" "favorable%3:00:02" "acceptable%3:00:00" "tolerable%3:00:00" "nice%3:00:00" "satisfactory%3:00:00" "bearable%5:00:00:tolerable:00" "satisfactory%5:00:00:good:01" "good%3:00:01" "tolerable%3:00:00" "tolerable%3:00:00" "bang-up%5:00:00:good:01" "bearable%5:00:00:tolerable:00" "alright%5:00:00:satisfactory:00" "superb%5:00:00:good:01" "good%5:00:00:nice:00" "adequate%5:00:00:satisfactory:00")
 ; Antonym: ONT::bad (W::BAD W::TERRIBLE W::AWFUL W::NASTY W::DREADFUL W::UNACCEPTABLE W::ROTTEN W::UNSUPPORTABLE W::UNBEARABLE W::INTOLERABLE W::INSUFFERABLE W::UNFAVORABLE W::MEDIOCRE W::LOUSY)
 )

(define-type ONT::bad
 :parent ONT::ACCEPTABILITY-VAL
 ; Words: (W::BAD W::TERRIBLE W::AWFUL W::NASTY W::DREADFUL W::UNACCEPTABLE W::ROTTEN W::UNSUPPORTABLE W::UNBEARABLE W::INTOLERABLE W::INSUFFERABLE W::UNFAVORABLE W::MEDIOCRE W::LOUSY)
 :wordnet-sense-keys ("intolerable%3:00:00" "unfavorable%3:00:02" "bad%3:00:00" "nasty%3:00:00" "unfavorable%5:00:00:bad:00" "unsupportable%5:00:00:intolerable:00" "unacceptable%3:00:00" "impossible%5:00:00:intolerable:00" "dirty%5:00:00:nasty:00" "mediocre%5:00:00:bad:00" "icky%5:00:00:bad:00" "atrocious%5:00:00:bad:00")
 ; Antonym: ONT::good (W::GOOD W::GREAT W::FINE W::NICE W::ACCEPTABLE W::ALRIGHT W::SATISFACTORY W::SUPERB W::OKAY W::OK W::PEACHY W::FAVORABLE W::BEARABLE W::TOLERABLE W::SUPPORTABLE ALL_RIGHT)
 )

(define-type ONT::noticeable
 :parent ONT::ATTENTION-WORTHY-VAL
 ; Words: (W::PROMINENT W::STRIKING W::NOTICEABLE W::PRONOUNCED W::OBTRUSIVE W::CONSPICUOUS)
:wordnet-sense-keys ("outstanding%5:00:00" "obtrusive%3:00:00" "obtrusive%3:00:00" "obtrusive%3:00:00" "conspicuous%3:00:00" "noticeable%3:00:00" "conspicuous%3:00:00" "big%5:00:00" "marked%5:00:00")
 ; Antonym: ONT::UNOBTRUSIVE (W::UNOBTRUSIVE W::INCONSPICUOUS W::UNNOTICEABLE)
 )

(define-type ONT::unnoticeable
 :parent ONT::ATTENTION-WORTHY-VAL
 ; Words: (W::UNOBTRUSIVE W::INCONSPICUOUS W::UNNOTICEABLE)
 :wordnet-sense-keys ("unobtrusive%3:00:00" "unobtrusive%3:00:00" "unobtrusive%3:00:00" "inconspicuous%3:00:00" "inconspicuous%3:00:00" "obscure%5:00:00:inconspicuous:00")
 ; Antonym: ONT::OUTSTANDING (W::PROMINENT W::STRIKING W::NOTICEABLE W::PRONOUNCED W::OBTRUSIVE W::CONSPICUOUS)
 )

(define-type ONT::UNCLEAR
 :parent ONT::CLARITY-VAL
 ; Words: (w::unobvious W::UNCLEAR W::OBSCURE W::OPAQUE)
 :wordnet-sense-keys ("ill-defined%3:00:00" "unclear%3:00:00" "opaque%3:00:00" "obscure%5:00:00:unclear:00")
 ; Antonym: ONT::clear (W::CLEAR w::obvious w::evident)
 )

(define-type ONT::clear
 :parent ONT::CLARITY-VAL
 ; Words: (w::clear W::OBVIOUS W::EVIDENT)
 :wordnet-sense-keys ("obvious%3:00:00" "apparent%5:00:00:obvious:00")
 ; Antonym: ONT::unclear (W::UNOBVIOUS)
 )

(define-type ONT::BEAUTIFUL
 :parent ONT::BEAUTY-VAL
 ; Words: (W::BEAUTIFUL W::LOVELY W::PRETTY)
 :wordnet-sense-keys ("beautiful%3:00:00" "beautiful%3:00:00" "lovely%5:00:00:beautiful:00" "pretty%5:00:00:beautiful:00")
 ; Antonym: NIL (W::UGLY)
 )

(define-type ONT::steady
  :wordnet-sense-keys ("steady%3:00:00" "steady%5:00:00:stable:00" "stable%3:00:00" "unchanged%3:00:00" "unchanged%3:00:04")
 :parent ONT::steadiness-val
 )

(define-type ONT::UNSTEADY
 :parent ONT::STEADINESS-VAL
 ; Words: (W::UNSTEADY W::SHAKY)
:wordnet-sense-keys ("unsteady%3:00:00" "shaky%5:00:00" "unstable%3:00:00" "volatile%3:00:00")
 ; Antonym: NIL (W::STEADY)
 )

(define-type ONT::lucky
 :parent ONT::LUCKINESS-VAL
 ; Words: (W::LUCKY W::FORTUNATE)
 :wordnet-sense-keys ("fortunate%3:00:00" "lucky%5:00:00:fortunate:00")
 ; Antonym: ONT::unlucky (W::UNFORTUNATE W::UNLUCKY)
 )

(define-type ONT::unlucky
 :parent ONT::LUCKINESS-VAL
 ; Words: (W::UNFORTUNATE W::UNLUCKY)
 :wordnet-sense-keys ("unfortunate%3:00:00" "doomed%5:00:00:unfortunate:00")
 ; Antonym: ONT::lucky (W::LUCKY W::FORTUNATE)
 )

(define-type ONT::primary
 :parent ONT::IMPORTANCE-VAL
 ; Words: (W::IMPORTANT W::MAIN W::MAJOR W::NECESSARY W::CENTRAL W::SERIOUS W::SIGNIFICANT W::ESSENTIAL W::PRIMARY W::SENIOR W::CRITICAL W::VITAL W::CRUCIAL W::INDISPENSABLE)
 :wordnet-sense-keys ("dangerous%5:00:00:critical:03" "important%3:00:00" "significant%3:00:00" "significant%3:00:00" "important%3:00:00" "senior%3:00:00" "cardinal%5:00:00:important:00" "chief%5:00:02" "all-important%5:00:00" "major%5:00:00" "basal%5:00:00")
 ; Antonym: ONT::SECONDARY (W::SECONDARY W::MINOR W::JUNIOR W::UNNECESSARY W::UNIMPORTANT W::INSIGNIFICANT)
 )

(define-type ONT::necessary
 :parent ONT::primary
 ; Words: (W::IMPORTANT W::MAIN W::MAJOR W::NECESSARY W::CENTRAL W::SERIOUS W::SIGNIFICANT W::ESSENTIAL W::PRIMARY W::SENIOR W::CRITICAL W::VITAL W::CRUCIAL W::INDISPENSABLE)
:wordnet-sense-keys ("necessary%3:00:00" "essential%3:00:00" "critical%3:00:03" "crucial%3:00:00" "critical%5:00:00" "essential%5:00:00" "vital%5:00:00" "indispensable%3:00:00" "major%5:00:00")
 ; Antonym: ONT::SECONDARY (W::SECONDARY W::MINOR W::JUNIOR W::UNNECESSARY W::UNIMPORTANT W::INSIGNIFICANT)
 )

(define-type ONT::SECONDARY
 :parent ONT::IMPORTANCE-VAL
 ; Words: (W::SECONDARY W::MINOR W::JUNIOR W::UNNECESSARY W::UNIMPORTANT W::INSIGNIFICANT)
 :wordnet-sense-keys ("junior-grade%5:00:00:junior:00" "unnecessary%3:00:00" "insignificant%3:00:00" "insignificant%3:00:00" "unimportant%3:00:00" "minor%3:00:06" "junior%3:00:00" "minor%5:00:00" "insignificant%5:00:00:minor:06")
 ; Antonym: ONT::SERIOUS (W::IMPORTANT W::MAIN W::MAJOR W::NECESSARY W::CENTRAL W::SERIOUS W::SIGNIFICANT W::ESSENTIAL W::PRIMARY W::SENIOR W::CRITICAL W::VITAL W::CRUCIAL W::INDISPENSABLE)
 )

(define-type ONT::intense
 :parent ONT::INTENSITY-VAL
 ; Words: (W::HIGH W::LOW W::STRONG W::DEEP W::POWERFUL W::SHARP W::INTENSE W::DULL W::SHALLOW W::POTENT)
 :wordnet-sense-keys ("high%3:00:03" "intense%3:00:00" "powerful%3:00:00" "strong%3:00:00" "deep%3:00:01" "high%3:00:02" "intense%3:00:00" "sharp%3:00:04" "intensive%5:00:00:intense:00" "potent%5:00:00:powerful:00" "strong%5:00:00:powerful:00" "strong%5:00:00:intense:00" "shrill%5:00:00:high:03" "deep%5:00:00:intense:00" "acute%5:00:00:sharp:04")
 ; Antonym: ONT::weak (W::WEAK W::FAINT)
 )

(define-type ONT::weak
 :parent ONT::INTENSITY-VAL
 ; Words: (W::WEAK W::FAINT)
 :wordnet-sense-keys ("weak%3:00:00" "weak%3:00:00" "low%3:00:02" "low%3:00:01" "shallow%3:00:01" "dull%3:00:04" "faint%5:00:00:weak:00")
 ; Antonym: ONT::intense (W::HIGH W::LOW W::STRONG W::DEEP W::POWERFUL W::SHARP W::INTENSE W::DULL W::SHALLOW W::POTENT)
 )

(define-type ONT::usefulness-val
 :parent ONT::can-be-done-val
 )

(define-type ONT::USEFUL
 :parent ONT::usefulness-VAL
 ; Words: (W::USEFUL W::PRACTICAL W::FUNCTIONAL)
 :wordnet-sense-keys ("useful%3:00:00" "functional%3:00:02" "functional%3:00:00" "practical%3:00:00" "practical%5:00:00:applied:00" "utilitarian%5:00:00" "functional%5:00:00")
 ; Antonym: ONT::useless (W::USELESS W::IMPRACTICAL)
 )

(define-type ONT::useless
 :parent ONT::usefulness-VAL
 ; Words: (W::USELESS W::IMPRACTICAL)
:wordnet-sense-keys ("useless%3:00:00" "impractical%3:00:00")
 ; Antonym: ONT::USEFUL (W::USEFUL W::PRACTICAL W::FUNCTIONAL)
 )

(define-type ONT::RELATIVE
 :parent ONT::involves-doing-VAL
 ; Words: (W::RELATIVE W::COMPARATIVE)
:wordnet-sense-keys ("relative%3:00:00" "relative%3:00:00")
 ; Antonym: NIL (W::ABSOLUTE)
 )

(define-type ONT::UNADORNED
 :parent ONT::substantial-property-VAL
 ; Words: (W::BARE W::NAKED W::UNADORNED)
 :wordnet-sense-keys ("unadorned%3:00:00" "bare%3:00:00" "plain%5:00:00:unadorned:00" "bare%5:00:00:unadorned:00" "naked%5:00:00:bare:00")
 ; Antonym: nil
 )

(define-type ONT::UNFILLED
 :parent ONT::configuration-property-VAL
 ; Words: (W::EMPTY)
:wordnet-sense-keys ("empty%3:00:00")
 ; Antonym: ONT::FULL (W::FULL W::FILLED)
 )

(define-type ONT::FULL
 :parent ONT::configuration-property-VAL
 ; Words: (W::FULL W::FILLED)
 :wordnet-sense-keys ("full%3:00:00" "filled%5:00:01:full:00")
 ; Antonym: ONT::UNFILLED (W::EMPTY)
 )

(define-type ONT::activity-val
    :comment "predicates relating to whether something is acting as intended for some process"
    :parent ONT::property-val
    :sem (F::Abstr-obj (F::gradability -))
    :arguments ((:required ONT::FIGURE ((? lof f::phys-obj)
					   (f::type (? !t2 ont::location))
					   (F::object-function F::provides-service)))
	      ))

(define-type ONT::active
 :parent ONT::activity-VAL
 :comment "operating as intended wrt some process"
 ; Words: (W::ACTIVE W::BUSY)
 :wordnet-sense-keys ("busy%3:00:00" "active%3:00:03" "active%3:00:06" "busy%5:00:01:active:06")
 )

(define-type ONT::ACTIVE-ON
    :parent ONT::ACTIVE
    :comment "operating as intended, typically due to some switching on/off"
    :wordnet-sense-keys ("on%3:00:02")
    :arguments ((:required ONT::FIGURE (f::phys-obj
					(f::type (? !t2 ont::location))
					(F::object-function F::provides-service-on-off))))
    )

(define-type ONT::ACTIVE-up
    :parent ONT::ACTIVE
    :comment "operating as intended, typically for ongoing available services using up/down"
    :wordnet-sense-keys ("functioning%3:00:00")
    :arguments ((:required ONT::FIGURE (f::phys-obj
					(f::type (? !t2 ont::location))
					(F::object-function F::provides-service-up-down))))
    )

(define-type ONT::ACTIVE-open
    :parent ONT::ACTIVE
    :comment "operating as intended, typically a physcal location with operating hours"
     :wordnet-sense-keys ("OPEN%3:00:02")
    :arguments ((:required ONT::FIGURE (f::phys-obj
					(f::type (? !t2 ont::location))
					(F::object-function F::provides-service-open-closed))))
    )

(define-type ONT::INACTIVE
 :parent ONT::activity-VAL
 :comment "not operating as intended wrt some process"
 :wordnet-sense-keys ("passive%3:00:01" "idle%3:00:00")
 ; Antonym: ONT::active (W::ACTIVE W::BUSY)
 )

(define-type ONT::INACTIVE-OFF
    :parent ONT::INACTIVE
    :comment "not operating as intended, typically due to some switching on/off"
    :wordnet-sense-keys ("off%3:00:01")
    :arguments ((:required ONT::FIGURE (f::phys-obj
					(f::type (? !t2 ont::location))
					(F::object-function F::provides-service-on-off))))
    )

(define-type ONT::INACTIVE-down
    :parent ONT::INACTIVE
    :comment "not operating is intended, typically for ongoing available services using up/down"
    :wordnet-sense-keys ("inoperative%3:00:00")
    :arguments ((:required ONT::FIGURE (f::phys-obj
					(f::type (? !t2 ont::location))
					(F::object-function F::provides-service-up-down))))
    )

(define-type ONT::INACTIVE-closed
    :parent ONT::INACTIVE
    :comment "not operating as intended,  typically a physcal location with operating hours"
    :wordnet-sense-keys ("closed%3:00:01")
    :arguments ((:required ONT::FIGURE (f::phys-obj
					(f::type (? !t2 ont::location))
					(F::object-function F::provides-service-open-closed))))
    )

(define-type ONT::NOT-IN-WORKING-ORDER-VAL
 :parent ONT::inactive
 :comment "broken/not-operational more permanently - needs fixing, not switching on"
 )

(define-type ONT::IN-WORKING-ORDER-VAL
 :parent ONT::activity-val
 :comment "operational but not necessarily on"
 )

(define-type ONT::dependence-val
 :parent ONT::property-val
 :arguments ((:optional ONT::GROUND ((? lof f::phys-obj f::abstr-obj))))  ;;  independent/dependent of X
 )

(define-type ONT::DEPENDENT
 :parent ONT::dependence-val
 ; Words: (W::DEPENDENT W::CONDITIONAL)
 :wordnet-sense-keys ("dependent%3:00:00" "conditional%3:00:00" "dependent%5:00:00:conditional:00")
 ; Antonym: NILx (W::INDEPENDENT)
 )

(define-type ONT::independent
 :parent ONT::dependence-val
 )

(define-type ONT::private
 :parent ONT::STATUS-VAL
 ; Words: (W::PERSONAL W::PRIVATE W::SECRET)
 :wordnet-sense-keys ("personal%3:00:00" "private%3:00:00" "private%5:00:02:personal:00" "privy%5:00:00:private:00")
 ; Antonym: NIL (W::PUBLIC)
 )

(define-type ONT::hidden
 :parent ONT::visible-property-VAL
 ; Words: (W::HIDDEN W::INVISIBLE W::OBSCURE)
 :wordnet-sense-keys ("invisible%3:00:00" "concealed%5:00:00:invisible:00" "inconspicuous%3:00:00" "obscure%5:00:00:inconspicuous:00")
 ; Antonym: NIL (W::VISIBLE)
 )

(define-type ONT::RELEVANT
 :parent ONT::STATUS-VAL
 ; Words: (W::RELEVANT W::APPLICABLE W::PERTINENT)
 :wordnet-sense-keys ("relevant%3:00:00" "relevant%3:00:00" "applicable%5:00:00:relevant:00" "pertinent%5:00:00:relevant:00")
 ; Antonym: NIL (W::IRRELEVANT)
 )

(define-type ONT::near
 :parent ONT::DISTANCE-VAL
 ; Words: (W::CLOSE W::NEAR W::NEARBY)
 :wordnet-sense-keys ("near%3:00:00" "close%3:00:02" "close%3:00:02" "nearby%5:00:00:near:00" "approximate%5:00:00:close:02")
 ; Antonym: ONT::REMOTE (W::FAR W::REMOTE W::DISTANT W::FARTHER)
 )

(define-type ONT::REMOTE
 :parent ONT::DISTANCE-VAL
 ; Words: (W::FAR W::REMOTE W::DISTANT W::FARTHER)
 :wordnet-sense-keys ("distant%5:00:02:far:00" "distant%3:00:02" "far%3:00:00" "distant%5:00:01:far:00" "farther%5:00:01:far:00")
 ; Antonym: ONT::near (W::CLOSE W::NEAR W::NEARBY)
 )

(define-type ONT::difficult
 :parent ONT::TASK-COMPLEXITY-VAL
 ; Words: (W::DIFFICULT W::HARD W::COMPLEX W::TOUGH W::COMPLICATED W::TRICKY W::CHALLENGING W::ARDUOUS)
 :wordnet-sense-keys ("difficult%3:00:00" "complex%3:00:00" "ambitious%5:00:00:difficult:00" "ambitious%5:00:00:difficult:00" "complex%3:00:00" "complicated%5:00:00:complex:00" "rugged%5:00:00:difficult:00" "arduous%5:00:00:difficult:00" "catchy%5:00:00:difficult:00")
 ; Antonym: ONT::easy (W::EASY W::SIMPLE)
 )

(define-type ONT::POSSIBLE
 :parent ONT::TASK-COMPLEXITY-VAL
 ; Words: (W::POSSIBLE W::DOABLE)
 :wordnet-sense-keys ("possible%3:00:00" "accomplishable%5:00:00:possible:00" "possible%3:00:00")
 ; Antonym: NIL (W::IMPOSSIBLE)
 )

(define-type ONT::easy
 :parent ONT::TASK-COMPLEXITY-VAL
 ; Words: (W::EASY W::SIMPLE)
 :wordnet-sense-keys ("simple%3:00:02" "easy%3:00:01" "elementary%5:00:00:easy:01")
 ; Antonym: ONT::difficult (W::DIFFICULT W::HARD W::COMPLEX W::TOUGH W::COMPLICATED W::TRICKY W::CHALLENGING W::ARDUOUS)
 )

(define-type ONT::UNCERTAIN
 :parent ONT::CONFIDENCE-VAL
 ; Words: (W::UNCERTAIN W::UNSURE)
:wordnet-sense-keys ("uncertain%3:00:02" "unsealed%3:00:02")
 ; Antonym: ONT::CERTAIN (W::CERTAIN W::SURE W::CONFIDENT)
 )

(define-type ONT::CERTAIN
 :parent ONT::CONFIDENCE-VAL
 ; Words: (W::CERTAIN W::SURE W::CONFIDENT)
 :wordnet-sense-keys ("certain%3:00:01" "convinced%5:00:00" "sealed%3:00:02" "certain%3:00:02" "indisputable%5:00:00:certain:01")
 ; Antonym: ONT::UNCERTAIN (W::UNCERTAIN W::UNSURE)
 )

(define-type ONT::AVAILABLE
 :parent ONT::AVAILABILITY-VAL
 ; Words: (W::AVAILABLE W::FREE)
 :wordnet-sense-keys ("available%3:00:00" "free%3:00:00" "available%5:00:00:free:00")
 ; Antonym: NIL (W::UNAVAILABLE)
 )

(define-type ONT::unable
 :parent ONT::ABILITY-VAL
 ; Words: (W::UNABLE W::INCAPABLE W::INCOMPETENT)
:wordnet-sense-keys ("incompetent%3:00:00" "incapable%3:00:00" "unable%5:00:00")
 ; Antonym: ONT::able (W::ABLE W::CAPABLE W::COMPETENT)
 )

(define-type ONT::able
 :parent ONT::ABILITY-VAL
 ; Words: (W::ABLE W::CAPABLE W::COMPETENT)
 :wordnet-sense-keys ("competent%3:00:00" "capable%3:00:00" "capable%3:00:00" "competent%3:00:00" "able%5:00:00:competent:00" "able%5:00:00:capable:00")
 ; Antonym: ONT::unable (W::UNABLE W::INCAPABLE W::INCOMPETENT)
 )

;; willing
(define-type ONT::willing
 :parent ONT::social-interaction-val
 ; Words: (W::willing w::inclined w::voluntary)
:wordnet-sense-keys ("willing%3:00:00" "incapable%3:00:00")
 ; Antonym: ONT::disinclined (W::unwilling w::disinclined w::involuntary)
  :arguments ((:required ONT::FIGURE ((? lof f::phys-obj f::abstr-obj) (f::intentional +)))
;	      (:optional ont::action (f::situation))
             )
 )

;; unwilling
(define-type ONT::unwilling
 :parent ONT::social-interaction-val
 ; Words: (W::unwilling w::disinclined w::involuntary)
:wordnet-sense-keys ("unwilling%3:00:00" "unwilling%3:00:00:involuntary:01")
 ; Antonym: ONT::inclined (W::willing w::inclined w::voluntary)
  :arguments ((:required ONT::FIGURE ((? lof f::phys-obj f::abstr-obj) (f::intentional +)))
;	      (:optional ont::action (f::situation))
             )
 )

(define-type ONT::manageable
 :parent ONT::MANAGEABILITY-VAL
 ; Words: (W::CONTROLLABLE W::MANAGEABLE)
 :wordnet-sense-keys ("controllable%5:00:00:manageable:00" "manageable%3:00:00")
 ; Antonym: ONT::unmanageable (W::UNCONTROLLABLE W::UNMANAGEABLE)
 )

(define-type ONT::unmanageable
 :parent ONT::MANAGEABILITY-VAL
 ; Words: (W::UNCONTROLLABLE W::UNMANAGEABLE)
 :wordnet-sense-keys ("indocile%5:00:00:unmanageable:00" "unmanageable%3:00:00")
 ; Antonym: ONT::manageable (W::CONTROLLABLE W::MANAGEABLE)
 )

(define-type ONT::insane
 :parent ONT::REASONABLE-VAL
 ; Words: (W::MAD W::CRAZY W::INSANE)
 :wordnet-sense-keys ("brainsick%5:00:00:insane:00" "insane%3:00:00")
 ; Antonym: NIL (W::SANE)
 )

(define-type ONT::INACCESSIBLE
 :parent ONT::ACCESSIBILITY-VAL
 ; Words: (W::UNACCESSIBLE W::INACCESSIBLE)
:wordnet-sense-keys ("inaccessible%3:00:00" "inaccessible%3:00:00")
 ; Antonym: NIL (W::ACCESSIBLE)
 )

(define-type ONT::inadequate
 :parent ONT::ENOUGH-VAL
 :arguments ((:required ONT::GROUND (f::phys-obj (f::type ont::material)))
	     (:required ONT::FIGURE ((? xx  F::phys-obj abstr-obj))))
 ; Words: (W::SHORT W::INADEQUATE W::INSUFFICIENT)
 :wordnet-sense-keys ("inadequate%5:00:00:insufficient:00" "insufficient%3:00:00")
 ; Antonym: ONT::ADEQUATE (W::SUFFICIENT W::ADEQUATE W::ENOUGH)
 )

(define-type ONT::ADEQUATE
 :parent ONT::ENOUGH-VAL
 ; Words: (W::SUFFICIENT W::ADEQUATE W::ENOUGH)
 :wordnet-sense-keys ("sufficient%3:00:00" "adequate%5:00:00:sufficient:00")
 ; Antonym: ONT::inadequate (W::SHORT W::INADEQUATE W::INSUFFICIENT)
 )

(define-type ONT::analog
 :parent ONT::MODE
 ; Words: (W::LINEAR W::ANALOG)
:wordnet-sense-keys ("analogue%3:00:00" "analogue%3:00:00")
 ; Antonym: NIL (W::DIGITAL)
 )

(define-type ONT::COMFORTABLE
 :parent ONT::COMFORT-VAL
 ; Words: (W::COMFORTABLE W::COMFY W::COZY)
 :wordnet-sense-keys ("comfortable%3:00:00" "comfortable%3:00:00" "cozy%5:00:00:comfortable:00")
 ; Antonym: ONT::uncomfortable (W::UNCOMFORTABLE W::UNEASY)
 )

(define-type ONT::uncomfortable
 :parent ONT::COMFORT-VAL
 ; Words: (W::UNCOMFORTABLE W::UNEASY)
 :wordnet-sense-keys ("uncomfortable%3:00:00" "uncomfortable%3:00:01" "awkward%5:00:00:uncomfortable:01")
 ; Antonym: ONT::COMFORTABLE (W::COMFORTABLE W::COMFY W::COZY)
 )

(define-type ONT::natural
 :parent ONT::NATURAL-VAL
 ; Words: (W::NATURAL W::WILDTYPE)
:wordnet-sense-keys ("natural%3:00:02")
 ; Antonym: NIL (W::ARTIFICIAL)
 )

(define-type ONT::artificial
 :parent ONT::NATURAL-VAL
 ; Words: (W::ARTIFICIAL W::FAKE W::UNNATURAL)
 :wordnet-sense-keys ("affected%3:00:01" "artificial%3:00:00" "unnatural%3:00:00" "artificial%5:00:00:affected:01" "fake%5:00:00:artificial:00")
 ; Antonym: NIL (W::NATURAL)
 )

(define-type ONT::mutant
 :parent ONT::NATURAL-VAL
 :wordnet-sense-keys ("mutant%3:01:00")
 )


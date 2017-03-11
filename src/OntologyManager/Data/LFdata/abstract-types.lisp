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
(define-type ONT::representation
 :parent ONT::kind
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
  :arguments ((:REQUIRED ONT::FIGURE)
             )
 )

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

;; More 'absolute' adjective classes e.g. colors, shapes, nationality
;(define-type ont::intersective-property
;  :parent ont::property-val
;  )
;
;;; relative adjectives, e.g. long, big, good
;(define-type ont::subsective-property
;  :parent ont::property-val
;  )
;
;;; fake, former, fictitious
;(define-type ont::privative-property
;  :parent ont::property-val
;  )
;
;;; alleged, potential, arguable
;(define-type ont::plain-nonsubsective
;  :parent ont::property-val
;  )



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

;;  too hot to go outside

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

;; Experiencer is used to formal
(define-type ONT::habituated
 :parent ONT::property-val
 :arguments ((:ESSENTIAL ONT::affected (F::phys-obj))
             (:ESSENTIAL ONT::Formal)
             )
 )

;;;;;;;;;;;;;;;
; the group-object-abstr hierarchy is a duplicate of group-object in phys-object 
; (Note: ONT::SHEET-abstr and ONT::QUANTITY-abstr are under GROUP-OBJECT-ABSTR too.)
; (Note 2: wordnet mappings are identical in the two hierarchies.)
;;;;;;;;;;;;;;;

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

;; digestive, immune,
(define-type ONT::body-system-val
 :parent ONT::body-related-property-val ;; of arguments can be more than just human; cardiac care; intestinal disturbance
 )

;; body location adjectives: intestinal, abdominal...
(define-type ONT::body-part-val
 :parent ONT::body-system-val ;; of arguments can be more than just human; cardiac care; intestinal disturbance
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

;;; > the traditional five senses:

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

 ; <

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
    :parent  ONT::PART-WHOLE-VAL)

(define-type ONt::partial-incomplete
    :wordnet-sense-keys ("incomplete%3:00:00")
    :parent  ONT::PART-WHOLE-VAL)

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

;;; these two books are the same


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

;; how does this relate to ont::truth-val??
;;; e.g., wrong, problematic, right,
;; the wrong day, the right time, the right number
(define-type ONT::EVALUATION-VAL
 :parent ONT::RELATION
 :arguments ((:ESSENTIAL ONT::neutral ((? tp f::time f::abstr-obj F::phys-obj F::situation)))
	     (:OPTIONAL  ONT::neutral1 ((? tp1 f::time f::abstr-obj F::phys-obj F::situation)))
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

;; for numbers
;(define-type ONT::number
; :parent ONT::ABSTRACT-OBJECT
; :sem (F::Abstr-obj (F::information F::data))
; )

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

;;  be enumerated: 5 sets of people
;;; set, group, series, string (of events), suite (of tests)
;(define-type ont::set-unit
;    :parent ont::quantity
;    )


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

;(define-type ONT::group-unit
; :parent ONT::tangible-unit
; :sem (F::abstr-obj (f::group +))
; )
;
;;; team, crew, company, detatchment
;(define-type ONT::HUMAN-GROUP-unit
; :parent ONT::GROUP-UNIT
; :sem (F::abstr-obj (F::SCALE -))
; :arguments ((:ESSENTIAL ONT::OF (F::phys-obj (F::origin F::human) (f::scale -) (F::intentional +)))
;             )
; )

(define-type ONT::DISCRETE-DOMAIN
 :parent ONT::DOMAIN
 )

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

;;;;; This is is currently just for the word "formula"
;;;;; In leAM we needed a special type for the typed-up formulas
;;;;; which cannot be identified by a lexical form
;;;;; and so Myrosia entered this as a special object
;;;(define-type ont::formula
;;;    :parent ONT::ABSTRACT-OBJECT
;;;    :sem (F::Abstr-obj (F::information F::data))
;;; )

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
  :wordnet-sense-keys ("subject%1:09:00::" "subject_area%1:09:00::" "subject_field%1:09:00::" "field%1:09:00::" "field_of_study%1:09:00::" "study%1:09:02::")
  :parent ONT::function-OBJECT
  :sem (F::Abstr-obj)
  :arguments ((:essential ONT::FIGURE)
	      )
 )

;; technology, nanotechnology, biotechnology
(define-type ONT::technology
 :parent ONT::discipline
 :sem (F::Abstr-obj)
 :arguments ((:essential ONT::FIGURE)
	     )
 )




(define-type ONT::information-function-object
 :parent ONT::FUNCTION-OBJECT
 :sem (F::Abstr-obj (F::information F::information-content) (F::intentional -) (F::container +))
 :arguments (
;	     (:optional ONT::Associated-information)
	     )
 )

(define-type ONT::composition
  :comment "composition, e.g., result of event-of-creation"
 :parent ONT::information-function-object
 )

;; information
(define-type ONT::information
 :wordnet-sense-keys ("information%1:10:00" "info%1:10:00")
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

;; specific type defined for obtw demo
(define-type ont::medical
    :parent ont::property-val
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

;(define-type ONT::computer-interface
;  :parent ONT::computer-program
;  )

;; MS word, photoshop, etc.
;(define-type ONT::document-editor
;  :parent ONT::computer-software
;  )

;; outlook, etc.
;(define-type ONT::email-reader
;  :parent ONT::computer-software
;  )

(define-type ONT::web-browser
  :parent ONT::computer-software
  )

;(define-type ONT::computer-game
;  :parent ONT::computer-software
;  )

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
    :wordnet-sense-keys ("language%1:10:00")
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

;(define-type ONT::particle
; :parent ONT::LINGUISTIC-OBJECT
; )

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

;; I want a computer/a service contract for under 1000 dollars (sense of 'for')
;; here we want attachment to the object
(define-type ONT::VALUE-COST
 :wordnet-sense-keys ("change%1:21:02" "return%1:21:00" "issue%1:21:00" "take%1:21:00" "takings%1:21:00" "proceeds%1:21:00" "yield%1:21:00" "payoff%1:21:02")
 :wordnet-sense-keys ("change%1:21:02" "return%1:21:00" "issue%1:21:00" "take%1:21:00" "takings%1:21:00" "proceeds%1:21:00" "yield%1:21:00" "payoff%1:21:02")
 :parent ONT::COST-RELATION
  :arguments ((:REQUIRED ont::FIGURE ((? lo f::phys-obj f::abstr-obj)))
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

;;; ----------------------------------------------------------

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

;; not currently used
;;;; swift 03/20/02 defined to handle 'severity' in monroe dialogues 'the severity of the problem'
;(define-type ONT::ASSESSMENT
; :parent ONT::ABSTRACT-OBJECT
; )

;; not currently used
;(define-type ONT::PERMISSION
;    :parent ONT::ABSTRACT-OBJECT
;    :arguments ((:ESSENTIAL ONT::OF (F::phys-obj (f::intentional +)))
;                (:ESSENTIAL ONT::VAL ((? lof F::Situation F::phys-obj F::abstr-obj))) ;; authorization of/for ...
;                (:OPTIONAL ONT::Action (F::Situation)) ;; permission to do something
;             )
; )


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

#|
;; We have a lot of things pertaining to artifacts only: lamps being lit, switches on and off etc.
(define-type ont::artifact-property-val
    :parent ont::configuration-property-val
    :arguments ((:required ont::of (f::phys-obj (f::origin f::artifact))))
    )
|#

(define-type ont::definition
    :parent ont::information-function-object
    :arguments ((:essential ont::FIGURE))
    )

;; To put in adjectives which are difficult to find a spot somewhere
;; else. They have something to do with the information (technical
;; solution, for example) but there's no clear way to characterize
;; them, and there are no clear synonyms/antonyms
(define-type ont::abstract-information-property-val
    :parent ont::property-val
    :sem (f::abstr-obj (:default (f::gradability -)))
    :arguments ((:required ont::FIGURE (?ft (f::information f::information-content))))
    )


;; changed parent to ont::discipline (from abstract-object)
;; 2005.04/20 Added by Myrosia to handle words like algebra, mathematics etc.
(define-type ONT::science-discipline
 :parent ONT::discipline
 :sem (F::Abstr-obj (F::container +)) ;; why is this container +?
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

;; unused -- now experience-change
;;; e.g. body weight & temperature
;(define-type ONT::body-change
; :parent ONT::bodily-process
; )

;(define-type ONT::dialog
; :parent ONT::LINGUISTIC-OBJECT
; )

(define-type ONT::eating-plan
 :parent ONT::RECIPE
 )

;; cardiac care
(define-type ONT::cardiac
 :parent ONT::MEDICAL  ;; of argument can be events (cardiac care, cardiac arrest) or phys-obj (cardiac muscle)
 )

;; congenital disease
(define-type ONT::congenital
 :parent ONT::body-related-property-val
 )

;; genetic, hereditary
(define-type ONT::hereditary
 :parent ONT::body-related-property-val
 )

;; hearing, sight, vision
(define-type ONT::physical-sense
 :parent ONT::non-measure-ordered-domain
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj (F::container -) (F::form F::solid-object) (F::origin F::human)))
             )
 )

;; natural, unnatural
(define-type ONT::natural-val
 :parent ONT::physical-property-val
 :arguments ((:REQUIRED ONT::FIGURE ((? lof f::phys-obj f::abstr-obj f::situation)))
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

;;; > Adjective subtypes automatically added using WordNet clustering information

#|
(define-type ONT::QUICK
 :parent ONT::EVENT-DURATION-MODIFIER
 ; Words: (W::QUICK W::FAST W::INSTANT W::INSTANTANEOUS)
 :wordnet-sense-keys ("fast%3:00:01" "instantaneous%5:00:00:fast:01" "quick%5:00:00:fast:01")
 ; Antonym: NIL (W::SLOW)
 )
|#

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

 ; <

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

;;(define-type ont::color-scale
;;  :parent ont::scale
;;  )

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

(define-type ont::phosphorilated
    :parent ont::physical-property-val
    :arguments (;(:ESSENTIAL ONT::of (F::phys-obj (f::type (? t ont::molecular-part ont::chemical))))
		(:ESSENTIAL ONT::figure (F::phys-obj (f::type (? t2 ont::molecular-part ont::chemical))))
		))

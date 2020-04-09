(in-package :om)



(define-type ONT::object-defined-by-relationship
    :parent ONT::PHYS-OBJECT
    :wordnet-sense-keys ("possession%1:03:00")
    :arguments ((:required ONT::FIGURE (F::Phys-obj)))
    )

(define-type ONT::defined-by-sequence-relationship
    :parent ONT::object-defined-by-relationship
    :arguments ((:required ONT::FIGURE (F::Phys-obj)))
    )

(define-type ONT::predecessor
    :parent ONT::defined-by-sequence-relationship
    :wordnet-sense-keys ("predecessor%1:18:00")
)

(define-type ONT::successor
    :parent ONT::defined-by-sequence-relationship
    :wordnet-sense-keys ("successor%1:18:01" "successor%1:09:00" "successor%1:18:00")
)

(define-type ONT::natural-object
    :wordnet-sense-keys ("natural_object%1:03:00" "actinoid%1:27:00::" "rare_earth%1:27:00" "galaxy%1:14:00" "galaxy%1:14:01" "oort_cloud%1:14:00")
    :parent ONT::PHYS-OBJECT
    :sem (F::Phys-obj (F::origin F::natural)(F::spatial-abstraction (? sa F::spatial-point F::spatial-region)))
    )

(define-type ONT::Physics-object
    :comment "objects developed in physics of natural world"
    :wordnet-sense-keys ("electron_shell%1:14:00" "particle%1:17:00" "halogen%1:27:00") 
    :parent ONT::natural-object
    )

(define-type ONT::WILDTYPE-OBJ
    :parent ONT::natural-object
    )

(define-type ONT::MUTANT-OBJ
    :wordnet-sense-keys ("mutant%1:18:00" "mutant%1:05:00")
    :parent ONT::natural-object
    )

;; DRUM
(define-type ONT::CELL-LINE
    :parent ONT::natural-object
    :sem (F::Phys-obj (F::container +))
    )

;; UMLS
(define-type ONT::organism
    :wordnet-sense-keys ("organism%1:03:00" "being%1:03:00" "life%1:19:00" "life%1:26:00" "life%1:03:00")
    :parent ONT::natural-object
    :sem (F::Phys-obj (F::origin F::living))
    )

(define-type ONT::MOLECULAR-PART
    :wordnet-sense-keys ("chemical_group%1:27:00" )
    :parent ONT::natural-object
    )

(define-type ONT::molecule
    :wordnet-sense-keys ("molecule%1:27:00")
    :parent ONT::MOLECULAR-PART
    )

(define-type ONT::terminus
    :parent ONT::MOLECULAR-PART
    )

;; DRUM
(define-type ONT::BIOLOGICAL-ROLE
;    :parent ONT::MOLECULE
    :parent ONT::MOLECULAR-PART
    )

(define-type ONT::gene-protein
    :sem (F::Phys-obj (F::container +))
    :parent ONT::MOLECULAR-PART
    )

(define-type ONT::gene
    :wordnet-sense-keys ("gene%1:08:00")
;    :parent ONT::MOLECULAR-PART
    :parent ONT::gene-protein
    )

(define-type ONT::protein
    :wordnet-sense-keys ("protein%1:27:00")
;    :parent ONT::MOLECULE
    :parent ONT::gene-protein
    )

;; DRUM
(define-type ONT::protein-family
;    :parent ONT::MOLECULE
    :parent ONT::gene-protein
    )

;; DRUM
(define-type ONT::macromolecular-complex
    :wordnet-sense-keys ("complex%1:27:00")
;    :parent ONT::MOLECULE
    :sem (F::Phys-obj (F::container +))
    :parent ONT::MOLECULAR-PART
    :arguments (;(:required ONT::CONTENTS (F::Phys-obj (f::type ont::molecular-part))))
		(:required ONT::FIGURE (F::Phys-obj (f::type ont::molecular-part))))
    )

(define-type ONT::RNA
    :wordnet-sense-keys ("rna%1:27:00")
    :parent ONT::MOLECULAR-PART
    )

(define-type ONT::mRNA
    :wordnet-sense-keys ("mrna%1:27:00")
    :parent ONT::RNA
    )

(define-type ONT::PROMOTER
    :parent ONT::MOLECULAR-PART
    )

(define-type ONT::MOLECULAR-DOMAIN
    :parent ONT::MOLECULAR-PART
    )

(define-type ONT::MOLECULAR-SITE
    :parent ONT::MOLECULAR-PART
    )

(define-type ONT::microorganism
    :wordnet-sense-keys ("microorganism%1:05:00")
    :parent ONT::organism
    )

;; tree, plant
(define-type ONT::plant
    :wordnet-sense-keys ("plant%1:03:00" "cultivar%1:20:00")
    :parent ONT::organism
    :sem (F::Phys-obj (F::origin F::plant))
    )

(define-type ONT::GRAINS
    :wordnet-sense-keys ("cereal%1:20:00" "teff%1:20:00")
    :parent ONT::plant
    )


(define-type ONT::TREE
    :wordnet-sense-keys ("tree%1:20:00")
    :parent ONT::plant
    )

(define-type ONT::SHRUB
    :wordnet-sense-keys ("shrub%1:20:00")
    :parent ONT::plant
    )

(define-type ont::herbaceous-plant
    :wordnet-sense-keys ("herb%1:20:00")
    :parent ONT::plant
    )

(define-type ont::grass
    :wordnet-sense-keys ("grass%1:20:00")
    :parent ONT::herbaceous-plant
    )

(define-type ont::vine
    :wordnet-sense-keys ("vine%1:20:00")
    :parent ONT::plant
    )

;; stem, leaf
(define-type ONT::plant-part
    :wordnet-sense-keys ("plant_part%1:20:00")
    :parent ONT::plant
    :arguments ((:required ONT::FIGURE (F::Phys-obj (F::origin F::plant))))
    )

;; UMLS
(define-type ONT::alga
    :wordnet-sense-keys ("algae%1:05:00")
    :parent ONT::plant
    )

;; UMLS
(define-type ONT::fungus
    :wordnet-sense-keys ("fungus%1:20:00")
    :parent ONT::organism
    )

;; UMLS
(define-type ONT::virus
    :wordnet-sense-keys ("virus%1:05:00")
    :parent ONT::microorganism
    )

;; UMLS
(define-type ONT::bacterium
    :wordnet-sense-keys ("bacteria%1:05:00" "bacterium%1:05:00")
    :parent ONT::microorganism
    )


;;====+SPACE++++

;; Can be natural or artifact, like buildings, cities
;; used for navigation in the transportation domains
(define-type ONT::GEO-OBJECT
    :wordnet-sense-keys ("location%1:03:00" "space%1:03:00")
    :parent ONT::phys-OBJECT
    :sem (F::Phys-obj (F::form F::geographical-object) (F::container +))
		      ;;(:default (F::object-function F::spatial-object)))
    )

;; a place in space
(define-type ONT::Location
    :parent ONT::GEO-OBJECT
    :sem (F::Phys-obj (F::origin F::non-living)
		      (F::Form F::Geographical-Object)
		      (F::MOBILITY F::fixed)
;		   (F::Object-Function F::Place)
		      ;;(F::Object-Function F::Spatial-object)
		      )
    :arguments (;(:OPTIONAL ONT::OF ((? lof F::Phys-obj)))
; this needs to be less restrictive as long as it's used for "where" clauses, e.g. the party where he met her
		(:OPTIONAL ONT::FIGURE ((? lof f::situation F::Phys-obj)))
; (:OPTIONAL ONT::OF ((? lof F::Phys-obj)))
		)
    )

(define-type ONT::specific-loc
    :parent ONT::location
    :arguments ((:OPTIONAL ONT::GROUND) ;; ppword-adv templates have an implicit subcat
		)
    )

;;; A region in space with no LF_OFs
(define-type ONT::Place
    :wordnet-sense-keys ("property%1:15:00" "place%1:15:04" "topographic_point%1:15:00" "place%1:15:00" "spot%1:15:01")
    :parent ONT::location
    :sem (F::Phys-obj (F::origin F::non-living) (F::Form F::Geographical-Object))
    )

(define-type ONT::GEOGRAPHIC-REGION
    :parent ONT::specific-loc
    :wordnet-sense-keys ("north_america%1:14:00" "south_america%1:14:00" "central_america%1:14:00" "asia%1:14:00" "europe%1:14:01")
    :sem (F::Phys-obj (F::form F::geographical-object)
;		      (F::spatial-abstraction (? sa F::spatial-point F::spatial-region))  ; It would seem we should have a restriction on spatial-abstraction, but its child ONT::ROUTE wants F::line F::strip and in general we want F::spatial-point F::spatial-region.  That covers all possibilities.
		      )
    )

;;;;Typo? was relative-locatopn
(define-type ONT::relative-location
    :parent ONT::specific-loc)


(define-type ONT::geo-formation
    :wordnet-sense-keys ("formation%1:17:00" "world%1:17:00")
    :parent ONT::GEO-OBJECT
    :sem (F::Phys-obj (F::origin F::natural) (F::trajectory -))
    )

(define-type ONT::watershed
    :wordnet-sense-keys ("watershed%1:15:01" "watershed%1:15:00" )
    :parent ONT::GEO-OBJECT
    :sem (F::Phys-obj (F::origin F::natural) (F::trajectory -))
    )

(define-type ONT::land
    :wordnet-sense-keys ("land%1:17:00" "grassland%1:15:00" "wetland%1:17:00")
    :parent ONT::GEO-formation
    :sem (F::Phys-obj (F::origin F::natural) (F::trajectory -))
    )

(define-type ONT::mountain
    :wordnet-sense-keys ("elevation%1:17:00")
    :parent ONT::GEO-formation
    :sem (F::Phys-obj (F::origin F::natural) (F::trajectory -))
    )

(define-type ONT::valley
    :wordnet-sense-keys ("valley%1:17:00")
    :parent ONT::GEO-formation
    :sem (F::Phys-obj (F::origin F::natural) (F::trajectory -))
    )

(define-type ONT::body-of-water
    :wordnet-sense-keys ("body_of_water%1:17:00")
    :parent ONT::GEO-formation
    :sem (F::Phys-obj (F::origin F::natural) (F::trajectory -))
    )

(define-type ONT::SHORE
    :parent ONT::geo-formation
    :wordnet-sense-keys ("shore%1:17:00")
    :sem (F::PHYS-OBJ (F::SPATIAL-ABSTRACTION (? SA F::SPATIAL-POINT F::LINE)) (F::MOBILITY F::FIXED))
    :arguments ((:OPTIONAL ONT::FIGURE (F::Phys-obj (F::form F::Geographical-object) (F::spatial-abstraction (? sao F::STrip F::Spatial-region))))
		)
    )



(define-type ONT::sunken-natural-formation
    :parent ONT::geo-object
    :wordnet-sense-keys ("depression%1:17:00")
    :sem (F::Phys-obj (F::origin F::natural) (F::trajectory -))
    )

;; hole
(define-type ONT::hole
    :parent ONT::sunken-natural-formation
    :wordnet-sense-keys ("hole%1:17:02")
    )

;; crater
(define-type ONT::crater
    :parent ONT::sunken-natural-formation
    :wordnet-sense-keys ("volcanic_crater%1:17:00" "crater%1:17:01" "crater%1:17:00")
    )



;; earth, venus, jupiter
(define-type ONT::planet
    :parent ONT::natural-object
    :wordnet-sense-keys ("planet%1:17:00" "major_planet%1:17:00" "planet%1:17:01")
    )

(define-type ONT::natural-phenomenon
    :parent ONT::natural-object
    :wordnet-sense-keys ("natural_phenomenon%1:19:00")
    )

;; force, pressure, compression
(define-type ont::physical-phenomenon
    :wordnet-sense-keys ("force%1:19:00" "causal_agency%1:03:00" "cause%1:03:00" "causal_agent%1:03:00"  "cause%1:11:00" "momentum%1:07:00")
    :parent ont::natural-phenomenon
    :sem (F::Phys-obj (F::origin F::non-living))
    )

(define-type ont::atmospheric-phenomenon
     :wordnet-sense-keys ("atmospheric_phenomenon%1:19:00")
    :parent ont::natural-phenomenon
    :sem (F::Phys-obj (F::origin F::non-living) (f::object-function f::weather))
    )

#|
(define-type ont::climate
    :wordnet-sense-keys ("climate%1:26:00")
    :parent ont::atmospheric-phenomenon
    :sem (F::Phys-obj (F::origin F::non-living))
    )
|#

(define-type ont::weather
    :wordnet-sense-keys ("atmospheric_condition%1:19:00" "conditions%1:19:00" "freeze%1:19:00" "weather%1:19:00" "weather_condition%1:19:00")
    :parent ont::atmospheric-phenomenon
    :sem (F::Phys-obj (F::origin F::non-living))
    )

(define-type ont::climate
    :wordnet-sense-keys ("climate%1:26:00")
    :parent ont::atmospheric-phenomenon
    :sem (F::Phys-obj (F::origin F::non-living))
    )

;; wind
(define-type ont::air-current
    :wordnet-sense-keys ("wind%1:19:00" "air_current%1:19:01" "current_of_air%1:19:00")
    :parent ont::atmospheric-phenomenon
    :sem (F::Phys-obj (F::origin F::non-living))
    )

;; rain, snow, sleet, hail
(define-type ONT::PRECIPITATION
    :wordnet-sense-keys ("precipitation%1:19:00" "downfall%1:19:00")
    :parent ONT::atmospheric-phenomenon
    :sem (F::Phys-obj (F::origin F::non-living) (f::form f::substance))
    )

(define-type ONT::STORM
    :wordnet-sense-keys ("storm%1:19:00" "cyclone%1:26:00")
    :parent ONT::atmospheric-phenomenon
    :sem (F::Phys-obj (F::origin F::non-living) (f::form f::substance))
    )

;; fog, smog, haze
(define-type ONT::cloud-like-object
    :wordnet-sense-keys ("cloud%1:19:01" "smoke%1:22:00")
    :parent ONT::atmospheric-phenomenon
    :sem (F::Phys-obj (F::origin F::non-living) (f::form f::substance))
    )

;; pollution
(define-type ONT::pollution
    :wordnet-sense-keys ("pollution%1:26:00" "pollution%1:26:02")
    :parent ONT::atmospheric-phenomenon
    :sem (F::Phys-obj (F::origin F::non-living) (F::trajectory -))
    )


(define-type ONT::POLITICAL-REGION
    :parent ONT::geographic-region
     :wordnet-sense-keys ("circuit%1:14:00" "free_world%1:14:00" "third_world%1:14:00" "eu%1:14:00" "world_power%1:14:00")
    :sem (F::Phys-obj (F::spatial-abstraction (? sa F::spatial-point F::spatial-region))
		      (F::origin F::Artifact)
		      (F::mobility f::fixed)) ;; (f::intentional +)) ; political regions can be intentional agents   -- have a rule in grammar that handles this
    )

(define-type ONT::religious-REGION
    :parent ONT::geographic-region
    )

(define-type ONT::functional-REGION
    :parent ONT::geographic-region
    )

(define-type ONT::region-for-activity
    :parent ONT::functional-region
    )

(define-type ONT::man-made-structure
    :comment "man made structures that are attached to the earther and thus act like locations"
    :parent ONT::functional-region
    )

(define-type ONT::general-structure
    :wordnet-sense-keys ("structure%1:06:00" "petting_zoo%1:14:00" "herbarium%1:14:00")
    :parent ONT::man-made-structure
    )

(define-type ONT::building
     :wordnet-sense-keys ("building%1:06:00")
    :parent ONT::man-made-structure
    )

(define-type ONT::bridge
    :wordnet-sense-keys ("bridge%1:06:00")
    :parent ONT::general-structure
    )

(define-type ONT::FACILITY
    :parent ONT::man-made-structure
    :wordnet-sense-keys ("facility%1:04:01" "facility%1:06:00" "museum%1:06:00")
    :sem (F::Phys-obj (F::spatial-abstraction (? sa F::spatial-point F::spatial-region))
		      (F::origin F::Artifact)(F::trajectory -)
		      (f::object-function f::provides-service-open-closed)
		      (F::mobility f::fixed) (f::container +))
   ) 

(define-type ONT::TOWER
  :wordnet-sense-keys ("tower%1:06:00")
  :parent ONT::man-made-structure
)

;how to deal with this?
;State and country are too similar. Perhaps country should be a child of state?
(define-type ONT::STATE
    :wordnet-sense-keys ("body_politic%1:14:00" "res_publica%1:14:00" "commonwealth%1:14:00" "state%1:15:01" "province%1:15:00")
    :parent ONT::political-region
    )

(define-type ONT::US-STATE
    :parent ONT::STATE
    :wordnet-sense-keys ("american_state%1:15:00")
    )

(define-type ONT::COUNTRY
    :wordnet-sense-keys ("country%1:15:00" "country%1:14:00")
    :parent ONT::POLITICAL-REGION
    )

(define-type ONT::COUNTY
    :parent ONT::POLITICAL-REGION
    :wordnet-sense-keys ("county%1:15:00" "county%1:15:01")
    )

(define-type ONT::CITY
    :parent ONT::POLITICAL-REGION
    :wordnet-sense-keys ("city%1:15:00" "city%1:15:01" "city%1:14:00")
    )

(define-type ONT::material
    :parent ONT::phys-object
    :wordnet-sense-keys ("material%1:27:00" "stuff%1:27:00")
    :sem (F::Phys-obj (F::mobility F::non-self-moving) (f::form f::substance))
    )

(define-type ONT::SUBSTANCE
    :wordnet-sense-keys ("substance%1:03:00" "substance%1:03:01" "substance%1:27:00" "matter%1:03:00" "paper%1:27:00")
    :parent ONT::material
    )

;; ASMA
(define-type ONT::dust
    :wordnet-sense-keys ("particulate%1:27:00")
    :parent ONT::substance
    )

;; UMLS
(define-type ONT::chemical
    :wordnet-sense-keys ("chemical%1:27:00" "chemical_substance%1:27:00"  "chemical_compound%1:27:00")
    :parent ONT::substance
    )

;; UMLS
(define-type ONT::AMINO-ACID
    :wordnet-sense-keys ("amino_acid%1:27:00")
    :Parent ont::chemical
    )

(define-type ONT::residue
;    :parent ONT::MOLECULE
;    :parent ONT::MOLECULAR-PART
    :parent ONT::CHEMICAL
  )

;; UMLS
(define-type ont::pharmacologic-substance
    :Parent ont::chemical
    )

#|
(define-type ont::nutrient
    :wordnet-sense-keys ("nutrient%1:03:01")
    :Parent ont::chemical
    )
|#

(define-type ont::fertilizer
    :wordnet-sense-keys ("fertilizer%1:27:00")
    :Parent ont::chemical
    )

(define-type ont::pesticide
    :wordnet-sense-keys ("pesticide%1:27:00")
    :Parent ont::chemical
    )

(define-type ont::herbicide
    :wordnet-sense-keys ("herbicide%1:27:00")
    :Parent ont::chemical
    )

(define-type ONT::MEDICATION
    :wordnet-sense-keys ("medicine%1:06:00" "medication%1:06:00" "medicament%1:06:00" "medicine%1:06:00" "medicinal_drug%1:06:00" "antibacterial%1:06:00" "antibacterial_drug%1:06:00" "drug%1:06:00" "agonist%1:06:00")
    :parent ONT::pharmacologic-substance
    :sem (F::Phys-obj (F::Object-Function F::comestible)) ;; too restrictive -- some medications are e.g. injections
    )

;; this can be either a control drug or a quick-relief drug
(define-type ont::bronchodilator
    :parent ont::medication
    :wordnet-sense-keys ("bronchodilator%1:06:00")
    )

;; typically taken regularly to control asthma
(define-type ont::long-term-control-drug
    :wordnet-sense-keys ("corticosteroid%1:27:00" "theophylline%1:06:00" "elixophyllin%1:06:00" "slo-bid%1:06:00" "theobid%1:06:00")
    :parent ont::bronchodilator
    )

(define-type ont::quick-relief-drug
    :wordnet-sense-keys ( "albuterol%1:06:00" "proventil%1:06:00"  "ventolin%1:06:00")
    :parent ont::bronchodilator
    )

(define-type ont::allergy-medication
    :wordnet-sense-keys ("antihistamine%1:06:00")
    :parent ont::medication
    )

(define-type ONT::antibiotic
    :wordnet-sense-keys ("Antibiotic%1:06:00" "antibiotic_drug%1:06:00")
    :parent ONT::medication
    )

(define-type ONT::pain-reliever
    :parent ont::medication
    :wordnet-sense-keys ("painkiller%1:06:00")
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; types of medication identified by Jill for CARDIAC.
;; These should be updated to link with UMLS categories, or removed
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define-type ONT::antihypertensive
; :parent ont::medication
; )
;
;(define-type ONT::diuretic
; :parent ont::antihypertensive
; )
;
;;; Angiotensin-converting enzyme (ACE)
;(define-type ONT::ACE-inhibitor
; :parent ont::antihypertensive
; )
;
;(define-type ONT::hypolipidemics
; :parent ont::medication
; )
;
;(define-type ONT::statin
; :parent ont::hypolipidemics
; )
;
;(define-type ONT::potassium-chloride
; :parent ont::medication
; )
;
;(define-type ONT::diabetes-med
; :parent ont::medication
; )
;
;
;(define-type ONT::anticoagulant
; :parent ont::medication
; )
;
;(define-type ONT::stomach-med
; :parent ont::medication
; )
;
;(define-type ONT::breathing-med
; :parent ont::medication
; )

;material vs substance?
; air, ozone
(define-type ONT::natural-SUBSTANCE
    :parent ONT::substance
    :sem (F::Phys-obj (f::origin f::natural))
    )

(define-type ONT::earth-substance
    :wordnet-sense-keys ("soil%1:27:01" "soil%1:17:00" "earth%1:27:00")
    :parent ONT::natural-substance
    )

(define-type ONT::gas-SUBSTANCE
    :parent ONT::SUBSTANCE
    :wordnet-sense-keys ("gas%1:27:00")
    :sem (F::Phys-obj (F::form F::gas))
    )

(define-type ONT::LIQUID-SUBSTANCE
    :wordnet-sense-keys ("liquid%1:27:00" "Fluid%1:27:02")
    :parent ONT::SUBSTANCE
    :sem (F::Phys-obj (F::form F::Liquid))
    )


(define-type ONT::natural-LIQUID-SUBSTANCE
    :parent ONT::liquid-SUBSTANCE
    :sem (F::Phys-obj (F::form F::Liquid) (f::origin f::natural))
    )

(define-type ONT::fuel
     :wordnet-sense-keys ("fuel%1:27:00" "petroleum%1:27:00") ; petroleum is a hyponym of fuel but it has two parents
    ;:parent ONT::liquid-SUBSTANCE
    :parent ONT::natural-liquid-SUBSTANCE
    :sem (F::Phys-obj (F::form F::Liquid) (f::origin f::natural))
    )

;; oxygen, hydrogen
(define-type ONT::natural-gas-SUBSTANCE
    :parent ONT::gas-SUBSTANCE
    :sem (F::Phys-obj (F::form F::gas) (f::origin f::natural))
    )

;;air
(define-type ONT::air
    :wordnet-sense-keys ("air%1:27:00" "air%1:19:00" "air%1:27:01")
    :parent ONT::natural-gas-SUBSTANCE
    :sem (F::Phys-obj (F::form F::gas) (f::origin f::natural))
    )

;; saliva, urine
(define-type ONT::bodily-fluid
    :wordnet-sense-keys ("liquid_body_substance%1:08:00" "bodily_fluid%1:08:00" "body_fluid%1:08:00" "humor%1:08:00" "humour%1:08:00" "secretion%1:08:00" "sweat%1:08:00" "mucus%1:08:00" "phlegm%1:08:00" )
    :parent ONT::natural-LIQUID-SUBSTANCE
    :sem (F::Phys-obj (F::form F::Liquid)) ;;(f::origin f::living))
    )

;; water
(define-type ONT::WATER
    :wordnet-sense-keys ("water%1:27:00" "H2O%1:27:00")
    :parent ONT::natural-LIQUID-SUBSTANCE
    :sem (F::Phys-obj (F::Object-Function F::comestible) (F::origin F::non-living))
    )

(define-type ONT::SOLID-SUBSTANCE
    :wordnet-sense-keys ("solid%1:27:04")
    :parent ONT::SUBSTANCE
    :sem (F::Phys-obj (F::Form F::Solid))
    )

(define-type ONT::MANUFACTURED-OBJECT
    :wordnet-sense-keys ("artifact%1:03:00" "artefact%1:03:00")
    :parent ONT::PHYS-OBJECT
    :sem (F::Phys-obj (:required (F::origin F::artifact))(:default (F::Form F::solid-object)))
    )

(define-type ONT::technology
 :parent ONT::manufactured-object
 :wordnet-sense-keys ("technology%1:04:00" "technology%1:06:00")
 :arguments ((:essential ONT::FIGURE)
	     )
 )

(define-type ONT::BLOCK
    :parent ONT::manufactured-object
    :wordnet-sense-keys ("block%1:06:00")
    )

;; automaton
(define-type ONT::automaton
    :parent ONT::manufactured-object
    :wordnet-sense-keys ("automaton%1:06:00")
    )

;; robot
(define-type ONT::robot
    :parent ONT::automaton
;;    :wordnet-sense-keys ("robot%1:06:00") #in same synset as automaton%1:06:00
    :sem (F::Phys-obj (f::intentional +) (f::object-function f::occupation)) ; certain features to pattern with people
    )

;; trash, waste
(define-type ONT::waste
    :wordnet-sense-keys ("waste%1:27:00" "waste_material%1:27:00" "waste_matter%1:27:00" "waste_product%1:27:00")
    :parent ONT::material
    )

;; these are marked intentional + because intentional + is a required setting for e.g. motion verbs like walk and run so either that requirement must be removed or animals must be intentional +
(define-type ONT::ANIMAL
    :wordnet-sense-keys ("animal%1:03:00" "animate_being%1:03:00" "beast%1:03:00" "brute%1:03:00" "creature%1:03:00" "fauna%1:14:00"
					  "wildlife%1:14:00")
    :parent ONT::organism ;; umls
    :sem (F::Phys-obj (F::intentional +) (f::form f::solid-object) (F::origin (? o f::human f::non-human-animal)) (F::trajectory -))
    )

;; umls
(define-type ont::invertebrate
    :wordnet-sense-keys ("invertebrate%1:05:00")
    :sem (F::Phys-obj (F::intentional +) (F::origin (? o f::non-human-animal)) (F::trajectory -))
    :parent ont::animal
    )

;;umls
(define-type ont::vertebrate
    :wordnet-sense-keys ("vertebrate%1:05:00")
    :parent ont::animal
    )

;;umls
(define-type ont::amphibian
    :wordnet-sense-keys ("amphibian%1:05:00")
    :parent ont::vertebrate
    )

;;umls
(define-type ont::bird
    :wordnet-sense-keys ("bird%1:05:00")
    :parent ont::vertebrate
    )

;;umls
(define-type ont::fish
    :wordnet-sense-keys ("fish%1:05:00")
    :parent ont::vertebrate
    :sem (F::Phys-obj (:required (F::Object-Function F::comestible)))
    )

;;umls
(define-type ont::reptile
    :wordnet-sense-keys ("reptile%1:05:00")
    :parent ont::vertebrate
    )

;;umls
(define-type ont::mammal
    :wordnet-sense-keys ("mammal%1:05:00" "mammalian%1:05:00")
    :parent ont::vertebrate
    )

;; dog, bear, ...
(define-type ont::nonhuman-animal
    :sem (F::Phys-obj (F::intentional +) (F::origin F::non-human-animal) (F::trajectory -))
    :parent ont::mammal
    )

(define-type ont::insect
    :wordnet-sense-keys ("insect%1:05:00")
    :parent ont::invertebrate
    )

;; carnivore, vegetarian -- can be animals as well as human
(define-type ONT::eater
    :wordnet-sense-keys ("eater%1:18:00")
    :parent ONT::organism
    :sem (F::Phys-obj (F::origin f::living) (F::trajectory -))
    )


(define-type ONT::person
    :wordnet-sense-keys ("person%1:03:00" "people%1:14:00")
    :parent ONT::mammal ;; umls
    :sem (F::Phys-obj (F::form F::solid-object)
		      (F::spatial-abstraction F::spatial-point)
		      (F::origin F::Human)
		      (F::Object-Function F::Occupation)
		      (f::mobility f::self-moving)
		      (F::Container -) (F::intentional +) (F::information -))
    )

;; self
(define-type ont::referential-person
    :wordnet-sense-keys ("self%1:18:00")
    :parent ont::person
    )

;; angel, devil
(define-type ONT::supernatural-being
    :wordnet-sense-keys ("supernatural_being%1:18:00")
    :parent ONT::organism ;; for parsing purposes, they are like people ;) ;; umls
    :sem (F::Phys-obj (F::form F::solid-object)
		      (F::spatial-abstraction F::spatial-point)
		      (F::origin F::Human)
		      (F::Object-Function F::Occupation)
		      (f::mobility f::self-moving)
		      (F::Container -) (F::intentional +) (F::information -))
    )

(define-type ONT::imaginary-being
    :wordnet-sense-keys ("imaginary_being%1:18:00")
    :parent ONT::organism ; not all of them are people, e.g., unicorn
    :sem (F::Phys-obj (F::form F::solid-object)
		      (F::spatial-abstraction F::spatial-point)
		      ;(F::origin F::Human)
		      ;(F::Object-Function F::Occupation)
		      (f::mobility f::self-moving)
		      (F::Container -) (F::intentional +) (F::information -))
    )

(define-type ONT::user
    :wordnet-sense-keys ("user%1:18:00")
    :parent ONT::person
    )

;; mother, father...
(define-type ONT::family-relation
    :wordnet-sense-keys ("relative%1:18:00" "relation%1:18:00")
    :parent ONT::person
    :arguments ((:required ONT::FIGURE (F::Phys-obj (F::intentional +) (F::origin F::human) (F::trajectory -)))
		)
    )


;; person that has a nonspecific ont::of role
;; subscriber, attendee
(define-type ONT::person-reln
    :parent ONT::person
    :arguments ((:required ONT::FIGURE ((? lof F::Phys-obj f::abstr-obj f::situation)))
		)
    )

;; engineer, artist, scientist
(define-type ONT::professional
    :wordnet-sense-keys ("professional%1:18:00" "professional_person%1:18:00"
						"skilled_worker%1:18:00")
    :comment "a person defined by a role that they play. e.g., doctor, leader, ..."
    :parent ONT::PERSON
    :sem (F::Phys-obj (F::form F::solid-object)
		      (F::spatial-abstraction F::spatial-point)
		      (F::origin F::Human)
		      (F::Object-Function F::Occupation)
		      (F::Container -) (F::intentional +) (F::information -))
    :arguments ((:OPTIONAL ONT::FIGURE ((? lof F::Phys-obj f::abstr-obj)))
		(:optional ont::norole)  ;; here for "driver", "fixer": see note in DRV-NOM-RELN-TEMPL 
		)
    )

;; scholar, student, graduate
(define-type ONT::scholar
    :wordnet-sense-keys ("student%1:18:01" "bookman%1:18:00" "scholarly_person%1:18:00" "scholar%1:18:00" "student%1:18:00" "pupil%1:18:00" "educatee%1:18:00")
    :parent ONT::professional
    :arguments ((:OPTIONAL ONT::FIGURE ((? lof F::Phys-obj f::abstr-obj)))
		)
    )

(define-type ONT::author
    :wordnet-sense-keys ("writer%1:18:00" "author%1:18:00")
    :parent ONT::professional
    :arguments ((:OPTIONAL ONT::FIGURE (F::Phys-obj (F::origin F::artifact) (f::information f::information-content))))
    )

;; expert, specialist, afficionado, gourmet, gourmand
(define-type ONT::specialist
    :wordnet-sense-keys ("expert%1:18:00")
    :parent ONT::person-reln
    )

;; doctor, nurse
(define-type ONT::health-professional
    :wordnet-sense-keys ("health_professional%1:18:00" "primary_care_provider%1:18:00" "PCP%1:18:00" "health_care_provider%1:18:00" "caregiver%1:18:01")
    :parent ONT::professional
    )

;; runner, swimmer
(define-type ONT::athlete
    :wordnet-sense-keys ("athlete%1:18:00" "jock%1:18:00")
    :parent ONT::person
    )

;; passenger, traveller
(define-type ONT::traveller
    :wordnet-sense-keys ("traveler%1:18:00" "traveller%1:18:00")
    :parent ONT::person
    )

;; consumer, customer, client
(define-type ONT::consumer
    :wordnet-sense-keys ("consumer%1:18:00")
    :parent ONT::person
    )

;; friend, buddy
(define-type ONT::friend
    :wordnet-sense-keys ("friend%1:18:00")
    :parent ONT::person-reln
    )

;; woman, lady, female
(define-type ONT::female-person
    :wordnet-sense-keys ("woman%1:18:00" "adult_female%1:18:00")
    :parent ONT::person
    )

;; girl
;; woman, lady, female
(define-type ONT::female-child
    :wordnet-sense-keys ("girl%1:18:02")
    :parent ONT::female-person
    )

;; man, male
(define-type ONT::male-person
    :wordnet-sense-keys ("man%1:18:00" "adult_male%1:18:00")
    :parent ONT::person
    )

;; boy
(define-type ONT::male-child
    :wordnet-sense-keys ("boy%1:18:00")
    :parent ONT::male-person
    )

;; child, kid, girl, boy (need multiple inheritance for male, female)
(define-type ONT::child
    :wordnet-sense-keys ("child%1:18:00")
    :parent ONT::family-relation
    )

;; nationality, regional identity
(define-type ont::identity-and-origin
    :parent ont::person-reln
)

;; american, german, 
(define-type ont::person-of-nationality
    :wordnet-sense-keys ("nation%1:14:01" "ethnic_group%1:14:00")
    :comment "A person classified by their nationality"
    :parent ont::identity-and-origin
    )

;;British, 
#|(define-type ont::persons-of-nationality
    :wordnet-sense-keys ()
    :parent ont::person-of-nationality)
|#

;; north american, south american, asian etc
(define-type ont::regional-identity
 :parent ont::identity-and-origin
 :wordnet-sense-keys ("north_american%1:18:00" "asian%1:18:00" "european%1:18:00")
)

;; foreigner 
(define-type ont::foreigner
 :parent ont::identity-and-origin
 :wordnet-sense-keys ("foreigner%1:18:00")
)

;; citizen, inhabitant, resident
(define-type ONT::inhabitant
    :wordnet-sense-keys ("indweller%1:18:00" "denizen%1:18:00" "dweller%1:18:00" "habitant%1:18:00" "inhabitant%1:18:00" "citizen%1:18:00" "national%1:18:00" "native%1:18:01" "people%1:14:01")
    :parent ont::identity-and-origin
)

;; hindu, buddhist, christian
(define-type ont::religious-identity
 :parent ont::identity-and-origin
 :wordnet-sense-keys ("hindu%1:18:01" "buddhist%1:18:00" "christian%1:18:00")
)

;; sender, receiver, caller
(define-type ONT::communication-party
    :wordnet-sense-keys ("communicator%1:18:00")
    :parent ONT::person-reln
    )

;; insider, outsider, stranger
(define-type ONT::member-reln
    :parent ONT::person-reln
    )

;; intruder, trespasser, interloper
;; not relational?
(define-type ONT::entrant
    :wordnet-sense-keys ("entrant%1:18:02")
    :parent ONT::person
    )



;; owner, possessor
(define-type ONT::possessor-reln
    :wordnet-sense-keys ("possessor%1:18:00" "owner%1:18:02" "owner%1:18:00" "proprietor%1:18:00")
    :parent ONT::person-reln
    )


;;; NOTE: myrosia set container to +. Really we need to figure out what to do with default binary feats
(define-type ONT::VEHICLE
    :wordnet-sense-keys ("transport%1:06:00" "conveyance%1:06:00" "vehicle%1:06:00")
    :parent ONT::MANUFACTURED-OBJECT
    :sem (F::Phys-obj (F::Object-Function F::vehicle) (F::MOBILITY F::Self-moving)
		      (F::trajectory +) (F::container (? fcont + -)))
    )

(define-type ONT::AIR-VEHICLE
    :wordnet-sense-keys ("plane%1:06:01" "aeroplane%1:06:00" "airplane%1:06:00" "aircraft%1:06:00")
    :parent ONT::VEHICLE
    :sem (F::Phys-obj (F::mobility F::air-movable) (F::form F::Enclosure) (F::spatial-abstraction F::spatial-point))
    )

(define-type ONT::LAND-VEHICLE
    :wordnet-sense-keys ("motorcar%1:06:00" "machine%1:06:01" "automobile%1:06:00" "auto%1:06:00" "car%1:06:00" "railroad_train%1:06:00" "train%1:06:00" "wheeled_vehicle%1:06:00")
    :parent ONT::VEHICLE
    :sem (F::Phys-obj (F::mobility F::land-movable) (F::form F::Enclosure) (F::spatial-abstraction F::spatial-point))
    )

;;  20111006 for obtw demo
(define-type ONT::TRUCK
    :parent ONT::LAND-VEHICLE
    :wordnet-sense-keys ("truck%1:06:00" "motortruck%1:06:00")
    )

;;  20111006 for obtw demo
(define-type ONT::TOW-TRUCK
    :parent ONT::TRUCK
    :wordnet-sense-keys ("tow_truck%1:06:00" "tow_car%1:06:00")
    )

;;  20111016 for obtw demo
(define-type ONT::FIRE-TRUCK
    :parent ONT::TRUCK
    :wordnet-sense-keys ("fire_engine%1:06:00" "fire_truck%1:06:00")
    )

(define-type ONT::stretcher
    :parent ONT::MANUFACTURED-OBJECT
    :wordnet-sense-keys ("stretcher%1:06:00")
    :sem (F::Phys-obj (F::container +))
    )


(define-type ONT::vessel
    :parent ONT::MANUFACTURED-OBJECT
    :comment "an object designed to hold something"
    :wordnet-sense-keys ("vessel%1:06:01")
    :sem (F::Phys-obj (F::container +)
		      (F::Object-Function F::Container-object)
		       (F::Form F::Enclosure))
    )

(define-type ONT::VEHICLE-CONTAINER
    :parent ONT::vessel
   ;; :wordnet-sense-keys ("vessel%1:06:01")
    :sem (F::Phys-obj (F::Mobility F::non-self-moving)
		      )
    :arguments ((:OPTIONAL ONT::Contents (F::PHYS-OBJ (F::MOBILITY F::MOVABLE) (F::Spatial-abstraction F::Spatial-point))
			   )
		)
    )

(define-type ONT::vehicle-part
    :parent ONT::VEHICLE
    )

(define-type ONT::boxcar
    :wordnet-sense-keys ("boxcar%1:06:00")
    :parent ONT::VEHICLE-CONTAINER
    )

(define-type ONT::TANK
    :parent ONT::Vessel
     :wordnet-sense-keys ("tank%1:06:00")
    )

(define-type ONT::TANKER
    :parent ONT::VEHICLE-CONTAINER
    :wordnet-sense-keys ("tank%1:06:02")
    )

(define-type ONT::unfortunate
    :parent ONT::PERSON
    :wordnet-sense-keys ("unfortunate%1:18:00" "victim%1:18:00")
    )

(define-type ont::patient
    :parent ont::unfortunate
    :wordnet-sense-keys ("patient%1:18:00")
    )

;; 20111006 for obtw demo
(define-type ONT::scout
    :parent ONT::PERSON
    :wordnet-sense-keys ("scout%1:18:00")
    )

;;; swift 24/01/02 added types addressee, recipient, focus-of-emotion to replace roles labelled 'beneficiary' in e.g. verbs of communication
(define-type ONT::ADDRESSEE
    :parent ONT::PERSON
    :wordnet-sense-keys ("addressee%1:18:00")
    )

(define-type ONT::OFFICIAL
    :comment "Person that serves in some administrative role"
     :wordnet-sense-keys ("leader%1:18:00")
    :parent ONT::PERSON
    )


(define-type ONT::RECIPIENT
    :parent ONT::PERSON
    :wordnet-sense-keys ("recipient%1:18:00")
    )

(define-type ONT::FOOD
    :wordnet-sense-keys ("food%1:03:00" "food%1:13:00" "meal%1:13:00" "meal%1:13:01" "nutrient%1:03:00" "nutrient%1:03:01" "nutriment%1:13:00" "solid_food%1:13:00")
    :parent ONT::SUBSTANCE
    :sem (F::Phys-obj (:required (F::Object-Function F::comestible))(:default (F::form f::solid)))
    )

(define-type ONT::LIGHT
    :parent ONT::SUBSTANCE
    :sem (F::Phys-obj (F::origin F::non-living))
    :wordnet-sense-keys ("light%1:15:00" "light%1:19:00" "light%1:26:00")
    )

(define-type ONT::PRODUCE
    :parent ONT::FOOD
    :wordnet-sense-keys ("produce%1:13:00" )
    :sem (F::Phys-obj (F::origin F::natural) (F::form F::Solid))
    )

;; for territorial districts, like country, state, city, town, village, downtown, park
(define-type ONT::DISTRICT
    :parent ONT::POLITICAL-REGION
    :wordnet-sense-keys ("district%1:15:00")
    :sem (F::Phys-obj (f::origin f::artifact) (F::trajectory -)(F::mobility f::fixed) )
    )

(define-type ONT::public-service-facility
    :parent ONT::facility
   ;; :sem (F::Phys-obj (F::object-function F::Building))
    :wordnet-sense-keys ("local_department%1:14:00")
    )

;; lab, laboratory
(define-type ONT::research-facility
    :parent ONT::facility
    :wordnet-sense-keys ("research_center%1:06:00" "lab%1:06:00" "laboratory%1:06:00" "research_lab%1:06:00" "research_laboratory%1:06:00" "science_lab%1:06:00" "science_laboratory%1:06:00")
    ;;:sem (F::Phys-obj (F::object-function F::Building))
    )

;; pool, gym
(define-type ONT::athletic-facility
    :parent ONT::facility
    :wordnet-sense-keys ("athletic_facility%1:06:00")
    ;;:sem (F::Phys-obj (F::object-function F::Building))
    )

;; office
(define-type ONT::business-facility
    :parent ONT::facility
    :wordnet-sense-keys ("office_building%1:06:00")
    ;;:sem (F::Phys-obj (F::object-function F::Building))
    )

;; store, shop
(define-type ONT::commercial-facility
    :parent ONT::facility
    :wordnet-sense-keys ("shop%1:06:00" "outlet%1:06:01")
    ;;:sem (F::Phys-obj (F::object-function F::Building))
    )

;; disco
(define-type ONT::entertainment-establishment
    :parent ONT::commercial-facility
    :wordnet-sense-keys ("discotheque%1:06:00")
    ;;:sem (F::Phys-obj (F::object-function F::Building))
    )

;; cafe, etc.
(define-type ONT::eating-establishment
    :parent ONT::commercial-facility
;    :wordnet-sense-keys ("eating_place%1:06:00" "eatery%1:06:00") #these are synsets to restaurant%1:06:00 (see ont::restaurant)
    )

;; restaurant
(define-type ont::restaurant
    :wordnet-sense-keys ("restaurant%1:06:00")
    :parent ont::eating-establishment
    )

;; coffee shop, cafe
(define-type ont::coffee-shop
    :wordnet-sense-keys ("coffeehouse%1:06:00" "coffee_shop%1:06:00" "coffee_bar%1:06:00")
    :parent ont::eating-establishment
    )

;; bar, lounge
(define-type ONT::drinking-establishment
    :parent ONT::entertainment-establishment
    :wordnet-sense-keys ("barroom%1:06:00" "saloon%1:06:00" "ginmill%1:06:00" "taproom%1:06:00")
    ;;:sem (F::Phys-obj (F::object-function F::Building))
    )

;; warehouse
(define-type ONT::storage-facility
    :parent ONT::facility
    :wordnet-sense-keys ("warehouse%1:06:00" "storage_warehouse%1:06:00")
    ;;:sem (F::Phys-obj (F::object-function F::Building))
    )

;; factory, plant
(define-type ONT::production-facility
    :parent ONT::facility
    :wordnet-sense-keys ("factory%1:06:00" "manufacturing_plant%1:06:00" "manufactory%1:06:00")
    ;;:sem (F::Phys-obj (F::object-function F::Building))
    )

(define-type ONT::transportation-facility
    :parent ONT::facility
    :wordnet-sense-keys ("terminal%1:06:00" "terminus%1:06:01" "depot%1:06:00")
    ;;:sem (F::Phys-obj (F::object-function F::Building))
    )

(define-type ont::airport
    :wordnet-sense-keys ("airport%1:06:00" "airdrome%1:06:00" "aerodrome%1:06:00" "drome%1:06:00")
    :parent ont::transportation-facility
    )

#|
(define-type ont::structure
    :parent ONT::facility
    :wordnet-sense-keys ("structure%1:06:00" "construction%1:06:00")
    :sem (F::Phys-obj (F::object-function F::Building) (f::container +))
    )
|#

;; house, apartment -- general types of places where people live
(define-type ONT::lodging
    :wordnet-sense-keys ("housing%1:06:00" "lodging%1:06:00" "living_accommodations%1:06:00")
    :parent ONT::facility
    ;;:sem (F::Phys-obj (F::object-function F::Building))
    )

(define-type ONT::health-care-facility
    :parent ONT::facility
    :wordnet-sense-keys ("hospital%1:06:00" "hospital%1:14:00" "fire_department%1:14:00" "police_department%1:14:00")
    ;;:sem (F::Phys-obj (F::object-function F::Building))
    )

(define-type ONT::education-facility
    :parent ONT::facility
    :wordnet-sense-keys ("school%1:06:00" "university%1:06:00" "college%1:06:00")
   ;; :sem (F::Phys-obj (F::object-function F::Building))
    )

(define-type ONT::religious-facility
    :parent ONT::facility
    :wordnet-sense-keys ("place_of_worship%1:06:00" "house_of_prayer%1:06:00" "house_of_god%1:06:00" "house_of_worship%1:06:00")
    ;;:sem (F::Phys-obj (F::object-function F::Building))
    )

;; hotel, inn, guesthouse
(define-type ONT::accommodation
    :parent ONT::lodging
    :wordnet-sense-keys ("hotel%1:06:00")
    ;;:sem (F::Phys-obj (F::object-function F::Building))
    )

(define-type ONT::bedandbreakfast
    :parent ONT::accommodation
    :wordnet-sense-keys ("boarding_house%1:06:00" "boarding_house%1:06:00")
    )


;; room, hall -- for internal spaces of buildings & other structures
(define-type ont::internal-enclosure
    :wordnet-sense-keys ("room%1:06:00")
    :parent ONT::general-structure
    :sem (F::Phys-obj (F::origin F::Artifact)(F::trajectory -)
		      (F::mobility f::fixed) (f::container +))
    )

;; door, window, gate
(define-type ont::structural-opening
    :comment "part of a structure that can open to allow access"
    :parent ONT::general-structure
    :wordnet-sense-keys ("entrance%1:06:00" "entranceway%1:06:00" "entryway%1:06:00" "entry%1:06:00" "window%1:06:01" "window%1:06:05"
					    "movable_barrier%1:06:00")
    :arguments ((:OPTIONAL ONT::FIGURE (F::Phys-obj))
		)
    )

;; wing, annex
(define-type ont::structural-component
    :parent ONT::general-structure
    :wordnet-sense-keys ("annex%1:06:00" "annexe%1:06:00" "extension%1:06:00" "wing%1:06:00")
    :arguments ((:OPTIONAL ONT::FIGURE (F::Phys-obj))
		)
    )



;; wall, ceiling, floor
(define-type ont::structure-internal-component
    :parent ONT::structural-component
    :wordnet-sense-keys ("wall%1:06:00" "wall%1:06:03" "ceiling%1:06:00" "floor%1:06:00"
					"passageway%1:06:00")
    :arguments ((:OPTIONAL ONT::FIGURE (F::Phys-obj))
		)
    )

(define-type ont::structure-external-component
    :comment "parts of exterior of buildings: e.g., roof, window"
    :parent ONT::structural-component
    :wordnet-sense-keys ("roof%1:06:00")
    :arguments ((:OPTIONAL ONT::FIGURE (F::Phys-obj))
		)
    )

(define-type ont::step
    :comment "part of a staircase"
    :parent ONT::structural-component
    :wordnet-sense-keys ("step%1:06:00")
    :arguments ((:OPTIONAL ONT::FIGURE (F::Phys-obj))
		)
    )

(define-type ont::furnishings
    :comment "e.g.,  chair, desk, table"
    :parent ONT::manufactured-object
    :wordnet-sense-keys ("furniture%1:06:00" "piece_of_furniture%1:06:00" "article_of_furniture%1:06:00")
    :sem (F::Phys-obj (:required (F::Form F::solid-object)) (:default (F::Object-Function F::furniture)))
    )

(define-type ont::furnishings-component
    :comment "parts typically of furnishings: e.g., drawer"
    :parent ont::furnishings
    )

(define-type ont::bedding
    :comment "objects related to bedding: pillow, blanket"
    :parent ONT::furnishings
    :wordnet-sense-keys ("bedclothes%1:06:00" "bed_clothing%1:06:00" "pillow%1:06:00")
    :sem (F::Phys-obj (F::Form F::solid-object) (F::Object-Function F::furniture))
    )

;; anywhere, anyplace, ...
(define-type ONT::wh-location
    :parent ONT::location
    :arguments ((:OPTIONAL ONT::GROUND) ;; ppword-adv templates have an implicit subcat
		)
    )

(define-type ONT::loc-as-point
    :wordnet-sense-keys ("point%1:15:00")
    :parent ONT::location
    )

(define-type ONT::location-by-description
    :parent ONT::location
    )

(define-type ONT::loc-as-defined-by-reln-to-ground
    :parent ONT::location
    )

(define-type ONT::loc-as-area
    :comment "places that occupy space"
    :parent ONT::location-by-description
    )

(define-type ONT::loc-def-by-goal
    :comment " place related to a trajectory by a goal: destination"
    :parent ONT::loc-as-area
    )

(define-type ONT::loc-def-by-intersection
    :comment " place defined by the intersection of two lines/areas"
    :parent ONT::loc-as-area
    )

; oil field, wheat field
(define-type ONT::area-def-by-use
    :wordnet-sense-keys ("field%1:15:00" "field%1:15:05" "pasture%1:15:00")
    :comment "places defined by their function: e.g.,  lot, plot, region, scene, section, site, territory, zone"
    :parent ONT::loc-as-area
    )


(define-type ont::garden-grounds
    :wordnet-sense-keys ("garden%1:06:00" "grounds%1:06:00" "plot%1:15:00")
    :parent ONT::area-def-by-use
    )

(define-type ONT::workplace
    :wordnet-sense-keys ("work%1:06:01")
    :parent ONT::area-def-by-use
    )

(define-type ONT::mine
    :wordnet-sense-keys ("mine%1:06:01")
    :parent ONT::workplace
    )

(define-type ONT::fishery
    :wordnet-sense-keys ("fishery%1:06:00" "fish_farm%1:06:00")
    :parent ONT::workplace
    )

(define-type ONT::bakery
    :wordnet-sense-keys ("bakery%1:06:00")
    :parent ONT::workplace
    )

(define-type ONT::lumberyard
    :wordnet-sense-keys ("lumberyard%1:06:00")
    :parent ONT::workplace
    )

(define-type ONT::farm
    :wordnet-sense-keys ("farm%1:06:00")
    :parent ONT::workplace
    )

(define-type ONT::loc-defined-by-contrast
    :comment "Objects that are subparts of larger surface but delineated by a contrasting property: e.g., spot, patch"
    :parent ONT::loc-as-area
    :wordnet-sense-keys ("spot%1:07:00" "spot%1:07:01" "spot%1:10:02")
    )

(define-type ONT::loc-wrt-ground-as-spatial-obj
    :parent ONT::loc-as-defined-by-reln-to-ground
    )

(define-type ONT::startpoint
    :parent ONT::loc-wrt-ground-as-spatial-obj
    :wordnet-sense-keys ("beginning%1:15:00" "beginning%1:09:00")
    )

(define-type ONT::endpoint
    :parent ONT::loc-wrt-ground-as-spatial-obj
    :wordnet-sense-keys ("end%1:15:00" "end%1:15:02")
    )

(define-type ONT::waypoint
    :parent ONT::loc-wrt-ground-as-spatial-obj
    )

(define-type ONT::Center
    :parent ONT::loc-wrt-ground-as-spatial-obj
    :wordnet-sense-keys ("center%1:15:00" "center%1:15:01" "centre%1:15:01" "eye%1:15:00" "heart%1:15:00" "middle%1:15:00")
    :sem (F::Phys-obj (F::spatial-abstraction (? sa1 F::spatial-point)))
    :arguments ((:OPTIONAL ONT::FIGURE (F::PHYS-OBJ (F::SPATIAL-ABSTRACTION (? SA F::SPATIAL-REGION))))
		)
    )

; used by ORIENTS-TO
(define-type ONT::loc-wrt-orientation
    :parent ONT::loc-as-defined-by-reln-to-ground
    )

(define-type ONT::right-loc
    :parent ONT::loc-wrt-orientation
    :wordnet-sense-keys ("right%1:15:00")
    )

(define-type ONT::left-loc
    :parent ONT::loc-wrt-orientation
    :wordnet-sense-keys ("left%1:15:00")
    )

(define-type ONT::setting
    :parent ONT::location
    :wordnet-sense-keys ("setting%1:26:00" "background%1:26:00" "scope%1:26:00" "situation%1:26:00")
    )

(define-type ONT::ROUTE
    :parent ONT::functional-region
    ;;check this
    :wordnet-sense-keys ("path%1:17:00" "track%1:17:00" "course%1:17:00" "route%1:15:00")
    :sem (F::Phys-obj (F::origin F::non-living) (F::Form F::Geographical-Object)
		      (F::Object-Function F::Path) (F::Mobility F::Fixed)
		      ;; Myrosia 2007/11/20 marked as container + to account for examples like "this path contains a bulb/2 terminals/3 segments"
		      (f::Container +)
		      (F::spatial-abstraction (? sa F::line F::strip)) (F::trajectory +))
    :arguments ((:OPTIONAL ONT::FIGURE (F::Phys-obj))
;		(:OPTIONAL ONT::from-loc (F::Phys-obj))
;		(:OPTIONAL ONT::to-loc (F::Phys-obj))
;		(:OPTIONAL ONT::via (F::Phys-obj))
		)
    )

;; road
(define-type ONT::road
    :parent ONT::route
    :wordnet-sense-keys ("road%1:06:00")
    )

(define-type ont::lane
    :parent ont::road
    :wordnet-sense-keys ("lane%1:06:00")
    )

;; highway, expressway
(define-type ONT::highway
    :parent ONT::route
    :wordnet-sense-keys ("highway%1:06:00")
    )

;; thruway
(define-type ONT::thruway
    :parent ONT::highway
    :wordnet-sense-keys ("throughway%1:06:00")
    )

(define-type ONT::tunnel
    :parent ONT::route
    :wordnet-sense-keys ("tunnel%1:06:00")
    )

(define-type ONT::shortcut
    :parent ONT::route
    :wordnet-sense-keys ("shortcut%1:06:00")
    )

;; thoroughfare, street, avenue
(define-type ONT::thoroughfare
    :parent ONT::route
    :wordnet-sense-keys ("thoroughfare%1:06:00" "street%1:06:00" "avenue%1:06:00" "boulevard%1:06:00")
    )

(define-type ONT::junction
    :parent ONT::LOCATION-by-description
    :wordnet-sense-keys ("junction%1:06:00")
    :sem (F::Phys-obj (F::spatial-abstraction (? sa1 F::spatial-point)))
    :arguments ((:OPTIONAL ONT::FIGURE (F::PHYS-OBJ (F::SPATIAL-ABSTRACTION (? SA F::LINE F::STRIP)) (F::FORM F::GEOGRAPHICAL-OBJECT) (F::MOBILITY F::FIXED)))
		(:OPTIONAL ONT::FIGURE1 (F::PHYS-OBJ (F::SPATIAL-ABSTRACTION (? SA2 F::LINE F::STRIP)) (F::FORM F::GEOGRAPHICAL-OBJECT) (F::MOBILITY F::FIXED)))
		)
    )

(define-type ONT::coordinate
    :parent ONT::LOCATION
    :wordnet-sense-keys ("coordinate%1:09:00" "co-ordinate%1:09:00")
    :sem (F::Phys-obj (F::spatial-abstraction F::spatial-point) )
    ;; both of these arguments should be numbers -- they are restricted in the templates since numbers don't have semantics
    ;; e.g. point 4 5
    :arguments ((:required ONT::FIGURE) ;;x
		(:required ONT::FIGURE1) ;;y
		)
    )

(define-type ONT::Corner
    :parent ONT::LOCATION-by-description
    :wordnet-sense-keys ("corner%1:15:02" "corner%1:06:00")
    :sem (F::Phys-obj (F::spatial-abstraction (? sa1 F::spatial-point)) (f::container +))
    :arguments ((:OPTIONAL ONT::FIGURE (F::PHYS-OBJ (F::FORM F::OBJECT) (F::SPATIAL-ABSTRACTION (? SA F::STRIP F::SPATIAL-REGION))))
		)
    )

(define-type ONT::edge
    :parent ONT::LOCATION-by-description
    :wordnet-sense-keys ("boundary%1:25:00" "edge%1:25:00" "bound%1:25:00")
    :sem (F::Phys-obj (F::spatial-abstraction (? sa1 F::line)))
    :arguments ((:OPTIONAL ONT::FIGURE (F::PHYS-OBJ (F::FORM F::OBJECT) (F::SPATIAL-ABSTRACTION (? SA F::SPATIAL-REGION))))                             )
    )


;;; These are the location that relate to well-formed physical objects - e.g. the top of smth
(define-type ONT::object-dependent-location
    :wordnet-sense-keys ("region%1:15:00")
    :COMMENT "these are locations defined relative to another object"
    :parent ONT::LOCATION
    :arguments ((:OPTIONAL ONT::FIGURE (F::PHYS-OBJ (F::Form F::object)))
		)
    )

(define-type ONT::TOP-LOCATION
    :wordnet-sense-keys ("top%1:15:01" "top%1:15:00")
    :parent ONT::object-dependent-location
    )

(define-type ONT::BOTTOM-LOCATION
    :wordnet-sense-keys ("bottom%1:15:00" "bottom%1:15:01")
    :parent ONT::object-dependent-location
    )

#|
(define-type ONT::END-LOCATION
    :wordnet-sense-keys ("end%1:15:00" "end%1:15:02")
    :parent ONT::object-dependent-location
    )
|#

(define-type ONT::SIDE-LOCATION
    :wordnet-sense-keys ("side%1:15:02")
    :parent ONT::object-dependent-location
    )

(define-type ONT::SURFACE-LOCATION
    :wordnet-sense-keys ("surface%1:06:00" "surface%1:15:00")
    :parent ONT::object-dependent-location
    )

;; noun sense of north, south, east, west
; used by ORIENTS-TO
(define-type ONT::cardinal-point
    :parent ONT::LOCATION-by-description
    :wordnet-sense-keys ("cardinal_compass_point%1:24:00")
    :arguments ((:OPTIONAL ONT::FIGURE (F::PHYS-OBJ))
		)
    )

(define-type ONT::Destination
    :parent ONT::LOCATION
    :wordnet-sense-keys ("finish%1:15:00" "destination%1:15:00" "goal%1:15:00")
    :arguments ((:OPTIONAL ONT::FIGURE (F::PHYS-OBJ))
		)
    )

;SAME AS ONT::STARTPOINT
;;; check this
;(define-type ONT::Origin
;    :parent ONT::LOCATION-by-description
;    :wordnet-sense-keys ("beginning%1:15:00" "origin%1:15:00" "root%1:15:00" "source%1:15:00")
;    :arguments ((:OPTIONAL ONT::FIGURE (F::PHYS-OBJ))
;		)
;    )

#|
;; middle of the road
(define-type ONT::location-reln
    :parent ONT::LOCATION
    :sem (F::Phys-obj (F::spatial-abstraction (? sa1 F::spatial-point)))
    :arguments ((:OPTIONAL ONT::FIGURE (F::PHYS-OBJ (F::SPATIAL-ABSTRACTION (? SA F::Strip F::SPATIAL-REGION f::Line))))
		)
    )

;;; This is for locations defined dependent on a line
(define-type ONT::line-dependent-location
    :parent ONT::LOCATION-reln
    :arguments ((:OPTIONAL ONT::FIGURE (F::PHYS-OBJ (F::spatial-abstraction (? sa F::line F::strip))))
		)
    )
|#


;;; > REPRESENTATIONS

;; Types like diagram and image, which are physical representations of physical or abstract objects
;; these are typically containers (e.g. diagram, map) to which things can be added
(define-type ONT::PHYS-REPRESENTATION
    :parent ONT::PHYS-OBJECT
    :wordnet-sense-keys ("representation%1:04:01" "representation%1:06:00")
    :comment "anything used to encode/store or share information physically, e.g., it could be a device to store information, or a physical representation to enable encoding information etc."
    :sem (F::Phys-obj (:required (F::Form F::Object) (F::Origin F::Artifact) (F::Intentional -) (F::object-function F::representation) (f::trajectory +))
		      ;; swift -- we need trajectory + and f::strip so we can scroll up and down displays, pages, documents etc.
		      (:default (f::container +)  (F::information F::information-content) (f::mobility f::non-self-moving) (f::spatial-abstraction (? sab f::spatial-point f::spatial-region f::strip))))
    :arguments ((:OPTIONAL ONT::FIGURE))
    )

;; items in this class don't stand for something, but they can contain representations
;; e.g. page, book, display
(define-type ONT::info-medium
    :wordnet-sense-keys ("speech%1:10:01" "written_communication%1:10:00")
    :parent ONT::PHYS-REPRESENTATION ;info-holder
    :comment "a medium to transmit or store information"
    :sem (F::Phys-obj (F::information F::data)) ;; why (f::container -) here?
    )

;;INFO-Holder needs much more work
(define-type ONT::INFO-HOLDER
    :parent ONT::INFO-MEDIUM ;PHYS-REPRESENTATION
    :arguments ((:optional ONT::formal))
    :comment "a method/manner or device to store information, may also be used for sharing information"
    )

;; channel
(define-type ONT::communication-channel
    :parent ONT::INFO-MEDIUM ;info-holder
    :wordnet-sense-keys ("channel%1:10:01")
    :comment "means to transmit information (through broadcasting/telecasting)"
    )


;; items in this class "stand for" something, and have ont::of arguments
;(define-type ont::direct-representation
;    :comment "a copy of a document or other means to store information in a hard copy" ;"a physical object that conatins information (e.g., text, graphics)"
;    :parent ONT::info-holder
;    )
    

;; items in this class represent comments on something, and have ont::of arguments
(define-type ont::annotation
    :parent ONT::info-holder
    :wordnet-sense-keys ("comment%1:10:01" "commentary%1:10:00")
    :arguments (
;		(:optional ONT::Associated-information)
		)
    )

;; items in this class represent comments on something, and have ont::of arguments
(define-type ont::document
    :parent ONT::info-holder
    :wordnet-sense-keys ("document%1:06:00" "document%1:21:00")
    :comment "a report or other types of documents to record information"
    :arguments (
;;		(:optional ONT::Associated-information)
		)
    )

;; items in this class don't stand for something, but they can contain representations
;; e.g. page, book, display
;(define-type ONT::info-medium
;    :wordnet-sense-keys ("speech%1:10:01" "written_communication%1:10:00")
;    :parent ONT::PHYS-REPRESENTATION ;info-holder
;    :comment "a medium to transmit or store information"
;    :sem (F::Phys-obj (F::information F::data)) ;; why (f::container -) here?
;    )

;; narratives, stories, accounts, histories
(define-type ONT::chronicle
    :parent ONT::document ;:parent ONT::info-medium
    :wordnet-sense-keys ("record%1:10:03")
    )

;; review
;(define-type ont::review
;  :parent ont::chronicle
;  )

;; licenses or agreements
(define-type ont::official-document
    :parent ont::DOCUMENT ;direct-representation
    :wordnet-sense-keys ("document%1:10:00" "written_document%1:10:00" "papers%1:10:00")
    :comment "a document recording a formal agreement"
    )

;; e.g. invoice
(define-type ont::financial-statement
    :wordnet-sense-keys ("statement%1:10:01" "financial_statement%1:10:00")
    :parent ONT::official-document
    )

(define-type ont::letter-mail
    :parent ONT::DOCUMENT ;direct-representation
    :wordnet-sense-keys ("letter%1:10:00" "missive%1:10:00")
    )


;; copy, backup, cc, bcc
(define-type ont::copy
    :parent ONT::info-holder ;direct-representation
    :comment "a copy of a document or other means to store information in a hard copy"
    )

;(define-type ONT::reservation
; :parent ont::direct-representation
;
; )

(define-type ONT::INFO-REPRESENTATION
    :parent ONT::INFO-HOLDER
    :wordnet-sense-keys ("visual_communication%1:10:00");"representation%1:09:00")
    :comment "method to store/encode information for conveying it in a visual manner"
    )

(define-type ONT::MAP
    :parent ONT::INFO-REPRESENTATION ;direct-REPRESENTATION
    :wordnet-sense-keys ("map%1:06:00" "chart%1:06:00")
    :arguments ((:OPTIONAL ONT::FIGURE (F::Phys-obj (F::form F::Geographical-object)))
		)
    )

(define-type ONT::IMAGE
    :parent ONT::INFO-REPRESENTATION ;direct-REPRESENTATION
    :wordnet-sense-keys ("picture%1:06:00" "image%1:06:00" "icon%1:06:00" "ikon%1:06:00")
    :arguments ((:OPTIONAL ONT::FIGURE (F::Phys-obj))
		)
    )

(define-type ONT::CHART
    :parent ONT::INFO-REPRESENTATION ;direct-REPRESENTATION
    :wordnet-sense-keys ("chart%1:10:00" "table%1:14:00")
    :arguments ((:OPTIONAL ONT::FIGURE (?o (F::information F::information-content)))
		)
    )

(define-type ONT::PLOT-DIAGRAM-DRAWING
    :parent ONT::INFO-REPRESENTATION ;direct-REPRESENTATION
    :wordnet-sense-keys ("plot%1:10:01" "diagram%1:06:00");"spectrum%1:19:00" 
    :arguments ((:OPTIONAL ONT::FIGURE (?o (F::information F::information-content)))
		)
    )

;which kind?
(define-type ONT::DISPLAY
    :parent ONT::info-medium
    :wordnet-sense-keys ("blackboard%1:06:00" "display%1:06:00" "display%1:06:01" "screen%1:06:06")
    :comment "a device used to display information"
    )

(define-type ONT::PUBLICATION
    :parent ONT::DOCUMENT ;info-medium
    :wordnet-sense-keys ("publication%1:10:00")
    :comment "printed material such as a book"
    :arguments ((:OPTIONAL ONT::FIGURE (?o (F::information F::information-content)))
;		(:optional ont::originator (?org (f::intentional +))) ;; a book/paper/article by an author
		)
    )

;; book, paperback, hardcover
(define-type ONT::book
    :wordnet-sense-keys ("volume%1:06:00" "book%1:06:00" "book%1:10:00")
    :parent ONT::publication
    :arguments ((:OPTIONAL ONT::FIGURE (?o (F::information F::information-content)))
		)
    )

;; rss feed
(define-type ONT::feed
    :parent ONT::INFO-HOLDER ;publication
    )


;; proto type for article
(define-type ONT::article
    :wordnet-sense-keys ("article%1:10:00")
    :parent ONT::publication
    :arguments ((:OPTIONAL ONT::FIGURE (?o (F::information F::information-content)))
		)
    )

(define-type ONT::reference-work
    :wordnet-sense-keys ("reference_book%1:10:00" "reference%1:10:04" "reference_work%1:10:00" "book_of_facts%1:10:00")
    :parent ONT::publication
    :arguments ((:OPTIONAL ONT::FIGURE (?o (F::information F::information-content)))
		)
    )

(define-type ONT::database
    :parent ONT::INFO-HOLDER ;info-medium
    :wordnet-sense-keys ("database%1:10:00")
    )

;; list
(define-type ONT::list
    :wordnet-sense-keys ("list%1:10:00" "listing%1:10:00")
    :parent ONT::INFO-HOLDER ;database
    )

(define-type ONT::WEBSITE
    :parent ONT::INFO-HOLDER ;info-medium
    :wordnet-sense-keys ("web_site%1:10:00" "website%1:10:00" "internet_site%1:10:00" "site%1:10:00")
    )

;; items in this class can be "filled in"
;; e.g. application
(define-type ONT::template-info-object
    :parent ONT::INFO-HOLDER ;info-medium
    :comment "a templatic document to store and provide information"
    )

;; field, slot, box, blank
(define-type ONT::text-field
    :parent ONT::template-info-object
    )

;; for order, requisition -- things that are filled out and submitted
;; for a purchase action
(define-type ONT::order
    :parent ONT::template-info-object
    :arguments ((:REQUIRED ONT::FIGURE ((? lof f::abstr-obj f::phys-obj)))
		)
    :wordnet-sense-keys ("order%1:10:01")
    )

;; symbols representing objects/entities, such as squiggle, symbol etc.
;; normally these are not containers. A container like a map is ont::phys-representation
(define-type ONT::SYMBOLIC-REPRESENTATION
    :parent ONT::PHYS-representation
    :sem (F::Phys-obj (:required (F::Form F::Object) (F::Origin F::Artifact) (F::intentional -) (F::object-function F::representation) (F::information F::data))
		      (:default (F::Container -) (f::mobility f::non-self-moving) (f::spatial-abstraction (? n f::spatial-point f::spatial-region f::line))))
    :wordnet-sense-keys ("symbol%1:09:00" "url%1:10:00")
    :comment "method to represent information symbolically"
    )

;; the physical representation of text
(define-type ONT::TEXT-REPRESENTATION
     :wordnet-sense-keys ("hypertext%1:10:00" "matter%1:10:00")
    :parent ONT::symbolic-REPRESENTATION
    :comment "textual symbols used to represent information or properties of text used to represent information"
    )

;; font types and faces
(define-type ONT::font
    :parent ONT::text-REPRESENTATION
    :wordnet-sense-keys ("font%1:10:00")
    )

;; ligature, accent mark, diacritic, etc.
(define-type ONT::graphic-symbol
     :wordnet-sense-keys ("mark%1:10:03")
    :parent ONT::symbolic-REPRESENTATION
    :comment "a symbol to represent information but in a pictorial way"
    )

;; buttons, arrows (all non-textual)
(define-type ONT::ICON
    :wordnet-sense-keys ("symbol%1:10:00")
    :parent ONT::graphic-symbol
    )

;; link
(define-type ONT::LINK
    :parent ONT::symbolic-representation
    :wordnet-sense-keys ("link%1:06:03")
    )

; <


(define-type ONT::Wheel
    :parent ONT::MANUFACTURED-OBJECT
    :wordnet-sense-keys ("wheel%1:06:00")
    :sem (F::Phys-obj (F::form F::solid-object) (F::mobility F::non-self-moving))
    )

;this overlaps with structural opening
(define-type ONT::Window
    :parent ONT::MANUFACTURED-OBJECT
    :wordnet-sense-keys ("window%1:06:00")
    :sem (F::Phys-obj (F::form F::solid-object) (F::mobility F::non-self-moving))
    )

;;; these are objects specified by function, e.g. cargo
(define-type ONT::FUNCTIONAL-PHYS-OBJECT
    :parent ONT::PHYS-OBJECT
    :comment "These are objects that are described by their function in an activity- and have other physical descriptions: e.g., the things you are travelling with is LUGGAGE" 
    :wordnet-sense-keys ("instrumentality%1:06:00" "instrumentation%1:06:00")
    :sem (F::Phys-obj (:required)(:default (F::origin F::artifact) (F::form F::object) (F::mobility F::movable)))
    )

;;;storage, repository
(define-type ONT::repository
    :wordnet-sense-keys ("depository%1:06:00" "deposit%1:06:00" "depositary%1:06:00" "repository%1:06:00")
    :sem (F::Phys-obj (F::form F::solid-object) (F::container +))
    :parent ONT::functional-phys-object
    )

;;; By definition commodities are movables of any form, so we override the default for object
(define-type ONT::Commodity
    :wordnet-sense-keys ("commodity%1:06:00" "trade_good%1:06:00" "good%1:06:00")
    :parent ONT::FUNCTIONAL-PHYS-OBJECT
    :sem (F::Phys-obj (F::mobility F::movable) (F::form F::any-form))
    )

(define-type ONT::junk-reject
    :comment "a functional device that does not perform its function"
    :wordnet-sense-keys ("reject%1:09:00")
    :parent ONT::commodity
    )

;; product
(define-type ONT::product
    :wordnet-sense-keys ("ware%1:06:01" "product%1:06:01")
    :parent ONT::commodity
    )

;; crop
(define-type ONT::crop
    :wordnet-sense-keys ("crop%1:20:00" "crop%1:06:01")
    :parent ONT::commodity
    )

;; card
(define-type ONT::card
    :wordnet-sense-keys ("card%1:06:00")
    :parent ONT::manufactured-object
    )

;;; > Anatomy

(define-type ont::anatomy
    :parent ont::natural-object
    :wordnet-sense-keys ("human_body%1:08:00")
    )

;;; maybe this should be renamed to human-body-part?
;; what about animal body parts? the dog's leg?
(define-type ONT::body-part
    :wordnet-sense-keys ("body_part%1:08:00" )
    :parent ONT::anatomy
    :sem (F::Phys-obj ;;(F::origin F::living)
	  (f::intentional -) (f::form f::object)
	  (f::object-function f::comestible)  ;; f::body-part was never used, and caused problems with eating a leg of lamb, etc.
	  (f::container +))
 ;;; too strong, but better than unconstrained
    :arguments ((:OPTIONAL ONT::FIGURE (F::Phys-obj (F::origin F::natural)
						    (f::type (? xx ont::animal ont::meat))
						    (f::form (? xy f::object f::substance))))
		)
    )

(define-type ONT::external-body-part
    :wordnet-sense-keys ("external_body_part%1:08:00" "skin%1:08:00" "animal_skin%1:27:00" )
    :parent ONT::BODY-PART
    :sem (F::Phys-obj (F::intentional -))
    )

(define-type ont::head
     :wordnet-sense-keys ("head%1:08:00")
     :parent ONT::external-BODY-PART
     :sem (F::Phys-obj (F::intentional -))
    )

(define-type ONT::internal-body-part
    :parent ONT::BODY-PART
    :sem (F::Phys-obj (F::intentional -))
    :wordnet-sense-keys ("digestive_system%1:08:00" "somatic_cell%1:08:00" "tract%1:08:01" "vocal_cord%1:08:00" "brain%1:08:00" "blood_vessel%1:08:00" "connective_tissue%1:08:00" "organ%1:08:00" "cavity%1:08:00" "duct%1:08:00" "abdomen%1:08:00" "stomach%1:08:00" "lymphatic_tissue%1:08:00")
    )

(define-type ONT::CELL-PART
    :parent ONT::INTERNAL-BODY-PART
    )

(define-type ONT::CELL
    :wordnet-sense-keys ("cell%1:03:00")
    :parent ONT::CELL-PART
    )

(define-type ONT::CELL-COMPARTMENT
    :wordnet-sense-keys ("compartment%1:06:00::")
    :parent ONT::CELL-PART
    )


(define-type ONT::NUCLEUS
    :wordnet-sense-keys ("nucleus%1:08:00" "cell_nucleus%1:08:00" "karyon%1:08:00")
    :parent ONT::CELL-COMPARTMENT
    )

(define-type ONT::CYTOPLASM
    :wordnet-sense-keys ("cytoplasm%1:08:00")
    :parent ONT::CELL-COMPARTMENT
    )

(define-type ONT::PLASMA-MEMBRANE
    :wordnet-sense-keys ("cell_membrane%1:08:00" "cytomembrane%1:08:00" "plasma_membrane%1:08:00")
    :parent ONT::CELL-PART
    )

(define-type ONT::CYTOSOL
    :wordnet-sense-keys ("cytosol%1:08:00")
    :parent ONT::CELL-PART
    )

(define-type ONT::NUCLEOPLASM
    :wordnet-sense-keys ("nucleoplasm%1:08:00" "karyoplasm%1:08:00")
    :parent ONT::CELL-PART
    )

(define-type ONT::genome
    :wordnet-sense-keys ("genome%1:14:00")
    :parent ONT::CELL-PART
    )
; <

(define-type ONT::DEVICE
    :parent ONT::MANUFACTURED-OBJECT
    :wordnet-sense-keys ("buoy%1:10:00" "device%1:06:00" "equipment%1:06:00")
    :sem (F::Phys-obj (F::Origin F::Artifact))
    )

(define-type ont::appliance
    :wordnet-sense-keys ("appliance%1:06:00" "burner%1:06:00")
    :parent ont::device
    :sem (f::phys-obj (f::object-function f::instrument))
    )

(define-type ont::medical-instrument
    :wordnet-sense-keys ("medical_instrument%1:06:00")
    :parent ont::device
    :sem (f::phys-obj (f::object-function f::instrument))
    )

(define-type ont::medical-dressing
    :wordnet-sense-keys ("medical_dressing%1:06:00")
    :parent ont::medical-instrument ;covering
    )

;; walk up, down the stairs
(define-type ONT::STAIRS
    :wordnet-sense-keys ("stairway%1:06:00" "staircase%1:06:00")
    :parent ONT::general-structure
    )

;; laser
(define-type ONT::optical-device
    :parent ONT::device
    :wordnet-sense-keys ("optical_device%1:06:00")
    )

;; sonar
(define-type ONT::acoustic-device
    :parent ONT::device
    :wordnet-sense-keys ("acoustic_device%1:06:00" "sonar%1:06:00" "echo_sounder%1:06:00" "asdic%1:06:00")
    )

;; sensor
(define-type ONT::sensor
    :parent ONT::device
    :wordnet-sense-keys ("detector%1:06:00" "sensor%1:06:00" "sensing_element%1:06:00")
    :sem (f::phys-obj (F::mobility F::non-self-moving))
    )

;; weapon
(define-type ONT::weapon
    :parent ONT::device
    :wordnet-sense-keys ("weapon%1:06:00" "arm%1:06:01" "weapon_system%1:06:00" "warhead%1:06:00::")
    :sem (f::phys-obj (F::mobility F::non-self-moving))
    )

;; bomb
(define-type ONT::bomb
    :parent ONT::weapon
    :wordnet-sense-keys ("explosive_device%1:06:00")
    :sem (f::phys-obj (F::mobility F::non-self-moving))
    )

;; gun
(define-type ONT::firearm
    :parent ONT::weapon
    :wordnet-sense-keys ("gun%1:06:00")
    :sem (f::phys-obj (F::mobility F::non-self-moving))
    )

;; violin
(define-type ONT::musical-instrument
    :parent ONT::device
    :wordnet-sense-keys ("musical_instrument%1:06:00" "instrument%1:06:01" "brass_family%1:14:00"
						      "violin_family%1:14:00"   "woodwind_family%1:14:00"
						     )
    :sem (F::Phys-obj (F::mobility F::non-self-moving)(F::object-function F::instrument))
    )

;; sink, bathtub, fixture
(define-type ONT::fixture
    :parent ONT::MANUFACTURED-OBJECT
    :sem (F::Phys-obj (F::Origin F::Artifact))
    :wordnet-sense-keys ("fixture%1:06:00")
    )

;; conductor, semiconductor
(define-type ONT::conductor
    :parent ONT::device
    :wordnet-sense-keys ("semiconductor%1:27:00" "semiconducting_material%1:27:00" "semiconductor_device%1:06:00" "semiconductor_unit%1:06:00" "semiconductor%1:06:00")
    :sem (F::Phys-obj (F::Origin F::Artifact))
    )

#|(define-type ONT::EQUIPMENT
    :comment "manufactured objects that assist in ior perform some function"
    :parent ONT::MANUFACTURED-OBJECT
    :wordnet-sense-keys ("equipment%1:06:00")
    :sem (F::Phys-obj (F::Origin F::Artifact))
    )|#

(define-type ont::tool
    :parent ont::DEVICE ;EQUIPMENT
    :comment "a piece of equipment typically controlled manually (whether powered or not)"
    :wordnet-sense-keys ("tool%1:06:00")
    :sem (f::phys-obj (f::object-function f::instrument))
    )

(define-type ont::cutting-tool
    :comment "tools that assist in cutting material"
    :parent ont::TOOL
    :wordnet-sense-keys ("cutting_implement%1:06:00")
    :sem (f::phys-obj (f::object-function f::instrument))
    )

(define-type ont::drilling-tool
    :comment "tools that are used to create holes in material"
    :parent ont::TOOL
    :wordnet-sense-keys ("drill%1:06:00")
    :sem (f::phys-obj (f::object-function f::instrument))
    )

(define-type ont::hammer-tool
    :comment "tools that are used to hit things"
    :parent ont::TOOL
    :wordnet-sense-keys ("hammer%1:06:00")
    :sem (f::phys-obj (f::object-function f::instrument))
    )

(define-type ont::gripping-tool
    :comment "tools that are used to hit things"
    :parent ont::TOOL
    :wordnet-sense-keys ("plyers%1:06:00" "holding_device%1:06:00")
    :sem (f::phys-obj (f::object-function f::instrument))
    )


(define-type ONT::machine
    :parent ONT::device
    :wordnet-sense-keys ("machine%1:06:00")
    :sem (F::Phys-obj (F::Origin F::Artifact))
    )

(define-type ONT::product-model
    :parent ONT::device
    :wordnet-sense-keys ("model%1:09:03")
    :sem (F::Phys-obj (F::Origin F::Artifact))
    :arguments ((:OPTIONAL ONT::FIGURE (F::Phys-obj (F::Origin F::Artifact) (f::type ONT::manufactured-object)))
		)
    )

(define-type ONT::recording-device
    :parent ONT::MACHINE
    :wordnet-sense-keys ("recorder%1:06:01" "recording_equipment%1:06:00" "recording_machine%1:06:00")
    :sem (F::Phys-obj (F::Origin F::Artifact))
    )

(define-type ONT::projector
    :wordnet-sense-keys ("projector%1:06:00")
    :parent ONT::MACHINE
    :sem (F::Phys-obj (F::mobility F::non-self-moving)(f::form f::object) 
		      (F::object-function F::instrument))
    )

(define-type ONT::computer
    :wordnet-sense-keys ("computer%1:06:00" "computing_machine%1:06:00" "computing_device%1:06:00" "data_processor%1:06:00" "electronic_computer%1:06:00" "information_processing_system%1:06:00")
    :parent ONT::MACHINE
    :sem (F::Phys-obj (F::mobility F::non-self-moving)(f::form f::object) 
		      (F::object-function (? xx F::provides-service-on-off f::provides-service-up-down))
    )) 

;; powerbook, ibook
(define-type ONT::computer-model
    :parent ONT::computer
    )

;; ibm, macintosh, dell
(define-type ONT::computer-make
    :parent ONT::computer
    )

;; laptop, pc
(define-type ONT::computer-type
    :parent ONT::computer
    )

;; a physical arrangement of components, e.g. a stereo system
(define-type ONT::instrumentation
    :parent ONT::manufactured-object
    :sem (F::Phys-obj (F::mobility F::non-self-moving)(F::object-function F::instrument)(F::group +))
    )

(define-type ONT::computer-part
    :parent ONT::device
    :sem (F::Phys-obj (F::mobility F::non-self-moving)(F::object-function F::instrument) (f::origin f::artifact))
    :arguments ((:OPTIONAL ONT::FIGURE (F::Phys-obj (F::Origin F::Artifact)(F::mobility F::non-self-moving) ))
		)
    :wordnet-sense-keys ("chip%1:06:00" "console%1:06:02" "jack%1:06:04" "computer_circuit%1:06:00" "slot%1:06:02")
    )

(define-type ONT::computer-monitor
    :parent ONT::computer-part
    :sem (F::Phys-obj (F::mobility F::non-self-moving)(F::object-function F::instrument) (f::origin f::artifact))
    :wordnet-sense-keys ("monitor%1:06:02")
    )

(define-type ONT::computer-hardware
    :parent ONT::computer-PART
    :wordnet-sense-keys ("hardware%1:06:01")
    )

;; mouse, keyboard, keypad, trackball, scanner
(define-type ONT::computer-input-device
    :parent ONT::computer-hardware
    :wordnet-sense-keys ("keyboard%1:06:01" "mouse%1:06:00" "scanner%1:06:02")
    )

(define-type ONT::computer-firmware
    :parent ONT::computer-hardware
    :wordnet-sense-keys ("firmware%1:10:00")
    )

(define-type ONT::computer-network
    :parent ONT::computer-PART
    :wordnet-sense-keys ("computer_network%1:06:00" "digital_subscriber_line%1:06:00" "server%1:06:01")
    )

;; should these be hardware or software?
#|(define-type ONT::computer-network-type
    :parent ONT::computer-network
    )
|#

(define-type ONT::computer-card
    :parent ONT::COMPUTER-PART
    )

(define-type ONT::computer-processor
    :parent ONT::COMPUTER-PART
    :wordnet-sense-keys ("central_processing_unit%1:06:00" "microprocessor%1:06:00")
    )

(define-type ONT::internal-computer-storage
    :parent ONT::COMPUTER-PART
    :wordnet-sense-keys ("storage_device%1:06:00" "disk_space%1:15:00")
    )

(define-type ONT::data-storage-medium
    :parent ONT::INFO-HOLDER ;info-medium
    :comment "physical devices used to store data in a soft copy"
    )

(define-type ONT::io-device
    :parent ONT::COMPUTER-PART
    :wordnet-sense-keys ("magnetic_disk%1:06:00" "disk_drive%1:06:00" "drive%1:06:03")
    )

(define-type ONT::wireless
    :wordnet-sense-keys ("wireless_local_area_network%1:06:00" "WLAN%1:06:00" "wireless_fidelity%1:06:00" "WiFi%1:06:00")
    :parent ONT::COMPUTER-PART
    :arguments ((:OPTIONAL ONT::FIGURE (f::Phys-obj (f::origin f::artifact)))
		)
    )

;; pen, pencil, marker
(define-type ONT::writing-implement
    :parent ONT::manufactured-object
    :wordnet-sense-keys ("writing_implement%1:06:00")
    :sem (F::Phys-obj (F::mobility F::non-self-moving)(F::object-function F::instrument))
    )

(define-type ONT::member
    :parent ONT::part
    :wordnet-sense-keys ("member%1:18:00" "member%1:24:00" "member%1:14:00")
    :arguments ((:OPTIONAL ONT::FIGURE (f::Phys-obj (f::origin f::living)))
		)
    )

(define-type ONT::AUDIO
    :wordnet-sense-keys ("audio%1:10:00" "sound%1:10:00")
    :parent ONT::substance
    :sem (F::PHYS-OBJ (f::form f::wave) (F::INTENTIONAL -) (F::CONTAINER -))
    :arguments ((:OPTIONAL ONT::FIGURE (F::Phys-obj))
		)
    )

;; power
(define-type ONT::POWER
    :wordnet-sense-keys ("electricity%1:19:01" "electrical_energy%1:19:00" "electricity%1:19:00" "energy%1:19:00" "energy%1:19:01")
    :parent ONT::substance
    )


;; cash, money
(define-type ONT::money
    :wordnet-sense-keys ("change%1:21:03" "change%1:21:01")
    :parent ONT::manufactured-object
    :sem (F::phys-obj (f::object-function f::currency))
    )


(define-type ONT::CREDIT-CARD
    :parent ONT::manufactured-object
    :wordnet-sense-keys ("credit_card%1:21:00" "charge_card%1:21:00" "charge_plate%1:21:00" "plastic%1:21:00")
    :sem  (F::phys-obj (f::object-function f::currency))
    :arguments ((:OPTIONAL ONT::FIGURE (F::Phys-obj (f::origin f::human) (f::intentional +)))
		)
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; boudreaux-types.lisp
;; swift 20030918
;; LFs to support boudreaux vocabulary


;; boudreaux wears a suit
(define-type ONT::attire
    :parent ONT::manufactured-object
    :wordnet-sense-keys ("clothing%1:06:00" "article_of_clothing%1:06:00" "vesture%1:06:00" "wear%1:06:00" "wearable%1:06:00" "habiliment%1:06:00")
    :sem (f::Phys-obj (F::Origin F::Artifact))
    )

(define-type ONT::washing
    :parent ONT::attire
    :wordnet-sense-keys ("laundry%1:06:01")
    )

;; boudreaux takes samples of environmental materials, fossils, etc.
(define-type ONT::geo-SAMPLE
    :parent ONT::natural-object
    :wordnet-sense-keys ("core%1:17:01")
    :arguments ((:Optional ONT::FIGURE (f::Phys-obj (F::origin f::artifact)))
                )
    )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; swift 20050131
;; LFs to support fruitcarts vocabulary
;;

#|
;; the space between objects
(define-type ONT::shape
    :parent  ONT::Phys-object
    :wordnet-sense-keys ("shape%1:07:00" "form%1:07:01" "configuration%1:07:00" "contour%1:07:00" "conformation%1:07:00" "form%1:09:00" "shape%1:09:00" "pattern%1:09:00")
    :sem (f::Phys-obj (:required (f::intentional -) (f::information -))
		      (:default (f::mobility f::non-self-moving))
		      )
    :arguments ((:OPTIONAL ONT::FIGURE))
    )
|#

;; square, triangle, etc.  -- objects defined by their shape
(define-type ONT::shape-object
    ;:parent  ONT::shape
    :parent ONT::PHYS-OBJECT
    :wordnet-sense-keys ("shape%1:03:00")
    :sem (f::Phys-obj (:required  (F::origin f::artifact) (f::form f::object) (f::intentional -) (f::information -)
				  (f::object-function f::representation))
		      (:default (f::mobility f::non-self-moving))
		      ))

(define-type ONT::FLAG
    :parent ONT::manufactured-object
    :wordnet-sense-keys ("flag%1:06:00")
    :sem (f::Phys-obj (:required (f::form f::object) (f::intentional -) (f::information -) (f::object-function f::representation))
		      (:default (f::mobility f::non-self-moving))
		      ))


;; hole (in the ozone layer)
(define-type ont::opening
    :parent ONT::shape-object
    :wordnet-sense-keys ("opening%1:17:00" "gap%1:17:00")
    )

;; exit, entrance
(define-type ont::exit
    :parent ont::opening
    :wordnet-sense-keys ("exit%1:06:00" "issue%1:06:00" "outlet%1:06:00" "way_out%1:06:00")
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; myrosia 20040401
;; LFs to support BEE vocabulary
;;

;; These are all sorts of device parts (possibly computer components)
(define-type ONT::DEVICE-COMPONENT
    :parent ONT::manufactured-object
    :wordnet-sense-keys ("component%1:06:00" "constituent%1:06:00" "element%1:06:00")
    :sem (f::Phys-obj (:required (f::origin f::artifact) (f::form f::object))
		      (:default (f::intentional -) (f::container -) (f::mobility f::non-self-moving) (f::information -))
		      )
    :arguments ((:essential ONT::FIGURE))
    )

;; shades, curtains, blinds
(define-type ont::covering
    :parent ONT::device-component
    :wordnet-sense-keys ("covering%1:06:00" "cover%1:06:03")
    :sem (F::Phys-obj (F::Form F::solid-object) (F::Object-Function F::covering))
    )

#|(define-type ont::medical-dressing
    :wordnet-sense-keys ("medical_dressing%1:06:00")
    :parent ont::medical-instrument ;covering
    )|#

(define-type ont::support-stand
    :parent ONT::device-component
    :wordnet-sense-keys ("support%1:06:00") ;"pedestal%1:06:00" "stand%1:06:00" "support%1:06:00")
    :sem (F::Phys-obj (F::Form F::solid-object) (F::Object-Function F::support))
    )

#|(define-type ont::kettle-base
    :parent ONT::support-stand
    :wordnet-sense-keys ("heater%1:06:00")
    :sem (F::Phys-obj (F::Form F::solid-object))
    )|#


;; button, switch
(define-type ont::operating-switch
    :parent ONT::device-component
    :sem (F::Phys-obj (F::Form F::solid-object))
    :wordnet-sense-keys ("control%1:06:00")
    )

;; specific types added for CAET

(define-type ont::button
    :parent ont::operating-switch
    :wordnet-sense-keys ("push_button%1:06:00")
    )

(define-type ont::switch
    :parent ont::operating-switch
    :wordnet-sense-keys ("switch%1:06:01")
    )

(define-type ont::base
    :wordnet-sense-keys ("base%1:06:00")
    :parent ont::support-stand ;kettle-base
    )

(define-type ont::heater
    :wordnet-sense-keys ("heater%1:06:00")
    :parent ont::appliance ;kettle-base
    )

#|(define-type ont::element
    :parent ont::kettle-base
    )|#

(define-type ont::heating-element ;burner
    :wordnet-sense-keys ("heating_element%1:06:00")
    :parent ont::device-component ;kettle-base
    )

#|(define-type ont::stand
    :parent ont::kettle-base
    )|#

(define-type ont::lid
    :parent ont::covering
    :wordnet-sense-keys ("lid%1:06:00")
    )
;; Added during a portability experiment
#|
(define-type ONT::phys-shape
    :parent  ont::shape ;ONT::Phys-object
    :sem (f::Phys-obj (:required (f::form f::object) (f::intentional -) (f::information -))
		      (:default (f::mobility f::non-self-moving))
		      ))
|#

(define-type ONT::container
    :wordnet-sense-keys ("container%1:06:00")
    :parent ONT::MANUFACTURED-OBJECT
    :sem (F::Phys-obj (F::container +) (F::form F::solid-object) (F::origin F::artifact) (f::object-function f::container-object))
    :arguments ((:OPTIONAL ONT::CONTENTS)
		)
    )

;; added for asma
(define-type ONT::dispenser
    :wordnet-sense-keys ("dispenser%1:06:00")
    :parent ONT::container
    )


;; added for asma
(define-type ONT::inhaler
    :wordnet-sense-keys ("inhaler%1:06:00")
    :parent ONT::dispenser
    )


;; box, bag
(define-type ONT::small-container
    :parent ONT::container
    :wordnet-sense-keys ("basket%1:06:00" "backpack%1:06:00" "bottle%1:06:00" "can%1:06:00" "jar%1:06:00" "receptacle%1:06:00")
    )

(define-type ont::package
    :parent ont::small-container
    :wordnet-sense-keys ("package%1:06:00")
    )

;; pot, pan
(define-type ONT::cookware
    :wordnet-sense-keys ("cookware%1:06:00")
    :parent ONT::manufactured-object
    :sem (F::Phys-obj (F::container +) (F::form F::solid-object) (F::origin F::artifact)(f::trajectory -)(f::mobility f::non-self-moving))
    :arguments ((:OPTIONAL ONT::CONTENTS)
		)
    )

;; plate, bowl, cup
(define-type ONT::tableware
    :wordnet-sense-keys ("tableware%1:06:00")
    :parent ONT::manufactured-object
    :sem (F::Phys-obj (F::form F::solid-object) (F::origin F::artifact)(f::trajectory -))
    :arguments ((:OPTIONAL ONT::CONTENTS)
		)
    )

;; knife, fork, spoon
(define-type ONT::cutlery
    :wordnet-sense-keys ("cutlery%1:06:00")
    :parent ONT::tableware
    :sem (F::Phys-obj (F::form F::solid-object) (F::origin F::artifact)(f::trajectory -) (f::object-function f::instrument))
    )


;; specific types defined for CAET system
(define-type ont::spoon
    :parent ont::cutlery
    :wordnet-sense-keys ("spoon%1:06:00")
    )

(define-type ont::cup
    :parent ont::tableware
    :sem (F::Phys-obj (F::container +))
    :wordnet-sense-keys ("cup%1:06:00")
    )

(define-type ont::mug
    :parent ont::tableware
    :sem (F::Phys-obj (F::container +))
    :wordnet-sense-keys ("mug%1:06:00")
    )

(define-type ont::glass
    :parent ont::tableware
    :sem (F::Phys-obj (F::container +))
    :wordnet-sense-keys ("glass%1:06:00")
    )

(define-type ont::box
    :parent ont::small-container
    :wordnet-sense-keys ("box%1:06:00")
    )

(define-type ont::bag
    :parent ont::small-container
    :wordnet-sense-keys ("bag%1:06:00")
    )

(define-type ont::kettle
    :parent ont::cookware
    :wordnet-sense-keys ("kettle%1:06:01")
    )

(define-type ont::pot
    :parent ont::cookware
    :wordnet-sense-keys ("pot%1:06:00")
    )

(define-type ont::sink
    :parent ont::fixture
    :wordnet-sense-keys ("sink%1:06:00")
    )

(define-type ont::storage-furnishings
    :parent ont::furnishings
    :wordnet-sense-keys ("wardrobe%1:06:00")
    :sem (F::Phys-obj (F::origin F::Artifact)(F::trajectory -)
		      (F::mobility f::fixed) (f::container +))
    )

(define-type ont::cabinet
    :parent ont::storage-furnishings
    :wordnet-sense-keys ("cabinet%1:06:00" "cabinet%1:06:02")
    )

(define-type ont::cupboard
    :parent ont::storage-furnishings
    :wordnet-sense-keys ("cupboard%1:06:00")
    )

(define-type ont::table
    :parent ont::furnishings
    :wordnet-sense-keys ("table%1:06:01")
    )

(define-type ont::drawer
    :parent ont::furnishings-component
    :wordnet-sense-keys ("drawer%1:06:00")
    )

(define-type ont::refrigerator
    :parent ont::appliance
    :wordnet-sense-keys ("refrigerator%1:06:00" "icebox%1:06:00")
    )

(define-type ont::microwave
    :parent ont::appliance
    :wordnet-sense-keys ("microwave%1:06:00" "microwave_oven%1:06:00")
    )

;; hack for medadvisor
;(define-type ONT::system
; :parent ONT::person
; :sem (F::phys-obj)
; )

(define-type ONT::hotspot
    :parent ONT::location-by-description
;there are three relatively different senses for hotspot. the sense below is political unrest etc
    :wordnet-sense-keys ("hot_spot%1:15:01")
    )

(define-type ONT::prescription
    :parent ONT::DOCUMENT
    :wordnet-sense-keys ("prescription%1:10:02" "prescription%1:10:01")
    :sem (F::PHYS-OBJ (F::FORM F::SOLID-OBJECT) (F::ORIGIN F::ARTIFACT) (F::INTENTIONAL -))
    :arguments ((:OPTIONAL ONT::FIGURE (F::Phys-obj))
		)
    )

;; parent type for vitamins/minerals
(define-type ONT::nutritional-supplement
    :parent ONT::food
    :wordnet-sense-keys ("dietary_supplement%1:13:00")
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; > Types added for FOOD KB

(define-type ONT::FOOD-GRAINS
    :parent ONT::FOOD
    :wordnet-sense-keys ("grain%1:13:00")
    :sem (f::phys-obj (f::origin f::natural))
    :comment "grains and grain products for food"
    )

(define-type ONT::DAIRY
    :parent ONT::FOOD
    :wordnet-sense-keys ("dairy_product%1:13:00")
    )

(define-type ONT::BEVERAGES
    :parent ONT::FOOD
    :wordnet-sense-keys ("beverage%1:13:00" "drink%1:13:00" "drinkable%1:13:00" "potable%1:13:00"
					    "milkshake%1:13:00")
    :sem (f::phys-obj (F::form F::liquid) (f::origin f::natural))
    )

(define-type ONT::alcohol
    :parent ONT::beverages
    :wordnet-sense-keys ("alcohol%1:13:00" "alcoholic_drink%1:13:00" "alcoholic_beverage%1:13:00")
    )

(define-type ONT::PREPARED-FOOD ;PREPARED
    :parent ONT::FOOD
    )

(define-type ONT::INGREDIENTS
    :parent ONT::FOOD
    :wordnet-sense-keys ("ingredient%1:13:00" "fixings%1:13:00")
    )

;; specific type for CAET

(define-type ont::sugar
    :wordnet-sense-keys ("sugar%1:13:00")
    :parent ont::ingredients
    )

(define-type ONT::VITAMINS-MINERALS
   
    :parent ONT::nutritional-supplement
    )

(define-type ONT::MEAT
    :wordnet-sense-keys ("meat%1:13:01")
    :parent ONT::FOOD
    )

(define-type ONT::FATS-OILS
    :wordnet-sense-keys ("vegetable_oil%1:13:00")
    :parent ONT::INGREDIENTS
    )

(define-type ONT::FRUIT
    :wordnet-sense-keys ("edible_fruit%1:13:00")
    :parent ONT::PRODUCE
    )

(define-type ONT::VEGETABLE
    :wordnet-sense-keys ("vegetable%1:13:00" "veggie%1:13:00" "veg%1:13:00")
    :parent ONT::PRODUCE
    )

(define-type ONT::VITAMINS
    :wordnet-sense-keys ("vitamin%1:27:00")
    :parent ONT::VITAMINS-MINERALS
    )

(define-type ONT::MINERALS
    :parent ONT::CHEMICAL ; VITAMINS-MINERALS
    )

(define-type ONT::BEEF
    :wordnet-sense-keys ("beef%1:13:00")
    :parent ONT::MEAT
    )

(define-type ONT::PORK
    :wordnet-sense-keys ("pork%1:13:00")
    :parent ONT::MEAT
    )

(define-type ONT::lamb
    :wordnet-sense-keys ("lamb%1:13:00" "mutton%1:13:00")
    :parent ONT::MEAT
    )

(define-type ONT::MEAT-PRODUCT ;MEAT-OTHER
    :parent ONT::MEAT
    :wordnet-sense-keys ("bacon%1:13:00" "sausage%1:13:00" "pastrami%1:13:00")
    )

(define-type ONT::WILD-GAME
    :wordnet-sense-keys ("game%1:05:00")
    :parent ONT::MEAT
    )

(define-type ONT::EMU
    :parent ONT::MEAT
    )

(define-type ONT::POULTRY
    :wordnet-sense-keys ("bird%1:13:00")
    :parent ONT::MEAT
    )

(define-type ONT::CHICKEN
    :parent ONT::POULTRY
    )

(define-type ONT::GOOSE
    :wordnet-sense-keys ("goose%1:05:00")
    :parent ONT::POULTRY
    )

(define-type ONT::PHEASANT
    :wordnet-sense-keys ("pheasant%1:05:00")
    :parent ONT::POULTRY
    )

(define-type ONT::QUAIL
    :parent ONT::POULTRY
    )

(define-type ONT::PIGEON
     :wordnet-sense-keys ("pigeon%1:05:00")
    :parent ONT::POULTRY
    )

(define-type ONT::DUCK
     :wordnet-sense-keys ("duck%1:13:00")
    :parent ONT::POULTRY
    )

(define-type ONT::OSTRICH
      :wordnet-sense-keys ("ostrich%1:05:00")
    :parent ONT::POULTRY
    )

(define-type ONT::TURKEY
     :wordnet-sense-keys ("turkey%1:05:00")
    :parent ONT::POULTRY
    )

(define-type ONT::SEAFOOD
    :wordnet-sense-keys ("seafood%1:13:00")
    :parent ONT::FOOD
    )

(define-type ONT::SALTWATER-FISH
    :wordnet-sense-keys ("saltwater_fish%1:13:00")
    :parent ONT::SEAFOOD
    )

(define-type ONT::FRESHWATER-FISH
     :wordnet-sense-keys ("freshwater_fish%1:13:00")
    :parent ONT::SEAFOOD
    )

(define-type ONT::MOLLUSKS
    :parent ONT::SEAFOOD
    )

(define-type ONT::CRUSTACEANS
    :parent ONT::SEAFOOD
    )

(define-type ONT::BAKED-GOODS
     :wordnet-sense-keys ("baked_goods%1:13:00")
    :parent ONT::PREPARED-FOOD ;PREPARED
    )

(define-type ONT::PASTA
    :wordnet-sense-keys ("pasta%1:13:00")
    :parent ONT::FOOD
    )

(define-type ONT::BREAD
     :wordnet-sense-keys ("bread%1:13:00")
    :parent ONT::BAKED-GOODS
    )

(define-type ONT::CRACKERS
     :wordnet-sense-keys ("cracker%1:13:00")
    :parent ONT::BAKED-GOODS
    )

(define-type ONT::COOKIES
     :wordnet-sense-keys ("cookie%1:13:00")
    :parent ONT::BAKED-GOODS
    )

(define-type ONT::CAKE-PIE
     :wordnet-sense-keys ("cake%1:13:00" "pie%1:13:00")
    :parent ONT::BAKED-GOODS
    )

(define-type ONT::BAGELS-BISCUITS
     :wordnet-sense-keys ("bun%1:13:00" )
    :parent ONT::BAKED-GOODS
    )

(define-type ONT::SWEETS
    :wordnet-sense-keys ("sweet%1:13:00" )
    :parent ONT::PREPARED-FOOD ;PREPARED
    )

(define-type ONT::MEALS
    :parent ONT::PREPARED-FOOD ;PREPARED
    )

(define-type ONT::SOUP
    :wordnet-sense-keys ("soup%1:13:00")
    :parent ONT::MEALS
    :sem (f::phys-obj (F::form F::liquid))
    )

(define-type ONT::CEREALS
    :wordnet-sense-keys ("cereal%1:13:00" )
    :parent ONT::PREPARED-FOOD ;PREPARED
    )

(define-type ONT::FAST-FOOD
    :wordnet-sense-keys ("fast_food%1:13:00")
    :parent ONT::PREPARED-FOOD ;PREPARED
    )

(define-type ONT::SPICES-HERBS
    :wordnet-sense-keys ("spice%1:27:00" "spice%1:13:00" "herb%1:13:00")
    :parent ONT::INGREDIENTS
    )

(define-type ONT::BEANS-PEAS
    :wordnet-sense-keys ("legume%1:20:02")
    :parent ONT::vegetable
    )

(define-type ONT::DRESSINGS-SAUCES-COATINGS
    :wordnet-sense-keys ("sauce%1:13:00")
    :parent ONT::INGREDIENTS
    )

(define-type ONT::PRESERVATIVES
    :parent ONT::INGREDIENTS
    )

(define-type ONT::NUTS-SEEDS
    :wordnet-sense-keys ("seed%1:20:00")
    :parent ONT::INGREDIENTS
    )

(define-type ONT::CONDIMENTS
    :wordnet-sense-keys ("condiment%1:13:00")
    :parent ONT::INGREDIENTS
    )

(define-type ONT::JUICE
    :parent ONT::BEVERAGES
    )

(define-type ONT::SODA
    :wordnet-sense-keys ("soda%1:13:00")
    :parent ONT::BEVERAGES
    )

(define-type ONT::TEAS-COCKTAILS-BLENDS
    :parent ONT::BEVERAGES
    )

;; > specific types for CAET

(define-type ont::tea
    :parent ont::teas-cocktails-blends
    )

(define-type ont::coffee
    :parent ont::teas-cocktails-blends
    )

(define-type ONT::YOGURT
    :parent ONT::DAIRY
    )

(define-type ONT::MILK
    :parent ONT::DAIRY
    )

(define-type ONT::CHEESE
    :parent ONT::DAIRY
    )

(define-type ONT::BUTTER
    :parent ONT::DAIRY
    )
; <
; <


;;;;;;;;;;;;;;;
; the group-object hierarchy is a duplicate of group-object-abstr in abstract-object 
;;;;;;;;;;;;;;;

(define-type ont::group-object
    :wordnet-sense-keys ( "grouping%1:03:00")
    :comment "a collection of objects considered as a unit. So there are many things but grammatically is a singular noun"
					;:parent ont::abstract-object-nontemporal
    :parent ont::phys-object
					;  :sem (F::Abstr-obj (f::group +)) ; group feature not defined for abstract objects
    :sem (F::phys-obj (f::container +)) 
    :arguments ((:OPTIONAL ONT::FIGURE)
		(:optional ont::contents)
		)
    )

(define-type ONT::collection
    :comment "This type contains concepts that explicitly denotethe group (rather than the members of the grop), e.g., group, herd, collection,..."
    :wordnet-sense-keys ("collection%1:14:00" "array%1:10:00" "series%1:14:01" "trinketry%1:14:00" "population%1:14:01" "batch%1:23:00")
    :parent ONT::group-object
    )

(define-type ONT::arrangement-configuration
    :wordnet-sense-keys ("arrangement%1:14:00" "array%1:14:00" "straggle%1:14:00" "configuration%1:09:00")
    :comment "An group of objects organized in some way"
    :parent ONT::collection
    )

(define-type ONT::data
    :wordnet-sense-keys ("data%1:14:00")
    :comment "A group of information organized in some way"
    :parent ONT::collection
    )

(define-type ONT::implicit-group
    :comment "This type contains concepts that denote a group via a common characteristic of the elements, e.g., kingdom, "
    :parent ONT::group-object)

(define-type ONT::mail
    :comment "A set of letters"
    :wordnet-sense-keys ("mail%1:10:01" )
    :parent ONT::implicit-group)

;; email, spam
(define-type ont::email
    :wordnet-sense-keys ("electronic_mail%1:10:00" "e-mail%1:10:00" "email%1:10:00")
    :parent ONT::mail
    )

(define-type ont::natural-group
    :wordnet-sense-keys ("kingdom%1:14:00" "biological_group%1:14:00" "association%1:14:01" "class%1:14:00")
    :comment "An group defined by a classification of living things"
    :sem (F::PHYS-OBJ (F::FORM F::SOLID-OBJECT) (F::origin f::living))
    :parent ONT::implicit-group
    )

(define-type ont::animal-group
    :wordnet-sense-keys ("animal_group%1:14:00" "bacteria_order%1:05:00" "menagerie%1:14:00")
    :comment "An group defined by a classification of animals"
    :sem (F::PHYS-OBJ (F::INTENTIONAL +))
    :parent ONT::natural-group)
    
(define-type ont::plant-group
    :wordnet-sense-keys ("vegetation%1:14:00")
    :comment "An group defined by a classification of animals"
    :sem (F::PHYS-OBJ (F::INTENTIONAL +))
    :parent ONT::natural-group
    )



(define-type ont::people-group 
    :wordnet-sense-keys ("population%1:14:00" )
    :comment "a group of persons"
    :parent ont::animal-group
    )

(define-type ont::people-subgroup
    :wordnet-sense-keys ("race%1:14:00" "sainthood%1:14:00" "varna%1:14:00" "tenantry%1:14:00" "world%1:05:00")
    :comment "a group of persons defined by origin or race"
    :parent ont::implicit-group
    )

(define-type ONT::system
  :wordnet-sense-keys ("system%1:14:00" "system%1:14:00" "system%1:06:00")
  :comment "An interconnected group of objects, abstract or physical"
 :parent ONT::collection
 )

(define-type ONT::ecosystem
  :wordnet-sense-keys ("biotic_community%1:14:00" "ecosystem%1:14:00" "biosphere%1:15:00" "biota%1:14:00")
  :comment "An interconnected group of entities forming an ecosystem"
  :parent ONT::system
 )

(define-type ONT::structure
  :wordnet-sense-keys ("structure%1:07:00")
  :comment "A collection of objects organized for some purpose" 
  :parent ONT::system
  )

(define-type ONT::economic-system
  :wordnet-sense-keys ("economy%1:14:00" )
  :comment "An interconnected group of entities forming an economy"
 :parent ONT::system
 )

(define-type ONT::formation
 :parent ONT::group-object
 )

(define-type ONT::row-formation
 :wordnet-sense-keys ("row%1:14:00" "row%1:14:01" "row%1:17:00")
 :parent ONT::formation
 :arguments ((:OPTIONAL ONT::FIGURE (F::phys-obj))  ; to distinguish between steps as steps in a plan and steps in a staircase
             )
 )

(define-type ONT::column-formation
 :wordnet-sense-keys ("pile%1:14:00" "column%1:14:00"  "column%1:14:01" "column%1:25:02")
 :parent ONT::formation
 :arguments ((:OPTIONAL ONT::FIGURE (F::phys-obj))  ; to distinguish between steps as steps in a plan and steps in a staircase
             )
 )

;; crowd, audience
(define-type ont::social-group
 :wordnet-sense-keys ("social_group%1:14:00"  "congregation%1:14:01" "defense%1:14:00" "unit%1:14:00")
  :parent ont::implicit-group
  ;:sem (F::Abstr-obj (F::information F::information-content) (f::intentional +) (F::Object-Function F::Occupation) (F::Container -))
  :sem (F::phys-obj (f::intentional +) (F::Object-Function F::Occupation)) ; (F::Container -)) GROUP-OBJECT has container +
  :arguments ((:OPTIONAL ONT::FIGURE ((? lof f::phys-obj f::abstr-obj))))
  )

(define-type ont::family-group
  :wordnet-sense-keys ("family%1:14:02")
    :parent ont::social-group
    )

(define-type ONT::organization
 :wordnet-sense-keys ("organization%1:14:00" )
 :parent ONT::social-group
 )

;; these subtypes came about because of generation issues
;; commerce, finance, business, marketing
(define-type ONT::enterprise
 :parent ONT::organization
 )

;; institution
(define-type ONT::institution
 :parent ONT::organization
 )

;; an institution created for conduction business
;; company
(define-type ONT::company
 :parent ONT::institution
 )

;; google, amazon, isp
(define-type ONT::internet-organization
 :parent ONT::organization
 )

;; bank
(define-type ONT::financial-institution
 :parent ONT::institution
 )

;; apple, ibm, hp
(define-type ONT::electronics-company
 :parent ONT::company
 )

;; officemax, officedepot
(define-type ONT::office-supply-company
 :parent ONT::company
 )

;; fetch, gnu
(define-type ONT::software-company
 :parent ONT::company
 )

;; court
(define-type ONT::legal-organization
    :wordnet-sense-keys ("court%1:14:00")
    :parent ONT::organization
    )

;; market
(define-type ONT::financial-organization
 :parent ONT::organization
 )

;; government, gsa, darpa
(define-type ONT::federal-organization
 :wordnet-sense-keys ("government%1:14:00" "authorities%1:14:00" "regime%1:14:00"  "federal_department%1:14:00" "legislature%1:14:00")
 :parent ONT::organization
 )

;; ieee
(define-type ONT::professional-organization
 :parent ONT::organization
 )

;; ansi
(define-type ONT::regulatory-organization
 :parent ONT::organization
 :wordnet-sense-keys ("organization%1:14:01")
 )

(define-type ONT::airline
 :parent ONT::enterprise
 )

;; affiliate, partner, subsidiary
(define-type ONT::affiliate
 :parent ONT::company
 )

;; affiliate, partner, subsidiary
(define-type ONT::supplier
 :parent ONT::company
 )

;; sri
(define-type ONT::research-institution
 :parent ONT::company
 )

;; university, college
(define-type ONT::academic-institution
    :parent ONT::research-institution
 )

;; fedex, ups
(define-type ONT::shipping-company
 :parent ONT::company
 )

(define-type ONT::military-group
 :wordnet-sense-keys ("military_unit%1:14:00" "military_force%1:14:00" "military_group%1:14:00" "force%1:14:01")
 :parent ONT::social-group
 )


(define-type ONT::sequence
 :wordnet-sense-keys ("ordering%1:14:00" "order%1:14:00" "ordination%1:14:00")
 :parent ONT::group-object
 )

(define-type ONT::linear-grouping
 :wordnet-sense-keys ("line%1:14:01")
 :parent ONT::sequence
 )

(define-type ONT::combination
 :wordnet-sense-keys ("combination%1:14:00")
 :parent ONT::group-object
 )

;; layer (of ozone, chocolate), sheet (of ice, paper), slice
(define-type ont::sheet
;  :parent ont::non-measure-ordered-domain
  :parent ONT::SHAPE-OBJECT   ;; why was this in GROUP? JFA 4/19
  )

;; a number/amount/quantity of X
(define-type ONT::QUANTITY
 ;;:wordnet-sense-keys ("measure%1:03:00" "quantity%1:03:00" "amount%1:03:00" "quantity%1:09:01" "couple%1:23:01")
 ;:parent ONT::ORDERED-DOMAIN
 :parent ONT::GROUP-OBJECT
 :arguments ((:ESSENTIAL ONT::FIGURE ((? val f::phys-obj f::abstr-obj f::situation )))
             )
 )

;; surplus, excess
(define-type ONT::surplus
 :parent ONT::quantity
 :wordnet-sense-keys ("surplus%1:07:00")
 )

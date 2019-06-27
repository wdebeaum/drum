(in-package :om)

; > starts a block                                                                                                  
; < ends the block                                                                                                  
; use `[code]` to use lisp syntax in code (optional, but makes eliminating commented code easier)                   


;;; ===========ONT::DOMAIN
;;; A domain is a single-valued function                                                                            
(define-type ONT::DOMAIN
 :parent ONT::TANGIBLE-ABSTRACT-object
 :wordnet-sense-keys ("attribute%1:03:00")
 :comment "Nouns that name domain/scales, and can serve as relational nouns (e.g., the COLOR of the box)"
 :arguments ((:REQUIRED ONT::FIGURE)
             (:optional ont::GROUND)
             (:optional ont::EXTENT)
             )
 )


;; ORDERED DOMAIN

(define-type ont::ordered-domain
 :parent ont::domain 
; :sem (F::Abstr-obj (F::Scale ?!sc))
)

;; ATTRIBUTIVE SCALE
;(define-type ont::attributive-scale
; :parent ont::ordered-domain 
; :wordnet-sense-keys ("quality%1:07:00")
; :comment "scales dealing with quality or property of something or someone(e.g. smoothness or kindness) and are (relatively) permanent in nature. These are distinguished from ont::stage-scale types that describes scales dealing with temporary stages or states in which an entity is found (a state of being; e.g. healthiness or sleepiness)."
; ;; WORDS: quality
;)


;; PHYSICAL-PROPERTY-SCALE
(define-type ont::physical-property-scale
 :parent ont::ordered-domain
 :comment "scales associated with properties pertaining to the attributes of physical entities or substances. Note: many properties can apply to non-physical objects."
)

;; BODY CONDITION SCALE
(define-type ont::body-condition-scale
 :parent ont::physical-property-scale 
; :sem (f::situation (f::aspect f::indiv-level)) ;; prevent attachment of temporal adv                               
; :arguments ((:OPTIONAL ONT::FIGURE (F::phys-obj (F::origin F::natural)))
;             )
)

(define-type ont::positive-body-condition-scale
 :parent ont::body-condition-scale
)

(define-type ont::health-scale
 :parent ont::positive-body-condition-scale 
 :wordnet-sense-keys ("wellness%1:26:00" "wellbeing%1:26:00" "health%1:26:00" "healthiness%1:26:00" "condition%1:26:02")
 ;; WORDS: wellness, healthiness, condition (in condition), health, wellbeaing
)

(define-type ont::fitness-scale
 :parent ont::positive-body-condition-scale
 :wordnet-sense-keys ("fitness%1:26:00")
)

(define-type ont::negative-body-condition-scale
 :parent ont::body-condition-scale
)

(define-type ont::pain-scale
 :wordnet-sense-keys ("painfulness%1:07:00" "soreness%1:12:00" "pain%1:12:00" "soreness%1:12:00")
 :parent ONT::negative-body-condition-scale
)

(define-type ont::not-well-nourished-scale
 :parent ont::negative-body-condition-scale
 :wordnet-sense-keys ("hunger%1:26:00" "thirst%1:26:00")
)

(define-type ont::illness-scale
 :parent ont::negative-body-condition-scale
 :wordnet-sense-keys ("unhealthiness%1:26:00" "illness%1:26:00")
)

;; restlessness
(define-type ONT::restlessness-scale
 :wordnet-sense-keys ("restlessness%1:07:01")
 :parent ont::negative-body-condition-scale
)

;; feebleness                                                                                                       
(define-type ONT::feebleness-scale
 :wordnet-sense-keys ("feebleness%1:26:00")
 :parent ont::negative-body-condition-scale
 )

;; fatigue, tiredness, exhaustion                                                                                   
(define-type ONT::fatigue-scale
 :wordnet-sense-keys ("exhaustion%1:26:00" "fatigue%1:26:00" "tiredness%1:26:00")
 :parent ont::feebleness-scale
 )

;; weakness
(define-type ONT::weakness-scale
 :wordnet-sense-keys ("weakness%1:07:00")
 :parent ont::feebleness-scale
 )


(define-type ont::neutral-body-condition-scale
 :parent ont::body-condition-scale
)

(define-type ont::sleepiness-scale
 :parent ont::neutral-body-condition-scale 
  :wordnet-sense-keys ("sleepiness%1:26:00" "drowsiness%1:26:00")
 ;; WORDS: drowsiness, sleepiness
)

;; polarity-scale
(define-type ont::polarity-scale
 :parent ont::physical-property-scale
 :wordnet-sense-keys ("polarity%1:24:01")
 ;; WORDS: polarity
)

;; temperature scale
(define-type ont::temperature-scale
 :wordnet-sense-keys ("temperature%1:07:00" "temperature%1:09:00")
 :parent ont::physical-property-scale 
 :sem (F::abstr-obj (F::Scale Ont::temperature-scale))
 ;; WORDS: temperature
)

(define-type ont::heat-scale
 :wordnet-sense-keys ("heat%1:07:01" "heat%1:09:00")
 :parent ont::temperature-scale
 ;; WORDS: heat
)

(define-type ont::cold-scale
 :wordnet-sense-keys ("cold%1:07:00" "cold%1:09:00")
 :parent ont::temperature-scale
 ;; Words: cold
)


;; atmospheric scale
(define-type ont::atmospheric-scale
 :parent ont::physical-property-scale
)

;; humidity scale
(define-type ont::humidity-scale
 :parent ont::atmospheric-scale 
 :wordnet-sense-keys ("humidity%1:26:00")
 :arguments ((:ESSENTIAL ONT::GROUND (F::Abstr-obj (F::Scale Ont::humidity-scale )))) 
 :sem (F::Abstr-obj )
 ;; WORDS: humidity
)

;; configuration property scale
(define-type ont::configuration-property-scale
 :parent ont::physical-property-scale
 :comment "scales for properties regarding the configuration, arrangement or layout of elements"
)

(define-type ont::constriction-scale
 :parent ont::configuration-property-scale
)

(define-type ont::tightness-scale
 :parent ont::constriction-scale
 :wordnet-sense-keys("tightness%1:07:00")
)

(define-type ont::looseness-scale
 :parent ont::constriction-scale
 :wordnet-sense-keys("looseness%1:07:00")
)

;;  appearance scale
(define-type ont::appearance-scale
 :parent ont::physical-property-scale 
 :comment "scales related to surface appearance of a physical entity or object preceptible through sensory input"
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj))
             )
)

(define-type ont::smell-scale
 :parent ont::appearance-scale 
 :wordnet-sense-keys ("smell%1:07:00")
 ;; WORDS: smell
)

(define-type ont::negative-smell-scale
 :parent ont::smell-scale 
 :wordnet-sense-keys ("stinkiness%1:07:00" "rancidness%1:07:00")
 ;; WORDS: malodorousness, rancidness, muskiness
 
)

(define-type ont::positive-smell-scale
 :parent ont::smell-scale 
 :wordnet-sense-keys ("sweetness%1:07:01")
 ;; WORDS: sweetness, fragrance
 ;; specifically referring to the pleasing olfactory property as perceived by the senses 
 ;; and NOT the gragrance/perfume possessed by the item that is being smelled (i.e., fragrance%1:09:00).
)

(define-type ont::sound-scale
 :parent ont::appearance-scale 
)

(define-type ont::sound-volume-scale
 :parent ont::sound-scale 
)

(define-type ont::loudness-scale
 :parent ont::sound-volume-scale
 :wordnet-sense-keys ("loudness%1:07:00")
 ;; WORDS: loudness
)

(define-type ont::sound-softness-scale
 :parent ont::sound-volume-scale
 :wordnet-sense-keys ("softness%1:07:01")
 ;; WORDS: softness
 :comment "quality of being low in volume"
)

(define-type ont::quietness-scale
 :parent ont::sound-volume-scale
 :wordnet-sense-keys ("quietness%1:07:00")
 ;; WORDS: quietness
 :comment "quality of lacking sound"
)

(define-type ont::sound-texture-scale
 :parent ont::sound-scale
)

(define-type ont::hoarseness-scale
 :parent ont::sound-texture-scale
 :wordnet-sense-keys ("hoarseness%1:07:00")
)

(define-type ont::touch-scale
 :parent ont::appearance-scale 
)

(define-type ont::flexibility-scale
 :parent ont::touch-scale 
 :wordnet-sense-keys ("flexibility%1:07:02")
 ;; WORDS: flexibility
)

(define-type ont::texture-scale
 :parent ont::touch-scale 
 :wordnet-sense-keys ("texture%1:07:00")
 ;; WORDS: texture
)

(define-type ont::tactile-hardness-scale
 :parent ont::texture-scale 
 :wordnet-sense-keys ("hardness%1:07:01")
 ;; WORDS: hardness
)

(define-type ont::tactile-softness-scale
 :parent ont::texture-scale 
 :wordnet-sense-keys ("softness%1:07:00")
 ;; WORDS: softness
)

(define-type ont::texture-thickness-scale
 :parent ont::texture-scale
 :wordnet-sense-keys ("thickness%1:07:02")
)

(define-type ont::texture-thinness-scale
 :parent ont::texture-scale
 :wordnet-sense-keys ("thinness%1:07:02")
)

(define-type ont::roughness-scale
 :parent ont::texture-scale
 :wordnet-sense-keys ("roughness%1:07:00") 
 ;; WORDS: choppiness, roughness
)

(define-type ont::sharp-texture-scale
 :parent ont::texture-scale 
 :wordnet-sense-keys ("sharpness%1:07:01")
 ;; WORDS: sharpness
)

(define-type ont::smoothness-scale
 :parent ont::texture-scale 
 :wordnet-sense-keys ("smoothness%1:07:00")
 ;; WORDS: smoothness
)

(define-type ont::taste-scale
 :parent ont::appearance-scale 
)

(define-type ont::spiciness-scale
 :parent ont::taste-scale 
 :wordnet-sense-keys ("spiciness%1:07:00")
 ;; WORDS: spiciness
)

(define-type ont::sourness-scale
 :parent ont::taste-scale 
 :wordnet-sense-keys ("sourness%1:07:00")
 ;; WORDS: sourness
)

(define-type ont::sweetness-scale
 :parent ont::taste-scale 
 :wordnet-sense-keys ("sweetness%1:07:00" "sweetness%1:09:00")
 ;; WORDS: sweetness
)

(define-type ont::bitterness-scale
 :parent ont::taste-scale 
 :wordnet-sense-keys ("bitterness%1:07:00")
 ;; WORDS: bitterness
)

(define-type ont::visual-scale
 :parent ont::appearance-scale 
)


(define-type ont::luminosity-scale
 :parent ont::visual-scale 
 :wordnet-sense-keys ("brightness%1:07:00" "brightness%1:07:02")
; :arguments ((:ESSENTIAL ONT::GROUND (F::Abstr-obj (F::Scale Ont::luminosity-scale)))
;             )
 ;; WORDS: brightness
)

(define-type ont::brightness-scale
 :parent ont::visual-scale 
 :wordnet-sense-keys ("brightness%1:07:00" "brightness%1:07:02")
 ;; WORDS: brightness
)

(define-type ont::visual-dullness-scale
 :parent ont::visual-scale 
 :wordnet-sense-keys ("dimness%1:07:01" "softness%1:07:06")
 ;; WORDS: brightness
)

(define-type ont::color-scale
 :parent ont::visual-scale 
 :arguments ((:REQUIRED ONT::GROUND (F::abstr-obj (F::scale ont::color-scale )));?? what 's this? the car' s color of red? ) 
            )
 :sem (F::abstr-obj (F::scale ont::color-scale ))
 ;; WORDS: color, colour
 :wordnet-sense-keys ("colouring%1:07:00" "coloring%1:07:00" "colour%1:07:00" "color%1:07:00" "color%1:09:01" "colour%1:09:01")
)

(define-type ont::red-scale
 :parent ont::color-scale
 :wordnet-sense-keys ("redness%1:07:00")
 ;; WORDS: redness
)

(define-type ont::blue-scale
 :parent ont::color-scale
 :wordnet-sense-keys ("blue%1:07:00")
)

(define-type ont::green-scale
 :parent ont::color-scale
 :wordnet-sense-keys ("green%1:07:00")
)

(define-type ont::grey-scale
 :parent ont::color-scale
 :wordnet-sense-keys ("greyness%1:07:00")
)

(define-type ont::pink-scale
 :parent ont::color-scale
 :wordnet-sense-keys ("pink%1:07:00")
)

(define-type ont::magenta-scale
 :parent ont::color-scale
 :wordnet-sense-keys ("magenta%1:07:00")
)

(define-type ont::silver-scale
 :parent ont::color-scale
 :wordnet-sense-keys ("silver%1:07:00")
)

(define-type ont::gold-scale
 :parent ont::color-scale
 :wordnet-sense-keys ("gold%1:07:00")
)

(define-type ont::white-scale
 :parent ont::color-scale
 :wordnet-sense-keys ("white%1:07:00")
)

(define-type ont::brown-scale
 :parent ont::color-scale
 :wordnet-sense-keys ("brownness%1:07:00")
)

(define-type ont::black-scale
 :parent ont::color-scale
 :wordnet-sense-keys ("blackness%1:07:00")
)

(define-type ont::purple-scale
 :parent ont::color-scale
 :wordnet-sense-keys ("purple%1:07:00")
)

(define-type ont::orange-scale
 :parent ont::color-scale
 :wordnet-sense-keys ("orange%1:07:00")
)

(define-type ont::yellow-scale
 :parent ont::color-scale
 :wordnet-sense-keys ("yellow%1:07:00")
)

(define-type ont::tan-scale
 :parent ont::color-scale
 :wordnet-sense-keys ("tan%1:07:00")
)

(define-type ont::copper-scale
 :parent ont::color-scale
 :wordnet-sense-keys ("yellow%1:07:00")
)


(define-type ont::presence-of-light-scale
 :parent ont::visual-scale 
)

(define-type ont::lightness-scale
 :parent ont::presence-of-light-scale
 :wordnet-sense-keys ("lightness%1:07:01")
 ;; WORDS: lightness
)

(define-type ont::darkness-scale
 :parent ont::presence-of-light-scale
 :wordnet-sense-keys ("darkness%1:26:00")
 ;; WORDS: darkness
)

(define-type ont::visual-distinctivenss-scale
 :parent ont::visual-scale 
)

(define-type ont::visual-sharpness-scale
 :parent ont::visual-distinctivenss-scale
 :wordnet-sense-keys ("sharpness%1:07:03" "focus%1:07:01")
 ;; WORDS: sharpness, focus
)

(define-type ont::blurriness-scale
 :parent ont::visual-distinctivenss-scale
 :wordnet-sense-keys ("blurriness%1:07:00")
 ;; WORDS: blurry
)

(define-type ont::light-passage-scale
 :parent ont::visual-scale 
)

(define-type ont::opacity-scale
 :parent ont::light-passage-scale 
 ;; WORDS: opacity
)

(define-type ont::visual-clarity-scale
 :parent ont::light-passage-scale 
 ;; WORDS: transparency, clarity, clearness
)


;; sensory scale
(define-type ont::sensory-scale
 :parent ont::physical-property-scale
 :comment "scales that deal with the quality to being perceivable through sensory input"
 ;; WORDS: sensitivity
)

(define-type ont::sensitivity-scale
 :parent ont::sensory-scale
 :wordnet-sense-keys ("sensitivity%1:09:00")
)

(define-type ont::sight-scale
 :parent ont::sensory-scale 
 ;; WORDS: invisibility, visibility
)

(define-type ont::visibility-scale
 :parent ont::sight-scale
 :wordnet-sense-keys ("visibility%1:07:00")
)

(define-type ont::invisibility-scale
 :parent ont::sight-scale
 :wordnet-sense-keys ("invisibility%1:07:00")
)

(define-type ont::tactile-scale
 :parent ont::sensory-scale 
 ;; WORDS: tangibility, intangibility
)

(define-type ont::tangibility-scale
 :parent ont::tactile-scale
 :wordnet-sense-keys ("tangibility%1:07:00")
)

(define-type ont::intangibility-scale
 :parent ont::tactile-scale
 :wordnet-sense-keys ("intangibility%1:07:00")
)

(define-type ont::olfactory-scale
 :parent ont::sensory-scale
)

(define-type ont::auditory-scale
 :parent ont::sensory-scale 
 :wordnet-sense-keys ("audition%1:09:00")
)

;; dampness scale

(define-type ont::moisture-content-scale
 :parent ont::physical-property-scale
)

(define-type ont::wet-scale
 :parent ont::moisture-content-scale
 :wordnet-sense-keys ("wetness%1:26:00")
)

(define-type ont::dry-scale
 :parent ont::moisture-content-scale
 :wordnet-sense-keys ("dryness%1:26:00")
)

(define-type ont::dehydrated-scale
 :parent ont::dry-scale
 :wordnet-sense-keys ("dehydration%1:26:00")
)


;;; SPATIAL SCALE
(define-type ont::spatial-scale
 :parent ont::ordered-domain
 :comment "scales relating to the properties of space"
)

(define-type ont::shape-scale
 :parent ont::spatial-scale
)

(define-type ont::angularity-scale
 :parent ont::shape-scale
 :wordnet-sense-keys ("angularity%1:07:00")
)

(define-type ont::squareness-scale
 :parent ont::angularity-scale
 :wordnet-sense-keys ("squareness%1:07:00")
)

(define-type ont::rectangularity-scale
 :parent ont::angularity-scale
 :wordnet-sense-keys ("rectangularity%1:07:00")
)

(define-type ont::triangularity-scale
 :parent ont::angularity-scale
 :wordnet-sense-keys ("triangularity%1:07:00")
)

(define-type ont::roundness-scale
 :parent ont::shape-scale
 :wordnet-sense-keys ("roundness%1:07:00")
)

;;; BEHAVIORAL SCALE
(define-type ont::behavioral-scale
 :parent ont::ordered-domain
 :comment "scales relating to behavioral or psychological attributes that characterize an entity."
)

(define-type ont::endurance-scale
 :parent ont::behavioral-scale 
 :wordnet-sense-keys ("endurance%1:07:00")
 ;; WORDS: endurance
)

(define-type ont::kindness-scale
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("kindness%1:07:00" "kindness%1:07:01" "tenderness%1:07:00")
 ;; WORDS: tenderness, kindness
)

(define-type ont::aggressiveness-scale
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("aggressiveness%1:12:00")
 ; aggressiveness, violence
 )

(define-type ont::responsibility-scale
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("responsibility%1:07:00") 
 ;; WORDS: responsibility, responsibleness
)

(define-type ont::skillfulness-scale
 :parent ont::behavioral-scale 
 :wordnet-sense-keys ("skillfulness%1:09:00" "skill%1:09:01" "expertise%1:09:00")
)

;;; evaluation-scale
(define-type ont::evaluation-scale
 :parent ont::ordered-domain 
 :wordnet-sense-keys ("quality%1:07:02")
 :comment "scales relating to subjective evaluation of an entity or a situation"
 ;; WORDS: quality
)


(define-type ont::necessity-scale
 :parent ont::evaluation-scale 
 :wordnet-sense-keys ("necessity%1:17:00")
 ;; WORDS: need
)


;; typicality
(define-type ont::typicality-scale
 :parent ont::evaluation-scale
)

(define-type ont::typical-scale
 :parent ont::typicality-scale
 :wordnet-sense-keys ("normality%1:07:00" "normality%1:07:01" "normality%1:26:00" "ordinariness%1:07:00")
)

(define-type ont::not-typical-scale
 :parent ont::typicality-scale
 :wordnet-sense-keys ("irregularity%1:04:00" "abnormality%1:07:00" "extraordinariness%1:07:00" "atypicality%1:26:00")
 ;; WORDS: abnormality, irregularity
)


;; comfort
(define-type ont::comfort-scale
 :parent ont::evaluation-scale 
 ;; WORDS: comfort, discomfort
 :comment "deals with ease and comfort"
)

(define-type ont::comfortable-scale
 :parent ont::comfort-scale
 :wordnet-sense-keys ("comfort%1:26:00" "convenience%1:26:00")
)

(define-type ont::not-comfortable-scale
 :parent ont::comfort-scale
 :wordnet-sense-keys ("discomfort%1:26:00")
)

;; conventionality
(define-type ont::conventionality-scale
 :parent ont::evaluation-scale
)

(define-type ont::conventional-scale
 :parent ont::conventionality-scale
 :wordnet-sense-keys ("orthodoxy%1:07:00")
)

(define-type ont::not-conventional-scale
 :parent ont::conventionality-scale
 :wordnet-sense-keys ("originality%1:07:00")
)

;; cleanliness
(define-type ont::cleanliness-scale
 :parent ont::evaluation-scale 
)

(define-type ont::clean-scale
 :parent ont::cleanliness-scale 
 :wordnet-sense-keys ("cleanliness%1:26:00" "cleanliness%1:07:00")
 ;; WORDS: cleanliness
)

(define-type ont::unclean-scale
 :parent ont::cleanliness-scale 
 :wordnet-sense-keys ("dirtiness%1:26:00")
 ;; WORDS: cleanliness
)

(define-type ont::luckiness-scale
 :parent ont::evaluation-scale 
 :wordnet-sense-keys ("luck%1:26:00")
 ;; WORDS: luck
)

(define-type ont::cost-scale
 :parent ont::evaluation-scale
)

(define-type ont::expensive-scale
 :parent ont::cost-scale
 :wordnet-sense-keys ("costliness%1:07:00" "expensiveness%1:07:00")
)

(define-type ont::not-expensive-scale
 :parent ont::cost-scale
 :wordnet-sense-keys ("cheapness%1:07:00")
)

(define-type ont::ability-scale
 :parent ont::evaluation-scale 
)

(define-type ont::able-scale
 :parent ont::ability-scale 
 :wordnet-sense-keys ("ability%1:07:00" "ability%1:09:00" "capability%1:07:00" "capacity%1:07:00" "competence%1:07:00")
 ;; WORDS: capability, capacity, 
)

(define-type ont::not-able-scale
 :parent ont::ability-scale 
 :wordnet-sense-keys ("inability%1:07:00" "incapacity%1:07:00" "incapability%1:07:00")
 ;; WORDS: inability
)

(define-type ont::attraction-scale
 :parent ont::evaluation-scale
)

(define-type ont::attractive-scale
 :parent ont::attraction-scale
 :wordnet-sense-keys ("appeal%1:07:00" "attractiveness%1:07:00")
)

(define-type ont::not-attractive-scale
 :parent ont::attraction-scale
 :wordnet-sense-keys ("disgust%1:12:00" "repugnance%1:12:00")
)

(define-type ont::acceptability-scale
 :parent ont::evaluation-scale 
)

(define-type ont::goodness-scale
 :parent ont::acceptability-scale 
 :wordnet-sense-keys ("benefit%1:07:00" "goodness%1:07:02")
 ;; WORDS: benefit
)

(define-type ont::badness-scale
 :parent ont::acceptability-scale 
 :wordnet-sense-keys ("badness%1:07:00")
)

(define-type ont::beauty-scale
 :parent ont::evaluation-scale
)

(define-type ont::beautiful-scale
 :parent ont::beauty-scale
 :wordnet-sense-keys ("beauty%1:07:00" "loveliness%1:07:00" "prettiness%1:07:00" "handsomeness%1:07:00") 
 ;; WORDS: beauty
)

(define-type ont::ugly-scale
 :parent ont::beauty-scale
 :wordnet-sense-keys ("ugliness%1:07:00" "unsightliness%1:07:00" "hideousness%1:07:00") 
)



(define-type ont::adaptability-scale
 :parent ont::evaluation-scale
 :wordnet-sense-keys("flexibility%1:07:01" "adaptability%1:07:00" "pliability%1:07:01")
 ;; WORDS: flexibility
)

(define-type ont::importance-scale
 :parent ont::evaluation-scale 
 ;; WORDS: importance, priority
)

(define-type ont::important-scale
 :parent ont::importance-scale
 :wordnet-sense-keys ("significance%1:07:00" "importance%1:07:00" "importance%1:26:00")
)

(define-type ont::not-important-scale
 :parent ont::importance-scale
 :wordnet-sense-keys ("unimportance%1:07:00")
)

(define-type ont::reliability-scale
 :parent ont::evaluation-scale 
 :wordnet-sense-keys ("reliability%1:07:00" "trustworthiness%1:07:00")
 ;; WORDS: reliability
)

(define-type ont::safety-scale
 :parent ont::evaluation-scale 
)

(define-type ont::safe-scale
 :parent ont::safety-scale 
 :wordnet-sense-keys ("safety%1:26:00")
 ;; WORDS: safety, security
)

(define-type ont::unsafe-scale
 :parent ont::safety-scale 
 :wordnet-sense-keys ("insecurity%1:26:00") ; danger?
)

;; appropriateness
(define-type ont::appropriateness-scale
 :parent ont::evaluation-scale 
 ;; WORDS: fitness
)

(define-type ont::appropriate-scale
 :parent ont::appropriateness-scale
 :wordnet-sense-keys ("suitability%1:07:00" "fitness%1:07:00")
)

(define-type ont::not-appropriate-scale
 :parent ont::appropriateness-scale
 :wordnet-sense-keys ("unsuitability%1:07:00")
)

;; convenience
(define-type ont::convenience-scale
 :parent ont::evaluation-scale
)

(define-type ont::convenient-scale
 :parent ont::convenience-scale 
 :wordnet-sense-keys("convenience%1:07:00")
 ;; WORDS: convenience
)

(define-type ont::not-convenient-scale
 :parent ont::convenience-scale
 :wordnet-sense-keys("inconvenience%1:07:00")
)

;; INFORMATION PROPERTY SCALE
(define-type ont::information-property-scale
 :parent ont::ordered-domain 
)

;; trueness scale
(define-type ont::trueness-scale
 :parent ont::information-property-scale
 :comment "trueness scale as opposed to categorical true/false distinction (see ont::truth-scale)"
)

(define-type ont::true-scale
 :parent ont::trueness-scale
 :wordnet-sense-keys ("trueness%1:26:00")
)

(define-type ont::false-scale
 :parent ont::trueness-scale
 :wordnet-sense-keys ("falseness%1:26:00")
)

;; likelihood scale
(define-type ont::likelihood-scale
 :parent ont::information-property-scale 
 :sem (F::Abstr-obj (F::measure-function F::term))
 :arguments ((:REQUIRED ONT::FIGURE (f::situation))
             )
)

(define-type ont::likely-scale
 :parent ont::likelihood-scale
 :wordnet-sense-keys ("probability%1:07:00")
 ;; WORDS: probability, likelihood, chance
)

(define-type ont::not-likely-scale
 :parent ont::likelihood-scale
 :wordnet-sense-keys ("improbability%1:07:00")
 ;; WORDS: improbability
)

;; possibility scale
(define-type ont::possibility-scale
 :parent ont::information-property-scale 
)

(define-type ont::possible-scale
 :parent ont::possibility-scale
 :wordnet-sense-keys ("possibility%1:26:00")
 ;; WORDS: possibility
)

(define-type ont::not-possible-scale
 :parent ont::possibility-scale
 :wordnet-sense-keys ("impossibility%1:26:00")
 ;; WORDS: impossibility
)

;; correctness scale
(define-type ont::correctness-scale
 :parent ont::information-property-scale 
 :wordnet-sense-keys ("accuracy%1:07:02" "correctness%1:07:01")
 ;; WORDS: accuracy
)

;; MEASURE SCALE
(define-type ont::measure-scale
 :parent ont::ordered-domain 
 :wordnet-sense-keys ("quantity%1:03:00" "measurement%1:04:00")
 :arguments ((:essential ont::figure ((? val f::phys-obj f::abstr-obj f::situation))))
 ;; WORDS: quantity, measurement
)

;; time measure scale
(define-type ont::time-measure-scale
  :sem (F::Abstr-obj (F::Scale Ont::time-measure-scale))
  :parent ont::measure-scale
 )

(define-type ont::duration-scale
  :parent ont::time-measure-scale
)

(define-type ont::age-scale
 ;:parent ont::time-measure-scale
 :parent ont::duration-scale
 :wordnet-sense-keys ("age%1:28:00" "age%1:07:00")
)

(define-type ont::time-loc-scale
  :parent ont::time-measure-scale
)

(define-type ont::number-scale
  :parent ont::measure-scale
)

(define-type ont::total-scale
; :parent ont::measure-scale
 :parent ont::number-scale
 :wordnet-sense-keys ("total%1:06:00" "count%1:23:00")
 ;; WORDS: aggregate, total, count
)

(define-type ont::population-scale
 :parent ont::total-scale
 :sem (F::Abstr-obj (F::Measure-function F::term))
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj (F::form F::geographical-object)))
             )
 :wordnet-sense-keys ("population%1:23:00") 
 ;; WORDS: population
)

(define-type ont::money-scale
 :parent ont::measure-scale
)

(define-type ont::predefined-measure-scale
 :parent ont::measure-scale
)

;; resolution scale
(define-type ont::resolution-scale
 :parent ont::measure-scale 
 :wordnet-sense-keys ("resolution%1:19:01")
 ;; WORDS: definition, resolution
 :arguments ((:ESSENTIAL ONT::GROUND (f::abstr-obj (f::scale ont::other-scale)))
             )
)

;; dimensional scale
(define-type ont::dimensional-scale
 :parent ont::measure-scale 
 :wordnet-sense-keys ("dimension%1:07:00" "dimension%1:07:01")
 ;; WORDS: dimension
)

(define-type ont::intensity-scale
 :parent ont::measure-scale 
 :wordnet-sense-keys ("intensity%1:07:00" "intensity%1:07:03")
 ;; WORDS: intensity
)

(define-type ont::severity-scale
 :parent ont::measure-scale 
 :wordnet-sense-keys ("severity%1:07:01")
 ;; WORDS: severity
)

;; size
(define-type ont::size-scale
 :parent ont::dimensional-scale 
 :wordnet-sense-keys("size%1:07:00" "size%1:07:02" "size%1:07:01" "magnitude%1:07:00")
 :sem (F::abstr-obj (F::scale ont::size-scale))
 :arguments ((:REQUIRED ONT::FIGURE ((? fig f::phys-obj f::time))) ; f::time: length of the season
             (:OPTIONAL ONT::EXTENT)
             )
 ;; WORDS: size
 :comment "the property of relative size or extent"
)

;; size > relative-to-height-scale
(define-type ont::relative-to-height-scale
 :parent ont::size-scale
)

(define-type ont::fat-scale
 :wordnet-sense-keys ("obesity%1:07:00" "fatness%1:07:00")
 :parent ont::relative-to-height-scale
)

(define-type ont::skinny-scale
 :wordnet-sense-keys ("thinness%1:07:00" "skinniness%1:07:00" "slenderness%1:07:01" "wiriness%1:07:00")
 :parent ont::relative-to-height-scale
)

;; size > linear-extent
(define-type ont::linear-extent-scale
 :parent ont::size-scale 
 :sem (F::Abstr-obj (F::Scale Ont::Linear-extent-scale))
 :wordnet-sense-keys ("dimension%1:07:00")
 :arguments (;;(:ESSENTIAL ONT::val (F::Abstr-obj (F::Scale Ont::Linear-scale) (F::measure-function F::value)))     
             (:ESSENTIAL ONT::EXTENT (F::abstr-obj (F::scale ont::linear-extent-scale) (F::measure-function F::value))))
)

(define-type ont::length-scale
 :parent ont::linear-extent-scale 
 :wordnet-sense-keys ("length%1:07:00")
 ;; WORDS: length
)

(define-type ont::vertical-scale
 :parent ont::linear-extent-scale 
)

(define-type ont::height-scale
 :parent ont::vertical-scale 
 :sem (F::Abstr-obj (F::Scale Ont::height-scale ))
 :wordnet-sense-keys ("height%1:07:00")
 ;; WORDS: height
)

(define-type ont::depth-scale
 :parent ont::vertical-scale 
 :sem (F::Abstr-obj (F::Scale Ont::depth-scale ))
 :wordnet-sense-keys ("depth%1:07:00")
 ;; WORDS: depth
)

(define-type ont::non-vertical-scale
 :parent ont::linear-extent-scale 
)

(define-type ont::thickness-scale
 :parent ont::non-vertical-scale 
 :wordnet-sense-keys ("thickness%1:07:01")
 :sem (F::Abstr-obj (F::Scale Ont::thickness-scale))
 ;; WORDS: thickness
)

(define-type ont::thinness-scale
 :parent ont::non-vertical-scale
 :wordnet-sense-keys ("thinness%1:07:01")
)

(define-type ont::area-scale
 :parent ont::non-vertical-scale
 :wordnet-sense-keys ("area%1:07:00" "footprint%1:07:00") 
 :sem (F::Abstr-obj (F::Scale Ont::area-scale))
 :arguments ((:ESSENTIAL ONT::GROUND (F::Abstr-obj (F::Scale Ont::area-scale)))
             )
 ;; WORDS: area, footprint
)

(define-type ont::width-scale
 :parent ont::linear-extent-scale 
 :sem (F::Abstr-obj (F::Scale Ont::width-scale ))
 :wordnet-sense-keys ("width%1:07:00")
 ;; WORDS: width
)


;; size > volume scale
(define-type ont::volume-scale
 :parent ont::size-scale 
 :wordnet-sense-keys ("volume%1:23:00")
 :sem (F::Abstr-obj (F::Scale Ont::Volume-scale))
 :arguments ((:ESSENTIAL ONT::GROUND (F::Abstr-obj (F::Scale Ont::Volume-scale)))
             )
 ;; WORDS: volume
)

;; strength
(define-type ont::strength-scale
 :parent ont::dimensional-scale 
 :wordnet-sense-keys ("strength%1:07:00" "vigor%1:07:00" "might%1:07:00" "force%1:07:00")
 ;; WORDS: vigor, vim, strength
)

;; weight
(define-type ont::weight-scale
 :parent ont::dimensional-scale 
 :wordnet-sense-keys ("weight%1:07:00" "heaviness%1:07:00" "weightiness%1:07:00")
 :sem (F::Abstr-obj (F::Scale Ont::Weight-scale))
 :arguments ((:ESSENTIAL ONT::GROUND (F::Abstr-obj (F::Scale Ont::Weight-scale)))
             )
 ;; WORDS: heaviness, weight
)

;; distance
(define-type ont::distance-scale
 :parent ont::measure-scale 
 :arguments ((:REQUIRED ONT::neutral (F::phys-obj))
             (:OPTIONAL ONT::neutral1 (F::phys-obj))
             (:OPTIONAL ONT::FIGURE (F::phys-obj))
             )
 :wordnet-sense-keys ("distance%1:07:00" "interval%1:07:00" "way%1:07:02")
 ;; WORDS: distance, interval, way, ways
)

(define-type ont::mileage-scale
 :parent ont::distance-scale 
 :wordnet-sense-keys ("mileage%1:07:00")
 ;; WORDS: mileage
)

;; electric measure scale
(define-type ont::electric-measure-scale
 :parent ont::measure-scale 
 :wordnet-sense-keys ("current%1:19:01" "charge%1:19:00" "resistance%1:19:01" "polarity%1:24:00") 
 ;; WORDS: current, charge, resistance
)

(define-type ont::ratio-scale
 :parent ont::measure-scale 
 :wordnet-sense-keys ("scale%1:24:01")
; :arguments ((:REQUIRED ONT::FIGURE ((? fot F::phys-obj F::situation)))
;             (:ESSENTIAL ONT::EXTENT (F::abstr-obj (F::measure-function F::value) (F::scale ont::rate-scale)))
;            (:essential ont::FORMAL (F::SITUATION (f::type ont::event-of-change)))                                 
;             )
 ;; WORDS: quotient, scale
)

;; ratio-scale > percent scale
(define-type ont::percent-scale
 :parent ont::ratio-scale 
 :wordnet-sense-keys ("percentage%1:24:00" )
 ;; WORDS: percentage
)

(define-type ont::density-scale
 :parent ont::ratio-scale 
 :wordnet-sense-keys ("density%1:07:00" "concentration%1:07:02" "concentration%1:07:03")
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj))
             )
 ;; WORDS: concentration
)

(define-type ont::distance-per-gasoline-scale
 :parent ont::ratio-scale 
 :wordnet-sense-keys ("mileage%1:24:00")
 ;; WORDS: mileage
)

(define-type ont::pressure-scale
 :parent ont::ratio-scale 
 :wordnet-sense-keys ("pressure%1:19:00")
 ;; WORDS: pressure
)

;; rate
(define-type ont::rate-scale
 :parent ont::ratio-scale 
 :wordnet-sense-keys ("rate%1:21:00" "rate%1:28:00" "incidence%1:24:00")
 :sem (F::Abstr-obj (F::Scale Ont::Rate-scale))
 ;; WORDS: rate
)

(define-type ont::bit-rate-scale
 :parent ont::rate-scale 
 :wordnet-sense-keys ("bandwidth%1:23:00")
 ;; WORDS: bandwidth
)

(define-type ont::clock-speed-scale
 :parent ont::rate-scale 
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj (F::origin F::artifact)))
             )
 ;; WORDS: clock_speed
)

(define-type ont::speed-scale
 :parent ont::rate-scale 
 :wordnet-sense-keys ("speed%1:28:00" "velocity%1:28:00")
 ;; WORDS: speed, velocity
)

(define-type ont::voltage-scale
 :parent ont::ratio-scale 
 :wordnet-sense-keys ("voltage%1:19:02" "electromotive_force%1:19:00" "emf%1:19:00")
 ;; WORDS: voltage
)


;;; PROCESS PROPERTY SCALE
(define-type ont::process-property-scale
 :parent ont::ordered-domain 
)

(define-type ont::process-evaluation-scale
 :parent ont::process-property-scale
)

(define-type ont::task-complexity-scale
 :parent ont::process-evaluation-scale 
 ;; WORDS: complexity
)

(define-type ont::difficult-scale
 :parent ont::task-complexity-scale
 :wordnet-sense-keys ("difficulty%1:07:00")
 ;; WORDS: difficulty
)

(define-type ont::easy-scale
 :parent ont::task-complexity-scale
 :wordnet-sense-keys ("ease%1:07:00")
 ;; WORDS: ease
)

(define-type ont::steadiness-scale
 :parent ont::process-evaluation-scale 
)

(define-type ont::steady-scale
 :parent ont::steadiness-scale
 :wordnet-sense-keys ("steadiness%1:07:01" "stability%1:07:00")
)

(define-type ont::not-steady-scale
 :parent ont::steadiness-scale
 :wordnet-sense-keys ("unsteadiness%1:07:01" "instability%1:07:00")
)

;; temporal occurrence
(define-type ont::temporal-occurrence-scale
    :parent ont::process-property-scale
    ;; :wordnet-sense-keys ("incidence%1:24:00")
)

(define-type ont::regularity-scale
 :parent ont::temporal-occurrence-scale
 ;; WORDS: irregularity, regularity
)

(define-type ont::regular-scale
 :parent ont::regularity-scale
 :wordnet-sense-keys ("regularity%1:07:00")
 ;; regularity
)

(define-type ont::not-regular-scale
 :parent ont::regularity-scale
 :wordnet-sense-keys ("intermittence%1:07:00" "irregularity%1:07:00")
 ;; irregularity
)


;; RELATIONAL PROPERTY SCALE
(define-type ont::relational-property-scale
 :parent ont::ordered-domain 
)

(define-type ont::compatibility-scale
 :parent ont::relational-property-scale 
 :wordnet-sense-keys ("compatibility%1:07:00")
 ;; WORDS: compatibility
)

(define-type ont::similarity-scale
 :parent ont::relational-property-scale 
)

(define-type ont::similar-scale
 :parent ont::similarity-scale
 :wordnet-sense-keys ("similarity%1:07:00" "likeness%1:07:00" "comparability%1:07:00")
)

(define-type ont::different-scale
 :parent ont::similarity-scale
 :wordnet-sense-keys ("difference%1:07:00" "distinction%1:09:00")
)

(define-type ont::equal-scale
 :parent ont::similarity-scale
 :wordnet-sense-keys ("equivalence%1:26:00")
)

(define-type ont::same-scale
 :parent ont::similarity-scale
 :wordnet-sense-keys ("identity%1:07:02" "sameness%1:07:00")
)





;; STATUS SCALE

(define-type ont::status-property-scale
 :parent ont::ordered-domain 
)

(define-type ont::confidentiality-scale
 :parent ont::status-property-scale 
:wordnet-sense-keys ("privacy%1:07:00" "privacy%1:26:02")
 ;; WORDS: privacy
)


;;;; STAGE SCALE
;(define-type ont::stage-scale
; :parent ont::ordered-domain 
;)


;; OBJECT-AFFORDANCE-SCALE
(define-type ont::object-affordances-scale
 :parent ont::ordered-domain 
)

(define-type ont::functionality-scale
 :parent ont::object-affordances-scale
 :wordnet-sense-keys ("functionality%1:07:00")
 ;; WORDS: functionality
)

(define-type ont::availability-scale
 :parent ont::object-affordances-scale
 :wordnet-sense-keys ("availability%1:07:00")
 ;; WORDS: availability
)


(define-type ont::utility-scale
 :parent ont::object-affordances-scale 
 :wordnet-sense-keys ("utility%1:07:00")
 ;; WORDS: utility
 :comment "useful function (e.g. grep - high utility, low usability)"
)

(define-type ont::usability-scale
 :parent ont::object-affordances-scale 
 :wordnet-sense-keys ("usability%1:07:00")
 ;; WORDS: usability
 :comment "ease of use (e.g. ipad - high usability, low utility)"
)

;; PSYCHOLOGICAL CONDITION SCALE
(define-type ont::psychological-condition-scale
    :wordnet-sense-keys ("psychological_state%1:26:00")
    :parent ont::ordered-domain 
    )

(define-type ont::ambitiousness-scale
 :parent ont::psychological-condition-scale
 :wordnet-sense-keys ("ambition%1:07:00")
 ;; WORDS: ambition
)

(define-type ont::cautiousness-scale
 :parent ont::psychological-condition-scale
 :wordnet-sense-keys ("caution%1:07:00")
 ;; WORDS: caution
)

;; confidence
(define-type ont::confidence-scale
 :parent ont::psychological-condition-scale 
 :wordnet-sense-keys ("trust%1:26:00" "authority%1:07:00" "confidence%1:26:02" "confidence%1:12:00") 
 ;; WORDS: authority, confidence, trust
)

(define-type ont::awareness-scale
 :parent ont::psychological-condition-scale 
 :wordnet-sense-keys ("consciousness%1:09:01")
 ;; WORDS: awareness, consciousness
)

;; certainty
(define-type ont::certainty-scale
 :parent ont::psychological-condition-scale 
)

(define-type ont::certain-scale
 :parent ont::certainty-scale
 :wordnet-sense-keys ("certainty%1:09:00" "assurance%1:09:00")
)

(define-type ont::not-certain-scale
 :parent ont::certainty-scale
 :wordnet-sense-keys ("doubt%1:09:00")
)

;; rationality
(define-type  ont::rationality-scale
 :parent ont::psychological-condition-scale
)

(define-type ont::not-rational-scale
 :parent ont::rationality-scale
 :wordnet-sense-keys ("irrationality%1:26:00")
)

(define-type ont::craziness-scale
 :parent ont::not-rational-scale 
 :wordnet-sense-keys ("craziness%1:26:00")
)

(define-type ont::rational-scale
 :parent ont::rationality-scale 
 :wordnet-sense-keys ("sanity%1:26:00" "rationality%1:26:00")
)

;; emotion scale
(define-type ont::emotion-scale
 :parent ont::psychological-condition-scale 
)

;; negative emotions
(define-type ont::negative-emotion-scale
 :parent ont::emotion-scale
)

(define-type ont::loneliness-scale
 :parent ont::negative-emotion-scale 
 :wordnet-sense-keys ("loneliness%1:26:00" "loneliness%1:12:00")
 ;; WORDS: loneliness
)

(define-type ont::nervousness-scale
 :parent ont::negative-emotion-scale 
 :wordnet-sense-keys ("nervousness%1:12:00" "anxiety%1:12:00")
 ;; WORDS: nervousness
)

(define-type ont::sadness-scale
 :parent ont::negative-emotion-scale 
 :wordnet-sense-keys ("sadness%1:12:00")
 ;; WORDS: sadness
)

(define-type ont::grief-scale
 :parent ont::negative-emotion-scale
 :wordnet-sense-keys ("grief%1:12:00" "sorrow%1:12:00")
 ;; WORDS: heartbreak, heartache, (also grief, brokenheartedness)
)

;; positive emotions
(define-type ont::positive-emotion-scale
 :parent ont::emotion-scale
)

(define-type ont::happiness-scale
 :parent ont::positive-emotion-scale 
 :wordnet-sense-keys ("happiness%1:26:00" "happiness%1:12:00")
 ;; WORDS: happiness
)

(define-type ont::excitement-scale
 :parent ont::positive-emotion-scale 
 :wordnet-sense-keys ("excitement%1:12:00")
 ;; WORDS: happiness
)

(define-type ont::pleasantness-scale
 :parent ont::positive-emotion-scale 
 :wordnet-sense-keys ("agreeableness%1:07:00" "pleasantness%1:07:00")
 ;; WORDS: amenity, pleasantness
)

(define-type ont::pride-scale
 :parent ont::positive-emotion-scale 
 :wordnet-sense-keys ("pride%1:12:00")
 ;; WORDS: pride
)

;; interest
(define-type ont::interest-scale
 :parent ont::psychological-condition-scale
)

(define-type ont::interested-scale
 :parent ont::interest-scale
 :wordnet-sense-keys ("interest%1:09:00" "enthusiasm%1:09:00")
)

(define-type ont::not-interested-scale
 :parent ont::interest-scale
 :wordnet-sense-keys ("apathy%1:07:01")
)

;; confusion                                                                                                        
(define-type ont::confusion-scale
 :wordnet-sense-keys ("confusion%1:09:00" "confusion%1:12:00")
 :parent ont::psychological-condition-scale
 )

;; stress                                                                                                           
(define-type ont::stress-scale
 :wordnet-sense-keys ("stress%1:26:01")
 :parent ont::psychological-condition-scale
 )

; for distress                                                                                                      
(define-type ONT::distress-scale
 :parent ONT::psychological-condition-scale
 :wordnet-sense-keys ("distress%1:26:00" "distress%1:12:02")
 )



;; STATE OF AFFAIRS SCALE
(define-type ont::state-of-affairs-scale
 :parent ont::ordered-domain 
)

(define-type ont::balance-scale
 :parent ont::state-of-affairs-scale 
 :wordnet-sense-keys ("balance%1:26:00")
 ;; WORDS: balance
)

(define-type ont::connectivity-scale
 :parent ont::state-of-affairs-scale 
 :wordnet-sense-keys ("connectivity%1:07:00")
 ;; WORDS: connectivity
)


;;;;;; UNORDERED DISCRETE DOMAIN
(define-type ont::unordered-domain
 :parent ont::domain 
)

(define-type ont::gender-scale
    :parent ont::unordered-domain
    :wordnet-sense-keys ("sex%1:14:00")
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj))
             )
 :wordnet-sense-keys ("gender%1:07:00")
 ;; WORDS: gender, sex
)

(define-type ont::truth-scale
 :parent ont::unordered-domain 
 :wordnet-sense-keys ("truth%1:09:00")
 ;; WORDS: truth
)

(define-type ont::discrete-color-scale
 :parent ont::unordered-domain
)



;;;==== ONT::ATTRIBUTE ====                                                                                         

(define-type ONT::attribute
 :wordnet-sense-keys ("dimension%1:09:00" "attribute%1:09:00" "property%1:09:00" "property%1:07:00" "holding%1:21:00" "belongings%1:21:00" "property%1:21:00" "quality%1:07:00")
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

(define-type ONT::medical-disorders-and-conditions
 :wordnet-sense-keys ("disorder%1:26:03")
 :parent ONT::event-type
 :sem (F::situation ) ;;(F::container +))
 :arguments ((:OPTIONAL ONT::FIGURE (F::phys-obj (F::origin (? og2 f::human f::non-human-animal))))
             )
 )

;; symptom                                                                                                          
(define-type ONT::medical-symptom
 :wordnet-sense-keys ("symptom%1:26:00" "sign%1:26:00" "syndrome%1:26:00")
 :parent ONT::medical-disorders-and-conditions
 )

; muscle contraction
(define-type ont::muscle-contraction
 :parent ont::medical-symptom
 :wordnet-sense-keys ("contraction%1:04:01")
)

; sniffle
(define-type ont::sniffle
 :parent ont::medical-symptom
 :wordnet-sense-keys ("sniffle%1:04:00")
)

; tightness/constriction of a body part
(define-type ont::body-tightness
 :parent ont::medical-symptom
 :wordnet-sense-keys("tightness%1:09:00")
)

; for constipation                                                                                                  
(define-type ONT::constipation
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("constipation%1:26:00")
 )

; for cough                                                                                                         
(define-type ONT::cough
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("cough%1:26:00")
 )

; for diarrhea
(define-type ONT::diarrhea
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("diarrhea%1:26:00")
 )

; for dyspepsia                                                                                                     
(define-type ONT::dyspepsia
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("dyspepsia%1:26:00" "indigestion%1:26:00")
 )

(define-type ont::dyspnea
 :wordnet-sense-keys ("dyspnea%1:26:00" "dyspnea%1:26:00" "dyspnoea%1:26:00" "shortness_of_breath%1:26:00" "sob%1:26:00")
 :parent ont::medical-symptom
 )

; for edema                                                                                                         
(define-type ONT::edema
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("edema%1:26:00")
 )

; for fever                                                                                                         
(define-type ONT::fever
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("fever%1:26:00" "febrility%1:26:00" "febricity%1:26:00" "pyrexia%1:26:00" "feverishness%1:26:00")
 )

; for heartburn                                                                                                     
(define-type ONT::heartburn
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("heartburn%1:26:00")
 )

; for hives                                                                                                         
(define-type ONT::hives
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("hives%1:26:00")
 )

; for hyperkalemia                                                                                                  
(define-type ONT::hyperkalemia
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("hyperkalemia%1:26:00")
 )

; for hyperventilation                                                                                              
(define-type ONT::hyperventilation
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("hyperventilation%1:04:00")
 )

; for hypoglycemia                                                                                                  
(define-type ONT::hypoglycemia
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("hypoglycemia%1:26:00")
 )

; for inflammation, redness, phlebitis                                                                              
(define-type ONT::inflammation
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("inflammation%1:26:00" "phlebitis%1:26:00" "redness%1:26:00")
 )

; for jaundice                                                                                                      
(define-type ONT::jaundice
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("jaundice%1:26:00")
 )

; for lightheadedness                                                                                               
(define-type ONT::lightheadedness
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("lightheadedness%1:26:00" "dizziness%1:26:00")
 )

; for nausea                                                                                                        
(define-type ONT::nausea
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("nausea%1:26:00")
 )

; for nosebleed                                                                                                     
(define-type ONT::nosebleed
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("nosebleed%1:26:00")
 )

; for numbness                                                                                                      
(define-type ONT::numbness
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("numbness%1:26:00")
 )

; for pain                                                                                                          
(define-type ONT::pain
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("pain%1:26:00" "hurting%1:26:00" "pain_sensation%1:09:00" "painful_sensation%1:09:00" "soreness%1:26:00")
 )

; for backache                                                                                                      
(define-type ONT::backache
 :parent ONT::pain
 :wordnet-sense-keys ("backache%1:26:00")
 )

; for headache                                                                                                      
(define-type ONT::headache
 :parent ONT::pain
 :wordnet-sense-keys ("headache%1:26:00")
 )

; for stomachache                                                                                                   
(define-type ONT::stomachache
 :parent ONT::pain
 :wordnet-sense-keys ("stomachache%1:26:00")
 )

; for palpitation                                                                                                   
(define-type ONT::palpitation
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("palpitation%1:26:00")
 )

;; seizure                                                                                                          
(define-type ont::seizure
 :wordnet-sense-keys ("seizure%1:26:00")
 :parent ont::medical-symptom
 )

; for sneeze                                                                                                        
(define-type ONT::sneeze
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("sneeze%1:26:00")
 )

; for cramp, spasm                                                                                                  
(define-type ONT::spasm
 :parent ONT::medical-symptom
 :wordnet-sense-keys ("cramp%1:26:00" "spasm%1:26:00")
 )

;; condition                                                                                                        
(define-type ONT::medical-condition
 :wordnet-sense-keys ("condition%1:26:05" "sign%1:26:00" "malformation%1:26:00" "pathology%1:26:00")
 :parent ONT::medical-disorders-and-conditions
 )

;; coma
(define-type ont::coma
 :wordnet-sense-keys("coma%1:09:00")
 :parent ont::medical-condition
)

;; anesthesia                                                                                                       
(define-type ont::anesthesia
 :wordnet-sense-keys ("anesthesia%1:26:00")
 :parent ont::medical-condition
 )

;; angina                                                                                                           
(define-type ont::angina
 :wordnet-sense-keys ("angina%1:26:01")
 :parent ont::medical-condition
 )

;; pregnancy
(define-type ont::pregnancy
 :wordnet-sense-keys ("pregnancy%1:26:00")
 :parent ont::medical-condition
 )

;; disability
(define-type ont::disability
 :parent ont::medical-condition
 :wordnet-sense-keys ("disability%1:26:00")
)

;; stroke                                                                                                           
(define-type ont::stroke
 :wordnet-sense-keys ("stroke%1:26:00")
 :parent ont::medical-condition
 )

;; thrombosis                                                                                                       
(define-type ont::thrombosis
 :wordnet-sense-keys ("thrombosis%1:26:00")
 :parent ont::medical-condition
 )

;; bruise, contusion, bump                                                                                          
(define-type ont::injury
 :wordnet-sense-keys ("concussion%1:11:00" "injury%1:26:00" "hurt%1:26:00" "harm%1:26:00" "trauma%1:26:02" "bruise%1:26:00" "contusion%1:26:00" "bump%1:26:00")
  :parent ont::medical-disorders-and-conditions
 )

;; wound, lesion                                                                                                    
; bruise can come under physical-symptom too but WN defines it as an injury.                                        
(define-type ONT::wound
  :wordnet-sense-keys ("wound%1:26:00" "lesion%1:26:02" "sore%1:26:00")
  :parent ONT::injury
 )

;; sickness                                                                                                         
(define-type ONT::disease
 :wordnet-sense-keys ("disease%1:26:00" "ailment%1:26:00" "abnormality%1:26:00")
 :sem (F::situation (F::mobility f::movable)) ; movable: the disease has spread
 :parent ONT::medical-disorders-and-conditions
 )

; for AIDS                                                                                                          
(define-type ONT::acquired-immune-deficiency-syndrome
 :parent ONT::disease
 :wordnet-sense-keys ("aids%1:26:00" "acquired_immune_deficiency_syndrome%1:26:00")
 )

; for allergy                                                                                                       
(define-type ONT::allergy
 :parent ONT::disease
 :wordnet-sense-keys ("allergy%1:26:00")
 )

; for anemia                                                                                                        
(define-type ONT::anemia
 :parent ONT::disease
 :wordnet-sense-keys ("anemia%1:26:00")
 )

;; angina                                                                                                           
(define-type ont::angina-disease
 :wordnet-sense-keys ("angina%1:26:00")
 :parent ont::medical-symptom
 )

;; arteriosclerosis                                                                                                 
(define-type ont::arteriosclerosis
 :wordnet-sense-keys ("arteriosclerosis%1:26:00")
 :parent ont::disease
 )

(define-type ONT::arthritis
 :parent ONT::disease
 :wordnet-sense-keys ("arthritis%1:26:00")
 )

; for gout                                                                                                          
(define-type ONT::gout
 :parent ONT::arthritis
 :wordnet-sense-keys ("gout%1:26:00")
 )

; for osteoarthritis                                                                                                
(define-type ONT::osteoarthritis
 :parent ONT::arthritis
 :wordnet-sense-keys ("osteoarthritis%1:26:00")
 )

; for asthma                                                                                                        
(define-type ONT::breathing-disorder
 :parent ONT::disease
 :wordnet-sense-keys ("asthma%1:26:00" "respiratory_disorder%1:26:00")
 )

; for brain-disease                                                                                                 
(define-type ONT::brain-disease
 :parent ONT::disease
 :wordnet-sense-keys ("brain_disease%1:26:00")
 )

(define-type ONT::cancer
 :parent ONT::disease
 :wordnet-sense-keys ("cancer%1:26:00" "malignancy%1:26:00" "malignance%1:26:00" "carcinoma%1:26:00" "melanoma%1:26:00" "malignant_melanoma%1:26:00" "lymphoma%1:26:00" "leukemia%1:26:00" "leukaemia%1:26:00" "leucaemia%1:26:00" "cancer_of_the_blood%1:26:00" "sarcoma%1:26:00" "angiosarcoma%1:26:00" "myeloma%1:26:00")
 )

; for cardiovascular diseases etc                                                                                   
(define-type ONT::cardiovascular-disease
 :parent ONT::disease
 :wordnet-sense-keys ("cardiovascular_disease%1:26:00")
 )

; for aneurysm                                                                                                      
(define-type ONT::aneurysm
 :parent ONT::cardiovascular-disease
 :wordnet-sense-keys ("aneurysm%1:26:00")
 )

; for arrhythmia                                                                                                    
(define-type ONT::arrhythmia
 :parent ONT::cardiovascular-disease
 :wordnet-sense-keys ("arrhythmia%1:26:00" "tachycardia%1:26:00")
 )

; for heart attack                                                                                                  
(define-type ONT::heart-attack
 :parent ONT::cardiovascular-disease
 :wordnet-sense-keys ("heart_attack%1:26:00")
 )

; for high BP, hypertension                                                                                         
(define-type ONT::hypertension
 :parent ONT::cardiovascular-disease
 :wordnet-sense-keys ("hypertension%1:26:00")
 )

(define-type ONT::diabetes
 :parent ONT::disease
 :wordnet-sense-keys ("diabetes%1:26:00")
 )

;; ebola                                                                                                            
(define-type ONT::ebola
 :parent ONT::disease
 :wordnet-sense-keys ("ebola%1:26:00")
 )

;; epilepsy                                                                                                         
(define-type ONT::epilepsy
 :parent ONT::disease
 :wordnet-sense-keys ("epilepsy%1:26:00")
 )

;; flu                                                                                                              
(define-type ONT::flu
 :parent ONT::breathing-disorder
 :wordnet-sense-keys ("flu%1:26:00" "influenza%1:26:00" "grippe%1:26:00")
 )

;; infection                                                                                                        
(define-type ONT::infection
 :parent ONT::disease
 :wordnet-sense-keys ("infection%1:26:00")
 )

;; hepatitis, cirrhosis                                                                                             
;; but if we have this category, then liver-cancer can come under this category as well as under cancer.            
(define-type ONT::liver-disease
 :parent ONT::disease
 :wordnet-sense-keys ("liver_disease%1:26:00")
 )

; for osteoporosis                                                                                                  
(define-type ONT::osteoporosis
 :parent ONT::disease
 :wordnet-sense-keys ("osteoporosis%1:26:00")
 )

; for pancreatitis                                                                                                  
(define-type ONT::pancreatitis
 :parent ONT::disease
 :wordnet-sense-keys ("pancreatitis%1:26:00")
 )

; for pneuomnia                                                                                                     
(define-type ONT::pneumonia
 :parent ONT::breathing-disorder
 :wordnet-sense-keys ("pneumonia%1:26:00")
 )

; for STDs                                                                                                          
(define-type ONT::std
 :parent ONT::disease
 :wordnet-sense-keys ("sexually_transmitted_disease%1:26:00")
 )

;; stones, cholelithiasis (gall stones), kidney stones                                                              
(define-type ont::stones-disease
 :wordnet-sense-keys ("cholelithiasis%1:26:00" "gallstone%1:17:00" "kidney_stone%1:17:00")
 :parent ont::disease
 )

#|                                                                                                                  
;; cholelithiasis (gall stones)                                                                                     
(define-type ont::cholelithiasis
 :wordnet-sense-keys ("cholelithiasis%1:26:00" "gallstone%1:17:00")
 :parent ont::stones-disease
 )                                                                                                                  

;; kidney stones                                                                                                    
(define-type ont::kidney-stone
 :wordnet-sense-keys ("kidney_stone%1:17:00")
 :parent ont::stones-disease
 )
|#

; for tuberculosis
(define-type ONT::tuberculosis
 :parent ONT::disease
 :wordnet-sense-keys ("tb%1:26:00")
 )




;; mental illness, mental disorder, psychological disorder                                                          
(define-type ONT::mental-psychological-illness-or-disorder
 :wordnet-sense-keys ("mental_illness%1:26:00" "mental_disorder%1:26:00")
 :parent ONT::event-type
 )

;; addiction                                                                                                        
(define-type ont::addiction
 :wordnet-sense-keys ("addiction%1:26:00")
 :parent ont::mental-psychological-illness-or-disorder
 )

; for amnesia                                                                                                       
(define-type ONT::amnesia
 :parent ONT::mental-psychological-illness-or-disorder
 :wordnet-sense-keys ("amnesia%1:09:00")
 )

;; anorexia                                                                                                         
(define-type ont::eating-disorder
 :wordnet-sense-keys ("anorexia%1:26:00" "eating_disorder%1:26:00")
 :parent ont::mental-psychological-illness-or-disorder
 )

;; anxiety                                                                                                          
(define-type ont::anxiety
 :wordnet-sense-keys ("anxiety%1:26:00")
 :parent ont::mental-psychological-illness-or-disorder
 )

;; ataxia                                                                                                           
(define-type ont::nervous-disorder
 :wordnet-sense-keys ("ataxia%1:26:00" "nervous_disorder%1:26:00")
 :parent ont::mental-psychological-illness-or-disorder
 )


;; depression                                                                                                       
(define-type ont::depression
 :wordnet-sense-keys ("depression%1:26:03")
 :parent ont::mental-psychological-illness-or-disorder
 )

;; insanity                                                                                                         
(define-type ont::insanity
 :wordnet-sense-keys ("insanity%1:26:00")
 :parent ont::mental-psychological-illness-or-disorder
 )

;; mania                                                                                                            
(define-type ont::mania
 :wordnet-sense-keys ("mania%1:26:00")
 :parent ont::mental-psychological-illness-or-disorder
 )

;; phobia                                                                                                           
(define-type ont::phobia
 :wordnet-sense-keys ("phobia%1:26:00")
 :parent ont::mental-psychological-illness-or-disorder
 )




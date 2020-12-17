(in-package :om)

; > starts a block                                                                                                  
; < ends the block                                                                                                  
; use `[code]` to use lisp syntax in code (optional, but makes eliminating commented code easier)                   


;;; ===========ONT::DOMAIN
;;; A domain is a single-valued function                                                                            
(define-type ONT::DOMAIN
 ;:parent ONT::TANGIBLE-ABSTRACT-object
    :parent ONT::ABSTRACT-object ; not tangible
    :sem (F::abstr-obj (F::scale ont::domain))
    :wordnet-sense-keys ("attribute%1:03:00")
    :comment "Nouns that name domain/scales, and can serve as relational nouns (e.g., the COLOR of the box)"
    :arguments ((:REQUIRED ONT::FIGURE)
		(:optional ont::GROUND)
		(:optional ont::EXTENT)
             )
 )


;; ORDERED DOMAIN

(define-type ont::ordered-domain
 :sem (F::abstr-obj (F::scale ONT::ORDERED-DOMAIN))
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
 :sem (F::abstr-obj (F::scale ONT::PHYSICAL-PROPERTY-SCALE))
 :parent ont::ordered-domain
 :comment "scales associated with properties pertaining to the attributes of physical entities or substances. Note: many properties can apply to non-physical objects."
)

;; BODY CONDITION SCALE
(define-type ont::body-condition-scale
 :sem (F::abstr-obj (F::scale ONT::BODY-CONDITION-SCALE))
 :parent ont::physical-property-scale 
; :sem (f::situation (f::aspect f::indiv-level)) ;; prevent attachment of temporal adv                               
; :arguments ((:OPTIONAL ONT::FIGURE (F::phys-obj (F::origin F::natural)))
;             )
)

(define-type ont::positive-body-condition-scale
 :sem (F::abstr-obj (F::scale ONT::POSITIVE-BODY-CONDITION-SCALE))
 :parent ont::body-condition-scale
)

(define-type ont::alertness-scale
 :sem (F::abstr-obj (F::scale ONT::ALERTNESS-SCALE))
 :parent ont::positive-body-condition-scale
 :wordnet-sense-keys ("alertness%1:09:00")
)

(define-type ont::body-energy-scale
 :sem (F::abstr-obj (F::scale ONT::BODY-ENERGY-SCALE))
 :parent ont::positive-body-condition-scale
 :wordnet-sense-keys ("energy%1:07:01" "energy%1:26:00")
 ;; vim, vigor, energy (strength is in ont::strength scale)
)

(define-type ont::health-scale
 :sem (F::abstr-obj (F::scale ONT::HEALTH-SCALE))
 :parent ont::positive-body-condition-scale 
 :wordnet-sense-keys ("wellness%1:26:00" "wellbeing%1:26:00" "health%1:26:00" "healthiness%1:26:00" "condition%1:26:02")
 ;; WORDS: wellness, healthiness, condition (in condition), health, wellbeaing
)

(define-type ont::satiated-scale
 :sem (F::abstr-obj (F::scale ONT::SATIATED-SCALE))
 :parent ont::positive-body-condition-scale
 :wordnet-sense-keys ("satiation%1:26:00")
)

(define-type ont::fitness-scale
 :sem (F::abstr-obj (F::scale ONT::FITNESS-SCALE))
 :parent ont::positive-body-condition-scale
 :wordnet-sense-keys ("fitness%1:26:00")
)

(define-type ont::negative-body-condition-scale
 :sem (F::abstr-obj (F::scale ONT::NEGATIVE-BODY-CONDITION-SCALE))
 :parent ont::body-condition-scale
)

(define-type ont::pain-scale
 :sem (F::abstr-obj (F::scale ONT::PAIN-SCALE))
 :wordnet-sense-keys ("painfulness%1:07:00" "pain%1:12:00" "soreness%1:12:00")
 :parent ONT::negative-body-condition-scale
)

(define-type ont::not-satiated-scale
 :sem (F::abstr-obj (F::scale ONT::NOT-SATIATED-SCALE))
 :parent ont::negative-body-condition-scale
 :wordnet-sense-keys ("hunger%1:26:00" "thirst%1:26:00")
)

(define-type ont::illness-scale
 :sem (F::abstr-obj (F::scale ONT::ILLNESS-SCALE))
 :parent ont::negative-body-condition-scale
 :wordnet-sense-keys ("unhealthiness%1:26:00" "illness%1:26:00")
)

;; restlessness
(define-type ONT::restlessness-scale
 :sem (F::abstr-obj (F::scale ONT::RESTLESSNESS-SCALE))
 :wordnet-sense-keys ("restlessness%1:07:01" "restlessness%1:12:00")
 :parent ont::negative-body-condition-scale
)

;; feebleness                                                                                                       
(define-type ONT::feebleness-scale
 :sem (F::abstr-obj (F::scale ONT::FEEBLENESS-SCALE))
 :wordnet-sense-keys ("feebleness%1:26:00" "weakness%1:07:01") 
 :parent ont::negative-body-condition-scale
 )


(define-type ont::lack-of-energy-scale
 :sem (F::abstr-obj (F::scale ONT::LACK-OF-ENERGY-SCALE))
 :parent ont::negative-body-condition-scale
)

;; lethargy
(define-type ont::lethargy-scale
 :sem (F::abstr-obj (F::scale ONT::LETHARGY-SCALE))
 :wordnet-sense-keys ("lethargy%1:26:00" "lethargy%1:07:00")
 :parent ont::lack-of-energy-scale
)

;; fatigue, tiredness, exhaustion                                                                                   
(define-type ONT::fatigue-scale
 :sem (F::abstr-obj (F::scale ONT::FATIGUE-SCALE))
 :wordnet-sense-keys ("exhaustion%1:26:00" "fatigue%1:26:00" "tiredness%1:26:00")
 :parent ont::lack-of-energy-scale
 )

(define-type ont::neutral-body-condition-scale
 :sem (F::abstr-obj (F::scale ONT::NEUTRAL-BODY-CONDITION-SCALE))
 :parent ont::lack-of-energy-scale
)

(define-type ont::sleepiness-scale
 :sem (F::abstr-obj (F::scale ONT::SLEEPINESS-SCALE))
 :parent ont::neutral-body-condition-scale 
  :wordnet-sense-keys ("sleepiness%1:26:00" "drowsiness%1:26:00")
 ;; WORDS: drowsiness, sleepiness
)

;; polarity-scale
(define-type ont::polarity-scale
 :sem (F::abstr-obj (F::scale ONT::POLARITY-SCALE))
 :parent ont::physical-property-scale
 ;; :wordnet-sense-keys ("polarity%1:24:01")  There is no sense in WN for the scale
 ;; WORDS: polarity
)


;; completeness
(define-type ont::completeness-scale
 :sem (F::abstr-obj (F::scale ONT::COMPLETENESS-SCALE))
 :parent ont::physical-property-scale
)

(define-type ont::complete-scale
 :sem (F::abstr-obj (F::scale ONT::COMPLETE-SCALE))
 :wordnet-sense-keys ("completeness%1:26:00")
 :parent ont::completeness-scale
)

(define-type ont::incomplete-scale
 :sem (F::abstr-obj (F::scale ONT::INCOMPLETE-SCALE))
 :wordnet-sense-keys ("incompleteness%1:26:00")
 :parent ont::completeness-scale
)

;; atmospheric scale
(define-type ont::atmospheric-scale
 :sem (F::abstr-obj (F::scale ONT::ATMOSPHERIC-SCALE))
 :parent ont::physical-property-scale
)

;; humidity scale
(define-type ont::humidity-scale
 :parent ont::atmospheric-scale 
 :wordnet-sense-keys ("humidity%1:26:00")
 :arguments ((:ESSENTIAL ONT::GROUND (F::Abstr-obj (F::Scale Ont::humidity-scale )))) 
 :sem (F::Abstr-obj (F::scale ONT::HUMIDITY-SCALE) )
 ;; WORDS: humidity
)

;; configuration property scale
(define-type ont::configuration-property-scale
 :sem (F::abstr-obj (F::scale ONT::CONFIGURATION-PROPERTY-SCALE))
 :parent ont::physical-property-scale
 :comment "scales for properties regarding the configuration, arrangement or layout of elements"
)

(define-type ont::constriction-scale
 :sem (F::abstr-obj (F::scale ONT::CONSTRICTION-SCALE))
 :parent ont::configuration-property-scale
)

(define-type ont::tightness-scale
 :sem (F::abstr-obj (F::scale ONT::TIGHTNESS-SCALE))
 :parent ont::constriction-scale
 :wordnet-sense-keys("tightness%1:07:00")
)

(define-type ont::looseness-scale
 :sem (F::abstr-obj (F::scale ONT::LOOSENESS-SCALE))
 :parent ont::constriction-scale
 :wordnet-sense-keys("looseness%1:07:00")
)

;;  appearance scale
(define-type ont::appearance-scale
 :sem (F::abstr-obj (F::scale ONT::APPEARANCE-SCALE))
 :parent ont::physical-property-scale 
 :comment "scales related to surface appearance of a physical entity or object preceptible through sensory input"
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj))
             )
 )

(define-type ont::smell-scale
 :sem (F::abstr-obj (F::scale ONT::SMELL-SCALE))
 :parent ont::appearance-scale 
 :wordnet-sense-keys ("smell%1:07:00")
 ;; WORDS: smell
 )

(define-type ont::negative-smell-scale
 :sem (F::abstr-obj (F::scale ONT::NEGATIVE-SMELL-SCALE))
 :parent ont::smell-scale 
 :wordnet-sense-keys ("stinkiness%1:07:00" "rancidness%1:07:00")
 ;; WORDS: malodorousness, rancidness, muskiness
 )

(define-type ont::positive-smell-scale
 :sem (F::abstr-obj (F::scale ONT::POSITIVE-SMELL-SCALE))
 :parent ont::smell-scale 
 :wordnet-sense-keys ("sweetness%1:07:01")
 ;; WORDS: sweetness, fragrance
 ;; specifically referring to the pleasing olfactory property as perceived by the senses 
 ;; and NOT the gragrance/perfume possessed by the item that is being smelled (i.e., fragrance%1:09:00).
)

(define-type ont::sound-scale
 :sem (F::abstr-obj (F::scale ONT::SOUND-SCALE))
 :parent ont::appearance-scale 
)

(define-type ont::sound-volume-scale
 :sem (F::abstr-obj (F::scale ONT::SOUND-VOLUME-SCALE))
 :parent ont::sound-scale 
)

(define-type ont::loudness-scale
 :sem (F::abstr-obj (F::scale ONT::LOUDNESS-SCALE))
 :parent ont::sound-volume-scale
 :wordnet-sense-keys ("loudness%1:07:00")
 ;; WORDS: loudness
)

(define-type ont::sound-softness-scale
 :sem (F::abstr-obj (F::scale ONT::SOUND-SOFTNESS-SCALE))
 :parent ont::sound-volume-scale
 :wordnet-sense-keys ("softness%1:07:01")
 ;; WORDS: softness
 :comment "quality of being low in volume"
)

(define-type ont::quietness-scale
 :sem (F::abstr-obj (F::scale ONT::QUIETNESS-SCALE))
 :parent ont::sound-volume-scale
 :wordnet-sense-keys ("quietness%1:07:00")
 ;; WORDS: quietness
 :comment "quality of lacking sound"
)

(define-type ont::sound-texture-scale
 :sem (F::abstr-obj (F::scale ONT::SOUND-TEXTURE-SCALE))
 :parent ont::sound-scale
)

(define-type ont::hoarseness-scale
 :sem (F::abstr-obj (F::scale ONT::HOARSENESS-SCALE))
 :parent ont::sound-texture-scale
 :wordnet-sense-keys ("hoarseness%1:07:00")
)

(define-type ont::touch-scale
 :sem (F::abstr-obj (F::scale ONT::TOUCH-SCALE))
 :parent ont::appearance-scale 
)

(define-type ont::flexibility-scale
 :sem (F::abstr-obj (F::scale ONT::FLEXIBILITY-SCALE))
 :parent ont::touch-scale 
 :wordnet-sense-keys ("flexibility%1:07:02")
 ;; WORDS: flexibility
 )


(define-type ont::tactile-scale
 :sem (F::abstr-obj (F::scale ONT::TACTILE-SCALE))
 :parent ont::touch-scale 
 )


(define-type ont::texture-scale
 :sem (F::abstr-obj (F::scale ONT::TEXTURE-SCALE))
 :parent ont::tactile-scale 
 :wordnet-sense-keys ("texture%1:07:00")
 ;; WORDS: texture
)


(define-type ont::texture-thickness-scale
 :sem (F::abstr-obj (F::scale ONT::TEXTURE-THICKNESS-SCALE))
 :parent ont::texture-scale
 :wordnet-sense-keys ("thickness%1:07:02")
)

(define-type ont::texture-thinness-scale
 :sem (F::abstr-obj (F::scale ONT::TEXTURE-THINNESS-SCALE))
 :parent ont::texture-scale
 :wordnet-sense-keys ("thinness%1:07:02")
)

(define-type ont::roughness-scale
 :sem (F::abstr-obj (F::scale ONT::ROUGHNESS-SCALE))
 :parent ont::texture-scale
 :wordnet-sense-keys ("roughness%1:07:00") 
 ;; WORDS: choppiness, roughness
)

(define-type ont::sharp-texture-scale
 :sem (F::abstr-obj (F::scale ONT::SHARP-TEXTURE-SCALE))
 :parent ont::texture-scale 
 :wordnet-sense-keys ("sharpness%1:07:01")
 ;; WORDS: sharpness
)

(define-type ont::smoothness-scale
 :sem (F::abstr-obj (F::scale ONT::SMOOTHNESS-SCALE))
 :parent ont::texture-scale 
 :wordnet-sense-keys ("smoothness%1:07:00")
 ;; WORDS: smoothness
)

(define-type ont::taste-scale
 :sem (F::abstr-obj (F::scale ONT::TASTE-SCALE))
 :parent ont::appearance-scale 
)

(define-type ont::spiciness-scale
 :sem (F::abstr-obj (F::scale ONT::SPICINESS-SCALE))
 :parent ont::taste-scale 
 :wordnet-sense-keys ("spiciness%1:07:00")
 ;; WORDS: spiciness
)

(define-type ont::sourness-scale
 :sem (F::abstr-obj (F::scale ONT::SOURNESS-SCALE))
 :parent ont::taste-scale 
 :wordnet-sense-keys ("sourness%1:07:00")
 ;; WORDS: sourness
)

(define-type ont::sweetness-scale
 :sem (F::abstr-obj (F::scale ONT::SWEETNESS-SCALE))
 :parent ont::taste-scale 
 :wordnet-sense-keys ("sweetness%1:07:00" "sweetness%1:09:00")
 ;; WORDS: sweetness
)

(define-type ont::bitterness-scale
 :sem (F::abstr-obj (F::scale ONT::BITTERNESS-SCALE))
 :parent ont::taste-scale 
 :wordnet-sense-keys ("bitterness%1:07:00")
 ;; WORDS: bitterness
)

(define-type ont::visual-scale
 :sem (F::abstr-obj (F::scale ONT::VISUAL-SCALE))
 :parent ont::appearance-scale 
)


(define-type ont::luminosity-scale
 :sem (F::abstr-obj (F::scale ONT::LUMINOSITY-SCALE))
 :parent ont::visual-scale 
 :wordnet-sense-keys ("brightness%1:07:00" "brightness%1:07:02" "illumination%1:26:00")
; :arguments ((:ESSENTIAL ONT::GROUND (F::Abstr-obj (F::Scale Ont::luminosity-scale)))
;             )
 ;; WORDS: brightness
 :comment "quality of giving out or reflecting light"
)

;(define-type ont::brightness-scale
; :parent ont::visual-scale 
; :wordnet-sense-keys ("brightness%1:07:00" "brightness%1:07:02")
; ;; WORDS: brightness
;)

(define-type ont::visual-dullness-scale
 :sem (F::abstr-obj (F::scale ONT::VISUAL-DULLNESS-SCALE))
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
 :wordnet-sense-keys ("colouring%1:07:00" "coloring%1:07:00" "colour%1:07:00" "color%1:07:00" "color%1:09:01" "colour%1:09:01" "color%1:27:00")
)

(define-type ont::red-scale
 :sem (F::abstr-obj (F::scale ONT::RED-SCALE))
 :parent ont::color-scale
 :wordnet-sense-keys ("redness%1:07:00")
 ;; WORDS: redness
)

(define-type ont::blue-scale
 :sem (F::abstr-obj (F::scale ONT::BLUE-SCALE))
 :parent ont::color-scale
 :wordnet-sense-keys ("blue%1:07:00")
)

(define-type ont::green-scale
 :sem (F::abstr-obj (F::scale ONT::GREEN-SCALE))
 :parent ont::color-scale
 :wordnet-sense-keys ("green%1:07:00")
)

(define-type ont::grey-scale
 :sem (F::abstr-obj (F::scale ONT::GREY-SCALE))
 :parent ont::color-scale
 :wordnet-sense-keys ("greyness%1:07:00")
)

(define-type ont::pink-scale
 :sem (F::abstr-obj (F::scale ONT::PINK-SCALE))
 :parent ont::color-scale
 :wordnet-sense-keys ("pink%1:07:00")
)

(define-type ont::magenta-scale
 :sem (F::abstr-obj (F::scale ONT::MAGENTA-SCALE))
 :parent ont::color-scale
 :wordnet-sense-keys ("magenta%1:07:00")
)

(define-type ont::silver-scale
 :sem (F::abstr-obj (F::scale ONT::SILVER-SCALE))
 :parent ont::color-scale
 :wordnet-sense-keys ("silver%1:07:00")
)

(define-type ont::gold-scale
 :sem (F::abstr-obj (F::scale ONT::GOLD-SCALE))
 :parent ont::color-scale
 :wordnet-sense-keys ("gold%1:07:00")
)

(define-type ont::white-scale
 :sem (F::abstr-obj (F::scale ONT::WHITE-SCALE))
 :parent ont::color-scale
 :wordnet-sense-keys ("white%1:07:00")
)

(define-type ont::brown-scale
 :sem (F::abstr-obj (F::scale ONT::BROWN-SCALE))
 :parent ont::color-scale
 :wordnet-sense-keys ("brownness%1:07:00")
)

(define-type ont::black-scale
 :sem (F::abstr-obj (F::scale ONT::BLACK-SCALE))
 :parent ont::color-scale
 :wordnet-sense-keys ("blackness%1:07:00")
)

(define-type ont::purple-scale
 :sem (F::abstr-obj (F::scale ONT::PURPLE-SCALE))
 :parent ont::color-scale
 :wordnet-sense-keys ("purple%1:07:00")
)

(define-type ont::orange-scale
 :sem (F::abstr-obj (F::scale ONT::ORANGE-SCALE))
 :parent ont::color-scale
 :wordnet-sense-keys ("orange%1:07:00")
)

(define-type ont::yellow-scale
 :sem (F::abstr-obj (F::scale ONT::YELLOW-SCALE))
 :parent ont::color-scale
 :wordnet-sense-keys ("yellow%1:07:00")
)

(define-type ont::tan-scale
 :sem (F::abstr-obj (F::scale ONT::TAN-SCALE))
 :parent ont::color-scale
 :wordnet-sense-keys ("tan%1:07:00")
)

(define-type ont::copper-scale
 :sem (F::abstr-obj (F::scale ONT::COPPER-SCALE))
 :parent ont::color-scale
 :wordnet-sense-keys ("copper%1:07:00")
)


(define-type ont::presence-of-light-scale
 :sem (F::abstr-obj (F::scale ONT::PRESENCE-OF-LIGHT-SCALE))
 :parent ont::visual-scale 
)

(define-type ont::lightness-scale
 :sem (F::abstr-obj (F::scale ONT::LIGHTNESS-SCALE))
 :parent ont::presence-of-light-scale
 :wordnet-sense-keys ("lightness%1:07:01")
 ;; WORDS: lightness
)

(define-type ont::darkness-scale
 :sem (F::abstr-obj (F::scale ONT::DARKNESS-SCALE))
 :parent ont::presence-of-light-scale
 :wordnet-sense-keys ("darkness%1:26:00")
 ;; WORDS: darkness
)

(define-type ont::visual-distinctivenss-scale
 :sem (F::abstr-obj (F::scale ONT::VISUAL-DISTINCTIVENSS-SCALE))
 :parent ont::visual-scale 
)

(define-type ont::visual-sharpness-scale
 :sem (F::abstr-obj (F::scale ONT::VISUAL-SHARPNESS-SCALE))
 :parent ont::visual-distinctivenss-scale
 :wordnet-sense-keys ("sharpness%1:07:03" "focus%1:07:01")
 ;; WORDS: sharpness, focus
)

(define-type ont::blurriness-scale
 :sem (F::abstr-obj (F::scale ONT::BLURRINESS-SCALE))
 :parent ont::visual-distinctivenss-scale
 :wordnet-sense-keys ("blurriness%1:07:00")
 ;; WORDS: blurry
)

(define-type ont::light-passage-scale
 :sem (F::abstr-obj (F::scale ONT::LIGHT-PASSAGE-SCALE))
 :parent ont::visual-scale 
)

(define-type ont::opacity-scale
 :sem (F::abstr-obj (F::scale ONT::OPACITY-SCALE))
 :parent ont::light-passage-scale 
 ;; WORDS: opacity
)

(define-type ont::visual-clarity-scale
 :sem (F::abstr-obj (F::scale ONT::VISUAL-CLARITY-SCALE))
 :parent ont::light-passage-scale 
 ;; WORDS: transparency, clarity, clearness
)


;; sensory scale
(define-type ont::sensory-scale
 :sem (F::abstr-obj (F::scale ONT::SENSORY-SCALE))
 :parent ont::physical-property-scale
 :comment "scales that deal with the quality to being perceivable through sensory input"
 ;; WORDS: sensitivity
)

(define-type ont::perceptibility-scale
 :sem (F::abstr-obj (F::scale ONT::PERCEPTIBILITY-SCALE))
 :parent ont::sensory-scale
 :wordnet-sense-keys ("perceptibility%1:07:00")
)

(define-type ont::sensitivity-scale
 :sem (F::abstr-obj (F::scale ONT::SENSITIVITY-SCALE))
 :parent ont::sensory-scale
 :wordnet-sense-keys ("sensitivity%1:09:00")
)

(define-type ont::sight-scale
 :sem (F::abstr-obj (F::scale ONT::SIGHT-SCALE))
 :parent ont::sensory-scale 
 ;; WORDS: invisibility, visibility
)

(define-type ont::visibility-scale
 :sem (F::abstr-obj (F::scale ONT::VISIBILITY-SCALE))
 :parent ont::sight-scale
 :wordnet-sense-keys ("visibility%1:07:00")
)

(define-type ont::invisibility-scale
 :sem (F::abstr-obj (F::scale ONT::INVISIBILITY-SCALE))
 :parent ont::sight-scale
 :wordnet-sense-keys ("invisibility%1:07:00")
 )



(define-type ont::tactile-hardness-scale
 :sem (F::abstr-obj (F::scale ONT::TACTILE-HARDNESS-SCALE))
 :parent ont::texture-scale 
 :wordnet-sense-keys ("hardness%1:07:01")
 ;; WORDS: hardness
)

(define-type ont::tactile-softness-scale
 :sem (F::abstr-obj (F::scale ONT::TACTILE-SOFTNESS-SCALE))
 :parent ont::texture-scale 
 :wordnet-sense-keys ("softness%1:07:00")
 ;; WORDS: softness
 )

(define-type ont::tangibility-scale
 :sem (F::abstr-obj (F::scale ONT::TANGIBILITY-SCALE))
 :parent ont::tactile-scale
 :wordnet-sense-keys ("tangibility%1:07:00")
)

(define-type ont::intangibility-scale
 :sem (F::abstr-obj (F::scale ONT::INTANGIBILITY-SCALE))
 :parent ont::tactile-scale
 :wordnet-sense-keys ("intangibility%1:07:00")
)

(define-type ont::olfactory-scale
 :sem (F::abstr-obj (F::scale ONT::OLFACTORY-SCALE))
 :parent ont::sensory-scale
)

(define-type ont::auditory-scale
 :sem (F::abstr-obj (F::scale ONT::AUDITORY-SCALE))
 :parent ont::sensory-scale 
 :wordnet-sense-keys ("audibility%1:07:00")
)

;; dampness scale

(define-type ont::moisture-content-scale
 :sem (F::abstr-obj (F::scale ONT::MOISTURE-CONTENT-SCALE))
 :parent ont::physical-property-scale
)

(define-type ont::wet-scale
 :sem (F::abstr-obj (F::scale ONT::WET-SCALE))
 :parent ont::moisture-content-scale
 :wordnet-sense-keys ("wetness%1:26:00")
)

(define-type ont::dry-scale
 :sem (F::abstr-obj (F::scale ONT::DRY-SCALE))
 :parent ont::moisture-content-scale
 :wordnet-sense-keys ("dryness%1:26:00")
)

(define-type ont::dehydrated-scale
 :sem (F::abstr-obj (F::scale ONT::DEHYDRATED-SCALE))
 :parent ont::dry-scale
 :wordnet-sense-keys ("dehydration%1:26:00")
)


;;; SPATIAL SCALE
(define-type ont::spatial-scale
 :sem (F::abstr-obj (F::scale ONT::SPATIAL-SCALE))
 :parent ont::ordered-domain ;physical-property-scale
 :comment "scales relating to the properties of space"
)

(define-type ont::shape-scale
    :sem (F::abstr-obj (F::scale ONT::SHAPE-SCALE))
     :wordnet-sense-keys ("shape%1:03:00")
 :parent ont::appearance-scale
)

(define-type ont::angularity-scale
 :sem (F::abstr-obj (F::scale ONT::ANGULARITY-SCALE))
 :parent ont::shape-scale
 :wordnet-sense-keys ("angularity%1:07:00")
)

(define-type ont::squareness-scale
 :sem (F::abstr-obj (F::scale ONT::SQUARENESS-SCALE))
 :parent ont::angularity-scale
 :wordnet-sense-keys ("squareness%1:07:00")
)

(define-type ont::rectangularity-scale
 :sem (F::abstr-obj (F::scale ONT::RECTANGULARITY-SCALE))
 :parent ont::angularity-scale
 :wordnet-sense-keys ("rectangularity%1:07:00")
)

(define-type ont::triangularity-scale
 :sem (F::abstr-obj (F::scale ONT::TRIANGULARITY-SCALE))
 :parent ont::angularity-scale
 :wordnet-sense-keys ("triangularity%1:07:00")
)

(define-type ont::roundness-scale
 :sem (F::abstr-obj (F::scale ONT::ROUNDNESS-SCALE))
 :parent ont::shape-scale
 :wordnet-sense-keys ("roundness%1:07:00")
)

;;; BEHAVIORAL SCALE
(define-type ont::behavioral-scale
 :sem (F::abstr-obj (F::scale ONT::BEHAVIORAL-SCALE))
 :parent ont::ordered-domain
 :comment "scales relating to behavioral or psychological attributes that characterize an entity."
)

(define-type ont::communicativeness-scale
 :sem (F::abstr-obj (F::scale ONT::COMMUNICATIVENESS-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("communicativeness%1:07:00") 
)

(define-type ont::decisiveness-scale
 :sem (F::abstr-obj (F::scale ONT::DECISIVENESS-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("decisiveness%1:07:00")
)

#|
(define-type ont::discernment-scale
 :sem (F::abstr-obj (F::scale ONT::DISCERNMENT-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("discernment%1:09:00")
)
|#

(define-type ont::disposition-scale
 :sem (F::abstr-obj (F::scale ONT::DISPOSITION-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("disposition%1:07:00"); "good-temperedness%1:07:00" "ill_nature%1:07:00" "impatience%1:07:00" "irritability%1:07:00" "shrewishness%1:07:00" "sulkiness%1:07:00") ;"asperity%1:07:02" "crabbiness%1:07:00" "crankiness%1:07:00" 
)

(define-type ont::negative-disposition-scale
 :sem (F::abstr-obj (F::scale ONT::NEGATIVE-DISPOSITION-SCALE))
 :parent ont::disposition-scale
 :wordnet-sense-keys ("ill_nature%1:07:00") ;"asperity%1:07:02" "crabbiness%1:07:00" "crankiness%1:07:00" "ill_nature%1:07:00" "impatience%1:07:00" "irritability%1:07:00" "shrewishness%1:07:00" "sulkiness%1:07:00")
)

(define-type ont::positive-disposition-scale
 :sem (F::abstr-obj (F::scale ONT::POSITIVE-DISPOSITION-SCALE))
 :parent ont::disposition-scale
 :wordnet-sense-keys ("good-temperedness%1:07:00") 
)

(define-type ont::endurance-scale
 :sem (F::abstr-obj (F::scale ONT::ENDURANCE-SCALE))
 :parent ont::behavioral-scale 
 :wordnet-sense-keys ("endurance%1:07:00")
 ;; WORDS: endurance
)

#|
(define-type ont::faithfulness-scale
 :sem (F::abstr-obj (F::scale ONT::FAITHFULNESS-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("faithfulness%1:07:00")
)
|#

(define-type ont::financial-behavior-scale
 :sem (F::abstr-obj (F::scale ONT::FINANCIAL-BEHAVIOR-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("frugality%1:07:00" "wastefulness%1:07:00")
)

(define-type ont::frankness-scale
 :sem (F::abstr-obj (F::scale ONT::FRANKNESS-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("frankness%1:07:01")
)

(define-type ont::kindness-scale
 :sem (F::abstr-obj (F::scale ONT::KINDNESS-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("kindness%1:07:00" "kindness%1:07:01" "tenderness%1:07:00")
 ;; WORDS: tenderness, kindness
)

(define-type ont::knowledge-experience-scale
 :sem (F::abstr-obj (F::scale ONT::KNOWLEDGE-EXPERIENCE-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("knowledgeability%1:07:00" "experience%1:09:01")
)

(define-type ont::loyalty-faithfulness-scale ;loyalty-scale
 :sem (F::abstr-obj (F::scale ONT::LOYALTY-FAITHFULNESS-SCALE)) ;LOYALTY-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("loyalty%1:12:00" "faithfulness%1:07:00")
)

(define-type ont::gracefulness-scale
 :sem (F::abstr-obj (F::scale ONT::GRACEFULNESS-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("gracefulness%1:07:00")
)

(define-type ont::honesty-scale
 :sem (F::abstr-obj (F::scale ONT::HONESTY-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("honesty%1:07:00")
)

(define-type ont::modesty-scale
 :sem (F::abstr-obj (F::scale ONT::MODESTY-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("modesty%1:07:00")
)

(define-type ont::patience-scale
 :sem (F::abstr-obj (F::scale ONT::PATIENCE-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("patience%1:07:00")
)

(define-type ont::affection-scale
 :sem (F::abstr-obj (F::scale ONT::AFFECTION-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("affection%1:12:00" "friendliness%1:07:00" )
)

(define-type ont::sociability-scale
 :sem (F::abstr-obj (F::scale ONT::SOCIABILITY-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("sociality%1:07:00")
)



(define-type ont::wiseness-scale
 :sem (F::abstr-obj (F::scale ONT::WISENESS-SCALE))
 :parent ont::behavioral-scale
)

(define-type ont::wisdom-scale
 :sem (F::abstr-obj (F::scale ONT::WISDOM-SCALE))
 :parent ont::wiseness-scale
 :wordnet-sense-keys ("discernment%1:09:00" "judgment%1:07:00" "wisdom%1:09:01" "wiseness%1:07:00")
)

(define-type ont::folly-scale
 :sem (F::abstr-obj (F::scale ONT::FOLLY-SCALE))
 :parent ont::wiseness-scale
 :wordnet-sense-keys ("folly%1:07:00")
)

(define-type ont::aggressiveness-scale
 :sem (F::abstr-obj (F::scale ONT::AGGRESSIVENESS-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("aggressiveness%1:12:00")
 ; aggressiveness, violence
 )

(define-type ont::tameness-scale
 :sem (F::abstr-obj (F::scale ONT::TAMENESS-SCALE))
 :parent ont::behavioral-scale
)

(define-type ont::tame-scale
 :sem (F::abstr-obj (F::scale ONT::TAME-SCALE))
 :parent ont::tameness-scale
 :wordnet-sense-keys ("tameness%1:07:01")
)

(define-type ont::wild-scale
 :sem (F::abstr-obj (F::scale ONT::WILD-SCALE))
 :parent ont::tameness-scale
 :wordnet-sense-keys ("wildness%1:07:00")
)

;; compassion
(define-type ont::compassion-scale
 :sem (F::abstr-obj (F::scale ONT::COMPASSION-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("compassion%1:12:00")
)

#|;; bold and daring
(define-type ont::boldness-scale
 :sem (F::abstr-obj (F::scale ONT::BOLDNESS-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("daring%1:07:00")
 :comment "bold and daring; forceful approach to challenge"
)|#

;; courage and bravery
(define-type ont::courage-scale
 :sem (F::abstr-obj (F::scale ONT::COURAGE-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("courage%1:07:00")
 :comment "positive strength against fear"
)

;; bold and daring
(define-type ont::boldness-scale
 :sem (F::abstr-obj (F::scale ONT::BOLDNESS-SCALE))
 :parent ont::courage-scale ;behavioral-scale
 :wordnet-sense-keys ("daring%1:07:00")
 :comment "bold and daring; forceful approach to challenge"
)

;; reticence, reservedness
(define-type ont::reticence-scale
 :sem (F::abstr-obj (F::scale ONT::RETICENCE-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("uncommunicativeness%1:07:00")
)

(define-type ont::responsibility-scale
 :sem (F::abstr-obj (F::scale ONT::RESPONSIBILITY-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("responsibility%1:07:00") 
 ;; WORDS: responsibility, responsibleness
)

(define-type ont::self-centeredness-scale
 :sem (F::abstr-obj (F::scale ONT::SELF-CENTEREDNESS-SCALE))
 :parent ont::behavioral-scale
 :wordnet-sense-keys ("self-centeredness%1:07:00")
)

(define-type ont::skillfulness-scale
 :sem (F::abstr-obj (F::scale ONT::SKILLFULNESS-SCALE))
 :parent ont::behavioral-scale 
 :wordnet-sense-keys ("skillfulness%1:09:00" "skill%1:09:01" "expertise%1:09:00")
)

;;; evaluation-scale
(define-type ont::evaluation-scale
 :sem (F::abstr-obj (F::scale ONT::EVALUATION-SCALE))
 :parent ont::ordered-domain 
 :wordnet-sense-keys ("quality%1:07:02")
 :comment "scales relating to subjective evaluation of an entity or a situation"
 ;; WORDS: quality
)

(define-type ont::freedom-scale
 :sem (F::abstr-obj (F::scale ONT::FREEDOM-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("freedom%1:26:00")
)

(define-type ont::helpfulness-scale
 :sem (F::abstr-obj (F::scale ONT::HELPFULNESS-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("helpfulness%1:07:01")
)

(define-type ont::influence-susceptibility-scale
 :sem (F::abstr-obj (F::scale ONT::INFLUENCE-SUSCEPTIBILITY-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("susceptibility%1:26:00")
)

(define-type ont::influence-scale
 :sem (F::abstr-obj (F::scale ONT::INFLUENCE-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("influence%1:07:00")
)

(define-type ont::interestingness-scale
 :sem (F::abstr-obj (F::scale ONT::interestingness-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("interestingness%1:07:00")
)

(define-type ont::fame-scale
 :sem (F::abstr-obj (F::scale ONT::FAME-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("fame%1:26:01" "fame%1:26:02" "prominence%1:26:00")
)

;; freshness
(define-type ont::freshness-scale
 :sem (F::abstr-obj (F::scale ONT::FRESHNESS-SCALE))
 :parent ont::evaluation-scale
)

(define-type ont::fresh-scale
 :sem (F::abstr-obj (F::scale ONT::FRESH-SCALE))
 :parent ont::freshness-scale
 :wordnet-sense-keys ("freshness%1:07:01")
)

(define-type ont::not-fresh-scale
 :sem (F::abstr-obj (F::scale ONT::NOT-FRESH-SCALE))
 :parent ont::freshness-scale
 :wordnet-sense-keys ("staleness%1:07:01")
)

;; familiarity
(define-type ont::familiarity-scale
 :sem (F::abstr-obj (F::scale ONT::FAMILIARITY-SCALE))
 :parent ont::evaluation-scale
)

(define-type ont::familiar-scale
 :sem (F::abstr-obj (F::scale ONT::FAMILIAR-SCALE))
 :parent ont::familiarity-scale
 :wordnet-sense-keys ("familiarity%1:07:01")
)

(define-type ont::not-familiar-scale
 :sem (F::abstr-obj (F::scale ONT::NOT-FAMILIAR-SCALE))
 :parent ont::familiarity-scale
 :wordnet-sense-keys ("unfamiliarity%1:07:00")
)

(define-type ont::hospitability-scale
 :sem (F::abstr-obj (F::scale ONT::HOSPITABILITY-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("hospitableness%1:07:00")
)

(define-type ont::judgement-scale
 :sem (F::abstr-obj (F::scale ONT::JUDGEMENT-SCALE))
 :parent ont::evaluation-scale
 ;:wordnet-sense-keys ("judgment%1:04:02")
)

;; necessity
(define-type ont::necessity-scale
 :sem (F::abstr-obj (F::scale ONT::NECESSITY-SCALE))
 :parent ont::evaluation-scale 
 :wordnet-sense-keys ("necessity%1:17:00")
 ;; WORDS: need
)

(define-type ont::qualification-scale
 :sem (F::abstr-obj (F::scale ONT::QUALIFICATION-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("eligibility%1:07:00")
)

(define-type ont::seniority-scale
 :sem (F::abstr-obj (F::scale ONT::SENIORITY-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("seniority%1:26:00")
)

(define-type ont::shapeliness-scale
 :sem (F::abstr-obj (F::scale ONT::SHAPELINESS-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("shapeliness%1:07:00")
)

;; typicality
(define-type ont::typicality-scale
 :sem (F::abstr-obj (F::scale ONT::TYPICALITY-SCALE))
 :parent ont::evaluation-scale
)

(define-type ont::typical-scale
 :sem (F::abstr-obj (F::scale ONT::TYPICAL-SCALE))
 :parent ont::typicality-scale
 :wordnet-sense-keys ("normality%1:07:00" "normality%1:07:01" "normality%1:26:00" "ordinariness%1:07:00")
)

(define-type ont::not-typical-scale
 :sem (F::abstr-obj (F::scale ONT::NOT-TYPICAL-SCALE))
 :parent ont::typicality-scale
 :wordnet-sense-keys ("irregularity%1:04:00" "abnormality%1:07:00" "extraordinariness%1:07:00" "atypicality%1:26:00")
 ;; WORDS: abnormality, irregularity
)


;; comfort
(define-type ont::comfort-scale
 :sem (F::abstr-obj (F::scale ONT::COMFORT-SCALE))
 :parent ont::evaluation-scale 
 ;; WORDS: comfort, discomfort
 :comment "deals with ease and comfort"
)

(define-type ont::comfortable-scale
 :sem (F::abstr-obj (F::scale ONT::COMFORTABLE-SCALE))
 :parent ont::comfort-scale
 :wordnet-sense-keys ("comfort%1:26:00" "convenience%1:26:00")
)

(define-type ont::not-comfortable-scale
 :sem (F::abstr-obj (F::scale ONT::NOT-COMFORTABLE-SCALE))
 :parent ont::comfort-scale
 :wordnet-sense-keys ("discomfort%1:26:00")
)

(define-type ont::controlled-scale
 :sem (F::abstr-obj (F::scale ONT::CONTROL-SCALE))
 :parent ont::evaluation-scale
 :comment "scale corresponding to being controlled"
)

;; conventionality
(define-type ont::conventionality-scale
 :sem (F::abstr-obj (F::scale ONT::CONVENTIONALITY-SCALE))
 :parent ont::evaluation-scale
)

(define-type ont::conventional-scale
 :sem (F::abstr-obj (F::scale ONT::CONVENTIONAL-SCALE))
 :parent ont::conventionality-scale
 :wordnet-sense-keys ("orthodoxy%1:07:00")
)

(define-type ont::not-conventional-scale
 :sem (F::abstr-obj (F::scale ONT::NOT-CONVENTIONAL-SCALE))
 :parent ont::conventionality-scale
 :wordnet-sense-keys ("originality%1:07:00")
)

;; cleanliness
(define-type ont::cleanliness-scale
 :sem (F::abstr-obj (F::scale ONT::CLEANLINESS-SCALE))
 :parent ont::evaluation-scale 
)

(define-type ont::clean-scale
 :sem (F::abstr-obj (F::scale ONT::CLEAN-SCALE))
 :parent ont::cleanliness-scale 
 :wordnet-sense-keys ("cleanliness%1:26:00" "cleanliness%1:07:00")
 ;; WORDS: cleanliness
)

(define-type ont::unclean-scale
 :sem (F::abstr-obj (F::scale ONT::UNCLEAN-SCALE))
 :parent ont::cleanliness-scale 
 :wordnet-sense-keys ("dirtiness%1:26:00")
 ;; WORDS: cleanliness
)

(define-type ont::luckiness-scale
 :sem (F::abstr-obj (F::scale ONT::LUCKINESS-SCALE))
 :parent ont::evaluation-scale 
 :wordnet-sense-keys ("luck%1:26:00")
 ;; WORDS: luck
)

(define-type ont::cost-scale
 :sem (F::abstr-obj (F::scale ONT::COST-SCALE))
 :parent ont::evaluation-scale
)

(define-type ont::expensive-scale
 :sem (F::abstr-obj (F::scale ONT::EXPENSIVE-SCALE))
 :parent ont::cost-scale
 :wordnet-sense-keys ("costliness%1:07:00" "expensiveness%1:07:00")
)

(define-type ont::not-expensive-scale
 :sem (F::abstr-obj (F::scale ONT::NOT-EXPENSIVE-SCALE))
 :parent ont::cost-scale
 :wordnet-sense-keys ("cheapness%1:07:00")
)

;; wealth
(define-type ont::wealth-scale
 :sem (F::abstr-obj (F::scale ONT::WEALTH-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("financial_condition%1:26:00")
)

(define-type ont::poverty-scale
  :sem (F::abstr-obj (F::scale ONT::POVERTY-SCALE))
  :parent ont::wealth-scale
  :wordnet-sense-keys ("poverty%1:26:00")
 ;:arguments ((:OPTIONAL ONT::FIGURE)
 ;            )
  )

(define-type ont::ability-scale
 :sem (F::abstr-obj (F::scale ONT::ABILITY-SCALE))
 :parent ont::evaluation-scale 
)

(define-type ont::able-scale
 :sem (F::abstr-obj (F::scale ONT::ABLE-SCALE))
 :parent ont::ability-scale 
 :wordnet-sense-keys ("ability%1:07:00" "ability%1:09:00" "capability%1:07:00" "capacity%1:07:00" "competence%1:07:00" "capability%1:26:00")
 ;; WORDS: capability, capacity, 
)

(define-type ont::not-able-scale
 :sem (F::abstr-obj (F::scale ONT::NOT-ABLE-SCALE))
 :parent ont::ability-scale 
 :wordnet-sense-keys ("inability%1:07:00" "incapacity%1:07:00" "incapability%1:07:00")
 ;; WORDS: inability
)

(define-type ont::attraction-scale
 :sem (F::abstr-obj (F::scale ONT::ATTRACTION-SCALE))
 :parent ont::evaluation-scale
)

(define-type ont::attractive-scale
 :sem (F::abstr-obj (F::scale ONT::ATTRACTIVE-SCALE))
 :parent ont::attraction-scale
 :wordnet-sense-keys ("appeal%1:07:00" "attractiveness%1:07:00")
)

(define-type ont::not-attractive-scale
 :sem (F::abstr-obj (F::scale ONT::NOT-ATTRACTIVE-SCALE))
 :parent ont::attraction-scale
 :wordnet-sense-keys ("repugnance%1:12:00")
)

(define-type ont::acceptability-scale
 :sem (F::abstr-obj (F::scale ONT::ACCEPTABILITY-SCALE))
 :parent ont::evaluation-scale 
)

(define-type ont::goodness-scale
 :sem (F::abstr-obj (F::scale ONT::GOODNESS-SCALE))
 :parent ont::acceptability-scale 
 :wordnet-sense-keys ("benefit%1:07:00" "goodness%1:07:02")
 ;; WORDS: benefit
)


(define-type ont::badness-scale
 :sem (F::abstr-obj (F::scale ONT::BADNESS-SCALE))
 :parent ont::acceptability-scale 
 :wordnet-sense-keys ("badness%1:07:00")
)

(define-type ont::beauty-scale
 :sem (F::abstr-obj (F::scale ONT::BEAUTY-SCALE))
 :parent ont::evaluation-scale
)

(define-type ont::beautiful-scale
 :sem (F::abstr-obj (F::scale ONT::BEAUTIFUL-SCALE))
 :parent ont::beauty-scale
 :wordnet-sense-keys ("beauty%1:07:00" "loveliness%1:07:00" "prettiness%1:07:00" "handsomeness%1:07:00") 
 ;; WORDS: beauty
)

(define-type ont::ugly-scale
 :sem (F::abstr-obj (F::scale ONT::UGLY-SCALE))
 :parent ont::beauty-scale
 :wordnet-sense-keys ("ugliness%1:07:00" "unsightliness%1:07:00" "hideousness%1:07:00") 
)

(define-type ont::noticeability-scale
 :sem (F::abstr-obj (F::scale ONT::NOTICEABILITY-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("conspicuousness%1:07:00")
)

(define-type ont::adaptability-scale
 :sem (F::abstr-obj (F::scale ONT::ADAPTABILITY-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys("flexibility%1:07:01" "adaptability%1:07:00" "pliability%1:07:01")
 ;; WORDS: flexibility
)

(define-type ont::importance-scale
 :sem (F::abstr-obj (F::scale ONT::IMPORTANCE-SCALE))
 :parent ont::evaluation-scale 
 ;; WORDS: importance, priority
)

(define-type ont::important-scale
 :sem (F::abstr-obj (F::scale ONT::IMPORTANT-SCALE))
 :parent ont::importance-scale
 :wordnet-sense-keys ("significance%1:07:00" "importance%1:07:00" "importance%1:26:00"
					     "urgency%1:26:00")
)

(define-type ont::not-important-scale
 :sem (F::abstr-obj (F::scale ONT::NOT-IMPORTANT-SCALE))
 :parent ont::importance-scale
 :wordnet-sense-keys ("unimportance%1:07:00")
)

(define-type ont::reliability-scale
 :sem (F::abstr-obj (F::scale ONT::RELIABILITY-SCALE))
 :parent ont::evaluation-scale 
 :wordnet-sense-keys ("reliability%1:07:00" "trustworthiness%1:07:00")
 ;; WORDS: reliability
)

(define-type ont::safety-scale
 :sem (F::abstr-obj (F::scale ONT::SAFETY-SCALE))
 :parent ont::evaluation-scale 
)

(define-type ont::safe-scale
 :sem (F::abstr-obj (F::scale ONT::SAFE-SCALE))
 :parent ont::safety-scale 
 :wordnet-sense-keys ("safety%1:26:00")
 ;; WORDS: safety, security
)

(define-type ont::unsafe-scale
 :sem (F::abstr-obj (F::scale ONT::UNSAFE-SCALE))
 :parent ont::safety-scale 
 :wordnet-sense-keys ("insecurity%1:26:00") ; danger?
)

;; appropriateness
(define-type ont::appropriateness-scale
 :sem (F::abstr-obj (F::scale ONT::APPROPRIATENESS-SCALE))
 :parent ont::evaluation-scale 
 ;; WORDS: fitness
)

(define-type ont::appropriate-scale
 :sem (F::abstr-obj (F::scale ONT::APPROPRIATE-SCALE))
 :parent ont::appropriateness-scale
 :wordnet-sense-keys ("suitability%1:07:00" "fitness%1:07:00")
)

(define-type ont::not-appropriate-scale
 :sem (F::abstr-obj (F::scale ONT::NOT-APPROPRIATE-SCALE))
 :parent ont::appropriateness-scale
 :wordnet-sense-keys ("unsuitability%1:07:00")
)

;; convenience
(define-type ont::convenience-scale
 :sem (F::abstr-obj (F::scale ONT::CONVENIENCE-SCALE))
 :parent ont::evaluation-scale
)

(define-type ont::convenient-scale
 :sem (F::abstr-obj (F::scale ONT::CONVENIENT-SCALE))
 :parent ont::convenience-scale 
 :wordnet-sense-keys("convenience%1:07:00")
 ;; WORDS: convenience
)

(define-type ont::not-convenient-scale
 :sem (F::abstr-obj (F::scale ONT::NOT-CONVENIENT-SCALE))
 :parent ont::convenience-scale
 :wordnet-sense-keys("inconvenience%1:07:00")
)

;; basis, foundation
(define-type ont::basic-scale
 :sem (F::abstr-obj (F::scale ONT::BASIC-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("simplicity%1:07:02") ;"foundation%1:09:00")
)

;; plainness
(define-type ont::plain-scale
 :sem (F::abstr-obj (F::scale ONT::PLAIN-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("plainness%1:26:00")
)

;; preferred
;(define-type ont::preference-scale
; :parent ont::evaluation-scale
; :wordnet-sense-keys ("")
;)

;; level of quality, superior, inferior
(define-type ont::quality-level-scale
 :sem (F::abstr-obj (F::scale ONT::QUALITY-LEVEL-SCALE))
 :parent ont::evaluation-scale
)

(define-type ont::superiority-scale
 :sem (F::abstr-obj (F::scale ONT::SUPERIORITY-SCALE))
 :parent ont::quality-level-scale
 :wordnet-sense-keys ("superiority%1:07:00")
)

(define-type ont::inferiority-scale
 :sem (F::abstr-obj (F::scale ONT::INFERIORITY-SCALE))
 :parent ont::quality-level-scale
 :wordnet-sense-keys ("inferiority%1:07:00")
)


;; explicit, implicit
(define-type ont::explicitness-scale
 :sem (F::abstr-obj (F::scale ONT::EXPLICITNESS-SCALE))
 :parent ont::evaluation-scale
)

(define-type ont::explicit-scale
 :sem (F::abstr-obj (F::scale ONT::EXPLICIT-SCALE))
 :parent ont::explicitness-scale
 :wordnet-sense-keys ("explicitness%1:07:00")
)

(define-type ont::implicit-scale
 :sem (F::abstr-obj (F::scale ONT::IMPLICIT-SCALE))
 :parent ont::explicitness-scale
 :wordnet-sense-keys ("inexplicitness%1:07:00")
)

;; harmfulness (harmlessness not in WN, benigness a word?!)
(define-type ont::harmfulness-scale
 :sem (F::abstr-obj (F::scale ONT::HARMFULNESS-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("harmfulness%1:07:00")
)

;;asethetic-judgement
(define-type ont::aesthetic-tastefulness-scale
 :sem (F::abstr-obj (F::scale ONT::AESTHETIC-TASTEFULNESS-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("stylishness%1:07:00")
)

;; partiality, bias, prejudice
(define-type ont::partiality-scale
 :sem (F::abstr-obj (F::scale ONT::PARTIALITY-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("partiality%1:09:00" "impartiality%1:09:00")
)

;; morality
(define-type ont::morality-scale
 :sem (F::abstr-obj (F::scale ONT::MORALITY-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("morality%1:07:00")
)

;; offensiveness
(define-type ont::offensiveness-scale
 :sem (F::abstr-obj (F::scale ONT::OFFENSIVENESS-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("offensiveness%1:07:00")
)

;; messiness, orderliness
(define-type ont::orderliness-scale
 :sem (F::abstr-obj (F::scale ONT::ORDERLINESS-SCALE))
 :parent ont::evaluation-scale
)

(define-type ont::messy-scale
 :sem (F::abstr-obj (F::scale ONT::MESSY-SCALE))
 :parent ont::orderliness-scale
 :wordnet-sense-keys("disorderliness%1:26:00" "messiness%1:07:00")
)

(define-type ont::tidy-scale
 :sem (F::abstr-obj (F::scale ONT::TIDY-SCALE))
 :parent ont::orderliness-scale
 :wordnet-sense-keys("orderliness%1:26:00")
)

;; recommendability
(define-type ont::recommendability-scale
 :sem (F::abstr-obj (F::scale ONT::RECOMMENDABILITY-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("advisability%1:07:00")
 :comment "worthy of recommendation and advice; wise or prudent"
)

;; specificity generality
(define-type ont::specificity-scale
 :sem (F::abstr-obj (F::scale ONT::SPECIFICITY-SCALE))
 :parent ont::evaluation-scale
)

(define-type ont::general-scale
 :sem (F::abstr-obj (F::scale ONT::GENERAL-SCALE))
 :parent ont::specificity-scale
 :wordnet-sense-keys ("generality%1:07:00")
)

(define-type ont::specific-scale
 :sem (F::abstr-obj (F::scale ONT::SPECIFIC-SCALE))
 :parent ont::specificity-scale
 :wordnet-sense-keys ("specialness%1:07:01")
)

;; tolerability
(define-type ont::tolerability-scale
 :sem (F::abstr-obj (F::scale ONT::TOLERABILITY-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("tolerance%1:07:03")
)

;; worthiness
(define-type ont::worthiness-scale
 :sem (F::abstr-obj (F::scale ONT::WORTHINESS-SCALE))
 :parent ont::evaluation-scale
 :wordnet-sense-keys ("worthiness%1:07:00")
)

(define-type ont::real-vs-fake-scale
 :sem (F::abstr-obj (F::scale ONT::REAL-VS-FAKE-SCALE))
 :parent ont::evaluation-scale
)

;; artificiality
(define-type ont::artificiality-scale
 :sem (F::abstr-obj (F::scale ONT::ARTIFICIALITY-SCALE))
 :parent ont::real-vs-fake-scale
)

(define-type ont::artificial-scale
 :sem (F::abstr-obj (F::scale ONT::ARTIFICIAL-SCALE))
 :parent ont::artificiality-scale
 :wordnet-sense-keys ("affectedness%1:07:00" "artificiality%1:07:00")
)

(define-type ont::natural-scale
 :sem (F::abstr-obj (F::scale ONT::NATURAL-SCALE))
 :parent ont::artificiality-scale
 :wordnet-sense-keys ("unnaturalness%1:07:00")
)

;; actuality
(define-type ont::actuality-scale
 :sem (F::abstr-obj (F::scale ONT::ACTUALITY-SCALE))
 :parent ont::real-vs-fake-scale
)

(define-type ont::actual-scale
 :sem (F::abstr-obj (F::scale ONT::ACTUAL-SCALE))
 :parent ont::actuality-scale
 :wordnet-sense-keys ("actuality%1:26:00" "reality%1:07:00")
)

(define-type ont::non-actual-scale
 :sem (F::abstr-obj (F::scale ONT::NON-ACTUAL-SCALE))
 :parent ont::actuality-scale
 :wordnet-sense-keys ("unreality%1:26:00")
)

;; authenticity
(define-type ont::authenticity-scale
 :sem (F::abstr-obj (F::scale ONT::AUTHENTICITY-SCALE))
 :parent ont::real-vs-fake-scale
 :wordnet-sense-keys ("authenticity%1:07:00")
)

;; INFORMATION PROPERTY SCALE
(define-type ont::information-property-scale
 :sem (F::abstr-obj (F::scale ONT::INFORMATION-PROPERTY-SCALE))
 :parent ont::ordered-domain 
)

#|
;; comprehensibility
(define-type ont::comprehensibility-scale
 :sem (F::abstr-obj (F::scale ONT::COMPREHENSIBILITY-SCALE))
 :parent ont::information-property-scale
 :wordnet-sense-keys ("comprehensibility%1:07:00")
)
|#

;; consistency
(define-type ont::consistency-scale
 :sem (F::abstr-obj (F::scale ONT::CONSISTENCY-SCALE))
 :parent ont::information-property-scale
 :wordnet-sense-keys ("cohesiveness%1:26:00" "consistency%1:07:01")
)

;; credibility
(define-type ont::credibility-scale
 :sem (F::abstr-obj (F::scale ONT::CREDIBILITY-SCALE))
 :parent ont::information-property-scale
 :wordnet-sense-keys ("credibility%1:07:00")
)

;; meaningfulness
(define-type ont::meaningfulness-scale
 :sem (F::abstr-obj (F::scale ONT::MEANINGFULNESS-SCALE))
 :parent ont::information-property-scale
 :wordnet-sense-keys ("meaningfulness%1:07:00")
)

;; precision
(define-type ont::precision-scale
 :sem (F::abstr-obj (F::scale ONT::PRECISION-SCALE))
 :parent ont::information-property-scale
 :wordnet-sense-keys ("precision%1:07:00")
)

#|
;; predictability
(define-type ont::predictability-scale
 :sem (F::abstr-obj (F::scale ONT::PREDICTABILITY-SCALE))
 :parent ont::information-property-scale
 :wordnet-sense-keys ("predictability%1:07:00")
)
|#

;; relevance
(define-type ont::relevance-scale
 :sem (F::abstr-obj (F::scale ONT::RELEVANCE-SCALE))
 :parent ont::information-property-scale
 :wordnet-sense-keys ("relevance%1:24:00")
)

;; validity 
(define-type ont::validity-scale
 :sem (F::abstr-obj (F::scale ONT::VALIDITY-SCALE))
 :parent ont::information-property-scale
 :wordnet-sense-keys ("validity%1:07:01")
)

#|
;; reproducibility
(define-type ont::reproducibility-scale
 :sem (F::abstr-obj (F::scale ONT::REPRODUCIBILITY-SCALE))
 :parent ont::information-property-scale
 :wordnet-sense-keys ("reproducibility%1:07:00")
)
|#

;; trueness scale
(define-type ont::trueness-scale
 :sem (F::abstr-obj (F::scale ONT::TRUENESS-SCALE))
 :parent ont::information-property-scale
 ;:comment "trueness scale as opposed to categorical true/false distinction (see ont::truth-scale)"
)

(define-type ont::true-scale
 :sem (F::abstr-obj (F::scale ONT::TRUE-SCALE))
 :parent ont::trueness-scale
 :wordnet-sense-keys ("trueness%1:26:00")
)

(define-type ont::false-scale
 :sem (F::abstr-obj (F::scale ONT::FALSE-SCALE))
 :parent ont::trueness-scale
 :wordnet-sense-keys ("falseness%1:26:00")
)

;; likelihood scale
(define-type ont::likelihood-scale
 :parent ont::information-property-scale 
 :sem (F::Abstr-obj (F::scale ONT::LIKELIHOOD-SCALE) (F::measure-function F::term))
 :arguments ((:REQUIRED ONT::FIGURE (f::situation))
             )
)

(define-type ont::likely-scale
 :sem (F::abstr-obj (F::scale ONT::LIKELY-SCALE))
 :parent ont::likelihood-scale
 :wordnet-sense-keys ("probability%1:07:00")
 ;; WORDS: probability, likelihood, chance
)

(define-type ont::not-likely-scale
 :sem (F::abstr-obj (F::scale ONT::NOT-LIKELY-SCALE))
 :parent ont::likelihood-scale
 :wordnet-sense-keys ("improbability%1:07:00")
 ;; WORDS: improbability
)

;; possibility scale
(define-type ont::possibility-scale
 :sem (F::abstr-obj (F::scale ONT::POSSIBILITY-SCALE))
 :parent ont::information-property-scale 
)

(define-type ont::possible-scale
 :sem (F::abstr-obj (F::scale ONT::POSSIBLE-SCALE))
 :parent ont::possibility-scale
 :wordnet-sense-keys ("possibility%1:26:00")
 ;; WORDS: possibility
)

(define-type ont::not-possible-scale
 :sem (F::abstr-obj (F::scale ONT::NOT-POSSIBLE-SCALE))
 :parent ont::possibility-scale
 :wordnet-sense-keys ("impossibility%1:26:00")
 ;; WORDS: impossibility
)

;; correctness scale
(define-type ont::correctness-scale
 :sem (F::abstr-obj (F::scale ONT::CORRECTNESS-SCALE))
 :parent ont::information-property-scale 
 :wordnet-sense-keys ("accuracy%1:07:02" "correctness%1:07:01")
 ;; WORDS: accuracy
)

;; LIFE PROCESS SCALE
(define-type ont::life-process-scale
 :sem (F::abstr-obj (F::scale ONT::LIFE-PROCESS-SCALE))
 :parent ont::ordered-domain
 :wordnet-sense-keys ("organic_phenomenon%1:19:00")
)

;; MEASURE SCALE
(define-type ont::measure-scale
 :sem (F::abstr-obj (F::scale ONT::MEASURE-SCALE))
 :parent ont::ordered-domain 
 :wordnet-sense-keys ("measurement%1:04:00")
 :arguments ((:essential ont::figure ((? val f::phys-obj f::abstr-obj f::situation))))
 ;; WORDS: quantity, measurement
)

;; time measure scale
;(define-type ont::time-measure-scale
;  :sem (F::Abstr-obj (F::Scale Ont::time-measure-scale))
;  :parent ont::measure-scale
; )

;(define-type ont::duration-scale
; :sem (F::abstr-obj (F::scale ONT::DURATION-SCALE))
;  :parent ont::time-measure-scale
;  )

;; temperature scale
(define-type ont::temperature-scale
 :wordnet-sense-keys ("temperature%1:07:00" "temperature%1:09:00")
 :parent ont::measure-scale 
 :sem (F::abstr-obj (F::Scale Ont::temperature-scale))
 ;; WORDS: temperature
)

(define-type ont::heat-scale
 :sem (F::abstr-obj (F::scale ONT::HEAT-SCALE))
 :wordnet-sense-keys ("heat%1:07:01" "heat%1:09:00")
 :parent ont::temperature-scale
 ;; WORDS: heat
)

(define-type ont::cold-scale
 :sem (F::abstr-obj (F::scale ONT::COLD-SCALE))
 :wordnet-sense-keys ("cold%1:07:00" "cold%1:09:00")
 :parent ont::temperature-scale
 ;; Words: cold
)

;(define-type ont::age-scale
; :sem (F::abstr-obj (F::scale ONT::AGE-SCALE))
; ;:parent ont::time-measure-scale
; :parent ont::duration-scale
; :wordnet-sense-keys ("age%1:28:00" "age%1:07:00")
;)

;(define-type ont::time-loc-scale
; :sem (F::abstr-obj (F::scale ONT::TIME-LOC-SCALE))
;  :parent ont::time-measure-scale
;)

(define-type ont::number-scale
 :sem (F::abstr-obj (F::scale ONT::NUMBER-SCALE))
  :parent ont::measure-scale
)

(define-type ont::total-scale
 :sem (F::abstr-obj (F::scale ONT::TOTAL-SCALE))
; :parent ont::measure-scale
 :parent ont::number-scale
 :wordnet-sense-keys ("total%1:06:00" "count%1:23:00")
 ;; WORDS: aggregate, total, count
)

(define-type ont::population-scale
 :parent ont::total-scale
 :sem (F::Abstr-obj (F::scale ONT::POPULATION-SCALE) (F::Measure-function F::term))
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj (F::form F::geographical-object)))
             )
 :wordnet-sense-keys ("population%1:23:00") 
 ;; WORDS: population
)

(define-type ont::money-scale
 :sem (F::abstr-obj (F::scale ONT::MONEY-SCALE))
 :parent ont::measure-scale
)

(define-type ont::predefined-measure-scale
 :sem (F::abstr-obj (F::scale ONT::PREDEFINED-MEASURE-SCALE))
 :parent ont::measure-scale
)

;; resolution scale
(define-type ont::resolution-scale
 :sem (F::abstr-obj (F::scale ONT::RESOLUTION-SCALE))
 :parent ont::measure-scale 
 :wordnet-sense-keys ("resolution%1:19:01")
 ;; WORDS: definition, resolution
 )


;; dimensional scale
(define-type ont::dimensional-scale
 :sem (F::abstr-obj (F::scale ONT::DIMENSIONAL-SCALE))
 :parent ont::measure-scale 
 :wordnet-sense-keys ("dimension%1:07:01")
 ;; WORDS: dimension
)

(define-type ont::intensity-scale
 :sem (F::abstr-obj (F::scale ONT::INTENSITY-SCALE))
 :parent ont::measure-scale 
 :wordnet-sense-keys ("intensity%1:07:00" "intensity%1:07:03")
 ;; WORDS: intensity
)

(define-type ont::severity-scale
 :sem (F::abstr-obj (F::scale ONT::SEVERITY-SCALE))
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
 :sem (F::abstr-obj (F::scale ONT::RELATIVE-TO-HEIGHT-SCALE))
 :parent ont::size-scale
)

(define-type ont::fat-scale
 :sem (F::abstr-obj (F::scale ONT::FAT-SCALE))
 :wordnet-sense-keys ("obesity%1:07:00" "fatness%1:07:00")
 :parent ont::relative-to-height-scale
)

(define-type ont::skinny-scale
 :sem (F::abstr-obj (F::scale ONT::SKINNY-SCALE))
 :wordnet-sense-keys ("thinness%1:07:00" "skinniness%1:07:00" "slenderness%1:07:01" "wiriness%1:07:00")
 :parent ont::relative-to-height-scale
)

;; size > linear-extent
(define-type ont::linear-extent-scale
 :parent ont::size-scale 
 :sem (F::Abstr-obj (F::Scale Ont::Linear-extent-scale))
 :wordnet-sense-keys ("dimension%1:07:00")
 :arguments (;;(:ESSENTIAL ONT::val (F::Abstr-obj (F::Scale Ont::Linear-scale) (F::measure-function F::value)))
	     (:essential ont::figure (F::phys-obj))
             (:ESSENTIAL ONT::EXTENT (F::abstr-obj (F::scale ont::linear-extent-scale) (F::measure-function F::value))))
 )

(define-type ont::length-scale
 :sem (F::abstr-obj (F::scale ONT::LENGTH-SCALE))
 :parent ont::linear-extent-scale 
 :wordnet-sense-keys ("length%1:07:00")
 ;; WORDS: length
)

(define-type ont::vertical-scale
 :sem (F::abstr-obj (F::scale ONT::VERTICAL-SCALE))
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
 :sem (F::abstr-obj (F::scale ONT::NON-VERTICAL-SCALE))
 :parent ont::linear-extent-scale 
)

(define-type ont::thickness-scale
 :parent ont::non-vertical-scale 
 :wordnet-sense-keys ("thickness%1:07:01")
 :sem (F::Abstr-obj (F::Scale Ont::thickness-scale))
 ;; WORDS: thickness
)

(define-type ont::thinness-scale
 :sem (F::abstr-obj (F::scale ONT::THINNESS-SCALE))
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
 :wordnet-sense-keys ("volume%1:07:03")
 :sem (F::Abstr-obj (F::Scale Ont::Volume-scale))
 :arguments ((:ESSENTIAL ONT::GROUND (F::Abstr-obj (F::Scale Ont::Volume-scale)))
             )
 ;; WORDS: volume
)


;;; SHOULD THIS BE UNDER PHYSICAL PROPERTIES? (BODY PROPERTIES/CONDITIONS)
(define-type ont::physical-strength-scale
 :sem (F::abstr-obj (F::scale ONT::PHYSICAL-STRENGTH-SCALE))
 :parent ont::dimensional-scale
)
;; strength
(define-type ont::strength-scale
 :sem (F::abstr-obj (F::scale ONT::STRENGTH-SCALE))
 :parent ont::physical-strength-scale
 :wordnet-sense-keys ("strength%1:07:00" "vigor%1:07:00" "might%1:07:00" "force%1:07:00")
 ;; WORDS: strength
)

(define-type ont::weakness-scale
 :sem (F::abstr-obj (F::scale ONT::WEAKNESS-SCALE))
 :parent ont::physical-strength-scale
 :wordnet-sense-keys ("weakness%1:07:00")
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
 :sem (F::abstr-obj (F::scale ONT::DISTANCE-SCALE))
 :parent ont::measure-scale 
 :arguments ((:REQUIRED ONT::neutral (F::phys-obj))
             (:OPTIONAL ONT::neutral1 (F::phys-obj))
             (:OPTIONAL ONT::FIGURE (F::phys-obj))
             )
 :wordnet-sense-keys ("distance%1:07:00" "interval%1:07:00" "way%1:07:02")
 ;; WORDS: distance, interval, way, ways
)

(define-type ont::mileage-scale
 :sem (F::abstr-obj (F::scale ONT::MILEAGE-SCALE))
 :parent ont::distance-scale 
 :wordnet-sense-keys ("mileage%1:07:00")
 ;; WORDS: mileage
)

;; electric measure scale
(define-type ont::electric-measure-scale
 :sem (F::abstr-obj (F::scale ONT::ELECTRIC-MEASURE-SCALE))
 :parent ont::measure-scale 
 :wordnet-sense-keys ("current%1:19:01" "charge%1:19:00" "resistance%1:19:01" "polarity%1:24:00") 
 ;; WORDS: current, charge, resistance
)

(define-type ont::ratio-scale
 :sem (F::abstr-obj (F::scale ONT::RATIO-SCALE))
 :parent ont::measure-scale 
 :wordnet-sense-keys ("scale%1:24:01")
; :arguments ((:REQUIRED ONT::FIGURE ((? fot F::phys-obj F::situation)))
;             (:ESSENTIAL ONT::EXTENT (F::abstr-obj (F::measure-function F::value) (F::scale ont::rate-scale)))
;            (:essential ont::FORMAL (F::SITUATION (f::type ont::event-of-change)))                                 
;             )
 ;; WORDS: quotient, scale
)

;; unique lf for rate, as in gsa rate
(define-type ont::charge-per-unit
  :parent ont::ratio-scale ;value-cost
  :wordnet-sense-keys ("charge_per_unit%1:21:00")
)

;; ratio-scale > percent scale
(define-type ont::percent-scale
 :sem (F::abstr-obj (F::scale ONT::PERCENT-SCALE))
 :parent ont::ratio-scale 
 :wordnet-sense-keys ("percentage%1:24:00" )
 ;; WORDS: percentage
)

(define-type ont::density-scale
 :sem (F::abstr-obj (F::scale ONT::DENSITY-SCALE))
 :parent ont::ratio-scale 
 :wordnet-sense-keys ("density%1:07:00" "concentration%1:07:02" "concentration%1:07:03")
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj))
             )
 ;; WORDS: concentration
)

(define-type ont::distance-per-gasoline-scale
 :sem (F::abstr-obj (F::scale ONT::DISTANCE-PER-GASOLINE-SCALE))
 :parent ont::ratio-scale 
 :wordnet-sense-keys ("mileage%1:24:00")
 ;; WORDS: mileage
)

(define-type ont::pressure-scale
 :sem (F::abstr-obj (F::scale ONT::PRESSURE-SCALE))
 :parent ont::ratio-scale 
 :wordnet-sense-keys ("pressure%1:19:00")
 ;; WORDS: pressure
)

;; rate
(define-type ont::rate-scale
 :parent ont::ratio-scale 
 :wordnet-sense-keys ("rate%1:28:00" "incidence%1:24:00") ;"rate%1:21:00" 
 :sem (F::Abstr-obj (F::Scale Ont::Rate-scale))
 ;; WORDS: rate
)

(define-type ont::bit-rate-scale
 :sem (F::abstr-obj (F::scale ONT::BIT-RATE-SCALE))
 :parent ont::rate-scale 
 :wordnet-sense-keys ("bandwidth%1:23:00")
 ;; WORDS: bandwidth
)

(define-type ont::frequency-scale
 :sem (F::abstr-obj (F::scale ONT::CLOCK-SPEED-SCALE))
 :parent ont::rate-scale 
 :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj (F::origin F::artifact)))
             )
 ;; WORDS: clock_speed
)

(define-type ont::speed-scale
 :sem (F::abstr-obj (F::scale ONT::SPEED-SCALE))
 :parent ont::rate-scale 
 :wordnet-sense-keys ("speed%1:28:00" "velocity%1:28:00")
 ;; WORDS: speed, velocity
)

(define-type ont::voltage-scale
 :sem (F::abstr-obj (F::scale ONT::VOLTAGE-SCALE))
 :parent ont::ratio-scale 
 :wordnet-sense-keys ("voltage%1:19:02" "electromotive_force%1:19:00" "emf%1:19:00")
 ;; WORDS: voltage
)


;;; PROCESS PROPERTY SCALE
(define-type ont::process-property-scale
 :sem (F::abstr-obj (F::scale ONT::PROCESS-PROPERTY-SCALE))
 :parent ont::ordered-domain 
)

(define-type ont::process-evaluation-scale
 :sem (F::abstr-obj (F::scale ONT::PROCESS-EVALUATION-SCALE))
 :parent ont::evaluation-scale
)

(define-type ont::task-complexity-scale
 :sem (F::abstr-obj (F::scale ONT::TASK-COMPLEXITY-SCALE))
 :parent ont::process-evaluation-scale 
 ;; WORDS: complexity
)

(define-type ont::difficult-scale
 :sem (F::abstr-obj (F::scale ONT::DIFFICULT-SCALE))
 :parent ont::task-complexity-scale
 :wordnet-sense-keys ("difficulty%1:07:00")
 ;; WORDS: difficulty
)

(define-type ont::easy-scale
 :sem (F::abstr-obj (F::scale ONT::EASY-SCALE))
 :parent ont::task-complexity-scale
 :wordnet-sense-keys ("ease%1:07:00")
 ;; WORDS: ease
)

(define-type ont::steadiness-scale
 :sem (F::abstr-obj (F::scale ONT::STEADINESS-SCALE))
 :parent ont::process-evaluation-scale 
)

(define-type ont::steady-scale
 :sem (F::abstr-obj (F::scale ONT::STEADY-SCALE))
 :parent ont::steadiness-scale
 :wordnet-sense-keys ("steadiness%1:07:01" "stability%1:07:00" "stability%1:07:01" )
)

(define-type ont::not-steady-scale
 :sem (F::abstr-obj (F::scale ONT::NOT-STEADY-SCALE))
 :parent ont::steadiness-scale
 :wordnet-sense-keys ("unsteadiness%1:07:01" "instability%1:07:00")
)


;; effectiveness
(define-type ont::effectiveness-scale
 :sem (F::abstr-obj (F::scale ONT::EFFECTIVENESS-SCALE))
 :parent ont::process-evaluation-scale
 :wordnet-sense-keys ("effectiveness%1:07:00")
)

;; efficiency
(define-type ont::efficiency-scale
 :sem (F::abstr-obj (F::scale ONT::EFFICIENCY-SCALE))
 :parent ont::process-evaluation-scale
 :wordnet-sense-keys ("efficiency%1:09:00")
)

;; productivity
(define-type ont::productivity-scale
 :sem (F::abstr-obj (F::scale ONT::PRODUCTIVITY-SCALE))
 :parent ont::process-evaluation-scale
 :wordnet-sense-keys ("productivity%1:07:00")
)

;; success, failure
(define-type ont::successfulness-scale
 :sem (F::abstr-obj (F::scale ONT::SUCCESSFULNESS-SCALE))
 :parent ont::process-evaluation-scale
)

(define-type ont::successful-scale
 :sem (F::abstr-obj (F::scale ONT::SUCCESSFUL-SCALE))
 :parent ont::successfulness-scale
 :wordnet-sense-keys ("successfulness%1:26:00")
)

(define-type ont::failure-scale
 :sem (F::abstr-obj (F::scale ONT::FAILURE-SCALE))
 :parent ont::successfulness-scale
 :wordnet-sense-keys ("failure%1:26:00")
)

;; utility, usefulness
;; usability - under object affordances
(define-type ont::utility-scale
 :sem (F::abstr-obj (F::scale ONT::UTILITY-SCALE))
 :parent ont::process-evaluation-scale 
 :wordnet-sense-keys ("utility%1:07:00")
 ;; WORDS: utility
 :comment "useful function (e.g. grep - high utility, low usability)"
)

;; can be done
(define-type ont::can-be-done-scale
 :sem (F::abstr-obj (F::scale ONT::CAN-BE-DONE-SCALE))
 :parent ont::ordered-domain ;process-property-scale
)

;; accessibility - same thing as availability??
(define-type ont::accessibility-scale
 :sem (F::abstr-obj (F::scale ONT::ACCESSIBILITY-SCALE))
 :parent ont::can-be-done-scale
)

;; avoidability
(define-type ont::avoidability-scale
 :sem (F::abstr-obj (F::scale ONT::AVOIDABILITY-SCALE))
 :parent ont::can-be-done-scale
)

;; changeability
(define-type ont::changeability-scale
 :sem (F::abstr-obj (F::scale ONT::CHANGEABILITY-SCALE))
 :parent ont::can-be-done-scale
 :wordnet-sense-keys ("changeability%1:07:00")
)

;; chargeability
(define-type ont::chargeability-scale
 :sem (F::abstr-obj (F::scale ONT::CHARGEABILITY-SCALE))
 :parent ont::can-be-done-scale
)

;; comprehensibility
(define-type ont::comprehensibility-scale
 :sem (F::abstr-obj (F::scale ONT::COMPREHENSIBILITY-SCALE))
 :parent ont::can-be-done-scale ;information-property-scale
 :wordnet-sense-keys ("comprehensibility%1:07:00")
)

;; enforceability
(define-type ont::enforceability-scale
 :sem (F::abstr-obj (F::scale ONT::ENFORCEABILITY-SCALE))
 :parent ont::can-be-done-scale
)

;; expandability
(define-type ont::expandability-scale
 :sem (F::abstr-obj (F::scale ONT::EXPANDABILITY-SCALE))
 :parent ont::can-be-done-scale
)

;; manageability
(define-type ont::manageability-scale
 :sem (F::abstr-obj (F::scale ONT::MANAGEABILITY-SCALE))
 :parent ont::can-be-done-scale
 :wordnet-sense-keys ("manageability%1:07:00")
)

;; measurability
(define-type ont::measurability-scale
 :sem (F::abstr-obj (F::scale ONT::MEASURABILITY-SCALE))
 :parent ont::can-be-done-scale
 :wordnet-sense-keys ("measurability%1:07:00")
)

;; mobility
(define-type ont::mobility-scale
 :sem (F::abstr-obj (F::scale ONT::MOBILITY-SCALE))
 :parent ont::can-be-done-scale
 :wordnet-sense-keys ("mobility%1:07:00")
)

;; networkability
(define-type ont::networkability-scale
 :sem (F::abstr-obj (F::scale ONT::NETWORKABILITY-SCALE))
 :parent ont::can-be-done-scale
)

;; permeability
(define-type ont::permeability-scale
 :sem (F::abstr-obj (F::scale ONT::PERMEABILITY-SCALE))
 :parent ont::can-be-done-scale
)

;; portability
(define-type ont::portability-scale
 :sem (F::abstr-obj (F::scale ONT::PORTABILITY-SCALE))
 :parent ont::can-be-done-scale
 :wordnet-sense-keys ("portability%1:07:00")
)

;; predictability
(define-type ont::predictability-scale
 :sem (F::abstr-obj (F::scale ONT::PREDICTABILITY-SCALE))
 :parent ont::can-be-done-scale ;information-property-scale
 :wordnet-sense-keys ("predictability%1:07:00")
)

;; readability
(define-type ont::readability-scale
 :sem (F::abstr-obj (F::scale ONT::READABILITY-SCALE))
 :parent ont::can-be-done-scale ;information-property-scale
)

;; reparability
(define-type ont::reparability-scale
 :sem (F::abstr-obj (F::scale ONT::REPARABILITY-SCALE))
 :parent ont::can-be-done-scale ;information-property-scale
)

;; repleaceability
(define-type ont::replaceability-scale
 :sem (F::abstr-obj (F::scale ONT::REPLACEABILITY-SCALE))
 :parent ont::can-be-done-scale
 :wordnet-sense-keys ("replaceability%1:07:00")
)

;; reproducibility
(define-type ont::reproducibility-scale
 :sem (F::abstr-obj (F::scale ONT::REPRODUCIBILITY-SCALE))
 :parent ont::can-be-done-scale ;information-property-scale
 :wordnet-sense-keys ("reproducibility%1:07:00")
)

;; transferability
(define-type ont::transferability-scale
 :sem (F::abstr-obj (F::scale ONT::TRANSFERABILITY-SCALE))
 :parent ont::can-be-done-scale ;information-property-scale
 :wordnet-sense-keys ("transferability%1:07:00")
)

;; process status
(define-type ont::process-status-scale
 :sem (F::abstr-obj (F::scale ONT::PROCESS-STATUS-SCALE))
 :parent ont::process-property-scale
)


;; temporal occurrence
;(define-type ont::temporal-occurrence-scale
; :sem (F::abstr-obj (F::scale ONT::TEMPORAL-OCCURRENCE-SCALE))
;    :parent ont::process-property-scale
;    ;; :wordnet-sense-keys ("incidence%1:24:00")
;)

;(define-type ont::regularity-scale
; :sem (F::abstr-obj (F::scale ONT::REGULARITY-SCALE))
; :parent ont::temporal-occurrence-scale
; ;; WORDS: irregularity, regularity
;)

;(define-type ont::regular-scale
; :sem (F::abstr-obj (F::scale ONT::REGULAR-SCALE))
; :parent ont::regularity-scale
; :wordnet-sense-keys ("regularity%1:07:00")
; ;; regularity
;)

;(define-type ont::not-regular-scale
; :sem (F::abstr-obj (F::scale ONT::NOT-REGULAR-SCALE))
; :parent ont::regularity-scale
; :wordnet-sense-keys ("intermittence%1:07:00" "irregularity%1:07:00")
; ;; irregularity
;)


;; RELATIONAL PROPERTY SCALE
(define-type ont::relational-property-scale
 :sem (F::abstr-obj (F::scale ONT::RELATIONAL-PROPERTY-SCALE))
 :parent ont::ordered-domain 
)

(define-type ont::compatibility-scale
 :sem (F::abstr-obj (F::scale ONT::COMPATIBILITY-SCALE))
 :parent ont::relational-property-scale 
 :wordnet-sense-keys ("compatibility%1:07:00")
 ;; WORDS: compatibility
)

(define-type ont::dependence-scale
 :sem (F::abstr-obj (F::scale ONT::DEPENDENCE-SCALE))
 :parent ont::relational-property-scale
 :wordnet-sense-keys ("dependence%1:26:00")
)

(define-type ont::reciprocity-scale
 :sem (F::abstr-obj (F::scale ONT::RECIPROCITY-SCALE))
 :parent ont::relational-property-scale
 :wordnet-sense-keys ("reciprocity%1:24:00")
)

(define-type ont::similarity-scale
 :sem (F::abstr-obj (F::scale ONT::SIMILARITY-SCALE))
 :parent ont::relational-property-scale 
)

(define-type ont::similar-scale
 :sem (F::abstr-obj (F::scale ONT::SIMILAR-SCALE))
 :parent ont::similarity-scale
 :wordnet-sense-keys ("similarity%1:07:00" "likeness%1:07:00" "comparability%1:07:00")
)

(define-type ont::different-scale
 :sem (F::abstr-obj (F::scale ONT::DIFFERENT-SCALE))
 :parent ont::similarity-scale
 :wordnet-sense-keys ("difference%1:07:00" "distinction%1:09:00")
)

(define-type ont::equal-scale
 :sem (F::abstr-obj (F::scale ONT::EQUAL-SCALE))
 :parent ont::similarity-scale
 :wordnet-sense-keys ("equivalence%1:26:00")
)

(define-type ont::same-scale
 :sem (F::abstr-obj (F::scale ONT::SAME-SCALE))
 :parent ont::similarity-scale
 :wordnet-sense-keys ("identity%1:07:02" "sameness%1:07:00")
)

(define-type ont::systematicity-scale
 :sem (F::abstr-obj (F::scale ONT::SYSTEMATICITY-SCALE))
 :parent ont::relational-property-scale
)

(define-type ont::regularity-scale
  :sem (F::abstr-obj (F::scale ONT::REGULARITY-SCALE))
  :parent ont::systematicity-scale ;temporal-occurrence-scale
  ;; WORDS: irregularity, regularity
)

(define-type ont::regular-scale
  :sem (F::abstr-obj (F::scale ONT::REGULAR-SCALE))
  :parent ont::regularity-scale
  :wordnet-sense-keys ("regularity%1:07:00")
  ;; regularity
)
 
(define-type ont::not-regular-scale
  :sem (F::abstr-obj (F::scale ONT::NOT-REGULAR-SCALE))
  :parent ont::regularity-scale
  :wordnet-sense-keys ("intermittence%1:07:00" "irregularity%1:07:00")
  ;; irregularity
)

(define-type ont::sequence-scale
  :sem (F::abstr-obj (F::scale ONT::SEQUENCE-SCALE))
  :parent ont::systematicity-scale
  :wordnet-sense-keys ("earliness%1:07:00")
)

;; STATUS SCALE

(define-type ont::status-property-scale
 :sem (F::abstr-obj (F::scale ONT::STATUS-PROPERTY-SCALE))
 :parent ont::ordered-domain 
)

(define-type ont::confidentiality-scale
 :sem (F::abstr-obj (F::scale ONT::CONFIDENTIALITY-SCALE))
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
 :sem (F::abstr-obj (F::scale ONT::OBJECT-AFFORDANCES-SCALE))
 :parent ont::ordered-domain 
)

(define-type ont::functionality-scale
 :sem (F::abstr-obj (F::scale ONT::FUNCTIONALITY-SCALE))
 :parent ont::object-affordances-scale
 :wordnet-sense-keys ("functionality%1:07:00")
 ;; WORDS: functionality
)

(define-type ont::availability-scale
 :sem (F::abstr-obj (F::scale ONT::AVAILABILITY-SCALE))
 :parent ont::object-affordances-scale
 :wordnet-sense-keys ("availability%1:07:00")
 ;; WORDS: availability
)


;; utility moved to process-evaluation-scale to match the adj side
(define-type ont::usability-scale
 :sem (F::abstr-obj (F::scale ONT::USABILITY-SCALE))
 :parent ont::object-affordances-scale 
 :wordnet-sense-keys ("usability%1:07:00")
 ;; WORDS: usability
 :comment "ease of use (e.g. ipad - high usability, low utility)"
 )

;; PSYCHOLOGICAL CONDITION SCALE
(define-type ont::psychological-condition-scale
 :sem (F::abstr-obj (F::scale ONT::PSYCHOLOGICAL-CONDITION-SCALE))
    :wordnet-sense-keys ("trait%1:07:00")
    :parent ont::ordered-domain 
    )

(define-type ont::creativity-scale
 :sem (F::abstr-obj (F::scale ONT::CREATIVITY-SCALE))
 :parent ont::psychological-condition-scale
 :wordnet-sense-keys ("creativity%1:09:00")
)

(define-type ont::willingness-scale
 :sem (F::abstr-obj (F::scale ONT::WILLINGNESS-SCALE))
 :parent ont::psychological-condition-scale
 :wordnet-sense-keys ("willingness%1:07:00")
)
(define-type ont::intelligence-scale
 :sem (F::abstr-obj (F::scale ONT::INTELLIGENCE-SCALE))
 :parent ont::psychological-condition-scale
 :wordnet-sense-keys ("intelligence%1:09:00")
)

(define-type ont::intentionality-scale
 :sem (F::abstr-obj (F::scale ONT::INTENTIONALITY-SCALE))
 :parent ont::psychological-condition-scale
 :wordnet-sense-keys ("deliberation%1:07:00")
)

(define-type ont::attentiveness-scale
 :sem (F::abstr-obj (F::scale ONT::ATTENTIVENESS-SCALE))
 :parent ont::psychological-condition-scale
 :wordnet-sense-keys ("attentiveness%1:07:01")
)

(define-type ont::premeditation-scale
 :sem (F::abstr-obj (F::scale ONT::PREMEDITATION-SCALE))
 :parent ont::psychological-condition-scale
 :wordnet-sense-keys ("planning%1:09:00")
)

(define-type ont::ambitiousness-scale
 :sem (F::abstr-obj (F::scale ONT::AMBITIOUSNESS-SCALE))
 :parent ont::psychological-condition-scale
 :wordnet-sense-keys ("ambition%1:07:00")
 ;; WORDS: ambition
)

(define-type ont::cautiousness-scale
 :sem (F::abstr-obj (F::scale ONT::CAUTIOUSNESS-SCALE))
 :parent ont::psychological-condition-scale
 :wordnet-sense-keys ("caution%1:07:00")
 ;; WORDS: caution
)

(define-type ont::awareness-scale
 :sem (F::abstr-obj (F::scale ONT::AWARENESS-SCALE))
 :parent ont::psychological-condition-scale 
 :wordnet-sense-keys ("consciousness%1:09:01" "light%1:09:02")
 ;; WORDS: awareness, consciousness
)


;; certainty
(define-type ont::certainty-scale
 :sem (F::abstr-obj (F::scale ONT::CERTAINTY-SCALE))
 :parent ont::psychological-condition-scale 
)

(define-type ont::certain-scale
 :sem (F::abstr-obj (F::scale ONT::CERTAIN-SCALE))
 :parent ont::certainty-scale
 :wordnet-sense-keys ("certainty%1:09:00" "assurance%1:09:00" "certainty%1:07:00")
)

(define-type ont::not-certain-scale
 :sem (F::abstr-obj (F::scale ONT::NOT-CERTAIN-SCALE))
 :parent ont::certainty-scale
 :wordnet-sense-keys ("doubt%1:09:00" "uncertainty%1:07:00")
)

;; rationality
(define-type  ont::rationality-scale
 :sem (F::abstr-obj (F::scale ONT::RATIONALITY-SCALE))
 :parent ont::psychological-condition-scale
)

(define-type ont::not-rational-scale
 :sem (F::abstr-obj (F::scale ONT::NOT-RATIONAL-SCALE))
 :parent ont::rationality-scale
 :wordnet-sense-keys ("irrationality%1:26:00")
)

(define-type ont::craziness-scale
 :sem (F::abstr-obj (F::scale ONT::CRAZINESS-SCALE))
 :parent ont::not-rational-scale 
 :wordnet-sense-keys ("craziness%1:26:00")
)

(define-type ont::rational-scale
 :sem (F::abstr-obj (F::scale ONT::RATIONAL-SCALE))
 :parent ont::rationality-scale 
 :wordnet-sense-keys ("sanity%1:26:00" "rationality%1:26:00")
)

;;; EXPERIENCER CONDITION SCALE
(define-type ont::experiencer-condition-scale
 :sem (F::abstr-obj (F::scale ONT::EXPERIENCER-CONDITION-SCALE))
 :parent ont::psychological-condition-scale
)

;; confidence (AUTHORITY doesn't seem to fit. not sure where else it could go though. Leaving it in for now)
(define-type ont::confidence-scale
 :sem (F::abstr-obj (F::scale ONT::CONFIDENCE-SCALE))
 :parent ont::experiencer-condition-scale 
 :wordnet-sense-keys ("trust%1:26:00" "authority%1:07:00" "confidence%1:26:02" "confidence%1:12:00") 
 ;; WORDS: confidence, trust
 ;; not about certainty but more about hope and trust in someone (I have confidence in you; You have my trust.).
)

(define-type ont::interestedness-scale
 :sem (F::abstr-obj (F::scale ONT::INTERESTEDNESS-SCALE))
 :parent ont::experiencer-condition-scale
 :wordnet-sense-keys ("interestedness%1:09:00" "enthusiasm%1:09:00")
)

(define-type ont::apathy-scale
 :sem (F::abstr-obj (F::scale ONT::APATHY-SCALE))
 :parent ont::experiencer-condition-scale
 :wordnet-sense-keys ("apathy%1:07:01")
)

(define-type ont::desirability-scale ;desire-scale
 :sem (F::abstr-obj (F::scale ONT::DESIRABILITY-SCALE)) ;DESIRE-SCALE))
 :parent ont::evaluation-scale ;experiencer-condition-scale
 :wordnet-sense-keys ("desirability%1:07:00" "desire%1:26:00")
)

(define-type ont::pleasurability-scale
 :sem (F::abstr-obj (F::scale ONT::PLEASURABILITY-SCALE))
 :parent ont::experiencer-condition-scale
 :wordnet-sense-keys ("pleasure%1:12:00")
)

(define-type ont::loneliness-scale
 :sem (F::abstr-obj (F::scale ONT::LONELINESS-SCALE))
 :parent ont::experiencer-condition-scale 
 :wordnet-sense-keys ("loneliness%1:26:00" "loneliness%1:12:00")
 ;; WORDS: loneliness
)

(define-type ont::nervousness-scale
 :sem (F::abstr-obj (F::scale ONT::NERVOUSNESS-SCALE))
 :parent ont::experiencer-condition-scale 
 :wordnet-sense-keys ("nervousness%1:12:00" "anxiety%1:12:00")
 ;; WORDS: nervousness
)

(define-type ont::sadness-scale
 :sem (F::abstr-obj (F::scale ONT::SADNESS-SCALE))
 :parent ont::experiencer-condition-scale 
 :wordnet-sense-keys ("sadness%1:12:00" "grief%1:12:00" "sorrow%1:12:00" "sorrow%1:09:00")
 ;; WORDS: sadness
)

(define-type ont::regret-scale
 :sem (F::abstr-obj (F::scale ONT::REGRET-SCALE))
 :parent ont::sadness-scale
 :wordnet-sense-keys ("regret%1:12:00")
)

;; grief-scale joined with sadness-scale
;(define-type ont::grief-scale
; :parent ont::negative-emotion-scale
; :wordnet-sense-keys ("grief%1:12:00" "sorrow%1:12:00" "sorrow%1:09:00")
; ;; WORDS: heartbreak, heartache, (also grief, brokenheartedness)
;)

(define-type ont::bother-scale
 :sem (F::abstr-obj (F::scale ONT::BOTHER-SCALE))
 :parent ont::experiencer-condition-scale
 :wordnet-sense-keys("bother%1:09:00")
)

(define-type ont::worry-concern-scale
 :sem (F::abstr-obj (F::scale ONT::WORRY-CONCERN-SCALE))
 :parent ont::experiencer-condition-scale
 :wordnet-sense-keys("worry%1:09:00")
)

(define-type ont::anger-scale
 :sem (F::abstr-obj (F::scale ONT::ANGER-SCALE))
 :parent ont::experiencer-condition-scale
 :wordnet-sense-keys ("anger%1:26:00" "anger%1:12:00")
 )

(define-type ont::resentfulness-scale
 :sem (F::abstr-obj (F::scale ONT::RESENTFULNESS-SCALE))
 :parent ont::experiencer-condition-scale
 )


(define-type ont::fear-scale
 :sem (F::abstr-obj (F::scale ONT::FEAR-SCALE))
 :parent ont::experiencer-condition-scale
 :wordnet-sense-keys ("fear%1:12:00")
)

(define-type ont::disgust-scale
 :sem (F::abstr-obj (F::scale ONT::DISGUST-SCALE))
 :parent ont::experiencer-condition-scale
 :wordnet-sense-keys ("disgust%1:12:00")
)

(define-type ont::envy-scale
 :sem (F::abstr-obj (F::scale ONT::ENVY-SCALE))
 :parent ont::experiencer-condition-scale
 :wordnet-sense-keys ("envy%1:12:00")
)

(define-type ont::happiness-scale
 :sem (F::abstr-obj (F::scale ONT::HAPPINESS-SCALE))
 :parent ont::experiencer-condition-scale 
 :wordnet-sense-keys ("happiness%1:26:00" "happiness%1:12:00")
 ;; WORDS: happiness
)

(define-type ont::excitement-scale
 :sem (F::abstr-obj (F::scale ONT::EXCITEMENT-SCALE))
 :parent ont::experiencer-condition-scale 
 :wordnet-sense-keys ("excitement%1:12:00")
 ;; WORDS: happiness
)

(define-type ont::calmness-scale
 :sem (F::abstr-obj (F::scale ONT::CALMNESS-SCALE))
 :parent ont::experiencer-condition-scale
 ; considered having calm be mapped to negative orientation on excitement
 ; but the other end of the scale to calm isn't just excitement but can also include
 ; anger, frustration, annoyance etc. 
 :wordnet-sense-keys ("calmness%1:12:00")
)

(define-type ont::gratitude-scale
 :sem (F::abstr-obj (F::scale ONT::GRATITUDE-SCALE))
 :parent ont::experiencer-condition-scale
 :wordnet-sense-keys ("gratitude%1:12:00")
)

(define-type ont::relief-scale
 :sem (F::abstr-obj (F::scale ONT::RELIEF-SCALE))
 :parent ont::experiencer-condition-scale
 :wordnet-sense-keys ("relief%1:12:00")
)

(define-type ont::surprise-scale
 :sem (F::abstr-obj (F::scale ONT::SURPRISE-SCALE))
 :parent ont::experiencer-condition-scale
 :wordnet-sense-keys ("surprise%1:12:00")
)

(define-type ont::pleasantness-scale
 :sem (F::abstr-obj (F::scale ONT::PLEASANTNESS-SCALE))
 :parent ont::experiencer-condition-scale 
 :wordnet-sense-keys ("agreeableness%1:07:00" "pleasantness%1:07:00")
 ;; WORDS: amenity, pleasantness
)

(define-type ont::pride-scale
 :sem (F::abstr-obj (F::scale ONT::PRIDE-SCALE))
 :parent ont::experiencer-condition-scale 
 :wordnet-sense-keys ("dignity%1:07:00" "pride%1:12:00")
 ;; WORDS: pride
)

;; confusion
(define-type ont::confusion-scale
 :sem (F::abstr-obj (F::scale ONT::CONFUSION-SCALE))
 :wordnet-sense-keys ("confusion%1:09:00" "confusion%1:12:00")
 :parent ont::experiencer-condition-scale
 )

;; stress
(define-type ont::stress-scale
 :sem (F::abstr-obj (F::scale ONT::STRESS-SCALE))
 :wordnet-sense-keys ("stress%1:26:01")
 :parent ont::experiencer-condition-scale
 )

; for distress
(define-type ONT::distress-scale
 :sem (F::abstr-obj (F::scale ONT::DISTRESS-SCALE))
 :parent ONT::experiencer-condition-scale
 :wordnet-sense-keys ("distress%1:26:00" "distress%1:12:02")
 )



;; STATE OF AFFAIRS SCALE
(define-type ont::state-of-affairs-scale
 :sem (F::abstr-obj (F::scale ONT::STATE-OF-AFFAIRS-SCALE))
 :parent ont::ordered-domain 
)

(define-type ont::balance-scale
 :sem (F::abstr-obj (F::scale ONT::BALANCE-SCALE))
 :parent ont::state-of-affairs-scale 
 :wordnet-sense-keys ("balance%1:26:00")
 ;; WORDS: balance
)

(define-type ont::connectivity-scale
 :sem (F::abstr-obj (F::scale ONT::CONNECTIVITY-SCALE))
 :parent ont::state-of-affairs-scale 
 :wordnet-sense-keys ("connectivity%1:07:00")
 ;; WORDS: connectivity
)

(define-type ont::temporal-scale
 :sem (F::abstr-obj (F::scale ONT::TEMPORAL-SCALE))
 :parent ont::ordered-domain
)

;; time measure scale
(define-type ont::time-measure-scale
  :sem (F::Abstr-obj (F::Scale Ont::time-measure-scale))
  :parent ont::temporal-scale
)

(define-type ont::duration-scale
  :sem (F::abstr-obj (F::scale ONT::DURATION-SCALE))
  :parent ont::time-measure-scale
)

(define-type ont::age-scale
  :sem (F::abstr-obj (F::scale ONT::AGE-SCALE))
  ;:parent ont::time-measure-scale
  :parent ont::duration-scale
  :wordnet-sense-keys ("age%1:28:00" "age%1:07:00")
)

(define-type ont::time-loc-scale
 :sem (F::abstr-obj (F::scale ONT::TIME-LOC-SCALE))
  :parent ont::time-measure-scale
)

;; temporal occurrence
(define-type ont::temporal-occurrence-scale
  :sem (F::abstr-obj (F::scale ONT::TEMPORAL-OCCURRENCE-SCALE))
  :parent ont::temporal-scale ;process-property-scale
  ;; :wordnet-sense-keys ("incidence%1:24:00")
)

#| 
(define-type ont::regularity-scale
  :sem (F::abstr-obj (F::scale ONT::REGULARITY-SCALE))
  :parent ont::temporal-occurrence-scale
  ;; WORDS: irregularity, regularity
)

 
(define-type ont::regular-scale
  :sem (F::abstr-obj (F::scale ONT::REGULAR-SCALE))
  :parent ont::regularity-scale
  :wordnet-sense-keys ("regularity%1:07:00")
  ;; regularity
)
 
(define-type ont::not-regular-scale
  :sem (F::abstr-obj (F::scale ONT::NOT-REGULAR-SCALE))
  :parent ont::regularity-scale
  :wordnet-sense-keys ("intermittence%1:07:00" "irregularity%1:07:00")
  ;; irregularity
)
|#

;;;;;; UNORDERED DISCRETE DOMAIN
(define-type ont::unordered-domain
 :sem (F::abstr-obj (F::scale ONT::UNORDERED-DOMAIN))
 :parent ont::domain 
)

(define-type ont::quantity-related-scale
 :sem (F::abstr-obj (F::scale ONT::QUANTITY-RELATED-SCALE))
 :parent ont::unordered-domain
 ;:wordnet-sense-keys ("")
)

(define-type ont::gender-scale
 :sem (F::abstr-obj (F::scale ONT::GENDER-SCALE))
    :parent ont::unordered-domain
    :arguments ((:REQUIRED ONT::FIGURE (F::Phys-obj))
		)
    :wordnet-sense-keys ("gender%1:07:00")
    ;; WORDS: gender, sex
    )

#|
(define-type ont::truth-scale
 :sem (F::abstr-obj (F::scale ONT::TRUTH-SCALE))
 :parent ont::unordered-domain 
 :wordnet-sense-keys ("truth%1:26:00")
 ;; WORDS: truth
)
|#

(define-type ont::discrete-color-scale
 :sem (F::abstr-obj (F::scale ONT::DISCRETE-COLOR-SCALE))
 :parent ont::unordered-domain
)

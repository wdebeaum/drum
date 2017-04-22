(in-package :om)

(define-type ONT::SOUND-UNIT
 :parent ONT::UNIT
 :sem (F::abstr-obj)
)

#|
;We may want to move ONT::ACOUSTIC-UNIT from under ONT::FORMAL-UNIT to under ONT::SOUND-UNIT.
(define-type ONT::ACOUSTIC-UNIT
 :parent ONT::SOUND-UNIT
 :sem (F::abstr-obj)
)
|#

(define-type ONT::PITCH-UNIT
 :parent ONT::SOUND-UNIT
 :sem (F::abstr-obj)
)

;semitone, half step
(define-type ONT::SEMITONE
 :parent ONT::PITCH-UNIT
 :sem (F::abstr-obj)
 :wordnet-sense-keys ("half_step%1:10:00")
)

(define-type ONT::WHOLE-STEP
 :parent ONT::PITCH-UNIT
 :sem (F::abstr-obj)
 :wordnet-sense-keys ("step%1:10:00")
)

;ONT::MUSIC is already there in the ontology.

(define-type ONT::COMPOSITION-TYPE
 :parent ONT::MUSIC
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("musical_style%1:10:00")
)

#|
(define-type ONT::INSTRUMENTAL-MUSIC-PIECE
 :parent ONT::COMPOSITION-TYPE
 :sem (F::abstr-obj (F::SCALE -))
)
|#

;ONT::SONG as a lexical entry is already there, but its parent ont should be changed to ont::composition-type from ont::music.

(define-type ONT::COMPOSITION-ELEMENT
 :parent ONT::MUSIC
 :sem (F::abstr-obj (F::SCALE -))
)

(define-type ONT::BAR-MEASURE
 :parent ONT::COMPOSITION-ELEMENT
 :sem (F::abstr-obj)
 :arguments ((:ESSENTIAL ONT::FIGURE ((? t F::PHYS-OBJ F::ABSTR-OBJ))))
 :wordnet-sense-keys ("measure%1:10:00")
)

;beat, rhythm
(define-type ONT::BEAT
 :parent ONT::COMPOSITION-ELEMENT
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("beat%1:10:00")
)

(define-type ONT::CHORD
 :parent ONT::COMPOSITION-ELEMENT
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("chord%1:10:00")
)

;Add a musical sense for lexical entry ONT::SEQUENCE---maybe not needed
;If we have ONT::MUSIC-SEQUENCE though, a rhythmic sequence, modified sequence, false sequence, modulating sequence are examples of that, though they all can be interpreted compositionally if we have a generic ONT::SEQUENCE.
#| 
(define-type ONT::MUSIC-SEQUENCE
 :parent ONT::COMPOSITION-ELEMENT
 :sem (F::abstr-obj (F::SCALE -))
)
|#

;add a lexical entry for cadential-6-4
#|
(define-type ONT::CADENTIAL-6-4
 :parent ONT::MUSIC-SEQUENCE
 :sem (F::abstr-obj (F::SCALE -))
)
|#

(define-type ONT::KEY
 :parent ONT::COMPOSITION-ELEMENT
 :sem (F::abstr-obj)
 :wordnet-sense-keys ("key%1:10:00")
)

(define-type ONT::LYRIC
 :parent ONT::COMPOSITION-ELEMENT
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("lyric%1:10:01")
)

(define-type ONT::MELODY
 :parent ONT::COMPOSITION-ELEMENT
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("melody%1:09:00")
)

(define-type ONT::MUSIC-MOVEMENT
;There is ONT::MOVE in the ontology but I think this is a specific sense so I added this separately.
 :parent ONT::COMPOSITION-ELEMENT
)

(define-type ONT::NOTE-PITCH
 :parent ONT::COMPOSITION-ELEMENT
 :sem (F::abstr-obj)
 :wordnet-sense-keys ("note%1:10:04" "pitch%1:07:00")
;QUESTION: Does this sense of pitch fit here?
;Will: closest sense that WN has. the fact that it's a property might cause trouble with the way it's used in music, but let's wait and see. 
)

(define-type ONT::ACCIDENTAL
 :parent ONT::NOTE-PITCH
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("accidental%1:10:00")
)
#|
(define-type ONT::FLAT-PITCH
 :parent ONT::ACCIDENTAL
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("flat%1:10:00")
)

;ONT::b will be added as a lexical entry with parent ONT::FLAT.

(define-type ONT::SHARP-PITCH
 :parent ONT::ACCIDENTAL
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("sharp%1:10:00")
)

;ONT::# will be added as a lexical entry with parent ONT::SHARP.

(define-type ONT::NATURAL-PITCH
 :parent ONT::ACCIDENTAL
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("natural%1:10:00")
)
|#

;ONT::MIDIPITCH will be added as a lexical entry with parent ONT::NOTE-PITCH.

(define-type ONT::PITCH-INTERVAL
 :parent ONT::COMPOSITION-ELEMENT
 :sem (F::abstr-obj)
 :wordnet-sense-keys ("musical_interval%1:10:00")
;QUESTION: Does this sense of pitch-interval from WordNet fit here?
 )

(define-type ONT::OCTAVE
 :parent ONT::PITCH-INTERVAL
 :sem (F::abstr-obj)
 :wordnet-sense-keys ("octave%1:10:00")
)

(define-type ONT::MUSIC-SCALE
 :parent ONT::COMPOSITION-ELEMENT
 :sem (F::abstr-obj)
 :wordnet-sense-keys ("scale%1:10:00")
)

(define-type ONT::MUSICAL-DOCUMENT
 :parent ONT::INFO-MEDIUM
 :sem (F::Phys-obj)
 :wordnet-sense-keys ("sheet_music%1:10:00")

)


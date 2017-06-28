(in-package :om)

(define-type ONT::PITCH-SCALE
 :parent ONT::PHYS-MEASURE-DOMAIN
 :sem (F::Abstr-obj (F::Scale ONT::pitch-scale))
 :arguments ((:ESSENTIAL ONT::EXTENT (F::abstr-obj (F::scale ont::pitch-scale) (F::measure-function F::value)))
             )
 )

(define-type ONT::SOUND-UNIT
 :parent ONT::MEASURE-UNIT
 :sem (F::abstr-obj)
 :arguments ((:ESSENTIAL ONT::FIGURE ((? t F::ABSTR-OBJ)))) ; music
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
 :sem (F::Abstr-obj (F::scale ONT::pitch-scale))
)

(define-type ONT::NOTE-LENGTH-UNIT
 :parent ONT::SOUND-UNIT
 :wordnet-sense-keys ("whole_note%1:10:00")
)

;semitone, half step
(define-type ONT::HALF-STEP
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

(define-type ONT::MUSIC-COMPOSITION-TYPE
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

(define-type ONT::MUSIC-COMPOSITION-ELEMENT
 :parent ONT::MUSIC
 :sem (F::abstr-obj (F::SCALE -))
)

(define-type ONT::BAR-MEASURE
 :parent ONT::MUSIC-COMPOSITION-ELEMENT
 :sem (F::abstr-obj)
 :arguments ((:ESSENTIAL ONT::FIGURE ((? t F::PHYS-OBJ F::ABSTR-OBJ))))
 :wordnet-sense-keys ("measure%1:10:00")
)

;beat, rhythm
(define-type ONT::BEAT
 :parent ONT::MUSIC-COMPOSITION-ELEMENT
 :arguments ((:ESSENTIAL ONT::FIGURE ((? t F::PHYS-OBJ F::ABSTR-OBJ))))
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("beat%1:10:00")
)

(define-type ONT::CHORD
 :parent ONT::MUSIC-COMPOSITION-ELEMENT
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("chord%1:10:00")
)

(define-type ONT::KEY
 :parent ONT::MUSIC-COMPOSITION-ELEMENT
 :sem (F::abstr-obj)
 :wordnet-sense-keys ("key%1:10:00")
)

(define-type ONT::PITCH
 :parent ONT::MUSIC-COMPOSITION-ELEMENT
 :sem (F::abstr-obj)
 :wordnet-sense-keys ("note%1:10:04" "pitch%1:07:00")
)

(define-type ONT::ACCIDENTAL
 :parent ONT::MUSIC-COMPOSITION-ELEMENT
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("accidental%1:10:00")
)

(define-type ONT::PITCH-INTERVAL
 :parent ONT::MUSIC-COMPOSITION-ELEMENT
 :sem (F::abstr-obj)
 :wordnet-sense-keys ("musical_interval%1:10:00")
 )

(define-type ONT::OCTAVE
 ;:parent ONT::PITCH-INTERVAL
  :parent ONT::PITCH-UNIT
  :sem (F::abstr-obj)
 :wordnet-sense-keys ("octave%1:10:00")
)

(define-type ONT::MUSIC-SCALE
 :parent ONT::MUSIC-COMPOSITION-ELEMENT
 :sem (F::abstr-obj)
 :wordnet-sense-keys ("scale%1:10:00")
)

(define-type ONT::MUSICAL-DOCUMENT
 :parent ONT::INFO-MEDIUM
 :sem (F::Phys-obj)
 :wordnet-sense-keys ("sheet_music%1:10:00")
)

(define-type ONT::LYRIC
 :parent ONT::MUSIC
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("lyric%1:10:01")
)

(define-type ONT::MELODY
 :parent ONT::MUSIC
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("melody%1:09:00")
)

(define-type ONT::MUSIC-MOVEMENT
 :wordnet-sense-keys ("movement%1:10:00")
 :parent ONT::MUSIC
)

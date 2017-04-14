(in-package :om)

(define-type ONT::SOUND-UNIT
 :parent ONT::UNIT
 :sem (F::abstr-obj (F::SCALE -))
)

(define-type ONT::ACOUSTIC-UNIT
 :parent ONT::SOUND-UNIT
 :sem (F::abstr-obj (F::SCALE -))
)

(define-type ONT::PITCH-UNIT
 :parent ONT::SOUND-UNIT
 :sem (F::abstr-obj (F::SCALE -))
)

;semitone, half step
(define-type ONT::SEMITONE
 :parent ONT::PITCH-UNIT
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("half_step%1:10:00")
)

(define-type ONT::OCTAVE
 :parent ONT::PITCH-UNIT
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("octave%1:10:00")
)

(define-type ONT::UNISON
 :parent ONT::PITCH-UNIT
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("unison%1:07:01")
)

(define-type ONT::WHOLE-STEP
 :parent ONT::PITCH-UNIT
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("step%1:10:00")
)

;ONT::MUSIC is already there in the ontology.

(define-type ONT::COMPOSITION-TYPE
 :parent ONT::MUSIC
 :sem (F::abstr-obj (F::SCALE -))
)

(define-type ONT::INSTRUMENTAL-MUSIC-PIECE
 :parent ONT::COMPOSITION-TYPE
 :sem (F::abstr-obj (F::SCALE -))
)

;ONT::SONG as a lexical entry is already there, but its parent ont should be changed to ont::composition-type from ont::music.

(define-type ONT::COMPOSITION-ELEMENTS
 :parent ONT::MUSIC
 :sem (F::abstr-obj (F::SCALE -))
)

(define-type ONT::BAR/MEASURE
 :parent ONT::COMPOSITION-ELEMENTS
 :sem (F::abstr-obj)
 :wordnet-sense-keys ("measure%1:10:00")
)

;beat, rhythm
(define-type ONT::BEAT
 :parent ONT::BAR/MEASURE
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("beat%1:10:00")
)

(define-type ONT::CHORD
 :parent ONT::COMPOSITION-ELEMENTS
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("chord%1:10:00")
)

(define-type ONT::CHORD-PROGRESSION
 :parent ONT::COMPOSITION-ELEMENTS
 :sem (F::abstr-obj (F::SCALE -))
)

(define-type ONT::CADENTIAL 6/4
 :parent ONT::CHORD-PROGRESSION
 :sem (F::abstr-obj (F::SCALE -))
)

(define-type ONT::LYRICS
 :parent ONT::COMPOSITION-ELEMENTS
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("lyric%1:10:01")
)

(define-type ONT::MELODY
 :parent ONT::COMPOSITION-ELEMENTS
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("melody%1:09:00")
)

(define-type ONT::MOVEMENT
;There is ONT::MOVE in the ontology but I think this is a specific sense so I added this separately.
 :parent ONT::COMPOSITION-ELEMENTS
 :sem (F::abstr-ob)
)

(define-type ONT::NOTE/PITCH
 :parent ONT::COMPOSITION-ELEMENTS
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("note%1:10:04" "pitch%1:07:00")
;QUESTION: Does this sense of pitch fit here?
)

(define-type ONT::ACCIDENTAL
 :parent ONT::NOTE/PITCH
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("accidental%1:10:00")
;QUESTION: Is this the right sense?
)

(define-type ONT::FLAT
 :parent ONT::ACCIDENTAL
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("flat%1:10:00")
)

;ONT::b will be added as a lexical entry with parent ONT::FLAT.

(define-type ONT::SHARP
 :parent ONT::ACCIDENTAL
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("sharp%1:10:00")
)

;ONT::# will be added as a lexical entry with parent ONT::SHARP.

(define-type ONT::NATURAL
 :parent ONT::ACCIDENTAL
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("natural%1:10:00")
)

;ONT::MIDIPITCH will be added as a lexical entry with parent ONT::NOTE/PITCH.

(define-type ONT::SCALE
 :parent ONT::COMPOSITION-ELEMENTS
 :sem (F::abstr-obj)
 :wordnet-sense-keys ("scale%1:10:00")
)

(define-type ONT::NOTATION
 :parent ONT::MUSIC
 :sem (F::abstr-obj (F::SCALE -))
;This seems a general sense of notation, just used in the music domain, however the lexical entry we have in the ontology only has ONT::WRITE sense, is that enough or do we want a specialized sense? I think we should have this specialized ontology entry.
)

(define-type ONT::SCORE
 :parent ONT::NOTATION
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("score%1:10:00")
)

(define-type ONT::ACCELERADO
 :parent ONT::NOTATION
 :sem (F::abstr-obj (F::SCALE -))
)

(define-type ONT::LEADSHEET
 :parent ONT::NOTATION
 :sem (F::abstr-obj (F::SCALE -))
 :comment "basic outline"
)

;ONT::SCORE should have another sense related to music in the ontology.

(define-type ONT::SHEET-MUSIC
 :parent ONT::NOTATION
 :sem (F::abstr-obj (F::SCALE -))
 :wordnet-sense-keys ("sheet_music%1:10:00")
)
 

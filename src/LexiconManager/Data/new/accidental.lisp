;;;;
;;;; W::ACCIDENTAL
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::ACCIDENTAL
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20040915 :change-date nil :wn ("accidental%5:00:00:unintended:00") :comments caloy2)
     (example "an accidental meeting")
     (LF-PARENT ONT::not-intentional-val)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
;not sure about the template
;a musical notation that makes a note sharp or flat or natural although that is not part of the key signature
 :words (
  (W::ACCIDENTAL
   (SENSES
    ((LF-PARENT ONT::ACCIDENTAL)
     (example "the accidental alters the pitch of the note")
     (meta-data :wn ("accidental%1:10:00"))
     )
    )
   )
))



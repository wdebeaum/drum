;;;;
;;;; W::LONG
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::LONG
   (wordfeats (W::MORPH (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("long%3:00:01"))
     (EXAMPLE "a long line")
;     (LF-PARENT ONT::linear-dimension)
     (LF-PARENT ONT::LONG)
     )
;    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("long%3:00:01"))
;     (EXAMPLE "a 5 feet long line")
;     (LF-PARENT ONT::Linear-D)
;     (TEMPL ADJ-PREMOD-TEMPL)
;     (PREFERENCE 0.98) ;;use the no-premod meaning first
;     )
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("long%3:00:02"))
     (LF-PARENT ONT::event-duration-modifier)
     (example "how long did the meeting last" "a long meeting/conversation" "long term parking")
     )
    ;; this overgenerates
    ((meta-data :origin cardiac :entry-date 20090416 :change-date nil :comments nil :wn ("long%3:00:02"))
     (LF-PARENT ONT::event-duration-modifier)
     (example "all night long" "the whole week long")   
     (TEMPL postpositive-adj-optional-xp-templ)
     )
    )
   )
))


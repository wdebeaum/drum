;;;;
;;;; W::flush
;;;;

(define-words :pos W::v 
 :words (
  (W::flush
    (wordfeats (W::morph (:forms (-vb) :nom w::flush)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060608 :change-date 20090528 :comments nil :vn ("hiccup-40.1.1" "wipe_manner-10.4.1") :wn ("flush%2:29:00" "flush%2:30:01" "flush%2:30:02" "flush%2:30:03"))
     (LF-PARENT ONT::push-out-of)
     ;;(TEMPL agent-affected-source-optional-templ)
     (TEMPL agent-affected-xp-templ)
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("sweat%2:29:00"))
     (LF-PARENT ONT::bodily-process)
     (example "he was flushed")
     (TEMPL agent-templ) ; like bleed
     )
    )
   )
))

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::flush
   (SENSES
    ((LF-PARENT ONT::adjacent)
     (meta-data :origin fruitcarts :entry-date 20060215 :change-date nil :wn ("flush%5:00:00:even:01") :comments nil)
     (EXAMPLE "move it so it's flush with the side of the square")
     (TEMPL adj-theme-templ)
     )
    )
   )
))


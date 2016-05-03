;;;;
;;;; W::guide
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::guide
   (SENSES
    ((LF-PARENT ONT::reference-work)
     (EXAMPLE "see the pages user's guide")
     (meta-data :origin task-learning :entry-date 20050923 :change-date nil :wn ("guide%1:10:00") :comments nil)
     )
    ((LF-PARENT ONT::graphic-symbol)
     (EXAMPLE "Alignment guides will appear when the center or edges of objects are vertically or horizontally aligned")
     (meta-data :origin task-learning :entry-date 20050923 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (w::guide
   (senses
    ((meta-data :origin coordops :entry-date 20070511 :change-date nil :comments nil :vn ("judgement-33"))
     (LF-PARENT ont::guiding)
     (example "team alpha will guide the activity")
     (TEMPL agent-theme-xp-templ)
     )
    )
   )
))


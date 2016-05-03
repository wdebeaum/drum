;;;;
;;;; W::border
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::border
   (SENSES
    ((LF-PARENT ONT::edge)
     (EXAMPLE "The border of the selected cell is highlighted")
     (meta-data :origin task-learning :entry-date 20050812 :change-date nil :wn ("border%1:25:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(W::border
  (wordfeats (W::morph (:forms (-vb) :past W::bordered :ing w::bordering)))
   (SENSES
    ((LF-PARENT ONT::surround)
     (TEMPL neutral-neutral-xp-templ)
     (example "the fence borders the meadow")
     (meta-data :origin fruitcarts :entry-date 20060215 :change-date nil :comments nil)
     )
    )
   )
))


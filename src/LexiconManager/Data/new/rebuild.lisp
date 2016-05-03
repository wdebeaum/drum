;;;;
;;;; W::rebuild
;;;;

(define-words :pos W::v :templ agent-affected-create-templ
 :words (
  (W::rebuild
   (wordfeats (W::morph (:forms (-vb) :past W::rebuilt)))
   (SENSES
    ((LF-PARENT ONT::CREATE)
     (EXAMPLE "the file is rebuilt using data in the mbox file")
     (meta-data :origin task-learning :entry-date 20050825 :change-date nil :comments nil)
     )
    )
   )
))


;;;;
;;;; W::PROJECT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::PROJECT
   (SENSES
    ((meta-data :origin calo :entry-date 20040113 :change-date 20050817 :wn ("project%1:09:00") :comments lf-restructuring)
     (LF-PARENT ONT::activity)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::project
   (wordfeats (W::morph (:forms (-vb) :nom W::projection)))
   (SENSES
    ((meta-data :origin cause-result-relations :entry-date 20180310 :change-date nil :wn ("project%2:39:01"))
     (LF-PARENT ONT::visual-display)
     (example "project an image on screen")
     )
   )
)))

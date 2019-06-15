;;;;
;;;; W::extract
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (W::extract
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090408 :change-date nil :comments nil)
     (LF-PARENT ONT::liquid-substance)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
(W::extract
   (wordfeats (W::morph (:forms (-vb) :nom W::extraction)))
   (senses
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090527 :comments nil :vn ("remove-10.1") :wn ("extract%2:30:00" "extract%2:31:00" "extract%2:35:04" "extract%2:35:05" "extract%2:35:07" "extract%2:40:00"))
     (LF-PARENT ONT::cause-out-of)
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL)
     )
        )
   )))


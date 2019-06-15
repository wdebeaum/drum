;;;;
;;;; W::spread
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::spread
   (SENSES
    ((LF-PARENT ONT::info-holder) ; like format
     (EXAMPLE "adjust the outside and inside margins for two-page spreads")
     (meta-data :origin task-learning :entry-date 20051003 :change-date 20051021 :wn ("spread%1:10:00") :comments nil)
     (preference .96) ;;prefer verb sense
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::spread
   (wordfeats (W::morph (:forms (-vb) :past W::spread :ing W::spreading :nom w::spread)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1") :wn ("spread%2:35:13"))
     (LF-PARENT ONT::apply-on-surface)
 ; like spray
     (example "he spread the peanut butter on the bread")
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1"))
     (LF-PARENT ONT::disperse)
     (example "electricity distribution spread to farmers and country towns" "windmill usage spread across the country")
     (TEMPL affected-TEMPL)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1"))
     (LF-PARENT ONT::disperse)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))


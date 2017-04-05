;;;;
;;;; W::scan
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
 ;; changed from nominalization since we need the physical representation sense as well
  (W::scan
   (SENSES
    ((meta-data :origin cernl :entry-date 20110312)
     (LF-PARENT ONT::IMAGE)
     (example "the scan showed the nodes")
     (TEMPL OTHER-RELN-TEMPL)
     )
  #||  ((meta-data :origin cernl :entry-date 20110312)
     (LF-PARENT ONT::physical-scrutiny)
     (example "he did a scan") ; activity sense
     (TEMPL OTHER-RELN-theme-TEMPL)
     )||#
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(W::scan
  (wordfeats (W::morph (:forms (-vb) :nom w::scan)))
 ;;(wordfeats (W::morph (:forms (-vb))))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("sight-30.2" "investigate-35.4") :wn ("scan%2:31:01" "scan%2:39:00" "scan%2:39:01"))
     (LF-PARENT ONT::physical-scrutiny)
     (example "he scanned the area")
     (TEMPL agent-theme-xp-templ) ; like explore,investigate,examine,test,survey,inspect,regard
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20061005 :comments nil :vn ("sight-30.2" "investigate-35.4") :wn ("scan%2:31:01" "scan%2:39:00" "scan%2:39:01"))
     (LF-PARENT ONT::physical-scrutiny)
     (example "he scanned along the horizon for a glimpse of the ship")
     (TEMPL agent-templ) 
     (PREFERENCE 0.97)
     )
    )
   )
))


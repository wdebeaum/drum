;;;;
;;;; W::END
;;;;


(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::END
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("end%1:15:00"))
     (LF-PARENT ont::pos-end-of-trajectory);ONT::LINE-DEPENDENT-LOCATION)
     (example "the end of the line")
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    #|
    ((meta-data :origin plow :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::END)
     (example "the end of the range ")
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     ;; Myrosia lowered the preference slightly so that when "the end of the line" parses, it is given preference
     (preference 0.97)
     )
    |#
    ;; nominalizations of the verb 
;    ((LF-PARENT ONT::TIME-POINT)
;     (example "the end of the meeting/lesson")
;     (meta-data :origin newbeegle :entry-date 20050211 :change-date nil :wn ("end%1:28:00") :comments nil)
;     (TEMPL GEN-PART-OF-RELN-ACTION-TEMPL)
;     )
;    ((LF-PARENT ONT::TIME-POINT)
;     (example "the end of the year")
;     (meta-data :origin step :entry-date 20080623 :change-date nil :wn ("end%1:28:00") :comments nil)
;     (TEMPL GEN-PART-OF-RELN-INTERVAL-TEMPL)
;     )
    )
   )
))


(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::end
;   (wordfeats (W::morph (:forms (-vb) :nom w::end)))
   (SENSES
    ((EXAMPLE "He/It ended the document/meeting")
     (LF-PARENT ONT::STOP)
     (templ agent-affected-xp-templ)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin task-learning :entry-date 20050826 :change-date nil :comments nil)
     )
    ((LF-PARENT ONT::STOP)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL affected-TEMPL)
     (EXAMPLE "The meeting/story ended")
     (meta-data :origin task-learning :entry-date 20050826 :change-date nil :comments nil)
     )
    #|
    ((lf-parent ont::end-at-loc) 
     (example "the road ends at the church")
     (TEMPL neutral-location-templ)
     )
    |#
    )
   )
  
))


;;;;
;;;; W::develop
;;;;

(define-words :pos W::v 
 :tags (:base500)
 :words (
  (W::develop
   (wordfeats (W::morph (:forms (-vb) :past W::developed :ing W::developing :nom w::development :nomsubjpreps (w::of) :nomobjpreps -)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("grow-26.2") :wn ("develop%2:30:00" "develop%2:30:01" "develop%2:30:10" "develop%2:36:09"))
     (LF-PARENT ONT::grow)
     (TEMPL affected-templ) ; like grow
     ;(PREFERENCE 0.96)
     )
    #|
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("occurrence-48.3"))
     (LF-PARENT ONT::occurring)
     (example "a situation developed")
     (TEMPL neutral-templ) ; like occur,happen
     (PREFERENCE 0.98)
     )
    |#
    )
   )
))

  
(define-words :pos W::v 
 :tags (:base500)
 :words (
  (W::develop
   (wordfeats (W::morph (:forms (-vb) :past W::developed :ing W::developing :nom w::development)))
   (SENSES
    ((meta-data :origin cernl :entry-date 20110210 :change-date nil :comments ticket-244)
     ;(LF-PARENT ONT::CREATE)
     (LF-PARENT ont::incur-inherit-receive)
     (example "the patient developed a cough")
     ;(TEMPL affected-result-xp-templ)
     ;(templ AFFECTED-AFFECTED-RESULT-ARG-TEMPL)
     (TEMPL AFFECTED-AFFECTED1-XP-NP-TEMPL)
     )
    
    (
     (LF-PARENT ONT::CREATE)
     (example "We developed a theory")
     ;(TEMPL agent-affected-xp-templ)
     (TEMPL AGENT-AFFECTEDR-XP-TEMPL)
     )

    )
   )
))



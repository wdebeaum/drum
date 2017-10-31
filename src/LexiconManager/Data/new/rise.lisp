;;;;
;;;; W::rise
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::rise
   (wordfeats (W::morph (:forms (-vb) :past W::rose :pastpart W::risen :ing W::rising :nom w::rise)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("escape-51.1-2"))
;     (LF-PARENT ONT::move-upward)
     (LF-PARENT ONT::rise)
     (TEMPL affected-templ) ; like go,fall
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("assuming_position-50") :wn ("rise%2:29:08" "rise%2:38:00" "rise%2:38:01" "rise%2:38:04" "rise%2:38:05"))
     (LF-PARENT ONT::body-movement-self)
     (TEMPL agent-templ) ; like (lie
     )

    (
     (LF-PARENT ONT::increase)
     (TEMPL affected-templ) 
     )
    
    )
   )
))


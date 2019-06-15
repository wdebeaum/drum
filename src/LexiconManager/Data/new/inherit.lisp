;;;;
;;;; W::inherit
;;;;

(define-words :pos W::v 
 :words (
  (W::inherit
   (wordfeats (W::morph (:forms (-vb) :past W::inherited :ing W::inheriting :nom w::inheritance)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("obtain-13.5.2") :wn ("inherit%2:40:00"))
     
     (lf-parent ont::incur-inherit-receive) ;; 20120524 GUM change new parent
     (syntax (w::resultative +))
     (TEMPL AFFECTED-AFFECTED1-XP-NP-TEMPL )
     )
    )
   )
))


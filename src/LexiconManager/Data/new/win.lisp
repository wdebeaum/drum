;;;;
;;;; W::win
;;;;

(define-words :pos W::v 
 :words (
  (W::win
   (wordfeats (W::morph (:forms (-vb) :past W::won :ing W::winning)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("get-13.5.1") :wn ("win%2:40:00"))
     (LF-PARENT ONT::acquire)
     (TEMPL agent-affected-source-optional-templ (xp (% w::pp (w::ptype (? pt w::from))))) ; like catch
     )
    )
   )
))


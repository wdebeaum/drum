;;;;
;;;; W::grab
;;;;

(define-words :pos W::v
 :words (
  (W::grab
   (wordfeats (W::morph (:forms (-vb) :ing W::grabbing :past W::grabbed)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090430 :comments nil :vn ("obtain-13.5.2") :wn ("grab%2:35:00" "grab%2:40:00"))
     ;(LF-PARENT ONT::appropriate)
     (LF-PARENT ONT::acquire)
     (TEMPL agent-affected-source-optional-templ (xp (% w::pp (w::ptype w::from)))) ; like recover
          )
    )
   )
))


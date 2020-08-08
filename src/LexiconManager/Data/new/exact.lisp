;;;;
;;;; W::exact
;;;;

(define-words :pos W::v 
 :words (
  (W::exact
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090430 :comments nil :vn ("obtain-13.5.2") :wn ("exact%2:32:00"))
     (LF-PARENT ONT::appropriate)
     (TEMPL AGENT-AFFECTED-SOURCE-XP-TEMPL (xp (% w::pp (w::ptype w::from)))) ; like recover
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::exact
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date nil :wn ("exact%5:00:00:correct:00") :comments html-purchasing-corpus)   (SEM (F::GRADABILITY -))
     (lf-parent ont::precise)
     )
    )
   )
))


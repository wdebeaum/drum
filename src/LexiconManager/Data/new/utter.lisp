;;;;
;;;; w::utter
;;;; 

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::utter
   (SENSES
      ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090505 :comments nil :vn ("say-37.7"))
     (LF-PARENT ONT::say)
     (TEMPL agent-theme-xp-templ (xp (% w::cp (w::ctype w::s-that)))) ; like disclose
     (PREFERENCE 0.98)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::UTTER
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090731 :comments LM-vocab)
     (lf-parent ont::whole-complete) ;; like total
     )
    )
   )
))


;;;;
;;;; W::rejoice
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::rejoice
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("marvel-31.3-2"))
     (LF-PARENT ONT::appreciate)
     (TEMPL experiencer-theme-xp-templ (xp (% w::pp (w::ptype (? p w::in w::about)))))
     (example "Rejoice in the Lord")
     )
    #||((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("marvel-31.3-2"))
     (LF-PARENT ONT::experiencer-emotion)
     (TEMPL affected-templ) ; like mind
     (example "Australians, rejoice!")
     )
    ||#
   ))
))

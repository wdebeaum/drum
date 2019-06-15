;;;;
;;;; W::glom
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::glom
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("shake-22.3-2-1"))
     (LF-PARENT ONT::combine-objects)
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype (? pt w::to))))) ; like bond,splice,weld
     )
   #|| ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("shake-22.3-2-1"))
     (LF-PARENT ONT::attach)
     (TEMPL agent-affected2-templ (xp (% w::pp (w::ptype w::to)))) ; like bind
     )
   ||#
    ))
  ))
   


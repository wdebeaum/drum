;;;;
;;;; W::splice
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::splice
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("shake-22.3-2-1"))
     (LF-PARENT ONT::attach)
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype w::to)))) ; like bind,glom,graft,bond,fasten,moor,bundle
     )
   #|| ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("shake-22.3-2-1"))
     (LF-PARENT ONT::combine-objects)
     (TEMPL agent-affected2-optional-templ (xp (% w::pp (w::ptype (? pt w::to))))) ; like append
     )||#
    )
   )
))


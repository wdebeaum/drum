;;;;
;;;; W::package
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::package
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :wn ("package%1:06:00") :comments html-purchasing-corpus)
     (LF-PARENT ONT::package)
     (TEMPL pred-subcat-contents-templ)
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::package
   (SENSES
   #|| ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("shake-22.3-2-1"))
     (LF-PARENT ONT::attach)
     (TEMPL agent-affected-theme-optional-templ (xp (% w::pp (w::ptype (? pt w::to w::with))))) ; like bind,glom,graft,bond,fasten,moor,bundle
     )||#
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("shake-22.3-2-1"))
     ;(LF-PARENT ONT::combine-objects)
     (LF-PARENT ONT::cause-cover)
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype (? pt w::to w::with))))) ; like bond,splice,weld
     )
    #||((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("shake-22.3-2-1"))
     (LF-PARENT ONT::combine-objects)
     (example "package the computer with the printer")
     (TEMPL agent-affected-theme-optional-templ (xp (% w::pp (w::ptype (? pt w::with))))) ; like append
     )||#
    (;(LF-PARENT ont::combine-objects)
     (LF-PARENT ONT::cause-cover)
     (example "package the items together")
     (meta-data :origin calo-ontology :entry-date 20060214 :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    )
   )
))


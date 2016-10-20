;;;;
;;;; W::find
;;;;

(define-words :pos W::v 
 :words (
  ((W::find (W::out))
   (wordfeats (W::morph (:forms (-vb) :past W::found)))
   (SENSES
    ;;;; swier -- any of these s-that verbs can take a s[wh] complement in English...is that being captured here?
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090513 :comments nil :vn ("discover-82-1-1"))
     (EXAMPLE "I found out that he can do it")
     (LF-PARENT ONT::come-to-understand)
     (TEMPL agent-theme-xp-templ (xp (% W::cp (W::ctype (? ct W::s-that)))))
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090513 :comments nil :vn ("discover-82-1-1"))
     (EXAMPLE "I'll find out if he can do it")
     (syntax (w::exclude-passive +))
     (LF-PARENT ONT::come-to-understand)
     (TEMPL agent-theme-xp-templ (xp (% W::cp (W::ctype (? ct W::s-if)))))
     )
    
    ((EXAMPLE "She found out")
     (LF-PARENT ONT::come-to-understand)
     (TEMPL agent-templ)
     (preference .97) 
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::FIND
   (wordfeats (W::morph (:forms (-vb) :past W::found :nom w::find)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("declare-29.4-1-1-2"))
;     (LF-PARENT ONT::believe)
     (LF-PARENT ONT::come-to-understand)
     (example "I find him difficult")
     (TEMPL agent-neutral-adj-predicate-templ) 
     (PREFERENCE 0.96)
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090513 :comments nil :vn ("discover-82-1-1"))
     (EXAMPLE "i find him to be annoying")
     (LF-PARENT ONT::come-to-understand)
     (TEMPL agent-theme-objcontrol-templ)
     )
    ((EXAMPLE "I found that he can do it")
     (LF-PARENT ONT::come-to-understand)
     (TEMPL agent-theme-xp-templ (xp (% W::cp (W::ctype (? ct W::s-that)))))
     )
   
    (
;     (LF-PARENT ONT::come-to-understand)
     (LF-PARENT ONT::determine)
     (TEMPL agent-neutral-xp-templ)
     )
#|    
    ((EXAMPLE "Find a clear lane")
     (meta-data :origin lou :entry-date 20040615 :change-date nil :comments nil)
     (LF-PARENT ONT::FIND)
     (templ agent-affected-xp-templ)
     )
|#
    )
   )
))


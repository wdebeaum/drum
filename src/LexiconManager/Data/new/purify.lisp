;;;;
;;;; W::purify
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::purify
   (wordfeats (W::morph (:forms (-vb) :nom w::purification)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090601 :comments nil :vn ("cheat-10.6") :wn ("purify%2:30:00" "purify%2:30:01"))
     (LF-PARENT ONT::remove-parts)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (EXAMPLE "We copurified these proteins.")
     )
    (
     (LF-PARENT ONT::remove-parts)
     (TEMPL AFFECTED-AFFECTED1-XP-PP-TEMPL (xp (% W::pp (W::ptype (? xx w::with)))))
     (EXAMPLE "This protein copurified with that protein.")
     )
    (
     (LF-PARENT ONT::remove-parts)
     (TEMPL AFFECTED-TEMPL)
     (EXAMPLE "These proteins copurified.")
     )
    
    )
   )
))



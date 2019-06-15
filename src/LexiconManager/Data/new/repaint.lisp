;;;;
;;;; W::repaint
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::repaint
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("coloring-24"))
     (LF-PARENT ont::coloring)
     (TEMPL AGENT-AFFECTED-RESULT-XP-PP-INTO-OPTIONAL-TEMPL (xp (% w::adjp (w::set-modifier -)))) ; like lacquer,paint,varnish,tint,spraypaint,dye,color,stain,shellac,distemper,enamel
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("coloring-24"))
     (LF-PARENT ont::coloring)
 ; like color
     )
    )
   )
))


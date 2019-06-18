;;;;
;;;; W::crash
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::collide
    (wordfeats (W::morph (:forms (-vb) :nom w::crash)))
   (SENSES
    ((LF-PARENT ont::collide)
     (example "The car collided with the wall" )
     (TEMPL AFFECTED-AFFECTED1-XP-NP-TEMPL (xp (% W::PP (W::ptype (? x W::into w::with))))
     )
    ((LF-PARENT ont::collide)
     (example "The cars collided")
     (TEMPL AFFECTED-NP-PLURAL-TEMPL)
     )
     )
    )
   )))


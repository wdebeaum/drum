;;;;
;;;; W::VIA
;;;;

(define-words :pos W::ADV
 :words (
  (W::VIA
   (SENSES
     ;; ont::via is point, ont::along is strip
;    ((LF-PARENT ONT::ALONG)
;     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
;     )

    ((LF-PARENT ONT::obj-in-path) ;ont::via
    ;(TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL
            (xp (% W::NP (W::case (? cas W::obj -)) (w::gerund -) (w::refl -))))
     )

    ((LF-PARENT ONT::BY-MEANS-OF)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL
	    (xp (% W::NP (W::case (? cas W::obj -)) (w::gerund -) (w::refl -))))
     (EXAMPLE "via the activation")
     )

    ((LF-PARENT ONT::BY-MEANS-OF)
     (TEMPL BINARY-CONSTRAINT-S-SUBJCONTROL-TEMPL)
     (EXAMPLE "via activating this")
     )
    )
   )
))


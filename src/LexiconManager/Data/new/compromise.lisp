;;;;
;;;; W::compromise
;;;;

(define-words :pos W::v
 :words (
  (W::compromise
   (SENSES
    ((LF-PARENT ONT::compromise) ;accept-agree)
     (TEMPL AGENT-neutral-XP-TEMPL (xp (% W::PP (W::ptype (? xx W::to W::on w::with w::upon)))))
     (example "they decided on a compromise")
     (meta-data :wn ("compromise%2:32:01" "compromise%2:32:00"))
     )

    ((lf-parent ont::compromise) ;; 20120523 GUM change new parent
     (Example "he compromised")
     (TEMPL AGENT-AGENT1-XP-PP-OPTIONAL-TEMPL )
     )

    )
   )
))

;;;;
;;;; w::fight
;;;;

(define-words :pos W::v 
 :words (
  (w::fight
    (wordfeats (W::morph (:forms (-vb) :past W::fought :ing w::fighting)))
     (senses
      ((lf-parent ont::fighting)
       (example "he fought the proposal")
       (templ agent-neutral-xp-templ)
       (meta-data :origin step :entry-date 20080630 :change-date nil :comments nil)
       )
      ((lf-parent ont::fighting)
       (example "he fought (with/for/against them)")
       (templ agent-templ)
       (meta-data :origin step :entry-date 20080630 :change-date nil :comments nil)
       )
      ((lf-parent ont::fighting)
       (example "he fought to breath")
       (TEMPL AGENT-theme-SUBJCONTROL-TEMPL (xp (% W::cp (W::ctype W::s-to))))
       (meta-data :origin cardiac :entry-date 20080630 :change-date nil :comments nil)
       )
      ))
))


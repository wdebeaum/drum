;;;;
;;;; W::differ
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
(W::differ
   (SENSES
    ((lf-parent ont::differ)
      (example "they differ")
     (TEMPL NEUTRAL-NP-PLURAL-TEMPL)
     (meta-data :origin bolt-e :entry-date 20120516 :comments top500)
     (preference 0.97)
     )
    ((lf-parent ont::differ)
      (example "they differ in size")
     (TEMPL NEUTRAL-FORMAL-XP-NP-2-TEMPL (xp (% w::pp (w::ptype (? pptp w::in w::by)))))
     (meta-data :origin bolt-e :entry-date 20120516 :comments top500)
     )
    ))
))


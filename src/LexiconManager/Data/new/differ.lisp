;;;;
;;;; W::differ
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :tags (:base500)
 :words (
(W::differ
   (SENSES
    ((lf-parent ont::differ)
      (example "they differ")
     (TEMPL neutral-plural-templ)
     (meta-data :origin bolt-e :entry-date 20120516 :comments top500)
     (preference 0.97)
     )
    ((lf-parent ont::differ)
      (example "they differ in size")
     (templ neutral-theme-xp-templ (xp (% w::pp (w::ptype (? pptp w::in w::by)))))
     (meta-data :origin bolt-e :entry-date 20120516 :comments top500)
     )
    ))
))


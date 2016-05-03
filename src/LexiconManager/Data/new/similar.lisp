;;;;
;;;; W::SIMILAR
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::SIMILAR
   ;; can't auto-generate -ly with theme roles yet
   ;;(wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("similar%3:00:04"))
     (EXAMPLE "john is similar [to jane]")
     (LF-PARENT ONT::SIMILAR)
     (templ adj-co-theme-templ (xp (% w::pp (w::PTYPE (? pt w::in w::to)))))
     )
    )
   )
))


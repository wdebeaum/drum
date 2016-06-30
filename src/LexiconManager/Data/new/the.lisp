;;;;
;;;; W::THE
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ((W::THE W::SAME)
   (SENSES
    ((EXAMPLE "They are the same [as John's]")
     (LF-PARENT ONT::IDENTITY-VAL)
     (LF-FORM W::SAME)
     (SYNTAX (w::atype w::predicative-only))
     (TEMPL ADJ-CO-THEME-TEMPL (XP (% W::PP (W::PTYPE W::AS))))
     (preference .97) ;; reduce competition with the article
     )
    ((EXAMPLE "I want a book the same as John's")
     (LF-PARENT ONT::IDENTITY-VAL)
     (LF-FORM W::SAME)
     (SYNTAX (w::atype w::postpositive) (w::allow-deleted-comp -))
     (TEMPL ADJ-CO-THEME-TEMPL (XP (% W::PP (W::PTYPE W::AS))))
     (preference .97) ;; reduce competition with the article
     )    
    ))
))

(define-words :pos W::art :boost-word t
 :tags (:base500)
 :words (
;;;   )
  (W::THE
   (SENSES
    ((LF ONT::DEFINITE)
     (non-hierarchy-lf t)(TEMPL mass-agr-3s-TEMPL)
     )
    )
   )
))

(define-words :pos W::art :boost-word t
 :tags (:base500)
 :words (
  (W::THE
   (SENSES
    ((LF ONT::DEFINITE-PLURAL)
     (non-hierarchy-lf t)(TEMPL mass-agr-3p-TEMPL)
     )
    )
   )
))


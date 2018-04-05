;;;;
;;;; W::IT
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::IT
   (SENSES
    (;(sem (f::intentional -)) ;; can be a company, a country
     (LF-PARENT ONT::REFERENTIAL-SEM)
     )
    ;;;;; expletive it
    ((LF ONT::EXPLETIVE)
     (preference .98)
     (non-hierarchy-lf t)
     (SYNTAX ;(W::CASE W::SUB) ; OBJ in "Let it snow"
	     (W::expletive +))
     )
    )
   )
))


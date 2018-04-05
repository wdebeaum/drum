;;;;
;;;; W::OTHER
;;;;


;; this is not quite right yet -- but is needed for sentences like "are there any others"
;;   all the other cases of "other" treat it as an adjective as it should
(define-words :pos W::n :templ COUNT-PRED-TEMPL
	      :words (
		      (W::OTHERS
		       ;(wordfeats (W::agr W::3P))
		       (wordfeats (W::morph (:forms (-none))))
		       (SENSES
			((meta-data :origin monroe :entry-date 20031219 :change-date nil :comments s14)
			 (LF-PARENT ONT::other)
			 (TEMPL COUNT-PRED-3p-TEMPL)
			 )
			)
		       )
		      ))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::OTHER
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("other%3:00:00"))
     (LF-PARENT ONT::other)
     (TEMPL adj-co-theme-templ)
     (SEM (F::GRADABILITY F::-))
     (SYNTAX (W::atype W::attributive-only))
     )
    )
   )
  ))


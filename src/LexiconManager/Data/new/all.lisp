;;;;
;;;; w::all
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((w::all w::in w::one)
    (wordfeats (W::MORPH (:forms (-S-3P) :plur (w::all w::in w::ones))))
   (SENSES
    ((LF-PARENT ONT::device)
     (SEM (F::mobility F::non-self-moving))
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments concept-learning)
     (example "I want to buy an all in one")
     )
    )
   )
))

#|
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ((W::all w::inclusive)
   (SENSES
    ((meta-data :origin plow :entry-date 20060530 :change-date nil :comments pq)
     (example "an all inclusive resort")
     (LF-PARENT ONT::public-val)
     )
    )
   )
))
|#

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  ((W::ALL W::RIGHT)
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("alright%5:00:00:satisfactory:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
     (example "a good book")
     (LF-PARENT ONT::good)
     (SEM (f::orientation F::pos) (f::intensity ont::lo))
     (TEMPL central-adj-templ)
     (LF-FORM W::ALLRIGHT)	  
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("alright%5:00:00:satisfactory:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
     (example "a wall good for climbing")
     (LF-PARENT ONT::good)
     (SEM (f::orientation F::pos) (f::intensity ont::lo))
     (TEMPL adj-purpose-TEMPL (XP (% W::PP (W::PTYPE W::FOR))))
     (LF-FORM W::ALLRIGHT)	  
     )
;    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("alright%5:00:00:satisfactory:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
;     (EXAMPLE "a drug suitable for cancer")
;     (LF-PARENT ONT::good)
;     (SEM (f::orientation F::pos) (f::intensity ont::lo))
;     ;; this is a sense that allows for implicit/indirect senses of "for"
;     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
;     ;; the adj-purpose-implicit-templ is for indirect purposes, such as "this is good for cancer" where one has to infer that the actual use is in the treatment action
;     (TEMPL adj-purpose-implicit-XP-templ)
;     (LF-FORM W::ALLRIGHT)	  
;     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("alright%5:00:00:satisfactory:00") :comlex (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
     (EXAMPLE "a solution good for him")
     (LF-PARENT ONT::good)
     (SEM (f::orientation F::pos) (f::intensity ont::lo))
     ;; this is another indirect sense of "for"
     ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
     ;; the adj-affected-templ is for cases when adjective describes how people are affected, such as "this is good for him" where one has to infer the exact action/result it is good for
     (TEMPL adj-affected-XP-templ)
     (LF-FORM W::ALLRIGHT)	  
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
    ((w::all w::right)
   (SENSES
    ((LF-PARENT ONT::manner)
     (TEMPL PRED-S-POST-TEMPL)
     (example "are you breathing all right today")
     (meta-data :origin chf :entry-date 20070904 :change-date nil :comments nil :wn nil)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ pred-s-post-templ
 :tags (:base500)
 :words (
	    (w::all
	     (senses
	      ((LF-PARENT ONT::Modifier) ;; NO GOOD PLACE YET -- WILL HAVE TO SEE
	       (TEMPL PRED-VP-PRE-TEMPL)
	       (example "they will all be off")
	       (preference 0.91)
	       (meta-data :origin beetle2 :entry-date 20080513 :change-date nil :comments (pilot2) :wn nil)
	       )
	      ))
))

(define-words :pos W::name
 :words (
  ((w::all w::hallowes w::eve)
  (senses((LF-PARENT ONT::holiday)
    (TEMPL nname-templ)
    )
   )
)
))

(define-words :pos W::name
 :words (
  ((w::all w::saints w::day)
  (senses((LF-PARENT ONT::holiday)
    (TEMPL nname-templ)
    )
   )
)
))

(define-words :pos W::name
 :words (
  ((w::all w::saint^s w::day)
   (senses((LF-PARENT ONT::holiday)
	   
    (TEMPL nname-templ)
    )
   )
)
))

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
  (W::ALL
   (wordfeats ;(W::status ont::quantifier)
    (W::npmod +) (W::negatable +))
   (SENSES
    #|
    ((LF ONT::UNIVERSAL)
     (non-hierarchy-lf t)(TEMPL quan-count-mass-TEMPL)
     (SYNTAX (W::agr (? agr W::3s W::3p)))
     )
    |#
    ((LF ONT::UNIVERSAL)
#|
     (example "all of the trucks. all trucks")
     (non-hierarchy-lf t)(TEMPL quan-pl-count-templ)
     (SYNTAX (W::agr (? agr W::3p)) (w::status ont::indefinite-plural))
     )
|#
     (example "all of the trucks.  all trucks")
     (non-hierarchy-lf t)(TEMPL quan-cardinality-pl-templ);(TEMPL quan-count-mass-templ)
     (SYNTAX (W::agr (? agr W::3p)) (w::status ont::indefinite-plural));(w::status ont::QUANTIFIER))
     ) 
    ((LF ONT::UNIVERSAL)
     (example "all of the water")
     (non-hierarchy-lf t)(TEMPL quan-mass-TEMPL)
     (SYNTAX (W::agr (? agr W::3s)) (w::status ont::indefinite)) ; -- never plural if mass
     )
    
    )
   )
))


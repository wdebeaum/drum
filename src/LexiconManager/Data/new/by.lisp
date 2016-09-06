;;;;
;;;; W::by
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::by W::the W::way)
   (SENSES
    ((LF-PARENT ONT::TOPIC-SIGNAL)
     (TEMPL DISC-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::BY
   (SENSES
    ;;;;; ont::along requires a line or a strip as the ont::of
;    ((LF-PARENT ONT::ALONG)
;     (LF-FORM W::ALONG)
;     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL
;            (xp (% W::NP (W::case (? cas W::obj -)) (w::refl -))))
;     (EXAMPLE "go by the coast")
;     )
  
    (;(LF-PARENT ONT::MANNER)
     (LF-PARENT ONT::BY-MEANS-OF)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL
	    (xp (% W::NP (W::case (? cas W::obj -)) (w::gerund -) (w::refl -))))  ;; no gerund as we have BY-MEANS-OF sense
     (EXAMPLE "we can go by car/by air; he communicated by phone")
     )
    (;(LF-PARENT ONT::MANNER)
     (LF-PARENT ONT::BY-MEANS-OF)
     (TEMPL BINARY-CONSTRAINT-PRED-TEMPL
	    (xp (% W::NP (W::case (? cas W::obj -)) (w::gerund -) (w::refl -))))
     (EXAMPLE "it is accessible by helicopter")
     )

    
    ((LF-PARENT ONT::BY-MEANS-OF)
     (TEMPL BINARY-CONSTRAINT-S-subjcontrol-TEMPL)
     (EXAMPLE "he killed it by immersing it in water")
     )
    ;;;;; ont::via requires a point or region
    ;; don't need both this and ont::along
    ;; motion v only??
    ((LF-PARENT ONT::obj-in-path) ; ont::via
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL
            (xp (% W::NP (W::case (? cas W::obj -)) (w::gerund -) (w::refl -))))
     (EXAMPLE "go by pittsford")
     )
    ((LF-PARENT ont::adjacent) ; ont::spatial-loc --stative only
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL
	    (xp (% W::NP (W::case (? cas W::obj -)) (w::gerund -) (w::refl -))))
     (example "the box is by the door")
      (PREFERENCE .98)
     )

    ((LF-PARENT ONT::event-time)
     (LF-FORM W::BY)
     (example "by the time he arrived it was too late")
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL
	    (xp (% W::NP (W::case (? cas W::obj -)) (w::gerund -)
		   (w::refl -))))
     )

    ((LF-PARENT ONT::dimension)
     (example "8 by 10 (feet)")
;     (TEMPL BINARY-CONSTRAINT-NP-TEMPL
     (TEMPL BINARY-CONSTRAINT-NP-grd-grd1-TEMPL
	    (xp (% W::NP (W::case (? cas W::obj -)) (w::gerund -)(w::refl -))))
     )
    ((LF-PARENT ONT::originator)
     (example "a book by an author")
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL
	    (xp (% W::NP (W::case (? cas W::obj -)) (w::gerund -) (w::refl -))))
     ;; changed from ont::assoc-with (general) to more specific ont::originator
     (meta-data :origin caloy4 :entry-date 20070417 :change-date 20070517 :comments nil)
     )
    #||((LF-PARENT ONT::MANNER-REFL)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL
	    (xp (% W::NP (W::case (? cas W::obj -)) (w::refl +))))
     (example "the battery is in a closed path by itself" "the door closed by itself")
     (meta-data :origin beetle :entry-date 20090105 :change-date nil :comments beetle-pilots)
     ) ||#  

    ((LF-PARENT ONT::extent-predicate)
     (example "by two fold")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL
	    (xp (% W::NP (W::case (? cas W::obj -)) (w::gerund -)(w::refl -))))
     )
    
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::By W::WAY W::OF)
   (SENSES
    ;; ont::via is point, ont::along is strip
;    ((LF-PARENT ONT::ALONG)
;     (LF-FORM W::ALONG)
;     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
;     )
    ((LF-PARENT ont::obj-in-path) ; ONT::VIA)
     (LF-FORM W::VIA)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::By W::MEANS W::OF)
   (SENSES
    (
     (LF-PARENT ONT::BY-MEANS-OF)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL
	    (xp (% W::NP (W::case (? cas W::obj -)) (w::gerund -) (w::refl -))))  ;; no gerund as we have BY-MEANS-OF sense
     (EXAMPLE "we can go by car/by air; he communicated by phone")
     )
    (
     (LF-PARENT ONT::BY-MEANS-OF)
     (TEMPL BINARY-CONSTRAINT-PRED-TEMPL
	    (xp (% W::NP (W::case (? cas W::obj -)) (w::gerund -) (w::refl -))))
     (EXAMPLE "it is accessible by helicopter")
     )

    ((LF-PARENT ONT::BY-MEANS-OF)
     (TEMPL BINARY-CONSTRAINT-S-subjcontrol-TEMPL)
     (EXAMPLE "he killed it by immersing it in water")
     )

    )
   )
))


(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::BY
   (SENSES
    ((LF (W::BY))
     (non-hierarchy-lf t))
    )
   )
))


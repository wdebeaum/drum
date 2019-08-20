;;;;
;;;; W::ONE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  ;;  Note:: handling it this way overgenerates e.g., "the two ones in the corner" parses.
  (W::ONE
   (SENSES
	 ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("one%1:09:00"))
	  (syntax (W::one +))  ;; such a strange word, we give it its own feature
	  (LF-PARENT ont::referential-sem)
	  (example "the other one")
	  (preference .98) ;; prefer number sense
	 )
      )
    )
))

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :words (
  ((W::one w::another)
   (wordfeats (W::agr W::3p) (W::CASE W::OBJ))
   (SENSES
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     (TEMPL pronoun-indef-templ)
     (meta-data :origin csli-ts :entry-date 20070322 :change-date 20081111 :comments nil)
     (example "they evaluated one")
     )
    ))
   ((W::one W::^s)      ;; the derived interpretation can't compete well with others, so we make it explicit
    (wordfeats (W::agr W::1s) (W::stem W::one))
    (SENSES
     ((LF-PARENT ONT::PERSON)
      (TEMPL poss-pro-det-templ)
      (example "to display one's displeasure")))
    )
  ))

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::ONE
   (SENSES
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     (TEMPL PRONOUN-INDEF-TEMPL) ;; need PRO INDEF to get one's
     (example "how does one do that" "a room of one's own")
     (preference .98) ;; prefer number sense
     )
    )
   )
))


(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
	      :words (
		      ((W::ONE W::DAY)
		       (wordfeats (W::ATYPE (? atype W::pre W::post)))
		       (SENSES
			((LF-PARENT ONT::EVENT-TIME-REL)
			 (SYNTAX (W::IMPRO-CLASS (:* ONT::TIME-LOC W::SOMETIME)))
			 )
    )
   )
))

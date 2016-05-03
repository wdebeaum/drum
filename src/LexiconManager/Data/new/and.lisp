;;;;
;;;; w::and
;;;;

(define-words :pos w::adv
 :words (
;; Parentheticals
  ((w::and w::not)
  (senses
	   ((TEMPL BINARY-CONSTRAINT-NP-TEMPL)
	    (LF-PARENT ONT::PARENTHETICAL)
	    (meta-data :origin beetle :entry-date 20090116 :change-date nil :comments nil)
	    (preference 0.98) ;; lower preference so that other senses are considered first, since this sense is very underconstrained
	    )
	   )
)
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::and w::so w::on)
   (SENSES
    ((LF-PARENT ONT::etcetera)
     (TEMPL DISC-POST-TEMPL)
     (meta-data :origin calo :entry-date 20041122 :change-date nil :comments caloy2)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::and w::such)
   (SENSES
    ((LF-PARENT ONT::etcetera)
     (TEMPL DISC-POST-TEMPL)
     (meta-data :origin calo :entry-date 20041122 :change-date nil :comments caloy2)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::and w::so w::forth)
   (SENSES
    ((LF-PARENT ONT::etcetera)
     (TEMPL DISC-POST-TEMPL)
     (meta-data :origin calo :entry-date 20041122 :change-date nil :comments caloy2)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::AND
   (SENSES
    ((LF-PARENT ONT::CONJUNCT)
     )
    )
   )
))

(define-words :pos W::conj :boost-word t
 :tags (:base500)
 :words (
  (W::AND
   (wordfeats (W::conj +) (W::seq +))
   (SENSES
    ((LF W::AND)
     (non-hierarchy-lf t) (TEMPL SUBCAT-ANY-TEMPL)
     (syntax (w::status w::definite-plural))
     )
    )
   )
))

(define-words :pos W::conj :boost-word t
 :words (
;   )
  ((W::AND W::then)
   (wordfeats (W::conj +))
   (SENSES
    ;;;I think we don't have SEQ +, since this can't conjoin NPs - need to check JFA 4/02
    ((LF W::AND)
     (non-hierarchy-lf t)(TEMPL SUBCAT-ANY-TEMPL)
     (preference .98)
     )
    )
   )
))


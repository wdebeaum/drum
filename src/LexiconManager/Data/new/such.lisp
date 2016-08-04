;;;;
;;;; w::such
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ;; others?
  ((w::such w::a)  ; special treatment here becasue adjectives normally can't go in this position, e.g., "beautiful a cat"
   (SENSES
    ((meta-data :origin step :entry-date 20080612 :change-date nil :comments nil :wn ("like%3:00:00"))
     (LF-PARENT ONT::exemplifies)
     (example "such a proposal is a good candidate for funding")
     (templ central-adj-sing-templ)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ((w::such w::an)  ; special treatment here becasue adjectives normally can't go in this position, e.g., "beautiful a cat"
   (SENSES
    ((meta-data :origin step :entry-date 20080612 :change-date nil :comments nil :wn ("like%3:00:00"))
     (LF-PARENT ONT::exemplifies)
     (example "such an apple")
     (templ central-adj-sing-templ)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
   (w::such
   (SENSES
    ((LF-PARENT ONT::exemplifies)
     (example "such proposals are good candidates for funding")
     (templ central-adj-plur-templ)
     (preference .97) ; prefer such X as Y rule in grammar
     )
    )
   )))


(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ;; others?
  ((w::such) ;(w::such w::as)
   (SENSES
    ((meta-data :origin step :entry-date 20080612 :change-date nil :comments nil :wn ("like%3:00:00"))
     (EXAMPLE "a proposal such as that one")
     (LF-PARENT ONT::exemplifies)
;     (lf-form w::such-as)
     (templ binary-constraint-adj-templ)     )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::such
   (SENSES
    ((LF (W::such))
     (non-hierarchy-lf t))
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::such W::that)
   (SENSES
    (
     (LF-PARENT ONT::purpose)
     (TEMPL binary-constraint-s-decl-templ)
     (meta-data :origin trips :entry-date nil :change-date 20070609 :comments sentential-conjunction-cleanup)
     )
    )
   )
))

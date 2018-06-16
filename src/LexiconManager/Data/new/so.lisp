;;;;
;;;; w::so
;;;; 

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ((w::so w::so)
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080422 :change-date nil :comments nil)
     (lf-parent ont::neutral-acceptability-val)
     (SEM (F::GRADABILITY F::+) (f::orientation ont::neutral) (f::intensity ont::-))
     (TEMPL LESS-ADJ-TEMPL)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::SO
   (SENSES
    ((LF-PARENT ONT::actual)
     (example "that's so") ;?
     (preference .97) ; don't interfere w/ adv interpretations of so
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::SO
   (SENSES
    ((LF-PARENT ONT::degree-modifier-high)
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (example "he ran so fast")
     (PREFERENCE 0.98)  ;;;;; prefer discourse interps if possible
     )
    ((LF-PARENT ONT::intensifier)
     (TEMPL NON-DISC-ADV-OPERATOR-TEMPL)
     (example "he ran so very fast")
     (PREFERENCE 0.98)  ;;;;; prefer discourse interps if possible
     )
    ((LF-PARENT ONT::degree-modifier-high-event)
     (LF-FORM W::so)
     (example "he so wanted to go")
     (TEMPL PRED-VP-PRE-TEMPL)
     (PREFERENCE 0.98)  ;;;;; prefer discourse interps if possible
     )
    ((LF-PARENT ONT::therefore)
     (TEMPL binary-constraint-s-decl-templ-post-only)
     (example "she arrived so he left") 
     )
    ((LF-PARENT ONT::CONJUNCT)
     (TEMPL disc-pre-templ)
     (example "so you want to be a millionaire")
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::so W::that)
   (SENSES
    (;(LF-PARENT ONT::so-that)
     (LF-PARENT ONT::purpose)
     (TEMPL binary-constraint-s-decl-templ)
     (meta-data :origin trips :entry-date nil :change-date 20070609 :comments sentential-conjunction-cleanup)
     )
    )
   )
))

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :words (
  ((w::so w::far)
   (wordfeats (W::ATYPE (? atype W::pre W::post w::pre-vp)))
   (SENSES
    ((LF-PARENT ONT::TIME-REL-SO-FAR)
     (example "add the best answer so far to the list of choices")
      (SYNTAX (W::IMPRO-CLASS (:* ONT::TIME-LOC W::SO-FAR)))
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::so W::far)
   (SENSES
    ((LF-PARENT ONT::temporal-location)
     (TEMPL PRED-S-TEMPL)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::so W::long)
   (SENSES
    ((LF (W::GOODBYE))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_CLOSE))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::SO
   (SENSES
    ((LF (W::WAIT))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_DISCOURSE-MANAGE))
     ;; beetle fix -- do not use "so" in this sense unless it's stand-alone
     ;; if it is joined with something, it really should work as a regular adverbial 
     (preference 0.9)
     )
    )
   )
))


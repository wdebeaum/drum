;;;;
;;;; W::gotta
;;;;

(define-words :pos W::v 
 :words (
 (W::gotta
   (wordfeats (W::morph (:forms NIL)) (W::vform W::pastpart))
    (SENSES
    ;;;; The necessitiy sense is only typical of the perfective
    ;;;; "He gets to go" means he's allowed -- how can we do this?
    ((LF-PARENT ONT::NECESSITY)
     (meta-data :origin caet :entry-date 20120118 :change-date nil :comments nil)
     (example "he's gotta go")
     (SEM (F::Aspect F::Indiv-level) (f::Time-span f::extended)) ;; don't allow temporal mods on the higher verb (need)
     (TEMPL neutral-effect-SUBJCONTROL-BASE-TEMPL)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::gotta W::go)
   (SENSES
    ((LF (W::GOODBYE))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_CLOSE))
     )
    )
   )
))


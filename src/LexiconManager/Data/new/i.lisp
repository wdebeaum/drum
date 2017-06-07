;;;;
;;;; W::I
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::I w::D)
   (SENSES
    ;; physical form of id
     ((LF-PARENT ONT::symbolic-representation) (TEMPL other-reln-templ)
      (META-DATA :ORIGIN PLOT :ENTRY-DATE 20080505 :CHANGE-DATE NIL
      :COMMENTS NIL))
     ;; abstract
     ((LF-PARENT ONT::identification) (TEMPL other-reln-templ)
      (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE 20050113
      :COMMENTS HTML-PURCHASING-CORPUS))
    ))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    ((W::i w::s w::p)
   (SENSES
    ((LF-PARENT ONT::internet-organization)
     (EXAMPLE "isp is the abbr. for internet service provider")
     (meta-data :origin task-learning :entry-date 20050824 :change-date nil :comments nil)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::I W::GUESS)
   (SENSES
    ;;;;; Myrosia 02/10/03 changed to disc-post-templ to reflect only
    ;;;;; post-utterance "I guess", and lowered preference to not compete
    ;;;;; with "I guess" in the places where it is a VP starting a sentence
    ((LF-PARENT ONT::INTERJECTION)
     (LF-FORM W::I_GUESS)
     (TEMPL DISC-POST-TEMPL)
     (PREFERENCE 0.97)
     )
    ((LF-PARENT ONT::INTERJECTION)
     (LF-FORM W::I_GUESS)
     (TEMPL PRED-S-VP-TEMPL)
     (PREFERENCE 0.96)
     )
    )
   )
))

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::I
   (wordfeats (W::agr W::1S) (W::CASE W::SUB))
   (SENSES
    ((LF-PARENT ONT::PERSON)
     )
    )
   )
))

(define-words :pos W::value :boost-word t
 :tags (:base500)
 :words (
  (W::I
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::I W::see)
   (SENSES
    ((LF (W::i-see))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_ACK))
     (preference .97)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::I W::think W::not)
   (SENSES
    ((LF (ONT::UNSURE-NEG))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     (preference .97)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::I W::think W::so)
   (SENSES
    ((LF (ONT::UNSURE-POS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     (preference .97)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   ((W::I w::do w::n^t W::think W::so)
   (SENSES
    ((LF (ONT::UNSURE-NEG))
     (meta-data :origin cardiac :entry-date 20080814 :change-date nil :comments nil)
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     (preference .97)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ;; added here for now until they are handled compositionally
   ((W::I W::believe W::so)
   (SENSES
    ((LF (ONT::UNSURE-POS))
     (meta-data :origin cardiac :entry-date 20080814 :change-date nil :comments nil)
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   ((W::I w::do w::n^t W::believe W::so)
   (SENSES
    ((LF (ONT::UNSURE-NEG))
     (meta-data :origin cardiac :entry-date 20080814 :change-date nil :comments nil)
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     (preference .97)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
;   )
  ((W::I W::guess W::not)
   (SENSES
    ((LF (ONT::UNSURE-NEG))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     (preference .97)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::I W::guess W::so)
   (SENSES
    ((LF (ONT::UNSURE-POS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     (preference .97)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::I W::guess)
   (SENSES
    ((LF (ONT::UNSURE-POS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     (preference .97)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
    ((W::I W::suppose W::so)
   (SENSES
    ((LF (ONT::UNSURE-POS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     (preference .97)
     )
    )
   )
))

#||(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::I W::suppose)
   (SENSES
    ((LF (W::UNSURE-POS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     (preference .97)
     )
    )
   )
))||#

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::I W::^m W::sorry)
   (SENSES
    ((LF (W::SORRY))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_APOLOGIZE))
     (preference .97) ;; don't compete with pronoun
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::I W::apologize)
   (SENSES
    ((meta-data :origin monroe :entry-date 20031217 :change-date nil :comments s7)
     (LF (W::SORRY))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_APOLOGIZE))
     (preference .97)
     )
    )
   )
))


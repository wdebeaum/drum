;;;;
;;;; W::YOU
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::YOU W::KNOW)
   (SENSES
    ((LF-PARENT ONT::TOPIC-SIGNAL)
     (TEMPL DISC-TEMPL)
     )
    ((LF-PARENT ONT::INTERJECTION)
     (LF-FORM W::YOU_KNOW)
     (TEMPL PRED-S-VP-TEMPL)
     )
    )
   )
))

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::YOU
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (SYNTAX (W::agr W::2s) (w::status ont::pro))
     )
    ;; 2010 -- prefer 1 complex sense to two entries
;    ((LF-PARENT ONT::PERSON)
;     (SYNTAX (W::agr W::2p))
;     (PREFERENCE 0.98) ;; prefer singular
;     )
    ;; 3/3/2011 -- reinstating two entries now that we distinguish between w::pro and w::pro-set
    ((LF-PARENT ONT::PERSON)
     (SYNTAX (W::agr W::2p) (w::status ont::pro-set))
     (PREFERENCE 0.98) ;; prefer singular
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   ((W::you W::bet)
   (SENSES
    ((LF (ONT::POS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     (preference .97)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   ((W::you W::betcha)
   (SENSES
    ((LF (ONT::POS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     (preference .97)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::you W::^re W::welcome)
   (SENSES
    ((LF (W::WELCOME))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_WELCOME))
      )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::you W::^re W::very W::welcome)
   (SENSES
    ((LF (W::WELCOME))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_WELCOME))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::you W::^re W::so W::welcome)
   (SENSES
    ((LF (W::WELCOME))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_WELCOME))
      )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::you W::are W::welcome)
   (SENSES
    ((LF (W::WELCOME))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_WELCOME))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::you W::are W::very W::welcome)
   (SENSES
    ((LF (W::WELCOME))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_WELCOME))
     (preference .97) ;; don't compete with pronoun 'you'
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::you W::are W::most W::welcome)
   (SENSES
    ((LF (W::WELCOME))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_WELCOME))
     (preference .97) ;; don't compete with pronoun 'you'
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::you W::are W::so W::welcome)
   (SENSES
    ((LF (W::WELCOME))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_WELCOME))
     (preference .97) ;; don't compete with pronoun 'you'
     )
    )
   )
))


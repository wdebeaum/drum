;;;;
;;;; W::WHERE
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::WHERE
   (SENSES
    ;;;;;(sem (information f_information-content))
    ((LF-PARENT ONT::LOCATION)
      (TEMPL PRONOUN-WH-TEMPL)
     (SYNTAX (W::wh W::r) (W::case W::obj) (W::sing-lf-only +))
     )
     #||((LF-PARENT ONT::LOCATION)
      (TEMPL PRONOUN-WH-TEMPL)
     (SYNTAX (W::wh W::q) (W::case W::obj) (W::sing-lf-only +))
     )||#
    ;;;;; (sem (information f_information-content))
;    ((LF-PARENT ONT::PATH)
;     (TEMPL PRONOUN-WH-TEMPL)
;     (SYNTAX (W::wh W::r) (W::case W::obj) (W::sing-lf-only +))
;     )
    )
   )
))

(define-words :pos W::adv :templ PPWORD-QUESTION-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::WHERE
   (SENSES
    ((LF-PARENT ONT::AT-LOC)  ;; WH-LOCATION)
     (SYNTAX (W::IMPRO-CLASS ONT::LOCATION))
     (example "Where did you put the cake?")
     (TEMPL ppword-question-adv-pred-templ)
     (SEM (F::information F::information-content))
     (preference .98)  ;; prefer TO-LOC all other things being equal, as it is more restrictive
     )
    #|((LF-PARENT ONT::TO-LOC)   ;; no longer necessary given the RESULT construction
     (SYNTAX (W::IMPRO-CLASS ONT::LOCATION))
     (TEMPL ppword-question-adv-NP-templ)
     (SEM (F::information F::information-content))
    )|#

    ;; this relative clause pro is highest priority as it has strict role restrictions (e.g., modifying place, location, etc)
    ;;   so will be preferred when possible
   
    ((LF-PARENT ONT::loc-where-rel)
     (SYNTAX (W::wh W::R))
     (example "The model where it never rains; The place where it never rains")
     (TEMPL PPWORD-ADV-TEMPL)
     
     )

    ((LF-PARENT ONT::AT-LOC)
     (example "I found it where you put it")
     (TEMPL binary-constraint-S-decl-TEMPL)
     (preference .98)
     )
    
    )
   )
))

(define-words :pos W::n :templ PPWORD-N-TEMPL
 :tags (:base500)
 :words (
  (W::WHERE
   (wordfeats (W::WH W::Q))
   (SENSES
    ((LF-PARENT ONT::LOCATION)
     (PREFERENCE 0.9) ;; really prefer adv
     )
    )
   )
))


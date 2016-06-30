;;;;
;;;; W::THAT
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::THAT
   (SENSES
    ((LF-PARENT ONT::DEGREE-MODIFIER-HIGH) 
     (LF-FORM W::THAT)
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (example "it's not that unlikely")
     ;; this really is very unlikely, so Myrosia 2004.08.05 lowered the preference to avoid interference with better senses
     (preference 0.9)
     )
    )
   )
))

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
;;;   )
  (W::THAT
   (SENSES
    ;;;;; pronoun THAT, we prefer a situation antecedent if possible, but allow anything
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     ;; (SEM (F::origin (? !n F::human)))
     (SYNTAX (W::WH W::R) (W::agr (? agr W::3s W::3p)) (W::sing-lf-only +))
     )
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     (SEM (F::origin (? !n F::human)))
     (SYNTAX (W::WH -) (W::agr (? agr W::3s W::3p)) (W::case W::obj) (W::sing-lf-only +))
     )
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     (SEM (F::origin (? !n F::human)))
     (SYNTAX (W::WH -) (W::agr W::3s) (W::case W::sub) (W::sing-lf-only +))
     )
    )
   )
))

(define-words :pos W::art :boost-word t
 :tags (:base500)
 :words (
  (W::THAT
   (wordfeats (W::agr W::3s) (W::diectic +))
   (SENSES
    ((LF ONT::DEFINITE)
     (non-hierarchy-lf t)(TEMPL MASS-agr-templ)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
;   )
  ((W::that w::will w::do)
   (SENSES
    ( (meta-data :origin calo :entry-date 20040412 :change-date nil :comments y1v4)
     (LF (W::OK))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_CONFIRM))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::that w::^ll w::do)
   (SENSES
    ( (meta-data :origin calo :entry-date 20040414 :change-date nil :comments y1v4)
     (LF (W::OK))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_CONFIRM))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   ((W::that w::is w::all w::for w::now)
   (SENSES
    ((LF (W::GOODBYE))
     (meta-data :origin asma :entry-date 20110921 :change-date nil :comments nil)
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_CLOSE))
     )
    )
   )
))


;;;;
;;;; W::subordinate
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::subordinate
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (meta-data :origin calo-ontology :entry-date 20060714 :change-date nil :wn ("subordinate%1:18:00") :comments caloy3)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::subordinate
    (SENSES
     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("subordinate%5:00:00:junior:00") :comments html-purchasing-corpus :comlex (ADJECTIVE))
      (example "a good book")
      (lf-parent ont::substandard-val)
      (TEMPL central-adj-templ)
      )
;;;     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("subordinate%5:00:00:junior:00") :comments html-purchasing-corpus :comlex (ADJECTIVE))
;;;      (example "a wall good for climbing")
;;;      (LF-PARENT ONT::ACCEPTABILITY-VAL)
;;;      (TEMPL adj-purpose-TEMPL (XP (% W::PP (W::PTYPE W::to))))
;;;      )
;;;     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("subordinate%5:00:00:junior:00") :comments html-purchasing-corpus :comlex (ADJECTIVE))
;;;      (EXAMPLE "a drug suitable for cancer")
;;;      (LF-PARENT ONT::ACCEPTABILITY-VAL)
;;;      ;; this is a sense that allows for implicit/indirect senses of "for"
;;;      ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
;;;      ;; the adj-purpose-implicit-templ is for indirect purposes, such as "this is good for cancer" where one has to infer that the actual use is in the treatment action
;;;      (TEMPL adj-purpose-implicit-XP-templ)
;;;      )
;;;     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("subordinate%5:00:00:junior:00") :comments html-purchasing-corpus :comlex (ADJECTIVE))
;;;      (EXAMPLE "a solution good for him")
;;;      (LF-PARENT ONT::ACCEPTABILITY-VAL)
;;;      ;; this is another indirect sense of "for"
;;;      ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
;;;      ;; the adj-affected-templ is for cases when adjective describes how people are affected, such as "this is good for him" where one has to infer the exact action/result it is good for
;;;      (TEMPL adj-affected-XP-templ)
;;;      )
     )
    )
))


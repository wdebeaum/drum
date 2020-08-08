;;;;
;;;; W::splendid
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::splendid
    (wordfeats (W::MORPH (:FORMS (-LY))))
    (SENSES
     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("splendid%5:00:00:impressive:00") :comments html-purchasing-corpus :comlex (EXTRAP-ADJ-FOR-TO-INF))
      (example "a good book")
      (lf-parent ont::great-val)
      (SEM (f::gradability +) (f::orientation F::pos) (f::intensity ont::hi))
      (TEMPL central-adj-templ)
      )
;;;     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("splendid%5:00:00:impressive:00") :comments html-purchasing-corpus :comlex (EXTRAP-ADJ-FOR-TO-INF))
;;;      (example "a wall good for climbing")
;;;      (LF-PARENT ONT::ACCEPTABILITY-VAL)
;;;      (TEMPL adj-purpose-TEMPL)
;;;      )
;;;     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("splendid%5:00:00:impressive:00") :comments html-purchasing-corpus :comlex (EXTRAP-ADJ-FOR-TO-INF))
;;;      (EXAMPLE "a drug suitable for cancer")
;;;      (LF-PARENT ONT::ACCEPTABILITY-VAL)
;;;      ;; this is a sense that allows for implicit/indirect senses of "for"
;;;      ;; the main sense is adj-purpose-templ for cases such as "this is good for treating cancer"
;;;      ;; the adj-purpose-implicit-templ is for indirect purposes, such as "this is good for cancer" where one has to infer that the actual use is in the treatment action
;;;      (TEMPL adj-purpose-implicit-XP-templ)
;;;      )
;;;     ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("splendid%5:00:00:impressive:00") :comments html-purchasing-corpus :comlex (EXTRAP-ADJ-FOR-TO-INF))
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


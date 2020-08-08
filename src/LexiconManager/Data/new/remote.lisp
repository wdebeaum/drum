;;;;
;;;; W::remote
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::remote
   (SENSES
    ((meta-data :origin calo :entry-date 20050527 :change-date nil :wn ("remote%1:06:00") :comments projector-purchasing)
     (example "I'll take that projector as long as it has a remote")
     (LF-PARENT ONT::device)
     (PREFERENCE 0.96) ;; prefer adjective sense
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::REMOTE
    (SENSES
     ((meta-data :origin calo :entry-date 20031223 :change-date 20090731 :wn ("remote%5:00:01:far:00") :comments html-purchasing-corpus)
      (LF-PARENT ONT::REMOTE)
      (example "a remote possibility" "remote control")
      (templ adj-theme-templ)
      (SEM (f::orientation F::pos) (f::intensity ont::hi))
      )
     )
    )
))


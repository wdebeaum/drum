;;;;
;;;; W::HOW
;;;;

(define-words :pos W::adv :templ PPWORD-QUESTION-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::HOW
   (SENSES
    ((LF-PARENT ONT::by-means-of)
     (example "how did he do it")
     (SYNTAX (W::IMPRO-CLASS ONT::method))
     )
    ((LF-PARENT ont::at-scale-value)
     (example "how are you")
      (TEMPL ppword-question-adv-pred-templ)
     (syntax (W::IMPRO-CLASS ONT::STATUS) (W::HOW +))
     )
    )
   )))

(define-words :pos W::adv 
 :tags (:base500)
 :words (
  (W::HOW
   (SENSES
    ((LF-PARENT ONT::DEGREE)
     (example "how blue is it")
     (SYNTAX (W::IMPRO-CLASS ONT::degree) (W::HOW +))
     (SEM (F::information F::information-content))
     (templ ppword-question-adv-how-templ)
     (preference .97) ;; prefer method sense
     )
    )
   )
))



(define-words :pos W::adv :templ PPWORD-QUESTION-ADV-TEMPL
 :words (
   ((W::HOW w::far)
    (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((lf-parent ont::spatial-distance-rel)
     (example "how far did he run")
     (meta-data :origin step :entry-date 20080722 :change-date nil :comments step1) 
     (SYNTAX (W::IMPRO-CLASS ONT::degree))
     (SEM (F::information F::information-content))
     (templ ppword-question-adv-templ)
     (preference .96)
     )
    )
   )
))

#|
(define-words :pos W::n :templ PPWORD-N-TEMPL
 :words (
  ((W::HOW w::far)
   (wordfeats (W::morph (:forms (-none))) (w::wh w::q))
   (SENSES
    ((LF-PARENT ONT::distance)
     (example "how far did he run")
     (meta-data :origin step :entry-date 20080722 :change-date nil :comments step1) 
     (PREFERENCE 0.9) ; really prefer adv
     )))
))
|#
#||
(define-words :pos W::n :templ PPWORD-N-TEMPL
 :words (
  ((W::HOW w::long)
   (wordfeats (W::morph (:forms (-none))) (w::wh w::q))
   (SENSES
    ((LF-PARENT ont::duration)
     (example "how long did he run")
     (meta-data :origin step :entry-date 20080722 :change-date nil :comments step1) 
     (PREFERENCE 0.96) ; prefer adv
     )))
))
||#

(define-words :pos W::adv :templ PPWORD-QUESTION-ADV-TEMPL
 :words (
   ((W::HOW w::long)
    (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((LF-PARENT ONT::time-duration-rel)
     (example "how long did it take")
     (meta-data :origin step :entry-date 20080722 :change-date nil :comments step1) 
     (SYNTAX (W::IMPRO-CLASS ONT::degree))
     (templ ppword-question-adv-templ)
     (preference .96) ;; prefer bare word
     )
    )
   )
))

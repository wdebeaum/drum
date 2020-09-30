;;;;
;;;; W::OVER
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::OVER
   (SENSES
    ((LF-PARENT ONT::AGAIN) ;FREQUENCY)
     (TEMPL PRED-S-POST-TEMPL)
     (example "do it over" "let's start over from the beginning")
     )

    ((LF-PARENT ONT::DIRECTION-OVER) ;OVER-TO) ;DIRECTION)
     (TEMPL PRED-S-POST-TEMPL)
     (EXAMPLE "he threw the ball over (to me)" "hand it over (to them)" "got his point over" "he won them over (to the other side)" "invite some friends over (to your place)" "he fell over" "he turned his cards over" "she knocked the lamp over")
     ) ;Removed as this is covered by ont::over-to below.
    
    ;Merging the following two into one onttype ont::direction-over above since it is hard to distinguish between these two
    ;((LF-PARENT ONT::OVER-TO)
     ;(TEMPL ADV-OPERATOR-TEMPL (XP (% W::PP (W::PTYPE W::to))))
     ;(TEMPL PRED-S-POST-TEMPL (XP (% W::PP (W::PTYPE W::to))))
    ; (EXAMPLE "he threw the ball over (to me)" "hand it over (to them)" "got his point over" "he won them over (to the other side)" "invite some friends over (to your place)")
    ;)

    ;((LF-PARENT ONT::OVER-ORIENTATION-CHANGE)
     ;(TEMPL ADV-OPERATOR-TEMPL)
     ;(TEMPL PRED-S-POST-TEMPL)
    ; (EXAMPLE "he fell over" "he turned his cards over" "she knocked the lamp over")
    ;)

    ((LF-PARENT ONT::above)
     (TEMPL BINARY-CONSTRAINT-NP-implicit-TEMPL)
     (example "he put it over the box" "he threw the ball over the fence")
     )
    
    ((LF-PARENT ONT::distributed-pos)
     (TEMPL BINARY-CONSTRAINT-S-implicit-TEMPL)
     (example "he spread the seed over the field")
     )
    
  #| ((LF-PARENT ONT::QMODIFIER)
     (example "sell it for over five dollars")
     (TEMPL NUMBER-OPERATOR-TEMPL)
     (lf-form w::more)
     (meta-data :origin calo :entry-date 20040426 :change-date nil :comments calo-y1v1)
     )|#

    (;(LF-PARENT ONT::TIME-span-rel)
     (LF-PARENT ONT::EVENT-DURATION-MODIFIER)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (meta-data :origin step :entry-date 20080530 :change-date nil :comments nil)
     (example "the riot over the last week" "stay over" "sleep over")
     )
    
 #|
    (
     (LF-PARENT ONT::EVENT-DURATION-MODIFIER)
     (TEMPL binary-constraint-adj-postpos-templ)
     (meta-data :origin step :entry-date 20080530 :change-date nil :comments nil)
     (example "green over the last week")
     )
 |#
 
    ((LF-PARENT ONT::more-than-rel)
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     (example "purchases over five dollars" "he drove over the limit")
     (SYNTAX (W::ALLOW-DELETED-COMP +) (w::degree-adv +))
     (meta-data :origin calo :entry-date 20040112 :change-date nil :comments calo-y1v1)
     )

    ((LF-PARENT ONT::COMPLETELY)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (EXAMPLE "I looked over the files" "read it over" "talked the matter over with his wife")
    )
    
    #||((LF-PARENT ONT::OVER)
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     (EXAMPLE "I ran over the hill")
     )||#

    )
   )
))

(define-words
    :pos w::adv
 :tags (:base500)
 :words (
  (w::over
  (senses((lf-parent ont::distance-val) ;location-distance-modifier)
	    (templ adv-operator-templ)
	    (example "he is over there/at the house" "the next town over"));"he is out in the sunshine" "he is out in the street"))
	   )
)
))

(define-words :pos W::adj 
 :words (
  (W::over
   (SENSES
    (
     (LF-PARENT ONT::FINISHED)
     (example "The show is over.")
     (TEMPL predicative-only-adj-templ)
     )
    )
   )
))


(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::OVER
   (SENSES
    ((LF (W::OVER))
     (non-hierarchy-lf t))
    )
   )
))

(define-words :pos w::adv
 :words (
  (w::over-
  (senses
   ((lf-parent ont::DEGREE-MODIFIER-HIGH)
    (example "overexpress; overdo")
    (templ V-PREFIX-templ)
    )
   )
  )
))

(define-words :pos W::adj 
 :words (
  (W::over-
   (SENSES
    (
     (LF-PARENT ONT::DEGREE-MODIFIER-HIGH)
     (example "overthin")
     (TEMPL prefix-adj-templ)
     )
    )
   )
))

(define-words :pos w::adv
 :words (
  (w::over-
  (senses
   ((lf-parent ont::DEGREE-MODIFIER-HIGH)
    (example "oversensitive; overconfident")
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))

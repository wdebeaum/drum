;;;;
;;;; w::a
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ;; entry patterned on difficult
  ((w::a W::bitch)
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((meta-data :origin csli-ts :entry-date 20070313 :change-date nil :comments nil :wn nil)
     (lf-parent ont::difficult)
     (example "Programmers are a bitch to interview")
     (templ adj-content-templ)
     (preference .97) ;; reduce competition with the article
     )
    ((meta-data :origin csli-ts :entry-date 20070313 :change-date nil :comments nil :wn nil)
     (lf-parent ont::difficult)
     (example "Programmers are a bitch for managers")
     (templ adj-content-affected-XP-templ  (xp (% w::pp (w::ptype w::for))))
     (preference .97) ;; reduce competition with the article
     )
    ((meta-data :origin csli-ts :entry-date 20070321 :change-date nil :comments nil :wn ("hard%3:00:06"))
     (EXAMPLE "it's a bitch to see him")
     (lf-parent ont::difficult)
     (TEMPL adj-expletive-content-xp-TEMPL (XP (% W::CP (W::CTYPE W::s-to))))
     (preference .97) ;; reduce competition with the article
     )
    ((meta-data :origin csli-ts :entry-date 20070321 :change-date nil :comments nil :wn ("hard%3:00:06"))
     (example "Programmers are a bitch for managers to interview")
     (lf-parent ont::difficult)
     (TEMPL adj-expletive-content-control-TEMPL)
     (preference .97) ;; reduce competition with the article
     )
    ))
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::A W::BIT)
   (SENSES
    ((LF-PARENT ont::degree-modifier-low)
     (example "it's a bit green")
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (PREFERENCE 0.98) ;; reduce competition with article 
     )
    ((LF-PARENT ONT::DEGREE-MODIFIER-LOW)
     (TEMPL comparative-adj-ADV-OPERATOR-TEMPL)
     (example "a bit more")
     (preference .96) 
     )
    ((meta-data :origin fruitcarts :entry-date 20041117 :change-date nil :comments nil)
     (LF-PARENT ont::degree-modifier-low)
     (TEMPL PRED-VP-TEMPL)
     (example "slow down a bit")
     (preference .97)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::A W::LOT)
   (SENSES
    ((LF-PARENT ont::degree-modifier-high)
     (meta-data :origin calo :entry-date 20050214 :change-date nil :comments caloy2)
     (example "there are a lot lighter ones but they are more expensive")
     (TEMPL comparative-adj-adv-operator-templ)
     (PREFERENCE 0.98) ;; reduce competition with article 
     )
    ((LF-PARENT ont::degree-modifier-high)
     (meta-data :origin calo :entry-date 20050214 :change-date nil :comments caloy2)
     (example "do you run a lot?")
     (TEMPL pred-vp-templ)
     (PREFERENCE 0.98) ;; reduce competition with article 
     )
    ((LF-PARENT ont::degree-modifier-high)
     (TEMPL comparative-ADV-OPERATOR-TEMPL)
     (example "a lot more")
     (PREFERENCE 0.98) ;; reduce competition with article 
     (meta-data :origin calo :entry-date 20050427 :change-date nil :comments caloy2)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((w::a w::good w::deal)
   (SENSES
    ((LF-PARENT ont::degree-modifier-high)
     (meta-data :origin calo :entry-date 20050214 :change-date nil :comments caloy2)
     (example "do you run a good deal?")
     (TEMPL pred-vp-templ)
     (PREFERENCE 0.97) ;; reduce competition with article 
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
    ((w::a w::great w::deal)
   (SENSES
    ((LF-PARENT ont::degree-modifier-high)
     (meta-data :origin calo :entry-date 20050214 :change-date nil :comments caloy2)
     (example "do you run a great deal?")
     (TEMPL pred-vp-templ)
     (PREFERENCE 0.97) ;; reduce competition with article 
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::a W::little W::bit)
   (SENSES
    ((LF-PARENT ont::degree-modifier-low)
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (example "a little bit to the right")
     (PREFERENCE 0.98) ;; reduce competition with article 
     (meta-data :origin fruitcarts :entry-date 20060111 :change-date nil :comments fruitcart-co01)
     )
    ((LF-PARENT ont::degree-modifier-low)
     (TEMPL PRED-VP-TEMPL)
     (example "slow down a little bit")
     (preference .97) ;; prefer senses with complement
     )
    ((LF-PARENT ont::degree-modifier-low)
     (TEMPL comparative-adj-ADV-OPERATOR-TEMPL)
     (example "a little bit more")
     (PREFERENCE 0.98) ;; reduce competition with article 
     (meta-data :origin fruitcarts :entry-date 20050505 :change-date nil :comments fruitcart-11-2)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::a W::little)
   (SENSES
    ((meta-data :origin lou :entry-date 20040319 :change-date nil :comments lou)
     (LF-PARENT ont::degree-modifier-low)
     (example "slow down a little")
     (TEMPL PRED-VP-TEMPL)
     (preference .96) ;; prefer senses with complement
     )
    ((LF-PARENT ont::degree-modifier-low)
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (example "a little green" "a little down")
     (PREFERENCE 0.98) ;; reduce competition with article 
     (meta-data :origin fruitcarts :entry-date 20050331 :change-date nil :comments fruitcarts-11-1)
     )
    ((LF-PARENT ont::degree-modifier-low)
     (TEMPL comparative-adj-ADV-OPERATOR-TEMPL)
     (example "a little more")
     (preference .96) ;; prefer senses with complement
     (meta-data :origin fruitcarts :entry-date 20050505 :change-date nil :comments fruitcart-11-2)
     )
    )
   )
))

(define-words :pos W::value :boost-word t
 :words (
  ((w::a w::d)
  (senses
   ((LF-PARENT ONT::era)
    (TEMPL value-templ) (PREFERENCE 0.92)
    )
   )
)
))

(define-words :pos W::value :boost-word t
 :words (
  ((w::a. w::d.)
  (senses
   ((LF-PARENT ONT::era)
    (TEMPL value-templ) (PREFERENCE 0.92)
    )
   )
)
))

(define-words :pos W::value :boost-word t
 :words (
  ((w::a w::c w::e)
  (senses
   ((LF-PARENT ONT::era)
    (TEMPL value-templ) (PREFERENCE 0.92)
    )
   )
)
))

(define-words :pos W::value :boost-word t
 :words (
  ((w::a. w::c. w::e.)
  (senses
   ((LF-PARENT ONT::era)
    (TEMPL value-templ) (PREFERENCE 0.92)
    )
   )
)
))

(define-words :pos W::value :boost-word t
 :words (
  ((w::a. w::c. w::e.)
  (senses
   ((LF-PARENT ONT::era)
    (TEMPL value-templ) (PREFERENCE 0.92)
    )
   )
)
))

(define-words :pos W::value :boost-word t
 :tags (:base500)
 :words (
  (W::A
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

(define-words :pos W::n :templ PRED-NO-FORM-TEMPL
 :words (
  ((W::a W::m)
   (SENSES
    ((LF-PARENT ONT::morning-am) ;day-stage-AM)
     (LF-FORM W::AM)
     (preference 0.98) ;; reduce competion w/ the article
     (SEM (F::time-function F::day-period))
     )
    )
   )
))

(define-words :pos W::n :templ PRED-NO-FORM-TEMPL
 :words (
  ((W::a W::punc-period W::m W::punc-period)
   (SENSES
    ((LF-PARENT :morning-am) ;day-stage-AM)
     (preference 0.98) ;; reduce competion w/ the article
     (LF-FORM W::AM)
     (SEM (F::time-function F::day-period))
     )
    )
   )
))

(define-words :pos W::art :boost-word t
 :tags (:base500)
 :words (
  (W::A
   (SENSES
    ((LF ONT::INDEFINITE)
     (non-hierarchy-lf t)(TEMPL INDEFINITE-COUNTABLE-TEMPL)
     )
    )
   )
))

(define-words :pos W::quan :boost-word t
 :words (
  ((W::A W::FEW)
   (wordfeats (W::status ont::indefinite-plural))
   (SENSES
    ((LF ONT::A-FEW)
     (non-hierarchy-lf t)(TEMPL quan-3p-templ)
     )
    )
   )
))

(define-words :pos W::quan :boost-word t
 :words (
  ((W::A W::couple)
   (wordfeats (W::status ont::indefinite-plural))
   (SENSES
    ((LF ONT::A-FEW)
     (non-hierarchy-lf t)(TEMPL quan-3p-templ)
     )
    )
   )
))

(define-words :pos W::quan :boost-word t
 :words (
  ((W::A W::HANDFUL)
   (wordfeats (W::status ont::indefinite-plural)(W::NOsimple +) )
   (SENSES
    ((LF ONT::A-FEW)
     (non-hierarchy-lf t)(TEMPL quan-3p-templ)
     )
    )
   )
))

(define-words :pos W::quan :boost-word t
 :words (
  ((W::A W::LITTLE)
   (wordfeats (W::status ont::indefinite)(W::AGR W::3s))
   (SENSES
    ((LF ONT::A-LITTLE)
     (non-hierarchy-lf t)(TEMPL quan-mass-TEMPL)
     (example "a little water")
      )
     ((LF ONT::A-LITTLE)
     (non-hierarchy-lf t)(TEMPL quan-bare-TEMPL)     
     )
   ((LF ONT::A-LITTLE)
     (non-hierarchy-lf t)
     (example "there is a little of the truck left")
     (TEMPL quan-sing-sing-TEMPL)
     )
    )
  )
))

(define-words :pos W::quan :boost-word t
 :words (
  ((W::A W::good w::deal)
    (wordfeats (W::negatable +) (W::NOsimple +))
    (SENSES
     ((LF ONT::PLENTY)
      (example "a good deal of people")
      (non-hierarchy-lf t)(TEMPL quan-cardinality-pl-TEMPL)
      (SYNTAX (W::agr W::3p) (w::status ont::indefinite-plural))
      )
     ((LF ONT::PLENTY)
      (example "a good deal of water")
      (non-hierarchy-lf t)(TEMPL quan-mass-TEMPL)
      (SYNTAX (W::agr (? agr W::3s)) (w::status ont::indefinite))
      )
     )
    )
))

(define-words :pos W::quan :boost-word t
 :words (
  ((W::A W::bit)
    (wordfeats (W::status ont::indefinite) (W::negatable +) (W::NOsimple +))
    (SENSES
     ((LF ONT::a-bit)
      (example "a bit of sand")
      (non-hierarchy-lf t)(TEMPL quan-mass-TEMPL)
      (SYNTAX (W::agr W::3p))
      )
     )
    )
))

(define-words :pos W::quan :boost-word t
 :words (
   ((W::A W::LOT W::of)
    (wordfeats (W::negatable +))
    (SENSES
     ((LF ONT::PLENTY)
      (example "a lot of water")
      (non-hierarchy-lf t)(TEMPL quan-mass-TEMPL)
      (SYNTAX (W::agr (? agr W::3s)) (w::status ont::sm))
       )
     ((LF ONT::PLENTY)
      (example "a lot of people")
      (non-hierarchy-lf t)(TEMPL quan-cardinality-pl-TEMPL)
      (SYNTAX (W::agr W::3p) (w::status ont::indefinite-plural))
      )
     )
    )
))

(define-words :pos W::quan :boost-word t
 :words (
  ((W::A W::CERTAIN)
   (wordfeats (W::status ont::indefinite) (W::negatable +))
   (SENSES
    ((LF ONT::INDEFINITE)  ;; a certain man == some man
     (non-hierarchy-lf t)
     (TEMPL INDEFINITE-COUNTABLE-TEMPL)
    ))
   )
))


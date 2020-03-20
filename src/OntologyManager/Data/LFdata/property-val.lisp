(in-package :om)

; > starts a block
; < ends the block
; use `[code]` to use lisp syntax in code (optional, but makes eliminating commented code easier)

(define-type ONT::property-val
 :parent ONT::domain-property
 :sem (F::ABSTR-OBJ (:required (F::CONTAINER -) (F::INFORMATION -) (f::intentional -))
;                    (:default ;(f::scale -)
;                     (f::intensity -) (f::orientation -))
		    )
 :arguments ((:REQUIRED ONT::FIGURE)
             (:optional ONT::FORMAL  (f::situation))
             (:optional ONT::NEUTRAL1)
             (:optional ONT::NEUTRAL ((? pvt F::Phys-obj f::abstr-obj f::situation)))
                                        ;            (:optional ont::Purpose (f::situation (f::aspect f::dynamic)))
             (:optional ONT::Affected ((? aff f::phys-obj f::abstr-obj f::situation)))
                                        ;            (:optional ONT::Purpose-implicit ((? pi f::phys-obj f::abstr-obj f::situation)))
             (:optional ONT::REASON ((? pi f::phys-obj f::abstr-obj f::situation)))
             (:OPTIONAL ONT::GROUND)
             (:optional ont::standard)
             )
 )



(define-type ONT::STATE-RESULTING-FROM
    :parent ont::property-val
    :comment "relates an event to the object changed by that event (e.g., eventually
               we could map this to a property based on ontological/commonsense knowledge"
    :arguments ((:essential GROUND (f::situation (F::type ont::event-of-change)))
		))

;; higher-level type for evaluation
(define-type ont::evaluation-attribute-val
    :parent ont::property-val
  :wordnet-sense-keys ("disadvantageous%3:00:00::" "poor%3:00:02::")
    :arguments ((:REQUIRED ONT::FIGURE (?any (F::tangible +))))
    :comment "properties which need an observer to be recognized -- subjective to the observer"
    :sem (F::abstr-obj (F::scale ont::evaluation-scale ))
    )

;; fresh, stale
(define-type ont::freshness-val
 :parent ont::evaluation-attribute-val 
 :comment "property of items made or obtained recently/a while back"
 :sem (F::abstr-obj (F::scale ont::freshness-scale ))
)

(define-type ont::fresh-val
 :parent ont::freshness-val 
 :wordnet-sense-keys ("unsoured%3:00:00::" "fresh%3:00:01" "fresh%3:00:02")
:sem (F::abstr-obj (F::scale ont::fresh-scale ))
)

(define-type ont::not-fresh-val
 :parent ont::freshness-val 
 :wordnet-sense-keys ("soured%3:00:00::" "stale%3:00:00")
:sem (F::abstr-obj (F::scale ont::not-fresh-scale ))
)


; adjectives to do with cost
(define-type ont::cost-val
 :parent ont::evaluation-attribute-val 
 :sem (F::abstr-obj (F::scale ont::cost-scale ))
 :comment "properties having to do with evaluation of cost"
)

(define-type ont::expensive-val
 :parent ont::cost-val
 :sem (F::abstr-obj (F::scale ont::expensive-scale ))
 :wordnet-sense-keys ("upmarket%3:00:00::" "expensive%3:00:00" "pricy%5:00:00:expensive:00")
)

(define-type ont::not-expensive-val
 :parent ont::cost-val
 :sem (F::abstr-obj (F::scale ont::not-expensive-scale ))
 :wordnet-sense-keys ("downmarket%3:00:00::" "cheap%3:00:00")
)

(define-type ont::no-cost-val
 :parent ont::cost-val
 :wordnet-sense-keys ("complimentary%3:00:00::" "free%5:00:00:unpaid:00")
)

; good, bad, terrible, great
(define-type ont::acceptability-val
 :parent ont::evaluation-attribute-val 
 :comment "properties having to do with good vs. bad"
 :sem (F::abstr-obj (F::scale ont::acceptability-scale ))
)

(define-type ont::good
 :parent ont::acceptability-val 
 :wordnet-sense-keys ("advantageous%3:00:00::" "savory%3:00:00::" "savoury%3:00:04::" "palatable%3:00:00::" "toothsome%3:00:00::" "propitious%3:00:00::" "auspicious%3:00:00::" "inauspicious%3:00:00::" "unfortunate%3:00:04::" "adequate%5:00:00:satisfactory:00" "nice%3:00:00" "good%3:00:01" "satisfactory%5:00:00:good:01" "all_right%5:00:00:satisfactory:00" "good%5:00:00:nice:00" "satisfactory%3:00:00" "acceptable%3:00:00" "favorable%3:00:02" "alright%5:00:00:satisfactory:00")
 ; Words: (W::GOOD W::GREAT W::FINE W::NICE W::ACCEPTABLE W::ALRIGHT W::SATISFACTORY W::SUPERB W::OKAY W::OK W::PEACHY W::FAVORABLE W::BEARABLE W::TOLERABLE W::SUPPORTABLE ALL_RIGHT)
 ; Antonym: ONT::bad (W::BAD W::TERRIBLE W::AWFUL W::NASTY W::DREADFUL W::UNACCEPTABLE W::ROTTEN W::UNSUPPORTABLE W::UNBEARABLE W::INTOLERABLE W::INSUFFERABLE W::UNFAVORABLE W::MEDIOCRE W::LOUSY)
 :sem (F::abstr-obj (F::scale ont::goodness-scale))
)

(define-type ont::great-val
 :parent ont::good 
 :wordnet-sense-keys ("glorious%3:00:00::" "superb%5:00:00:good:01" "bang-up%5:00:00:good:01" "great%5:00:01:extraordinary:00" "phenomenal%5:00:00:extraordinary:00" "fantastic%5:00:00:extraordinary:00" "ideal%3:00:00:perfect:00" "perfect%3:00:00" "opulent%5:00:00:rich:03" "opulently%4:02:00")
 :sem (F::abstr-obj (F::scale ont::goodness-scale))
)

(define-type ont::favorite-val
 :parent ont::good
 :wordnet-sense-keys ("favourite%3:00:00:popular:00" "favourite%3:00:00:loved:00")
)


(define-type ont::bad
 :parent ont::acceptability-val 
 :wordnet-sense-keys ("unsound%3:00:00::" "unpropitious%3:00:00::" "dirty%5:00:00:nasty:00" "unacceptable%5:00:00:unsatisfactory:00" "unacceptable%3:00:00" "unfavorable%3:00:02" "icky%5:00:00:bad:00" "unfavorable%5:00:00:bad:00" "mediocre%5:00:00:bad:00" "bad%3:00:00" "unsatisfactory%3:00:00")
 ; Words: (W::BAD W::TERRIBLE W::AWFUL W::NASTY W::DREADFUL W::UNACCEPTABLE W::ROTTEN W::UNSUPPORTABLE W::UNBEARABLE W::INTOLERABLE W::INSUFFERABLE W::UNFAVORABLE W::MEDIOCRE W::LOUSY)
 ; Antonym: ONT::good (W::GOOD W::GREAT W::FINE W::NICE W::ACCEPTABLE W::ALRIGHT W::SATISFACTORY W::SUPERB W::OKAY W::OK W::PEACHY W::FAVORABLE W::BEARABLE W::TOLERABLE W::SUPPORTABLE ALL_RIGHT)
:sem (F::abstr-obj (F::scale ont::badness-scale))
)

(define-type ont::awful-val
 :parent ont::bad 
 :wordnet-sense-keys ("nasty%3:00:00" "awful%5:00:00:bad:00")
)

(define-type ont::neutral-acceptability-val
 :parent ont::acceptability-val 
)

(define-type ont::tolerability-val
 :parent ont::evaluation-attribute-val 
 :sem (F::abstr-obj (F::scale ont::tolerability-scale))
)

(define-type ont::not-tolerable-val
 :parent ont::tolerability-val 
 :wordnet-sense-keys ("impossible%5:00:00:intolerable:00" "unsupportable%5:00:00:intolerable:00" "intolerable%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::tolerability-scale) (f::orientation f::neg))
)

(define-type ont::tolerable-val
 :parent ont::tolerability-val 
 :wordnet-sense-keys ("bearable%5:00:00:tolerable:00" "tolerable%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::tolerability-scale) (f::orientation f::pos))
)

;; recommendable
;(define-type ont::recommendability-val
; :parent ont::evaluation-attribute-val
;)

(define-type ONT::recommendability-val
  :parent ONT::evaluation-attribute-val
  )

(define-type ont::recommendable-val
 :parent ONT::recommendability-val
 :wordnet-sense-keys ("advisable%3:00:00::")
 :sem (F::abstr-obj (F::scale ont::recommendability-scale))
)

;; preferable
(define-type ont::preference-val
 :parent ont::evaluation-attribute-val 
)

(define-type ont::preferred-val
 :parent ont::preference-val
 :wordnet-sense-keys ("preferable%5:00:00:desirable:00" )
)

; (in)appropriate, suitable
(define-type ont::appropriateness-val
    :parent ont::evaluation-attribute-val
    :sem (F::abstr-obj (F::scale ont::appropriateness-scale))
    )

(define-type ont::appropriate-val
 :parent ont::appropriateness-val 
 :wordnet-sense-keys ("apropos%3:00:00::" "appropriate%3:00:00" "pat%5:00:00:appropriate:00" "proper%5:00:00:appropriate:00" "suitable%5:00:00:fit:02" "fit%3:00:02")
 :sem (F::abstr-obj (F::scale ont::appropriate-scale))
)

(define-type ont::not-appropriate-val
 :parent ont::appropriateness-val 
 :wordnet-sense-keys ("inexpedient%3:00:00::" "improper%3:00:00::" "malapropos%3:00:00::" "inappropriate%3:00:00" "improper%5:00:00:inappropriate:00" "immoderate%3:00:00" "immoderately%4:02:00" "immoderately%4:02:02" )
 :sem (F::abstr-obj (F::scale ont::appropriate-scale))
)

;; convenient, inconveninet
(define-type ont::convenience-val
 :parent ont::evaluation-attribute-val 
 :sem (F::abstr-obj (F::scale ont::convenience-scale))
)

(define-type ont::convenient-val
 :parent ont::convenience-val 
 :wordnet-sense-keys ("convenient%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::convenient-scale))
)

(define-type ont::not-convenient-val
 :parent ont::convenience-val 
 :wordnet-sense-keys ("incommodious%3:00:00::" "inconvenient%3:00:00" "awkward%5:00:00:inconvenient:00" )
 :sem (F::abstr-obj (F::scale ont::convenient-scale))
)

;; particular, specific
(define-type ont::specificity-val
 :parent ont::evaluation-attribute-val 
)

(define-type ont::specific-val
 :parent ont::specificity-val 
 :wordnet-sense-keys ("nonarbitrary%3:00:00::" "unarbitrary%3:00:00::" "determinate%3:00:01::" "unspecified%3:00:00::" "specific%3:00:00" "particular%5:00:00:specific:00" "specified%3:00:00")
)

(define-type ont::general-val
 :parent ont::specificity-val 
 :wordnet-sense-keys ("gross%3:00:00::" "general%3:00:00" "overall%5:00:00:general:00" "nonspecific%3:00:00" )
)

;; clean, dirty
(define-type ont::cleanliness-val
 :parent ont::evaluation-attribute-val 
 :sem (F::abstr-obj (F::scale ont::cleanliness-scale))
)

(define-type ont::clean-val
 :parent ont::cleanliness-val 
 :wordnet-sense-keys ("clean%3:00:01")
 :sem (F::abstr-obj (F::scale ont::clean-scale))
)

(define-type ont::dirty-val
 :parent ont::cleanliness-val 
 :wordnet-sense-keys ("dirty%3:00:01" )
 :sem (F::abstr-obj (F::scale ont::unclean-scale))
)

; (un)lucky, (un)fortunate
(define-type ont::luckiness-val
 :parent ont::evaluation-attribute-val 
 :sem (F::abstr-obj (F::scale ont::luckiness-scale))
)

(define-type ont::lucky
 :parent ont::luckiness-val 
 :wordnet-sense-keys ("lucky%3:00:00::" "fortunate%3:00:00" "lucky%5:00:00:fortunate:00" )
 ; Words: (W::LUCKY W::FORTUNATE)
 ; Antonym: ONT::unlucky (W::UNFORTUNATE W::UNLUCKY)
 :sem (F::abstr-obj (F::scale ont::luckiness-scale) (F::orientation f::pos))
)

(define-type ont::unlucky
 :parent ont::luckiness-val 
 :wordnet-sense-keys ("unlucky%3:00:00::" "luckless%3:00:00::" "unfortunate%3:00:00" "doomed%5:00:00:unfortunate:00" )
 ; Words: (W::UNFORTUNATE W::UNLUCKY)
 ; Antonym: ONT::lucky (W::LUCKY W::FORTUNATE)
 :sem (F::abstr-obj (F::scale ont::luckiness-scale) (F::orientation f::neg))
)

;; able, capable, competent
(define-type ont::ability-val
 :parent ont::evaluation-attribute-val 
 :comment "evaluation of ability or capability to do something"
 :sem (F::abstr-obj (F::scale ont::ability-scale))
)

(define-type ont::able
 :parent ont::ability-val 
 :wordnet-sense-keys ("capable%3:00:02::" "able%3:00:00::" "capable%3:00:00" "able%5:00:00:capable:00" "able%5:00:00:competent:00" "competent%3:00:00")
 ; Words: (W::ABLE W::CAPABLE W::COMPETENT)
 ; Antonym: ONT::unable (W::UNABLE W::INCAPABLE W::INCOMPETENT)
 :sem (F::abstr-obj (F::scale ont::able-scale))
)

(define-type ont::dexterous
 :parent ont::able
 :wordnet-sense-keys ("adroit%3:00:00")
)

(define-type ont::unable
 :parent ont::ability-val 
 :wordnet-sense-keys ("maladroit%3:00:00::" "incapable%3:00:02::" "untalented%3:00:00::" "talentless%3:00:00::" "unable%3:00:00::" "incompetent%3:00:00" "incapable%3:00:00" "unable%5:00:00:incapable:00" )
 ; Words: (W::UNABLE W::INCAPABLE W::INCOMPETENT)
 ; Antonym: ONT::able (W::ABLE W::CAPABLE W::COMPETENT)
 :sem (F::abstr-obj (F::scale ont::not-able-scale))
)

;; (un)comfortable
(define-type ont::comfort-val
 :parent ont::evaluation-attribute-val 
 :sem (F::abstr-obj (F::scale ont::comfort-scale))
)

(define-type ont::comfortable
 :parent ont::comfort-val 
 :wordnet-sense-keys ("comfortable%3:00:01::" "cozy%5:00:00:comfortable:00" "comfortable%3:00:00" )
 ; Words: (W::COMFORTABLE W::COMFY W::COZY)
 ; Antonym: ONT::uncomfortable (W::UNCOMFORTABLE W::UNEASY)
 :sem (F::abstr-obj (F::scale ont::comfortable-scale))
)

(define-type ont::uncomfortable
 :parent ont::comfort-val 
 :wordnet-sense-keys ("uncomfortable%3:00:00" "awkward%5:00:00:uncomfortable:01" "uncomfortable%3:00:01" )
 ; Words: (W::UNCOMFORTABLE W::UNEASY)
 ; Antonym: ONT::COMFORTABLE (W::COMFORTABLE W::COMFY W::COZY)
 :sem (F::abstr-obj (F::scale ont::not-comfortable-scale))
)

;; harmless, harmful
(define-type ont::harmfulness-val
 :parent ont::evaluation-attribute-val 
 :comment "regarding capability of harm or injury (compare to ont::safety-val)"
 :sem (F::abstr-obj (F::scale ont::harmfulness-scale))
)

(define-type ont::benign-val
 :parent ont::harmfulness-val 
 :wordnet-sense-keys ("nontoxic%3:00:00::" "atoxic%3:00:00::" "innocuous%3:00:00::" "benign%3:00:00::" "benignant%3:00:04::" "harmless%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::harmfulness-scale) (f::orientation f::neg))
)

(define-type ont::harmful-val
 :parent ont::harmfulness-val 
 :wordnet-sense-keys ("destructive%3:00:00::" "toxic%3:00:00::" "noxious%3:00:00::" "damaging%5:00:00:harmful:00" "harmful%3:00:00" "ruinous%5:00:00:harmful:00" )
 :sem (F::abstr-obj (F::scale ont::harmfulness-scale) (f::orientation f::pos))
)

;;; secure, safe
(define-type ont::safety-val
 :parent ont::evaluation-attribute-val 
 :comment "regarding liability or exposure to danger, risk or peril (compare to ont::harmfulness-val)"
 :sem (F::abstr-obj (F::scale ont::safety-scale))
)

(define-type ont::dangerous
 :parent ont::safety-val 
 :wordnet-sense-keys ("dangerous%3:00:00" "insecure%3:00:02" )
 ; Words: (W::DANGEROUS W::UNSAFE)
 ; Antonym: ONT::SAFE (W::SAFE W::SECURE)
 :sem (F::abstr-obj (F::scale ont::unsafe-scale))
)

(define-type ont::safe
 :parent ont::safety-val 
 :wordnet-sense-keys ("secure%3:00:03::" "secure%3:00:02" "safe%3:00:01" )
 ; Words: (W::SAFE W::SECURE)
 ; Antonym: ONT::DANGEROUS (W::DANGEROUS W::UNSAFE)
 :sem (F::abstr-obj (F::scale ont::safe-scale))
)

; noticeable, noteworthy
(define-type ont::attention-worthy-val
 :parent ont::evaluation-attribute-val 
 :sem (F::abstr-obj (F::scale ont::noticeability-scale))
)

(define-type ont::noticeable
 :parent ont::attention-worthy-val 
 :wordnet-sense-keys ("protrusive%3:00:00::" "theatrical%3:00:00::" "appealing%3:00:00::" "ostentatious%3:00:00::" "pretentious%3:00:04::" "conspicuous%3:00:00" "noticeable%3:00:00" "marked%5:00:00:noticeable:00" "outstanding%5:00:00:conspicuous:00" "big%5:00:00:conspicuous:00" "obtrusive%3:00:00" )
 ; Words: (W::PROMINENT W::STRIKING W::NOTICEABLE W::PRONOUNCED W::OBTRUSIVE W::CONSPICUOUS)
 ; Antonym: ONT::UNOBTRUSIVE (W::UNOBTRUSIVE W::INCONSPICUOUS W::UNNOTICEABLE)
 :sem (F::abstr-obj (F::scale ont::noticeability-scale) (f::orientation f::pos))
)

(define-type ont::unnoticeable
 :parent ont::attention-worthy-val 
 :wordnet-sense-keys ("unnoticed%3:00:00::" "unappealing%3:00:00::" "inglorious%3:00:00::" "unostentatious%3:00:00::" "unpretentious%3:00:04::" "unpretending%3:00:00::" "inconsiderable%3:00:00::" "unnoticeable%3:00:00::" "unobtrusive%3:00:00" "obscure%5:00:00:inconspicuous:00" "inconspicuous%3:00:00" )
 ; Words: (W::UNOBTRUSIVE W::INCONSPICUOUS W::UNNOTICEABLE)
 ; Antonym: ONT::OUTSTANDING (W::PROMINENT W::STRIKING W::NOTICEABLE W::PRONOUNCED W::OBTRUSIVE W::CONSPICUOUS)
 :sem (F::abstr-obj (F::scale ont::noticeability-scale) (f::orientation f::neg))
)

;; (un)important, (un)necessary, (in)significant, central, critical, principal
(define-type ont::importance-val
    :wordnet-sense-keys ("immodest%3:00:02" "immodestly%4:02:00")
    :parent ont::evaluation-attribute-val 
    :comment "of primary (i.e., major, significant), secondary (i.e., minor), or no importance"
    :sem (F::abstr-obj (F::scale ont::importance-scale))
    )

(define-type ont::primary
 :parent ont::importance-val 
 :wordnet-sense-keys ("primary%3:00:00::" "operative%3:00:00::" "major%3:00:07::" "major%3:00:02::" "chief%5:00:02:important:00" "important%3:00:00" "all-important%5:00:00:important:00" "major%3:00:06" "cardinal%5:00:00:important:00" "basal%5:00:00:essential:00" "crucial%3:00:00" "significant%3:00:00" "imperative%3:00:00")
 ; Words: (W::IMPORTANT W::MAIN W::MAJOR W::NECESSARY W::CENTRAL W::SERIOUS W::SIGNIFICANT W::ESSENTIAL W::PRIMARY W::SENIOR W::CRITICAL W::VITAL W::CRUCIAL W::INDISPENSABLE)
 ; Antonym: ONT::SECONDARY (W::SECONDARY W::MINOR W::JUNIOR W::UNNECESSARY W::UNIMPORTANT W::INSIGNIFICANT)
 :comment "important; of primary importance"
 :sem (F::abstr-obj (F::scale ont::important-scale))
)

(define-type ont::urgent-val
 :parent ont::primary
 :wordnet-sense-keys ("noncritical%3:00:01::" "noncrucial%3:00:04::" "urgent%5:00:00:imperative:00" "serious%5:00:00:critical:03" "critical%3:00:03" "desperate%5:00:00:imperative:00" "seriously%4:02:00" "urgently%4:02:00" "desperately%4:02:01")
 :comment "time-sensitive or critical"
)

(define-type ont::secondary
 :parent ont::importance-val 
 :wordnet-sense-keys ("secondary%3:00:01::" "incidental%3:00:00::" "incident%3:00:00::" "minor%3:00:07::" "subordinate%3:00:01::" "low-level%3:00:04::" "minor%3:00:06" "junior%3:00:00" "insignificant%5:00:00:minor:06")
 ; Words: (W::SECONDARY W::MINOR W::JUNIOR W::UNNECESSARY W::UNIMPORTANT W::INSIGNIFICANT)
 ; Antonym: ONT::SERIOUS (W::IMPORTANT W::MAIN W::MAJOR W::NECESSARY W::CENTRAL W::SERIOUS W::SIGNIFICANT W::ESSENTIAL W::PRIMARY W::SENIOR W::CRITICAL W::VITAL W::CRUCIAL W::INDISPENSABLE)
 :comment "holding some but minor importance"
 :sem (F::abstr-obj (F::scale ont::important-scale))
)

(define-type ont::not-important-val
 :parent ont::importance-val 
 :wordnet-sense-keys ("insignificant%3:00:00" "unimportant%3:00:00" )
 :comment "devoid of significance or importance"
 :sem (F::abstr-obj (F::scale ont::not-important-scale))
)

;; necessary, unnecessary
(define-type ont::necessity-val
 :parent ont::evaluation-attribute-val 
 :sem (F::abstr-obj (F::scale ont::necessity-scale ))
)

(define-type ont::necessary
 :parent ont::necessity-val 
 :wordnet-sense-keys ("essential%5:00:00:necessary:00" "necessary%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::necessity-scale) (F::orientation f::pos))
)

(define-type ont::not-necessary-val
 :parent ont::necessity-val 
 :wordnet-sense-keys ("extrinsic%3:00:00::" "unnecessary%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::necessity-scale) (F::orientation f::neg))
)

;; senior, junior
(define-type ont::seniority-val
 :parent ont::evaluation-attribute-val 
)

(define-type ont::senior-val
 :parent ont::seniority-val 
 :wordnet-sense-keys ("senior%3:00:00" )
)

(define-type ont::junior-val
 :parent ont::seniority-val 
 :wordnet-sense-keys ("junior-grade%5:00:00:junior:00" )
)

;; conventional, traditional
(define-type ont::conventionality-val
 :parent ont::evaluation-attribute-val 
 :sem (F::abstr-obj (F::scale ont::conventionality-scale))
)

(define-type ont::conventional-val
 :parent ont::conventionality-val 
 :wordnet-sense-keys ("traditional%3:00:00::" "conventional%5:00:00:orthodox:00" "orthodox%3:00:00" "conventional%3:00:00" "traditional%5:00:00:orthodox:00" "conventional%3:00:01")
 :sem (F::abstr-obj (F::scale ont::conventional-scale))
)

;; new (original),fresh, innovative
(define-type ont::novelty-val
 :parent ont::conventionality-val 
 :wordnet-sense-keys ("innovative%5:00:00:original:00" "novel%5:00:00:new:00" "fresh%5:00:00:new:00" "fresh%5:00:00:original:00" "novel%5:00:00:original:00" "revolutionary%5:00:00:new:00" "original%3:00:00")
 :sem (F::abstr-obj (F::scale ont::not-conventional-scale))
)

;; familiar, known
(define-type ont::familiarity-val
 :parent ont::evaluation-attribute-val 
  ;;:arguments ((:optional ont::stimulus))
 :sem (F::abstr-obj (F::scale ont::familiarity-scale))
)

(define-type ont::familiar-val
 :parent ont::familiarity-val 
 :wordnet-sense-keys ("familiar%3:00:02" "familiar%3:00:00" "known%3:00:00")
 :sem (F::abstr-obj (F::scale ont::familiar-scale))
)

(define-type ont::unfamiliar-val
 :parent ont::familiarity-val 
 :wordnet-sense-keys ("unknown%3:00:00::" "unfamiliar%3:00:00" "strange%5:00:01:unfamiliar:00")
 :sem (F::abstr-obj (F::scale ont::not-familiar-scale))
)

;; typical (normal, usual, stereotypical); atypical (uncommon, unusual, strange, exceptional, unique)
(define-type ont::typicality-val
 :parent ont::evaluation-attribute-val 
 :comment "abiding by or breaking with customary or usual practices"
 :sem (F::abstr-obj (F::scale ont::typicality-scale))
)

(define-type ont::typical-val
 :parent ont::typicality-val 
 :wordnet-sense-keys ("standard%3:00:01::" "characteristic%3:00:00::" "neutral%3:00:02::" "standard%3:00:02::" "customary%5:00:00:usual:00" "typical%5:00:00:normal:01" "everyday%5:00:00:ordinary:00" "run-of-the-mill%5:00:00:ordinary:00" "typical%3:00:00" "regular%5:00:00:typical:00" "regular%5:00:00:normal:01" "normal%3:00:01" "common%5:00:00:ordinary:00" "ordinary%5:00:02:common:01" "usual%3:00:00" "ordinary%3:00:00" "standard%5:00:00:common:01" "common%3:00:01" )
 :sem (F::abstr-obj (F::scale ont::typical-scale))
)

(define-type ont::stereotypical-val
 :parent ont::typical-val 
 :wordnet-sense-keys ("stereotypical%5:00:00:conventional:01" "formulaic%5:00:00:conventional:00" "generic%3:01:00" "white-bread%5:00:00:conventional:01")
)

(define-type ont::atypical-val
 :parent ont::typicality-val 
 :wordnet-sense-keys ("supernatural%3:00:00::" "nonstandard%3:00:00::" "unorthodox%3:00:00::" "uncommon%3:00:00" "unconventional%3:00:01" "unorthodox%5:00:00:unconventional:00" "unusual%3:00:00" "unusual%5:00:00:uncommon:00" "unconventional%3:00:00" "nonstandard%3:00:02" "irregular%3:00:00")
 :sem (F::abstr-obj (F::scale ont::not-typical-scale))
)

(define-type ont::strange
 :parent ont::atypical-val 
 :wordnet-sense-keys ("paranormal%3:00:00::" "irregular%5:00:00:abnormal:00" "weird%5:00:00:strange:00" "freaky%5:00:00:strange:00" "bizarre%5:00:00:unconventional:01" "funky%5:00:00:unconventional:01" "atypical%3:00:00" "atypical%5:00:00:abnormal:00" "abnormal%3:00:00" "odd%5:00:00:unusual:00" "curious%5:00:00:strange:00" "strange%3:00:00")
 ; Words: (W::STRANGE W::ODD W::UNUSUAL W::REMARKABLE W::EXTRAORDINARY W::EXCEPTIONAL W::PECULIAR W::WEIRD W::BIZARRE W::UNFAMILIAR W::ABNORMAL W::FUNKY W::UNCONVENTIONAL W::UNCOMMON W::SINGULAR W::FREAKY W::ATYPICAL W::OUTLANDISH W::UNORTHODOX)
 ; Antonym: ONT::COMMON (W::COMMON W::NORMAL W::USUAL W::REGULAR W::ORDINARY W::STANDARD W::FAMILIAR W::TYPICAL W::CONVENTIONAL W::ORTHODOX W::UNREMARKABLE W::UNEXCEPTIONAL W::ROUTINE)
)

(define-type ont::exceptional-val
 :parent ont::atypical-val 
 :wordnet-sense-keys ("superhuman%3:00:00::" "precocious%3:00:00::" "remarkable%5:00:00:extraordinary:00" "exceptional%5:00:00:extraordinary:00" "special%5:00:00:uncommon:00" "singular%5:00:00:extraordinary:00" "extraordinary%3:00:00" )
)

(define-type ONT::uniqueness-VAL
 :parent ONT::atypical-val
 )

;; basic, fundamental, inherent
(define-type ont::basicness-val
 :parent ont::evaluation-attribute-val 
)

(define-type ont::basic-val
 :parent ont::basicness-val 
 :wordnet-sense-keys ("underived%3:00:00::" "basic%3:00:00" "fundamental%5:00:00:basic:00" "elementary%5:00:00:basic:00" "essential%3:00:00")
 :sem (F::abstr-obj (F::scale ont::basic-scale))
 :comment "Having to do with the fundamentals or the essentials"
)

(define-type ont::intrinsic-val
 :parent ont::basicness-val 
 :wordnet-sense-keys ("built-in%5:00:00:intrinsic:00" "intrinsic%3:00:00" )
)

(define-type ont::plain-val
 :parent ont::basicness-val 
 :wordnet-sense-keys ("unrhetorical%3:00:00::" "plain%3:00:02::" "unpatterned%3:00:04::" "undramatic%3:00:00::" "plain%3:00:01" "stark%5:00:00:plain:01" )
 :comment "inelaborate, simple and plain"
 :sem (F::abstr-obj (F::scale ont::plain-scale) (f::orientation f::pos))
)

(define-type ont::mere-val
 :parent ont::plain-val 
 :wordnet-sense-keys ("mere%5:00:00:specified:00")
 :comment "being nothing other than what's specified"
)

;; fanciful and elaborate
(define-type ont::not-plain-val
 :parent ont::basicness-val 
 :wordnet-sense-keys ("rhetorical%3:00:00::" "dramatic%3:00:00::" "fanciful%5:00:00:fancy:00" "fancy%3:00:00" "elaborate%5:00:00:fancy:00")
 :sem (F::abstr-obj (F::scale ont::plain-scale) (f::orientation f::neg))
)

;; explicit, implicit
(define-type ont::explicitness-val
 :parent ont::evaluation-attribute-val
 :sem (F::abstr-obj (F::scale ont::explicitness-scale))
)

(define-type ont::explicit-val
 :parent ont::explicitness-val
 :wordnet-sense-keys ("explicit%3:00:00")
 :sem (F::abstr-obj (F::scale ont::explicit-scale))
)

(define-type ont::implicit-val
 :parent ont::explicitness-val 
 :wordnet-sense-keys ("underlying%5:00:00:implicit:00" "implicit%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::implicit-scale))
)

;; superior, inferior
(define-type ont::quality-val
 :parent ont::evaluation-attribute-val 
 :sem (F::abstr-obj (F::scale ont::quality-level-scale))
)

(define-type ont::superior-val
 :parent ont::quality-val 
 :wordnet-sense-keys ("superior%3:00:01::" "rich%3:00:03::" "premium%5:00:00:superior:02" "superior%3:00:02" "supreme%5:00:00:superior:02" "sterling%5:00:00:superior:02" )
 :sem (F::abstr-obj (F::scale ont::superiority-scale))
)

(define-type ont::substandard-val
 :parent ont::quality-val 
 :wordnet-sense-keys ("inferior%3:00:01::" "unfit%3:00:02::" "second-rate%5:00:00:inferior:02" "low-grade%5:00:00:inferior:02" "substandard%5:00:00:nonstandard:02" "inferior%3:00:02" )
 :sem (F::abstr-obj (F::scale ont::inferiority-scale))
)


;; dependable, reliable
(define-type ont::reliability-val
 :parent ont::evaluation-attribute-val
)

(define-type ont::reliable
 :parent ont::reliability-val 
 :wordnet-sense-keys ("sound%3:00:00::" "trustful%3:00:00::" "trusting%3:00:02::" "trustworthy%3:00:00" "dependable%5:00:00:trustworthy:00" "reliable%3:00:00" )
 ; Words: (W::RELIABLE W::TRUSTWORTHY W::DEPENDABLE)
 ; Antonym: ONT::UNRELIABLE (W::UNCERTAIN W::UNRELIABLE)
 :sem (F::abstr-obj (F::scale ont::reliability-scale) (f::orientation f::pos))
)

(define-type ont::unreliable
 :parent ont::reliability-val 
 :wordnet-sense-keys ("untrustworthy%3:00:00::" "untrusty%3:00:00::" "uncertain%5:00:00:unreliable:00" "unreliable%3:00:00" )
 ; Words: (W::UNCERTAIN W::UNRELIABLE)
 ; Antonym: ONT::RELIABLE (W::RELIABLE W::TRUSTWORTHY W::DEPENDABLE)
 :sem (F::abstr-obj (F::scale ont::reliability-scale) (f::orientation f::neg))
)


;; worthy/valuable, unworthy/worthless
(define-type ont::worthiness-val
 :parent ont::evaluation-attribute-val 
 :comment "evaluation attributes dealing with the value or worth of something"
 :sem (F::abstr-obj (F::scale ont::worthiness-scale))
)

(define-type ont::worthy-val
 :parent ont::worthiness-val 
 :wordnet-sense-keys ("valuable%3:00:00::" "estimable%3:00:00::" "merited%3:00:00::" "deserved%3:00:00::" "honorable%3:00:00::" "honourable%3:00:00::" "worthwhile%5:00:00:worthy:00" "worthy%3:00:00" "valuable%5:00:00:worthy:00" "worthy%5:00:00:fit:02")
 :sem (F::abstr-obj (F::scale ont::worthiness-scale) (f::orientation f::pos))
)

(define-type ont::not-worthy-val
 :parent ont::worthiness-val 
 :wordnet-sense-keys ("dishonorable%3:00:00::" "dishonourable%3:00:00::" "unworthy%3:00:00" "worthless%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::worthiness-scale) (f::orientation f::neg))
)


;; judgement
(define-type ont::judgement-val
 :parent ont::evaluation-attribute-val
)

;; morality, righteousness, virtuousness, principle, purity
(define-type ont::morality-val
 :parent ont::judgement-val
 :comment "characterized by morality, righteousness, virtuousness, and principle"
 :sem (F::abstr-obj (F::scale ont::morality-scale))
)

(define-type ont::moral-val
 :parent ont::morality-val 
 :wordnet-sense-keys ("just%3:00:00::" "scrupulous%3:00:00::" "unblemished%3:00:00::" "unmarred%3:00:00::" "unmutilated%3:00:00::" "moral%3:00:00" "virtuous%3:00:00" "chaste%3:00:00" "good%3:00:02" "incorrupt%3:00:00" "principled%3:00:00" "pure%3:00:01" "regenerate%3:00:00" "right%3:00:01" "righteous%3:00:00" "straight%3:00:04")
 :sem (F::abstr-obj (F::scale ont::morality-scale) (f::orientation f::pos))
)

(define-type ont::not-moral-val
 :parent ont::morality-val
 :wordnet-sense-keys ("unrighteous%3:00:00::" "unethical%3:00:00::"  "immoral%3:00:00" "immoral%3:00:00:wrong:01" "unprincipled%3:00:00" "unregenerate%3:00:00" "wicked%3:00:00" "corrupt%3:00:00" "crooked%3:00:02" "evil%3:00:00" "illicit%3:00:00" "impure%3:00:01" "licit%3:00:00" "wrong%3:00:01" "unchaste%3:00:00")
 :sem (F::abstr-obj (F::scale ont::morality-scale) (f::orientation f::neg))
)

;; fairness, partiality, bias, and objectivity
(define-type ont::bias-val
 :parent ont::judgement-val
 :comment "characterized by fairness, partiality, bias and objectivity"
 :sem (F::abstr-obj (F::scale ont::partiality-scale))
)

(define-type ont::biased-val
 :parent ont::bias-val
 :wordnet-sense-keys ("unjust%3:00:00::" "unfair%3:00:00::" "unjust%3:00:04::" "subjective%3:00:00" "partial%3:00:01")
 :sem (F::abstr-obj (F::scale ont::partiality-scale) (f::orientation f::pos))
)

(define-type ont::prejudiced-val
 :parent ont::biased-val
 :wordnet-sense-keys ("prejudiced%3:00:00" "judgmental%3:00:00")
 :comment "bias characterized by dislike or distrust"
)

(define-type ont::not-biased-val
 :parent ont::bias-val
 :wordnet-sense-keys ("equitable%3:00:00::" "just%3:00:02::" "impartial%3:00:00" "objective%3:00:00" "fair%3:00:03" "unprejudiced%3:00:00" "nonjudgmental%3:00:00")
 :sem (F::abstr-obj (F::scale ont::partiality-scale) (f::orientation f::neg))
)

;; chic, classy, stylish vs. unstylish, tasteless
(define-type ont::aesthetic-judgement-val
 :parent ont::judgement-val 
 :sem (F::abstr-obj (F::scale ont::aesthetic-tastefulness-scale))
)

(define-type ont::good-aesthetic-judgement-val
 :parent ont::aesthetic-judgement-val 
 :wordnet-sense-keys ("refined%3:00:01::" "chic%5:00:00:stylish:00" "classy%5:00:00:stylish:00" "stylish%3:00:00" "tasteful%3:00:02" "elegant%3:00:00" "fashionable%3:00:00")
 :sem (F::abstr-obj (F::scale ont::aesthetic-tastefulness-scale) (f::orientation f::pos))
)

(define-type ont::bad-aesthetic-judgement-val
 :parent ont::aesthetic-judgement-val 
 :wordnet-sense-keys ("unrefined%3:00:01::" "inelegant%3:00:00::" "unstylish%3:00:00" "tasteless%3:00:02" "unfashionable%3:00:00")
 :sem (F::abstr-obj (F::scale ont::aesthetic-tastefulness-scale) (f::orientation f::neg))
)

;; beautiful, ugly
(define-type ont::beauty-val
 :parent ont::evaluation-attribute-val 
 :sem (F::abstr-obj (F::scale ont::beauty-scale))
)

(define-type ont::beautiful
 :parent ont::beauty-val 
 :wordnet-sense-keys ("lovely%5:00:00:beautiful:00" "beautiful%3:00:00" "pretty%5:00:00:beautiful:00" "gorgeous%5:00:00:beautiful:00" )
 ; Words: (W::BEAUTIFUL W::LOVELY W::PRETTY)
 ; Antonym: NIL (W::UGLY)
 :sem (F::abstr-obj (F::scale ont::beautiful-scale))
)

(define-type ont::ugly-val
 :parent ont::beauty-val 
 :wordnet-sense-keys ("ugly%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::ugly-scale))
)

;; offensive, inoffensive
(define-type ont::offensiveness-val
 :parent ont::evaluation-attribute-val 
 :sem (F::abstr-obj (F::scale ont::offensiveness-scale))
)

(define-type ont::offensive-val
 :parent ont::offensiveness-val 
 :wordnet-sense-keys ("uncomplimentary%3:00:00::" "contemptible%3:00:00::" "dirty%3:00:02::" "indecent%3:00:00::" "offensive%3:00:04" "offensive%3:00:02" "offensive%3:00:01" "distasteful%5:00:00:offensive:01" "wicked%5:00:00:offensive:01")
 :sem (F::abstr-obj (F::scale ont::offensiveness-scale) (f::orientation f::pos))
)

(define-type ont::not-offensive-val
 :parent ont::offensiveness-val 
 :wordnet-sense-keys ("clean%3:00:02::" "unobjectionable%3:00:02::" "euphemistic%3:00:00::" "inoffensive%3:00:05::" "inoffensive%3:00:02::" "unoffending%3:00:02::" "inoffensive%3:00:01" )
 :sem (F::abstr-obj (F::scale ont::offensiveness-scale) (f::orientation f::neg))
)

;; neat, tidy, (dis)orderly
(define-type ont::orderliness-val
 :parent ont::evaluation-attribute-val 
 :sem (F::abstr-obj (F::scale ont::orderliness-scale))
)

(define-type ONT::orderly-val
  :parent ONT::orderliness-val
  :wordnet-sense-keys ("arranged%3:00:00::" "ordered%3:00:04::" "orderly%3:00:00::")
  :comment "(orderly)"
  )

(define-type ont::tidy-val
 :parent ONT::orderly-val 
 :wordnet-sense-keys ("ungroomed%3:00:00::" "combed%3:00:00::" "tidy%5:00:00:groomed:00" "tidy%3:00:00" "neat%5:00:00:tidy:00" "uncluttered%5:00:00:tidy:00" "groomed%3:00:00")
 :sem (F::abstr-obj (F::scale ont::tidy-scale))
)

(define-type ONT::not-orderly-val
  :parent ONT::orderliness-val
  :wordnet-sense-keys ("disordered%3:00:00::" "unordered%3:00:00::")
  :comment "(disordered)"
  )

(define-type ont::messy-val
 :parent ONT::not-orderly-val 
 :wordnet-sense-keys ("disarranged%3:00:00::" "uncombed%3:00:00::" "messy%5:00:00:untidy:00" "untidy%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::messy-scale))
)

;; free vs. bound
(define-type ont::freedom-val
 :parent ont::evaluation-attribute-val 
)

(define-type ont::free-val
 :parent ont::freedom-val 
 :wordnet-sense-keys ("uncommitted%3:00:00::" "unencumbered%3:00:00::" "disenchanted%3:00:00::" "unconfined%3:00:00::" "unattached%3:00:00::" "uncommitted%3:00:04::" "unstuck%3:00:00::" "free%3:00:01" "loose%5:00:01:free:00" "free%3:00:00")
)

(define-type ont::not-free-val
 :parent ont::freedom-val 
 :wordnet-sense-keys ("confined%3:00:00::" "bound%3:00:01::" "attached%3:00:01::" "committed%3:00:04::" "servile%5:00:00:unfree:01" "unfree%3:00:01" "stuck%3:00:00" "bound%5:00:00:unfree:00" "unfree%3:00:00")
)

;; real vs. fake
(define-type ont::real-vs-fake-val
 :parent ont::evaluation-attribute-val
 :sem (F::abstr-obj (F::scale ont::real-vs-fake-scale))
)

(define-type ont::artificiality-val
 :parent ont::real-vs-fake-val 
 :comment "artificial vs. natural"
 :sem (F::abstr-obj (F::scale ont::artificiality-scale))
)

(define-type ont::artificial
 :parent ont::artificiality-val 
 :wordnet-sense-keys ("synthetic%5:00:00:artificial:00" "false%5:00:00:artificial:00" "artificial%3:00:00" "artificial%5:00:00:affected:01" "unreal%3:00:04" "faux%5:00:00:artificial:00" "imitation%5:00:02:artificial:00" "fake%5:00:00:artificial:00" "affected%3:00:01" "unnatural%3:00:00")
 :sem (F::abstr-obj (F::scale ont::artificial-scale))
)

;; natural, unnatural
(define-type ont::natural
 :parent ont::artificiality-val 
 :wordnet-sense-keys ("natural%3:00:03::" "artless%3:00:00::" "unaffected%3:00:01::" "natural%3:00:01::" "natural%3:00:02" )
 :arguments ((:REQUIRED ONT::FIGURE ((? lof f::phys-obj f::abstr-obj f::situation )))) 
 :sem (F::abstr-obj (F::scale ont::natural-scale))
)

(define-type ont::actuality-val
 :parent ont::real-vs-fake-val 
 ;:arguments ((:REQUIRED ONT::FIGURE (F::proposition (F::information F::mental-construct )))) 
 :comment "existing or occuring in fact vs. imagined or supposed"
 :sem (F::abstr-obj (F::scale ont::actuality-scale))
)

;; real, actual
(define-type ont::actual
 :parent ont::actuality-val 
 :wordnet-sense-keys ("realistic%3:00:00::" "actual%3:00:00::" "existent%3:00:04::" "real%3:00:00" "actual%5:00:00:real:00" "concrete%3:00:00")
 :sem (F::abstr-obj (F::scale ont::actual-scale))
)

;; fake, imaginary, hypothetical
;; see also ont::evaluation-val?
(define-type ont::nonactual
 :parent ont::actuality-val 
 :wordnet-sense-keys ("unrealistic%3:00:00::" "unreal%3:00:02::" "virtual%5:00:00:essential:00" "imaginary%5:00:00:unreal:00" "theoretical%3:00:00" "hypothetical%5:00:00:theoretical:00" "unreal%3:00:00" "abstract%3:00:00")
 :sem (F::abstr-obj (F::scale ont::non-actual-scale))
)

(define-type ont::authenticity-val
 :parent ont::real-vs-fake-val 
 :comment "truly what it is said to be vs. made as imitation"
 :sem (F::abstr-obj (F::scale ont::authenticity-scale))
)

(define-type ont::fake-val
 :parent ont::authenticity-val 
 :wordnet-sense-keys ("phoney%5:00:00:counterfeit:00" "fake%5:00:00:counterfeit:00" "counterfeit%3:00:00")
 :sem (F::abstr-obj (F::scale ont::authenticity-scale) (f::orientation f::neg))
)

(define-type ont::authentic-val
 :parent ont::authenticity-val 
 :wordnet-sense-keys ("genuine%3:00:00" "genuine%5:00:00:true:00" "authentic%5:00:00:genuine:00" "actual%5:00:00:true:00" "real%5:00:00:true:00" "true%5:00:00:real:02" "real%3:00:02")
 :sem (F::abstr-obj (F::scale ont::authenticity-scale) (f::orientation f::pos))
)

;; INFORMATION PROPERTY
(define-type ont::information-property-val
 :parent ont::property-val
  :wordnet-sense-keys ("deductive%3:00:00::" "qualitative%3:00:00::")
 :comment "properties regarding the evaluation of information, knowledge, or understanding (e.g. credible, correct, consistent)"
 :sem (F::abstr-obj (F::scale ont::information-property-scale))
)



(define-type ont::precision-val
 :parent ont::information-property-val 
 :sem (F::abstr-obj (F::scale ont::precision-scale))
)

(define-type ont::not-precise-val
 :parent ont::precision-val 
 :wordnet-sense-keys ("inexact%3:00:00::" "imprecise%3:00:00" "vague%3:00:04" )
 :sem (F::abstr-obj (F::scale ont::precision-scale) (f::orientation f::neg))
)

(define-type ont::precise
 :parent ont::precision-val 
 :wordnet-sense-keys ("exact%3:00:00::" "dead%5:00:00:precise:00" "precise%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::precision-scale) (f::orientation f::pos))
 ; Words: (W::DEAD W::PRECISE)
 ; Antonym: NIL (W::IMPRECISE)
)

;; meaningful, meaningless
(define-type ont::meaningfulness-val
 :parent ont::information-property-val
 :sem (F::abstr-obj (F::scale ont::meaningfulness-scale))
)

(define-type ont::meaningful-val
 :parent ont::meaningfulness-val 
 :wordnet-sense-keys ("meaningful%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::meaningfulness-scale) (f::orientation f::pos))
)

(define-type ont::not-meaningful-val
 :parent ont::meaningfulness-val 
 :wordnet-sense-keys ("unenlightening%3:00:00::" "unilluminating%3:00:00::" "meaningless%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::meaningfulness-scale) (f::orientation f::neg))
)


;; credibility
(define-type ont::credibility-val
 :parent ont::information-property-val 
 :sem (F::abstr-obj (F::scale ont::credibility-scale))
)

(define-type ont::credible-val
 :parent ont::credibility-val 
 :wordnet-sense-keys ("convincing%3:00:00::" "credulous%3:00:00::" "credible%3:00:00" "believable%3:00:04" )
 :sem (F::abstr-obj (F::scale ont::credibility-scale) (f::orientation f::pos))
)

(define-type ont::not-credible-val
 :parent ont::credibility-val 
 :wordnet-sense-keys ("unconvincing%3:00:00::" "flimsy%3:00:02::" "unbelievable%3:00:04" "incredible%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::credibility-scale) (f::orientation f::neg))
)

;; likely, unlikely, probably
(define-type ont::likelihood-val
 :parent ont::information-property-val
 :comment "likelihood to hold true"
 :sem (F::abstr-obj (F::scale ont::likelihood-scale))
)

(define-type ont::likely-val
 :parent ont::likelihood-val
 :wordnet-sense-keys ("expected%3:00:00::" "plausible%3:00:00::" "prospective%3:00:00::" "likely%3:00:00::" "likely%3:00:04" "probable%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::likely-scale))
 )

(define-type ont::at-risk-val
 :parent ont::likely-val
 )

(define-type ont::not-likely-val
 :parent ont::likelihood-val
 :wordnet-sense-keys ("unexpected%3:00:00::" "implausible%3:00:00::" "unthinkable%3:00:00::" "unlikely%3:00:00::" "unlikely%3:00:04" "improbable%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::not-likely-scale))
)

(define-type ont::predictability-val
 :parent ont::information-property-val 
 :sem (F::abstr-obj (F::scale ont::predictability-scale))
)

(define-type ont::predictable-val
 :parent ont::predictability-val 
 :wordnet-sense-keys ("predictable%3:00:00" "foreseeable%5:00:00:predictable:00" )
 :sem (F::abstr-obj (F::scale ont::predictability-scale) (f::orientation f::pos))
)

(define-type ont::not-predictable-val
 :parent ont::predictability-val 
 :wordnet-sense-keys ("unpredictable%3:00:00" "unforeseeable%5:00:00:unpredictable:00" )
 :sem (F::abstr-obj (F::scale ont::predictability-scale) (f::orientation f::neg))
)

;; understandable
(define-type ont::comprehensibility-val
 :parent ont::information-property-val
 :comment "able to be grasped or understood (different from ont::clarity-val e.g., an explanation might be clear but it still may not be understandable because of external reasons"
 :sem (F::abstr-obj (F::scale ont::comprehensibility-scale))
)

(define-type ont::comprehensible-val
 :parent ont::comprehensibility-val
 :wordnet-sense-keys ("intelligible%3:00:00::" "friendly%3:00:03" "comprehensible%3:00:00")
 :sem (F::abstr-obj (F::scale ont::comprehensibility-scale) (f::orientation f::pos))
)

(define-type ont::not-comprehensible-val
 :parent ont::comprehensibility-val
 :wordnet-sense-keys ("incoherent%3:00:00::" "unfathomable%3:00:00::" "unknowable%3:00:00::" "incomprehensible%3:00:00" "incomprehensible%3:00:04" "unfriendly%3:00:02")
 :sem (F::abstr-obj (F::scale ont::comprehensibility-scale) (f::orientation f::neg))
)

; obvious, obscure apparent
(define-type ont::clarity-val
 :parent ont::information-property-val 
 :comment "clear, obvious vs. unclear, obscure"
)

(define-type ont::clear
 :parent ont::clarity-val 
 :wordnet-sense-keys ("unequivocal%3:00:00::" "univocal%3:00:00::" "unambiguous%3:00:04::" "unambiguous%3:00:00::" "apparent%5:00:00:obvious:00" "obvious%3:00:00" "clear%3:00:00")
 ; Words: (w::clear W::OBVIOUS W::EVIDENT)
 ; Antonym: ONT::unclear (W::UNOBVIOUS)
)

(define-type ont::unclear
 :parent ont::clarity-val 
 :wordnet-sense-keys ("unobvious%3:00:00::" "unclear%3:00:00" "opaque%5:00:00:incomprehensible:00" "ill-defined%3:00:00" "obscure%5:00:00:unclear:00" )
 ; Words: (w::unobvious W::UNCLEAR W::OBSCURE W::OPAQUE)
 ; Antonym: ONT::clear (W::CLEAR w::obvious w::evident)
)

;; consistent in the logical sense
;; consistent, inconsistent
(define-type ont::consistent-val
 :parent ont::information-property-val 
 :arguments ((:REQUIRED ONT::FIGURE ((? lof f::abstr-obj f::situation )))(:REQUIRED ONT::GROUND ((? vl f::abstr-obj f::situation )))) 
 :comment "marked by a (il)logical or (dis)orderly consistent relation of parts"
 :sem (F::abstr-obj (F::scale ont::consistency-scale))
)

(define-type ont::consistent
 :parent ont::consistent-val 
 :wordnet-sense-keys ("consistent%3:00:00" "consistent%3:00:01" )
 :sem (F::abstr-obj (F::scale ont::consistency-scale) (f::orientation f::pos))
)

(define-type ont::inconsistent
 :parent ont::consistent-val 
 :wordnet-sense-keys ("inconsistent%3:00:00" "inconsistent%5:00:00:irreconcilable:00" )
 :sem (F::abstr-obj (F::scale ont::consistency-scale) (f::orientation f::neg))
)

;;; Myrosia 09/23/03 - a very bad name, really. This is for adjectives like "confused", "mixed up", etc - changed LF_Mistaken to LF_Correctness
(define-type ont::correctness-val
 :parent ont::information-property-val 
 :comment "describing the quality of being error-free or error-prone"
 :sem (F::abstr-obj (F::scale ont::correctness-scale))
)

(define-type ont::correct
 :parent ont::correctness-val 
 :wordnet-sense-keys ("accurate%3:00:00::" "right%3:00:04::" "correct%3:00:04::" "proper%3:00:00" "correct%5:00:00:proper:00" "correct%3:00:00" "accurate%5:00:00:correct:00" )
 ; Words: (W::PROPER W::CORRECT W::ACCURATE)
 ; Antonym: ONT::incorrect (W::MISTAKEN W::INACCURATE W::INCORRECT)
 :sem (F::abstr-obj (F::scale ont::correctness-scale) (F::orientation f::pos))
)

(define-type ont::incorrect
 :parent ont::correctness-val 
 :wordnet-sense-keys ("wrong%3:00:04::" "faulty%5:00:00:inaccurate:00" "false%5:00:00:incorrect:00" "incorrect%3:00:00" "inaccurate%3:00:00" )
 ; Words: (W::MISTAKEN W::INACCURATE W::INCORRECT)
 ; Antonym: ONT::correct (W::PROPER W::CORRECT W::ACCURATE)
 :sem (F::abstr-obj (F::scale ont::correctness-scale) (F::orientation f::neg))
)

(define-type ont::validity-val
 :parent ont::information-property-val 
 :sem (F::abstr-obj (F::scale ont::validity-scale))
)

(define-type ont::invalid-val
 :parent ont::validity-val 
 :wordnet-sense-keys ("invalid%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::validity-scale) (f::orientation f::neg))
)

(define-type ont::valid-val
 :parent ont::validity-val 
 :wordnet-sense-keys ("legitimate%5:00:00:valid:00" "valid%3:00:00" "logical%5:00:00:valid:00" )
 :sem (F::abstr-obj (F::scale ont::validity-scale) (f::orientation f::pos))
)

(define-type ont::relevance-val
 :parent ont::information-property-val 
 :sem (F::abstr-obj (F::scale ont::relevance-scale))
)

(define-type ont::relevant
 :parent ont::relevance-val 
 :wordnet-sense-keys ("material%3:00:02::" "pertinent%5:00:00:relevant:00" "applicable%5:00:00:relevant:00" "relevant%3:00:00" )
 ; Words: (W::RELEVANT W::APPLICABLE W::PERTINENT)
 ; Antonym: NIL (W::IRRELEVANT)
 :sem (F::abstr-obj (F::scale ont::relevance-scale) (f::orientation f::pos))
)

(define-type ont::not-relevant-val
 :parent ont::relevance-val 
 :wordnet-sense-keys ("irrelevant%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::relevance-scale) (f::orientation f::neg))
)

;; empirical or theoretical?
(define-type ont::basis-of-evidence-val
 :parent ont::information-property-val
)

(define-type ont::theoretical-val
 :parent ont::basis-of-evidence-val
 :wordnet-sense-keys ("theoretical%3:00:01" )
)

(define-type ont::empirical-val
 :parent ont::basis-of-evidence-val
 :wordnet-sense-keys ("empirical%3:00:00" )
)

(define-type ont::applied-val
 :parent ont::basis-of-evidence-val
 :wordnet-sense-keys ("applied%3:00:00")
)

;; (im)possible
(define-type ont::possibility-val
 :parent ont::information-property-val
 :sem (F::abstr-obj (F::scale ont::possibility-scale))
)

(define-type ont::not-possible-val
 :parent ont::possibility-val 
 :wordnet-sense-keys ("impossible%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::not-possible-scale))
)

(define-type ont::possible
 :parent ont::possibility-val 
 :wordnet-sense-keys ("accomplishable%5:00:00:possible:00" "possible%3:00:00" )
 ; Words: (W::POSSIBLE W::DOABLE)
 ; Antonym: NIL (W::IMPOSSIBLE)
 :sem (F::abstr-obj (F::scale ont::possible-scale))
)

;; truth-value-val
(define-type ont::truth-value-val
 :parent ont::information-property-val 
 :sem (F::abstr-obj (F::scale ont::trueness-scale))
)

(define-type ont::true-val
 :parent ont::truth-value-val
 :wordnet-sense-keys ("true%3:00:00")
 :sem (F::abstr-obj (F::scale ont::true-scale))
)

(define-type ont::false-val
 :parent ont::truth-value-val
 :wordnet-sense-keys ("false%3:00:00")
 :sem (F::abstr-obj (F::scale ont::false-scale))
)

;; verifiability-val
(define-type ont::verifiability-val
 :parent ont::information-property-val 
 :sem (F::abstr-obj (F::scale ont::reproducibility-scale))
)

(define-type ont::verifiable-val
 :parent ont::verifiability-val
 :wordnet-sense-keys ("verifiable%5:00:00:objective:00" "provable%5:00:00:obvious:00" "reproducible%3:00:00")
 :sem (F::abstr-obj (F::scale ont::reproducibility-scale) (f::orientation f::pos))
)

(define-type ont::not-verifiable-val
 :parent ont::verifiability-val
 :wordnet-sense-keys ("unverifiable%5:00:00:subjective:00" "unreproducible%3:00:00")
 :sem (F::abstr-obj (F::scale ont::reproducibility-scale) (f::orientation f::neg))
)

;; PHYSICAL-PROPERTY
(define-type ont::physical-property-val
 :parent ont::property-val 
  :wordnet-sense-keys ("unguiculate%3:00:00::" "unguiculated%3:00:00::" "nonwoody%3:00:00::" "grassy%3:00:00::" "starry%3:00:00::" "woody%3:00:00::" "unseamanlike%3:00:00::" "unstatesmanlike%3:00:00::" "statesmanlike%3:00:00::" "statesmanly%3:00:00::" "priestly%3:00:00::" "priestlike%3:00:00::" "seamanlike%3:00:00::" "seamanly%3:00:00::" "headless%3:00:00::" "seedless%3:00:00::" "unwooded%3:00:00::" "treeless%3:00:00::" "fingerless%3:00:00::" "unbelted%3:00:00::" "beltless%3:00:00::" "wingless%3:00:00::" "jawless%3:00:00::" "sugarless%3:00:00::" "nonsweet%3:00:00::" "backless%3:00:00::" "dry%3:00:02::" "tearless%3:00:00::" "dry-eyed%3:00:00::" "topless%3:00:00::" "limbless%3:00:00::" "pointless%3:00:00::" "unpointed%3:00:00::" "footless%3:00:00::" "juiceless%3:00:00::" "unbodied%3:00:00::" "branchless%3:00:00::" "bloodless%3:00:00::" "curtained%3:00:00::" "stemless%3:00:00::" "faceless%3:00:00::" "smokeless%3:00:00::" "flowerless%3:00:00::" "nonflowering%3:00:00::" "unheaded%3:00:00::" "lidless%3:00:00::" "avascular%3:01:00::" "armless%3:00:00::" "unrifled%3:00:00::" "smoothbore%3:00:00::" "spineless%3:00:00::" "uncrannied%3:00:00::" "landless%3:00:00::" "memberless%3:00:00::" "starchless%3:00:00::" "bottomless%3:00:00::" "untipped%3:00:00::" "unarmored%3:00:02::" "unarmoured%3:00:02::" "neckless%3:00:00::" "aplacental%3:00:00::" "toothless%3:00:00::" "noncolumned%3:00:00::" "uncolumned%3:00:00::" "unfeathered%3:00:00::" "featherless%3:00:00::" "unsaddled%3:00:00::" "leafless%3:00:00::" "unfaceted%3:00:00::" "untimbered%3:00:00::" "unrhymed%3:00:00::" "unrimed%3:00:00::" "rhymeless%3:00:00::" "rimeless%3:00:00::" "unshelled%3:00:00::" "shell-less%3:00:00::" "apetalous%3:00:00::" "petalless%3:00:00::" "breastless%3:00:00::" "noseless%3:00:00::" "fernless%3:00:00::" "eyeless%3:00:00::" "rimless%3:00:00::" "wheelless%3:00:00::" "meatless%3:00:00::" "ribless%3:00:00::" "skinless%3:00:00::" "sleeveless%3:00:00::" "trackless%3:00:00::" "weightless%3:00:00::" "abranchiate%3:00:00::" "abranchial%3:00:00::" "abranchious%3:00:00::" "gill-less%3:00:00::" "bedless%3:00:00::" "lipless%3:00:00::" "unlipped%3:00:00::" "curtainless%3:00:00::" "uncurtained%3:00:00::" "weedless%3:00:00::" "tubeless%3:00:00::" "handless%3:00:00::" "keyless%3:00:00::" "toeless%3:00:00::" "moneyless%3:00:00::" "bellyless%3:00:00::" "flat-bellied%3:00:00::" "awnless%3:00:00::" "lossless%3:00:00::" "toneless%3:00:00::" "grassless%3:00:00::" "soleless%3:00:00::" "hornless%3:00:00::" "tongueless%3:00:00::" "beakless%3:00:00::" "moonless%3:00:00::" "bibless%3:00:00::" "starless%3:00:00::" "earless%3:00:00::" "hipless%3:00:00::" "loamless%3:00:00::" "handleless%3:00:00::" "roofless%3:00:00::" "legless%3:00:00::" "unparented%3:00:00::" "parentless%3:00:00::" "wigless%3:00:00::" "hatless%3:00:00::" "gloveless%3:00:00::" "punctureless%3:00:00::" "nosed%3:00:00::" "armed%3:00:03::" "headed%3:00:01::" "lipped%3:00:00::" "bony%3:00:00::" "boney%3:00:00::" "multilane%3:00:00::" "shelled%3:00:00::" "spaced%3:00:00::" "cross-eyed%3:00:00::" "armored%3:00:02::" "armoured%3:00:02::" "bedded%3:00:00::" "ceilinged%3:00:00::" "belted%3:00:00::" "rimmed%3:00:00::" "timbered%3:00:00::" "skinned%3:00:00::" "peripteral%3:00:00::" "apteral%3:00:00::" "tracked%3:00:00::" "buttoned%3:00:00::" "fastened%3:00:02::" "columned%3:00:00::" "ferned%3:00:00::" "ferny%3:00:01::" "webbed%3:00:00::" "lined%3:00:00::" "ungulate%3:00:00::" "ungulated%3:00:00::" "hoofed%3:00:00::" "hooved%3:00:00::" "seamed%3:00:00::" "ribbed%3:00:00::" "jawed%3:00:00::" "monaural%3:00:00::" "swept%3:00:00::" "binaural%3:00:00::" "biaural%3:00:00::" "unspaced%3:00:00::" "prognathous%3:00:00::" "prognathic%3:00:00::" "hypognathous%3:00:00::" "germy%3:00:00::" "bound%3:00:02::" "unbound%3:00:02::" "bodied%3:00:00::" "ventral%3:00:00::" "awned%3:00:00::" "awny%3:00:00::" "tongued%3:00:00::" "breasted%3:00:00::" "dolichocephalic%3:00:00::" "dolichocranial%3:00:00::" "dolichocranic%3:00:00::" "bellied%3:00:00::" "fingered%3:00:00::" "juicy%3:00:00::" "bibbed%3:00:00::" "rifled%3:00:00::" "single-barreled%3:00:00::" "single-barrelled%3:00:00::" "floored%3:00:00::" "toned%3:00:00::" "balconied%3:00:00::" "stemmed%3:00:00::" "single-breasted%3:00:00::" "crannied%3:00:00::" "onymous%3:00:00::" "hairy%3:00:00::" "haired%3:00:00::" "hirsute%3:00:00::" "eyed%3:00:00::" "faced%3:00:00::" "winged%3:00:00::" "backed%3:00:00::" "tipped%3:00:00::" "horned%3:00:00::" "necked%3:00:00::" "limbed%3:00:00::" "bottomed%3:00:00::" "footed%3:00:00::" "feathered%3:00:00::" "seedy%3:00:00::" "petalous%3:00:00::" "petaled%3:00:00::" "petalled%3:00:00::" "eared%3:00:00::" "armed%3:00:02::" "hairless%3:00:00::" "legged%3:00:00::" "beaked%3:00:00::" "wooded%3:00:00::" "leafy%3:00:00::" "toothed%3:00:00::" "motorized%3:00:00::" "motorised%3:00:00::" "motored%3:00:00::" "splayfooted%3:00:00::" "splayfoot%3:00:00::" "light-footed%3:00:00::" "tubed%3:00:00::" "corbelled%3:44:00::" "headed%3:00:02::" "parented%3:00:00::" "ridged%3:44:00::" "carinate%3:44:00::" "carinated%3:44:00::" "keeled%3:44:00::" "saddled%3:00:00::" "soled%3:00:00::" "handled%3:00:00::" "appendaged%3:00:00::" "walleyed%3:00:00::" "faceted%3:00:00::" "pigeon-toed%3:00:00::" "quadrupedal%3:00:00::" "quadruped%3:00:00::" "four-footed%3:00:00::" "hipped%3:00:00::" "unmotorized%3:00:00::" "unmotorised%3:00:00::" "motorless%3:00:00::" "lidded%3:00:00::" "double-barreled%3:00:00::" "double-barrelled%3:00:00::" "bipedal%3:00:00::" "biped%3:00:00::" "two-footed%3:00:00::" "wheeled%3:00:00::" "gabled%3:00:00::" "unwebbed%3:00:00::" "roofed%3:00:00::" "carpeted%3:00:00::" "short-spurred%3:00:00::" "long-spurred%3:00:00::" "weedy%3:00:00::" "single-lane%3:00:00::" "porous%3:00:00::" "poriferous%3:00:00::" "toed%3:00:00::" "right-handed%3:00:00::" "left-handed%3:00:00::" "handed%3:00:00::" "heavy-footed%3:00:00::" "invertebrate%3:00:00::" "spineless%3:00:01::" "resinlike%3:01:00::" "chartaceous%3:01:00::" "papery%3:01:00::" "paperlike%3:01:00::" "baccate%3:01:00::" "berrylike%3:01:00::" "shelflike%3:01:00::" "daisylike%3:01:00::" "indexless%3:01:00::" "fungoid%3:01:00::" "funguslike%3:01:00::" "collarless%3:01:00::" "unalike%3:00:00::" "dissimilar%3:00:04::" "brimless%3:01:00::" "sessile%3:00:00::" "stalkless%3:00:00::" "stingless%3:01:00::" "yeasty%3:01:00::" "yeastlike%3:01:00::" "rayless%3:01:00::" "acaulescent%3:00:00::" "stemless%3:00:04::" "gutless%3:00:00::" "burrlike%3:01:00::" "arachnoid%3:01:00::" "arachnidian%3:01:00::" "spidery%3:01:00::" "spiderlike%3:01:00::" "spiderly%3:01:00::" "astomatous%3:00:00::" "mouthless%3:00:00::" "brassy%3:01:00::" "brasslike%3:01:00::" "custard-like%3:01:00::" "hooflike%3:01:00::" "ductless%3:01:00::" "frictionless%3:01:00::" "masted%3:01:00::" "sharp-pointed%3:01:00::" "recoilless%3:01:00::" "flagellate%3:01:00::" "flagellated%3:01:00::" "whiplike%3:01:00::" "lash-like%3:01:00::" "grapelike%3:01:00::" "bracteate%3:01:00::" "bracted%3:01:00::" "wolflike%3:01:00::" "wolfish%3:01:00::" "chaffy%3:01:00::" "chafflike%3:01:00::" "heathlike%3:01:00::" "lung-like%3:01:00::" "clinker-built%3:00:00::" "clincher-built%3:00:00::" "lap-strake%3:00:00::" "lap-straked%3:00:00::" "lap-streak%3:00:00::" "lap-streaked%3:00:00::" "ocellated%3:01:00::" "ratlike%3:01:00::" "ctenoid%3:01:00::" "comb-like%3:01:00::" "bladdery%3:01:00::" "bladderlike%3:01:00::" "conic%3:01:00::" "conical%3:01:00::" "conelike%3:01:00::" "cone-shaped%3:01:00::" "liquid%3:00:00::" "hollow%3:00:00::" "uncharged%3:00:00::" "miscible%3:00:00::" "mixable%3:00:00::" "immiscible%3:00:00::" "non-miscible%3:00:00::" "unmixable%3:00:00::" "solid%3:00:02::")
 :comment "properties pertaining to the attributes of physical entities or substances. note many physical adjectives can be used on non-physical objects"
; many physical adjectives can be used on non-physical objects
; :arguments ((:REQUIRED ONT::OF (F::phys-obj))
;             )
 :sem (F::abstr-obj (F::scale ont::physical-property-scale))
)

(define-type ont::configuration-property-val
 :parent ont::physical-property-val 
 :comment "properties regarding the configuration, arrangement, or layout of elements"
 :sem (F::abstr-obj (F::scale ont::configuration-property-scale))
)

(define-type ont::equipped-val
 :wordnet-sense-keys ("equipped%3:00:00" "equipped%3:00:02" "equipped%5:00:00:prepared:00" "armed%3:00:01")
 :parent ont::configuration-property-val 
)

;; flexible vs. rigid
(define-type ont::flexibility-val
 :parent ont::configuration-property-val 
)

(define-type ont::not-flexible-val
 :parent ont::flexibility-val
 :wordnet-sense-keys ("rigid%5:00:00:inflexible:01" "inflexible%3:00:01")
)

(define-type ont::flexible-val
 :parent ont::flexibility-val
 :wordnet-sense-keys ("elastic%3:00:00::" "flexible%3:00:01" "pliant%5:00:00:flexible:01")
)

;; loose vs. tight
(define-type ont::constricting-val
 :parent ont::configuration-property-val 
 :comment "describes how constricting something is with regards to another item (close-fitting vs. loose-fitting)"
 :sem (F::abstr-obj (F::scale ont::constriction-scale))
)

(define-type ont::loose-val
 :parent ont::constricting-val 
 :wordnet-sense-keys ("loose%3:00:02::" "unconstricted%3:00:00::" "loose%3:00:01" "loose%5:00:00:coarse:00")
 :sem (F::abstr-obj (F::scale ont::looseness-scale))
)

(define-type ont::not-loose-val
 :parent ont::constricting-val 
 :wordnet-sense-keys ("compact%3:00:00::" "constricted%3:00:00::" "tense%3:00:01::" "snug%5:00:00:tight:01" "tight%3:00:01" "choky%5:00:00:tight:01"  "isotonic%5:00:00:tense:03")
 :sem (F::abstr-obj (F::scale ont::tightness-scale))
)

;; loose vs. affixed
(define-type ont::binding-val
 :parent ont::configuration-property-val 
 :comment "describes how firmly something is fixed in place"
)

(define-type ont::affixed-val
 :parent ont::binding-val 
 :wordnet-sense-keys ("fixed%3:00:00" "affixed%3:00:00" )
)

(define-type ont::not-affixed-val
 :parent ont::binding-val 
 :wordnet-sense-keys ("insecure%3:00:03::" "loose%3:00:04" )
)

;; intersecting
(define-type ont::intersecting-val
 :parent ont::configuration-property-val 
 :wordnet-sense-keys ("intersecting%5:00:00:crossed:01" "crossed%3:00:01" )
)

;; blocked, congested, impassable -- for routes
(define-type ont::flow-val
 :parent ont::configuration-property-val 
 :arguments ((:REQUIRED ONT::FIGURE (F::phys-obj (F::spatial-abstraction (? sab F::line F::strip ))))) 
 :sem (F::abstr-obj (:required )(:default (F::gradability - )))
 :comment "presence or absense of obstruction, e.g., for routes"
)

(define-type ont::obstructed
 :parent ont::flow-val 
 :wordnet-sense-keys ("uncleared%3:00:00::" "obstructed%3:00:00" )
)

(define-type ont::unobstructed
 :parent ont::flow-val 
  :wordnet-sense-keys ("cleared%3:00:00::")
 :wordnet-sense-keys("unobstructed%3:00:00" "clear%5:00:00:unobstructed:00")
)

;; full vs. empty
(define-type ont::filled-val
 :parent ont::configuration-property-val 
)

(define-type ont::full
 :parent ont::filled-val 
 :wordnet-sense-keys ("filled%5:00:01:full:00" "full%3:00:00" )
 ; Words: (W::FULL W::FILLED)
 ; Antonym: ONT::UNFILLED (W::EMPTY)
)

(define-type ont::unfilled
 :parent ont::filled-val 
 :wordnet-sense-keys ("empty%3:00:00" )
 ; Words: (W::EMPTY)
 ; Antonym: ONT::FULL (W::FULL W::FILLED)
)

;; homogeneous vs. heterogeneous
(define-type ont::homogeneity-val
 :parent ont::configuration-property-val 
)

;; homogeneous, uniform
(define-type ont::homogeneous-val
 :parent ont::homogeneity-val 
 :wordnet-sense-keys ("uniform%3:00:00::" "unvarying%3:00:04::" "homogeneous%3:00:00" )
)

(define-type ont::not-homogeneous-val
 :parent ont::homogeneity-val 
 :wordnet-sense-keys ("heterogeneous%3:00:00" )
)

;; clothedness and adornment
(define-type ont::clothedness-adornment-val
 :parent ont::configuration-property-val 
)

(define-type ont::adorned-val
 :parent ont::clothedness-adornment-val 
 :wordnet-sense-keys ("bordered%3:00:00::" "unglazed%3:00:00::" "painted%3:00:01::" "adorned%3:00:00" )
)

(define-type ont::clothed-val
 :parent ont::clothedness-adornment-val 
 :wordnet-sense-keys ("crowned%3:00:00::" "shod%3:00:00::" "shodden%3:00:00::" "shoed%3:00:00::" "gloved%3:00:00::" "wigged%3:00:00::" "clothed%3:00:00" )
)

(define-type ont::bare-val
 :parent ont::clothedness-adornment-val 
 :wordnet-sense-keys ("unpainted%3:00:01::" "unpainted%3:00:00::" "unbordered%3:00:00::" "bare%5:00:00:unclothed:00" "unclothed%3:00:00" "naked%5:00:00:bare:00" 
                      "bare%5:00:00:unadorned:00" "unadorned%3:00:00" "bare%3:00:00" )
)

;; substantial properties
(define-type ont::substantial-property-val
 :parent ont::physical-property-val 
  :wordnet-sense-keys ("colloidal%3:01:00::" "subatomic%3:01:00::" "achromatinic%3:01:00::" "hyaloplasmic%3:01:00::" "aplitic%3:01:00::" "monatomic%3:01:00::" "monoatomic%3:01:00::" "micaceous%3:01:00::" "aphanitic%3:01:00::" "centrosomic%3:01:00::" "cytotoxic%3:01:00::" "biocatalytic%3:01:00::" "rupestral%3:01:00::" "rupicolous%3:01:00::" "single-stranded%3:01:00::" "allergenic%3:01:00::" "biotitic%3:01:00::" "porphyritic%3:01:00::" "amethystine%3:01:00::" "chlorophyllose%3:01:00::" "chlorophyllous%3:01:00::" "fungicidal%3:01:00::" "antifungal%3:01:00::" "mineral%3:01:01::" "pyrectic%3:01:00::" "earthen%3:01:00::" "cytoplasmic%3:01:00::" "cytoplasmatic%3:01:00::" "miotic%3:01:00::" "myotic%3:01:00::" "arenicolous%3:01:00::" "marmorean%3:01:00::" "marmoreal%3:01:00::" "antigenic%3:01:00::" "granuliferous%3:01:00::" "viricidal%3:01:00::" "virucidal%3:01:00::" "peaty%3:01:00::" "cryogenic%3:01:00::" "cytoplastic%3:01:00::" "lactogenic%3:01:00::" "alabaster%3:01:00::" "alabastrine%3:01:00::" "humified%3:01:00::" "lithic%3:01:00::" "lactic%3:01:00::" "chromatinic%3:01:00::" "epilithic%3:01:00::" "polyatomic%3:01:00::" "filar%3:01:00::" "nodular%3:01:00::" "fibrillose%3:01:00::" "prandial%3:01:00::" "earthy%3:01:00::" "isotopic%3:01:00::" "alkahestic%3:01:00::" "auxinic%3:01:00::" "calcitic%3:01:00::" "sapphirine%3:01:00::" "bauxitic%3:01:00::" "essential%3:01:00::" "dolomitic%3:01:00::" "quartzose%3:01:00::" "humic%3:01:00::" "narcotic%3:01:00::" "atomic%3:01:00::" "neurotoxic%3:01:00::" "mealy%3:01:00::" "augitic%3:01:00::" "marly%3:01:00::" "diatomic%3:01:00::" "pyrogenic%3:01:01::" "pyrogenous%3:01:01::" "pyrogenetic%3:01:01::" "borated%3:01:00::")
 :comment "properties having to do with characteristic substance of the described object"
)

;; broadband
;(define-type ont::bandwidth-val
; :parent ont::substantial-property-val 
;)


(define-type ont::cordless-val
 :parent ont::substantial-property-val 
 :wordnet-sense-keys ("cordless%3:01:00" "wireless%3:00:00")
)

(define-type ont::organic-val
 :parent ont::substantial-property-val 
 :wordnet-sense-keys ("organic%3:00:01" "organic%3:00:02")
)

(define-type ont::nuclear-val
 :parent ont::substantial-property-val 
 :wordnet-sense-keys ("nuclear%3:00:00")
)

;; 20111017 added for obtw demo (word for type)
(define-type ont::electrical
 :parent ont::substantial-property-val 
)

;; boneless, skinless
(define-type ont::food-preparation
 :parent ont::substantial-property-val 
  :wordnet-sense-keys ("boneless%3:00:00::")
)

;;; noisy (data, signal)
(define-type ont::interference-val
 :parent ont::substantial-property-val 
)

;; optical, magnetic, holographic
(define-type ont::medium
 :parent ont::substantial-property-val 
  :wordnet-sense-keys ("magnetic%3:00:00::" "magnetized%3:00:00::" "magnetised%3:00:00::")
 :arguments ((:OPTIONAL ONT::FIGURE )) 
 :sem (F::Abstr-obj (F::gradability - ))
 :comment "means of production and dissemination (c.f. ont::mode)"
)

(define-type ont::multimedia-val
 :parent ont::medium
)

;; e.g., graphical, tactile, vocal
(define-type ont::mode
 :parent ont::substantial-property-val 
 :arguments ((:OPTIONAL ONT::FIGURE )) 
 :sem (F::Abstr-obj (F::gradability - ))
 :comment "means of representation (c.f. ont::medium)"
)

(define-type ont::mode-of-control-val
 :parent ont::mode
)

(define-type ont::manual-val
 :parent ont::mode-of-control-val 
  :wordnet-sense-keys ("manual%3:00:00::")
)

(define-type ont::automatic
 :parent ont::mode-of-control-val 
  :wordnet-sense-keys ("automatic%3:00:00::" "unmanned%3:00:00::" "remote-controlled%3:00:00::")
 :arguments ((:required ont::FIGURE ((? lof f::abstr-obj f::phys-obj f::situation))
             ))
)

(define-type ont::signal-representation-val
 :parent ont::mode
)

(define-type ont::digital-val
 :parent ont::signal-representation-val 
)

(define-type ont::analog
 :parent ont::signal-representation-val 
 :wordnet-sense-keys ("analogue%3:00:00" )
 ; Words: (W::LINEAR W::ANALOG)
 ; Antonym: NIL (W::DIGITAL)
)

(define-type ont::sensory-mode-val
 :parent ont::mode
 ; auditory, visual, tactile communication/information
)

;; phospho, phospho-, p-
(define-type ont::phosphorilated
 :parent ont::physical-property-val 
 :arguments (; (:ESSENTIAL ONT::of (F::phys-obj (f::type (? t ont::molecular-part ont::chemical ))))
               (:ESSENTIAL ONT::figure (F::phys-obj (f::type (? t2 ont::molecular-part ont::chemical ))))) 
)

;; sunny, windy, cloudy, breezy
(define-type ont::atmospheric-val
 :parent ont::physical-property-val 
 :wordnet-sense-keys ("atmospheric%3:01:00" )
 :sem (F::abstr-obj )
 :comment "having to do with weather"
 :sem (F::abstr-obj (F::scale ont::atmospheric-scale))
)

(define-type ont::sunny-val
 :parent ont::atmospheric-val 
)

(define-type ont::mild-and-pleasant-val
 :parent ont::atmospheric-val 
 :wordnet-sense-keys ("temperate%3:00:01::" "favorable%3:00:01::" "favourable%3:00:01::" "clement%3:00:02")
)

(define-type ont::inclement-weather-val
 :parent ont::atmospheric-val
 :wordnet-sense-keys ("unfavorable%3:00:01::" "unfavourable%3:00:01::" "inclement%3:00:02::" "intemperate%3:00:01::" "stormy%3:00:00")
)

(define-type ont::windy-val
 :parent ont::atmospheric-val 
 :wordnet-sense-keys ("windy%5:00:00:stormy:00" "breezy%5:00:00:stormy:00" "gusty%5:00:00:stormy:00")
)

(define-type ont::humid-val
 :parent ont::atmospheric-val 
 :wordnet-sense-keys ("humid%5:00:00:wet:01" )
 :sem (F::abstr-obj (F::scale ont::humidity-scale))
)

(define-type ont::humid-warm-val
 :parent ont::humid-val 
 :wordnet-sense-keys ("muggy%5:00:00:wet:01" )
)

(define-type ont::precipitating-val
 :parent ont::atmospheric-val 
 :wordnet-sense-keys ("rainy%5:00:00:wet:01" )
)

(define-type ont::clear-weather
 :parent ont::atmospheric-val 
 :wordnet-sense-keys ("calm%3:00:00::" "clear%3:00:03" "fair%5:00:00:clear:03" "liquid%5:00:00:clear:02")
 ; Words: (W::CLEAR W::FAIR)
 ; Antonym: ONT::CLOUDY (W::SMOGGY W::OVERCAST W::FOGGY W::HAZY W::CLOUDY)
)

(define-type ont::cloudy
 :parent ont::atmospheric-val 
 :wordnet-sense-keys ("smoky%3:00:00::" "cloud-covered%5:00:00:cloudy:00" "cloudy%3:00:00" "brumous%5:00:00:cloudy:00" "smoggy%5:00:00:cloudy:00" )
 ; Words: (W::SMOGGY W::OVERCAST W::FOGGY W::HAZY W::CLOUDY)
 ; Antonym: ONT::CLEAR-WEATHER (W::CLEAR W::FAIR)
)

;; hot, cold
(define-type ont::temperature-val
 :parent ont::property-val 
 :sem (F::abstr-obj (F::scale ont::temperature-scale ))
 :comment "having to do with temperature"
)

(define-type ont::cold
 :parent ont::temperature-val 
 :wordnet-sense-keys ("cold%3:00:01" "cold%5:00:00:cool:03" "cool%3:00:01" "cool%3:00:03" "freeze%2:43:10")
 ; Words: (W::COLD W::COOL)
 ; Antonym: ONT::WARM (W::HOT W::WARM)
 :sem (F::abstr-obj (F::scale ont::cold-scale ))
)

(define-type ont::warm
 :parent ont::temperature-val 
 :wordnet-sense-keys ("hot%3:00:01::" "warm%3:00:01" "hot%5:00:00:warm:03" "warm%3:00:03" )
 ; Words: (W::HOT W::WARM)
 ; Antonym: ONT::COLD (W::COLD W::COOL)
 :sem (F::abstr-obj (F::scale ont::heat-scale ))
)

;; wet, dry
(define-type ont::moisture-content-val
 :parent ont::physical-property-val 
 :arguments ((:REQUIRED ONT::FIGURE ((? lof f::phys-obj f::situation )))) ;; dry towel, a dry cough
 :sem (F::abstr-obj (F::scale ont::moisture-content-scale ))
)

(define-type ont::dry-val
 :parent ont::moisture-content-val 
 :wordnet-sense-keys ("xeric%3:00:00::" "dry%3:00:01" )
 :sem (F::abstr-obj (F::scale ont::dry-scale ))
)

(define-type ont::dehydrated-val
 :parent ont::dry-val
 :wordnet-sense-keys("withered%5:00:00:dry:01" "dehydrated%5:00:00:preserved:02")
 :sem (F::abstr-obj (F::scale ont::dehydrated-scale ))
)

(define-type ont::wet-val
 :parent ont::moisture-content-val 
 :wordnet-sense-keys ("hydric%3:00:00::" "wet%3:00:01" )
 :sem (F::abstr-obj (F::scale ont::wet-scale ))
)

;; sensory properties: CHARACTERISTICS of perceived items THROUGH sensory input (e.g., red, white, fuzzy, soft, dark, light)
(define-type ont::appearance-property-val
 :parent ont::physical-property-val
 :comment "subjective chracteristics on HOW something is perceived through sensory input (compare to ont::sensory-property-val)"
 :sem (F::abstr-obj (F::scale ont::appearance-scale ))
)

;; sound properties
(define-type ont::sound-property-val
 :parent ont::appearance-property-val
  :wordnet-sense-keys ("reverberant%3:00:00::" "cacophonous%3:00:00::" "cacophonic%3:00:00::")
 :comment "subjective characteristics of an item perceived through auditory input"
 :sem (F::abstr-obj (F::scale ont::sound-scale ))
)

;; loud, soft, quiet
(define-type ont::loudness-val
 :parent ont::sound-property-val 
 :arguments ((:required ont::FIGURE ((? lof f::phys-obj f::abstr-obj f::situation ) (f::type (? !t ont::body-part ont::material))))) ;; an event can be loud, e.g. barking; abstr-obj: music; phys-obj: room
 :sem (F::Abstr-obj (F::MEasure-function F::VALUE ))
 :sem (F::abstr-obj (F::scale ont::sound-volume-scale ))
)

; loud concert/music/voice/room/floorboard/chair
(define-type ont::noisy
 :parent ont::loudness-val 
 :wordnet-sense-keys ("deafening%5:00:00:loud:00" "loud%3:00:00" "noisy%3:00:00")
 ; Words: (W::LOUD W::NOISY)
 ; Antonym: ONT::QUIET (W::SOFT W::QUIET W::SILENT W::STILL)
 :sem (F::abstr-obj (F::scale ont::loudness-scale ))
)

(define-type ont::quiet
 :parent ont::loudness-val 
 :wordnet-sense-keys ("hushed%5:00:00:soft:04" "quiet%3:00:02" "quiet%5:00:00:soft:04" "soft%3:00:04" )
 ; Words: (W::SOFT W::QUIET W::SILENT W::STILL)
 ; Antonym: ONT::NOISY (W::LOUD W::NOISY)
 :sem (F::abstr-obj (F::scale ont::quietness-scale ))
)

(define-type ont::silent-val
 :parent ont::sound-property-val 
 :wordnet-sense-keys ("silent%5:00:00:quiet:01" "still%5:00:00:quiet:01" "quiet%3:00:01" )
)


;; smell properties
(define-type ont::smell-property-val
 :parent ont::appearance-property-val
  :wordnet-sense-keys ("odorous%3:00:00::")
 :comment "subjective characteristics of an item perceived through olfactory input"
 :sem (F::abstr-obj (F::scale ont::smell-scale ))
)

(define-type ont::pleasant-smelling-val
 :parent ont::smell-property-val 
 :wordnet-sense-keys ("aromatic%5:00:00:fragrant:00" "perfumed%5:00:02:fragrant:00" "musky%5:00:00:fragrant:00" "fragrant%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::positive-smell-scale))
)

(define-type ont::unpleasant-smelling-val
 :parent ont::smell-property-val 
 :wordnet-sense-keys ("smelly%5:00:00:malodorous:00" "stinky%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::negative-smell-scale))
)


;; touch properties
(define-type ont::touch-property-val
 :parent ont::appearance-property-val
 :comment "subjective characteristics of an item perceived through tactile input"
 :sem (F::abstr-obj (F::scale ont::touch-scale ))
)

;; smooth, rough, soft, hard
(define-type ont::texture-val
 :parent ont::touch-property-val 
  :wordnet-sense-keys ("fine%3:00:00::")
 :sem (F::Abstr-obj (F::MEasure-function F::VALUE ) (F::scale ont::texture-scale))
 )

(define-type ont::thickeness-in-texture-val
 :parent ont::texture-val
)

(define-type ont::thick-texture-val
 :parent ont::thickeness-in-texture-val
 :wordnet-sense-keys ("thick%3:00:02")
 :sem (F::abstr-obj (F::scale ont::texture-thickness-scale))
)

(define-type ont::thin-texture-val
 :parent ont::thickeness-in-texture-val
 :wordnet-sense-keys ("thin%3:00:02")
 :sem (F::abstr-obj (F::scale ont::texture-thinness-scale))
)

(define-type ont::hardness-val
    :parent ont::texture-val
    )

(define-type ont::hard-val
 :parent ont::hardness-val 
 :wordnet-sense-keys ("leathery%5:00:00:tough:01" "hard%3:00:01" "solid%5:00:00:hard:01" "tough%3:00:01" )
; :sem (F::Abstr-obj (F::scale ONT::hardness*1--07--00 ))
 :sem (F::abstr-obj (F::scale ont::tactile-hardness-scale))
)

; soft skin/cloth/body/tissue/bed/seat/chair
(define-type ont::soft-val
 :parent ont::hardness-val 
 :wordnet-sense-keys ("tender%3:00:01::" "soft%3:00:01" "fluffy%5:00:00:soft:01" "plushy%5:00:00:coarse:00")
; :sem (F::Abstr-obj (F::scale ONT::softness*1--07--00 ))
 :sem (F::abstr-obj (F::scale ont::tactile-softness-scale))
)

(define-type ont::smoothness-val
 :parent ont::texture-val 
)

(define-type ont::not-smooth-val
 :parent ont::smoothness-val 
 :wordnet-sense-keys ("unironed%3:00:00::" "wrinkled%3:00:02::" "wrinkled%3:00:00::" "wrinkly%3:00:00::" "uneven%3:00:00::" "rough%3:00:00" "granular%5:00:00:coarse:00" "coarse%3:00:00")
 :sem (F::abstr-obj (F::scale ont::roughness-scale))
)

(define-type ont::smooth-val
 :parent ont::smoothness-val 
 :wordnet-sense-keys ("seamless%3:00:00::" "ironed%3:00:00::" "unwrinkled%3:00:00::" "wrinkleless%3:00:00::" "seamless%5:00:00:smooth:00" "smooth%3:00:00" )
; :sem (F::Abstr-obj (F::scale ONT::smoothness*1--07--00 ))
 :sem (F::abstr-obj (F::scale ont::smoothness-scale))
)

(define-type ont::sticky-val
 :parent ont::texture-val 
 :wordnet-sense-keys ("nonslippery%3:00:00::" "glutinous%5:00:00:adhesive:00" "gooey%5:00:00:adhesive:00" "gum-like%5:00:00:adhesive:00" "gummed%5:00:00:adhesive:00" "tarry%5:00:00:adhesive:00" "sticky%5:00:01:adhesive:00")
)

;; taste properties
(define-type ont::taste-property-val
 :parent ont::appearance-property-val 
  :wordnet-sense-keys ("dry%3:00:03::")
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Phys-obj (F::OBJECT-FUNCTION F::COMESTIBLE))))
 :sem (F::abstr-obj (F::scale ont::taste-scale))
 :comment "subjective characteristics of an item perceived through gustatory input"
)

(define-type ont::spicy-val
 :parent ont::taste-property-val 
 :wordnet-sense-keys ("spicy%5:00:01:tasty:00" "spicy%5:00:00:tasty:00" )
 :sem (F::abstr-obj (F::scale ont::spiciness-scale))
)

(define-type ont::salty-val
 :parent ont::taste-property-val 
 :wordnet-sense-keys ("salty%3:00:00::" "salty%5:00:00:tasty:00" )
)

(define-type ont::sour-val
 :parent ont::taste-property-val 
 :wordnet-sense-keys ("sour%3:00:00::" "sour%5:00:00:tasty:00" )
; :sem (F::Abstr-obj (F::scale ONT::sourness*1--07--00 ))
 :sem (F::abstr-obj (F::scale ont::sourness-scale))
)

(define-type ont::sweet-val
 :parent ont::taste-property-val 
 :wordnet-sense-keys ("sugary%3:00:00::" "sweet%3:00:02" )
; :sem (F::Abstr-obj (F::scale ONT::sweetness*1--07--00 ))
 :sem (F::abstr-obj (F::scale ont::sweetness-scale))
)

(define-type ont::tasty-val
 :parent ont::taste-property-val 
 :wordnet-sense-keys ("tasty%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::taste-scale) (f::orientation f::pos))
)

(define-type ont::not-tasty-val
 :parent ont::taste-property-val 
 :wordnet-sense-keys ("bland%5:00:00:tasteless:01" "tasteless%3:00:01" )
 :sem (F::abstr-obj (F::scale ont::taste-scale) (f::orientation f::neg))
; scale not there but would be under sensory-scale
)

;; sight properties
(define-type ont::visual-property-val
 :parent ont::appearance-property-val
  :wordnet-sense-keys ("tangled%3:00:00::" "blemished%3:00:00::" "glazed%3:00:00::" "shiny%3:00:04::" "unfocused%3:00:00::" "unfocussed%3:00:04::")
 :comment "subjective characteristics of an item perceived through visual input"
 :sem (F::abstr-obj (F::scale ont::visual-scale))
)

;; color saturation: dark blue, light green
(define-type ont::color-saturation-val
 :parent ont::visual-property-val 
)

(define-type ont::light-in-color-val
 :parent ont::color-saturation-val 
 :wordnet-sense-keys ("unsaturated%3:00:03::" "saturated%3:00:03::" "pure%3:00:04::" "light%3:00:05" )
)

(define-type ont::dark-in-color-val
 :parent ont::color-saturation-val 
 :wordnet-sense-keys ("dark%3:00:02" )
)

;; light & dark: light room, dark forest
(define-type ont::presense-of-light-val
 :parent ont::visual-property-val 
 :sem (F::abstr-obj (F::scale ont::presence-of-light-scale))
)

(define-type ont::light-val
 :parent ont::presense-of-light-val 
 :wordnet-sense-keys ("unshaded%3:00:01::" "light%3:00:06" )
; :sem (F::abstr-obj (F::scale ont::luminosity-scale ))
 :sem (F::abstr-obj (F::scale ont::lightness-scale ))
)

(define-type ont::dark-val
 :parent ont::presense-of-light-val 
 :wordnet-sense-keys ("shaded%3:00:01::" "dark%3:00:01")
 :sem (F::abstr-obj (F::scale ont::darkness-scale ))
)

;; bright & dim
(define-type ont::luminosity-val
 :parent ont::visual-property-val 
 :sem (F::abstr-obj (F::scale ont::luminosity-scale ))
)

(define-type ont::bright-val
 :parent ont::luminosity-val
 :wordnet-sense-keys ("bright%3:00:00")
 :sem (F::abstr-obj (F::scale ont::luminosity-scale ) (f::orientation f::pos))
)

(define-type ont::dim-val
 :parent ont::luminosity-val
 :wordnet-sense-keys ("dull%3:00:02::" "dim%5:00:00:dark:01" "dim%3:00:02")
 :sem (F::abstr-obj (F::scale ont::luminosity-scale ) (f::orientation f::neg))
)

;; opaque & transparent
(define-type ont::opacity-val
 :parent ont::visual-property-val 
 :sem (F::abstr-obj (F::scale ont::light-passage-scale))
)

(define-type ont::opaque-val
 :parent ont::opacity-val 
 :wordnet-sense-keys ("opaque%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::opacity-scale ))
)

(define-type ont::transparent-val
 :parent ont::opacity-val 
 :wordnet-sense-keys ("clear%3:00:02")
 :sem (F::abstr-obj (F::scale ont::visual-clarity-scale ))
)



;; ont::color-val

(define-type ONT::COLOR-VAL
 :parent ONT::visual-PROPERTY-VAL
 :sem (F::Abstr-obj (F::MEasure-function F::VALUE) (f::scale ont::color-scale))
 :arguments ((:OPTIONAL ONT::FIGURE (F::phys-obj))  ; to distinguish between steps as steps in a plan and steps in a staircase
             )
 :sem (F::abstr-obj (F::scale ont::color-quality-scale ))
 :wordnet-sense-keys ("colored%3:00:00::" "coloured%3:00:00::" "colorful%3:00:02::" "colorful%3:00:00::" "colourful%3:00:00::" "brunet%3:00:00::" "brunette%3:00:00::" "blond%3:00:00::" "blonde%3:00:00::" "light-haired%3:00:00::" "bistered%3:01:00::" "bistred%3:01:00::" "chromatic%3:00:00" "achromatic%3:00:00")
)

(define-type ont::bronze
 :parent ont::color-val
 :wordnet-sense-keys ("bronze%5:00:00:chromatic:00")
)

(define-type ont::copper
 :parent ont::color-val
 :sem (F::Abstr-obj (F::scale ONT::copper-scale))
 :wordnet-sense-keys ("coppery%5:00:00:chromatic:00")
)

(define-type ONT::red
 :parent ONT::color-VAL
 :wordnet-sense-keys ("red%5:00:01:chromatic:00")
 :sem (F::abstr-obj (F::scale ont::red-scale ))
 )

(define-type ONT::blue
 :parent ONT::color-VAL
 :wordnet-sense-keys ("blue%5:00:00:chromatic:00")
 :sem (F::Abstr-obj (F::scale ONT::blue-scale))
 )

(define-type ONT::green
 :parent ONT::color-VAL
 :sem (F::Abstr-obj (F::scale ONT::green-scale))
; :sem (F::Abstr-obj (F::scale ONT::GREEN*1--07--00))
 :wordnet-sense-keys ("green%5:00:00:chromatic:00")
 )

(define-type ONT::yellow
 :parent ONT::color-VAL
 :sem (F::Abstr-obj (F::scale ONT::yellow-scale))
; :sem (F::Abstr-obj (F::scale ONT::YELLOW*1--07--00))
 :wordnet-sense-keys ("yellow%5:00:00:chromatic:00")
 )

(define-type ONT::orange
 :parent ONT::color-VAL
 :sem (F::Abstr-obj (F::scale ONT::orange-scale))
;  :sem (F::Abstr-obj (F::scale ONT::ORANGE*1--07--00))
 :wordnet-sense-keys ("orange%5:00:00:chromatic:00")
  )

(define-type ONT::purple
 :parent ONT::color-VAL
 :sem (F::Abstr-obj (F::scale ONT::purple-scale))
;  :sem (F::Abstr-obj (F::scale ONT::PURPLE*1--07--00))
 :wordnet-sense-keys ("purple%5:00:00:chromatic:00")
 )

(define-type ONT::brown
 :parent ONT::color-VAL
 :sem (F::Abstr-obj (F::scale ONT::brown-scale))
;  :sem (F::Abstr-obj (F::scale ONT::BROWNNESS*1--07--00))
 :wordnet-sense-keys ("brown%5:00:00:chromatic:00")
 )

(define-type ONT::gold
 :parent ONT::color-VAL
 :sem (F::Abstr-obj (F::scale ONT::gold-scale))
;  :sem (F::Abstr-obj (F::scale ONT::gold*1--07--00))
 :wordnet-sense-keys ("gold%5:00:00:chromatic:00")
)

(define-type ONT::silver
 :parent ONT::color-VAL
 :sem (F::Abstr-obj (F::scale ONT::silver-scale))
;  :sem (F::Abstr-obj (F::scale ONT::silver*1--07--00))
 :wordnet-sense-keys ("silver%5:00:00:achromatic:00")
 )

(define-type ONT::magenta
 :parent ONT::color-VAL
 :sem (F::Abstr-obj (F::scale ONT::magenta-scale))
;  :sem (F::Abstr-obj (F::scale ONT::magenta*1--07--00))
 :wordnet-sense-keys ("magenta%5:00:00:chromatic:00")
 )

(define-type ONT::pink
 :parent ONT::color-VAL
 :sem (F::Abstr-obj (F::scale ONT::pink-scale))
;  :sem (F::Abstr-obj (F::scale ONT::pink*1--07--00))
 :wordnet-sense-keys ("pink%5:00:00:chromatic:00")
 )

(define-type ONT::tan
 :parent ONT::color-VAL
 :sem (F::Abstr-obj (F::scale ONT::tan-scale))
 :wordnet-sense-keys ("tan%5:00:00:chromatic:00")
 )

(define-type ONT::black
 :parent ONT::color-VAL
  :sem (F::Abstr-obj (F::scale ONT::black-scale))
; :sem (F::Abstr-obj (F::scale ONT::BLACKNESS*1--07--00))
 :wordnet-sense-keys ("black%3:00:01")
 )

(define-type ONT::white
 :parent ONT::color-VAL
 :sem (F::Abstr-obj (F::scale ONT::white-scale))
;  :sem (F::Abstr-obj (F::scale ONT::white*1--07--00))
 :wordnet-sense-keys ("white%3:00:01")
 )

(define-type ONT::gray
 :parent ONT::color-VAL
 :sem (F::Abstr-obj (F::scale ONT::grey-scale))
;  :sem (F::Abstr-obj (F::scale ONT::greyness*1--07--00))
 :wordnet-sense-keys ("gray%5:00:00:achromatic:00")
 )



;; sensory properties: PROPERTIES regarding the ABILITY to receive sensory input (e.g., invisible, visible, touchable, smellable)
(define-type ont::sensory-property-val
 :parent ont::physical-property-val 
  :wordnet-sense-keys ("sensory%3:00:00::" "sensorial%3:00:00::" "haptic%3:01:00::" "tactile%3:01:00::" "tactual%3:01:00::" "ocular%3:01:04::" "optic%3:01:04::" "optical%3:01:04::" "visual%3:01:04::" "exteroceptive%3:01:00::" "perceptual%3:01:00::" "interoceptive%3:01:00::" "proprioceptive%3:01:00::" "sensational%3:01:00::" "sensory%3:01:00::" "perceptive%3:01:00::" "somatosensory%3:01:00::" "euphonic%3:01:00::" "euphonical%3:01:00::" "allergic%3:01:00::" "cross-modal%3:01:00::" "olfactory%3:01:00::" "olfactive%3:01:00::" "auditory%3:01:00::" "audile%3:01:00::" "auditive%3:01:00::" "gustatory%3:01:00::" "gustative%3:01:00::" "gustatorial%3:01:00::" "auscultatory%3:01:00::" "squinty%3:01:00::" "anaphylactic%3:01:00::" "orthoptic%3:01:00::" "amphoric%3:01:00::" "synesthetic%3:01:00::" "synaesthetic%3:01:00::" "sensitizing%3:00:00::" "sensitising%3:00:00::")
 :comment "properties relating to the ability to receive sensory input (compare to ont::appearance-property-val)"
 :sem (F::abstr-obj (F::scale ont::sensory-scale ))
)

;; properties relating to ability to hear
(define-type ont::audibility-val
 :parent ont::sensory-property-val 
 :sem (F::abstr-obj (F::scale ont::auditory-scale ))
)

(define-type ont::audible-val
 :parent ont::audibility-val 
 :wordnet-sense-keys ("audible%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::auditory-scale ) (F::orientation f::pos))
)

(define-type ont::inaudible-val
 :parent ont::audibility-val 
 :wordnet-sense-keys ("inaudible%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::auditory-scale ) (F::orientation f::neg))
)

;; properties relating to ability to smell
(define-type ont::smellability-val
 :parent ont::sensory-property-val 
 :sem (F::abstr-obj (F::scale ont::olfactory-scale ))
)

(define-type ont::smellable-val
 :parent ont::smellability-val 
 :sem (F::abstr-obj (F::scale ont::olfactory-scale ) (F::orientation f::pos))
 ;no specific type under sensory-scale currently for smellability
)

(define-type ont::unsmellable-val
 :parent ont::smellability-val 
 :wordnet-sense-keys ("odorless%3:00:00")
 :sem (F::abstr-obj (F::scale ont::olfactory-scale ) (F::orientation f::neg))
 ;no specific type under sensory-scale currently for unsmellability
)

;; abiliity touch
(define-type ont::tangibility-val
 :parent ont::appearance-property-val 
 :sem (F::abstr-obj (F::scale ont::tactile-scale))
)

(define-type ont::tangible-val
 :parent ont::tangibility-val 
 :wordnet-sense-keys ("material%3:00:01::" "palpable%3:00:00" "tangible%3:00:04" "tangible%3:00:00" )
; :sem (F::abstr-obj (F::scale ont::tactile-scale) (F::orientation f::pos))
 :sem (F::abstr-obj (F::scale ont::tangibility-scale))
)

(define-type ont::intangible-val
 :parent ont::tangibility-val 
 :wordnet-sense-keys ("immaterial%3:00:01::" "nonmaterial%3:00:00::" "intangible%3:00:00" )
; :sem (F::abstr-obj (F::scale ont::tactile-scale) (F::orientation f::neg))
 :sem (F::abstr-obj (F::scale ont::intangibility-scale))
)

;; properties relating to ability to see
(define-type ont::visibility-val
 :parent ont::sensory-property-val 
 :sem (F::abstr-obj (F::scale ont::sight-scale))
)

(define-type ont::visible-val
 :parent ont::visibility-val 
 :wordnet-sense-keys ("unconcealed%3:00:00::" "visible%3:00:00" )
; :sem (F::abstr-obj (F::scale ont::visual-scale) (f::orientation f::pos))
 :sem (F::abstr-obj (F::scale ont::visibility-scale ))
)

(define-type ont::invisible-val
 :parent ont::visibility-val 
 :wordnet-sense-keys ("invisible%3:00:00" )
; :sem (F::abstr-obj (F::scale ont::visual-scale) (f::orientation f::neg))
 :sem (F::abstr-obj (F::scale ont::invisibility-scale ))
)

(define-type ont::hidden
 :parent ont::invisible-val 
 :wordnet-sense-keys ("veiled%3:00:00::" "concealed%3:00:00" "concealed%5:00:00:invisible:00")
 ; Words: (W::HIDDEN W::INVISIBLE W::OBSCURE)
 ; Antonym: NIL (W::VISIBLE)
)

;; properties relating to ability to perceive
(define-type ont::perceptibility-val
 :parent ont::sensory-property-val
 :sem (F::abstr-obj (F::scale ont::perceptibility-scale ))
)

(define-type ont::perceptible-val
 :parent ont::perceptibility-val 
 :wordnet-sense-keys ("distinct%3:00:00::" "discernible%3:00:00::" "discernable%3:00:00::" "perceptible%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::perceptibility-scale ) (f::orientation f::pos))
)

(define-type ont::imperceptible-val
 :parent ont::perceptibility-val 
 :wordnet-sense-keys ("indistinct%3:00:00::" "impalpable%3:00:00::" "indiscernible%3:00:00::" "imperceptible%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::perceptibility-scale ) (f::orientation f::neg))
)

;; properties relating to ability to sense
(define-type ont::sensitivity-val
 :parent ont::sensory-property-val 
 :wordnet-sense-keys ("sensitive%3:00:01" "sensitive%3:00:04" )
 :sem (F::abstr-obj (F::scale ont::sensitivity-scale ))
)


;; relationship between complete/complex whole and its parts
(define-type ont::part-whole-val
 :parent ont::physical-property-val 
 :arguments ((:optional ONT::GROUND )) 
 :sem (F::Abstr-obj )
 :comment "properties describing the relationship between the complete or complex whole and its parts"
)

;; complete vs. incomplete
(define-type ont::completeness-val
 :parent ont::part-whole-val
 :comment "having or not having all the necessary parts"
 :sem (F::abstr-obj (F::scale ont::completeness-scale))
)

(define-type ont::partial-incomplete
 :parent ont::completeness-val 
 :wordnet-sense-keys ("unfinished%3:00:02::" "discontinued%3:00:00::" "noncomprehensive%3:00:00::" "incomprehensive%3:00:00::" "fractional%3:00:00::" "partial%5:00:00:incomplete:00" "incomplete%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::incomplete-scale))
)

(define-type ont::whole-complete
 :parent ont::completeness-val 
 :wordnet-sense-keys ("finished%3:00:02::" "consummated%3:00:00::" "comprehensive%3:00:00::" "complete%3:00:00" "whole%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::complete-scale))
)

;; left over
(define-type ont::remaining-val
 :parent ont::part-whole-val 
 :wordnet-sense-keys ("net%3:00:00::" "nett%3:00:00::" "left%5:00:00:unexhausted:00" "remaining%5:00:00:unexhausted:00" )
 :comment "remaining after reaching a complete whole"
)

;; divided
(define-type ont::split-val
 :parent ont::part-whole-val 
 :wordnet-sense-keys ("divisible%3:00:00::" "uncombined%3:00:00::" "split%5:00:00:divided:00" "divided%3:00:00")
 :comment "sub-dividing a complete whole"
)

;; supplemental
(define-type ont::supplemental-val
 :parent ont::part-whole-val 
 :wordnet-sense-keys ("supplemental%5:00:00:additive:00" )
 :comment "supplemental addition to a complete whole"
)

;; positive, negative
(define-type ont::polarity-val
 :parent ont::physical-property-val 
 :wordnet-sense-keys ("bipolar%3:00:00::" "unipolar%3:00:00::" "charged%3:00:00")
 :sem (F::abstr-obj (F::scale ont::polarity-scale))
)

(define-type ont::polarity-val-positive
 :parent ont::polarity-val 
 :wordnet-sense-keys ("plus%3:00:00::" "positive%3:00:05" "positively%4:02:02" "positive%5:00:00:charged:00")
 :sem (F::abstr-obj (F::scale ont::polarity-scale) (f::orientation f::pos))
)

(define-type ont::polarity-val-negative
 :parent ont::polarity-val 
 :wordnet-sense-keys ("negative%3:00:01::" "minus%3:00:00::" "negative%3:00:05" "negatively%4:02:00" "negative%5:00:00:charged:00")
 :sem (F::abstr-obj (F::scale ont::polarity-scale) (f::orientation f::neg))
)

;; body-related-property-val
(define-type ont::body-condition-property-val
 :parent ont::physical-property-val 
 :comment "properties having to do with with conditions of the human/animal body; note, these adjectives are general purpose. for medical usages see ont::medical-condition-property-val"
 :sem (F::abstr-obj (F::scale ont::body-condition-scale) )
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Phys-obj (f::origin (? org f::human f::non-human-animal)))))
)

;; POSITIVE PROPERITIES
(define-type ont::positive-body-condition-property-val
 :parent ont::body-condition-property-val
)

(define-type ont::relaxed-val
 :parent ont::positive-body-condition-property-val 
 :wordnet-sense-keys ("relaxed%3:00:00" "restful%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::restlessness-scale) (f::orientation f::neg))
)

;; alertness
(define-type ont::alert-val
 :parent ont::positive-body-condition-property-val 
 :wordnet-sense-keys ("clearheaded%3:00:00::" "clear-thinking%3:00:00::" "wary%3:00:00::" "alert%5:00:00:aware:00" "alert%5:00:00:energetic:00" )
 :sem (F::abstr-obj (F::scale ont::alertness-scale))
)

;; energized, energetic
(define-type ont::energized-val
 :parent ont::positive-body-condition-property-val 
 :wordnet-sense-keys ("animated%3:00:00::" "alive%3:00:04::" "spirited%3:00:00::" "lively%3:00:00::" "energetic%3:00:00" "active%3:00:01")
 :comment "characterized by being full of energy and activity" 
:sem (F::abstr-obj (F::scale ont::body-energy-scale))
)

(define-type ont::athletic-val
 :parent ont::energized-val
 :wordnet-sense-keys ("acrobatic%5:00:00:active:01" "sporty%5:00:00:active:01")
)

(define-type ont::hyperactive-val
 :parent ont::energized-val
 :wordnet-sense-keys ("hyperactive%5:00:00:active:01")
)

;; healthy
(define-type ONT::not-ailing-val
  :parent ONT::positive-body-condition-property-val
  :comment "(uninjured)"
  )

(define-type ont::healthy-val
 :parent ONT::not-ailing-val
 :wordnet-sense-keys ("well%3:00:01" "healthy%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::health-scale))
)

;; satiated
(define-type ont::satiated-val
 :parent ont::positive-body-condition-property-val
 :wordnet-sense-keys ("nourished%3:00:00::" "satiated%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::satiated-scale))
)

;; fit
(define-type ont::fit-healthy-val
 :parent ONT::not-ailing-val
 :wordnet-sense-keys ("fit%3:00:01")
 :sem (F::abstr-obj (F::scale ont::fitness-scale))
)

;; NEGATIVE PROPERTIES
(define-type ont::negative-body-condition-property-val
 :parent ont::body-condition-property-val
)

;; restless, relaxed
;; THIS REALLY BELONGS TO EXPERIENCER CONDITION/PROPERTIES

(define-type ont::restless-val
 :parent ont::negative-body-condition-property-val 
 :wordnet-sense-keys ("restless%3:00:00" "edgy%5:00:00:tense:03" "nervy%5:00:00:tense:03" "jumpy%5:00:00:tense:03" "jittery%5:00:00:tense:03" "antsy%5:00:00:tense:03")
 :sem (F::abstr-obj (F::scale ont::restlessness-scale) (f::orientation f::pos))
)

(define-type ont::pained-val
 :parent ont::negative-body-condition-property-val
 :wordnet-sense-keys ("aching%5:00:00:painful:00" "achy%5:00:00:painful:00" "painful%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::pain-scale))
)

(define-type ont::not-satiated-val
 :parent ont::negative-body-condition-property-val
 :wordnet-sense-keys ("unsatiated%5:00:00:insatiate:00" "unsated%5:00:00:insatiate:00"  "insatiate%3:00:00")
 :sem (F::abstr-obj (F::scale ont::not-satiated-scale))
)

(define-type ont::thirsty-val
 :parent ont::not-satiated-val 
 :wordnet-sense-keys ("thirsty%3:00:00" )
)

(define-type ont::hungry
 :parent ont::not-satiated-val 
 :wordnet-sense-keys ("famished%5:00:00:hungry:00" "peckish%5:00:00:hungry:00" "hungry%3:00:00" )
 ; Words: (W::HUNGRY W::FAMISHED W::PECKISH)
 ; Antonym: NIL (W::THIRSTY)
)


(define-type ont::dehydration-val
 :parent ont::not-satiated-val
 :wordnet-sense-keys ("dehydrated%5:00:00:unhealthy:00" )
)

(define-type ont::ailing-val
 :parent ont::negative-body-condition-property-val
 :wordnet-sense-keys ("unwell%5:00:01:ill:01" "ailing%5:00:00:ill:01" "sick%3:00:01" "ill%3:00:01" "upset%5:00:00:ill:01" )
 ; Words: (W::SICK W::ILL W::UPSET W::GIDDY W::WOOZY W::LIGHTHEADED W::DIZZY W::UNWELL W::FEVERISH W::NAUSEOUS)
 ; Antonym: NIL (W::WELL)
 :sem (F::abstr-obj (F::scale ont::illness-scale))
 :comment "unhealthy, unwell or ailing"
)

(define-type ont::feeble-val
 :parent ont::negative-body-condition-property-val
 :wordnet-sense-keys ("frail%3:00:00")
 :comment "frail, feeble or weak in body"
 :sem (F::abstr-obj (F::scale ont::feebleness-scale))
)


(define-type ont::lack-of-energy-val
 :parent ont::negative-body-condition-property-val
  :wordnet-sense-keys ("unanimated%3:00:00::")
 :sem (F::abstr-obj (F::scale ont::lack-of-energy-scale))
)

(define-type ont::lethargic-val
 :parent ont::lack-of-energy-val
 :wordnet-sense-keys ("lax%3:00:01::" "spiritless%3:00:00::" "lethargic%3:00:00")
 :sem (F::abstr-obj (F::scale ont::lethargy-scale))
)

(define-type ont::fatigued-val
 :parent ont::lack-of-energy-val
 :wordnet-sense-keys ("tired%3:00:00" "exhausted%5:00:00:tired:00" "weary%5:00:00:tired:00" )
 :sem (F::abstr-obj (F::scale ont::fatigue-scale))
)

(define-type ont::dazed-val
 :parent ont::lack-of-energy-val
 :wordnet-sense-keys ("dazed%5:00:00:lethargic:00" "groggy%5:00:00:lethargic:00" )
)


;; NEUTRAL PROPERITIES
(define-type ont::neutral-body-condition-property-val
 :parent ont::body-condition-property-val
)

(define-type ont::drowsy-val
 :parent ont::neutral-body-condition-property-val
 :wordnet-sense-keys ("drowsy%5:00:00:inattentive:00" "drowsy%3:00:00:asleep:00")
 :sem (F::abstr-obj (F::scale ont::sleepiness-scale))
)


;; awake vs asleep
(define-type ont::awakeness-val
 :parent ont::neutral-body-condition-property-val 
)

(define-type ont::asleep-val
    :parent ont::awakeness-val
    :definitions (ont::not (ont::awake-val :figure ?neutral))
    :wordnet-sense-keys ("asleep%3:00:00::" "unawakened%3:00:00::" "dormant%3:00:00::" "inactive%3:00:05::" "asleep%4:02:00" )
    )

(define-type ont::awake-val
    :parent ont::awakeness-val
    :definitions (ont::not (ont::asleep-val :figure ?neutral))
    :wordnet-sense-keys ("awake%3:00:00" )
    )

;; sweaty
(define-type ont::sweaty-val
 :parent ont::neutral-body-condition-property-val 
 :wordnet-sense-keys ("clammy%5:00:00:wet:01" )
)

;; parallel to ont::medical-condition 
(define-type ont::medical-condition-property-val
 :parent ont::physical-property-val
)

;; mute, deaf, blind
(define-type ont::has-medical-condition
  :parent ont::medical-condition-property-val
  :arguments ((:essential ONT::FIGURE (F::phys-obj (f::origin (? org f::human f::non-human-animal)) (F::intentional +))))
  :wordnet-sense-keys ("neurotic%3:00:00::" "psychoneurotic%3:00:00::" "brachycephalic%3:00:00::" "brachycranial%3:00:00::" "brachycranic%3:00:00::" "incontinent%3:00:00::" "anastigmatic%3:00:00::" "stigmatic%3:00:00::" "astigmatic%3:00:00::" "hypertensive%3:00:00::" "mongoloid%3:01:00::" "narcoleptic%3:01:00::" "monochromatic%3:01:00::" "aneuploid%3:01:00::" "dyslexic%3:01:00::" "alexic%3:01:00::" "word-blind%3:01:00::" "cataleptic%3:01:00::" "anencephalic%3:01:00::" "anencephalous%3:01:00::" "bipolar%3:01:01::" "aphasic%3:01:00::" "agraphic%3:01:00::" "aniseikonic%3:01:00::" "acapnic%3:01:00::" "acapnial%3:01:00::" "acapnotic%3:01:00::" "amaurotic%3:01:00::" "immunosuppressed%3:01:00::" "aphakic%3:01:00::" "macrocephalic%3:01:00::" "macrocephalous%3:01:00::" "anosmic%3:01:00::" "anosmatic%3:01:00::" "hydrocephalic%3:01:00::" "amblyopic%3:01:00::" "macrencephalic%3:01:00::" "macrencephalous%3:01:00::" "hyperthermal%3:01:00::" "albinal%3:01:00::" "albinotic%3:01:00::" "albinic%3:01:00::" "albinistic%3:01:00::" "dysplastic%3:01:00::" "immunosuppressive%3:01:00::" "anoxic%3:01:00::" "squint-eyed%3:01:00::" "ateleiotic%3:01:00::" "anabiotic%3:01:00::" "tympanitic%3:01:00::" "anaesthetic%3:01:00::" "anesthetic%3:01:00::" "parous%3:01:00::" "immunocompromised%3:01:00::" "ataxic%3:01:00::" "atactic%3:01:00::" "adactylous%3:01:00::" "epileptic%3:01:00::" "dichromatic%3:01:00::" "microcephalic%3:01:00::" "microcephalous%3:01:00::" "nanocephalic%3:01:00::" "hypothermic%3:01:00::" "scotomatous%3:01:00::" "cyclothymic%3:01:00::" "anestrous%3:01:00::" "anestric%3:01:00::" "anoestrous%3:01:00::" "cryptobiotic%3:01:00::" "abasic%3:01:00::" "abatic%3:01:00::" "neurotic%3:01:00::" "bulimic%3:01:00::" "anaplastic%3:01:00::" "asphyxiated%3:01:00::" "progestational%3:01:00::" "sociopathic%3:01:00::" "gestational%3:01:00::" "deaf%3:00:00" "blind%3:00:00" "mute%5:00:01:inarticulate:00")
  )

;; properties relating to an injury
(define-type ONT::injury-property-val
 :parent ONT::medical-condition-property-val
 :wordnet-sense-keys ("bloody%3:00:00::" "puffy%5:00:00:unhealthy:00")
 )

;; medical symptoms
(define-type ont::medical-symptom-val
 :parent ONT::medical-condition-property-val
  :wordnet-sense-keys ("sciatic%3:01:01::" "apneic%3:01:00::" "apnoeic%3:01:00::" "natriuretic%3:01:00::" "myalgic%3:01:00::" "arthralgic%3:01:00::" "amenorrheic%3:01:00::" "amenorrhoeic%3:01:00::" "amenorrheal%3:01:00::" "amenorrhoeal%3:01:00::" "calcific%3:01:00::" "congestive%3:01:00::" "granulomatous%3:01:00::" "neuralgic%3:01:00::" "albuminuric%3:01:00::" "catarrhal%3:01:00::" "anicteric%3:01:00::" "icterogenic%3:01:00::" "prodromal%3:01:00::" "prodromic%3:01:00::" "eruptive%3:01:00::" "hyperemic%3:01:00::" "spastic%3:01:00::" "anasarcous%3:01:00::" "neuromatous%3:01:00::" "symptomatic%3:01:00::" "tetanic%3:01:01::" "hypoglycemic%3:01:00::" "hypoglycaemic%3:01:00::" "afebrile%3:01:00::" "atrophic%3:01:00::")
)

(define-type ont::dizzy-val
 :parent ont::medical-symptom-val
 :wordnet-sense-keys ("giddy%5:00:00:ill:01" "lightheaded%5:00:00:ill:01" "woozy%5:00:00:ill:01" "nauseated%5:00:00:ill:01" "dizzy%5:00:00:ill:01" "faint%5:00:00:ill:01" )
)

(define-type ont::breathless-val
 :parent ont::medical-symptom-val
 :wordnet-sense-keys ("breathless%3:00:00" )
)

(define-type ont::feverish-val
 :parent ont::medical-symptom-val
 :wordnet-sense-keys ("feverish%5:00:00:ill:01" "feverish%3:01:00" )
)

;; life-process adjectives
(define-type ont::life-process-val
 :parent ont::property-val
  :wordnet-sense-keys ("flowering%3:00:00::")
 :comment "properties that describe life processes"
 :sem (F::abstr-obj (F::scale ont::life-process-scale))
)

(define-type ont::of-death-val
 :parent ont::life-process-val
 :wordnet-sense-keys ("dying%3:00:00")
 :comment "relating to death"
)

(define-type ont::causing-death-val
 :parent ont::of-death-val
 :wordnet-sense-keys ("fatal%3:00:00")
)

(define-type ont::not-causing-death-val
 :parent ont::of-death-val
 :wordnet-sense-keys ("nonfatal%3:00:00")
)

(define-type ont::after-death-val
 :parent ont::of-death-val
 :wordnet-sense-keys ("postmortem%3:00:00" "postmortem%5:00:00:succeeding:00")
)

(define-type ont::of-mortality-val
 :parent ont::life-process-val
 :wordnet-sense-keys ("mortal%3:00:00" "immortal%3:00:00")
 :comment "subjectness to death"
)

(define-type ont::of-birth-val
 :parent ont::life-process-val
 :wordnet-sense-keys ("full-term%3:00:00::" "natal%3:01:00" "perinatal%3:00:00")
)

(define-type ont::after-birth-val
 :parent ont::of-birth-val
 :wordnet-sense-keys ("nidicolous%3:00:00::" "postnatal%3:00:00" "neonatal%3:01:00")
)

(define-type ont::before-birth-val
 :parent ont::of-birth-val
 :wordnet-sense-keys ("unborn%3:00:00::" "pregnant%3:00:00::" "prenatal%3:00:00")
)

;; dead vs alive
(define-type ont::living-val
 :parent ont::life-process-val
 :arguments ((:ESSENTIAL ONT::FIGURE (F::phys-obj (f::intentional + )(f::type ont::organism )))) 
 :comment "dead vs. alive"
)

(define-type ont::alive
 :parent ont::living-val 
 :wordnet-sense-keys ("live%3:00:00" "living%3:01:00" "living%5:00:00:extant:00")
 :arguments ((:ESSENTIAL ONT::FIGURE (F::phys-obj (f::intentional + )(f::type ont::organism )))) 
)

(define-type ont::dead
 :parent ont::living-val 
 :wordnet-sense-keys ("dead%3:00:02" "dead%3:00:01" "extinct%3:00:02")
 :arguments ((:ESSENTIAL ONT::FIGURE (F::phys-obj (f::intentional + )(f::type ont::organism )))) 
)

(define-type ont::not-living-val
 :parent ont::life-process-val
 :wordnet-sense-keys ("inorganic%3:00:02::" "extinct%3:00:01::" "nonextant%3:00:02::" "non-living%3:00:00" )
 :comment "e.g., rock, water, metal"
)


;;; process-related adjectives
(define-type ont::process-val
  :parent ont::property-val 
  :comment "properties that describe processes"
  :sem (F::abstr-obj (F::scale ont::process-property-scale))
)

;; adjectives meaning "can [not] be verb'd" for some verb
(define-type ont::can-be-done-val
 :parent ont::property-val 
 :wordnet-sense-keys ("inheritable%3:00:00::" "heritable%3:00:00::" "excitable%3:00:00::" "noninheritable%3:00:00::" "nonheritable%3:00:00::" "noncombustible%3:00:00::" "incombustible%3:00:00::" "unprofitable%3:00:00::" "profitable%3:00:00::" "passable%3:00:00::" "digestible%3:00:00::" "edible%3:00:00::" "comestible%3:00:00::" "eatable%3:00:00::" "nontaxable%3:00:00::" "exempt%3:00:02::" "impassable%3:00:00::" "unpassable%3:00:00::" "corrigible%3:00:00::" "attachable%3:00:00::" "unforgettable%3:00:00::" "incalculable%3:00:00::" "unpardonable%3:00:00::" "inevitable%3:00:00::" "determinable%3:00:00::" "expressible%3:00:00::" "pardonable%3:00:00::" "contestable%3:00:00::" "thinkable%3:00:00::" "indivisible%3:00:00::" "disposable%3:00:02::" "indeterminable%3:00:00::" "undeterminable%3:00:00::" "collapsible%3:00:00::" "collapsable%3:00:00::" "reversible%3:00:00::" "commutable%3:00:00::" "perishable%3:00:00::" "salable%3:00:00::" "saleable%3:00:00::" "infallible%3:00:00::" "undeniable%3:00:00::" "calculable%3:00:00::" "imperishable%3:00:00::" "incorrigible%3:00:00::" "undatable%3:00:00::" "soluble%3:00:02::" "unalterable%3:00:00::" "inalterable%3:00:00::" "deniable%3:00:00::" "revocable%3:00:00::" "revokable%3:00:00::" "placable%3:00:00::" "distinguishable%3:00:00::" "recoverable%3:00:00::" "incommutable%3:00:00::" "unrecoverable%3:00:00::" "irrecoverable%3:00:00::" "unsalable%3:00:00::" "unsaleable%3:00:00::" "immeasurable%3:00:00::" "unmeasurable%3:00:00::" "immensurable%3:00:00::" "unmeasured%3:00:00::" "extensile%3:00:00::" "extensible%3:00:00::" "indeterminate%3:00:01::" "undetermined%3:00:04::" "expendable%3:00:00::" "implacable%3:00:00::" "inexhaustible%3:00:00::" "inexcusable%3:00:00::" "forgettable%3:00:00::" "detachable%3:00:00::" "indefeasible%3:00:00::" "returnable%3:00:00::" "exchangeable%3:00:00::" "unexchangeable%3:00:00::" "inconsolable%3:00:00::" "disconsolate%3:00:04::" "unconsolable%3:00:00::" "livable%3:00:00::" "liveable%3:00:00::" "excusable%3:00:00::" "nondisposable%3:00:02::" "unlivable%3:00:00::" "unliveable%3:00:00::" "disposable%3:00:01::" "exportable%3:00:00::" "compressible%3:00:00::" "washable%3:00:00::" "scalable%3:00:00::" "explicable%3:00:00::" "inviolable%3:00:00::" "reversible%3:00:02::" "two-sided%3:00:02::" "noncollapsible%3:00:00::" "noncollapsable%3:00:00::" "inedible%3:00:00::" "uneatable%3:00:00::" "indigestible%3:00:00::" "exhaustible%3:00:00::" "inexpressible%3:00:00::" "unexpressible%3:00:00::" "unexcitable%3:00:00::" "stoppable%3:00:00::" "ponderable%3:00:00::" "irreversible%3:00:00::" "retractile%3:00:00::" "nonreflective%3:00:00::" "nonreflecting%3:00:00::" "irrevocable%3:00:00::" "irrevokable%3:00:00::" "unstoppable%3:00:00::" "pronounceable%3:00:00::" "unintelligible%3:00:00::" "unrenewable%3:00:00::" "nonrenewable%3:00:00::" "nonadsorbent%3:00:00::" "nonadsorptive%3:00:00::" "rentable%3:00:00::" "unpreventable%3:00:00::" "unshrinkable%3:00:00::" "extricable%3:00:00::" "unexportable%3:00:00::" "datable%3:00:00::" "dateable%3:00:00::" "operable%3:00:00::" "unexpendable%3:00:00::" "submersible%3:00:00::" "submergible%3:00:00::" "unreportable%3:00:00::" "unportable%3:00:00::" "nonretractile%3:00:00::" "nonretractable%3:00:00::" "nonextensile%3:00:00::" "inextensible%3:00:00::" "nonprotractile%3:00:00::" "shockable%3:00:00::" "narrow-minded%3:00:04::" "unactable%3:00:00::" "indistinguishable%3:00:00::" "undistinguishable%3:00:00::" "incurable%3:00:00::" "playable%3:00:00::" "alterable%3:00:00::" "nondisposable%3:00:01::" "invertible%3:00:00::" "unemployable%3:00:00::" "unappealable%3:00:00::" "traceable%3:00:00::" "trackable%3:00:00::" "incompressible%3:00:00::" "adsorbable%3:00:00::" "adsorbate%3:00:00::" "nonsubmersible%3:00:00::" "nonsubmergible%3:00:00::" "repeatable%3:00:00::" "quotable%3:00:00::" "actable%3:00:00::" "nonwashable%3:00:00::" "bridgeable%3:00:00::" "wearable%3:00:00::" "nonvolatile%3:00:00::" "nonvolatilizable%3:00:00::" "nonvolatilisable%3:00:00::" "knowable%3:00:00::" "cognizable%3:00:00::" "cognisable%3:00:00::" "cognoscible%3:00:00::" "unscalable%3:00:00::" "unclimbable%3:00:00::" "puncturable%3:00:00::" "imponderable%3:00:00::" "preventable%3:00:00::" "resistible%3:00:00::" "nonreversible%3:00:00::" "one-sided%3:00:02::" "consolable%3:00:00::" "non-invertible%3:00:00::" "paintable%3:00:00::" "printable%3:00:00::" "unpronounceable%3:00:00::" "employable%3:00:00::" "unopposable%3:00:00::" "unpublishable%3:00:00::" "irreducible%3:00:00::" "publishable%3:00:00::" "undrinkable%3:00:00::" "unsinkable%3:00:00::" "translatable%3:00:00::" "nondeductible%3:00:00::" "adoptable%3:00:00::" "seasonable%3:00:00::" "drinkable%3:00:00::" "potable%3:00:00::" "unrentable%3:00:00::" "inoperable%3:00:00::" "appealable%3:00:00::" "unshockable%3:00:00::" "broad-minded%3:00:04::" "irreplaceable%3:00:00::" "unreplaceable%3:00:00::" "inextinguishable%3:00:00::" "unseasonable%3:00:00::" "unadoptable%3:00:00::" "nonreturnable%3:00:00::" "opposable%3:00:00::" "apposable%3:00:00::" "reportable%3:00:00::" "sinkable%3:00:00::" "unrepeatable%3:00:00::" "unquotable%3:00:00::" "reducible%3:00:00::" "untraceable%3:00:00::" "nonarbitrable%3:00:00::" "absorbable%3:00:00::" "unpaintable%3:00:00::" "curable%3:00:00::" "unbridgeable%3:00:00::" "shrinkable%3:00:00::" "defeasible%3:00:00::" "unprintable%3:00:00::" "untranslatable%3:00:00::" "extinguishable%3:00:00::" "arbitrable%3:00:00::" "unwearable%3:00:00::" "violable%3:00:00::" "renewable%3:00:00::" "unplayable%3:00:00::" "breakable%3:00:00::" "unchangeable%3:00:00::" "intractable%3:00:00::" "admissible%3:00:00::" "attributable%3:00:00::" "destructible%3:00:00::" "deductible%3:00:00::" "inconvertible%3:00:00::" "unconvertible%3:00:00::" "unexchangeable%3:00:04::" "indispensable%3:00:00::" "inadmissible%3:00:00::" "nonabsorbent%3:00:00::" "nonabsorptive%3:00:00::" "indestructible%3:00:00::" "convertible%3:00:00::" "exchangeable%3:00:04::" "sympathetic%3:00:02::" "appealing%3:00:02::" "likeable%3:00:02::" "likable%3:00:02::" "charitable%3:01:00::" "unattributable%3:00:00::" "unascribable%3:00:00::" "dispensable%3:00:00::" "inhospitable%3:00:02::" "machine_readable%3:01:00::" "computer_readable%3:01:00::" "differentiable%3:01:00::" "hydrolyzable%3:01:00::" "packable%3:01:00::" "fermentable%3:01:00::" "positionable%3:01:00::" "nonfissionable%3:00:00::" "unsympathetic%3:00:02::" "unappealing%3:00:02::" "unlikeable%3:00:02::" "unlikable%3:00:02::" "deliverable%3:01:00::" "metastable%3:01:00::" "stainable%3:01:00::" "sustainable%3:01:00::" "diagonalizable%3:01:00::" "fissionable%3:00:00::" "fissile%3:00:02::" "uninjectable%3:00:00::" "injectable%3:00:00::" "measurable%3:00:00::" "mensurable%3:00:00::" "mensural%3:01:00::" "measured%3:01:00::" "mensurable%3:01:00::")
 :arguments ((:optional ONT::GROUND ))
 :sem (F::abstr-obj (F::scale ont::can-be-done-scale)) ;process-property-scale)) 
)

;; reparability, fixability, able to remedy, resolvable
(define-type ont::reparability-val
 :parent ont::can-be-done-val 
)

(define-type ont::reparable-val
 :parent ont::reparability-val
 :wordnet-sense-keys ("remediable%3:00:00" "reconcilable%3:00:00" "reparable%3:00:00")
 :comment "resolvable, reparable, fixable, remediable"
)

(define-type ont::not-reparable-val
 :parent ont::reparability-val
 :wordnet-sense-keys ("irreconcilable%3:00:00" "irremediable%3:00:00" "irreparable%3:00:00")
)

;; (in)accessible
(define-type ont::accessibility-val
 :parent ont::can-be-done-val 
 :arguments ((:REQUIRED ONT::FIGURE (F::phys-obj ))(:ESSENTIAL ONT::GROUND (F::phys-obj ))) 
; :sem (F::abstr-obj (F::scale ont::accessibility-scale))
)

(define-type ont::accessible-val
 :parent ont::accessibility-val 
 :wordnet-sense-keys ("accessible%3:00:00" )
; :sem (F::abstr-obj (F::scale ont::accessibility-scale) (f::orientation f::pos))
)

(define-type ont::not-accessible-val
 :parent ont::accessibility-val 
 :wordnet-sense-keys ("inaccessible%3:00:00" )
; :sem (F::abstr-obj (F::scale ont::accessibility-scale) (f::orientation f::neg))
)


;; adhearable stickable
(define-type ont::adhearable-val
 :parent ont::can-be-done-val
 :wordnet-sense-keys ("adhesive%3:00:00")
)

;; (un)manageable, (un)controllable
(define-type ont::manageability-val
 :parent ont::can-be-done-val 
)

(define-type ont::manageable
 :parent ont::manageability-val 
 :wordnet-sense-keys ("controllable%5:00:00:manageable:00" "manageable%3:00:00" )
 ; Words: (W::CONTROLLABLE W::MANAGEABLE)
 ; Antonym: ONT::unmanageable (W::UNCONTROLLABLE W::UNMANAGEABLE)
 :sem (F::abstr-obj (F::scale ont::manageability-scale) (f::orientation f::pos))
)

(define-type ont::unmanageable
 :parent ont::manageability-val 
 :wordnet-sense-keys ("indocile%5:00:00:unmanageable:00" "unmanageable%3:00:00" )
 ; Words: (W::UNCONTROLLABLE W::UNMANAGEABLE)
 ; Antonym: ONT::manageable (W::CONTROLLABLE W::MANAGEABLE)
 :sem (F::abstr-obj (F::scale ont::manageability-scale) (f::orientation f::neg))
)

;; (un)enforceable
(define-type ont::enforceability-val
 :parent ont::can-be-done-val 
)

(define-type ont::not-enforceable-val
 :parent ont::enforceability-val 
 :wordnet-sense-keys ("unenforceable%3:00:00" )
)

(define-type ont::enforceable-val
 :parent ont::enforceability-val 
 :wordnet-sense-keys ("enforceable%3:00:00" )
)

;; chargeable
(define-type ont::chargeability-val
 :parent ont::can-be-done-val 
)

(define-type ont::chargeable-val
 :parent ont::chargeability-val 
)

;; expandable
(define-type ont::expandability-val
 :parent ont::can-be-done-val 
)

(define-type ont::expandable-val
 :parent ont::expandability-val
 :wordnet-sense-keys ("expandable%5:00:00:expansive:00" "expansive%3:00:00" "expandable%5:00:00:elastic:00")
)

;; networkable
(define-type ont::networkable-val
 :parent ont::can-be-done-val 
)

;; replaceable
(define-type ont::replaceability-val
 :parent ont::can-be-done-val 
 :sem (F::abstr-obj (F::scale ont::replaceability-scale))
)

(define-type ont::replaceable-val
 :parent ont::replaceability-val
 :wordnet-sense-keys ("interchangeable%5:00:00:replaceable:00" "replaceable%3:00:00")
 :sem (F::abstr-obj (F::scale ont::replaceability-scale) (f::orientation f::pos))
)

;; removable, portable
(define-type ont::movable-val
 :parent ont::can-be-done-val 
)

(define-type ont::removable-val
 :parent ont::movable-val 
 :wordnet-sense-keys ("removable%3:00:00" )
)

(define-type ont::portable-val
 :parent ont::movable-val 
 :wordnet-sense-keys ("mobile%3:00:00::" "portable%3:00:00" "movable%5:00:00:portable:00" "movable%5:00:00:mobile:00")
 :sem (F::abstr-obj (F::scale ont::portability-scale))
)

;; readable, writable, burnable
(define-type ont::rw-status-val
 :parent ont::can-be-done-val 
)

(define-type ont::r-able-val
 :parent ont::rw-status-val 
)

(define-type ont::w-able-val
 :parent ont::rw-status-val 
)

;; Relating to time
(define-type ont::temporal-val
 :parent ont::property-val 
  :wordnet-sense-keys ("dominical%3:01:00::" "sabbatarian%3:01:00::" "semicentennial%3:01:00::" "semicentenary%3:01:00::" "tricentenary%3:01:00::" "tricentennial%3:01:00::" "spatiotemporal%3:01:00::" "quincentennial%3:01:00::" "quincentenary%3:01:00::" "ferial%3:01:00::" "temporal%3:01:01::" "equinoctial%3:01:00::" "bicentennial%3:01:00::" "bicentenary%3:01:00::" "sabbatical%3:01:01::" "sabbatic%3:01:00::")
 :comment "properties relating to time"
 :sem (F::abstr-obj (F::scale ont::temporal-scale))
)

;; temporal occurrence
(define-type ont::temporal-occurrence-val
 :parent ont::temporal-val
 :sem (F::abstr-obj (F::scale ont::temporal-occurrence-scale))
)

;; continuous, uninterrupted, can be either time or space dimensionality
(define-type ont::continuous-val
 :parent ont::temporal-occurrence-val 
  :wordnet-sense-keys ("continued%3:00:00::")
)

(define-type ont::continuous
 :parent ont::continuous-val 
 :wordnet-sense-keys ("unbroken%3:00:02::" "perpetual%5:00:00:continuous:01" "continuous%3:00:01" "uninterrupted%3:00:00" )
)

(define-type ont::discontinuous
 :parent ont::continuous-val 
 :wordnet-sense-keys ("broken%3:00:02::" "discontinuous%3:00:01" )
)

;; for frequency adjectives that modify event nouns: regular, frequent, daily, monthly, weekly
(define-type ont::frequency-val
 :parent ont::temporal-occurrence-val 
 :arguments ((:REQUIRED ONT::FIGURE )) ;;(F::situation)) ;; weekly meeting; daily vitamin; daily routine/practice
 :sem (F::abstr-obj )
 ;; f::situation restriction is too strong
)

(define-type ont::periodic-val
 :parent ont::frequency-val 
 :wordnet-sense-keys ("extrasystolic%3:01:00::" "migrational%3:01:00::" "tidal%3:01:00::" "systolic%3:01:00::" "tertian%3:01:00::" "diastolic%3:01:00::" "cyclic%3:01:00::" "periodic%3:00:00" )
)

(define-type ont::specified-period-val
 :parent ont::periodic-val 
 :wordnet-sense-keys ("horary%3:01:00::" "circadian%3:01:00::" "daily%5:00:00:periodic:00" "annual%5:00:00:periodic:00" "weekly%5:00:00:periodic:00" "monthly%5:00:00:periodic:00" "seasonal%3:00:00" )
)

(define-type ont::repetitive-val
 :parent ont::frequency-val 
 :wordnet-sense-keys ("cyclic%3:00:01::" "cyclical%3:00:00::" "continual%3:00:00")
)

(define-type ont::regularity-val
 :parent ont::frequency-val 
 :sem (F::abstr-obj (F::scale ont::regularity-scale))
)

(define-type ont::regular
 :parent ont::regularity-val 
 :wordnet-sense-keys ("regular%5:00:00:steady:00" "regular%3:00:00")
 ; Words: (W::REGULAR)
 :sem (F::abstr-obj (F::scale ont::regular-scale))
)

(define-type ont::irregular
 :parent ont::regularity-val 
 :wordnet-sense-keys ("aperiodic%3:00:00::" "nonperiodic%3:00:00::" "sporadic%3:00:00" "irregular%5:00:00:sporadic:00" "casual%5:00:00:irregular:00" )
 ; Words: (W::IRREGULAR)
 :sem (F::abstr-obj (F::scale ont::not-regular-scale))
)

(define-type ont::frequent
 :parent ont::frequency-val 
 :wordnet-sense-keys ("frequent%3:00:00" )
 ; Words: (W::FREQUENT)
)

(define-type ont::occasional
 :parent ont::frequency-val 
 :wordnet-sense-keys ("rare%5:00:00:infrequent:00" "infrequent%3:00:00" "occasional%5:00:00:infrequent:00" )
 ; Words: (W::RARE W::OCCASIONAL W::INFREQUENT)
)

;; incremental, step by step
(define-type ont::incremental-val
 :parent ont::temporal-occurrence-val 
 :wordnet-sense-keys ("incremental%5:00:00:additive:00" )
)

;; persistence
(define-type ont::persistence-val
 :parent ont::temporal-occurrence-val 
; :sem (F::Abstr-obj (F::scale ONT::TIME-MEASURE-SCALE ));(F::TIME-SCALE F::INTERVAL ))
)

(define-type ont::persistent
 :parent ont::persistence-val 
 :wordnet-sense-keys ("lasting%5:00:00:long:02" "permanent%3:00:00" "lasting%5:00:00:stable:00" "persistent%5:00:00:continual:00")
)

(define-type ont::temporary
 :parent ont::persistence-val 
 :wordnet-sense-keys ("impermanent%5:00:00:finite:00" "transient%5:00:00:impermanent:00" "temporary%3:00:00" )
)

;; status of a process
(define-type ont::process-status-val
 :parent ont::process-val
 :comment "properties relating to status of processes"
 :sem (F::abstr-obj (F::scale ont::process-status-scale))
)

;; done, finished
(define-type ont::completion-val
 :parent ont::process-status-val 
 :arguments ((:optional ONT::GROUND )) 
 :sem (F::Abstr-obj (F::gradability - ))
 :comment "done or finished"
)

(define-type ont::finished
 :parent ont::completion-val 
 :wordnet-sense-keys ("finished%3:00:01" "dead%5:00:00:complete:00" "done%5:00:00:finished:01" "complete%5:00:00:finished:01" "accomplished%5:00:00:complete:00" )
 ; Words: (W::COMPLETE W::FINISHED W::UTTER W::COMPLETED W::DONE)
 ; Antonym: ONT::INCOMPLETE (W::UNFINISHED W::INCOMPLETE)
)

;; "incomplete" is a misnomer. better label would be not-finished.
(define-type ont::incomplete
 :parent ont::completion-val 
 :wordnet-sense-keys ("unfinished%3:00:01" "incomplete%5:00:00:unfinished:01" )
 ; Words: (W::UNFINISHED W::INCOMPLETE)
 ; Antonym: ONT::FINISHED (W::COMPLETE W::FINISHED W::UTTER W::COMPLETED W::DONE)
 :comment "unfinished"
)

;; mid, middle, midway, halfway
(define-type ont::stage-val
 :parent ont::process-status-val 
)

(define-type ont::tentative-val
 :parent ont::stage-val 
 :wordnet-sense-keys ("tentative%5:00:00:conditional:00" "provisional%5:00:00:conditional:00" "preliminary%5:00:00:exploratory:00")
)

(define-type ont::startup-val
 :parent ont::stage-val 
 :wordnet-sense-keys ("alpha%5:00:00:exploratory:00" "beta%5:00:00:exploratory:00" "introductory%5:00:00:preceding:00" "preparatory%5:00:00:preceding:00" "premedical%5:00:00:preceding:00")
)

;; resulting
(define-type ont::outcome-val
 :parent ont::process-status-val 
 :wordnet-sense-keys ("ensuing%5:00:00:succeeding:00" )
 :comment "outcome of the process"
)

;; process evaluation
(define-type ont::process-evaluation-val
  :parent ont::evaluation-attribute-val ;process-val
  :comment "evaluation properties of processes"
  :sem (F::abstr-obj (F::scale ont::process-evaluation-scale))
)


;; process evaluation
;(define-type ont::process-evaluation-val
; :parent ont::evaluation-attribute-val ;process-val
; :comment "evaluation properties of processes"
; :sem (F::abstr-obj (F::scale ont::process-evaluation-scale))
;)

;; productive
(define-type ont::productivity-val
 :parent ont::process-evaluation-val
 :sem (F::abstr-obj (F::scale ont::productivity-scale))
)

(define-type ont::productive-val
 :parent ont::productivity-val 
 :wordnet-sense-keys ("productive%3:00:00" "productive%5:00:00:fruitful:00" "fruitful%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::productivity-scale) (f::orientation f::pos))
)

;; useful vs. useless
(define-type ont::usefulness-val
 :parent ont::evaluation-attribute-val
 :sem (F::abstr-obj (F::scale ont::utility-scale))
)

;; useful
(define-type ont::useful
 :parent ont::usefulness-val 
 :wordnet-sense-keys ("useful%3:00:00" "utilitarian%5:00:00:useful:00" "functional%5:00:00:practical:00" "practical%3:00:00" "functional%3:00:00" )
 ; Words: (W::USEFUL W::PRACTICAL W::FUNCTIONAL)
 ; Antonym: ONT::useless (W::USELESS W::IMPRACTICAL)
 :sem (F::abstr-obj (F::scale ont::utility-scale) (f::orientation f::pos))
)

(define-type ont::useless
 :parent ont::usefulness-val 
 :wordnet-sense-keys ("purposeless%3:00:00::" "impractical%3:00:00" "useless%3:00:00" )
 ; Words: (W::USELESS W::IMPRACTICAL)
 ; Antonym: ONT::USEFUL (W::USEFUL W::PRACTICAL W::FUNCTIONAL)
 :sem (F::abstr-obj (F::scale ont::utility-scale) (f::orientation f::neg))
)

;; effective vs. ineffective
(define-type ont::effectiveness-val
 :parent ont::process-evaluation-val
 :comment "evaluation attributes dealing with the effectiveness or efficacy of something"
 :sem (F::abstr-obj (F::scale ont::effectiveness-scale))
)

(define-type ont::effective-val
 :parent ont::effectiveness-val
 :wordnet-sense-keys ("effective%3:00:00" "effective%5:00:00:efficacious:00" "efficacious%3:00:00")
 :sem (F::abstr-obj (F::scale ont::effectiveness-scale) (f::orientation f::pos))
)

(define-type ont::not-effective-val
 :parent ont::effectiveness-val
 :wordnet-sense-keys ("ineffective%3:00:00" "toothless%5:00:00:ineffective:00")
 :sem (F::abstr-obj (F::scale ont::effectiveness-scale) (f::orientation f::neg))
)

;; (in)efficient
(define-type ont::efficiency-val
 :parent ont::process-evaluation-val 
 :sem (F::abstr-obj (F::scale ont::efficiency-scale))
)

(define-type ont::efficient-val
 :parent ont::efficiency-val 
 :wordnet-sense-keys ("efficient%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::efficiency-scale) (f::orientation f::pos))
)

(define-type ont::not-efficient-val
 :parent ont::efficiency-val 
 :wordnet-sense-keys ("inefficient%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::efficiency-scale) (f::orientation f::neg))
)

;; (un)steady, off balance
(define-type ont::steadiness-val
 :parent ont::process-evaluation-val 
 :sem (F::abstr-obj (F::scale ont::steadiness-scale))
)

(define-type ont::steady
 :parent ont::steadiness-val 
 :wordnet-sense-keys ("constant%3:00:00::" "inconstant%3:00:00::" "stable%3:00:00" "unchanged%3:00:04" "unchanged%3:00:00" "steady%3:00:00" "steady%5:00:00:stable:00" )
 :sem (F::abstr-obj (F::scale ont::steady-scale))
)


(define-type ont::unsteady
 :parent ont::steadiness-val 
 :wordnet-sense-keys ("unsteady%3:00:00" "volatile%3:00:00" "unstable%3:00:00" "shaky%5:00:00:unsteady:00" )
 ; Words: (W::UNSTEADY W::SHAKY)
 ; Antonym: NIL (W::STEADY)
 :sem (F::abstr-obj (F::scale ont::not-steady-scale))
)

; (un)successful
(define-type ont::success-val
 :parent ont::process-evaluation-val 
 :sem (F::abstr-obj (F::scale ont::successfulness-scale))
)

(define-type ont::successful-val
 :parent ont::success-val 
 :wordnet-sense-keys ("successful%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::successful-scale))
)

(define-type ont::not-successful-val
 :parent ont::success-val 
 :wordnet-sense-keys ("unfruitful%3:00:00::" "unsuccessful%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::failure-scale))
)

;; "tough" adjectives, e.g. hard, easy, possible
;; the task is easy for him to do/understand
(define-type ont::task-complexity-val
 :parent ont::process-evaluation-val 
 :arguments ((:essential ONT::Affected )(:essential ont::FORMAL ((? ct f::phys-obj f::abstr-obj f::situation )))) 
 :sem (F::abstr-obj (F::scale ont::task-complexity-scale))
)

(define-type ont::difficulty-val
 :parent ont::task-complexity-val 
)

(define-type ont::difficult
 :parent ont::difficulty-val 
 :wordnet-sense-keys ("effortful%3:00:00::" "tough%3:00:03::" "unwieldy%3:00:00::" "unmanageable%3:00:04::" "catchy%5:00:00:difficult:00" "complex%3:00:00" "ambitious%5:00:00:difficult:00" "arduous%5:00:00:difficult:00" "difficult%3:00:00" "rugged%5:00:00:difficult:00" "complicated%5:00:00:complex:00" )
 ; Words: (W::DIFFICULT W::HARD W::COMPLEX W::TOUGH W::COMPLICATED W::TRICKY W::CHALLENGING W::ARDUOUS)
 ; Antonym: ONT::easy (W::EASY W::SIMPLE)
 :sem (F::abstr-obj (F::scale ont::difficult-scale))
)

(define-type ont::easy
 :parent ont::difficulty-val 
 :wordnet-sense-keys ("undemanding%3:00:01::" "effortless%3:00:00::" "elementary%5:00:00:easy:01" "easy%3:00:01" "simple%3:00:02" )
 ; Words: (W::EASY W::SIMPLE)
 ; Antonym: ONT::difficult (W::DIFFICULT W::HARD W::COMPLEX W::TOUGH W::COMPLICATED W::TRICKY W::CHALLENGING W::ARDUOUS)
  :sem (F::abstr-obj (F::scale ont::easy-scale))
)

(define-type ont::straightforward-val
 :parent ont::difficulty-val 
 :wordnet-sense-keys ("straightforward%5:00:00:unequivocal:00" )
)

(define-type ont::demanding-val
 :parent ont::difficulty-val 
 :wordnet-sense-keys ("tight%5:00:00:demanding:01" "rigorous%5:00:00:demanding:01" "demanding%3:00:01" )
)

;; animal-propensity
(define-type ont::animal-propensity-val
 :parent ont::property-val 
  :wordnet-sense-keys ("diligent%3:00:00::" "enterprising%3:00:00::")
 :comment "properties relating to human or animal tendencies or inclinations to behave in a particular manner"
 :sem (F::abstr-obj (F::scale ont::behavioral-scale))
)


(define-type ont::disposition-val ;animal-disposition-val
 :parent ont::animal-propensity-val
 :wordnet-sense-keys ("temperamental%3:01:00::" "adventuristic%3:01:00::")
 :sem (F::abstr-obj (F::scale ont::disposition-scale))
)

;; grumpy, cholaric, churlish, crabbed etc
(define-type ont::negative-disposition-val
 :parent ont::disposition-val ;animal-disposition-val
 :wordnet-sense-keys ("ill-natured%3:00:00") ;"cantankerous%5:00:00:ill-natured:00" "churlish%5:00:00:ill-natured:00" "currish%5:00:00:ill-natured:00" "disagreeable%5:00:00:ill-natured:00" "hotheaded%5:00:00:ill-natured:00" "ill-humored%5:00:00:ill-natured:00" "misanthropic%5:00:00:ill-natured:00" "misogynous%5:00:00:ill-natured:00" "moody%5:00:00:ill-natured:00" "nagging%5:00:00:ill-natured:00" "prickly%5:00:00:ill-natured:00" "snappy%5:00:00:ill-natured:00" "snorty%5:00:00:ill-natured:00" "spoilt%5:00:00:ill-natured:00" "sulky%5:00:00:ill-natured:00" "surly%5:00:00:ill-natured:00" "vinegarish%5:00:00:ill-natured:00")
)

;; amiable, euqable, placid
(define-type ont::positive-disposition-val
 :parent ont::disposition-val ;animal-disposition-val
 :wordnet-sense-keys ("good-natured%3:00:00") ;"amiable%5:00:00:good-natured:00" "even-tempered%5:00:00:good-natured:00" "good-natured%3:00:00")
)

;; (dis)honest
(define-type ont::honesty-val
 :parent ont::animal-propensity-val 
 :sem (F::abstr-obj (F::scale ont::honesty-scale))
)

(define-type ont::honest-val
 :parent ont::honesty-val
 :wordnet-sense-keys ("truthful%3:00:00::" "true%3:00:04::" "honest%3:00:00")
 :sem (F::abstr-obj (F::scale ont::honesty-scale) (f::orientation f::pos))
)

(define-type ont::not-honest-val
 :parent ont::honesty-val
 :wordnet-sense-keys ("pretentious%3:00:00::" "untruthful%3:00:00::" "insincere%3:00:00::" "misleading%5:00:00:dishonest:00" "dishonest%3:00:00")
 :sem (F::abstr-obj (F::scale ont::honesty-scale) (f::orientation f::neg))
)

(define-type ont::sneaky-val
 :parent ont::not-honest-val
 :wordnet-sense-keys ("furtive%5:00:00:concealed:00")
)

;; bold, timid
(define-type ont::boldness-val
 :parent ont::animal-propensity-val
 :sem (F::abstr-obj (F::scale ont::boldness-scale)) 
 :comment "foreceful approach to situations or challenge"
)

(define-type ont::bold-val
 :parent ont::boldness-val 
 :wordnet-sense-keys ("forward%3:00:02::" "gutsy%3:00:00::" "plucky%3:00:00::" "confident%3:00:00::" "bold%3:00:00" "adventurous%3:00:00" "peremptory%5:00:02:imperative:00")
 :sem (F::abstr-obj (F::scale ont::boldness-scale) (f::orientation f::pos))
)

(define-type ont::aggressive-val
 :parent ont::boldness-val 
 :wordnet-sense-keys ("domineering%3:00:00::" "assertive%3:00:00::" "self-asserting%3:00:00::" "self-assertive%3:00:00::" "aggressive%3:00:00" "violent%3:00:00" "strident%5:00:00:imperative:00")
 :sem (F::abstr-obj (F::scale ont::aggressiveness-scale) (f::orientation f::pos))
)

(define-type ont::not-bold-val
 :parent ont::boldness-val 
 :wordnet-sense-keys ("unassertive%3:00:00::" "unadventurous%3:00:00::" "unenterprising%3:00:00::" "nonenterprising%3:00:00::" "backward%3:00:02::" "timid%3:00:00" "unaggressive%3:00:00")
 :sem (F::abstr-obj (F::scale ont::aggressiveness-scale) (f::orientation f::neg))
)

(define-type ont::docile-val
 :parent ont::not-bold-val 
 :wordnet-sense-keys ("tame%3:00:01::" "tamed%3:00:04::" "tractable%3:00:00::" "manipulable%3:00:00::" "meek%5:00:00:docile:00" "docile%3:00:00")
)

;; courageous, cowardly
(define-type ont::courage-val
 :parent ont::animal-propensity-val
 :sem (F::abstr-obj (F::scale ont::courage-scale))
 :comment "strength in face of fear or tribulation"
)

(define-type ont::courageous-val
 :parent ont::courage-val
 :wordnet-sense-keys ("courageous%3:00:04")
 :sem (F::abstr-obj (F::scale ont::courage-scale) (f::orientation f::pos))
)

(define-type ont::cowardly-val
 :parent ont::courage-val
 :wordnet-sense-keys ("cowardly%3:00:00")
 :sem (F::abstr-obj (F::scale ont::courage-scale) (f::orientation f::neg))
)

;; modest, arrogant
(define-type ont::modesty-val
 :parent ont::animal-propensity-val 
 :sem (F::abstr-obj (F::scale ont::modesty-scale))
)

(define-type ont::modest-val
 :parent ont::modesty-val 
 :wordnet-sense-keys ("humble%3:00:00::" "modest%3:00:01::" "modest%3:00:02" "unassuming%5:00:00:modest:02" )
 :sem (F::abstr-obj (F::scale ont::modesty-scale) (f::orientation f::pos))
)

(define-type ont::arrogant-val
 :parent ont::modesty-val 
 :wordnet-sense-keys ("arrogant%5:00:00:proud:00" "immodest%3:00:02" )
 :sem (F::abstr-obj (F::scale ont::modesty-scale) (f::orientation f::pos))
)

;; friendly, affectionate, kind, mean, considerate
;; no experiencer role; currently no distinction between human and non-human ont::of
(define-type ont::social-interaction-val
 :parent ont::animal-propensity-val 
 :arguments ((:required ont::FIGURE ((? lof f::abstr-obj f::phys-obj f::situation )))) 
 :sem (F::abstr-obj (F::scale ont::sociability-scale))
 :comment "sociability"
; :comment "properties of human behavior having to do with social interaction, e.g. friendly, kind, mean.)"
)

;; social
(define-type ont::social-val
 :parent ont::social-interaction-val
 :wordnet-sense-keys ("social%3:01:00" "social%3:00:00" "gregarious%3:00:00" "sociable%3:00:00")
 :sem (F::abstr-obj (F::scale ont::sociability-scale) (f::orientation f::pos))
)

(define-type ont::not-social-val
 :parent ont::social-interaction-val
 :wordnet-sense-keys ("unapproachable%3:00:00::" "cool%3:00:02::" "ungregarious%3:00:00" "unsociable%3:00:00" "unsocial%3:00:00")
 :sem (F::abstr-obj (F::scale ont::sociability-scale) (f::orientation f::neg))
)

;; (keep) in touch
(define-type ont::in-touch-val
 :parent ont::social-val
)

;; responsible, irresponsible
(define-type ont::responsibility-val
 :parent ont::animal-propensity-val 
 :arguments ((:required ont::FIGURE ((? lof f::abstr-obj f::phys-obj f::situation )))(:optional ont::GROUND )) 
 :sem (F::abstr-obj (F::scale ont::responsibility-scale))
)

(define-type ont::responsible-val
 :parent ont::responsibility-val 
 :wordnet-sense-keys ("responsible%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::responsibility-scale) (f::orientation f::pos))
)

(define-type ont::not-responsible-val
 :parent ont::responsibility-val 
 :wordnet-sense-keys ("immature%3:00:02::" "irresponsible%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::responsibility-scale) (f::orientation f::neg))
)

;; confused, surprised, happy
(define-type ont::psychological-property-val
    :parent ont::property-val
  :wordnet-sense-keys ("extroversive%3:00:00::" "extraversive%3:00:00::" "maladjusted%3:00:00::" "abnormal%3:00:03::" "introversive%3:00:00::" "introvertive%3:00:00::" "adjusted%3:00:02::" "psychosexual%3:01:00::")
    :arguments ((:REQUIRED ONT::FIGURE (F::phys-obj (f::intentional +) (f::origin (? org f::human f::non-human-animal))))) 
    :comment "properties pertaining to psychological, mental or emotional states"
    ;;(:optional ont::stimulus ((? stm f::phys-obj f::situation f::abstr-obj)))
    ;; the object that is involved in a situation, but which is not a stimulus directly
    ;; for example, I am afraid for her, for the project
    ;;(:optional ont::formal (f::situation f::phys-obj f::abstr-obj)))
    :sem (F::abstr-obj (F::scale ont::psychological-condition-scale) )
)

;; willing, unwilling
(define-type ont::willingness-val
 :parent ont::psychological-property-val
 :sem (F::abstr-obj (F::scale ont::willingness-scale))
)

;; unwilling
(define-type ont::unwilling
 :parent ont::willingness-val 
 :wordnet-sense-keys ("stubborn%3:00:00::" "obstinate%3:00:00::" "unregenerate%3:00:01::" "disinclined%3:00:00::" "uncooperative%3:00:00::" "unwilling%3:00:00" "unwilling%5:00:00:involuntary:01" "involuntary%3:00:01")
 :arguments ((:required ONT::FIGURE ((? lof f::phys-obj f::abstr-obj )(f::intentional + )))) 
 ; Words: (W::unwilling w::disinclined w::involuntary)
 ; Antonym: ONT::inclined (W::willing w::inclined w::voluntary)
;	      (:optional ont::action (f::situation))
 :sem (F::abstr-obj (F::scale ont::willingness-scale) (f::orientation f::neg))
)

;; willing
(define-type ont::willing
 :parent ont::willingness-val 
 :wordnet-sense-keys ("voluntary%3:00:01::" "inclined%3:00:02::" "compliant%3:00:00::" "compromising%3:00:00::" "conciliatory%3:00:04::" "flexible%3:00:04::" "willing%3:00:00" )
 :arguments ((:required ONT::FIGURE ((? lof f::phys-obj f::abstr-obj )(f::intentional + )))) 
 ; Words: (W::willing w::inclined w::voluntary)
 ; Antonym: ONT::disinclined (W::unwilling w::disinclined w::involuntary)
;	      (:optional ont::action (f::situation))
 :sem (F::abstr-obj (F::scale ont::willingness-scale) (f::orientation f::pos))
)

;; (un)friendly, affectionate
(define-type ont::affection-val
 :parent ont::social-interaction-val
 :sem (F::abstr-obj (F::scale ont::affection-scale))
)

(define-type ont::affectionate-val
 :parent ont::affection-val 
 :wordnet-sense-keys ("tender%3:00:03::" "soft%3:00:02::" "softhearted%3:00:00::" "soft-boiled%3:00:00::" "warmhearted%3:00:00::" "amicable%3:00:00" "warm%3:00:02" "friendly%3:00:01" "loving%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::affection-scale) (f::orientation f::pos))
)

(define-type ont::not-affectionate-val
 :parent ont::affection-val 
 :wordnet-sense-keys ("uncompassionate%3:00:00::" "hardhearted%3:00:00::" "heartless%3:00:02::" "coldhearted%3:00:00::" "cold%3:00:02::" "unfriendly%3:00:01" "hostile%3:00:01" "hostile%5:00:00:irreconcilable:00")
 :sem (F::abstr-obj (F::scale ont::affection-scale) (f::orientation f::neg))
)

;; tame, wild
(define-type ont::tameness-val
 :parent ont::animal-propensity-val 
 :sem (F::abstr-obj (F::scale ont::tameness-scale))
)

(define-type ont::wild-val
 :parent ont::tameness-val 
 :wordnet-sense-keys ("disorderly%3:00:00::" "wild%3:00:01::" "untamed%3:00:04::" "wild%3:00:02" )
 :sem (F::abstr-obj (F::scale ont::wild-scale))
)

(define-type ont::tame-val
 :parent ont::tameness-val 
 :wordnet-sense-keys ("tame%3:00:02" )
 :sem (F::abstr-obj (F::scale ont::tame-scale))
)

;; graceful, clumsy
(define-type ont::gracefulness-val
 :parent ont::animal-propensity-val 
 :sem (F::abstr-obj (F::scale ont::gracefulness-scale))
)

(define-type ont::awkward-val
 :parent ont::gracefulness-val 
 :wordnet-sense-keys ("awkward%3:00:00" "clumsy%5:00:00:awkward:00" "ungraceful%5:00:00:awkward:00" "graceless%5:00:00:awkward:00" )
 :sem (F::abstr-obj (F::scale ont::gracefulness-scale) (f::orientation f::neg))
)

(define-type ont::graceful-val
 :parent ont::gracefulness-val 
 :wordnet-sense-keys ("graceful%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::gracefulness-scale) (f::orientation f::pos))
)

;; wise vs. foolish
(define-type ont::wiseness-val
 :parent ont::animal-propensity-val
 :sem (F::abstr-obj (F::scale ont::wiseness-scale))
)

(define-type ont::wise-val
 :parent ont::wiseness-val 
 :wordnet-sense-keys ("wise%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::wisdom-scale))
)

(define-type ont::foolish-val
 :parent ont::wiseness-val 
 :wordnet-sense-keys ("ridiculous%5:00:00:foolish:00" "absurd%5:00:00:foolish:00" "foolish%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::folly-scale))
)

(define-type ont::silly-val
 :parent ont::foolish-val 
 :wordnet-sense-keys ("goofy%5:00:00:foolish:00" )
)

(define-type ONT::skillfulness-val
  :parent ONT::animal-propensity-val
  :sem (F::abstr-obj (F::scale ont::skillfulness-scale))
  )

(define-type ont::skillful-val
 :parent ONT::skillfulness-val
 :sem (F::abstr-obj (F::scale ont::skillfulness-scale))
 :wordnet-sense-keys ("skilled%3:00:00")
)

;; dimensional-property
(define-type ont::dimensional-property-val
    :parent ont::property-val
    :sem (F::abstr-obj (F::scale ont::dimensional-scale)) ;measure-scale))
    :comment "properties pertaining to dimensions and measurable extents"
    )

;;; This is for speed values - fast, slow, etc
(define-type ont::speed-val
 :parent ont::dimensional-property-val ;process-evaluation-val ;process-val 
 :arguments ((:REQUIRED ONT::FIGURE ((? type F::phys-obj F::situation F::abstr-obj )))) ;; e.g., "rate" is an abstract object
 :sem (F::abstr-obj (F::scale ont::speed-scale ))
)

(define-type ont::slow-val
 :parent ont::speed-val 
 :wordnet-sense-keys ("slow%3:00:01" "gradually%4:02:00")
 )

(define-type ont::instantaneous-val
 :parent ont::speed-val 
 :wordnet-sense-keys ("instantaneous%5:00:00:fast:01" "instant%5:00:00:fast:01" "sudden%3:00:00" )
)

(define-type ont::speedy
 :parent ont::speed-val 
 :wordnet-sense-keys ("quick%5:00:02:fast:01" "fleet%5:00:00:fast:01" "rapid%5:00:00:fast:01" "rapid%5:00:02:fast:01" "fast%3:00:01" "quick%5:00:00:fast:01"
					      "hurried%3:00:00" "hastily%4:02:00")
 ; Words: (W::QUICK W::FAST W::RAPID W::SWIFT W::SPEEDY)
 ; Antonym: NIL (W::SLOW)
)

(define-type ont::physical-strength-val
 :parent ont::dimensional-property-val
  :sem (F::abstr-obj (F::scale ont::physical-strength-scale))
)

;; strong, hardy, robust
(define-type ont::strong-val
 :parent ont::physical-strength-val
  :wordnet-sense-keys ("robust%3:00:00::" "tough%3:00:02::" "toughened%3:00:02::" "rugged%3:00:00::")
 :wordnet-sense-keys("strong%3:00:00")
 :sem (F::abstr-obj (F::scale ont::strength-scale))
)

(define-type ont::weak-in-strength-val
 :parent ont::physical-strength-val
  :wordnet-sense-keys ("tender%3:00:02::" "untoughened%3:00:02::" "delicate%3:00:00::")
 :wordnet-sense-keys("weak%3:00:00")
 :sem (F::abstr-obj (F::scale ont::weakness-scale))
)

(define-type ont::position-on-dimension-scale-val
    :parent ont::dimensional-property-val
    :wordnet-sense-keys ("scale_value%1:09:00")
    :sem (F::abstr-obj (F::scale ont::dimensional-scale))
    :comment "indicates a position given a dimensional scale. These adjectives do not specify the shape, direction, or alignment of the scale."
)

(define-type ont::high-val
    :parent ont::position-on-dimension-scale-val
    :wordnet-sense-keys ("high%3:00:02" "high%3:00:01" )
    )

(define-type ont::low-val
    :parent ont::position-on-dimension-scale-val
    :wordnet-sense-keys ("low%3:00:01" "low%3:00:02" )
    )

(define-type ont::medium-val
 :parent ont::position-on-dimension-scale-val 
 :wordnet-sense-keys ("average%1:09:01" "average%5:00:00:moderate:00" "medium%5:00:00:moderate:00" )
)

;;; big/large/small
(define-type ont::size-val
 :parent ont::dimensional-property-val 
  :wordnet-sense-keys ("sized%3:00:00::")
 :sem (F::abstr-obj (F::scale ont::size-scale ))
 :comment "indicates relative extent or magnitude of something on a size scale"
)

;; size on a linear scale
(define-type ont::linear-extent-val
     :sem (F::abstr-obj (F::scale ont::linear-extent-scale ))
     :parent ont::size-val 
     :comment "size on a linear scale"
)

;; size specific to non-vertical linear scale
(define-type ont::non-vertical-val
 :parent ont::linear-extent-val 
 :comment "size specific to non-vertical linear scale"
 )

(define-type ont::narrow-val
    :sem (F::abstr-obj (F::scale ONT::width-scale))
    :parent ont::non-vertical-val 
    :wordnet-sense-keys ("narrow%3:00:00" )
)

(define-type ont::broad
    :sem (F::abstr-obj (F::scale ONT::width-scale))
    :parent ont::non-vertical-val 
    :wordnet-sense-keys ("broad%3:00:04" "wide%3:00:00" )
    )

(define-type ont::thick-val
    :sem (F::abstr-obj (F::scale ont::thickness-scale ))
    :parent ont::non-vertical-val 
    :wordnet-sense-keys ("thick%3:00:01" )
    )

(define-type ont::thin-val
    :sem (F::abstr-obj (F::scale ont::thinness-scale ))
    :parent ont::non-vertical-val 
    :wordnet-sense-keys ("thin%3:00:01" "flat%5:00:00:thin:01" )
    )

;; size specific to vertical linear scale
(define-type ont::vertical-val
    :sem (F::abstr-obj (F::scale ont::vertical-scale ))
    :parent ont::linear-extent-val 
    :comment "size specific to vertical linear scale"
)

(define-type ont::tall-val
    :sem (F::abstr-obj (F::scale ont::height-scale ))
    :parent ont::vertical-val 
    :wordnet-sense-keys ("high-rise%3:00:00::" "tall%3:00:00" )
)

(define-type ont::deep-val
    :sem (F::abstr-obj (F::scale ont::depth-scale ))
    :parent ont::vertical-val 
    :wordnet-sense-keys ("deep%3:00:01" )
)

(define-type ont::shallow-val
    :sem (F::abstr-obj (F::scale ont::depth-scale ))
    :parent ont::vertical-val 
    :wordnet-sense-keys ("shallow%3:00:01" )
    )

;; short on a linear scale; does not indicate horizontality or verticality of the object
(define-type ont::short-val
    :sem (F::abstr-obj (F::scale ont::height-scale ))
    :parent ont::linear-extent-val 
    :wordnet-sense-keys ("low-rise%3:00:00::" "short%3:00:03::" "little%3:00:00::" "short%3:00:02" "short%3:00:01" )
    :comment "less in orientation on a linear scale -- does not indicate horizontality or verticality of the object"
    )

;; long on a linear scale; does not indicate horizontality or verticality of the object
(define-type ont::long
    :sem (F::abstr-obj (F::scale ont::length-scale ))
    :parent ont::linear-extent-val 
    :wordnet-sense-keys ("long%3:00:01" "long%3:00:02" )
    :comment "more in orientation on a linear scale -- does not indicate horizontality or verticality of the object"
    )

;; size that deals with 2D area
(define-type ont::two-dimensional-val
 :parent ont::size-val 
 :wordnet-sense-keys ("commodious%3:00:00" "roomy%5:00:00:commodious:00")
 :comment "size the deals with area(2D)"
)

(define-type ont::relative-to-height-val
 :parent ont::size-val 
 :comment "size specification relative to height. E.g. fat means more in  horizontal orientation with respect to the height"
)

(define-type ont::fat-val
    :sem (F::abstr-obj (F::scale ont::fat-scale ))
    :parent ont::relative-to-height-val 
    :wordnet-sense-keys ("plump%5:00:00:fat:01" "fat%3:00:01" )
    :comment "more in  horizontal orientation with respect to the height"
)

(define-type ont::skinny-val
    :sem (F::abstr-obj (F::scale ont::skinny-scale ))
    :parent ont::relative-to-height-val 
    :wordnet-sense-keys ("slim%5:00:00:thin:03" "slender%5:00:00:thin:03" "thin%3:00:03")
    :comment "less in horizontal orientation with respect to the height"
    )

;; large
(define-type ont::large
 :parent ont::size-val 
 :wordnet-sense-keys ("wide%3:00:02::" "broad%5:00:00:large:00" "large%3:00:00")
 ; Words: (W::LARGE W::BIG W::HUGE W::BROAD W::DOUBLE W::VAST W::MASSIVE W::ENORMOUS W::EXTENSIVE W::GIANT W::EXTENDED W::UNLIMITED W::SPACIOUS W::WHOPPING)
 ; Antonym: ONT::small (W::SMALL W::LITTLE W::LIMITED W::TINY W::TEENY)
 :comment "more in orientation on a size scale"
)

(define-type ont::substantial-val
  :parent ont::large
  :wordnet-sense-keys ("insubstantial%3:00:00::" "unsubstantial%3:00:00::" "unreal%3:00:03::" "substantial%5:00:00:considerable:00" "considerable%3:00:00" "extensive%5:00:00:large:00")
 )


(define-type ont::huge-val
  :parent ont::large
  :wordnet-sense-keys ("massive%5:00:00:large:00" "enormous%5:00:00:large:00" "humongous%5:00:00:large:00" "elephantine%5:00:00:large:00" "huge%5:00:01:large:00")  
 )

;; small
(define-type ont::small
 :parent ont::size-val 
 :wordnet-sense-keys ("narrow%3:00:02::" "small%3:00:00")
 ; Words: (W::SMALL W::LITTLE W::LIMITED W::TINY W::TEENY)
 ; Antonym: ONT::large (W::LARGE W::BIG W::HUGE W::BROAD W::DOUBLE W::VAST W::MASSIVE W::ENORMOUS W::EXTENSIVE W::GIANT W::EXTENDED W::UNLIMITED W::SPACIOUS W::WHOPPING)
 :comment "negative orientation on a size scale"
)

;; teeny, tiny
(define-type ont::tiny
  :parent ont::small
  :wordnet-sense-keys ("tiny%5:00:00:small:00" "teeny%5:00:00:small:00" "bitty%5:00:00:small:00")
  )

;; little; removing. overlaps with ont::small
;(define-type ont::little
;  :parent ont::small
;  :wordnet-sense-keys ("little%3:00:01")
;  )

(define-type ont::boundedness-val
 :parent ont::dimensional-property-val
 :comment "indicates a restriction in regards to size, amount, or extent"
)

;; limited
(define-type ont::limited-val
  :parent ont::boundedness-val
  :wordnet-sense-keys ("limited%3:00:00" "minor%5:00:00:limited:00" "finite%3:00:00")
  )

(define-type ont::not-limited-val
  :parent ont::boundedness-val
  :wordnet-sense-keys ("unlimited%3:00:00" "infinite%3:00:00")
 )

;; heavy
(define-type ont::weight-val
 :parent ont::dimensional-property-val 
 :sem (F::abstr-obj (F::scale ont::weight-scale ))
 :comment "indicates relative extent or magnitude of something on a weight scale"
)

(define-type ont::heavy
 :parent ont::weight-val 
 :wordnet-sense-keys ("weighty%3:00:00::" "heavy%3:00:01")
 :sem (F::abstr-obj (F::scale ont::weight-scale ))
 :comment "indicates more in orientation on a weight scale"
)

(define-type ont::overweight
 :parent ont::heavy 
 :wordnet-sense-keys ("overweight%5:00:00:fat:01" )
)

(define-type ont::lightweight
 :parent ont::weight-val 
 :wordnet-sense-keys ("light%3:00:01" "lightweight%5:00:00:light:01" )
 ; Words: (W::LIGHT W::LIGHTWEIGHT)
 ; Antonym: NIL (W::HEAVY)
 :comment "indicates less in orientation on a weight scale"
)

(define-type ont::underweight
 :parent ont::lightweight 
 :wordnet-sense-keys ("underweight%5:00:00:thin:03" )
)

;; intense, powerful, weak
(define-type ont::intensity-val
 :parent ont::dimensional-property-val 
 :comment "indicates relative extent or magnitude of something on an intensity scale"
 :sem (F::abstr-obj (F::scale ont::intensity-scale))
)

(define-type ont::intense
 :parent ont::intensity-val 
 :wordnet-sense-keys ("full%3:00:01::" "profound%3:00:00::" "heavy%3:00:08::" "deep%3:00:02::" "potent%3:00:00::" "strong%3:00:04::" "stiff%3:00:00::" "potent%5:00:00:powerful:00" "acute%5:00:00:sharp:04" "strong%5:00:00:powerful:00" "intense%5:00:00:sharp:04" "shrill%5:00:00:high:03" "powerful%3:00:00" "strong%5:00:00:intense:00" "sharp%3:00:04" "intense%3:00:00" "high%3:00:03" "deep%5:00:00:intense:00" "intensive%5:00:00:intense:00" "forceful%3:00:00"
						   "hearty%3:00:00" "heartily%4:02:03" "heartily%4:02:01" )
 ; Words: (W::HIGH W::LOW W::STRONG W::DEEP W::POWERFUL W::SHARP W::INTENSE W::DULL W::SHALLOW W::POTENT)
 ; Antonym: ONT::weak (W::WEAK W::FAINT)
)

(define-type ont::weak
 :parent ont::intensity-val 
 :wordnet-sense-keys ("forceless%3:00:00::" "unforceful%3:00:00::" "light%3:00:08::" "thin%3:00:04::" "powerless%3:00:00::" "impotent%3:00:00::" "shallow%3:00:02" "dull%3:00:04" "weak%5:00:00:perceptible:00" "weak%5:00:00:diluted:00")
 ; Words: (W::WEAK W::FAINT)
 ; Antonym: ONT::intense (W::HIGH W::LOW W::STRONG W::DEEP W::POWERFUL W::SHARP W::INTENSE W::DULL W::SHALLOW W::POTENT)
)

;;; the severity of the problem/situation/crisis/emergency
;; severe, mild
(define-type ont::severity-val
    :parent ont::dimensional-property-val 
    :arguments ((:required ont::FIGURE ((? of f::situation f::abstr-obj )))) ;; adding restriction to prevent "acute stomach"
    :comment "indicates relative extent or magnitude of something on a severity scale"
    :sem (F::abstr-obj (F::scale ont::severity-scale))
    )

(define-type ont::severe-val
 :parent ont::severity-val 
 :wordnet-sense-keys ("steep%3:00:00::" "intemperate%3:00:00::" "heavy%3:00:03::" "inclement%3:00:01::" "severe%5:00:01:bad:00" "severe%5:00:00:intense:00" "extreme%5:00:00:intense:00" "drastic%5:00:00:forceful:00")
)

(define-type ont::mild-val
 :parent ont::severity-val 
 :wordnet-sense-keys ("mild%3:00:00" "slight%3:00:00" )
)

(define-type ont::moderate-val
 :parent ont::severity-val 
 :wordnet-sense-keys ("gradual%3:00:02::" "gradual%3:00:01::" "moderate%3:00:00")
)

;; 
(define-type ont::proportion-val
 :parent ont::dimensional-property-val 
 :comment "indicates relative proportion/ratio on a specified scale"
)

(define-type ont::fattiness-val
 :parent ont::proportion-val 
 :comment "indicates relative proportion/ratio on a fattiness scale"
)

(define-type ont::lean-val
 :parent ont::fattiness-val 
)

(define-type ont::fatty-val
 :parent ont::fattiness-val 
 :wordnet-sense-keys ("fatty%3:00:00" "fat%3:00:02" )
)

(define-type ont::predefined-measure-val
 :parent ont::dimensional-property-val 
 :comment "standardized size/quantity given a predefined measure"
 :sem (F::abstr-obj (F::scale ont::predefined-measure-scale) )
)

;; queen, king e.g. bed size
(define-type ont::predefined-size-val
 :parent ont::predefined-measure-val 
 :wordnet-sense-keys("double%5:00:00:large:00")
)

;; for foodkb
;; lean, nonfat, lowfat
(define-type ont::fat-content
 :parent ont::predefined-measure-val 
 :wordnet-sense-keys ("nonfat%3:00:00" "skim%5:00:00:nonfat:00")
)

;; relational-attribute: describes an entity with respect to another related entity
(define-type ont::relational-attribute-val
 :parent ont::property-val 
 :comment "properties that describe an entity with respect to another related entity, an implicit second entity always present when these properties are evoked"
  :sem (F::abstr-obj (F::scale ont::relational-property-scale))
)

;; similarity
(define-type ONT::SIMILARITY-val
 :parent ONT::relational-attribute-val
 :arguments ((:ESSENTIAL ONT::neutral)
             (:ESSENTIAL ONT::neutral1)
             (:ESSENTIAL ONT::FIGURE)
             (:optional ont::formal)
             (:optional ont::GROUND) ;; for backwards compat
)
  :sem (F::abstr-obj (F::scale ont::similarity-scale))
)

;; such as, as in. These should have the same representation as produced by the grammar rule such-X-as-Y>
;; using roles :of and :val b.c. the :formal :formal1 roles are going to be replaced real soon now   

(define-type ont::exemplifies
  :parent ont::similarity-val
    )

(define-type ONT::IDENTITY-val
 :parent ONT::similarity-val
  )

(define-type ONT::SAME
 :parent ONT::IDENTITY-VAL
 :wordnet-sense-keys ("identical%5:00:00:same:02" "same%3:00:00" "same%3:00:02")
 ; Antonym: NIL (W::OTHER) 
  :sem (F::abstr-obj (F::scale ont::same-scale))
)

(define-type ONT::other
 :parent ONT::IDENTITY-VAL
 :wordnet-sense-keys ("other%3:00:00" "another%3:00:00")
 )

(define-type ONT::SIMILAR
 :wordnet-sense-keys ("synonymous%3:00:00::" "similar%3:00:00::" "like%5:00:00:same:00" "comparable%5:00:00:same:00" "like%3:00:04" "similar%3:00:04" "alike%3:00:00" "same%3:00:04" "like%3:00:02" "like%3:00:00" "similar%3:00:02" "corresponding%5:00:00")
 :wordnet-sense-keys ("like%5:00:00:same:00" "comparable%5:00:00:same:00" "like%3:00:04" "similar%3:00:04" "alike%3:00:00" "same%3:00:04" "like%3:00:02" "like%3:00:00" "similar%3:00:02" "corresponding%5:00:00")
 :parent ONT::SIMILARITY-VAL
 ; Words: (W::SIMILAR W::LIKE W::ANALOGOUS W::KINDRED)
 :wordnet-sense-keys ("like%3:00:00" "like%3:00:00" "analogous%5:00:00:similar:00" "similar%3:00:00" "akin%5:00:00")
 ; Antonym: ONT::DIFFERENT (W::DIFFERENT W::SEPARATE W::DISTINCT)
  :sem (F::abstr-obj (F::scale ont::similar-scale))
 )

(define-type ONT::DIFFERENT
 :parent ONT::SIMILARITY-VAL
 ; Words: (W::DIFFERENT W::SEPARATE W::DISTINCT)
 :wordnet-sense-keys ("inequitable%3:00:00::" "unjust%3:00:02::" "unlike%3:00:02::" "unequal%3:00:00::" "unlike%3:00:00" "unlike%3:00:00" "discrete%5:00:00:separate:00" "different%3:00:00" "separate%3:00:00" "distinct%5:00:00")
 ; Antonym: ONT::SIMILAR (W::SIMILAR W::LIKE W::ANALOGOUS W::KINDRED) 
  :sem (F::abstr-obj (F::scale ont::different-scale))
)

(define-type ONT::EQUAL
 :parent ONT::SIMILARITY-VAL
 ; Words: (W::EQUAL W::EQUIVALENT)
 :wordnet-sense-keys ("equal%3:00:00" "equivalent%5:00:00:equal:00" "even%3:00:01" "even%3:00:01:equal:00" "even%5:00:01:equal:00" "even%5:00:02:equal:00")
 ; Antonym: NIL (W::UNEQUAL) 
  :sem (F::abstr-obj (F::scale ont::equal-scale))
 )



;; relative, absolute
(define-type ont::comparative-val
 :parent ont::relational-attribute-val 
)

(define-type ont::relative
 :parent ont::comparative-val 
 :wordnet-sense-keys ("relative%3:00:00" "comparative%3:00:00" )
 ; Words: (W::RELATIVE W::COMPARATIVE)
 ; Antonym: NIL (W::ABSOLUTE)
)

(define-type ont::not-relative-val
 :parent ont::comparative-val 
 :wordnet-sense-keys ("absolute%3:00:00" )
)

;; dependent, independent
(define-type ont::dependence-val
 :parent ont::relational-attribute-val 
 :arguments ((:optional ONT::GROUND ((? lof f::phys-obj f::abstr-obj )))) ;;  independent/dependent of X
)

(define-type ont::dependent
 :parent ont::dependence-val 
 :wordnet-sense-keys ("dependent%3:00:00" "dependent%5:00:00:conditional:00" "conditional%3:00:00" )
 ; Words: (W::DEPENDENT W::CONDITIONAL)
 ; Antonym: NILx (W::INDEPENDENT)
)

(define-type ont::independent
 :parent ont::dependence-val 
 :wordnet-sense-keys ("independent%3:00:00" )
)

;; (in)compatible
(define-type ont::compatibility-val
 :parent ont::relational-attribute-val 
 :sem (F::abstr-obj (F::scale ont::compatibility-scale))
)

(define-type ont::compatible-val
 :parent ont::compatibility-val 
 :wordnet-sense-keys ("compatible%3:00:02" "compatible%3:00:01" )
)

(define-type ont::not-compatible-val
 :parent ont::compatibility-val 
 :wordnet-sense-keys ("mismatched%3:00:00::" "incompatible%3:00:01" "incompatible%3:00:04")
)

;; consecutive, sequential, groups of ordered items
;; didn't use ordered-domain here because these words describe the ordered
;; nature of the objects, but not the domain itself
(define-type ont::ordered-val
 :parent ont::relational-attribute-val 
 :comment "properties that deal with ordered nature of objects"
 :wordnet-sense-keys ("ordered%3:00:00")
)

(define-type ont::sequential-val
 :parent ont::ordered-val 
 :wordnet-sense-keys ("serial%3:01:00::" "sequential%5:00:00:ordered:00" "consecutive%5:00:00:ordered:00" "sequent%5:00:00:ordered:00" "successive%5:00:00:ordered:00" "serial%5:00:00:ordered:00" "progressive%5:00:00:ordered:00")
)

(define-type ont::random-val
 :parent ont::ordered-val 
 :wordnet-sense-keys ("arbitrary%3:00:00::" "random%3:00:00" "randomized%5:00:00:irregular:00")
)

;; next, previous, last, penultimate, etc.
(define-type ont::sequence-val
 :parent ont::relational-attribute-val 
 :arguments ((:optional ONT::GROUND )) 
 :sem (F::Abstr-obj (F::gradability - ))
 :comment "properties that deal with an object's location with respect to another object in an ordered sequence"
)

(define-type ont::previous-val
 :parent ont::sequence-val 
 :wordnet-sense-keys ("antecedent%3:00:00::" "previous%5:00:00:past:00" "previous%5:00:00:preceding:00" "former%5:00:02:past:00" "late%5:00:02:past:00" "preceding%3:00:00")
)

(define-type ont::one-before-last-val
 :parent ont::sequence-val 
 :wordnet-sense-keys ("next-to-last%5:00:01:intermediate:00" "penultimate%5:00:00:intermediate:00" )
)

(define-type ont::first-val
 :parent ont::sequence-val 
 :wordnet-sense-keys ("opening%3:00:00::" "original%5:00:01:first:00" "first%3:00:00" "initial%5:00:00:first:00" )
)

(define-type ont::last-val
 :parent ont::sequence-val 
 :wordnet-sense-keys ("closing%3:00:00::" "conclusive%3:00:00::" "ultimate%5:00:00:last:00" "latter%3:00:00" "final%5:00:00:ultimate:00" "last%3:00:00" )
)

(define-type ont::sequence-val-next
 :parent ont::sequence-val 
 :wordnet-sense-keys ("following%3:00:00::" "following%5:00:02:succeeding:00" "subsequent%3:00:00" "succeeding%3:00:00")
)

(define-type ont::middle-val
 :parent ont::sequence-val 
 :wordnet-sense-keys ("halfway%5:00:00:intermediate:00" "intermediate%3:00:00" )
)

;; old, young
(define-type ont::age-val
 :parent ont::temporal-val 
 :sem (F::abstr-obj (F::scale ont::age-scale ))
;  :sem (F::abstr-obj (F::scale ont::duration-scale))
 :comment "age given the length or duration of existence"
)

(define-type ont::old-val
 :parent ont::age-val 
 :wordnet-sense-keys ("geriatric%3:01:01::" "old%3:00:02" "old%3:00:01")
)

(define-type ont::young-val
 :parent ont::age-val 
 :wordnet-sense-keys ("nascent%3:00:00::" "young%3:00:00" "immature%3:00:03" "new%3:00:09")
)

(define-type ont::historical-era-val
 :parent ont::temporal-val 
  :wordnet-sense-keys ("medieval%3:01:00::" "mediaeval%3:01:00::" "millenarian%3:01:00::" "chiliastic%3:01:00::" "lenten%3:01:00::" "anaphasic%3:01:00::" "mesolithic%3:01:00::" "elizabethan%3:01:00::" "juvenile%3:01:00::" "terminal%3:01:00::" "matutinal%3:01:00::" "baroque%3:01:00::" "sabbatical%3:01:00::" "neolithic%3:01:00::" "victorian%3:01:00::" "paleolithic%3:01:00::" "palaeolithic%3:01:00::" "adolescent%3:01:00::" "canicular%3:01:02::" "pubertal%3:01:00::" "centennial%3:01:00::" "centenary%3:01:00::" "nocturnal%3:01:01::" "bimillenial%3:01:00::" "millennial%3:01:00::" "millennian%3:01:00::" "quarterly%3:01:00::" "eolithic%3:01:00::" "pre-christian%3:01:00::" "quartan%3:01:00::" "infantile%3:01:00::")
 :comment "relating to the distinct periods in history"
)

(define-type ont::relative-time-location-val
 :parent ont::temporal-val 
)

(define-type ont::ancient-val
 :parent ont::relative-time-location-val
 :wordnet-sense-keys ("nonmodern%3:00:00::" "early%3:00:00::" "classical%3:00:00::" "classic%3:00:00::" "ancient%5:00:00:past:00" "prehistoric%5:00:00:past:00" "prehistoric%3:01:00" )
)

(define-type ont::modern-val
 :parent ont::relative-time-location-val
 :wordnet-sense-keys ("nonclassical%3:00:00::" "modern%3:00:00" "contemporary%5:00:00:modern:00" )
)

(define-type ont::current-val
 :parent ont::relative-time-location-val
 :wordnet-sense-keys ("contemporary%5:00:00:current:00" "current%3:00:00")
)

;; object-affordances: properties pertaining to function of an entity or an object
(define-type ont::object-affordances-val
 :parent ont::property-val 
 :comment "properties pertaining to the function of an entity or an object"
 :sem (F::abstr-obj (F::scale ont::object-affordances-scale) )
)

(define-type ont::functionality-val
 :parent ont::object-affordances-val 
 :arguments ((:essential ONT::FIGURE (F::PHYS-OBJ (F::ORIGIN F::ARTIFACT) (F::object-function f::provides-service-up-down))))
 :comment "properties relating to  whether something is functioning as intended"
 :sem (F::abstr-obj (F::scale ont::functionality-scale) )
)

(define-type ont::defective-val
 :parent ont::functionality-val 
 :wordnet-sense-keys ("impaired%3:00:00::" "maladaptive%3:00:00::" "imperfect%3:00:00::" "malfunctioning%3:00:00" "defective%5:00:00:malfunctioning:00" )
 :comment "not functioning as intended; malfunctioning e.g., printer is printing only in red"
)

(define-type ont::not-in-working-order-val
 :parent ont::functionality-val 
 :wordnet-sense-keys ("broken%5:00:00:damaged:00" "inoperative%3:00:00")
 :comment "broken/not-operational e.g., not switching on"
)

(define-type ont::in-working-order-val
 :parent ont::functionality-val 
 :wordnet-sense-keys ("go%3:00:00::" "running%5:00:00:functioning:00" "functional%5:00:00:functioning:00" "operative%5:00:00:functioning:00" "working%5:00:00:functioning:00" "functioning%3:00:00" )
 :comment "operational but not necessarily on"
)

;; usable, useless
(define-type ont::usability-val
 :parent ont::object-affordances-val
 :sem (F::abstr-obj (F::scale ont::usability-scale) )
)

(define-type ont::usable-val
 :parent ont::usability-val 
 :wordnet-sense-keys ("usable%5:00:00:serviceable:00" "serviceable%3:00:00")
)

(define-type ont::not-usable-val
 :parent ont::usability-val 
 :wordnet-sense-keys ("unserviceable%3:00:00::" "unusable%5:00:00:useless:00" )
)

;; operating/active as intended?
(define-type ont::activity-val
 :parent ont::process-val 
 :arguments ((:required ONT::FIGURE ((? lof f::phys-obj )
				     ;;(f::type (? !t2 ont::location ))
				     (F::object-function F::provides-service )))) 
; :sem (F::Abstr-obj (F::gradability - ))
 :comment "properties relating to whether something is operating/active as intended"
)

(define-type ont::active
 :parent ont::activity-val 
 :wordnet-sense-keys ("active%3:00:07::" "occupied%3:00:00::" "live%3:00:01::" "active%3:00:05::" "live%3:00:02::" "unrecorded%3:00:04::" "active%3:00:04::" "busy%5:00:01:active:06" "active%3:00:06" "active%3:00:03" "busy%3:00:00" )
 ; Words: (W::ACTIVE W::BUSY)
 :comment "operating and active"
)

(define-type ont::active-open
 :parent ont::active 
 :wordnet-sense-keys ("open%3:00:02" )
 :arguments ((:required ONT::FIGURE (f::phys-obj 
						 (F::object-function F::provides-service-open-closed )))) 
 :comment "operating as intended, typically a physcal location with operating hours"
)

(define-type ont::active-on
 :parent ont::active 
 :wordnet-sense-keys ("on%3:00:00" "on%3:00:02" )
 :arguments ((:required ONT::FIGURE (f::phys-obj (f::type (? !t2 ont::location ))
						 (F::object-function F::provides-service-on-off )))) 
 :comment "operating as intended, typically due to some switching on/off"
)

(define-type ont::inactive
 :parent ont::activity-val 
 :wordnet-sense-keys ("inactive%3:00:03::" "inactive%3:00:07::" "inactive%3:00:02::" "inactive%3:00:01::" "vegetative%3:01:01::" "vegetive%3:01:01::" "idle%3:00:00" "passive%3:00:01" )
 :comment "not operating as intended wrt some process (switch is off)"
 ; Antonym: ONT::active (W::ACTIVE W::BUSY)
)

(define-type ont::inactive-off
 :parent ont::inactive 
 :wordnet-sense-keys ("off%3:00:02" "off%3:00:00" )
 :arguments ((:required ONT::FIGURE (f::phys-obj (f::type (? !t2 ont::location ))
						 (F::object-function F::provides-service-on-off )))) 
 :comment "not operating as intended, typically due to some switching on/off"
)

(define-type ont::inactive-closed
 :parent ont::inactive 
 :wordnet-sense-keys ("closed%3:00:01" "blocked%5:00:00:closed:01" )
 :arguments ((:required ONT::FIGURE (f::phys-obj ;;(f::type (? !t2 ont::location ))
						 (F::object-function F::provides-service-open-closed )))) 
 :comment "not operating as intended,  typically a physcal location with operating hours"
)

;; static, dynamic                                              
(define-type ont::motion-val
 :parent ont::object-affordances-val
 :arguments ((:REQUIRED ONT::FIGURE (F::phys-obj )))
 :comment "describes dynamicity of motion"
)

(define-type ONT::DYNAMIC-MOVING-VAL
 :parent ont::motion-val
 :wordnet-sense-keys ("standing%3:00:02::" "moving%3:00:02::" "running%3:00:02::" "moving%3:00:03::" "dynamic%3:00:00::" "dynamical%3:00:00::" "dynamic%3:00:04")
)

(define-type ONT::STATIC-NONMOVING-VAL
 :parent ont::motion-val
 :wordnet-sense-keys ("immobile%3:00:00::" "undynamic%3:00:00::" "adynamic%3:00:04::" "still%3:00:03::" "static%5:00:00:nonmoving:00" "still%5:00:01:nonmoving:00" "motionless%5:00:00:nonmoving:00" "nonmoving%3:00:00")
)

;; (un)available
(define-type ont::availability-val
 :parent ont::object-affordances-val 
 :arguments ((:optional ONT::GROUND ((? tp F::phys-obj F::situation )))) 
	     ;; available in 4 MW capacity
;	     (:optional ont::property (f::abstr-obj))
 :sem (F::abstr-obj (F::scale ont::availability-scale) )
)

(define-type ont::not-available-val
 :parent ont::availability-val
 :wordnet-sense-keys ("missing%5:00:00:nonexistent:00" "unavailable%3:00:00" )
)

(define-type ont::available
 :parent ont::availability-val 
 :wordnet-sense-keys ("ready%5:00:01:available:00" "free%3:00:02" "available%3:00:00" "free%5:00:02:unoccupied:00" "handy%5:00:00:accessible:00" "available%5:00:00:free:00" "unoccupied%3:00:00")
 ; Words: (W::AVAILABLE W::FREE)
 ; Antonym: NIL (W::UNAVAILABLE)
)


;; status: properties that describe social, political or official status or position
(define-type ont::status-val
 :parent ont::property-val 
 :arguments ((:optional ONT::GROUND )) 
 :comment "properties that describe social, political or official status or position"
 :sem (F::abstr-obj (F::scale ont::unordered-domain))
)

;; reputation
(define-type ont::reputability-val
 :parent ont::status-val
)

(define-type ont::reputable-val
 :parent ont::reputability-val
 :wordnet-sense-keys ("reputable%3:00:00" "respectable%3:00:00")
)

(define-type ont::not-reputable-val
 :parent ont::reputability-val
 :wordnet-sense-keys ("disreputable%3:00:00" "unrespectable%3:00:00")
)

;; classified, unclassified
(define-type ONT::classification-val
 :parent ONT::status-val
 :comment "official designation that allows access to only authorized persons"
 )

(define-type ONT::classified-val
 :parent ONT::classification-val
 :wordnet-sense-keys ("classified%3:00:02")
 )

(define-type ONT::unclassified-val
 :parent ONT::classification-val
 :wordnet-sense-keys ("unclassified%3:00:02")
 )

;; wealthy, poor
(define-type ont::wealthiness-val
 :sem (F::abstr-obj (F::scale ont::wealth-scale))
 :parent ont::evaluation-attribute-val ;status-val 
)

(define-type ont::poor-val
 :parent ont::wealthiness-val 
 :wordnet-sense-keys ("underprivileged%3:00:00::" "poor%3:00:03" "poor%3:00:00" )
)

(define-type ont::wealthy-val
 :parent ont::wealthiness-val 
 :wordnet-sense-keys ("privileged%3:00:00::" "rich%3:00:00" "wealthy%5:00:00:rich:00" )
)

;; private, public
(define-type ont::privacy-val
 :parent ont::status-val 
)

(define-type ont::private
 :parent ont::privacy-val 
 :wordnet-sense-keys ("private%5:00:02:personal:00" "personal%3:00:00" "private%3:00:00" "privy%5:00:00:private:00" "confidential%5:00:00:private:00" "private%5:00:00:personal:00" "unshared%3:00:00" "exclusive%3:00:00")
 ; Words: (W::PERSONAL W::PRIVATE W::SECRET)
 ; Antonym: NIL (W::PUBLIC)
)

(define-type ont::public-val
 :parent ont::privacy-val 
 :wordnet-sense-keys ("publicized%3:00:00::" "publicised%3:00:00::" "common%3:00:02::" "public%3:00:00" )
)

;; confidential, secret
(define-type ont::secrecy-val
 :parent ont::status-val 
)

(define-type ont::secret-val
 :parent ont::secrecy-val 
 :wordnet-sense-keys ("covert%3:00:00::" "suppressed%3:00:00::" "esoteric%3:00:00::" "confidential%5:00:02:private:00" "secret%5:00:00:concealed:00" "private%5:00:00:inward:00" "secret%5:00:00:inward:00" "dark%5:00:00:concealed:00" "incognito%5:00:00:concealed:00" "sealed%5:00:00:concealed:00" "sneaking%5:00:00:concealed:00")
)

(define-type ont::country-related-val
 :parent ont::status-val 
 :comment "adjectives relating to a nation or a country"
)

;; national, federal
(define-type ont::national-val
 :parent ont::country-related-val 
 :wordnet-sense-keys ("national%3:00:00::" "domestic%3:00:00" "national%5:00:00:domestic:00" "interior%5:00:00:domestic:00" "home%5:00:00:domestic:00" "internal%5:00:00:domestic:00" "national%3:00:01")
 :comment "having to do with a nation (or its government)"
)

(define-type ont::federal-val
 :parent ont::national-val 
 :wordnet-sense-keys ("federal%3:00:00::" "federal%5:00:00:national:01" "federal%3:01:02" )
)

(define-type ONT::origin-related-val
  :parent ONT::status-val
  )

(define-type ont::foreign-val
 :parent ONT::origin-related-val 
 :wordnet-sense-keys ("nonnative%3:00:00::" "foreign%3:00:01::" "strange%3:00:01::" "foreign%3:00:02" "outside%5:00:00:foreign:02" "international%5:00:00:foreign:02" "external%5:00:00:foreign:02" )
)

(define-type ont::national-identity-val
 :parent ont::country-related-val 
 :comment "identity based on the country/region of origin or residence"
)

;; american, korean, indian...
(define-type ont::nationality-val
 :parent ont::national-identity-val 
)

;; north-american, south-american, european...
(define-type ont::regional-identity-val
 :parent ont::national-identity-val 
)

(define-type ont::city-related-val
 :parent ont::status-val 
 :wordnet-sense-keys ("civic%3:01:00" )
 :comment "having to do with a city (or its government)"
)

;; urban, rural
(define-type ont::urban-val
 :parent ont::city-related-val
 :wordnet-sense-keys ("urban%3:00:00" "urban%3:01:00" )
)

(define-type ont::rural-val
 :parent ont::city-related-val 
 :wordnet-sense-keys ("provincial%3:00:00::" "rural%3:00:00" "rural%3:01:01" )
)


;; social status relating to fame
(define-type ont::fame-val
 :parent ont::EVALUATION-ATTRIBUTE-VAL ;status-val 
 :wordnet-sense-keys ("famous%5:00:00:known:00" "celebrated%5:00:00:known:00" "legendary%5:00:00:known:00")
 :comment "social status relating to fame"
 :sem (F::abstr-obj (F::scale ont::fame-scale))
)

(define-type ont::infamous-val
 :parent ont::fame-val 
 :wordnet-sense-keys ("notorious%5:00:00:disreputable:00" "ill-famed%5:00:00:disreputable:00" "infamous%5:00:00:disreputable:00" )
)

;; obligatory, optional
(define-type ont::obligatoriness-val
 :parent ont::status-val 
)

(define-type ont::obligatory-val
 :parent ont::obligatoriness-val 
 :wordnet-sense-keys ("obligatory%3:00:00" )
)

(define-type ont::optional-val
 :parent ont::obligatoriness-val 
 :wordnet-sense-keys ("optional%3:00:00" )
)

;; reasonable/sensible vs unreasonable (mental states)
(define-type ont::sensibility-val
 :parent ont::psychological-property-val
 :comment "describing mental ability or sensitivity to respond to emotional influences"
 :sem (F::abstr-obj (F::scale ont::rationality-scale) )
)

(define-type ont::sensible-val
 :parent ont::sensibility-val 
 :wordnet-sense-keys ("rational%3:00:00::" "sober%3:00:01::" "unneurotic%3:00:00::" "sensible%3:00:04" "sane%5:00:00:rational:00" "sane%3:00:00" "in_his_right_mind%5:00:00:sane:00")
 :comment "sensible, reasonable"
 :sem (F::abstr-obj (F::scale ont::rational-scale) )
)

(define-type ont::not-sensible-val
 :parent ont::sensibility-val 
 :wordnet-sense-keys ("irrational%3:00:00::" "immoderate%3:00:00::" "unreasonable%3:00:00" )
 :comment "unreasonable, irrational"
 :sem (F::abstr-obj (F::scale ont::rational-scale) )
)

(define-type ont::insane
 :parent ont::not-sensible-val 
 :wordnet-sense-keys ("brainsick%5:00:00:insane:00" "insane%3:00:00" )
 ; Words: (W::MAD W::CRAZY W::INSANE)
 ; Antonym: NIL (W::SANE)
 :comment "not sound of mind; afflicted by mental disorder(s) or mentally unhealthy"
)


;; careful, aware, attentive
(define-type ont::attention-val
 :parent ont::psychological-property-val 
 :comment "attributes that indicate the presence or absence of attention"
)

(define-type ont::carefulness-val
 :parent ont::attention-val 
 :sem (F::abstr-obj (F::scale ont::cautiousness-scale) )
)

(define-type ont::careful-val
 :parent ont::carefulness-val 
 :wordnet-sense-keys ("fastidious%3:00:00::" "cautious%3:00:00::" "prudent%3:00:00::" "hard%3:00:02::" "critical%3:00:02::" "discreet%3:00:00::" "careful%3:00:00" "careful%5:00:00:mindful:00" "heedful%5:00:00:mindful:00" )
 :sem (F::abstr-obj (F::scale ont::cautiousness-scale) (F::orientation f::pos))
)

(define-type ont::not-careful-val
 :parent ont::carefulness-val 
 :wordnet-sense-keys ("incautious%3:00:00::" "uncritical%3:00:02::" "noncritical%3:00:04::" "imprudent%3:00:00::" "careless%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::cautiousness-scale) (F::orientation f::neg))
)

(define-type ont::attentiveness-val
 :parent ont::attention-val
 :sem (F::abstr-obj (F::scale ont::attentiveness-scale)) 
)

(define-type ont::attentive-val
 :parent ont::attentiveness-val 
 :wordnet-sense-keys ("alert%3:00:00::" "watchful%3:00:00::" "attentive%3:00:00" "attentive%3:00:04" )
 :sem (F::abstr-obj (F::scale ont::attentiveness-scale) (f::orientation f::pos))
)

(define-type ont::not-attentive-val
 :parent ont::attentiveness-val 
 :wordnet-sense-keys ("unalert%3:00:00::" "unwatchful%3:00:00::" "unvigilant%3:00:00::" "negligent%3:00:00::" "unmindful%3:00:00::" "forgetful%3:00:02::" "mindless%3:00:00::" "inattentive%3:00:00" "heedless%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::attentiveness-scale) (f::orientation f::neg))
)

;; aware (of x)
(define-type ont::awareness-val
 :parent ont::attention-val 
 :sem (F::abstr-obj (F::scale ont::awareness-scale) )
)

(define-type ont::aware-val
 :parent ont::awareness-val 
 :wordnet-sense-keys ("conscious%3:00:00::" "conscientious%3:00:00::" "witting%3:00:00::" "aware%3:00:00" "mindful%3:00:00" "aware%3:00:04" )
 :sem (F::abstr-obj (F::scale ont::awareness-scale) (F::orientation f::pos))
)

(define-type ont::not-aware-val
 :parent ont::awareness-val 
 :wordnet-sense-keys ("unconscious%3:00:00::" "indiscreet%3:00:00::" "unwitting%3:00:00::" "insensitive%3:00:02::" "unaware%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::awareness-scale) (F::orientation f::neg))
)

;; (un)ambitious
(define-type ont::ambitiousness-val
 :parent ont::psychological-property-val 
 :sem (F::abstr-obj (F::scale ont::ambitiousness-scale) )
)

(define-type ont::ambitious-val
 :parent ont::ambitiousness-val 
 :wordnet-sense-keys ("ambitious%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::ambitiousness-scale) (F::orientation f::pos))
)

(define-type ont::not-ambitious-val
 :parent ont::ambitiousness-val 
 :wordnet-sense-keys ("unambitious%3:00:00" "ambitionless%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::ambitiousness-scale) (F::orientation f::neg))
)

;; sure, certain, confident
(define-type ont::certainty-val
 :parent ont::psychological-property-val
 :arguments ((:optional ONT::GROUND )) 
 :sem (F::abstr-obj (F::scale ont::certainty-scale) )
)

(define-type ont::certain
 :parent ont::certainty-val 
 :wordnet-sense-keys ("incontestable%3:00:00::" "incontestible%3:00:00::" "certain%3:00:03::" "sure%3:00:04::" "definite%3:00:00::" "unquestionable%3:00:00::" "definite%5:00:00:certain:01" "certain%3:00:01" "certain%3:00:02" "indisputable%5:00:00:certain:01" "sealed%3:00:02" "convinced%5:00:00:certain:02" )
 ; Words: (W::CERTAIN W::SURE W::CONFIDENT)
 ; Antonym: ONT::UNCERTAIN (W::UNCERTAIN W::UNSURE)
 :sem (F::abstr-obj (F::scale ont::certainty-scale) (F::orientation f::pos))
)

(define-type ont::uncertain
 :parent ont::certainty-val 
 :wordnet-sense-keys ("indefinite%3:00:00::" "uncertain%3:00:03::" "unsettled%3:00:02::" "inconclusive%3:00:00::" "uncertain%3:00:01::" "unsure%3:00:00" "unsealed%3:00:02" "uncertain%3:00:02" )
 ; Words: (W::UNCERTAIN W::UNSURE)
 ; Antonym: ONT::CERTAIN (W::CERTAIN W::SURE W::CONFIDENT)
 :sem (F::abstr-obj (F::scale ont::certainty-scale) (F::orientation f::neg))
)

(define-type ont::skeptical-val
 :parent ont::uncertain
 :wordnet-sense-keys ("skeptical%5:00:00:incredulous:00" "doubtful%5:00:00:uncertain:02" "incredulous%3:00:00")
)

(define-type ont::questionable-val
 :parent ont::uncertain
 :wordnet-sense-keys ("problematic%5:00:00:questionable:00" "funny%5:00:00:questionable:00" "questionable%3:00:00")
)

;; worrying, disturbing, tiresome
(define-type ont::evoking-experience-property-val
 :parent ont::psychological-property-val 
 :comment "attributes that indicate the evocation of a particular emotion"
)

(define-type ont::evoking-neg-experience-property-val
 :parent ont::evoking-experience-property-val 
)

(define-type ont::distressing-val
 :parent ont::evoking-neg-experience-property-val 
 :wordnet-sense-keys ("unpeaceful%3:00:00::" "worrying%5:00:00:heavy:02" "distressing%5:00:00:heavy:02" "perturbing%5:00:00:heavy:02" "worrisome%3:00:04" "troubling%5:00:00:heavy:02" "disturbing%5:00:00:heavy:02" "distressful%5:00:00:heavy:02" "worrisome%5:00:00:heavy:02" "appalling%5:00:00:alarming:00" "atrocious%5:00:00:alarming:00" "weighty%5:00:00:heavy:02" "heavy%3:00:02")
 :sem (F::abstr-obj (F::scale ont::distress-scale) )
)


; scary, frightening, fearsome
(define-type ont::frightening-val
 :parent ont::evoking-neg-experience-property-val 
 :wordnet-sense-keys ("alarming%3:00:00")
 :sem (F::abstr-obj (F::scale ont::fear-scale) )
)

(define-type ont::not-pleasing-val
 :parent ont::evoking-neg-experience-property-val 
 :wordnet-sense-keys ("unrewarding%3:00:00::" "unpalatable%3:00:00::" "infelicitous%3:00:00::" "unwelcome%3:00:00" "disagreeable%5:00:00:uncongenial:00" "unpleasant%3:00:00" "disagreeable%3:00:00" "displeasing%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::pleasantness-scale) (f::orientation f::neg))
)

(define-type ont::grievous-val
 :parent ont::evoking-neg-experience-property-val 
 :wordnet-sense-keys ("tearful%3:00:00::" "heartrending%5:00:00:sorrowful:00" "heartbreaking%5:00:00:sorrowful:00" "grievous%5:00:00:sorrowful:00")
 :sem (F::abstr-obj (F::scale ont::sadness-scale) )
)

(define-type ont::confusing-val
 :parent ont::evoking-neg-experience-property-val 
 :wordnet-sense-keys ("disorienting%3:00:00::" "confusing%5:00:00:disorienting:00" )
)

(define-type ont::boring
 :parent ont::evoking-neg-experience-property-val 
 :wordnet-sense-keys ("humorless%3:00:00::" "humourless%3:00:00::" "unhumorous%3:00:00::" "dull%3:00:03::" "uninteresting%3:00:00" "boring%5:00:00:uninteresting:00" "wearisome%5:00:00:uninteresting:00" "tiresome%5:00:00:uninteresting:00")
 ; Words: (W::DULL W::BORING W::UNINTERESTING)
 ; Antonym: NIL (W::INTERESTING)
 :sem (F::abstr-obj (F::scale ont::interestingness-scale) (f::orientation f::neg))
)

;(define-type ont::tiresome-val
; :parent ont::evoking-neg-emotion-val 
; :wordnet-sense-keys ("burdensome%5:00:00:heavy:02" "oppressive%5:00:00:heavy:02" "leaden%5:00:00:heavy:02")
;)

(define-type ont::evoking-pos-experience-property-val
 :parent ont::evoking-experience-property-val 
)

(define-type ont::calming-val
 :parent ont::evoking-pos-experience-property-val 
 :wordnet-sense-keys ("soothing%5:00:00:reassuring:00" "reassuring%3:00:00" "assuasive%5:00:00:reassuring:00")
 :sem (F::abstr-obj (F::scale ont::calmness-scale) )
)

(define-type ont::enjoyable-val
 :parent ont::evoking-pos-experience-property-val 
 :wordnet-sense-keys ("enjoyable%5:00:00:pleasant:00" )
 :sem (F::abstr-obj (F::scale ont::pleasurability-scale) )
)

(define-type ont::interesting-val
 :parent ont::evoking-pos-experience-property-val 
 :wordnet-sense-keys ("colorful%3:00:03::" "colourful%3:00:03::" "fascinating%5:00:00:interesting:00" "interesting%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::interestingness-scale) )
)

(define-type ont::pleasing-val
 :parent ont::evoking-pos-experience-property-val 
 :wordnet-sense-keys ("flattering%3:00:00::" "rewarding%3:00:00::" "congenial%3:00:00::" "felicitous%3:00:00::" "pleasing%3:00:00" "welcome%3:00:00" "agreeable%3:00:00" "delightful%5:00:00:pleasing:00" "pleasant%3:00:00")
 :sem (F::abstr-obj (F::scale ont::pleasantness-scale) )
)

;(define-type ONT::PLEASANT
; :parent ONT::pos-experiencer-property-val
; :wordnet-sense-keys ("pleasant%3:00:00")
; :sem (F::abstr-obj (F::scale ont::pleasantness-scale) )
; )

(define-type ont::desirable-val
 :parent ont::evoking-pos-experience-property-val 
 :wordnet-sense-keys ("desirable%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::desire-scale) )
)


;; EXPERIENCER PROPERTIES
(define-type ont::experiencer-property-val
 :parent ont::psychological-property-val
  :wordnet-sense-keys ("appetitive%3:01:00::" "algolagnic%3:01:00::" "technophobic%3:01:00::" "anglophilic%3:01:00::" "aversive%3:01:00::" "technophilic%3:01:00::" "libidinal%3:01:00::" "emotional%3:01:00::" "anglophobic%3:01:00::" "agonal%3:01:00::")
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Phys-obj (f::origin (? org f::human f::non-human-animal)))))
 :comment "state of experiencing a particular emotion or cognitive state"
)

;; happy, sad, gloomy...
;(define-type ont::emotional-val
; :parent ont::psychological-property-val 
; :arguments ((:ESSENTIAL ONT::FIGURE (F::Phys-obj (f::origin (? org f::human f::non-human-animal)))))
; :comment "state of having a particular emotion"
;)

;; experiencer properties: POSITIVE experiences
(define-type ont::pos-experiencer-property-val
 :parent ont::experiencer-property-val
 :comment "experiencing positive experiences"
)

;; experiencer properties: NEGATIVE experiences
(define-type ont::neg-experiencer-property-val
 :parent ont::experiencer-property-val
 :comment "experiencing negative experiences"
)

(define-type ont::bitter-val
 :parent ONT::taste-property-val
 :wordnet-sense-keys ("bitter%5:00:00:tasty:00" )
 :sem (F::abstr-obj (F::scale ont::bitterness-scale))
 )

(define-type ont::bitter-resentful-val
 :parent ONT::neg-experiencer-property-val 
 :wordnet-sense-keys ("resentful%3:00:00::" )
; :sem (F::Abstr-obj (F::scale ONT::bitter*1--07--00 ))
 :sem (F::abstr-obj (F::scale ont::resentfulness-scale))
)

;; experiencer properties: NEUTRAL experiences
(define-type ont::neutral-experiencer-property-val
 :parent ont::experiencer-property-val
 :comment "experiencing experiences that are neither positive nor negative"
)

(define-type ont::surprised
 :parent ont::neutral-experiencer-property-val
 :wordnet-sense-keys ("surprised%3:00:00")
 :sem (F::abstr-obj (F::scale ont::surprise-scale) )
) 


; intellectualy or cognitively attractive
(define-type ont::interested-val
 :parent ont::pos-experiencer-property-val 
 :wordnet-sense-keys ("curious%3:00:00::" "interested%3:00:00" "curious%5:00:00:interested:00" )
 :sem (F::abstr-obj (F::scale ont::interestedness-scale) )
)

; wanting/desiring
(define-type ONT::desirous
 :parent ONT::pos-experiencer-property-val
 :wordnet-sense-keys ("wanted%3:00:00::" "desirous%3:00:00" "avariciously%4:02:00" )
 :sem (F::abstr-obj (F::scale ont::desire-scale) )
 ; every living thing is desirous of avoiding pain
 )

;; both desiring and interested
(define-type ont::eager-val
 :parent ont::pos-experiencer-property-val 
 :wordnet-sense-keys ("eager%3:00:00" "enthusiastic%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::desire-scale) ) 
; technically this should map to  both desire and interest scales; but 
; i think it's stronger as desire than interest
)

(define-type ont::grateful
 :parent ont::pos-experiencer-property-val 
 :wordnet-sense-keys ("glad%5:00:00:grateful:00" "grateful%3:00:00" "glad%3:00:00" "thankful%3:00:00" )
 ; Words: (W::GLAD W::GRATEFUL W::CHEERFUL W::THANKFUL)
 ; Antonym: ONT::UNGRATEFUL (W::SAD W::MELANCHOLY W::UNGRATEFUL)
 :sem (F::abstr-obj (F::scale ont::gratitude-scale) )
)

(define-type ont::proud-val
 :parent ont::pos-experiencer-property-val 
 :wordnet-sense-keys ("proud%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::pride-scale) )
)

(define-type ont::happy-val
 :parent ont::pos-experiencer-property-val
 :wordnet-sense-keys ("contented%3:00:00::" "content%3:00:00::" "joyful%3:00:00::" "light%3:00:02::" "happy%3:00:00" "pleased%3:00:00")
 :sem (F::abstr-obj (F::scale ont::happiness-scale) (F::orientation f::pos)) 
)

(define-type ONT::EUPHORIC
 :parent ONT::happy-val
 ; Words: (W::HAPPY W::EUPHORIC)
 :wordnet-sense-keys ("elated%3:00:00::" "joyous%3:00:00::" "euphoric%3:00:00" "cheerful%3:00:00" "beaming%5:00:00:cheerful:00")
 ; Antonym: ONT::UNHAPPY (W::UNHAPPY W::MISERABLE)
 :sem (F::abstr-obj (F::scale ont::happiness-scale) (F::orientation f::pos) (F::intensity f::hi)) 
)

(define-type ONT::excited
 :parent ONT::pos-experiencer-property-val
 :wordnet-sense-keys ("passionate%3:00:00::" "moved%3:00:00::" "affected%3:00:02::" "stirred%3:00:00::" "touched%3:00:01::" "excited%3:00:00" "excited%5:00:00:wild:02" "pumped-up%5:00:00:tense:03")
 :sem (F::abstr-obj (F::scale ont::excitement-scale) )
 )


(define-type ONT::amused
 :parent ONT::pos-experiencer-property-val
 :wordnet-sense-keys ("amused%5:00:00:pleased:00")
 :sem (F::abstr-obj (F::scale ont::pleasurability-scale) )
 )

(define-type ONT::calm
 :parent ONT::pos-experiencer-property-val
 :wordnet-sense-keys ("discomposed%3:00:00::" "unhurried%3:00:00::" "secure%3:00:01::" "unafraid%3:00:02::" "untroubled%3:00:02::" "easy%3:00:02::" "calm%5:00:00:composed:00" "composed%3:00:00")
 :sem (F::abstr-obj (F::scale ont::calmness-scale) )
 )



;;; negative experiencer properties

(define-type ont::not-grateful-val
 :parent ont::neg-experiencer-property-val 
 :wordnet-sense-keys ("ungrateful%3:00:00" "thankless%3:00:00" "unthankful%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::gratitude-scale) )
)

(define-type ont::concerned-val
 :parent ont::neg-experiencer-property-val
 :wordnet-sense-keys ("concerned%3:00:00")
 :sem (F::abstr-obj (F::scale ont::worry-concern-scale) )
)

(define-type ont::puzzled-val
 :parent ont::neg-experiencer-property-val 
 :wordnet-sense-keys ("confused%3:00:00::" "perplexed%3:00:00" "baffled%5:00:00:perplexed:00" "puzzled%5:00:00:perplexed:00" "stuck%5:00:00:perplexed:00" )
 :sem (F::abstr-obj (F::scale ont::confusion-scale) )
)

(define-type ONT::afraid
 :parent ONT::neg-experiencer-property-val
 :wordnet-sense-keys ("afraid%3:00:00")
 :sem (F::abstr-obj (F::scale ont::fear-scale) )
 )

(define-type ONT::angry
 :parent ONT::neg-experiencer-property-val
 :wordnet-sense-keys ("angry%3:00:00")
 :sem (F::abstr-obj (F::scale ont::anger-scale) )
 )

(define-type ONT::envious
 :parent ONT::neg-experiencer-property-val
 :wordnet-sense-keys ("envious%5:00:00:desirous:00")
 :sem (F::abstr-obj (F::scale ont::envy-scale) )
 )

(define-type ONT::disgusted
 :parent ONT::neg-experiencer-property-val
 :wordnet-sense-keys ("disgusted%5:00:00:displeased:00")
 :sem (F::abstr-obj (F::scale ont::disgust-scale) )
 )

(define-type ONT::agitated
 :parent ONT::neg-experiencer-property-val
 :wordnet-sense-keys ("agitated%3:00:00" "hectic%5:00:00:agitated:00" "frantic%5:00:00:agitated:00")
 :sem (F::abstr-obj (F::scale ont::distress-scale) )
 )

(define-type ONT::uneasy
 :parent ONT::neg-experiencer-property-val
 :wordnet-sense-keys ("unquiet%3:00:00::" "insecure%3:00:01::" "anxious%5:00:00:troubled:00" "uneasy%3:00:00" "troubled%3:00:00" "tense%3:00:03")
 :sem (F::abstr-obj (F::scale ont::nervousness-scale) ) 
)

(define-type ONT::annoyed-val
 :parent ONT::neg-experiencer-property-val
 :wordnet-sense-keys ("grumpy%5:00:00:ill-natured:00" "irritable%5:00:00:ill-natured:00")
 :sem (F::abstr-obj (F::scale ont::bother-scale) )
)

(define-type ONT::unhappy
 :parent ONT::neg-experiencer-property-val
 ; Words: (W::UNHAPPY W::MISERABLE)
 :wordnet-sense-keys ("displeased%3:00:00::" "discontented%3:00:00::" "discontent%3:00:00::" "dysphoric%3:00:00" "unhappy%3:00:00" "gloomy%5:00:00:dejected:00" 
"miserable%5:00:00:unhappy:00" "dejected%3:00:00")
 ; Antonym: ONT::EUPHORIC (W::HAPPY W::EUPHORIC)
 :sem (F::abstr-obj (F::scale ont::happiness-scale) (f::orientation f::neg)) 
 :comment "not happy, dissatisfied or displeased"
)

(define-type ont::sad-val
 :parent ont::neg-experiencer-property-val
  :wordnet-sense-keys ("joyless%3:00:00::")
 :wordnet-sense-keys("sad%3:00:00" "melancholy%5:00:00:sad:00" "sorrowful%3:00:00")
 :sem (F::abstr-obj (F::scale ont::sadness-scale)) 
 :comment "feeling sorrow and sadness from misfortune, disappointment or regret"
)

(define-type ONT::sorry
 :parent ONT::neg-experiencer-property-val
 ; Words: (W::sorry)
 :wordnet-sense-keys ("ashamed%3:00:00::" "penitent%3:00:00::" "repentant%3:00:00::" "apologetic%3:00:00::" "excusatory%3:00:00::" "sorry%3:00:02")
 :sem (F::abstr-obj (F::scale ont::regret-scale) )
 )

(define-type ONT::bored
 :parent ONT::neg-experiencer-property-val
 :wordnet-sense-keys ("incurious%3:00:00::" "uneager%3:00:00::" "uninterested%3:00:00")
 ;:wordnet-sense-keys ("bored%5:00:00:tired:00" "bored%5:00:00:uninterested:00")
 :sem (F::abstr-obj (F::scale ont::apathy-scale) (f::orientation f::neg))
 )

#|
(define-type ont::not-interested-val
 :parent ont::neg-experiencer-property-val 
 :wordnet-sense-keys ("incurious%3:00:00::" "uneager%3:00:00::" "uninterested%3:00:00")
 :sem (F::abstr-obj (F::scale ont::apathy-scale) )
)
|#

;; smart, (un)intelligent
(define-type ont::intelligence-val
 :parent ont::psychological-property-val 
 :sem (F::abstr-obj (F::scale ont::intelligence-scale) )
)

(define-type ont::smart
 :parent ont::intelligence-val 
 :wordnet-sense-keys ("smart%3:00:00" "bright%5:00:00:intelligent:00" "cagey%5:00:00:smart:00" "intelligent%3:00:00" )
 ; Words: (W::CLEVER W::INTELLIGENT W::SMART)
 ; Antonym: ONT::STUPID (W::STUPID W::DUMB)
 :sem (F::abstr-obj (F::scale ont::intelligence-scale) (f::orientation f::pos))
)

(define-type ont::clever-val
 :parent ont::smart 
 :wordnet-sense-keys ("politic%3:00:00::" "cunning%5:00:00:adroit:00" "clever%5:00:00:adroit:00" "ingenious%5:00:00:adroit:00" "quick-witted%5:00:00:adroit:00")
)

(define-type ont::stupid
 :parent ont::intelligence-val 
 :wordnet-sense-keys ("dense%5:00:00:stupid:00" "unintelligent%3:00:00" "stupid%3:00:00" )
 ; Words: (W::STUPID W::DUMB)
 ; Antonym: ONT::smart (W::CLEVER W::INTELLIGENT W::SMART)
 :sem (F::abstr-obj (F::scale ont::intelligence-scale) (f::orientation f::neg))
)

;;; deliberate, on purpose
(define-type ont::intentionality-val
 :parent ont::psychological-property-val
 :sem (F::abstr-obj (F::scale ont::intentionality-scale))
 )

(define-type ont::intentional-val
 :parent ont::intentionality-val
  :wordnet-sense-keys ("purposeful%3:00:00::" "nonrandom%3:00:00::")
 :wordnet-sense-keys("deliberate%5:00:00:intended:00" "intentional%5:00:00:intended:00" "intended%3:00:00") 
 :sem (F::abstr-obj (F::scale ont::intentionality-scale) (f::orientation f::pos))
)

(define-type ont::not-intentional-val
 :parent ont::intentionality-val
 :wordnet-sense-keys("accidental%5:00:00:unintended:00" "unintentional%5:00:00:unintended:00" "unintended%3:00:00")
 :sem (F::abstr-obj (F::scale ont::intentionality-scale) (f::orientation f::neg))
 )

;; ad hoc vs premeditated process
(define-type ont::premeditation-val
 :parent ont::psychological-property-val 
 :comment "is the process planned (careful forethought and planning)?"
 :sem (F::abstr-obj (F::scale ont::premeditation-scale))
)

(define-type ont::premeditated-val
 :parent ont::premeditation-val
 :wordnet-sense-keys ("studied%3:00:00::" "planned%3:00:00" "premeditated%3:00:00")
 :sem (F::abstr-obj (F::scale ont::premeditation-scale) (f::orientation f::pos))
)

(define-type ont::not-premeditated-val
 :parent ont::premeditation-val
 :wordnet-sense-keys ("spontaneous%3:00:00::" "self-generated%3:00:04::" "unstudied%3:00:00::" "uncontrived%3:00:00::" "haphazard%5:00:00:random:00" "ad_hoc%5:00:00:unplanned:00" "impulsive%5:00:00:unpremeditated:00" "spontaneous%5:00:00:unscripted:00" "unpremeditated%3:00:00" "unscripted%3:00:00" "unplanned%3:00:00")
 :sem (F::abstr-obj (F::scale ont::premeditation-scale) (f::orientation f::neg))
)

;; resulting-state-val
(define-type ont::resulting-state-val
 :parent ont::property-val
  :wordnet-sense-keys ("modified%3:00:00::" "changed%3:00:00::" "altered%3:00:00::" "paid%3:00:00::" "undiluted%3:00:00::" "unmodified%3:00:00::" "unpaid%3:00:00::" "diluted%3:00:00::" "dilute%3:00:00::" "decreased%3:00:00::" "reduced%3:00:04::" "unsettled%3:00:01::" "inhabited%3:00:00::" "supported%3:00:00::" "increased%3:00:00::" "planted%3:00:00::" "agitated%3:00:02::" "balanced%3:00:00::" "uninhabited%3:00:00::" "saved%3:00:00::" "unmerited%3:00:00::" "unbuttoned%3:00:00::" "unfastened%3:00:02::" "cursed%3:00:00::" "curst%3:00:00::" "extended%3:00:00::" "lowered%3:00:00::" "surmounted%3:44:00::" "settled%3:00:01::" "hurried%3:00:00::" "treated%3:00:02::" "unplanted%3:00:00::" "unfueled%3:00:00::" "appointive%3:00:00::" "appointed%3:00:02::" "elective%3:00:00::" "elected%3:00:02::" "unlisted%3:00:00::" "unaccustomed%3:00:00::" "addressed%3:00:00::" "unaddressed%3:00:00::" "unperplexed%3:00:00::" "unobligated%3:00:00::" "nonaligned%3:00:00::" "aligned%3:00:01::" "opposed%3:00:00::" "fitted%3:44:00::" "scented%3:00:00::" "unsized%3:00:01::" "sized%3:00:01::" "preconceived%3:44:00::" "plugged%3:44:00::" "geared%3:00:00::" "ungeared%3:00:00::" "unmotivated%3:00:00::" "unassisted%3:00:00::" "powered%3:00:00::" "assisted%3:00:00::" "aided%3:00:00::" "fueled%3:00:00::" "cooked%3:00:00::" "covered%3:00:00::" "destroyed%3:00:00::" "frozen%3:00:00::" "involved%3:00:00::" "cut%3:00:01::" "uninhibited%3:00:00::" "melted%3:00:00::" "liquid%3:00:04::" "liquified%3:00:04::" "unfrozen%3:00:00::" "undiversified%3:00:00::" "unaffected%3:00:00::" "recorded%3:00:00::" "cut%3:00:03::" "ununderstood%3:00:00::" "unshaven%3:00:00::" "unshaved%3:00:00::" "segregated%3:00:00::" "unintegrated%3:00:01::" "unventilated%3:00:00::" "unaccompanied%3:00:00::" "moderating%3:00:00::" "trained%3:00:00::" "coated%3:00:00::" "unmitigated%3:00:00::" "afloat%3:00:00::" "undefeated%3:00:00::" "reported%3:00:00::" "painted%3:00:00::" "mined%3:00:00::" "raised%3:00:00::" "crowded%3:00:00::" "abridged%3:00:00::" "encumbered%3:00:00::" "burdened%3:00:00::" "specialized%3:00:00::" "specialised%3:00:00::" "detected%3:00:00::" "furrowed%3:00:00::" "rugged%3:00:02::" "castrated%3:00:00::" "unsexed%3:00:00::" "delineated%3:00:00::" "represented%3:00:02::" "delineate%3:00:00::" "unpolished%3:00:00::" "mitigated%3:00:00::" "ventilated%3:00:00::" "corrected%3:00:00::" "unfixed%3:00:00::" "unprocessed%3:00:00::" "derived%3:00:00::" "improved%3:00:00::" "undetected%3:00:00::" "unplowed%3:00:00::" "unploughed%3:00:00::" "unbroken%3:00:04::" "understood%3:00:00::" "unimproved%3:00:00::" "unagitated%3:00:02::" "unconstipated%3:00:00::" "regular%3:00:06::" "signed%3:00:00::" "motivated%3:00:00::" "reflected%3:00:00::" "blended%3:00:00::" "induced%3:00:00::" "unscheduled%3:00:00::" "developed%3:00:00::" "undeveloped%3:00:00::" "assigned%3:00:00::" "defeated%3:00:00::" "uncut%3:00:01::" "unfastened%3:00:00::" "unmelted%3:00:00::" "used%3:00:00::" "paved%3:00:00::" "loaded%3:00:00::" "unexploited%3:00:00::" "undeveloped%3:00:04::" "wired%3:00:00::" "sold%3:00:00::" "shared%3:00:00::" "caulked%3:00:00::" "unbound%3:00:01::" "unendowed%3:00:00::" "uncared-for%3:00:00::" "undelineated%3:00:00::" "unaccompanied%3:00:01::" "drained%3:00:00::" "unsupported%3:00:00::" "unturned%3:00:00::" "lighted%3:00:00::" "lit%3:00:02::" "shaven%3:00:00::" "shaved%3:00:00::" "lost%3:00:02::" "adjusted%3:00:00::" "expired%3:00:00::" "misused%3:00:00::" "touched%3:00:00::" "unloaded%3:00:00::" "polished%3:00:00::" "fastened%3:00:00::" "cosmopolitan%3:00:02::" "unseasoned%3:00:00::" "buried%3:00:00::" "inhumed%3:00:00::" "interred%3:00:00::" "unarmed%3:00:02::" "unconvinced%3:00:00::" "uncensored%3:00:00::" "unrequested%3:00:00::" "abused%3:00:01::" "ill-treated%3:00:00::" "maltreated%3:00:00::" "mistreated%3:00:00::" "unmarked%3:00:00::" "undifferentiated%3:00:00::" "uniform%3:00:04::" "censored%3:00:00::" "untreated%3:00:00::" "punished%3:00:00::" "unreflected%3:00:00::" "modulated%3:00:00::" "untrimmed%3:00:00::" "uncut%3:00:05::" "determined%3:00:00::" "unmated%3:00:00::" "exploited%3:00:00::" "uncut%3:00:03::" "rough%3:00:05::" "sealed%3:00:01::" "traveled%3:00:00::" "uncorrected%3:00:00::" "unbalanced%3:00:00::" "imbalanced%3:00:00::" "untraveled%3:00:00::" "untravelled%3:00:00::" "trimmed%3:00:00::" "cut%3:00:05::" "packaged%3:00:00::" "untangled%3:00:00::" "unsupervised%3:00:00::" "unlighted%3:00:00::" "unlit%3:00:02::" "mated%3:00:00::" "contracted%3:00:00::" "compartmented%3:00:00::" "unblended%3:00:00::" "unstained%3:00:00::" "unadjusted%3:00:00::" "earned%3:00:00::" "tied%3:00:00::" "fastened%3:00:03::" "unabridged%3:00:00::" "diversified%3:00:00::" "unburdened%3:00:00::" "contaminated%3:00:00::" "cared-for%3:00:00::" "unpunished%3:00:00::" "uninsured%3:00:00::" "unpatronized%3:00:00::" "unpatronised%3:00:00::" "patronless%3:00:00::" "unmodulated%3:00:00::" "scheduled%3:00:00::" "unoccupied%3:00:02::" "unspecialized%3:00:00::" "unspecialised%3:00:00::" "uncrowned%3:00:00::" "crownless%3:00:00::" "uninvolved%3:00:00::" "rusted%3:00:00::" "amended%3:00:00::" "found%3:00:00::" "noticed%3:00:00::" "uncultivated%3:00:00::" "unexpired%3:00:00::" "unstaged%3:00:00::" "unpackaged%3:00:00::" "unconditioned%3:00:00::" "innate%3:00:02::" "unlearned%3:00:02::" "lost%3:00:03::" "sworn%3:00:00::" "silenced%3:00:00::" "unanalyzed%3:00:00::" "awakened%3:00:00::" "insured%3:00:00::" "plowed%3:00:00::" "ploughed%3:00:00::" "blessed%3:00:00::" "blest%3:00:00::" "born%3:00:00::" "unsuspected%3:00:00::" "inbred%3:00:00::" "owned%3:00:00::" "endowed%3:00:00::" "supported%3:00:02::" "shaded%3:00:02::" "seasoned%3:00:00::" "untempered%3:00:02::" "unhardened%3:00:04::" "tapped%3:00:00::" "constipated%3:00:00::" "uncastrated%3:00:00::" "undetermined%3:00:00::" "irremovable%3:00:00::" "unearned%3:00:00::" "inflected%3:00:00::" "designed%3:00:00::" "intentional%3:00:04::" "selected%3:00:00::" "unpaved%3:00:00::" "analyzed%3:00:00::" "calibrated%3:44:00::" "graduated%3:44:00::" "defined%3:00:00::" "unfurnished%3:00:00::" "noninstitutionalized%3:00:00::" "noninstitutionalised%3:00:00::" "undeciphered%3:00:00::" "re-created%3:44:00::" "elapsed%3:44:00::" "parked%3:44:00::" "supervised%3:00:00::" "pursued%3:44:00::" "published%3:00:00::" "unpasteurized%3:44:00::" "unpasteurised%3:44:00::" "ionized%3:00:00::" "ionised%3:00:00::" "contested%3:44:00::" "unwrapped%3:00:00::" "unfunded%3:00:00::" "unconsummated%3:00:00::" "expanded%3:00:01::" "unsigned%3:00:00::" "enfranchised%3:00:00::" "unchartered%3:00:00::" "shrieked%3:44:00::" "disenfranchised%3:00:00::" "disfranchised%3:00:00::" "voiceless%3:00:00::" "voteless%3:00:00::" "uncarved%3:00:00::" "requested%3:00:00::" "accompanied%3:00:00::" "untied%3:00:00::" "unfastened%3:00:03::" "accompanied%3:00:01::" "attended%3:00:00::" "differentiated%3:00:00::" "unrefined%3:00:02::" "unprocessed%3:00:02::" "crude%3:00:02::" "counterrevolutionary%3:00:00::" "unwaxed%3:00:00::" "noncommissioned%3:00:00::" "convinced%3:00:00::" "crystallized%3:00:00::" "crystallised%3:00:00::" "pasteurized%3:44:00::" "pasteurised%3:44:00::" "sheared%3:00:00::" "shorn%3:00:00::" "hypophysectomized%3:44:00::" "hypophysectomised%3:44:00::" "tucked%3:00:00::" "baptized%3:00:00::" "baptised%3:00:00::" "unowned%3:00:00::" "ownerless%3:00:00::" "brainwashed%3:00:00::" "unleavened%3:00:00::" "unraised%3:00:00::" "crossed%3:00:02::" "uncrossed%3:00:02::" "atrophied%3:00:00::" "wasted%3:00:04::" "diminished%3:00:04::" "hypertrophied%3:00:00::" "enlarged%3:00:04::" "cultivated%3:00:00::" "commissioned%3:00:00::" "seated%3:00:00::" "sitting%3:00:02::" "unshaded%3:00:02::" "well-defined%3:00:00::" "clear%3:00:04::" "adopted%3:00:00::" "adoptive%3:00:04::" "reconstructed%3:00:00::" "unreconstructed%3:00:00::" "tempered%3:00:01::" "posed%3:44:00::" "stacked%3:44:00::" "moneyed%3:00:00::" "monied%3:00:00::" "labeled%3:00:00::" "labelled%3:00:00::" "tagged%3:00:00::" "focused%3:00:00::" "focussed%3:00:04::" "thoriated%3:44:00::" "tittering%3:44:00::" "suspected%3:00:00::" "collected%3:44:00::" "gathered%3:44:00::" "laced%3:00:00::" "tied%3:00:02::" "oxidized%3:44:00::" "oxidised%3:44:00::" "regulated%3:00:00::" "deciphered%3:00:00::" "saponified%3:44:00::" "tanned%3:00:00::" "penciled%3:44:00::" "pencilled%3:44:00::" "wrapped%3:00:00::" "solved%3:00:00::" "resolved%3:00:04::" "glazed%3:00:01::" "glassed%3:00:00::" "keyed%3:00:00::" "avenged%3:44:00::" "unavenged%3:44:00::" "enforced%3:00:00::" "implemented%3:00:02::" "sintered%3:44:00::" "funded%3:00:00::" "beneficed%3:00:00::" "chartered%3:00:00::" "hired%3:00:02::" "leased%3:00:02::" "camphorated%3:00:00::" "unoiled%3:00:00::" "unlabeled%3:00:00::" "unlabelled%3:00:00::" "untagged%3:00:00::" "untucked%3:00:00::" "leavened%3:00:00::" "sanitized%3:44:00::" "sanitised%3:44:00::" "sleeved%3:00:00::" "stained%3:00:00::" "branded%3:00:00::" "lamented%3:00:00::" "unposed%3:44:00::" "unassigned%3:00:00::" "uncollected%3:44:00::" "ungathered%3:44:00::" "unburied%3:00:00::" "uncarpeted%3:00:00::" "uncaulked%3:00:00::" "uncompartmented%3:00:00::" "uncamphorated%3:00:00::" "nonionized%3:00:00::" "nonionised%3:00:00::" "unionized%3:00:00::" "unionised%3:00:00::" "nonionic%3:00:00::" "unsaponified%3:44:00::" "untanned%3:00:00::" "uncontaminated%3:00:00::" "uncrossed%3:00:01::" "unlaureled%3:00:00::" "unlaurelled%3:00:00::" "undedicated%3:00:00::" "uncontested%3:44:00::" "undesigned%3:00:00::" "undrained%3:00:00::" "unenforced%3:00:00::" "unextended%3:00:00::" "uncrystallized%3:00:00::" "uncrystallised%3:00:00::" "unglazed%3:00:01::" "glassless%3:00:00::" "untreated%3:00:02::" "unlamented%3:00:00::" "unmourned%3:00:00::" "unbeneficed%3:00:00::" "uncoated%3:00:00::" "unlined%3:00:00::" "unsheathed%3:00:00::" "bare%3:00:04::" "unappendaged%3:00:00::" "unbalconied%3:00:00::" "untouched%3:00:00::" "ungusseted%3:00:00::" "unopposed%3:00:00::" "unpowered%3:00:00::" "unbaptized%3:00:00::" "unbaptised%3:00:00::" "unbanded%3:00:00::" "unbarreled%3:00:00::" "unbarrelled%3:00:00::" "nonintegrated%3:00:00::" "unintegrated%3:00:02::" "confined%3:00:01::" "unlubricated%3:00:00::" "ungreased%3:00:00::" "undimmed%3:00:00::" "bright%3:00:02::" "unenlivened%3:00:00::" "unbranded%3:00:00::" "unfurrowed%3:00:00::" "unmined%3:00:00::" "unabused%3:00:01::" "unpotted%3:00:00::" "unframed%3:00:00::" "unshuttered%3:00:00::" "unpublished%3:00:00::" "unregulated%3:00:00::" "unreported%3:00:00::" "unseeded%3:00:00::" "unselected%3:00:00::" "unsheared%3:00:00::" "unshorn%3:00:00::" "unsilenced%3:00:00::" "unsolved%3:00:00::" "unresolved%3:00:04::" "untapped%3:00:00::" "unbrainwashed%3:00:00::" "unweaned%3:00:00::" "mercerized%3:44:00::" "mercerised%3:44:00::" "malted%3:44:00::" "unmalted%3:44:00::" "platyrrhine%3:00:00::" "platyrrhinian%3:00:00::" "platyrhine%3:00:00::" "platyrhinian%3:00:00::" "platyrrhinic%3:00:00::" "broadnosed%3:00:00::" "potted%3:00:00::" "filled%3:44:00::" "unfilled%3:44:00::" "institutionalized%3:00:00::" "institutionalised%3:00:00::" "listed%3:00:00::" "landed%3:00:00::" "sublimed%3:44:00::" "sublimated%3:44:00::" "forced%3:44:00::" "voiced%3:00:00::" "sonant%3:00:00::" "soft%3:00:00::" "hardened%3:00:06::" "gusseted%3:00:00::" "framed%3:00:00::" "branchiate%3:00:00::" "gilled%3:00:00::" "posted%3:44:00::" "barreled%3:00:00::" "barrelled%3:00:02::" "hammered%3:44:00::" "lubricated%3:00:00::" "greased%3:00:00::" "squashed%3:44:00::" "oiled%3:00:00::" "waxed%3:00:00::" "calced%3:00:00::" "shod%3:00:04::" "staged%3:00:00::" "connected%3:00:00::" "unconnected%3:00:00::" "carved%3:00:00::" "carven%3:00:00::" "committed%3:00:00::")
 :comment "adjectives that describe the resulting states of the verb that it pertains to" 
 :sem (F::abstr-obj (F::scale ont::unordered-domain))
)

;; exhausted
(define-type ont::use-up-val
 :parent ont::resulting-state-val
)

(define-type ont::used-up-val
 :parent ont::use-up-val
 :wordnet-sense-keys ("exhausted%3:00:00")
)

(define-type ont::not-used-up-val
 :parent ont::use-up-val
 :wordnet-sense-keys ("unexhausted%3:00:00")
)


(define-type ont::processed-val
 :parent ont::resulting-state-val
 :wordnet-sense-keys ("treated%3:00:00::" "tempered%3:00:02::" "treated%3:00:04::" "hardened%3:00:04::" "toughened%3:00:04::" "refined%3:00:02::" "processed%3:00:02::" "preserved%3:00:02" "processed%3:00:00")
 :comment "processed (e.g., pickling, curing, milling)"
)

(define-type ont::conserved-val
 :parent ont::resulting-state-val
 :wordnet-sense-keys ("preserved%3:00:01")
 :comment "preserved by keeping things intact"
)

(define-type ont::preparedness-val
 :parent ont::resulting-state-val
 :comment "make something ready or suitable beforehand; mental planning, preparation, and premeditation belongs to ont::premeditation"
)

(define-type ont::prepared-val
 :parent ont::preparedness-val
 :wordnet-sense-keys ("unequipped%3:00:00::" "ready%3:00:00::" "unready%3:00:00::" "prepared%3:00:00")
)

(define-type ont::not-prepared-val
 :parent ont::preparedness-val
 :wordnet-sense-keys ("unprepared%3:00:00")
)

(define-type ont::protection-val
 :parent ont::resulting-state-val
)

(define-type ont::protected-val
 :parent ont::protection-val
 :wordnet-sense-keys ("sheathed%3:00:00::" "protected%3:00:00")
)

(define-type ont::not-protected-val
 :parent ont::protection-val
 :wordnet-sense-keys ("unprotected%3:00:00")
)

;; finalized, settled

(define-type ont::finalized-val
 :parent ont::resulting-state-val
 :wordnet-sense-keys ("settled%3:00:02") 
)

;; verified/tested or not verified?                                                                                                        
(define-type ont::verification-result-val
 :parent ont::resulting-state-val
)

(define-type ont::verified-val
 :parent ont::verification-result-val
 :wordnet-sense-keys ("verified%5:00:00:proved:00" "tested%5:00:00:proved:00" "proved%3:00:00")
)

(define-type ont::not-verified-val
 :parent ont::verification-result-val
 :wordnet-sense-keys ("unverified%5:00:00:unproved:00" "unproved%3:00:00")
)

;; habituated
(define-type ONT::habituated-val
 :parent ONT::resulting-state-val
 :arguments ((:ESSENTIAL ONT::affected (F::phys-obj))
             (:ESSENTIAL ONT::Formal)
             )
 :wordnet-sense-keys("accustomed%3:00:00")
 )

;; associated with - words that pertain to another word
(define-type ont::associated-with-val
 :parent ont::property-val
 :comment "adjectives that classify the noun that it is pertaining to" 
 :sem (F::abstr-obj (F::scale ont::unordered-domain))
)

;; associated with purpose
(define-type ont::associated-with-purpose-val
 :parent ont::associated-with-val
)

(define-type ont::exploratory-val
 :parent ont::associated-with-purpose-val
 :wordnet-sense-keys ("exploratory%3:00:00" "nonexploratory%3:00:00")
)

;; standard measurement 
(define-type ont::std-measurement-val
 :parent ont::associated-with-val
 :wordnet-sense-keys ("metric%3:01:00" "imperial%3:01:02")
)

;; associated with food
(define-type ont::associated-with-food-val
 :parent ont::associated-with-val
)

(define-type ont::dietary-val
 :parent ont::associated-with-food-val
 :wordnet-sense-keys ("dietary%3:01:00")
)

(define-type ont::nutritional-val
 :parent ont::associated-with-food-val
 :wordnet-sense-keys ("nutritional%3:01:00")
)

;; relating to commercial enterprise
(define-type ont::commercial-enterprise-val
 :parent ont::associated-with-val
)

(define-type ONT::COMMERCE-VAL
 :parent ONT::commercial-enterprise-val
 :wordnet-sense-keys ("commercial%3:00:00" "rental%3:01:01")
 )

(define-type ont::industrial-val
 :parent ont::commercial-enterprise-val
 :wordnet-sense-keys ("nonindustrial%3:00:00::" "industrial%3:01:00" "industrial%3:00:00")
)


(define-type ont::agricultural-val
 :parent ont::commercial-enterprise-val 
 :wordnet-sense-keys ("agricultural%3:01:00" "agricultural%5:00:00:rural:00" )
)

(define-type ont::livestock-val
 :parent ont::commercial-enterprise-val
 :wordnet-sense-keys ("pastoral%3:01:02" )
)

;; relating to economy
(define-type ont::economic-val
 :parent ont::associated-with-val 
 :wordnet-sense-keys ("economic%3:01:00" "economic%3:01:01" "financial%3:01:00" "fiscal%3:01:00" )
)

;; To put in adjectives which are difficult to find a spot somewhere
;; else. They have something to do with the information (technical
;; solution, for example) but there's no clear way to characterize
;; them, and there are no clear synonyms/antonyms
(define-type ont::abstract-information-property-val
 :parent ont::associated-with-val 
 :wordnet-sense-keys ("high-tech%3:00:00::" "hi-tech%3:00:00::" "low-tech%3:00:00::" "technical%3:00:00" "technical%3:01:00" )
 :arguments ((:required ont::FIGURE (?ft (f::information f::information-content )))) 
 :sem (f::abstr-obj (:default (f::gradability - )))
)

(define-type ont::associated-with-science-val
  :parent ont::associated-with-val   
  :wordnet-sense-keys ("unscientific%3:00:00::" "scientific%3:00:00::")
)

;; specific type defined for obtw demo
(define-type ont::medical-val
 :parent ont::associated-with-science-val 
 :wordnet-sense-keys ("surgical%3:00:00::" "operative%3:00:04::" "paramedical%3:01:00::" "medical%3:01:00" )
)

;; medicinal
(define-type ont::medicinal-val
 :parent ont::medical-val
 :wordnet-sense-keys ("medicinal%5:00:00:healthful:00")
)

;; associated with body
(define-type ont::associated-with-body-val
 :parent ont::associated-with-val 
)

;; body system associations digestive, immune, 
(define-type ONT::body-system-val
 :parent ONT::associated-with-body-val ;; of arguments can be more than just human; cardiac care; intestinal disturbance
  :wordnet-sense-keys ("ergotropic%3:01:00::" "lucifugous%3:01:00::" "lucifugal%3:01:00::" "circulatory%3:01:01::" "parasympathetic%3:01:00::" "skeletal%3:01:00::" "nervous%3:01:00::" "neural%3:01:01::" "phrenic%3:01:00::" "trophotropic%3:01:00::" "parasympathomimetic%3:01:00::" "neuroendocrine%3:01:00::" "urogenital%3:01:00::" "musculoskeletal%3:01:00::" "sympathetic%3:01:00::" "esophageal%3:01:00::" "urinary%3:01:01::" "gastroesophageal%3:01:00::")
 :comment "adjective having to do with body systems" 
)

;; congenital disease
(define-type ONT::congenital-val
 :parent ONT::body-system-val
 :wordnet-sense-keys ("congenital%5:00:00:noninheritable:00")
 )

;; genetic, hereditary
(define-type ONT::hereditary-val
 :parent ONT::body-system-val
 :wordnet-sense-keys ("hereditary%5:00:01:inheritable:00")
 )

;; body part associations
(define-type ONT::body-part-val
 :parent ONT::associated-with-body-val ;; of arguments can be more than just human; cardiac care; intestinal disturbance
  :wordnet-sense-keys ("atheromatous%3:01:00::" "atheromatic%3:01:00::" "vesicular%3:01:00::" "ocular%3:01:00::" "optic%3:01:00::" "optical%3:01:00::" "opthalmic%3:01:00::" "rectal%3:01:00::" "intervertebral%3:01:00::" "intracerebral%3:01:00::" "bursal%3:01:00::" "sacral%3:01:00::" "sternal%3:01:00::" "maxillodental%3:01:00::" "glottal%3:01:00::" "follicular%3:01:00::" "acrocentric%3:01:00::" "arteriovenous%3:01:00::" "abdominovesical%3:01:00::" "iridic%3:01:00::" "ectopic%3:01:00::" "phallic%3:01:00::" "exocrine%3:01:00::" "aboral%3:00:00::" "bregmatic%3:01:00::" "tubercular%3:01:01::" "hymenal%3:01:00::" "pneumogastric%3:01:00::" "gastric%3:01:00::" "stomachic%3:01:00::" "stomachal%3:01:00::" "palatoglossal%3:01:00::" "chiasmal%3:01:00::" "chiasmic%3:01:00::" "chiasmatic%3:01:00::" "enteric%3:01:00::" "enteral%3:01:00::" "genial%3:01:00::" "mental%3:01:04::" "glomerular%3:01:00::" "labyrinthine%3:01:00::" "operculate%3:01:00::" "operculated%3:01:00::" "hypothalamic%3:01:00::" "adnexal%3:01:00::" "annexal%3:01:00::" "hypophyseal%3:01:00::" "hypophysial%3:01:00::" "epithelial%3:01:00::" "tomentose%3:01:00::" "ventricular%3:01:01::" "genital%3:01:00::" "venereal%3:01:00::" "colonic%3:01:00::" "iritic%3:01:00::" "tendinous%3:01:00::" "sinewy%3:01:00::" "alveolar%3:01:00::" "vascular%3:01:00::" "pneumonic%3:01:00::" "pulmonary%3:01:00::" "pulmonic%3:01:00::" "cerebrospinal%3:01:00::" "guttural%3:01:00::" "bronchial%3:01:00::" "gastroduodenal%3:01:00::" "tentacular%3:01:00::" "side-to-side%3:01:00::" "coeliac%3:01:00::" "celiac%3:01:00::" "natal%3:01:01::" "papillate%3:01:00::" "condylar%3:01:00::" "myeloid%3:01:01::" "intercostal%3:01:00::" "alveolar%3:01:02::" "anoperineal%3:01:00::" "fanged%3:01:00::" "lobar%3:01:00::" "mastoid%3:01:00::" "mastoidal%3:01:01::" "mammary%3:01:00::" "bicipital%3:01:00::" "epiphyseal%3:01:00::" "epiphysial%3:01:00::" "neurogenic%3:01:00::" "pouched%3:01:00::" "diaphyseal%3:01:00::" "diaphysial%3:01:00::" "laryngeal%3:01:00::" "biliary%3:01:00::" "turbinate%3:01:00::" "glossopharyngeal%3:01:00::" "neuroglial%3:01:00::" "bubonic%3:01:00::" "transdermal%3:01:00::" "transdermic%3:01:00::" "percutaneous%3:01:00::" "transcutaneous%3:01:00::" "plantar%3:01:00::" "thyroid%3:01:01::" "thyroidal%3:01:00::" "nephritic%3:01:00::" "renal%3:01:00::" "occipital%3:01:00::" "cytological%3:01:00::" "cytologic%3:01:00::" "carotid%3:01:00::" "luteal%3:01:00::" "cranial%3:01:00::" "uninucleate%3:01:00::" "skinny%3:01:00::" "exuvial%3:01:00::" "pituitary%3:01:00::" "carinal%3:01:00::" "uterine%3:01:00::" "subclavian%3:01:00::" "areolar%3:01:00::" "areolate%3:01:00::" "orbital%3:01:02::" "duodenal%3:01:00::" "vesical%3:01:00::" "gastrointestinal%3:01:00::" "gi%3:01:00::" "conjunctival%3:01:00::" "vacuolate%3:01:00::" "vacuolated%3:01:00::" "branchial%3:01:00::" "temporal%3:01:00::" "cystic%3:01:01::" "mesenteric%3:01:00::" "cartilaginous%3:01:00::" "cerebrovascular%3:01:00::" "sartorial%3:01:01::" "pineal%3:01:00::" "nuclear%3:01:02::" "cardiovascular%3:01:00::" "perianal%3:01:00::" "maxillary%3:01:00::" "tarsal%3:01:00::" "capsular%3:01:02::" "perinasal%3:01:00::" "perirhinal%3:01:00::" "intradermal%3:01:00::" "intradermic%3:01:00::" "intracutaneous%3:01:00::" "crural%3:01:00::" "lobate%3:01:00::" "lobated%3:01:00::" "retinal%3:01:00::" "ciliary%3:01:01::" "ciliate%3:01:01::" "cilial%3:01:00::" "calycular%3:01:00::" "calicular%3:01:00::" "fistulous%3:01:00::" "metacarpal%3:01:00::" "paranasal%3:01:00::" "stomatal%3:01:02::" "stomatous%3:01:02::" "alvine%3:01:00::" "sublingual%3:01:00::" "abomasal%3:01:00::" "urethral%3:01:00::" "otic%3:01:00::" "auricular%3:01:00::" "cheliceral%3:01:00::" "chelicerate%3:01:00::" "perithelial%3:01:00::" "auricular%3:01:02::" "maxillomandibular%3:01:00::" "cardiopulmonary%3:01:00::" "cardiorespiratory%3:01:00::" "multinucleate%3:01:00::" "intraventricular%3:01:00::" "vulvar%3:01:00::" "vulval%3:01:00::" "hemal%3:01:00::" "haemal%3:01:00::" "hematal%3:01:00::" "haematal%3:01:00::" "aponeurotic%3:01:00::" "coccygeal%3:01:00::" "zonal%3:01:00::" "zonary%3:01:00::" "clitoral%3:01:00::" "clitoric%3:01:00::" "ovarian%3:01:00::" "polydactyl%3:01:00::" "polydactylous%3:01:00::" "femoral%3:01:00::" "lobular%3:01:00::" "precordial%3:01:00::" "hyoid%3:01:00::" "gluteal%3:01:00::" "undescended%3:01:00::" "centromeric%3:01:00::" "carunculate%3:01:00::" "carunculated%3:01:00::" "calyculate%3:01:00::" "calycled%3:01:00::" "cochlear%3:01:00::" "caudal%3:01:00::" "nucleated%3:01:00::" "nucleate%3:01:00::" "endometrial%3:01:00::" "dural%3:01:00::" "amniotic%3:01:00::" "amnionic%3:01:00::" "amnic%3:01:00::" "coronary%3:01:00::" "pectoral%3:01:00::" "thoracic%3:01:00::" "supraorbital%3:01:00::" "supraocular%3:01:00::" "myeloid%3:01:00::" "fleshy%3:01:00::" "sarcoid%3:01:00::" "ophthalmic%3:01:01::" "ciliary%3:01:02::" "ciliate%3:01:00::" "hypodermic%3:01:00::" "subcutaneous%3:01:00::" "telocentric%3:01:00::" "bodily%3:01:01::" "membranous%3:01:00::" "mesoblastic%3:01:00::" "mesodermal%3:01:00::" "palmar%3:01:00::" "volar%3:01:00::" "venous%3:01:00::" "agonadal%3:01:00::" "ciliary%3:01:00::" "pericardial%3:01:00::" "pericardiac%3:01:00::" "postganglionic%3:01:00::" "lumbar%3:01:00::" "canalicular%3:01:00::" "acinar%3:01:01::" "acinous%3:01:00::" "acinose%3:01:00::" "acinic%3:01:00::" "subdural%3:01:00::" "anorectal%3:01:00::" "arthromeric%3:01:00::" "ampullar%3:01:00::" "ampullary%3:01:00::" "endermic%3:01:00::" "endermatic%3:01:00::" "meningeal%3:01:00::" "glial%3:01:00::" "papillary%3:01:00::" "papillose%3:01:00::" "narial%3:01:00::" "adrenal%3:01:00::" "splenic%3:01:00::" "splenetic%3:01:00::" "lienal%3:01:00::" "shouldered%3:01:00::" "ectodermal%3:01:00::" "ectodermic%3:01:00::" "subcortical%3:01:00::" "tracheal%3:01:00::" "palpebrate%3:01:00::" "mandibular%3:01:00::" "inframaxillary%3:01:00::" "glabellar%3:01:00::" "pancreatic%3:01:00::" "paleocortical%3:01:00::" "pleural%3:01:00::" "glandular%3:01:00::" "fenestral%3:01:01::" "mucocutaneous%3:01:00::" "ossicular%3:01:00::" "ossiculate%3:01:00::" "proximal%3:00:00::" "intimal%3:01:00::" "auricular%3:01:01::" "binocular%3:01:00::" "arterial%3:01:00::" "chelate%3:01:01::" "peritoneal%3:01:00::" "canine%3:01:02::" "laniary%3:01:00::" "testicular%3:01:00::" "epidural%3:01:00::" "extradural%3:01:00::" "septal%3:01:00::" "septate%3:01:00::" "intestinal%3:01:00::" "enteric%3:01:01::" "enteral%3:01:01::" "pyloric%3:01:00::" "oral%3:01:00::" "aural%3:01:01::" "mental%3:01:03::" "tentacled%3:01:00::" "adenoidal%3:01:00::" "marsupial%3:01:00::" "costal%3:01:00::" "penile%3:01:00::" "penial%3:01:00::" "cutaneous%3:01:00::" "cutaneal%3:01:00::" "dermal%3:01:02::" "acentric%3:01:00::" "mastoid%3:01:01::" "sacculated%3:01:00::" "sacculate%3:01:00::" "blastodermatic%3:01:00::" "blastodermic%3:01:00::" "tympanic%3:01:00::" "antennal%3:01:00::" "antennary%3:01:00::" "adventitial%3:01:00::" "cervical%3:01:01::" "caruncular%3:01:00::" "carunculous%3:01:00::" "pharyngeal%3:01:00::" "neocortical%3:01:00::" "apophyseal%3:01:01::" "patellar%3:01:00::" "preanal%3:01:00::" "uveal%3:01:00::" "uveous%3:01:00::" "zygomatic%3:01:00::" "vertebral%3:01:00::" "labial%3:01:01::" "pudendal%3:01:00::" "endothelial%3:01:00::" "buccal%3:01:00::" "carnal%3:01:00::" "abdominal%3:01:00::" "cerebral%3:01:00::" "bulbar%3:01:00::" "myelic%3:01:00::" "pubic%3:01:00::" "chemoreceptive%3:01:00::" "lingual%3:01:01::" "endocrine%3:01:00::" "endocrinal%3:01:00::" "hypodermal%3:01:00::" "chorionic%3:01:00::" "metacentric%3:01:00::" "cortical%3:01:00::" "mandibulofacial%3:01:00::" "scapulohumeral%3:01:00::" "intramuscular%3:01:00::" "cerebellar%3:01:00::" "anatomic%3:01:00::" "anatomical%3:01:00::" "corneal%3:01:00::" "adrenal%3:01:01::" "limbic%3:01:00::" "adrenocortical%3:01:00::" "atrial%3:01:00::" "mucosal%3:01:00::" "parietal%3:01:00::" "osteal%3:01:01::" "sclerotic%3:01:00::" "iliac%3:01:00::" "sarcosomal%3:01:00::" "gingival%3:01:00::" "papilliform%3:01:00::" "phalangeal%3:01:00::" "tuberculate%3:01:00::" "faucal%3:01:00::" "appendicular%3:01:00::" "gonadal%3:01:00::" "pectineal%3:01:00::" "jugular%3:01:00::" "scalene%3:01:01::" "cervical%3:01:00::" "axillary%3:01:01::" "sarcolemmal%3:01:00::" "manual%3:01:00::" "cytogenetic%3:01:00::" "cytogenetical%3:01:00::" "digital%3:01:00::" "myoid%3:01:00::" "blastocoelic%3:01:00::" "dental%3:01:00::" "medullary%3:01:00::" "lacrimal%3:01:01::" "lachrymal%3:01:01::" "muscular%3:01:00::" "distal%3:00:01::" "allantoic%3:01:00::" "suborbital%3:01:02::" "subocular%3:01:00::" "genitourinary%3:01:00::" "gu%3:01:00::" "vaginal%3:01:00::" "spinal%3:01:00::" "neuroanatomic%3:01:00::" "neuroanatomical%3:01:00::" "scapular%3:01:00::" "atrioventricular%3:01:00::" "auriculoventricular%3:01:00::" "labial%3:01:00::" "prostate%3:01:00::" "prostatic%3:01:00::" "aculeate%3:01:00::" "aculeated%3:01:00::" "trabecular%3:01:00::" "trabeculate%3:01:00::" "chelicerous%3:01:00::" "astragalar%3:01:00::" "laryngopharyngeal%3:01:00::" "brachial%3:01:00::" "neuromuscular%3:01:00::" "anatomic%3:01:01::" "anatomical%3:01:01::" "nasopharyngeal%3:01:00::" "perineal%3:01:00::" "mandibulate%3:01:00::" "uvular%3:01:00::" "intravenous%3:01:00::" "endovenous%3:01:00::" "sarcolemmic%3:01:00::" "sarcolemnous%3:01:00::" "ossiferous%3:01:00::" "cecal%3:01:00::" "caecal%3:01:00::" "rectosigmoid%3:01:00::" "metatarsal%3:01:00::" "pelvic%3:01:00::" "capillary%3:01:00::" "cephalic%3:01:00::" "pilar%3:01:00::" "tubal%3:01:00::" "scrotal%3:01:00::" "hepatic%3:01:00::" "intrapulmonary%3:01:00::" "molar%3:01:03::" "cheliferous%3:01:00::" "facial%3:01:00::" "epitheliod%3:01:00::" "visceral%3:01:00::" "splanchnic%3:01:00::" "maxillofacial%3:01:00::" "adenoid%3:01:00::" "calcaneal%3:01:00::" "anal%3:01:00::" "aortal%3:01:00::" "aortic%3:01:00::" "sulcate%3:01:00::" "thenal%3:01:00::" "thenar%3:01:00::" "hemispheric%3:01:00::" "bronchiolar%3:01:00::" "trophoblastic%3:01:00::" "medullary%3:01:01::" "articular%3:01:00::" "articulary%3:01:00::" "histological%3:01:00::" "histologic%3:01:00::" "velar%3:01:00::" "hilar%3:01:00::" "rhinal%3:01:00::" "nasal%3:01:00::" "thyroid%3:01:00::" "blastoporal%3:01:00::" "blastoporic%3:01:00::" "epigastric%3:01:00::" "arteriolar%3:01:00::" "oropharyngeal%3:01:00::" "parenteral%3:01:00::" "myocardial%3:01:00::" "fibrocartilaginous%3:01:00::" "colorectal%3:01:00::" "cuneiform%3:01:00::" "gyral%3:01:00::" "medullary%3:01:02::" "inguinal%3:01:00::")
 :comment "adjectives having to do with body parts"
 )

;; cardiac
(define-type ont::cardiac-val
 :parent ont::body-part-val ;; of argument can be events (cardiac care, cardiac arrest) or phys-obj (cardiac muscle)
 :wordnet-sense-keys ("cardiac%3:01:00" )
)

;; permission related
(define-type ont::permission-related-val
 :parent ont::associated-with-val 
)

(define-type ont::allows-doing-val
 :parent ont::permission-related-val
  :wordnet-sense-keys ("nonspeaking%3:00:00::" "walk-on%3:00:00::")
)

;; mental, cerebral
(define-type ont::mental-val
 :parent ont::associated-with-val 
 :wordnet-sense-keys ("inward%3:00:00::" "cerebral%3:00:00::" "intellectual%3:00:05::" "mental%3:00:00" "mental%3:01:00" )
 :arguments ((:optional ONT::GROUND )) 
)

;; physical, bodily
(define-type ONT::physics-val
  :parent ONT::associated-with-science-val
  )

(define-type ont::physical-val
 :parent ONT::physics-val 
 :wordnet-sense-keys ("corporeal%3:00:00::" "material%3:00:04::" "physical%3:00:00" "bodily%5:00:00:physical:00" )
 :arguments ((:optional ONT::GROUND )) 
)

;; relating to machines
(define-type ont::mechanical-val
 :parent ont::physical-val
 :wordnet-sense-keys ("nonmechanical%3:00:00::" "mechanical%3:00:00")
)

;; monroe
(define-type ONT::POLITICAL
 :parent ont::associated-with-val
 :wordnet-sense-keys ("democratic%3:00:00::" "political%3:00:00")
 )

;; assoc with languages and linguistics
(define-type ont::associated-with-languages-val
 :parent ont::associated-with-val
 :wordnet-sense-keys ("linguistic%3:01:00" "intralinguistic%3:01:00")
)

;; relating to languages
(define-type ont::related-to-languages-val
 :parent ont::associated-with-languages-val
)

(define-type ont::language-specific-val
 :parent ONT::associated-with-languages-val
 :wordnet-sense-keys ("nordic%3:01:01::" "thai%3:01:01::" "tai%3:01:01::" "siamese%3:01:01::" "hebraic%3:01:00::" "hebraical%3:01:00::" "hebrew%3:01:00::" "tamil%3:01:00::" "scythian%3:01:00::" "nilotic%3:01:00::" "altaic%3:01:00::" "turkic%3:01:00::" "koranic%3:01:00::" "syntagmatic%3:01:00::" "aramaic%3:01:00::" "latin%3:01:00::" "amharic%3:01:00::" "gothic%3:01:01::" "germanic%3:01:00::" "bantu-speaking%3:01:00::" "italic%3:01:00::" "demotic%3:01:01::" "slavonic%3:01:00::" "slavic%3:01:00::" "cockney%3:01:01::" "synchronic%3:00:00::" "prakritic%3:01:00::" "sinhala%3:01:00::" "singhalese%3:01:01::" "sinhalese%3:01:01::" "latinate%3:01:00::" "vietnamese%3:01:00::" "sentential%3:01:00::" "indo-european%3:01:01::" "indo-germanic%3:01:00::" "bantoid%3:01:00::" "english%3:01:01::" "malayo-polynesian%3:01:00::" "creole%3:01:02::" "catalan%3:01:00::" "romance%3:01:00::" "latin%3:01:02::" "quechuan%3:01:00::" "kechuan%3:01:00::" "sotho%3:01:00::" "macaronic%3:01:00::" "semitic%3:01:01" "cyrillic%3:01:00")
 :comment "associated specifically with language written or spoken"
)

;; not related to languages
(define-type ont::not-related-to-languages-val
 :parent ont::associated-with-languages-val
 :wordnet-sense-keys ( "nonlinguistic%3:01:00" "extralinguistic%3:01:00")
)

;; relating to linguistics
(define-type ONT::LINGUISTICS-VAL
 :parent ont::associated-with-languages-val 
 :wordnet-sense-keys ("linguistic%3:01:01" "psycholinguistic%3:01:00" "sociolinguistic%3:01:00" "diachronic%3:00:00" "morphologic%3:01:01" "phonological%3:01:00" "phonetic%3:01:00" "semantic%3:01:00")
 :comment "associated with the discipline of linguistics and its various subfields"
)

(define-type ont::linguistic-property-val
 :parent ONT::LINGUISTICS-VAL
 :wordnet-sense-keys ("syllabic%3:00:00::" "figurative%3:00:00::" "nonliteral%3:00:00::" "antonymous%3:00:00::" "connotative%3:00:00::" "unstressed%3:00:00::" "rhymed%3:00:00::" "rhyming%3:00:04::" "riming%3:00:00::" "soft%3:00:03::" "stressed%3:00:00::" "accented%3:00:00::" "literal%3:00:00::" "hard%3:00:03::" "unvoiced%3:00:00::" "voiceless%3:00:04::" "surd%3:00:00::" "hard%3:00:00::" "nonsyllabic%3:00:00::" "unsyllabic%3:00:00::" "lowercase%3:00:00::" "vocalic%3:00:02::" "syllabic%3:00:02::" "syllabic%3:00:01::" "uninflected%3:00:00::" "tonic%3:00:00::" "accented%3:00:04::" "atonic%3:00:00::" "unaccented%3:00:04::" "late%3:00:01::" "standard%3:00:03::" "received%3:00:04::" "nonstandard%3:00:03::" "attributive%3:00:00::" "prenominal%3:00:00::" "synthetic%3:00:02::" "tense%3:00:02::" "voluble%3:00:00::" "analytic%3:00:02::" "uninflected%3:00:04::" "formal%3:00:02::" "early%3:00:01::" "participial%3:01:00::" "intransitive%3:00:00::" "predicative%3:00:00::" "passive%3:00:02::" "lexical%3:01:01::" "deictic%3:01:00::" "verbal%3:01:00::" "lexicostatistic%3:01:00::" "phonemic%3:01:00::" "titular%3:01:01::" "aphetic%3:01:00::" "imperative%3:01:00::" "independent%3:00:02::" "main%3:00:02::" "phonic%3:01:01::" "objective%3:01:00::" "accusative%3:01:00::" "terminological%3:01:00::" "prepositional%3:01:00::" "prescriptive%3:00:00::" "normative%3:00:00::" "scopal%3:01:00::" "substantival%3:01:00::" "uninflected%3:00:01::" "contextual%3:01:00::" "allophonic%3:01:00::" "dependent%3:00:02::" "subordinate%3:00:03::" "asyndetic%3:00:00::" "syntactic%3:01:00::" "syntactical%3:01:00::" "pronominal%3:01:00::" "lexical%3:01:00::" "nominal%3:01:01::" "descriptive%3:00:00::" "appositional%3:01:00::" "appositive%3:01:00::" "inanimate%3:00:02::" "inflected%3:00:01::" "aphaeretic%3:01:00::" "apheretic%3:01:00::" "anagrammatic%3:01:00::" "anagrammatical%3:01:00::" "homonymic%3:01:00::" "homonymous%3:01:00::" "morphemic%3:01:00::" "adjectival%3:01:00::" "adjective%3:01:00::" "infinitival%3:01:00::" "nominative%3:01:00::" "patronymic%3:01:00::" "syncretic%3:01:01::" "syncretical%3:01:01::" "syncretistic%3:01:01::" "syncretistical%3:01:01::" "lexicographic%3:01:00::" "lexicographical%3:01:00::" "allomorphic%3:01:00::" "syndetic%3:00:00::" "gerundial%3:01:00::" "lax%3:00:02::" "nominal%3:01:00::" "nonlexical%3:01:00::" "vocalic%3:01:00::" "eponymous%3:01:00::" "eponymic%3:01:00::" "consonantal%3:01:00::" "adverbial%3:01:00::" "vocative%3:01:00::" "coreferential%3:01:00::" "co-referent%3:01:00::" "lexicalized%3:01:00::" "lexicalised%3:01:00::" "philological%3:01:00::" "long%3:00:04::" "future%3:01:00::" "aspectual%3:01:00::" "ablative%3:01:00::" "subordinating%3:00:00::" "subordinative%3:00:00::" "morphophonemic%3:01:00::" "titular%3:01:02::" "transitive%3:00:00::" "onomastic%3:01:00::" "aoristic%3:01:00::" "bilabial%3:01:00::" "homophonous%3:01:00::" "modal%3:01:00::" "middle%3:00:01::" "possessive%3:01:00::" "genitive%3:01:00::" "animate%3:00:02::" "acronymic%3:01:00::" "acronymous%3:01:00::" "titular%3:01:00::" "appellative%3:01:00::" "verbal%3:01:01::" "syllabic%3:01:01::" "strong%5:00:00:irregular:00" "stative%3:00:00" "subjunctive%3:01:00" "finite%3:00:02" "indicative%3:01:00" "interrogative%3:01:00" "infinite%3:00:02""optative%3:01:00" "unrestricted%5:00:00:unmodified:00" "grammatical%3:00:00" "radical%3:01:01" "affixal%3:01:00" "cross-linguistic%3:01:00" "singular%3:00:00" "ungrammatical%3:00:00" "coordinating%3:00:00" "endocentric%3:00:00" "exocentric%3:00:00" "feminine%3:00:02" "grammatical%3:01:00" "inflectional%3:00:00" "masculine%3:00:02" "neuter%3:00:00" "paradigmatic%3:01:01" "personal%3:01:00" "plural%3:00:00" "syncategorematic%3:00:00" "active%3:00:09" "categorematic%3:00:00" "derivational%3:00:00" "short%3:00:04""polyphonic%3:01:01" "phonetic%3:01:01")
)

(define-type ONT::WRITTEN-LANGUAGE-PROPERTY-VAL
 :parent ONT::associated-with-languages-val
 :wordnet-sense-keys ("alphabetic%3:00:00::" "alphabetical%3:00:00::" "majuscule%3:00:00::" "uppercase%3:00:00::" "minuscule%3:00:00::" "minuscular%3:00:00::" "allographic%3:01:00::" "hieroglyphic%3:01:00::" "hieroglyphical%3:01:00::" "hieratic%3:01:00::" "phonogramic%3:01:00::" "boustrophedonic%3:01:00::" "ideographic%3:01:00::" "runic%3:01:00::" "analphabetic%3:01:00::" "logogrammatic%3:01:00::" "logographic%3:01:00::" "alphabetic%3:01:00::" "alphabetical%3:01:00::" "syllabic%3:01:00::" "uncial%3:01:00::" "scriptural%3:01:02::" "pictographic%3:01:00::" "analphabetic%3:00:00" "separative%5:00:02:disjunctive:00" "orthographic%3:01:00")
)

;(define-type ont::semantic-val
; :parent ONT::associated-with-languages-val 
; :wordnet-sense-keys ("semantic%3:01:00" )
;)

(define-type ONT::associated-with-society-and-culture-val
  :parent ONT::associated-with-val
  )

(define-type ONT::CULTURE-VAL
 :parent ONT::associated-with-society-and-culture-val
 :wordnet-sense-keys ("sociocultural%3:01:00::" "cultural%3:01:00::" "transcultural%3:01:00" "cross-cultural%3:01:00" "multicultural%3:01:00")
 :comment "associated with culture, people, nation, or language"
)

(define-type ont::culture-specific-val
 :parent ONT::CULTURE-VAL
 :wordnet-sense-keys ("aegean%3:01:01::" "minoan%3:01:00::" "aleutian%3:01:00::" "incan%3:01:00::" "sinitic%3:01:00" "siouan%3:01:00" "somalian%3:01:00")
 :comment "associated specifically with culture or peole"
)

(define-type ONT::RACE-VAL
 :parent ONT::associated-with-society-and-culture-val
  :wordnet-sense-keys ("black%3:00:02::" "racial%3:00:00::" "white%3:00:02::")
)

(define-type ont::race-specific-val
 :parent ONT::RACE-VAL
  :wordnet-sense-keys ("racial%3:01:00::")
 :comment "associated with race"
)

(define-type ONT::associated-with-belief-systems-val
  :parent ONT::associated-with-val
  )

(define-type ONT::RELIGION-VAL
 :parent ONT::associated-with-belief-systems-val
  :wordnet-sense-keys ("heavenly%3:00:00::" "infernal%3:00:00::" "nonsectarian%3:00:00::" "unsectarian%3:00:00::" "sectarian%3:00:00::" "monotheistic%3:00:00::" "religious%3:00:01::")
 :comment "having to do with religion"
)

(define-type ont::religion-specific-val
 :parent ONT::RELIGION-VAL 
  :wordnet-sense-keys ("christian%3:00:00::" "unchristian%3:00:00::" "pentecostal%3:01:01::" "rastafarian%3:01:00::" "taoist%3:01:02::" "formalistic%3:01:00::" "formalized%3:01:00::" "formalised%3:01:00::" "catechismal%3:01:00::" "bahai%3:01:00::" "mystic%3:01:00::" "mystical%3:01:00::" "cataphatic%3:01:00::" "apophatic%3:01:00::" "calvinist%3:01:00::" "calvinistic%3:01:00::" "calvinistical%3:01:00::" "unitarian%3:01:00::" "catholic%3:01:00::" "fundamentalist%3:01:00::" "fundamentalistic%3:01:00::" "pantheist%3:01:00::" "pantheistic%3:01:00::" "arminian%3:01:00::" "yogistic%3:01:00::" "yogic%3:01:00::" "tantric%3:01:00::" "tantrik%3:01:00::" "discalced%3:00:00::" "discalceate%3:00:00::" "unshod%3:00:04::" "orthodox%3:01:01::" "eastern_orthodox%3:01:00::" "russian_orthodox%3:01:00::" "greek_orthodox%3:01:00::" "ecclesiastical%3:01:00::" "ecclesiastic%3:01:00::" "shuha%3:01:00::" "donatist%3:01:00::" "mithraic%3:01:00::" "mithraistic%3:01:00::" "kokka%3:01:00::" "wiccan%3:01:00::" "hindu%3:01:00::" "hindi%3:01:00::" "hindoo%3:01:00::" "shinto%3:01:00::" "shintoist%3:01:00::" "shintoistic%3:01:00::" "evangelical%3:01:01::" "roman%3:01:00::" "r.c.%3:01:00::" "romanist%3:01:00::" "romish%3:01:00::" "roman_catholic%3:01:00::" "popish%3:01:00::" "papist%3:01:00::" "papistic%3:01:00::" "papistical%3:01:00::" "muslim%3:01:00::" "moslem%3:01:00::" "islamic%3:01:00::" "sikh%3:01:00::" "carthusian%3:01:00::" "buddhist%3:01:00::" "buddhistic%3:01:00::" "theist%3:01:00::" "theistical%3:01:00::" "theistic%3:01:00::" "orthodox%3:01:00::" "jewish-orthodox%3:01:00::" "manichaean%3:01:00::" "manichean%3:01:00::" "manichee%3:01:00::" "judaic%3:01:00::" "judaical%3:01:00::" "jain%3:01:00::" "jainist%3:01:00::" "uniate%3:01:00::" "hasidic%3:01:00::" "hassidic%3:01:00::" "chasidic%3:01:00::" "chassidic%3:01:00::" "revivalistic%3:01:00::" "byzantine%3:01:01::" "christian%3:01:00::" "judeo-christian%3:01:00::" "shamanist%3:01:00::" "shamanistic%3:01:00::" "albigensian%3:01:00::")
 :comment "identity specifically based on religious affiliation, dogma, or theology (properties referring to the culture of the practicing people or nations belong to ont::culture-specific)"
)

;; for boudreaux
(define-type ONT::BIOLOGY-VAL
 :parent ONT::associated-with-science-val
 :wordnet-sense-keys ("biological%3:00:00::" "biological%3:01:00")
 )

(define-type ONT::chemistry-val
  :parent ONT::associated-with-science-val
  )

(define-type ONT::chemical-val
 :parent ONT::chemistry-val
 :wordnet-sense-keys ("chemical%3:01:00")
 )

(define-type ONT::astronomy-val
 :parent ONT::associated-with-science-val
 :wordnet-sense-keys ("lunar%3:01:00")
)

;; words relating to space
(define-type ont::spatial-val
; :parent ont::abstract-object
 :parent ont::property-val
 :arguments ((:OPTIONAL ONT::FIGURE ((? of F::Phys-obj F::Situation f::abstr-obj)))
	     (:OPTIONAL ONT::GROUND ((? val F::Phys-obj F::Situation f::abstr-obj)))
	     ;(:OPTIONAL ONT::FIGURE)
	     ;(:OPTIONAL ONT::GROUND)
             )
 :wordnet-sense-keys ("spatial%3:01:00")
 :comment "properties relating to space"
 :sem (F::abstr-obj (F::scale ont::spatial-scale ))
 )

;; circular, direct
(define-type ONT::ROUTE-TOPOLOGY-VAL
  :wordnet-sense-keys ("straight%3:00:05::" "straight%3:00:01" "nonstop%5:00:00:direct:00" "direct%3:00:00")
 :parent ONT::spatial-val
 :sem (F::abstr-obj (:required)(:default (F::gradability -)))
 :arguments ((:REQUIRED ONT::FIGURE (F::phys-obj (F::spatial-abstraction (? sab F::line F::strip))))
             )
 )

(define-type ONT::orientation-val
    :parent ONT::spatial-val
    :comment "spatial relations defining the orientation or an object"
  :wordnet-sense-keys ("oriented%3:00:00::" "orientated%3:00:00::" "orienting%3:00:00::" "orientating%3:00:00::" "unoriented%3:00:00::")
  :arguments ((:OPTIONAL ONT::GROUND (F::phys-obj))
             )
 )

(define-type ont::spatial-arrangement-val
 :parent ONT::orientation-val
)

(define-type ont::concentric-val
 :parent ont::spatial-arrangement-val
 :wordnet-sense-keys ("concentric%3:00:00")
)

(define-type ont::peripheral-val
 :parent ont::spatial-arrangement-val
 :wordnet-sense-keys ("peripheral%3:00:00")
)

(define-type ONT::VERTICAL
 :parent ONT::ORIENTATION-VAL
 ; Words: (W::STRAIGHT W::VERTICAL W::PERPENDICULAR)
 :wordnet-sense-keys ("erect%3:00:00" "vertical%3:00:00" "perpendicular%3:00:04" "upright%5:00:00:vertical:00")
 ; Antonym: ONT::HORIZONTAL (W::PARALLEL W::HORIZONTAL)
 )

(define-type ONT::HORIZONTAL
 :parent ONT::ORIENTATION-VAL
 ; Words: (W::PARALLEL W::HORIZONTAL)
:wordnet-sense-keys ("horizontal%3:00:00" "parallel%3:00:00")
 ; Antonym: ONT::VERTICAL (W::STRAIGHT W::VERTICAL W::PERPENDICULAR)
)

(define-type ont::at-an-angle-val
 :parent ont::orientation-val
 :wordnet-sense-keys ("hipped%3:00:02::" "oblique%3:00:00" "inclined%3:00:01")
)

(define-type ont::diagonal-val
 :parent ont::at-an-angle-val
 :wordnet-sense-keys ("diagonal%5:00:00:inclined:01" "diagonal%5:00:02:oblique:00")
)

(define-type ont::inverted-val
 :parent ont::orientation-val
 :wordnet-sense-keys ("inverted%5:00:00:turned:00" "turned%3:00:00")
)

(define-type ont::inside-out-val
 :parent ont::orientation-val
 :wordnet-sense-keys ("inside-out%5:00:00:turned:00" "reversed%5:00:00:turned:00")
)

(define-type ont::2D-orientation-val
 :parent ont::orientation-val
)

(define-type ont::landscape-val
 :parent ont::2D-orientation-val
)

(define-type ont::portrait-val
 :parent ont::2D-orientation-val
)

(define-type ONT::SHAPE-VAL
  :parent ONT::appearance-property-val
  :wordnet-sense-keys ("formed%3:00:00::" "rounded%3:00:00::" "curved%3:00:00::" "curving%3:00:00::" "pointed%3:00:00::" "reticulate%3:00:00::" "reticular%3:00:00::" "coiled%3:00:00::" "crosswise%3:00:00::" "uncoiled%3:00:00::" "straight%3:00:02::" "prolate%3:00:00::" "watermelon-shaped%3:00:00::" "oblate%3:00:00::" "pumpkin-shaped%3:00:00::" "crooked%3:00:01::" "concave%3:00:00::" "convex%3:00:00::" "bulging%3:00:06::" "curly%3:00:00::" "straight%3:00:03::" "azimuthal%3:01:00::" "sigmoid%3:01:01::" "hyperbolic%3:01:00::" "two-humped%3:01:00::" "double-humped%3:01:00::" "polygonal%3:01:00::" "pentangular%3:01:00::" "pentagonal%3:01:00::" "sectorial%3:01:00::" "diametral%3:01:00::" "diametric%3:01:00::" "diametrical%3:01:00::" "hemispherical%3:01:00::" "campanulate%3:01:00::" "campanular%3:01:00::" "campanulated%3:01:00::" "octangular%3:01:00::" "octagonal%3:01:00::" "radial%3:01:00::" "bicylindrical%3:01:00::" "icosahedral%3:01:00::" "rhombic%3:01:00::" "rhomboid%3:01:00::" "rhomboidal%3:01:00::" "polyhedral%3:01:00::" "asymptotic%3:01:00::" "shaped%3:01:00::" "triangulate%3:01:00::" "striate%3:01:00::" "quadratic%3:01:01::" "hexangular%3:01:00::" "hexagonal%3:01:00::" "nonspherical%3:01:00::" "angular%3:01:00::" "spherical%3:01:00::" "quadrangular%3:01:00::" "toroidal%3:01:00::" "tangential%3:01:00::" "wedge-shaped%3:01:00::" "cuneal%3:01:00::" "cuneiform%3:01:01::" "trapezoidal%3:01:00::" "tetragonal%3:01:00::" "asteriated%3:01:00::" "one-humped%3:01:00::" "single-humped%3:01:00::" "stemmatic%3:01:00::")
 :sem (F::Abstr-obj (F::Measure-function F::VALUE))
 :sem (F::abstr-obj (F::scale ont::shape-scale ))
 )

(define-type ONT::round-val
 :parent ONT::shape-VAL
;  :sem (F::Abstr-obj (F::scale ONT::roundness*1--07--00))
 :wordnet-sense-keys ("round%3:00:00")
 :sem (F::abstr-obj (F::scale ont::roundness-scale ))
 )

(define-type ont::angular-val
 :parent ont::shape-val
 :wordnet-sense-keys ("re-entrant%3:00:00::" "reentrant%3:00:00::" "angular%3:00:00")
 :sem (F::abstr-obj (F::scale ont::angularity-scale ))
)

(define-type ONT::square-val
 :parent ONT::angular-VAL
;  :sem (F::Abstr-obj (F::scale ONT::squareness*1--07--00))
 :wordnet-sense-keys ("square%3:00:00" "square-shaped%5:00:00:angular:00")
 :sem (F::abstr-obj (F::scale ont::squareness-scale ))
 )

(define-type ONT::rectangular-val
 :parent ONT::angular-VAL
 :wordnet-sense-keys ("rectangular%5:00:00:angular:00")
 :sem (F::abstr-obj (F::scale ont::rectangularity-scale ))
 )

(define-type ont::triangular-val
 :parent ONT::angular-VAL
 :wordnet-sense-keys ("three-cornered%5:00:00:angular:00" "triangular%5:00:00:angular:00")
 :sem (F::abstr-obj (F::scale ont::triangularity-scale ))
)

(define-type ONT::geometric-relationship-VAL
  :parent ONT::spatial-val
 :wordnet-sense-keys ("asymmetrical%3:00:00::" "asymmetric%3:00:00::" "symmetric%3:00:00" "perpendicular%3:00:00" "asymmetrical%5:00:00:irregular:00")
 )

(define-type ONT::associated-with-location-val
  :parent ONT::associated-with-val
  :wordnet-sense-keys ("outdoor%3:00:00::" "out-of-door%3:00:00::" "outside%3:00:04::")
  )

(define-type ONT::geo-location-val
  :parent ONT::associated-with-location-val
  :wordnet-sense-keys ("polar%3:00:00::" "continental%3:00:00::" "equatorial%3:00:00::" "intercontinental%3:00:00::" "pilosebaceous%3:01:00::" "bengali%3:01:00::" "sumerian%3:01:00::" "latvian%3:01:00::" "new_zealander%3:01:00::" "west_african%3:01:00::" "andalusian%3:01:00::" "irish%3:01:00::" "manchurian%3:01:00::" "barbadian%3:01:00::" "levantine%3:01:00::" "african%3:01:00::" "thracian%3:01:00::" "corsican%3:01:00::" "ionic%3:01:01::" "chian%3:01:00::" "ottoman%3:01:00::" "arabian%3:01:01::" "south_american%3:01:00::" "alsatian%3:01:00::" "sri_lankan%3:01:00::" "ceylonese%3:01:00::" "siberian%3:01:00::" "afro-asian%3:01:00::" "mongoloid%3:01:01::" "eurafrican%3:01:00::" "timorese%3:01:00::" "olympian%3:01:01::" "olympic%3:01:01::" "continental%3:01:00::" "north_american%3:01:00::" "lithuanian%3:01:00::" "manx%3:01:00::" "saharan%3:01:00::" "archipelagic%3:01:00::" "bahraini%3:01:00::" "insular%3:01:00::" "central_american%3:01:00::" "australian%3:01:00::" "phoenician%3:01:00::" "cappadocian%3:01:00::" "bohemian%3:01:00::" "scandinavian%3:01:02::" "norse%3:01:01::" "chaldean%3:01:00::" "chaldaean%3:01:00::" "chaldee%3:01:00::" "edwardian%3:01:00::" "nordic%3:01:00::" "european%3:01:00::" "caucasian%3:01:00::" "caucasic%3:01:00::" "korean%3:01:00::" "moravian%3:01:00::" "eurasian%3:01:00::" "eurasiatic%3:01:00::" "australasian%3:01:00::" "hindustani%3:01:00::" "semiterrestrial%3:01:00::" "hebridean%3:01:00::" "chechen%3:01:00::" "chinese%3:01:00::" "trinidadian%3:01:00::" "continental%3:01:02::" "austronesian%3:01:00::" "latin-american%3:01:00::" "jacobean%3:01:00::" "anguillan%3:01:01::" "gallic%3:01:00::" "east_indian%3:01:00::" "antiguan%3:01:01::" "polynesian%3:01:00::" "subtropical%3:01:00::" "subtropic%3:01:00::" "semitropical%3:01:00::" "semitropic%3:01:00::" "peloponnesian%3:01:00::" "transpolar%3:01:00::" "kashmiri%3:01:00::" "norman%3:01:01::" "isthmian%3:01:00::" "mongol%3:01:00::" "mongolian%3:01:01::" "iberian%3:01:00::" "lusitanian%3:01:00::" "parotid%3:01:00::" "parthian%3:01:00::" "bermudan%3:01:00::" "dalmatian%3:01:00::" "tellurian%3:01:00::" "telluric%3:01:00::" "terrestrial%3:01:00::" "terrene%3:01:00::" "peninsular%3:01:00::" "baltic%3:01:01::" "polar%3:01:00::" "prussian%3:01:00::" "middle_eastern%3:01:00::" "cyprian%3:01:00::" "cypriote%3:01:00::" "cypriot%3:01:00::" "kurdish%3:01:00::" "dumpy%3:01:00::" "east_african%3:01:00::" "asian%3:01:00::" "asiatic%3:01:00::" "appalachian%3:01:00::" "continental%3:01:01::" "sub-saharan%3:01:00::" "british%3:01:00::" "estonian%3:01:00::" "sumatran%3:01:00::" "greenside%3:01:00::" "new_caledonian%3:01:00::" "javanese%3:01:00::" "javan%3:01:00::" "georgian%3:01:06::" "provencal%3:01:00::" "north_african%3:01:00::" "tahitian%3:01:00::" "taiwanese%3:01:00::" "chinese%3:01:01::" "formosan%3:01:00::" "tobagonian%3:01:00::" "hispaniolan%3:01:00::" "american%3:01:00::" "melanesian%3:01:00::" "byzantine%3:01:00::" "czech%3:01:00::" "czechoslovakian%3:01:00::")
  )

(define-type ONT::GEO-FEATURE-VAL
 :parent ONT::geo-location-val
  :wordnet-sense-keys ("coastal%3:00:00::" "upland%3:00:00::" "highland%3:00:04::" "inland%3:00:00::" "lowland%3:00:00::" "offshore%3:00:00::" "seaward%3:00:00::")
 )

;; internal, external, inner, outer, onsite facilities
(define-type ONT::LOCATION-VAL
 :parent ONT::spatial-val
  :wordnet-sense-keys ("downstage%3:00:00::" "indoor%3:00:00::" "terminal%3:00:00::" "back%3:00:00::" "sinistral%3:00:00::" "dextral%3:00:00::" "aft%3:00:00::" "inside%3:00:00::" "outside%3:00:00::" "superjacent%3:00:00::" "outward%3:00:00::" "anterior%3:00:00::" "inner%3:00:00::" "outer%3:00:00::" "exterior%3:00:00::" "posterior%3:00:00::")
 :arguments ((:REQUIRED ONT::FIGURE ((? lof f::phys-obj f::situation f::abstr-obj))))
 )

(define-type ONT::MIDDLE-LOCATION-VAL
  :wordnet-sense-keys ("middle%5:00:01:central:01" "middle%3:00:00")
 :parent ONT::LOCATION-VAL
 )

(define-type ONT::TOP-LOCATION-VAL
  :wordnet-sense-keys ("top%3:00:00" "upper%5:00:00:top:00")
 :parent ONT::LOCATION-VAL
 )

(define-type ONT::BOTTOM-LOCATION-VAL
  :wordnet-sense-keys ("bottom%3:00:00")
 :parent ONT::LOCATION-VAL
 )

(define-type ONT::SIDE-LOCATION-VAL
  :wordnet-sense-keys ("side%3:00:00")
 :parent ONT::LOCATION-VAL
 )

;; these are cardinal directions: northern, northeastern
(define-type ONT::subarea-LOCATION-VAL
 :parent ONT::spatial-val
 )

;; inbound, outbound
(define-type ONT::DIRECTION-VAL
 :parent ONT::spatial-val
  :wordnet-sense-keys ("unidirectional%3:00:00::" "bidirectional%3:00:00::")
 )

;; the proposal is close to done; the hotel is close to an address; the reporter got close to the riot;
;; close to, near
(define-type ONT::distance-val
 :parent ONT::dimensional-property-val
 :sem (F::abstr-obj (:required (f::scale ont::distance-scale))
		    (:default (F::gradability +)))
 :arguments ((:REQUIRED ONT::neutral ((? th f::situation f::phys-obj f::abstr-obj)))
             (:ESSENTIAL ONT::neutral1 ((? cth f::situation f::phys-obj f::abstr-obj)))
;	     (:OPTIONAL ONT::PROPERTY)
             )
 )

#||
;; adjacent, contiguous  -- moved to ADJACENT
(define-type ont::connected-val
 :parent ont::spatial-val
 :sem (F::abstr-obj (:required)(:default (F::gradability +)))
 :arguments ((:REQUIRED ONT::neutral)
             (:ESSENTIAL ONT::neutral1)
;	     (:OPTIONAL ONT::PROPERTY)
             )
 )||#


(define-type ONT::LEFT
 :parent ONT::LOCATION-VAL
 ; Words: (W::LEFT W::LEFTMOST)
 :wordnet-sense-keys ("left%3:00:00" "leftmost%5:00:00:left:00")
 ; Antonym: ONT::RIGHT (W::RIGHT W::RIGHTMOST)
 )

(define-type ONT::RIGHT
 :parent ONT::LOCATION-VAL
 ; Words: (W::RIGHT W::RIGHTMOST)
 :wordnet-sense-keys ("right%3:00:03" "right%3:00:00" "rightmost%5:00:00:right:00")
 ; Antonym: ONT::LEFT (W::LEFT W::LEFTMOST)
 )

(define-type ONT::INTERNAL
 :parent ONT::LOCATION-VAL
 ; Words: (W::INTERNAL W::INNER)
 :wordnet-sense-keys ("internal%3:00:00" "inner%5:00:00:internal:00")
 ; Antonym: ONT::EXTERNAL (W::EXTERNAL W::OUTER)
 )

(define-type ONT::EXTERNAL
 :parent ONT::LOCATION-VAL
 ; Words: (W::EXTERNAL W::OUTER)
 :wordnet-sense-keys ("external%3:00:00" "outer%5:00:00:external:00")
 ; Antonym: ONT::INTERNAL (W::INTERNAL W::INNER)
 )

(define-type ONT::ON-SITE-val
 :parent ONT::LOCATION-VAL
  :wordnet-sense-keys ("on-site%3:00:00::")
)


(define-type ONT::NORTH
    :parent ONT::subarea-LOCATION-VAL
    :comment "modifiers that select a subarea of a larger area: e.g., northern Canada"
 ; Words: (W::NORTHERN W::NORTH)
 :wordnet-sense-keys ("north%3:00:00" "northerly%5:00:02:north:00" "northeastern%5:00:00:north:00" "northwestern%5:00:00:north:00")
 ; Antonym: ONT::SOUTH (W::SOUTHERN W::SOUTH)
 )

(define-type ONT::SOUTH
 :parent ONT::subarea-LOCATION-VAL
 ; Words: (W::SOUTHERN W::SOUTH)
 :wordnet-sense-keys ("southeasterly%5:00:02:south:00" "southerly%5:00:02:south:00" "southwesterly%5:00:02:south:00" "south%3:00:00")
 ; Antonym: ONT::NORTH (W::NORTHERN W::NORTH)
 )

(define-type ONT::EAST
 :parent ONT::subarea-LOCATION-VAL
 ; Words: (W::EASTERN W::EAST)
 :wordnet-sense-keys ("east%3:00:00" "eastern%5:00:00:east:00")
 ; Antonym: ONT::WEST (W::WESTERN W::WEST)
 )

(define-type ONT::WEST
 :parent ONT::subarea-LOCATION-VAL
 ; Words: (W::WESTERN W::WEST)
 :wordnet-sense-keys ("west%3:00:00" "western%5:00:00:west:00")
 ; Antonym: ONT::EAST (W::EASTERN W::EAST)
 )

(define-type ont::central-val
 :parent ont::subarea-location-val
 :wordnet-sense-keys ("central%3:00:01")
)

(define-type ONT::INCOMING
 :parent ONT::DIRECTION-VAL
 ; Words: (W::INCOMING W::INBOUND)
 :wordnet-sense-keys ("incoming%3:00:01::" "inbound%5:00:00:incoming:00" "incoming%3:00:00")
 ; Antonym: ONT::OUTGOING (W::OUTGOING W::OUTBOUND)
 )

(define-type ONT::OUTGOING
 :parent ONT::DIRECTION-VAL
 ; Words: (W::OUTGOING W::OUTBOUND)
 :wordnet-sense-keys ("exaugural%3:00:00::" "outgoing%3:00:01::" "outbound%5:00:00:outgoing:00" "outgoing%3:00:00")
 ; Antonym: ONT::INCOMING (W::INCOMING W::INBOUND)
 )

(define-type ONT::near
 :parent ONT::DISTANCE-VAL
 ; Words: (W::CLOSE W::NEAR W::NEARBY)
 :wordnet-sense-keys ("close%3:00:01::" "near%3:00:00" "close%3:00:02" "nearby%5:00:00:near:00" "approximate%5:00:00:close:02")
 ; Antonym: ONT::REMOTE (W::FAR W::REMOTE W::DISTANT W::FARTHER)
 )

(define-type ONT::REMOTE
 :parent ONT::DISTANCE-VAL
 ; Words: (W::FAR W::REMOTE W::DISTANT W::FARTHER)
 :wordnet-sense-keys ("distant%3:00:01::" "distant%5:00:02:far:00" "distant%3:00:02" "far%3:00:00" "distant%5:00:01:far:00" "farther%5:00:01:far:00")
 ; Antonym: ONT::near (W::CLOSE W::NEAR W::NEARBY)
 )

(define-type ont::local-val
 :parent ont::distance-val
 :wordnet-sense-keys ("local%3:00:01" "local%3:01:01")
)

(define-type ont::equal-distance-val
 :parent ont::distance-val
 :wordnet-sense-keys ("equidistant%5:00:00:equal:00")
)

;;
(define-type ONT::quantity-related-property-val
 :parent ONT::property-val
  :wordnet-sense-keys ("no%3:00:00::" "some%3:00:00::" "much%3:00:00::" "few%3:00:00::" "many%3:00:00::")
 :arguments ((:optional ONT::GROUND)
	     (:optional ONT::STANDARD)
             )
 :wordnet-sense-keys("quantitative%3:00:00")
 :sem (F::abstr-obj (F::scale ont::quantity-related-scale))
 )

(define-type ONT::additional-val
 :parent ONT::quantity-related-property-val
 :wordnet-sense-keys ("extra%5:00:00:additive:00" "complemental%5:00:00:additive:00" "intercalary%5:00:00:additive:00")
 )

(define-type ONT::measure-related-property-val
 :parent ONT::quantity-related-property-val
  :wordnet-sense-keys ("thermometric%3:01:00::" "monetary%3:01:00::" "pecuniary%3:01:00::" "multiphase%3:01:00::" "polyphase%3:01:00::" "anthropometric%3:01:00::" "anthropometrical%3:01:00::" "quadratic%3:01:00::" "fahrenheit%3:01:00::" "molal%3:01:00::" "non-metric%3:01:00::" "bathymetric%3:01:00::" "bathymetrical%3:01:00::" "anemometric%3:01:00::" "anemometrical%3:01:00::" "algometric%3:01:00::" "algometrical%3:01:00::" "actinometric%3:01:00::" "actinometrical%3:01:00::" "hydrometric%3:01:00::" "gravimetric%3:01:00::" "molar%3:01:02::" "igneous%3:01:01::" "pyrogenic%3:01:00::" "pyrogenous%3:01:00::" "biquadratic%3:01:00::" "mensural%3:01:01::" "probabilistic%3:01:00::" "calorimetric%3:01:00::" "caloric%3:01:02::" "noncaloric%3:01:00::" "anemographic%3:01:00::" "audiometric%3:01:00::" "cytophotometric%3:01:00::" "ohmic%3:01:00::" "molar%3:01:00::" "photometric%3:01:00::" "photometrical%3:01:00::" "normative%3:01:00::" "centigrade%3:01:00::" "telemetered%3:01:00::" "integral%3:01:00::")
 )

;; previously numerical-property-val
(define-type ONT::math-related-property-val
    :parent ONT::quantity-related-property-val
  :wordnet-sense-keys ("closed%3:00:02::" "continuous%3:00:02::" "nonlinear%3:00:00::" "direct%3:00:03::" "geometric%3:01:00::" "geometrical%3:01:00::" "rational%3:01:00::" "scalene%3:01:02::" "statistical%3:01:00::" "bivariate%3:01:00::" "binary%3:01:00::" "exponential%3:01:00::" "logarithmic%3:01:00::" "octal%3:01:00::" "topological%3:01:00::" "topologic%3:01:00::" "nonmonotonic%3:00:00::" "analytic%3:01:00::" "arithmetical%3:01:00::" "arithmetic%3:01:00::" "open%3:00:04::" "discontinuous%3:00:02::" "mathematical%3:01:00::" "polynomial%3:01:00::" "multinomial%3:01:00::" "differential%3:01:01::" "affine%3:01:00::" "isometric%3:01:00::" "irrational%3:01:00::")
    :sem (f::abstr-obj (f::gradability -))
    ;:arguments ((:REQUIRED ONT::FIGURE (f::abstr-obj (F::measure-function f::term))))
    :arguments ((:REQUIRED ONT::FIGURE ((? x f::abstr-obj f::situation) (F::measure-function f::term))))  ; f::situation: exponential increase
    :wordnet-sense-keys("algebraic%3:01:00" "trigonometric%3:01:00" "additive%3:00:00")
)

(define-type ONT::adequacy-VAL
;   :sem (F::Abstr-obj (F::scale ONT::adequacy-val))
   :parent  ONT::evaluation-attribute-val ;quantity-related-property-val
   :arguments ((:ESSENTIAL ONT::GROUND)
	       )
  ; :sem (F::abstr-obj (F::scale ont::evaluation-scale))
 )

(define-type ONT::number-related-property-val
 :parent ONT::quantity-related-property-val
  :wordnet-sense-keys ("odd%3:00:00::" "uneven%3:00:04::" "even%3:00:02::" "nilpotent%3:01:00::" "hexadecimal%3:01:00::" "hex%3:01:00::" "vicennial%3:01:00::" "prime%3:01:00::" "vigesimal%3:01:00::" "sexagesimal%3:01:00::" "millenary%3:01:00::" "digital%3:01:01::" "centesimal%3:01:00::")
 :wordnet-sense-keys("numerical%5:00:00:quantitative:00" "numerical%3:01:00" "decimal%5:00:01:quantitative:00" "duodecimal%5:00:00:quantitative:00" "vicenary%5:00:00:quantitative:00" "three-figure%5:00:00:quantitative:00")
 )

;; single, dual, lone, twin, only
(define-type ONT::CARDINALITY-VAL
 :parent ONT::number-related-property-val
  :wordnet-sense-keys ("cardinal%3:00:00::")
 )

(define-type ont::n-tuple-val
 :parent ONT::cardinality-val
 :wordnet-sense-keys("double%5:00:02:multiple:00" "double%5:00:03:multiple:00" "triple%5:00:00:multiple:00" "triple%5:00:01:multiple:00" "quadruple%5:00:02:multiple:00" "quadruple%5:00:00:multiple:00" "multiple%3:00:00")
)

(define-type ont::n-plex-val
 :parent ONT::cardinality-val
 :wordnet-sense-keys("duplex%5:00:00:multiple:00" "multiplex%5:00:00:multiple:00")
)

#|
(define-type ONT::zero-val
 :parent ONT::cardinality-val
 :wordnet-sense-keys("zero%5:00:00:cardinal:00")
 )

(define-type ONT::non-zero-val
 :parent ONT::cardinality-val
 )
|#

(define-type ONT::numerical-grouping-VAL
 :parent ONT::number-related-property-val
 :wordnet-sense-keys ("bigeminal%5:00:00:multiple:00" "triune%5:00:00:multiple:00" "quaternate%5:00:00:multiple:00")
 )

;; collective-val could easily belong to part-whole-val
(define-type ONT::collective-val
 :parent ONT::numerical-grouping-val
 :wordnet-sense-keys("aggregate%5:00:01:collective:00" "collective%3:00:00" "aggregate%5:00:00:multiple:00")
 )

;; alone, individual
(define-type ONT::Singularity-VAL
 :parent ONT::numerical-grouping-val
  :wordnet-sense-keys ("single%3:00:05::")
 :wordnet-sense-keys("singularity%1:07:01" "singular%3:00:01" "alone%5:00:00:exclusive:00" "individual%3:00:00" "alone%4:02:00")
 )

(define-type ONT::inclusive  ; co-, together
 :parent ONT::numerical-grouping-val
 )

;; duplicate
(define-type ONT::duplicate-VAL
 :parent ONT::numerical-grouping-val
 :wordnet-sense-keys("duplicate%5:00:00:matched:00")
 )

(define-type ONT::inadequate
  :parent ONT::adequacy-VAL
  :arguments ((:required ONT::GROUND (f::phys-obj (f::type ont::material)))
	      (:required ONT::FIGURE ((? xx  F::phys-obj abstr-obj))))
					; Words: (W::SHORT W::INADEQUATE W::INSUFFICIENT)
  :wordnet-sense-keys ("meager%3:00:00::" "meagre%3:00:00::" "meagerly%3:00:00::" "stingy%3:00:02::" "scrimpy%3:00:00::" "inadequate%3:00:00::" "unequal%3:00:03::" "inadequate%5:00:00:insufficient:00" "insufficient%3:00:00" "scarce%3:00:00")
					; Antonym: ONT::ADEQUATE (W::SUFFICIENT W::ADEQUATE W::ENOUGH)
  )

(define-type ONT::ADEQUATE
  :parent ONT::adequacy-VAL
					; Words: (W::SUFFICIENT W::ADEQUATE W::ENOUGH)
 :wordnet-sense-keys ("adequate%3:00:00::" "equal%3:00:03::" "sufficient%3:00:00" "adequate%5:00:00:sufficient:00")
 ; Antonym: ONT::inadequate (W::SHORT W::INADEQUATE W::INSUFFICIENT)
 )

;(define-type ONT::scarce-val
; :parent ONT::number-related-property-val
; :wordnet-sense-keys("scarce%4:02:00")
; )

(define-type ONT::related-to-cardinality-val
 :parent ONT::number-related-property-val
)

(define-type ONT::num-prefix-val
 :parent ONT::related-to-cardinality-val
)

(define-type ONT::mono-val
 :parent ONT::num-prefix-val
)

(define-type ONT::di-val
 :parent ONT::num-prefix-val
)

(define-type ONT::tri-val
 :parent ONT::num-prefix-val
)

(define-type ONT::poly-val
 :parent ONT::num-prefix-val
)
(define-type ONT::relatedness-val
  :parent ONT::relational-attribute-val
  :comment "(related)"
  )

(define-type ONT::related-val
  :parent ONT::relatedness-val
  :wordnet-sense-keys ("related%3:00:02::" "related_to%3:00:00::" "associative%3:00:00::" "associatory%3:00:00::")
  :comment "(related)"
  )

(define-type ONT::not-related-val
  :parent ONT::relatedness-val
  :wordnet-sense-keys ("unrelated%3:00:02::" "nonassociative%3:00:00::")
  :comment "(unrelated)"
  )

(define-type ONT::reciprocality-val
  :parent ONT::relational-attribute-val
  :comment "(reciprocal)"
  )

(define-type ONT::reciprocal-val
  :parent ONT::reciprocality-val
  :wordnet-sense-keys ("reciprocal%3:00:00::" "mutual%3:00:00::")
  :comment "(reciprocal)"
  )

(define-type ONT::not-reciprocal-val
  :parent ONT::reciprocality-val
  :wordnet-sense-keys ("nonreciprocal%3:00:00::")
  :comment "(nonreciprocial)"
  )

(define-type ONT::serving-as-connection-val
  :parent ONT::relational-attribute-val
  :comment "(connecting, conjunctive)"
  )

(define-type ONT::connecting-val
  :parent ONT::serving-as-connection-val
  :wordnet-sense-keys ("conjunctive%3:00:00::")
  :comment "(connecting, conjunctive)"
  )

(define-type ONT::correspondence-val
  :parent ONT::relational-attribute-val
  :comment "(commesurate, congruous)"
  )

(define-type ONT::corresponding-val
  :parent ONT::correspondence-val
  :wordnet-sense-keys ("commensurate%3:00:00::" "congruous%3:00:00::" "congruent%3:00:04::" "homologous%3:00:01::")
  :comment "(commesurate, congruous)"
  )

(define-type ONT::not-corresponding-val
  :parent ONT::correspondence-val
  :wordnet-sense-keys ("incommensurate%3:00:00::")
  :comment "(incommesurate)"
  )

(define-type ONT::dignity-val
  :parent ONT::evaluation-attribute-val
  :comment "(dignified)"
  )

(define-type ONT::dignified-val
  :parent ONT::dignity-val
  :wordnet-sense-keys ("dignified%3:00:00::")
  :comment "(dignified)"
  )

(define-type ONT::not-dignified-val
  :parent ONT::dignity-val
  :wordnet-sense-keys ("undignified%3:00:00::")
  :comment "(undignified)"
  )

(define-type ONT::shapeliness-val
  :parent ONT::evaluation-attribute-val
  :comment "(shapely)"
  )

(define-type ONT::shapely-val
  :parent ONT::shapeliness-val
  :wordnet-sense-keys ("shapely%3:00:00::")
  :comment "(shapely)"
  )

(define-type ONT::not-shapely-val
  :parent ONT::shapeliness-val
  :wordnet-sense-keys ("unshapely%3:00:00::")
  :comment "(unshapely)"
  )

(define-type ONT::not-recommendable-val
  :parent ONT::recommendability-val
  :wordnet-sense-keys ("inadvisable%3:00:00::" "unadvisable%3:00:00::")
  :comment "(unadvisable)"
  )

(define-type ONT::favorability-to-life-val
  :parent ONT::evaluation-attribute-val
  :comment "(hospitable)"
  )

(define-type ONT::hospitable-val
  :parent ONT::favorability-to-life-val
  :wordnet-sense-keys ("hospitable%3:00:00::")
  :comment "(hospitable)"
  )

(define-type ONT::not-hospitable-val
  :parent ONT::favorability-to-life-val
  :wordnet-sense-keys ("inhospitable%3:00:00::")
  :comment "(inhospitable)"
  )

(define-type ONT::restriction-val
  :parent ONT::evaluation-attribute-val
  :comment "(restricted, restrained)"
  )

(define-type ONT::restricted-val
  :parent ONT::restriction-val
  :wordnet-sense-keys ("restrained%3:00:00::" "restricted%3:00:00::" "inhibited%3:00:00::" "restrictive%3:00:00::")
  :comment "(restricted, restrained)"
  )

(define-type ONT::not-restricted-val
  :parent ONT::restriction-val
  :wordnet-sense-keys ("unrestricted%3:00:00::" "unrestrictive%3:00:00::" "unconditional%3:00:00::" "unconditioned%3:00:01::")
  :comment "(unrestricted)"
  )

(define-type ONT::qualification-val
  :parent ONT::evaluation-attribute-val
  :comment "(qualified, eligible)"
  )

(define-type ONT::qualified-val
  :parent ONT::qualification-val
  :wordnet-sense-keys ("eligible%3:00:00::" "qualified%3:00:02::" "qualified%3:00:01::")
  :comment "(qualified, eligible)"
  )

(define-type ONT::not-qualified-val
  :parent ONT::qualification-val
  :wordnet-sense-keys ("unqualified%3:00:02::" "ineligible%3:00:00::" "unqualified%3:00:01::")
  :comment "(unqualified)"
  )

(define-type ONT::helpfulness-val
  :parent ONT::evaluation-attribute-val
  :comment "(helpful)"
  )

(define-type ONT::helpful-val
  :parent ONT::helpfulness-val
  :wordnet-sense-keys ("helpful%3:00:00::" "accommodating%3:00:00::" "accommodative%3:00:00::")
  :comment "(helpful)"
  )

(define-type ONT::good-for-health-val
  :parent ONT::helpful-val
  :wordnet-sense-keys ("healthful%3:00:00::" "wholesome%3:00:00::" "sanitary%3:00:00::" "healthful%3:00:02::")
  :comment "(healthful)"
  )

(define-type ONT::not-helpful-val
  :parent ONT::helpfulness-val
  :wordnet-sense-keys ("unhelpful%3:00:00::")
  :comment "(unhelpful)"
  )

(define-type ONT::bad-for-health-val
  :parent ONT::not-helpful-val
  :wordnet-sense-keys ("unwholesome%3:00:00::" "unhealthful%3:00:00::" "unsanitary%3:00:00::" "insanitary%3:00:00::" "unhealthful%3:00:02::")
  :comment "(unhealthful)"
  )

(define-type ONT::organized-val
  :parent ONT::orderly-val
  :wordnet-sense-keys ("organized%3:00:02::" "organized%3:00:01::" "classified%3:00:01::" "structured%3:00:00::")
  :comment "having category, organization and/or structure (organized)"
  )

(define-type ONT::not-organized-val
  :parent ONT::not-orderly-val
  :wordnet-sense-keys ("disorganized%3:00:00::" "disorganised%3:00:00::" "unorganized%3:00:00::" "unorganised%3:00:00::" "unstructured%3:00:00::" "unclassified%3:00:01::" "unsystematic%3:00:00::")
  :comment "lacking category, organization and/or structure (disorganized)"
  )

(define-type ONT::control-val
  :parent ONT::evaluation-attribute-val
  :comment "controlled or controllable by a human entity (controlled, guided)"
  )

(define-type ONT::controlled-val
  :parent ONT::control-val
  :wordnet-sense-keys ("controlled%3:00:00::" "guided%3:00:00::" "manned%3:00:00::")
  :comment "controlled or controllable by a human entity (controlled, guided)"
  )

(define-type ONT::not-controlled-val
  :parent ONT::control-val
  :wordnet-sense-keys ("unrestrained%3:00:00::" "untempered%3:00:01::" "unguided%3:00:00::"
						"uncontrolled%3:00:00::")
  :comment "not controlled by human entity (uncontrolled, unguided)"
  )

(define-type ONT::inarticulate-val
  :parent ONT::unable
  :wordnet-sense-keys ("inarticulate%3:00:00::" "unarticulate%3:00:00::")
  :comment "unable to talk (inarticulate)"
  )

(define-type ONT::articulate-val
  :parent ONT::able
  :wordnet-sense-keys ("articulate%3:00:00::")
  :comment "able to talk (articulate)"
  )

(define-type ONT::permissibility-val
  :parent ONT::evaluation-attribute-val
  :comment "(permissible)"
  )

(define-type ONT::permissible-val
  :parent ONT::permissibility-val
  :wordnet-sense-keys ("permissive%3:00:02::" "permissible%3:00:00::" "allowable%3:00:00::")
  :comment "(permissible)"
  )

(define-type ONT::lawful-val
  :parent ONT::permissible-val
  :wordnet-sense-keys ("lawful%3:00:00::")
  :comment "(legal, lawful)"
  )

(define-type ONT::not-permissible-val
  :parent ONT::permissibility-val
  :wordnet-sense-keys ("impermissible%3:00:00::")
  :comment "(impermissible)"
  )

(define-type ONT::not-lawful-val
  :parent ONT::not-permissible-val
  :wordnet-sense-keys ("illegal%3:00:00::" "unlawful%3:00:00::")
  :comment "(unlawful, illegal)"
  )

(define-type ONT::influence-val
  :parent ONT::evaluation-attribute-val
  :comment "(influential)"
  )

(define-type ONT::influential-val
  :parent ONT::influence-val
  :wordnet-sense-keys ("influential%3:00:00::" "dominant%3:00:01::")
  :comment "(influential)"
  )

(define-type ONT::not-influential-val
  :parent ONT::influence-val
  :wordnet-sense-keys ("uninfluential%3:00:00::")
  :comment "(uninfluential)"
  )

(define-type ONT::influence-susceptibility-val
  :parent ONT::evaluation-attribute-val
  :comment "not allowing penetration or influence (vulnerable)"
  )

(define-type ONT::vulnerable-val
  :parent ONT::influence-susceptibility-val
  :wordnet-sense-keys ("vulnerable%3:00:00::" "susceptible%3:00:00::" "impressionable%3:00:00::" "waxy%3:00:00::" "impressible%3:00:00::" "sensitive%3:00:02::")
  :comment "not allowing penetration or influence (vulnerable)"
  )

(define-type ONT::not-vulnerable-val
  :parent ONT::influence-susceptibility-val
  :wordnet-sense-keys ("unsusceptible%3:00:00::" "insusceptible%3:00:00::" "invulnerable%3:00:00::" "impenetrable%3:00:00::" "unimpressionable%3:00:00::" "penetrable%3:00:00::" "unbreakable%3:00:00::")
  :comment "susceptible to penetration or influence (invulnerable)"
  )

(define-type ONT::not-basic-val
  :parent ONT::basicness-val
  :wordnet-sense-keys ("inessential%3:00:00::" "unessential%3:00:00::")
  :comment "(inessential)"
  )

(define-type ONT::manufacture-process-val
  :parent ONT::process-val
  :wordnet-sense-keys ("factory-made%3:00:00::" "ready-made%3:00:00::" "handmade%3:00:00::" "hand-crafted%3:00:00::" "custom-made%3:00:00::" "custom%3:00:00::" "woven%3:00:00::" "unwoven%3:00:00::")
  :comment "(handmade, factory-made)"
  )

(define-type ONT::not-productive-val
  :parent ONT::productivity-val
  :wordnet-sense-keys ("unproductive%3:00:00::")
  :comment "(unproductive)"
  )

(define-type ONT::designed-to-demand-val
  :parent ONT::task-complexity-val
  :comment "(heavy-duty)"
  )

(define-type ONT::heavy-duty-val
  :parent ONT::designed-to-demand-val
  :wordnet-sense-keys ("heavy-duty%3:00:00::")
  :comment "(heavy-duty)"
  )

(define-type ONT::light-duty-val
  :parent ONT::designed-to-demand-val
  :wordnet-sense-keys ("light%3:00:04::" "light-duty%3:00:00::")
  :comment "(light-duty)"
  )

(define-type ONT::avoidability-val
  :parent ONT::can-be-done-val
  :comment "(evitable)"
  )

(define-type ONT::avoidable-val
  :parent ONT::avoidability-val
  :wordnet-sense-keys ("evitable%3:00:00::" "avoidable%3:00:00::" "avertible%3:00:00::" "avertable%3:00:00::")
  :comment "(evitable)"
  )

(define-type ONT::not-avoidable-val
  :parent ONT::avoidability-val
  :comment "(inevitable)"
  )

(define-type ONT::measurability-val
  :parent ONT::can-be-done-val
  :comment "(measurable)"
  )

(define-type ONT::measurable-val
  :parent ONT::measurability-val
  :wordnet-sense-keys ("fathomable%3:00:00::" "plumbable%3:00:00::" "soundable%3:00:00::")
  :comment "(measurable)"
  )

(define-type ONT::not-measurable-val
  :parent ONT::measurability-val
  :comment "(immeasurable)"
  )

(define-type ONT::readability-val
  :parent ONT::can-be-done-val
  :comment "(readable, legible)"
  )

(define-type ONT::readable-val
  :parent ONT::readability-val
  :wordnet-sense-keys ("legible%3:00:00::")
  :comment "(readable, legible)"
  )

(define-type ONT::not-readable-val
  :parent ONT::readability-val
  :wordnet-sense-keys ("illegible%3:00:00::")
  :comment "(unreadable, illegible)"
  )

(define-type ONT::transferability-val
  :parent ONT::can-be-done-val
  :comment "(transferable, alienable)"
  )

(define-type ONT::transferable-val
  :parent ONT::transferability-val
  :wordnet-sense-keys ("alienable%3:00:00::")
  :comment "(transferable, alienable)"
  )

(define-type ONT::not-transferable-val
  :parent ONT::transferability-val
  :wordnet-sense-keys ("inalienable%3:00:00::" "unalienable%3:00:00::")
  :comment "(nontransferable, inalienable)"
  )

(define-type ONT::permeability-val
  :parent ONT::can-be-done-val
  :comment "(permeable, leaky)"
  )

(define-type ONT::permeable-val
  :parent ONT::permeability-val
  :wordnet-sense-keys ("leaky%3:00:00::" "permeable%3:00:00::" "pervious%3:00:00::")
  :comment "(permeable, leaky)"
  )

(define-type ONT::not-permeable-val
  :parent ONT::permeability-val
  :wordnet-sense-keys ("impervious%3:00:00::" "imperviable%3:00:00::" "tight%3:00:02::" "impermeable%3:00:00::")
  :comment "(impermeable, impervious)"
  )

(define-type ONT::changeability-val
  :parent ONT::can-be-done-val
  :comment "(modifiable, changeable)"
  )

(define-type ONT::changeable-val
  :parent ONT::changeability-val
  :wordnet-sense-keys ("variable%3:00:00::" "mutable%3:00:00::" "changeable%3:00:04::" "modifiable%3:00:00::" "unmodifiable%3:00:00::" "changeable%3:00:00::" "changeful%3:00:00::")
  :comment "(modifiable, changeable)"
  )

(define-type ONT::not-changeable-val
  :parent ONT::changeability-val
  :wordnet-sense-keys ("invariable%3:00:00::" "immutable%3:00:00::" "changeless%3:00:04::")
  :comment "(unchangeable, unmodifiable)"
  )

(define-type ONT::adaptability-val
  :parent ONT::can-be-done-val
  :comment "(adaptable)"
  )

(define-type ONT::adaptable-val
  :parent ONT::adaptability-val
  :wordnet-sense-keys ("adaptable%3:00:00::" "inelastic%3:00:00::" "flexible%3:00:02::")
  :comment "(adaptable)"
  )

(define-type ONT::not-adaptable-val
  :parent ONT::adaptability-val
  :wordnet-sense-keys ("unadaptable%3:00:00::")
  :comment "(unadaptable)"
  )

(define-type ONT::surmountability-val
  :parent ONT::can-be-done-val
  :comment "(surmountable,)"
  )

(define-type ONT::surmountable-val
  :parent ONT::surmountability-val
  :wordnet-sense-keys ("conquerable%3:00:00::" "surmountable%3:00:00::")
  :comment "(surmountable,)"
  )

(define-type ONT::not-surmountable-val
  :parent ONT::surmountability-val
  :wordnet-sense-keys ("unconquerable%3:00:00::" "insoluble%3:00:02::" "insurmountable%3:00:00::" "unsurmountable%3:00:00::" "inextricable%3:00:00::")
  :comment "(unconquerable, insoluble, insurmountable)"
  )

(define-type ONT::season-property-val
  :parent ONT::atmospheric-val
  :wordnet-sense-keys ("summery%3:00:00::" "wintry%3:00:00::" "wintery%3:00:00::" "autumnal%3:00:00::")
  :comment "(summery, wintry)"
  )

(define-type ONT::atmospheric-phenomenon-val
  :parent ONT::atmospheric-val
  :wordnet-sense-keys ("auroral%3:01:02::" "boreal%3:01:01::" "cyclonic%3:01:00::" "cyclonal%3:01:00::" "cyclonical%3:01:00::" "greenhouse%3:01:00::" "diluvian%3:01:00::" "diluvial%3:01:00::" "elemental%3:01:02::")
  :comment "(boreal, auroral, cyclonic)"
  )

(define-type ONT::not-healthy-val
  :parent ONT::negative-body-condition-property-val
  :wordnet-sense-keys ("unhealthy%3:00:00::")
  :comment "(unhealthy)"
  )

(define-type ONT::injured-val
  :parent ONT::ailing-val
  :wordnet-sense-keys ("injured%3:00:00::")
  :comment "(injured)"
  )

(define-type ONT::unfit-val
  :parent ONT::ailing-val
  :wordnet-sense-keys ("unfit%3:00:01::")
  :comment "(unfit)"
  )

(define-type ONT::not-alert-val
  :parent ONT::negative-body-condition-property-val
  :wordnet-sense-keys ("unwary%3:00:00::")
  :comment "(unwary)"
  )

(define-type ONT::starved-val
  :parent ONT::not-satiated-val
  :wordnet-sense-keys ("malnourished%3:00:00::")
  :comment "(starved)"
  )

(define-type ONT::not-injured-val
  :parent ONT::not-ailing-val
  :wordnet-sense-keys ("uninjured%3:00:00::")
  :comment "(uninjured)"
  )

(define-type ONT::painless-val
  :parent ONT::positive-body-condition-property-val
  :wordnet-sense-keys ("painless%3:00:00::")
  :comment "(painless)"
  )

(define-type ONT::disease-infection-val
  :parent ONT::medical-condition-property-val
  :comment "(septic)"
  )

(define-type ONT::diseased-val
  :parent ONT::disease-infection-val
  :wordnet-sense-keys ("septic%3:00:00::" "infected%3:00:00::")
  :comment "(septic)"
  )

(define-type ONT::infectious-val
  :parent ONT::diseased-val
  :wordnet-sense-keys ("infectious%3:00:00::" "virulent%3:00:00::" "invasive%3:00:00::")
  :comment "(infectious, invasive)"
  )

(define-type ONT::not-infectious-val
  :parent ONT::diseased-val
  :wordnet-sense-keys ("noninfectious%3:00:00::")
  :comment "(noninfectious)"
  )

(define-type ONT::not-diseased-val
  :parent ONT::disease-infection-val
  :wordnet-sense-keys ("antiseptic%3:00:00::")
  :comment "(antiseptic)"
  )

(define-type ONT::disease-property-val
  :parent ONT::disease-infection-val
  :wordnet-sense-keys ("tubercular%3:01:00::" "acidotic%3:01:00::" "syphilitic%3:01:00::" "myopathic%3:01:00::" "anticancer%3:01:00::" "antineoplastic%3:01:00::" "antitumor%3:01:00::" "antitumour%3:01:00::" "anginal%3:01:00::" "anginose%3:01:00::" "anginous%3:01:00::" "paralytic%3:01:00::" "paralytical%3:01:00::" "haemophilic%3:01:00::" "hemophilic%3:01:00::" "carcinogenic%3:01:00::" "pyogenic%3:01:00::" "varicelliform%3:01:00::" "acneiform%3:01:00::" "cachectic%3:01:00::" "chancrous%3:01:00::" "thyrotoxic%3:01:00::" "vitiliginous%3:01:00::" "morbilliform%3:01:00::" "arteriosclerotic%3:01:00::" "zoonotic%3:01:00::" "variolar%3:01:00::" "variolic%3:01:00::" "variolous%3:01:00::" "emphysematous%3:01:00::" "pyemic%3:01:00::" "pyaemic%3:01:00::" "leprous%3:01:00::" "bacteremic%3:01:00::" "encysted%3:01:00::" "pyknotic%3:01:00::" "pycnotic%3:01:00::" "ulcerative%3:01:00::" "asynergic%3:01:00::" "tetanic%3:01:00::" "excrescent%3:01:00::" "celiac%3:01:01::" "hemorrhagic%3:01:00::" "haemorrhagic%3:01:00::" "anuretic%3:01:00::" "anuric%3:01:00::" "malarial%3:01:00::" "actinomycotic%3:01:00::" "pneumonic%3:01:01::" "blastomycotic%3:01:00::" "angiomatous%3:01:00::" "sclerotic%3:01:01::" "sclerosed%3:01:00::" "cystic%3:01:00::" "azotemic%3:01:00::" "uremic%3:01:00::" "uraemic%3:01:00::" "acanthotic%3:01:00::" "traumatic%3:01:00::" "anoxemic%3:01:00::" "hypovolemic%3:01:00::" "hypovolaemic%3:01:00::" "ascitic%3:01:00::" "scorbutic%3:01:00::" "avitaminotic%3:01:00::" "rabid%3:01:00::" "ankylotic%3:01:00::" "croupy%3:01:00::" "precancerous%3:01:00::" "idiopathic%3:01:00::" "pemphigous%3:01:00::" "neoplastic%3:01:00::" "adenocarcinomatous%3:01:00::" "aneurysmal%3:01:00::" "aneurismal%3:01:00::" "aneurysmatic%3:01:00::" "aneurismatic%3:01:00::" "choleraic%3:01:00::" "tuberculoid%3:01:00::" "nephritic%3:01:01::" "chlorotic%3:01:00::" "greensick%3:01:00::" "anemic%3:01:00::" "anaemic%3:01:00::" "impetiginous%3:01:00::" "ischemic%3:01:00::" "ischaemic%3:01:00::" "infectious%3:01:00::" "tubercular%3:01:02::" "agranulocytic%3:01:00::" "cancroid%3:01:00::" "erythematous%3:01:00::" "carcinomatous%3:01:00::" "alkalotic%3:01:00::")
  )

(define-type ONT::united-val
  :parent ONT::part-whole-val
  :wordnet-sense-keys ("united%3:00:00::" "combined%3:00:00::" "joint%3:00:00::" "integrated%3:00:02::" "integrative%3:00:00::" "integrated%3:00:00::" "convergent%3:00:00::")
  :comment "(integrated, united)"
  )

(define-type ONT::having-constituent-parts-val
  :parent ONT::physical-property-val
  )

(define-type ONT::having-component-or-part-val
  :parent ONT::having-constituent-parts-val
  )

(define-type ONT::lacking-component-or-part-val
  :parent ONT::having-constituent-parts-val
  )

(define-type ONT::resembling-val
  :parent ONT::having-constituent-parts-val
  )

(define-type ONT::having-human-constituents-val
  :parent ONT::having-constituent-parts-val
  :wordnet-sense-keys ("patronized%3:00:00::" "patronised%3:00:00::")
  )

(define-type ONT::containing-substance-val
  :parent ONT::substantial-property-val
  :wordnet-sense-keys ("metallic%3:00:00::" "metal%3:00:00::" "crystalline%3:00:00::" "noncrystalline%3:00:00::" "unleaded%3:00:00::" "leadless%3:00:00::" "leaded%3:00:00::" "starchy%3:00:00::" "nonmetallic%3:00:00::" "nonmetal%3:00:00::" "alcoholic%3:00:00::" "wet%3:00:04::" "vegetal%3:00:00::" "vegetational%3:00:00::" "vegetative%3:00:00::")
  :comment "(crystalline, alcoholic)"
  )

(define-type ONT::not-containing-substance-val
  :parent ONT::substantial-property-val
  :wordnet-sense-keys ("rustless%3:00:00::" "nonalcoholic%3:00:00::")
  :comment "(rustless)"
  )

(define-type ONT::liquid-property-val
  :parent ONT::substantial-property-val
  :wordnet-sense-keys ("effervescent%3:00:00::" "noneffervescent%3:00:00::" "still%3:00:00::" "noneffervescent%3:00:04::" "sparkling%3:00:00::" "effervescent%3:00:04::")
  :comment "(effervescent)"
  )

(define-type ONT::information-delivery-mode-val
  :parent ONT::mode
  :comment "(written)"
  )

(define-type ONT::written-val
  :parent ONT::information-delivery-mode-val
  :wordnet-sense-keys ("written%3:00:00::" "written%3:00:02::" "scripted%3:00:00::" "written%3:00:04::")
  :comment "(written)"
  )

(define-type ONT::not-written-val
  :parent ONT::information-delivery-mode-val
  :wordnet-sense-keys ("unwritten%3:00:00::")
  :comment "(unwritten)"
  )

(define-type ONT::oral-mode-val
  :parent ONT::not-written-val
  :wordnet-sense-keys ("spoken%3:00:00::")
  :comment "(spoken)"
  )

(define-type ONT::representation-method-val
  :parent ONT::mode
  :wordnet-sense-keys ("anagogic%3:01:00::" "anagogical%3:01:00::" "radiographic%3:01:00::" "roentgenographic%3:01:00::" "antitypic%3:01:00::" "antitypical%3:01:00::" "scenic%3:01:00::" "xerographic%3:01:00::" "paradigmatic%3:01:00::" "concretistic%3:01:00::" "radiological%3:01:00::" "microcosmic%3:01:00::" "photographic%3:01:00::" "anamorphic%3:01:01::")
  )

(define-type ONT::sensitive-val
  :parent ONT::sensitivity-val
  :wordnet-sense-keys ("perceptive%3:00:00::")
  :comment "(sensitive, perceptive)"
  )

(define-type ONT::not-sensitive-val
  :parent ONT::sensitivity-val
  :wordnet-sense-keys ("unperceptive%3:00:00::" "unperceiving%3:00:00::" "insensible%3:00:00::" "scentless%3:00:00::" "insensitive%3:00:01::")
  :comment "(insensitive)"
  )

(define-type ONT::seeing-val
  :parent ONT::visibility-val
  :wordnet-sense-keys ("sighted%3:00:00::" "farsighted%3:00:00::" "presbyopic%3:00:00::" "nearsighted%3:00:00::" "shortsighted%3:00:00::" "myopic%3:00:00::")
  :comment "(sighted)"
  )

(define-type ONT::extra-sensory-property-val
  :parent ONT::sensory-property-val
  :wordnet-sense-keys ("extrasensory%3:00:00::" "paranormal%3:00:02::" "prophetic%3:00:00::" "prophetical%3:00:00::" "unprophetic%3:00:00::")
  :comment "(extrasensory)"
  )

(define-type ONT::sharp-texture-val
  :parent ONT::texture-val
  :comment "(sharp)"
  )

(define-type ONT::sharp-val
  :parent ONT::sharp-texture-val
  :wordnet-sense-keys ("sharp%3:00:00::")
  :comment "(sharp)"
  )

(define-type ONT::dull-val
  :parent ONT::sharp-texture-val
  :wordnet-sense-keys ("dull%3:00:01::")
  :comment "(dull)"
  )

(define-type ONT::slippery-val
  :parent ONT::texture-val
  :wordnet-sense-keys ("slippery%3:00:00::" "slippy%3:00:00::")
  :comment "(slippery)"
  )

(define-type ONT::sided-val
  :parent ONT::shape-val
  :wordnet-sense-keys ("multilateral%3:00:00::" "many-sided%3:00:04::")
  :comment "(multilateral)"
  )

(define-type ONT::no-shape-val
  :parent ONT::shape-val
  :wordnet-sense-keys ("unformed%3:00:00::")
  :comment "(unformed)"
  )

(define-type ONT::pattern-val
  :parent ONT::visual-property-val
  :wordnet-sense-keys ("patterned%3:00:00::")
  :comment "(patterned)"
  )

(define-type ONT::no-color-val
  :parent ONT::color-val
  :wordnet-sense-keys ("colorless%3:00:03::" "colourless%3:00:03::" "colorless%3:00:02::" "colourless%3:00:02::" "uncolored%3:00:00::" "uncoloured%3:00:00::" "unpigmented%3:01:00::" "black-and-white%3:01:00::")
  :comment "(colorless)"
  )

(define-type ONT::light-property-val
  :parent ONT::visual-property-val
  :wordnet-sense-keys ("hard%3:00:05::" "concentrated%3:00:01::" "soft%3:00:05::" "diffuse%3:00:00::" "diffused%3:00:00::")
  )

(define-type ONT::partially-filled-val
  :parent ONT::filled-val
  :wordnet-sense-keys ("uncrowded%3:00:00::")
  :comment "(uncrowded)"
  )

(define-type ONT::enclosure-val
  :parent ONT::configuration-property-val
  :comment "(enclosed)"
  )

(define-type ONT::enclosed-val
  :parent ONT::enclosure-val
  :wordnet-sense-keys ("enclosed%3:00:00::")
  :comment "(enclosed)"
  )

(define-type ONT::not-enclosed-val
  :parent ONT::enclosure-val
  :wordnet-sense-keys ("unenclosed%3:00:00::")
  :comment "(unenclosed)"
  )

(define-type ONT::closure-val
  :parent ONT::configuration-property-val
  :comment "(open)"
  )

(define-type ONT::open-val
  :parent ONT::closure-val
  :wordnet-sense-keys ("open%3:00:08::" "opened%3:00:02::" "open%3:00:01::" "unfastened%3:00:04::" "unsealed%3:00:01::")
  :comment "(open)"
  )

(define-type ONT::closed-val
  :parent ONT::closure-val
  :wordnet-sense-keys ("closed%3:00:03::" "shut%3:00:02::" "shuttered%3:00:00::" "shut%3:00:00::" "unopen%3:00:04::" "closed%3:00:04::")
  :comment "(closed)"
  )

(define-type ONT::dispersion-val
  :parent ONT::configuration-property-val
  :comment "(concentrated, saturated)"
  )

(define-type ONT::concentrated-val
  :parent ONT::dispersion-val
  :wordnet-sense-keys ("concentrated%3:00:00::" "saturated%3:00:01::" "concentrated%3:00:02::")
  :comment "(concentrated, saturated)"
  )

(define-type ONT::dispersed-val
  :parent ONT::dispersion-val
  :wordnet-sense-keys ("distributed%3:00:00::" "unsaturated%3:00:01::")
  :comment "(distributed)"
  )

(define-type ONT::ordinality-val
  :parent ONT::number-related-property-val
  :wordnet-sense-keys ("ordinal%3:00:00::")
  :comment "(ordinal)"
  )

(define-type ONT::abundant-val
  :parent ONT::adequacy-val
  :wordnet-sense-keys ("abundant%3:00:00::" "ample%3:00:00::")
  :comment "(abundant)"
  )

(define-type ONT::quantifier-val
  :parent ONT::quantity-related-property-val
  )

(define-type ONT::damaged-val
  :parent ONT::functionality-val
  :wordnet-sense-keys ("damaged%3:00:00::" "worn%3:00:00::" "unsound%3:00:01::")
  :comment "(damaged)"
  )

(define-type ONT::not-damaged-val
  :parent ONT::functionality-val
  :wordnet-sense-keys ("sound%3:00:01::" "undamaged%3:00:00::" "unimpaired%3:00:00::")
  :comment "(sound, undamaged)"
  )

(define-type ONT::not-having-function-val
  :parent ONT::functionality-val
  :wordnet-sense-keys ("nonfunctional%3:00:00::")
  :comment "(nonfunctional)"
  )

(define-type ONT::evoking-neutral-experience-property-val
  :parent ONT::evoking-experience-property-val
  :comment "(impressive)"
  )

(define-type ONT::awe-inspiration-val
  :parent ONT::evoking-neutral-experience-property-val
  :comment "(impressive)"
  )

(define-type ONT::inspiring-awe-val
  :parent ONT::awe-inspiration-val
  :wordnet-sense-keys ("impressive%3:00:00::")
  :comment "(impressive)"
  )

(define-type ONT::not-inspiring-awe-val
  :parent ONT::awe-inspiration-val
  :wordnet-sense-keys ("unimpressive%3:00:00::" "uninspiring%3:00:00::")
  :comment "(unimpressivle)"
  )

(define-type ONT::attractiveness-val
  :parent ONT::evoking-neutral-experience-property-val
  :comment "(attractive)"
  )

(define-type ONT::attractive-val
  :parent ONT::attractiveness-val
  :wordnet-sense-keys ("sexy%3:00:00::" "attractive%3:00:01::" "seductive%3:00:00::" "inviting%3:00:00::" "irresistible%3:00:00::" "resistless%3:00:04::" "appetizing%3:00:00::" "appetising%3:00:00::")
  :comment "(attractive)"
  )

(define-type ONT::not-attractive-val
  :parent ONT::attractiveness-val
  :wordnet-sense-keys ("unattractive%3:00:00::" "unsexy%3:00:00::" "unseductive%3:00:00::" "uninviting%3:00:00::")
  :comment "(unattractive)"
  )

(define-type ONT::provoking-excitement-val
  :parent ONT::evoking-neutral-experience-property-val
  :comment "(exciting, hot, stimulating)"
  )

(define-type ONT::provocative-val
  :parent ONT::provoking-excitement-val
  :wordnet-sense-keys ("stimulating%3:00:00::" "provocative%3:00:00::" "inspiring%3:00:00::" "exciting%3:00:00::" "hot%3:00:02::" "sensational%3:00:00::")
  :comment "(exciting, hot, stimulating)"
  )

(define-type ONT::not-provocative-val
  :parent ONT::provoking-excitement-val
  :wordnet-sense-keys ("unstimulating%3:00:00::" "unexciting%3:00:04::" "unprovocative%3:00:00::" "unprovoking%3:00:04::" "unexciting%3:00:00::")
  :comment "(unexciting)"
  )

(define-type ONT::emotionality-val
  :parent ONT::evoking-neutral-experience-property-val
  :comment "(emotional)"
  )

(define-type ONT::emotional-val
  :parent ONT::emotionality-val
  :wordnet-sense-keys ("emotional%3:00:00::" "moving%3:00:01::")
  :comment "(emotional)"
  )

(define-type ONT::not-emotional-val
  :parent ONT::emotionality-val
  :wordnet-sense-keys ("unmoving%3:00:00::" "unemotional%3:00:00::")
  :comment "(unemotional)"
  )

(define-type ONT::evoking-surprise-val
  :parent ONT::evoking-neutral-experience-property-val
  :comment "(surprising)"
  )

(define-type ONT::surprising-val
  :parent ONT::evoking-surprise-val
  :wordnet-sense-keys ("surprising%3:00:00::")
  :comment "(surprising)"
  )

(define-type ONT::not-surprising-val
  :parent ONT::evoking-surprise-val
  :wordnet-sense-keys ("unsurprising%3:00:00::")
  :comment "(unsurprising)"
  )

(define-type ONT::not-desirable-val
  :parent ONT::evoking-neg-experience-property-val
  :wordnet-sense-keys ("unwanted%3:00:00::" "undesirable%3:00:00::" "unwanted%3:00:01::")
  :comment "(undesirable)"
  )

(define-type ONT::discouraging-val
  :parent ONT::evoking-neg-experience-property-val
  :wordnet-sense-keys ("discouraging%3:00:00::" "dissuasive%3:00:00::")
  :comment "(discouraging)"
  )

(define-type ONT::depressing-val
  :parent ONT::evoking-neg-experience-property-val
  :wordnet-sense-keys ("depressing%3:00:00::" "cheerless%3:00:00::" "uncheerful%3:00:04::")
  :comment "(depressing)"
  )

(define-type ONT::peaceful-val
  :parent ONT::evoking-pos-experience-property-val
  :wordnet-sense-keys ("peaceful%3:00:00::" "peaceable%3:00:00::" "conciliatory%3:00:00::" "conciliative%3:00:00::")
  :comment "(peaceful)"
  )

(define-type ONT::imparting-energy-val
  :parent ONT::evoking-pos-experience-property-val
  :wordnet-sense-keys ("invigorating%3:00:00::")
  :comment "(invigorating)"
  )

(define-type ONT::encouraging-val
  :parent ONT::evoking-pos-experience-property-val
  :wordnet-sense-keys ("encouraging%3:00:00::" "edifying%3:00:00::" "enlightening%3:00:04::")
  :comment "(encouraging)"
  )

(define-type ONT::lovable-val
  :parent ONT::evoking-pos-experience-property-val
  :wordnet-sense-keys ("lovable%3:00:00::" "loveable%3:00:00::")
  :comment "(lovable)"
  )

(define-type ONT::remorseless-val
  :parent ONT::neg-experiencer-property-val
  :wordnet-sense-keys ("unashamed%3:00:00::" "unregretful%3:00:00::" "unregretting%3:00:00::" "unapologetic%3:00:00::")
  :comment "(unapologetic, unregretful)"
  )

(define-type ONT::hopeless-val
  :parent ONT::neg-experiencer-property-val
  :wordnet-sense-keys ("hopeless%3:00:00::")
  :comment "(hopeless)"
  )

(define-type ONT::not-liked-val
  :parent ONT::neg-experiencer-property-val
  :wordnet-sense-keys ("unloved%3:00:00::" "disliked%3:00:00::")
  :comment "(disliked, unloved)"
  )

(define-type ONT::hateful-val
  :parent ONT::neg-experiencer-property-val
  :wordnet-sense-keys ("hateful%3:00:00::" "hatred%1:12:00" "evil%1:07:00")
  :comment "(hateful)"
  )

(define-type ONT::affected-val
  :parent ONT::neutral-experiencer-property-val
  :wordnet-sense-keys ("affected%3:00:00::" "awed%3:00:00::" "awestruck%3:00:00::" "awestricken%3:00:00::")
  :comment "(affected)"
  )

(define-type ONT::unaffected-val
  :parent ONT::neutral-experiencer-property-val
  :wordnet-sense-keys ("passionless%3:00:00::" "unenthusiastic%3:00:00::" "unawed%3:00:00::" "undemonstrative%3:00:00::" "unresentful%3:00:00::" "unmoved%3:00:00::" "unaffected%3:00:02::" "untouched%3:00:01::" "unangry%3:00:00::" "unrevived%3:00:00::" "unrenewed%3:00:00::" "unagitated%3:00:00::" "unexcited%3:00:00::" "unsurprised%3:00:00::" "not_surprised%3:00:00::" "unafraid%3:00:00::" "fearless%3:00:00::")
  :comment "(passionless, unawed)"
  )

(define-type ONT::not-concerned-val
  :parent ONT::unaffected-val
  :wordnet-sense-keys ("untroubled%3:00:00::" "unconcerned%3:00:00::")
  :comment "(untroubled, unconcerned)"
  )

(define-type ONT::enchanted-val
  :parent ONT::neutral-experiencer-property-val
  :wordnet-sense-keys ("enchanted%3:00:00::")
  :comment "(enchanted)"
  )

(define-type ONT::not-desirous-val
  :parent ONT::neutral-experiencer-property-val
  :wordnet-sense-keys ("undesirous%3:00:00::" "undesiring%3:00:00::")
  :comment "(undesirous)"
  )

(define-type ONT::liked-val
  :parent ONT::pos-experiencer-property-val
  :wordnet-sense-keys ("loved%3:00:00::" "liked%3:00:00::")
  :comment "(liked, loved)"
  )

(define-type ONT::hopeful-val
  :parent ONT::pos-experiencer-property-val
  :wordnet-sense-keys ("hopeful%3:00:00::")
  :comment "(hopeful)"
  )

(define-type ONT::revived-val
  :parent ONT::pos-experiencer-property-val
  :wordnet-sense-keys ("revived%3:00:00::" "rested%3:00:00::" "enlivened%3:00:00::")
  :comment "(revived)"
  )

(define-type ONT::altering-consciousness-val
  :parent ONT::not-sensible-val
  :wordnet-sense-keys ("intoxicated%3:00:00::" "drunk%3:00:00::" "inebriated%3:00:00::" "psychoactive%3:00:00::" "psychotropic%3:00:00::")
  :comment "(intoxicated)"
  )

(define-type ONT::mentally-slow-val
  :parent ONT::intelligence-val
  :wordnet-sense-keys ("retarded%3:00:00::")
  :comment "(retarded)"
  )

(define-type ONT::not-clever-val
  :parent ONT::stupid
  :wordnet-sense-keys ("impolitic%3:00:00::")
  :comment "(impolitic)"
  )

(define-type ONT::willing-to-accept-val
  :parent ONT::willing
  :wordnet-sense-keys ("receptive%3:00:00::" "open%3:00:00::" "broad-minded%3:00:00::" "tolerant%3:00:00::")
  :comment "(receptive, broad-minded)"
  )

(define-type ONT::not-willing-to-accept-val
  :parent ONT::unwilling
  :wordnet-sense-keys ("intolerant%3:00:00::" "unreceptive%3:00:00::")
  :comment "(intolerant, unreceptive)"
  )

(define-type ONT::creativity-val
  :parent ONT::psychological-property-val
  :comment "(creative)"
  )

(define-type ONT::creative-val
  :parent ONT::creativity-val
  :wordnet-sense-keys ("creative%3:00:00::" "originative%3:00:00::")
  :comment "(creative)"
  )

(define-type ONT::not-creative-val
  :parent ONT::creativity-val
  :wordnet-sense-keys ("unoriginal%3:00:00::" "uncreative%3:00:00::")
  :comment "(uncreative)"
  )

(define-type ONT::intellect-val
  :parent ONT::psychological-property-val
  :comment "(intellectual)"
  )

(define-type ONT::intellectual-val
  :parent ONT::intellect-val
  :wordnet-sense-keys ("intellectual%3:00:00::")
  :comment "(intellectual)"
  )

(define-type ONT::not-intellectual-val
  :parent ONT::intellect-val
  :wordnet-sense-keys ("nonintellectual%3:00:00::")
  :comment "(nonintellectual)"
  )

(define-type ONT::reservation-val
  :parent ONT::status-val
  :comment "(reserved, booked)"
  )

(define-type ONT::reserved-val
  :parent ONT::reservation-val
  :wordnet-sense-keys ("reserved%3:00:02::")
  :comment "(reserved, booked)"
  )

(define-type ONT::not-reserved-val
  :parent ONT::reservation-val
  :wordnet-sense-keys ("unreserved%3:00:02::")
  :comment "(unreserved)"
  )

(define-type ONT::native-val
  :parent ONT::origin-related-val
  :wordnet-sense-keys ("native%3:00:01::" "native%3:00:03::" "native%3:00:02::" "aboriginal%3:00:00::")
  :comment "(native)"
  )

(define-type ONT::employment-status-val
  :parent ONT::status-val
  :wordnet-sense-keys ("part-time%3:00:00::" "parttime%3:00:00::" "employed%3:00:00::" "nonunion%3:00:00::" "unemployed%3:00:00::" "union%3:00:00::" "inactive%3:00:08::" "active%3:00:08::" "full-time%3:00:00::" "salaried%3:00:00::")
  :comment "(blue-collar)"
  )

(define-type ONT::employment-class-val
  :parent ONT::employment-status-val
  :wordnet-sense-keys ("blue-collar%3:00:00::" "white-collar%3:00:00::")
  :comment "(blue-collar)"
  )

(define-type ONT::international-val
  :parent ONT::country-related-val
  :wordnet-sense-keys ("international%3:00:00::")
  :comment "(international)"
  )

(define-type ONT::not-secret-val
  :parent ONT::secrecy-val
  :wordnet-sense-keys ("overt%3:00:00::" "open%3:00:07::" "unveiled%3:00:00::")
  :comment "(overt)"
  )

(define-type ONT::legal-culpability-val
  :parent ONT::status-val
  :comment "(guilty)"
  )

(define-type ONT::culpable-val
  :parent ONT::legal-culpability-val
  :wordnet-sense-keys ("guilty%3:00:00::")
  :comment "(guilty)"
  )

(define-type ONT::not-culpable-val
  :parent ONT::legal-culpability-val
  :wordnet-sense-keys ("innocent%3:00:00::" "guiltless%3:00:00::" "clean-handed%3:00:00::")
  :comment "(innocent)"
  )

(define-type ONT::sacredness-val
  :parent ONT::status-val
  :comment "(sacred)"
  )

(define-type ONT::sacred-val
  :parent ONT::sacredness-val
  :wordnet-sense-keys ("sacred%3:00:00::" "holy%3:00:00::" "consecrated%3:00:00::" "consecrate%3:00:00::" "dedicated%3:00:02::")
  :comment "(sacred)"
  )

(define-type ONT::not-sacred-val
  :parent ONT::sacredness-val
  :wordnet-sense-keys ("unholy%3:00:00::" "unhallowed%3:00:00::" "desecrated%3:00:00::" "profane%3:00:00::" "secular%3:00:05::")
  :comment "(profane)"
  )

(define-type ONT::social-relation-val
  :parent ONT::status-val
  :comment "(related)"
  )

(define-type ONT::related-socially-val
  :parent ONT::social-relation-val
  :wordnet-sense-keys ("related%3:00:01::")
  :comment "(related)"
  )

(define-type ONT::not-related-socially-val
  :parent ONT::social-relation-val
  :wordnet-sense-keys ("unrelated%3:00:01::")
  :comment "(unrelated)"
  )

(define-type ONT::official-authority-val
  :parent ONT::status-val
  :comment "(official, authorized)"
  )

(define-type ONT::official-val
  :parent ONT::official-authority-val
  :wordnet-sense-keys ("official%3:00:00::" "legal%3:00:00::" "certified%3:00:00::" "documented%3:00:00::" "registered%3:00:00::" "authorized%3:00:00::" "authorised%3:00:00::" "unauthorized%3:00:00::" "unauthorised%3:00:00::")
  :comment "(official, authorized)"
  )

(define-type ONT::not-official-val
  :parent ONT::official-authority-val
  :wordnet-sense-keys ("unofficial%3:00:00::" "unregistered%3:00:00::" "undocumented%3:00:00::" "uncertified%3:00:00::")
  :comment "(unofficial)"
  )

(define-type ONT::open-acknowledgement-val
  :parent ONT::status-val
  :comment "openly and publicly acknowledged (confirmed)"
  )

(define-type ONT::acknowledged-val
  :parent ONT::open-acknowledgement-val
  :wordnet-sense-keys ("established%3:00:00::" "constituted%3:00:00::" "declared%3:00:00::" "acknowledged%3:00:00::" "legitimate%3:00:00::" "confirmed%3:00:00::")
  :comment "openly and publicly acknowledged (confirmed)"
  )

(define-type ONT::not-acknowledged-val
  :parent ONT::open-acknowledgement-val
  :wordnet-sense-keys ("illegitimate%3:00:00::" "unacknowledged%3:00:00::" "undeclared%3:00:00::" "unestablished%3:00:00::" "unconfirmed%3:00:00::")
  :comment "not acknowledged openly nor publicly (unconfirmed)"
  )

(define-type ONT::marriage-val
  :parent ONT::status-val
  :comment "(married)"
  )

(define-type ONT::married-val
  :parent ONT::marriage-val
  :wordnet-sense-keys ("married%3:00:00::")
  :comment "(married)"
  )

(define-type ONT::not-married-val
  :parent ONT::marriage-val
  :wordnet-sense-keys ("unmarried%3:00:00::" "single%3:00:02::")
  :comment "(unmarried, single)"
  )

(define-type ONT::protected-property-val
  :parent ONT::status-val
  :comment "(proprietary)"
  )

(define-type ONT::proprietary-val
  :parent ONT::protected-property-val
  :wordnet-sense-keys ("proprietary%3:00:00::")
  :comment "(proprietary)"
  )

(define-type ONT::not-proprietary-val
  :parent ONT::protected-property-val
  :wordnet-sense-keys ("nonproprietary%3:00:00::")
  :comment "(nonproprietary)"
  )

(define-type ONT::forward-val
  :parent ONT::location-val
  :wordnet-sense-keys ("forward%3:00:01::" "forward%3:00:03::")
  :comment "(forward)"
  )

(define-type ONT::backward-val
  :parent ONT::location-val
  :wordnet-sense-keys ("backward%3:00:01::")
  :comment "(backward)"
  )

(define-type ONT::toward-center-val
  :parent ONT::location-val
  :wordnet-sense-keys ("centripetal%3:00:00::" "centrifugal%3:00:00::" "centralized%3:00:00::" "centralised%3:00:00::")
  :comment "(centralizing)"
  )

(define-type ONT::away-from-center-val
  :parent ONT::location-val
  :wordnet-sense-keys ("decentralized%3:00:00::" "decentralised%3:00:00::" "decentralizing%3:00:00::" "decentralising%3:00:00::")
  :comment "(decentralizing)"
  )

(define-type ONT::dimensionality-val
  :parent ONT::spatial-val
  :wordnet-sense-keys ("multidimensional%3:00:00::" "cubic%3:00:00::" "three-dimensional%3:00:02::" "dimensional%3:01:00::" "planar%3:00:00::" "two-dimensional%3:00:02::" "linear%3:00:01::" "one-dimensional%3:00:00::" "unidimensional%3:00:00::" "one-dimensional%3:00:02::")
  :comment "(unidimensional)"
  )

(define-type ONT::centric-val
  :parent ONT::spatial-arrangement-val
  :wordnet-sense-keys ("eccentric%3:00:00::" "nonconcentric%3:00:00::" "heliocentric%3:00:00::" "geocentric%3:00:00::")
  :comment "(centric)"
  )

(define-type ONT::coordinate-system-val
  :parent ONT::spatial-val
  :wordnet-sense-keys ("latitudinal%3:01:00::" "longitudinal%3:01:00::" "altitudinal%3:01:00::")
  :comment "(longitudinal)"
  )

(define-type ONT::sincere-val
  :parent ONT::honest-val
  :wordnet-sense-keys ("unpretentious%3:00:00::" "ingenuous%3:00:00::" "artless%3:00:02::" "sincere%3:00:00::")
  :comment "(sincere)"
  )

(define-type ONT::earnest-val
  :parent ONT::honest-val
  :wordnet-sense-keys ("serious%3:00:00::")
  :comment "(earnest, serious)"
  )

(define-type ONT::decisiveness-val
  :parent ONT::animal-propensity-val
  :comment "(decisive)"
  )

(define-type ONT::decisive-val
  :parent ONT::decisiveness-val
  :wordnet-sense-keys ("resolute%3:00:00::")
  :comment "(decisive)"
  )

(define-type ONT::not-decisive-val
  :parent ONT::decisiveness-val
  :wordnet-sense-keys ("irresolute%3:00:00::" "indecisive%3:00:01::")
  :comment "(indecisive)"
  )

(define-type ONT::communicativeness-val
  :parent ONT::animal-propensity-val
  :comment "(communicative)"
  )

(define-type ONT::communicative-val
  :parent ONT::communicativeness-val
  :wordnet-sense-keys ("communicative%3:00:00::" "communicatory%3:00:00::")
  :comment "(communicative)"
  )

(define-type ONT::not-communicative-val
  :parent ONT::communicativeness-val
  :wordnet-sense-keys ("uncommunicative%3:00:00::" "incommunicative%3:00:04::")
  :comment "(uncommunicative)"
  )

(define-type ONT::financial-behavior-val
  :parent ONT::animal-propensity-val
  :wordnet-sense-keys ("thrifty%3:00:00::")
  :comment "(thrifty)"
  )

(define-type ONT::not-skillful-val
  :parent ONT::skillfulness-val
  :wordnet-sense-keys ("unskilled%3:00:00::")
  :comment "(unskilled)"
  )

(define-type ONT::loyalty-val
  :parent ONT::animal-propensity-val
  :comment "(loyal)"
  )

(define-type ONT::loyal-val
  :parent ONT::loyalty-val
  :wordnet-sense-keys ("loyal%3:00:00::" "patriotic%3:00:00::" "loyal%3:00:06::")
  :comment "(loyal)"
  )

(define-type ONT::not-loyal-val
  :parent ONT::loyalty-val
  :wordnet-sense-keys ("unpatriotic%3:00:00::" "disloyal%3:00:06::" "disloyal%3:00:00::")
  :comment "(disloyal)"
  )

(define-type ONT::faithfulness-val
  :parent ONT::animal-propensity-val
  :comment "faithful, dedicated, devoted to something (faithful)"
  )

(define-type ONT::faithful-val
  :parent ONT::faithfulness-val
  :wordnet-sense-keys ("dedicated%3:00:00::" "faithful%3:00:01::" "faithful%3:00:00::")
  :comment "faithful, dedicated, devoted to something (faithful)"
  )

(define-type ONT::not-faithful-val
  :parent ONT::faithfulness-val
  :wordnet-sense-keys ("unfaithful%3:00:01::" "unfaithful%3:00:00::")
  :comment "(unfaithful)"
  )

(define-type ONT::self-centeredness-val
  :parent ONT::animal-propensity-val
  :sem (F::abstr-obj (F::scale ont::self-centeredness-scale))
  :comment "(selfish, egoistic)"
  )

(define-type ONT::selfish-val
  :parent ONT::self-centeredness-val
  :wordnet-sense-keys ("unselfish%3:00:00::" "selfish%3:00:00::" "egoistic%3:00:00::" "egoistical%3:00:00::" "egocentric%3:00:00::" "self-centered%3:00:00::" "self-centred%3:00:00::")
  :comment "(selfish, egoistic)"
  )

(define-type ONT::selfless-val
  :parent ONT::self-centeredness-val
  :wordnet-sense-keys ("altruistic%3:00:00::" "selfless%3:00:00::")
  :comment "(selfless)"
  )

(define-type ONT::reticence-val
  :parent ONT::animal-propensity-val
  :comment "(taciturn, reticent)"
  )

(define-type ONT::reticent-val
  :parent ONT::reticence-val
  :wordnet-sense-keys ("reserved%3:00:01::" "taciturn%3:00:00::") ; "modest%3:00:01::")
  :comment "(taciturn, reticent)"
  )

(define-type ONT::not-reticent-val
  :parent ONT::reticence-val
  :wordnet-sense-keys ("unreserved%3:00:01::" "immodest%3:00:01::")
  :comment "(unreserved)"
  )

(define-type ONT::patience-val
  :parent ONT::animal-propensity-val
  :comment "(patient)"
  )

(define-type ONT::patient-val
  :parent ONT::patience-val
  :wordnet-sense-keys ("patient%3:00:00::")
  :comment "(patient)"
  )

(define-type ONT::not-patient-val
  :parent ONT::patience-val
  :wordnet-sense-keys ("impatient%3:00:00::")
  :comment "(impatient)"
  )

(define-type ONT::gentle-val
  :parent ONT::boldness-val
  :wordnet-sense-keys ("nonviolent%3:00:00::")
  :comment "(gentle)"
  )

(define-type ONT::submissiveness-val
  :parent ONT::social-interaction-val
  :comment "(submissive, servile)"
  )

(define-type ONT::submissive-val
  :parent ONT::submissiveness-val
  :wordnet-sense-keys ("subordinate%3:00:02::" "submissive%3:00:00::" "servile%3:00:00::" "obedient%3:00:00::")
  :comment "(submissive, servile)"
  )

(define-type ONT::not-submissive-val
  :parent ONT::submissiveness-val
  :wordnet-sense-keys ("insubordinate%3:00:00::" "disobedient%3:00:00::" "defiant%3:00:00::" "noncompliant%3:00:00::")
  :comment "(defiant)"
  )

(define-type ONT::social-intent-val
  :parent ONT::social-interaction-val
  :comment "(malicious, artful)"
  )

(define-type ONT::mal-intended-val
  :parent ONT::social-intent-val
  :wordnet-sense-keys ("malicious%3:00:00::" "artful%3:00:00::" "disingenuous%3:00:00::" "artful%3:00:02::" "sadistic%3:00:00::" "maleficent%3:00:00::")
  :comment "(malicious, artful)"
  )

(define-type ONT::well-intended-val
  :parent ONT::social-intent-val
  :comment "(benevolent)"
  )

(define-type ONT::social-care-and-support-val
  :parent ONT::social-interaction-val
  :comment "(caring, supportive)"
  )

(define-type ONT::caring-val
  :parent ONT::social-care-and-support-val
  :wordnet-sense-keys ("supportive%3:00:00::" "sympathetic%3:00:00::" "compassionate%3:00:00::")
  :comment "(caring, supportive)"
  )

(define-type ONT::not-caring-val
  :parent ONT::social-care-and-support-val
  :wordnet-sense-keys ("unloving%3:00:00::" "unsupportive%3:00:00::" "unsympathetic%3:00:00::")
  :comment "(unloving)"
  )

(define-type ONT::inclined-to-passing-judgement-val
  :parent ONT::social-interaction-val
  :comment "(critical)"
  )

(define-type ONT::judgemental-val
  :parent ONT::inclined-to-passing-judgement-val
  :wordnet-sense-keys ("critical%3:00:01::")
  :comment "(critical)"
  )

(define-type ONT::not-judgemental-val
  :parent ONT::inclined-to-passing-judgement-val
  :wordnet-sense-keys ("uncritical%3:00:01::")
  :comment "(uncritical)"
  )

(define-type ONT::generosity-val
  :parent ONT::social-interaction-val
  :comment "(charitable, generous)"
  )

(define-type ONT::generous-val
  :parent ONT::generosity-val
  :wordnet-sense-keys ("generous%3:00:01::" "hospitable%3:00:02::" "charitable%3:00:00::")
  :comment "(charitable, generous)"
  )

(define-type ONT::not-generous-val
  :parent ONT::generosity-val
  :wordnet-sense-keys ("stingy%3:00:00::" "ungenerous%3:00:04::" "uncharitable%3:00:00::" "ungenerous%3:00:00::" "meanspirited%3:00:02::")
  :comment "(stingy)"
  )

(define-type ONT::consideration-val
  :parent ONT::social-interaction-val
  :comment "to be thoughtful, tactful (thoughtful, considerate)"
  )

(define-type ONT::considerate-val
  :parent ONT::consideration-val
  :wordnet-sense-keys ("thoughtful%3:00:00::" "considerate%3:00:00::")
  :comment "to be thoughtful, tactful (thoughtful, considerate)"
  )

(define-type ONT::not-considerate-val
  :parent ONT::consideration-val
  :wordnet-sense-keys ("thoughtless%3:00:00::" "inconsiderate%3:00:00::" "unconscientious%3:00:00::" "tactless%3:00:00::" "untactful%3:00:00::")
  :comment "(thoughtless, inconsiderate)"
  )

(define-type ONT::courtesy-politeness-val
  :parent ONT::social-interaction-val
  :comment "to be pleasant, gracious, kind (courteous, gracious, polite)"
  )

(define-type ONT::courteous-val
  :parent ONT::courtesy-politeness-val
  :wordnet-sense-keys ("gracious%3:00:00::" "kind%3:00:00::" "polite%3:00:00::" "courteous%3:00:00::" "generous%3:00:02::" "diplomatic%3:00:00::" "diplomatical%3:00:00::" "tactful%3:00:00::" "decorous%3:00:00::")
  :comment "to be pleasant, gracious, kind (courteous, gracious, polite)"
  )

(define-type ONT::not-courteous-val
  :parent ONT::courtesy-politeness-val
  :wordnet-sense-keys ("merciless%3:00:00::" "unmerciful%3:00:04::" "unkind%3:00:00::" "impolite%3:00:00::" "discourteous%3:00:00::" "ungracious%3:00:00::")
  :comment "(discourteous, impolite)"
  )

(define-type ONT::respect-val
  :parent ONT::social-interaction-val
  :comment "(respectful, reverent)"
  )

(define-type ONT::respectful-val
  :parent ONT::respect-val
  :wordnet-sense-keys ("reverent%3:00:00::" "respectful%3:00:00::" "impious%3:00:00::")
  :comment "(respectful, reverent)"
  )

(define-type ONT::not-respectful-val
  :parent ONT::respect-val
  :wordnet-sense-keys ("disrespectful%3:00:00::" "irreverent%3:00:00::")
  :comment "(disrespectful, irreverent)"
  )

(define-type ONT::forgiveness-val
  :parent ONT::animal-propensity-val
  :sem (F::abstr-obj (F::scale ont::compassion-scale))
  :comment "(forgiving)"
  )

(define-type ONT::forgiving-val
  :parent ONT::forgiveness-val
  :wordnet-sense-keys ("forgiving%3:00:00::" "clement%3:00:01::" "merciful%3:00:00::")
  :comment "(forgiving)"
  )

(define-type ONT::not-forgiving-val
  :parent ONT::forgiveness-val
  :wordnet-sense-keys ("unforgiving%3:00:00::")
  :comment "(unforgiving)"
  )

(define-type ONT::frankness-val
  :parent ONT::animal-propensity-val
  :comment "direct, blunt (frank, blunt)"
  )

(define-type ONT::frank-val
  :parent ONT::frankness-val
  :comment "direct, blunt (frank, blunt)"
  )

(define-type ONT::not-frank-val
  :parent ONT::frankness-val
  :wordnet-sense-keys ("direct%3:00:02::" "indirect%3:00:02::")
  :comment "indirect, allusive (indirect, allusive)"
  )

(define-type ONT::knowledge-experience-val
  :parent ONT::animal-propensity-val
  :comment "having experience and knowledge (informed, experienced)"
  )

(define-type ONT::have-knowledge-val
  :parent ONT::knowledge-experience-val
  :wordnet-sense-keys ("informed%3:00:00::" "experienced%3:00:00::" "experient%3:00:00::" "sophisticated%3:00:00::" "educated%3:00:00::" "enlightened%3:00:00::" "numerate%3:00:00::" "innumerate%3:00:00::")
  :comment "having experience and knowledge (informed, experienced)"
  )

(define-type ONT::lack-knowledge-val
  :parent ONT::knowledge-experience-val
  :wordnet-sense-keys ("uninformed%3:00:00::" "inexperienced%3:00:00::" "inexperient%3:00:00::" "uneducated%3:00:00::" "untrained%3:00:00::" "unenlightened%3:00:00::")
  :comment "(uninformed)"
  )

(define-type ONT::naive-val
  :parent ONT::lack-knowledge-val
  :wordnet-sense-keys ("naive%3:00:00::" "naif%3:00:00::")
  :comment "positive form of inexperience (naive)"
  )

(define-type ONT::discernment-val
  :parent ONT::animal-propensity-val
  :comment "(discerning, discriminate)"
  )

(define-type ONT::discerning-val
  :parent ONT::discernment-val
  :wordnet-sense-keys ("discerning%3:00:00::" "discriminate%3:00:00::")
  :comment "(discerning, discriminate)"
  )

(define-type ONT::not-discerning-val
  :parent ONT::discernment-val
  :wordnet-sense-keys ("indiscriminate%3:00:00::" "undiscerning%3:00:00::")
  :comment "(indiscriminate)"
  )

(define-type ONT::temporal-relation-val
  :parent ONT::temporal-val
  :comment "(contemporary, synchronous)"
  )

(define-type ONT::contemporaneous-val
  :parent ONT::temporal-relation-val
  :wordnet-sense-keys ("synchronous%3:00:00::" "synchronal%3:00:00::" "synchronic%3:00:04::")
  :comment "(contemporary, synchronous)"
  )

(define-type ONT::not-contemporaneous-val
  :parent ONT::temporal-relation-val
  :wordnet-sense-keys ("asynchronous%3:00:00::")
  :comment "(asynchronous)"
  )

(define-type ONT::old-fashioned-val
  :parent ONT::relative-time-location-val
  :comment "(old_fashioned)"
  )

(define-type ONT::timeliness-val
  :parent ONT::temporal-val
  :comment "(punctual)"
  )

(define-type ONT::on-time-val
  :parent ONT::timeliness-val
  :wordnet-sense-keys ("punctual%3:00:00::")
  :comment "(punctual)"
  )

(define-type ONT::after-expected-time-val
  :parent ONT::timeliness-val
  :wordnet-sense-keys ("unpunctual%3:00:00::" "late%3:00:00::" "retrospective%3:00:00::")
  :comment "(late)"
  )

(define-type ONT::at-convenient-time-val
  :parent ONT::timeliness-val
  :wordnet-sense-keys ("opportune%3:00:00::")
  :comment "(opportune)"
  )

(define-type ONT::at-inconvenient-time-val
  :parent ONT::timeliness-val
  :wordnet-sense-keys ("inopportune%3:00:00::")
  :comment "(inopportune)"
  )

(define-type ONT::clock-time-val
  :parent ONT::temporal-val
  :wordnet-sense-keys ("postmeridian%3:00:00::" "antemeridian%3:00:00::" "auroral%3:01:01::" "aurorean%3:01:00::" "meridian%3:01:00::")
  :comment "(noon)"
  )

(define-type ONT::logicality-val
  :parent ONT::information-property-val
  :comment "(logical)"
  )

(define-type ONT::logical-val
  :parent ONT::logicality-val
  :wordnet-sense-keys ("logical%3:00:00::")
  :comment "(logical)"
  )

(define-type ONT::not-logical-val
  :parent ONT::logicality-val
  :wordnet-sense-keys ("illogical%3:00:00::" "unlogical%3:00:04::")
  :comment "(illogical)"
  )

(define-type ONT::ambiguous-val
  :parent ONT::clarity-val
  :wordnet-sense-keys ("ambiguous%3:00:00::" "equivocal%3:00:00::" "ambiguous%3:00:04::")
  :comment "(ambiguous)"
  )

(define-type ONT::substantiation-val
  :parent ONT::information-property-val
  :comment "(substantiated, supported)"
  )

(define-type ONT::substantiated-val
  :parent ONT::substantiation-val
  :comment "(substantiated, supported)"
  )

(define-type ONT::not-substantiated-val
  :parent ONT::substantiation-val
  :wordnet-sense-keys ("unsupported%3:00:02::")
  :comment "(unsupported)"
  )

(define-type ONT::maturity-val
  :parent ONT::life-process-val
  :comment "(mature)"
  )

(define-type ONT::mature-val
  :parent ONT::maturity-val
  :wordnet-sense-keys ("mature%3:00:02::" "ripe%3:00:00::" "mature%3:00:06::" "fledged%3:00:00::" "mature%3:00:04::" "mature%3:00:01::")
  :comment "(mature)"
  )

(define-type ONT::not-mature-val
  :parent ONT::maturity-val
  :wordnet-sense-keys ("unfledged%3:00:00::" "immature%3:00:04::" "green%3:00:00::" "unripe%3:00:00::" "unripened%3:00:00::" "immature%3:00:06::" "premature%3:00:00::" "immature%3:00:01::")
  :comment "(immature, green)"
  )

(define-type ONT::associated-with-architecture-val
  :parent ONT::associated-with-val
  )

(define-type ONT::architectural-property-val
  :parent ONT::associated-with-architecture-val
  :wordnet-sense-keys ("detached%3:00:00::" "megalithic%3:01:00::" "arched%3:01:00::" "bistroic%3:01:00::" "fenestral%3:01:00::" "basilican%3:01:00::" "multilevel%3:01:00::" "attached%3:00:02::" "amphitheatric%3:01:00::" "amphitheatrical%3:01:00::" "architectural%3:01:00::" "monumental%3:01:00::" "oxonian%3:01:01::" "locker-room%3:01:00::" "domestic%3:01:00::" "mural%3:01:00::" "domiciliary%3:01:00::" "bungaloid%3:01:00::")
  )

(define-type ONT::associated-with-arts-val
  :parent ONT::associated-with-val
  )

(define-type ONT::artistic-property-val
  :parent ONT::associated-with-arts-val
  :wordnet-sense-keys ("aesthetic%3:00:00::" "esthetic%3:00:00::" "aesthetical%3:00:00::" "esthetical%3:00:00::" "inaesthetic%3:00:00::" "unaesthetic%3:00:00::" "nonrepresentational%3:00:00::" "postmodernist%3:01:00::" "postmodern%3:01:00::" "impressionist%3:01:00::" "impressionistic%3:01:00::" "statuary%3:01:00::" "short-handled%3:01:00::" "sculptural%3:01:00::" "expressionist%3:01:00::" "expressionistic%3:01:00::" "futuristic%3:01:00::" "futurist%3:01:00::" "doric%3:01:00::" "homiletic%3:01:00::" "homiletical%3:01:00::" "choreographic%3:01:00::" "romantic%3:01:00::" "romanticist%3:01:00::" "romanticistic%3:01:00::" "cubist%3:01:00::" "cubistic%3:01:00::" "anaglyphic%3:01:00::" "anaglyphical%3:01:00::" "anaglyptic%3:01:00::" "anaglyptical%3:01:00::" "corinthian%3:01:02::" "ionic%3:01:02::" "classicistic%3:01:00::" "minimalist%3:01:00::" "iconic%3:01:00::" "graphic%3:01:01::" "sphingine%3:01:00::" "neoclassicist%3:01:00::" "neoclassicistic%3:01:00::" "pyrographic%3:01:00::")
  )

(define-type ONT::not-hereditary-val
  :parent ONT::body-system-val
  )

(define-type ONT::body-substance-val
  :parent ONT::associated-with-body-val
  :wordnet-sense-keys ("ichorous%3:01:00::" "sanious%3:01:00::" "lymphoid%3:01:00::" "bilious%3:01:00::" "biliary%3:01:02::" "mucous%3:01:00::" "mucose%3:01:00::" "seminal%3:01:00::" "humoral%3:01:00::" "mucopurulent%3:01:00::" "chylific%3:01:00::" "chylifactive%3:01:00::" "chylifactory%3:01:00::" "synovial%3:01:00::" "autacoidal%3:01:00::" "nectar-rich%3:01:00::" "uricosuric%3:01:00::" "vitreous%3:01:01::" "hidrotic%3:01:00::" "hormonal%3:01:00::" "steroidal%3:01:00::" "hemic%3:01:00::" "haemic%3:01:00::" "hematic%3:01:00::" "haematic%3:01:00::" "uric%3:01:00::" "milch%3:01:00::" "secretory%3:01:00::" "mucoid%3:01:00::" "mucoidal%3:01:00::" "lacteal%3:01:00::" "chylaceous%3:01:00::" "chylous%3:01:00::" "muciferous%3:01:00::" "serous%3:01:00::" "chyliferous%3:01:00::" "salivary%3:01:00::" "gonadotropic%3:01:00::" "gonadotrophic%3:01:00::" "urinary%3:01:00::" "seminiferous%3:01:00::")
  )

(define-type ONT::animal-material-val
  :parent ONT::associated-with-body-val
  :wordnet-sense-keys ("osseous%3:01:00::" "osteal%3:01:00::" "bony%3:01:00::" "nacreous%3:01:00::" "butyraceous%3:01:00::")
  )

(define-type ONT::associated-with-communication-val
  :parent ONT::associated-with-val
  )

(define-type ONT::written-communication-property-val
  :parent ONT::associated-with-communication-val
  :wordnet-sense-keys ("testamentary%3:01:00::" "masoretic%3:01:00::" "epic%3:01:00::" "epical%3:01:00::" "documentary%3:01:00::" "documental%3:01:00::" "holographic%3:01:00::" "holographical%3:01:00::" "literary%3:01:00::" "stenographic%3:01:00::" "stenographical%3:01:00::" "lithographic%3:01:00::" "planographic%3:01:00::" "dithyrambic%3:01:00::" "antistrophic%3:01:00::" "invitational%3:01:00::" "interlinear%3:01:00::" "interlineal%3:01:00::" "thespian%3:01:00::" "calligraphic%3:01:00::" "calligraphical%3:01:00::" "fictional%3:01:00::" "inscriptive%3:01:00::" "tetrametric%3:01:00::" "textual%3:01:00::" "elegiac%3:01:00::" "dramatic%3:01:00::" "autographic%3:01:00::" "lyric%3:01:01::" "hieroglyphic%3:01:01::" "hieroglyphical%3:01:01::" "puranic%3:01:00::")
  )

(define-type ONT::transmission-property-val
  :parent ONT::associated-with-communication-val
  :wordnet-sense-keys ("fiber-optic%3:01:00::" "fiberoptic%3:01:00::" "fibre-optic%3:01:00::" "fibreoptic%3:01:00::" "telephonic%3:01:00::" "radiotelephonic%3:01:00::" "radiophonic%3:01:00::" "long-distance%3:01:00::")
  )

(define-type ONT::message-property-val
  :parent ONT::associated-with-communication-val
  :wordnet-sense-keys ("annunciatory%3:01:00::" "elocutionary%3:01:00::" "poetic%3:01:00::" "poetical%3:01:00::" "formulary%3:01:00::" "synoptic%3:01:00::" "bolographic%3:01:00::" "proverbial%3:01:00::" "rhetorical%3:01:00::" "bromidic%3:01:00::" "idiomatic%3:01:00::" "idiomatical%3:01:00::" "vocal%3:01:00::" "stigmatic%3:01:01::" "articulatory%3:01:00::" "articulative%3:01:00::" "symbolic%3:01:01::" "thematic%3:01:00::" "symbolic%3:01:00::" "symbolical%3:01:00::" "promotional%3:01:00::" "canonist%3:01:00::" "unthematic%3:01:00::" "graphic%3:01:00::" "graphical%3:01:00::" "communicative%3:01:00::" "vocal%3:01:02::" "axiomatic%3:01:00::" "axiomatical%3:01:00::" "postulational%3:01:00::" "testimonial%3:01:01::" "back-channel%3:01:00::" "dialectal%3:01:00::" "dramaturgic%3:01:00::" "dramaturgical%3:01:00::" "pictorial%3:01:00::" "pictural%3:01:00::" "indexical%3:01:00::" "postal%3:01:00::" "theatrical%3:01:00::" "canonic%3:01:02::" "canonical%3:01:02::" "prosodic%3:01:00::" "extropic%3:01:00::" "promissory%3:01:00::" "evidentiary%3:01:00::" "axiomatic%3:01:02::" "aphoristic%3:01:00::" "promotional%3:01:01::" "archaistic%3:01:00::" "testimonial%3:01:00::")
  )

(define-type ONT::cultural-property-val
  :parent ONT::culture-val
  :wordnet-sense-keys ("noncivilized%3:00:00::" "noncivilised%3:00:00::" "civilized%3:00:00::" "civilised%3:00:00::")
  )

(define-type ONT::people-and-inhabitants-val
  :parent ONT::culture-val
  :wordnet-sense-keys ("malay%3:01:00::" "malayan%3:01:00::" "hmong%3:01:00::" "german-american%3:01:00::" "aboriginal%3:01:00::" "arabic%3:01:00::" "shona%3:01:00::" "celtic%3:01:00::" "gaelic%3:01:00::" "romany%3:01:00::" "romani%3:01:00::" "hispanic%3:01:00::" "latino%3:01:00::" "montserratian%3:01:00::" "frankish%3:01:00::" "saxon%3:01:00::" "zapotec%3:01:00::" "alaskan%3:01:00::" "gothic%3:01:00::" "anglo-saxon%3:01:00::" "mongoloid%3:01:02::" "singhalese%3:01:00::" "sinhalese%3:01:00::" "sabine%3:01:00::" "lao%3:01:00::" "cockney%3:01:00::" "briton%3:01:00::" "mesoamerican%3:01:00::" "algonquian%3:01:00::" "algonkian%3:01:00::" "algonquin%3:01:00::" "norman%3:01:00::" "creole%3:01:01::" "teutonic%3:01:00::" "germanic%3:01:02::" "roman%3:01:02::" "muscovite%3:01:00::" "thai%3:01:02::" "tai%3:01:02::" "siamese%3:01:02::")
  )

(define-type ONT::society-val
  :parent ONT::associated-with-society-and-culture-val
  :wordnet-sense-keys ("social%3:01:02::")
  )

(define-type ONT::social-science-property-val
  :parent ONT::society-val
  :wordnet-sense-keys ("craniometric%3:01:00::" "craniometrical%3:01:00::" "ethnographic%3:01:00::" "ethnographical%3:01:00::" "demographic%3:01:00::" "archaeological%3:01:00::" "archeological%3:01:00::" "archaeologic%3:01:00::" "archeologic%3:01:00::" "geopolitical%3:01:00::" "geostrategic%3:01:00::" "criminological%3:01:00::" "ethnological%3:01:00::" "ethnologic%3:01:00::")
  )

(define-type ONT::organization-property-val
  :parent ONT::society-val
  :wordnet-sense-keys ("institutional%3:00:00::" "institutional%3:01:00::" "administrative%3:01:00::" "sectarian%3:01:01::" "municipal%3:01:00::" "collegiate%3:01:00::" "collegial%3:01:02::" "colonial%3:01:00::" "allied%3:01:02::" "choric%3:01:00::" "allied%3:01:01::" "capitular%3:01:00::" "capitulary%3:01:00::" "feudal%3:01:00::" "feudalistic%3:01:00::" "congregational%3:01:00::" "meritocratic%3:01:00::" "humanistic%3:01:03::" "humanist%3:01:03::" "intertribal%3:01:00::" "tribal%3:01:00::")
  )

(define-type ONT::social-event-val
  :parent ONT::society-val
  :wordnet-sense-keys ("celebratory%3:01:00::" "funerary%3:01:00::" "postnuptial%3:01:00::" "commemorative%3:01:00::" "commemorating%3:01:00::" "prenuptial%3:01:00::" "premarital%3:01:00::" "antenuptial%3:01:00::" "olympic%3:01:00::" "bridal%3:01:01::" "nuptial%3:01:00::" "spousal%3:01:01::" "mortuary%3:01:00::" "burlesque%3:01:00::")
  )

(define-type ONT::social-action-val
  :parent ONT::society-val
  :wordnet-sense-keys ("callithumpian%3:01:00::" "executive%3:01:00::" "mediatory%3:01:00::" "directional%3:01:01::" "counterinsurgent%3:01:00::" "counterrevolutionary%3:01:00::" "managerial%3:01:00::" "penal%3:01:00::" "processional%3:01:01::" "procedural%3:01:00::" "confrontational%3:01:00::" "revolutionary%3:01:00::" "insurrectional%3:01:00::" "insurrectionary%3:01:00::" "appellate%3:01:00::" "appellant%3:01:00::" "processional%3:01:00::" "litigious%3:01:00::")
  )

(define-type ONT::food-property-val
  :parent ONT::associated-with-food-val
  :wordnet-sense-keys ("vinous%3:01:00::" "vinaceous%3:01:00::" "alimentative%3:01:00::" "carroty%3:01:00::" "vanilla%3:01:00::" "wheaten%3:01:00::" "whole-wheat%3:01:00::" "wholemeal%3:01:00::" "herbal%3:01:00::" "garlicky%3:01:00::" "oaten%3:01:00::")
  )

(define-type ONT::political-val
  :parent ONT::associated-with-val
  :wordnet-sense-keys ("nonpolitical%3:00:00::")
  )

(define-type ONT::political-property-val
  :parent ONT::political-val
  :wordnet-sense-keys ("statutory%3:01:00::" "legislative%3:01:01::" "congressional%3:01:00::" "legislative%3:01:00::" "unamended%3:00:00::" "parliamentary%3:01:00::" "washingtonian%3:01:02::")
  )

(define-type ONT::socio-political-ideology-val
  :parent ONT::political-val
  :wordnet-sense-keys ("left%3:00:02::" "undemocratic%3:00:00::" "capitalistic%3:00:00::" "capitalist%3:00:04::" "patriarchal%3:00:00::" "socialistic%3:00:00::" "socialist%3:00:04::" "matriarchal%3:00:00::" "center%3:00:00::" "pro-choice%3:00:00::" "pro-life%3:00:00::" "liberal%3:00:00::" "conservative%3:00:00::" "maoist%3:01:00::" "fascist%3:01:00::" "fascistic%3:01:00::" "marxist-leninist%3:01:00::" "post-communist%3:01:00::" "theocratic%3:01:00::" "communist%3:01:00::" "communistic%3:01:00::" "oligarchic%3:01:00::" "oligarchical%3:01:00::" "ideological%3:01:00::" "autarchic%3:01:00::" "autarchical%3:01:00::" "autarkical%3:01:00::" "totalitarian%3:01:00::" "totalistic%3:01:00::" "republican%3:01:01::" "imperialistic%3:01:00::" "imperialist%3:01:00::" "marxist%3:01:00::" "unitary%3:00:00::" "absolutist%3:01:00::" "absolutistic%3:01:00::" "green%3:01:00::" "anarchistic%3:01:00::" "imperial%3:01:01::" "fabian%3:01:00::")
  )

(define-type ONT::printed-language-property-val
  :parent ONT::associated-with-languages-val
  :wordnet-sense-keys ("gothic%3:01:02::" "italic%3:01:01::" "roman%3:01:03::")
  )

(define-type ONT::not-related-to-language-val
  :parent ONT::associated-with-languages-val
  )

(define-type ONT::language-use-val
  :parent ONT::associated-with-languages-val
  :wordnet-sense-keys ("multilingual%3:00:00::" "monolingual%3:00:00::")
  )

(define-type ONT::associated-with-law-val
  :parent ONT::associated-with-val
  )

(define-type ONT::law-property-val
  :parent ONT::associated-with-law-val
  :wordnet-sense-keys ("ultra_vires%3:00:00::" "intra_vires%3:00:00::" "intestate%3:00:00::" "minor%3:00:03::" "nonaged%3:00:00::" "underage%3:00:00::" "residuary%3:01:01::" "testate%3:00:00::" "substantive%3:00:00::" "essential%3:00:04::" "reversionary%3:01:00::" "undue%3:00:02::" "legal%3:01:00::" "major%3:00:03::" "adjective%3:00:00::" "procedural%3:00:00::" "canonic%3:01:00::" "canonical%3:01:00::" "incompetent%3:00:02::" "unqualified%3:00:03::")
  )

(define-type ONT::territory-land-val
  :parent ONT::associated-with-location-val
  :wordnet-sense-keys ("territorial%3:00:00::" "terrestrial%3:00:00::")
  )

(define-type ONT::geo-direction-val
  :parent ONT::geo-location-val
  :wordnet-sense-keys ("eastern%3:00:02::" "western%3:00:02::" "northern%3:00:01::" "southern%3:00:01::" "southern%3:00:02::" "eastern%3:00:01::" "western%3:00:01::" "northern%3:00:02")
  )

(define-type ONT::country-nation-val
  :parent ONT::geo-location-val
  :wordnet-sense-keys ("nigerian%3:01:01::" "mexican%3:01:00::" "bangladeshi%3:01:00::" "east_pakistani%3:01:00::" "colombian%3:01:00::" "togolese%3:01:00::" "laotian%3:01:00::" "ethiopian%3:01:00::" "portuguese%3:01:00::" "lusitanian%3:01:01::" "sierra_leonean%3:01:00::" "mozambican%3:01:01::" "bulgarian%3:01:00::" "brazilian%3:01:00::" "qatari%3:01:00::" "katari%3:01:00::" "anglo-indian%3:01:00::" "algerian%3:01:00::" "tibetan%3:01:00::" "cambodian%3:01:00::" "kampuchean%3:01:00::" "bhutanese%3:01:00::" "andorran%3:01:00::" "south_korean%3:01:00::" "rhodesian%3:01:00::" "bolivian%3:01:00::" "american%3:01:01::" "samoan%3:01:00::" "finnish%3:01:00::" "bahamian%3:01:00::" "pakistani%3:01:00::" "monacan%3:01:00::" "monegasque%3:01:00::" "georgian%3:01:05::" "slovakian%3:01:00::" "malawian%3:01:00::" "norwegian%3:01:00::" "norse%3:01:00::" "grenadian%3:01:00::" "guatemalan%3:01:00::" "east_german%3:01:00::" "madagascan%3:01:00::" "libyan%3:01:00::" "german%3:01:00::" "austrian%3:01:00::" "botswanan%3:01:00::" "zimbabwean%3:01:00::" "rwandan%3:01:00::" "ruandan%3:01:00::" "namibian%3:01:00::" "liberian%3:01:00::" "malaysian%3:01:00::" "malayan%3:01:01::" "russian%3:01:00::" "costa_rican%3:01:00::" "scots%3:01:00::" "scottish%3:01:00::" "scotch%3:01:00::" "gabonese%3:01:00::" "guyanese%3:01:00::" "dominican%3:01:01::" "romanian%3:01:00::" "rumanian%3:01:00::" "roumanian%3:01:00::" "flemish%3:01:00::" "hungarian%3:01:00::" "magyar%3:01:00::" "kazakhstani%3:01:00::" "albanian%3:01:00::" "salvadoran%3:01:00::" "salvadorean%3:01:00::" "iranian%3:01:00::" "persian%3:01:00::" "britannic%3:01:00::" "chadian%3:01:00::" "indonesian%3:01:00::" "canadian%3:01:00::" "beninese%3:01:00::" "mongolian%3:01:02::" "venezuelan%3:01:00::" "croatian%3:01:00::" "sudanese%3:01:00::" "lebanese%3:01:00::" "nepalese%3:01:00::" "nepali%3:01:00::" "armenian%3:01:00::" "slovenian%3:01:00::" "thai%3:01:00::" "tai%3:01:00::" "siamese%3:01:00::" "swazi%3:01:02::" "tajikistani%3:01:00::" "ukrainian%3:01:00::" "soviet%3:01:00::" "macedonian%3:01:00::" "zambian%3:01:00::" "swedish%3:01:00::" "pro-american%3:01:00::" "burmese%3:01:00::" "burundi%3:01:00::" "burundian%3:01:00::" "tanzanian%3:01:00::" "ghanaian%3:01:00::" "ghanese%3:01:00::" "ghanian%3:01:00::" "cameroonian%3:01:00::" "tongan%3:01:01::" "djiboutian%3:01:00::" "anti-american%3:01:00::" "greek%3:01:01::" "grecian%3:01:00::" "hellenic%3:01:01::" "maltese%3:01:00::" "luxembourgian%3:01:00::" "indian%3:01:00::" "icelandic%3:01:00::" "bruneian%3:01:00::" "polish%3:01:00::" "panamanian%3:01:00::" "japanese%3:01:00::" "nipponese%3:01:00::" "belgian%3:01:00::" "kyrgyzstani%3:01:00::" "french%3:01:00::" "gallic%3:01:01::" "malian%3:01:00::" "omani%3:01:00::" "bosnian%3:01:00::" "turkmen%3:01:00::" "ecuadorian%3:01:00::" "azerbaijani%3:01:00::" "iraqi%3:01:00::" "iraki%3:01:00::" "nauruan%3:01:00::" "san_marinese%3:01:00::" "dutch%3:01:00::" "zairean%3:01:00::" "zairese%3:01:00::" "yemeni%3:01:00::" "senegalese%3:01:00::" "north_korean%3:01:00::" "seychellois%3:01:00::" "moroccan%3:01:00::" "maroc%3:01:00::" "angolan%3:01:00::" "cuban%3:01:00::" "congolese%3:01:00::" "saudi-arabian%3:01:00::" "saudi%3:01:00::" "nicaraguan%3:01:00::" "guinean%3:01:00::" "uruguayan%3:01:00::" "liechtensteiner%3:01:00::" "singaporean%3:01:02::" "argentine%3:01:00::" "argentinian%3:01:00::" "eritrean%3:01:00::" "north_vietnamese%3:01:00::" "ugandan%3:01:00::" "fijian%3:01:00::" "filipino%3:01:00::" "philippine%3:01:00::" "egyptian%3:01:00::" "aramean%3:01:00::" "aramaean%3:01:00::" "tunisian%3:01:01::" "gambian%3:01:00::" "luxemburger%3:01:00::" "turkish%3:01:00::" "haitian%3:01:00::" "mauritanian%3:01:00::" "mauritian%3:01:01::" "israeli%3:01:00::" "kuwaiti%3:01:01::" "italian%3:01:00::" "romaic%3:01:00::" "jamaican%3:01:00::" "afghani%3:01:00::" "afghan%3:01:00::" "afghanistani%3:01:00::" "south_african%3:01:00::" "syrian%3:01:00::" "uzbekistani%3:01:00::" "paraguayan%3:01:00::" "peruvian%3:01:00::" "english%3:01:00::" "spanish%3:01:00::" "moldovan%3:01:00::" "jordanian%3:01:00::" "chilean%3:01:00::" "danish%3:01:00::" "belarusian%3:01:00::" "honduran%3:01:00::" "kenyan%3:01:00::" "afrikaans%3:01:00::" "afrikaner%3:01:00::" "national%3:01:02::" "swiss%3:01:00::" "yugoslavian%3:01:00::" "yugoslav%3:01:00::")
  )

(define-type ONT::city-state-district-val
  :parent ONT::geo-location-val
  :wordnet-sense-keys ("singaporean%3:01:01::" "roman%3:01:01::" "romanic%3:01:00::" "venetian%3:01:00::" "sarawakian%3:01:00::" "ephesian%3:01:00::" "damascene%3:01:00::" "athenian%3:01:00::" "low-tension%3:00:00::" "low-voltage%3:00:00::" "kuwaiti%3:01:00::" "milanese%3:01:00::" "nicaean%3:01:00::" "nicene%3:01:00::" "inductive%3:01:00::" "palestinian%3:01:00::" "tyrolean%3:01:00::" "tyrolese%3:01:00::" "alternating%3:00:00::" "sicilian%3:01:00::" "viennese%3:01:00::" "territorial%3:01:00::" "electrostatic%3:01:00::" "static%3:01:00::" "piezoelectric%3:01:00::" "florentine%3:01:00::" "tuscan%3:01:00::" "quebecois%3:01:00::" "cornish%3:01:00::" "tunisian%3:01:02::" "texan%3:01:00::" "national%3:01:00::" "pyroelectric%3:01:00::" "pyroelectrical%3:01:00::" "genoese%3:01:00::" "genovese%3:01:00::" "thermoelectric%3:01:00::" "thermoelectrical%3:01:00::" "oxonian%3:01:00::" "photoelectric%3:01:00::" "photoelectrical%3:01:00::" "suburban%3:01:00::" "aberdonian%3:01:00::" "noncyclic%3:00:00::" "noncyclical%3:00:00::" "neapolitan%3:01:00::" "myotonic%3:01:00::" "glaswegian%3:01:00::" "californian%3:01:00::" "hydroelectric%3:01:00::" "sabahan%3:01:00::" "lancastrian%3:01:02::" "babylonian%3:01:00::" "atonic%3:01:00::" "prefectural%3:01:00::" "bavarian%3:01:00::" "abkhaz%3:01:00::" "abkhazian%3:01:00::" "mancunian%3:01:00::" "mycenaean%3:01:00::" "corinthian%3:01:00::" "boeotian%3:01:00::" "liverpudlian%3:01:00::" "archdiocesan%3:01:00::" "washingtonian%3:01:01::" "tonic%3:01:02::" "theban%3:01:01::" "communal%3:01:02::" "latin%3:01:01::" "hawaiian%3:01:00::" "civil%3:01:01::" "sardinian%3:01:00::" "voltaic%3:01:00::" "galvanic%3:01:00::" "metropolitan%3:01:00::" "direct%3:00:01::" "galwegian%3:01:00::" "frisian%3:01:00::" "washingtonian%3:01:00::" "spartan%3:01:00::" "parisian%3:01:00::" "luxemburger%3:01:02::" "georgian%3:01:04::" "attic%3:01:00::" "delphic%3:01:00::" "delphian%3:01:00::" "theban%3:01:00::" "trojan%3:01:00::" "carthaginian%3:01:00::" "punic%3:01:00::" "calcuttan%3:01:00::" "argive%3:01:00::" "eparchial%3:01:00::" "electrical%3:01:00::" "parochial%3:01:00::" "electric%3:01:00::" "electrical%3:01:01::" "assamese%3:01:00::" "tasmanian%3:01:00::" "diocesan%3:01:00::" "catalan%3:01:01::" "ionian%3:01:00::" "cantonal%3:01:00::" "provincial%3:01:00::")
  )

(define-type ONT::sea-ocean-val
  :parent ONT::associated-with-location-val
  :wordnet-sense-keys ("marine%3:00:00::" "aquatic%3:00:00::" "transatlantic%3:01:00::" "pacific%3:01:00::" "mediterranean%3:01:00::" "estuarine%3:01:00::" "estuarial%3:01:00::" "atlantic%3:01:00::" "nilotic%3:01:01::" "fluvial%3:01:00::" "aegean%3:01:00::" "oceanic%3:01:00::" "pelagic%3:01:00::" "thalassic%3:01:00::" "marine%3:01:00::" "lacustrine%3:01:00::" "baltic%3:01:00::" "transoceanic%3:01:00::" "rhenish%3:01:00::")
  )

(define-type ONT::body-of-water-val
  :parent ONT::associated-with-location-val
  )

(define-type ONT::imaginary-place-val
  :parent ONT::associated-with-location-val
  :wordnet-sense-keys ("celestial%3:01:00::" "heavenly%3:01:00::" "lilliputian%3:01:00::" "brobdingnagian%3:01:00::" "laputan%3:01:00::" "ruritanian%3:01:00::" "elysian%3:01:00::" "purgatorial%3:01:00::")
  )

(define-type ONT::associated-with-military-val
  :parent ONT::associated-with-val
  :wordnet-sense-keys ("military%3:00:01::" "military%3:00:02::" "unmilitary%3:00:00::" "nonmilitary%3:00:00::" "irregular%3:00:02::" "armored%3:00:01::" "armoured%3:00:01::" "unarmed%3:00:01::" "operational%3:00:00::" "regular%3:00:02::" "marine%3:01:01::" "paramilitary%3:01:00::" "naval%3:01:00::" "divisional%3:01:00::" "unarmored%3:00:01::" "unarmoured%3:00:01::" "nonoperational%3:00:00::" "inactive%3:00:00::" "regimental%3:01:00::" "tactical%3:01:00::" "military%3:01:00::")
  )

(define-type ONT::associated-with-music-val
  :parent ONT::associated-with-val
  )

(define-type ONT::music-property-val
  :parent ONT::associated-with-music-val
  :wordnet-sense-keys ("decreasing%3:00:02::" "piano%3:00:00::" "soft%3:00:07::" "increasing%3:00:02::" "forte%3:00:00::" "loud%3:00:02::" "musical%3:00:02::" "unmelodious%3:00:00::" "unmelodic%3:00:04::" "unmusical%3:00:04::" "pitched%3:44:00::" "unmusical%3:00:02::" "nonmusical%3:00:02::" "euphonious%3:00:00::" "euphonous%3:00:00::" "harmonious%3:00:00::" "inharmonious%3:00:00::" "unharmonious%3:00:02::" "melodious%3:00:00::" "melodic%3:00:04::" "musical%3:00:04::" "tuneful%3:00:00::" "melodious%3:00:04::" "tuneless%3:00:00::" "untuneful%3:00:00::" "unmelodious%3:00:04::" "slow%3:00:02::" "fast%3:00:02::" "tonal%3:00:00::" "musical%3:00:01::" "monophonic%3:00:00::" "plucked%3:00:00::" "staccato%3:00:00::" "disconnected%3:00:02::" "bowed%3:00:00::" "unfretted%3:01:00::" "dramatic%3:00:02::" "natural%3:00:04::" "disjunct%3:00:00::" "major%3:00:04::" "percussive%3:01:00::" "pianistic%3:01:01::" "chordal%3:01:00::" "modal%3:01:01::" "operatic%3:01:00::" "thematic%3:01:01::" "unmusical%3:00:01::" "nonmusical%3:00:01::" "orchestrated%3:01:00::" "prolusory%3:01:00::" "isotonic%3:01:00::" "chromatic%3:00:01::" "harmonic%3:01:00::" "serial%3:01:02::" "contrapuntal%3:01:00::" "orchestral%3:01:00::" "tertian%3:01:01::" "legato%3:00:00::" "smooth%3:00:03::" "musicological%3:01:00::" "scalar%3:01:01::" "conjunct%3:00:00::" "first%3:00:03::" "choral%3:01:01::" "fretted%3:01:00::" "polyphonic%3:00:00::" "contrapuntal%3:00:00::" "atonal%3:00:00::" "unkeyed%3:00:00::" "flat%3:00:00::" "minor%3:00:04::" "musical%3:01:00::" "tympanic%3:01:01::" "instrumental%3:01:00::" "anticlimactic%3:01:00::" "anticlimactical%3:01:00::" "sharp%3:00:03::" "scalic%3:01:00::" "diatonic%3:00:00::" "lyric%3:01:02::" "polyphonic%3:01:00::" "polyphonous%3:01:00::" "fugal%3:01:00::" "philharmonic%3:01:00::" "atonalistic%3:01:00::" "symphonic%3:01:00::" "lyric%3:00:00::" "melodic%3:01:00::" "balletic%3:01:00::" "pentatonic%3:01:00::" "second%3:00:00::" "nonharmonic%3:01:00::")
  )

(define-type ONT::rhythm-val
  :parent ONT::associated-with-music-val
  )

(define-type ONT::rhythmic-val
  :parent ONT::rhythm-val
  :wordnet-sense-keys ("rhythmical%3:00:00::" "rhythmic%3:00:00::")
  )

(define-type ONT::not-rhythmic-val
  :parent ONT::rhythm-val
  :wordnet-sense-keys ("unrhythmical%3:00:00::" "unrhythmic%3:00:00::")
  )

(define-type ONT::associated-with-person-val
  :parent ONT::associated-with-val
  )

(define-type ONT::famous-person-val
  :parent ONT::associated-with-person-val
  :wordnet-sense-keys ("columbian%3:01:00::" "jacobinic%3:01:00::" "jacobinical%3:01:00::")
  )

(define-type ONT::artist-craftsman-val
  :parent ONT::famous-person-val
  :wordnet-sense-keys ("palladian%3:01:00::" "rembrandtesque%3:01:00::" "ambrosian%3:01:00::" "stravinskyan%3:01:00::" "stravinskian%3:01:00::" "mozartian%3:01:00::" "mozartean%3:01:00::" "handelian%3:01:00::" "pre-raphaelite%3:01:00::" "beethovenian%3:01:00::" "gauguinesque%3:01:00::" "michelangelesque%3:01:00::" "chippendale%3:01:00::" "wagnerian%3:01:00::")
  )

(define-type ONT::scientist-val
  :parent ONT::famous-person-val
  :wordnet-sense-keys ("jamesian%3:01:01::" "cartesian%3:01:00::" "gaussian%3:01:00::" "riemannian%3:01:00::" "hertzian%3:01:00::" "boolean%3:01:00::" "pythagorean%3:01:00::" "pasteurian%3:01:00::" "ptolemaic%3:01:00::" "einsteinian%3:01:00::" "mendelian%3:01:00::" "malthusian%3:01:00::" "keynesian%3:01:00::" "pavlovian%3:01:00::" "euclidian%3:01:00::" "euclidean%3:01:00::" "postdoctoral%3:01:00::" "piagetian%3:01:00::" "leibnizian%3:01:00::" "leibnitzian%3:01:00::" "skinnerian%3:01:00::" "linnaean%3:01:00::" "linnean%3:01:00::" "jungian%3:01:00::" "newtonian%3:01:00::" "galilean%3:01:01::" "huxleyan%3:01:00::" "huxleian%3:01:00::")
  )

(define-type ONT::us-president-val
  :parent ONT::famous-person-val
  :wordnet-sense-keys ("lincolnesque%3:01:00::" "lincolnian%3:01:00::" "rooseveltian%3:01:00::" "wilsonian%3:01:00::" "jeffersonian%3:01:00::" "washingtonian%3:01:03::" "jacksonian%3:01:00::")
  )

(define-type ONT::leader-val
  :parent ONT::famous-person-val
  :wordnet-sense-keys ("demosthenic%3:01:00::" "bismarckian%3:01:00::" "draconian%3:01:00::" "napoleonic%3:01:00::" "gregorian%3:01:02::" "gandhian%3:01:00::" "gregorian%3:01:01::" "caesarian%3:01:01::" "caesarean%3:01:01::" "cromwellian%3:01:00::" "nestorian%3:01:00::" "hitlerian%3:01:00::" "machiavellian%3:01:00::" "augustan%3:01:00::" "julian%3:01:00::")
  )

(define-type ONT::intellectual-person-val
  :parent ONT::famous-person-val
  :wordnet-sense-keys ("platonic%3:01:00::" "presocratic%3:01:00::" "pre-socratic%3:01:00::" "wittgensteinian%3:01:00::" "socratic%3:01:00::" "epicurean%3:01:00::" "deweyan%3:01:00::" "hegelian%3:01:00::" "aristotelian%3:01:00::" "aristotelean%3:01:00::" "aristotelic%3:01:00::" "peripatetic%3:01:00::" "freudian%3:01:00::" "kantian%3:01:00::")
  )

(define-type ONT::writer-val
  :parent ONT::famous-person-val
  :wordnet-sense-keys ("donnean%3:01:00::" "donnian%3:01:00::" "ibsenian%3:01:00::" "orwellian%3:01:00::" "gilbertian%3:01:00::" "jamesian%3:01:00::" "hemingwayesque%3:01:00::" "hugoesque%3:01:00::" "autobiographical%3:01:01::" "autobiographic%3:01:01::" "voltarian%3:01:00::" "voltarean%3:01:00::" "coleridgian%3:01:00::" "coleridgean%3:01:00::" "wordsworthian%3:01:00::" "kiplingesque%3:01:00::" "yeatsian%3:01:00::" "kafkaesque%3:01:00::" "dantean%3:01:00::" "dantesque%3:01:00::" "dickensian%3:01:00::" "bardic%3:01:00::" "zolaesque%3:01:00::" "rousseauan%3:01:00::" "shakespearian%3:01:00::" "shakespearean%3:01:00::" "audenesque%3:01:00::" "aeschylean%3:01:00::" "poetic%3:01:01::" "senecan%3:01:00::" "churchillian%3:01:00::" "frostian%3:01:00::" "proustian%3:01:00::" "thoreauvian%3:01:00::" "homeric%3:01:00::" "dostoevskian%3:01:00::" "dostoyevskian%3:01:00::" "balzacian%3:01:00::" "shavian%3:01:00::" "goethean%3:01:00::" "goethian%3:01:00::")
  )

(define-type ONT::entertainer-val
  :parent ONT::famous-person-val
  :wordnet-sense-keys ("bogartian%3:01:00::" "rabelaisian%3:01:00::")
  )

(define-type ONT::social-class-val
  :parent ONT::associated-with-person-val
  :wordnet-sense-keys ("noble%3:00:02::" "lowborn%3:00:00::" "subhuman%3:00:00::" "upper-class%3:00:00::" "lower-class%3:00:00::" "low-class%3:00:04::" "middle-class%3:00:00::" "noble%3:00:01::" "ignoble%3:00:01::")
  )

(define-type ONT::gender-val
  :parent ONT::associated-with-person-val
  :wordnet-sense-keys ("unwomanly%3:00:00::")
  )

(define-type ONT::occupational-val
  :parent ONT::associated-with-person-val
  :wordnet-sense-keys ("professional%3:00:01::" "unscholarly%3:00:00::" "nonprofessional%3:00:00::" "unprofessional%3:00:00::" "professional%3:00:02::" "professional%3:01:01::" "on-the-job%3:01:00::" "occupational%3:01:00::" "professional%3:01:00::" "vocational%3:01:00::")
  )

(define-type ONT::occupation-specific-val
  :parent ONT::occupational-val
  :wordnet-sense-keys ("scholarly%3:00:00::" "civilian%3:00:00::" "tonsorial%3:01:00::" "pyrrhic%3:01:00::" "secretarial%3:01:00::" "gubernatorial%3:01:00::" "doctoral%3:01:00::" "doctorial%3:01:00::" "consular%3:01:00::" "masonic%3:01:01::" "fictile%3:01:00::" "pyrotechnic%3:01:00::" "pyrotechnical%3:01:00::" "typographic%3:01:00::" "typographical%3:01:00::" "imperial%3:01:00::" "bibliothecal%3:01:00::" "bibliothecarial%3:01:00::" "solomonic%3:01:00::" "viceregal%3:01:00::" "professorial%3:01:00::" "priestly%3:01:00::" "hieratic%3:01:01::" "hieratical%3:01:01::" "sacerdotal%3:01:00::" "magisterial%3:01:00::" "pianistic%3:01:00::" "apiarian%3:01:00::" "archidiaconal%3:01:00::" "ministerial%3:01:01::" "brahminic%3:01:00::" "brahminical%3:01:00::" "editorial%3:01:01::" "proconsular%3:01:00::" "equestrian%3:01:01::" "papal%3:01:00::" "apostolic%3:01:01::" "apostolical%3:01:01::" "pontifical%3:01:01::" "collegial%3:01:00::" "vicarial%3:01:00::" "czarist%3:01:00::" "czaristic%3:01:00::" "tsarist%3:01:00::" "tsaristic%3:01:00::" "tzarist%3:01:00::" "bureaucratic%3:01:00::" "mediatorial%3:01:00::" "despotic%3:01:00::" "despotical%3:01:00::" "pastoral%3:01:00::" "censorial%3:01:00::" "meretricious%3:01:00::" "supervisory%3:01:00::" "clerical%3:01:01::" "tutorial%3:01:00::" "ambassadorial%3:01:00::" "caroline%3:01:00::" "carolean%3:01:00::" "senatorial%3:01:00::" "rabbinical%3:01:00::" "rabbinic%3:01:00::" "political%3:01:02::" "episcopal%3:01:00::" "pontifical%3:01:00::" "sartorial%3:01:00::" "patristic%3:01:00::" "patristical%3:01:00::" "praetorian%3:01:00::" "praetorial%3:01:00::" "pretorian%3:01:00::" "pretorial%3:01:00::" "mayoral%3:01:00::" "archducal%3:01:00::" "actuarial%3:01:00::" "ministerial%3:01:02::" "presidential%3:01:00::" "clerical%3:01:00::" "demagogic%3:01:00::" "demagogical%3:01:00::" "legal%3:01:01::")
  )

(define-type ONT::person-behavior-tendency-val
  :parent ONT::associated-with-person-val
  :wordnet-sense-keys ("bibliopolic%3:01:00::" "philhellenic%3:01:00::" "philhellene%3:01:00::" "graecophile%3:01:00::" "graecophilic%3:01:00::" "chauvinistic%3:01:00::" "amoristic%3:01:00::" "bacchantic%3:01:00::" "voyeuristic%3:01:00::" "voyeuristical%3:01:00::" "puerile%3:01:00::" "androgynous%3:01:01::" "avuncular%3:01:00::" "anthropophagous%3:01:00::" "carpetbag%3:01:00::" "entrepreneurial%3:01:00::" "bibliophilic%3:01:00::" "sophistic%3:01:00::" "valetudinarian%3:01:00::" "valetudinary%3:01:00::" "cannibalistic%3:01:00::")
  )

(define-type ONT::kinship-val
  :parent ONT::associated-with-person-val
  :wordnet-sense-keys ("lineal%3:00:00::" "direct%3:00:04::" "filial%3:00:00::" "wifely%3:00:00::" "wifelike%3:00:00::" "uxorial%3:00:00::" "parental%3:00:00::" "maternal%3:00:04::" "paternal%3:00:04::" "sisterly%3:00:00::" "sisterlike%3:00:00::" "sororal%3:00:02::" "brotherly%3:00:00::" "brotherlike%3:00:00::" "fraternal%3:00:02::" "fraternal%3:00:00::" "biovular%3:00:00::" "maternal%3:00:00::" "paternal%3:00:00::" "fraternal%3:01:01::" "familial%3:01:00::" "genealogic%3:01:00::" "genealogical%3:01:00::" "patriarchal%3:01:00::" "avuncular%3:01:01::")
  )

(define-type ONT::lineage-val
  :parent ONT::associated-with-person-val
  :wordnet-sense-keys ("capetian%3:01:00::" "seljuk%3:01:00::" "lancastrian%3:01:03::" "carolingian%3:01:00::" "dynastic%3:01:00::" "tudor%3:01:00::" "hanoverian%3:01:00::" "merovingian%3:01:00::" "royal%3:01:01::")
  )

(define-type ONT::age-group-val
  :parent ONT::associated-with-person-val
  :wordnet-sense-keys ("generational%3:01:00::")
  )

(define-type ONT::imaginary-beings-val
  :parent ONT::associated-with-person-val
  :wordnet-sense-keys ("heroic%3:01:00::" "sisyphean%3:01:00::" "faustian%3:01:00::" "chimeric%3:01:00::" "chimerical%3:01:00::" "chimeral%3:01:00::" "procrustean%3:01:00::" "orphic%3:01:00::" "arthurian%3:01:00::" "falstaffian%3:01:00::" "cyclopean%3:01:00::")
  )

(define-type ONT::follower-of-doctrine-val
  :parent ONT::religion-specific-val
  :wordnet-sense-keys ("carmelite%3:01:00::" "protestant%3:01:00::" "moorish%3:01:00::" "moresque%3:01:00::" "dominican%3:01:00::" "messianic%3:01:00::" "coptic%3:01:00::" "benedictine%3:01:00::" "cenobitic%3:01:00::" "coenobitic%3:01:00::" "cenobitical%3:01:00::" "coenobitical%3:01:00::" "mosaic%3:01:00::" "eremitic%3:01:00::" "eremitical%3:01:00::" "marian%3:01:00::" "jesuitical%3:01:00::" "jesuitic%3:01:00::" "jesuit%3:01:00::" "sufi%3:01:00::" "apostolic%3:01:00::" "apostolical%3:01:00::" "erasmian%3:01:00::" "puritanical%3:01:00::" "muhammadan%3:01:00::" "mohammedan%3:01:00::" "benedictine%3:01:01::" "pauline%3:01:00::" "gentile%3:01:00::" "zoroastrian%3:01:00::" "lutheran%3:01:00::")
  )

(define-type ONT::religious-denomination-val
  :parent ONT::religion-specific-val
  :wordnet-sense-keys ("congregational%3:01:02::" "congregationalist%3:01:00::" "episcopal%3:01:01::" "episcopalian%3:01:00::" "denominational%3:01:00::" "anglo-catholic%3:01:00::" "methodist%3:01:00::" "wesleyan%3:01:00::" "baptistic%3:01:00::" "mormon%3:01:00::" "lutheran%3:01:02::" "anglican%3:01:00::")
  )

(define-type ONT::spiritual-being-val
  :parent ONT::religion-specific-val
  :wordnet-sense-keys ("seraphic%3:01:00::" "seraphical%3:01:00::" "satanic%3:01:00::" "jovian%3:01:01::" "cacodemonic%3:01:00::" "cacodaemonic%3:01:00::" "adonic%3:01:00::" "elfin%3:01:00::" "georgian%3:01:00::" "aesculapian%3:01:00::" "medical%3:01:02::" "vestal%3:01:00::" "mercurial%3:01:01::" "satyric%3:01:00::" "satyrical%3:01:00::" "dionysian%3:01:00::" "numinous%3:01:00::" "aeolian%3:01:00::" "angelic%3:01:00::" "angelical%3:01:00::" "franciscan%3:01:00::" "archangelic%3:01:00::" "archangelical%3:01:00::")
  )

(define-type ONT::religious-val
  :parent ONT::religion-val
  :wordnet-sense-keys ("religious%3:00:00::")
  )

(define-type ONT::not-religious-val
  :parent ONT::religion-val
  :wordnet-sense-keys ("irreligious%3:00:00::" "agnostic%3:00:00::" "agnostical%3:00:00::" "secular%3:00:00::")
  )

(define-type ONT::religious-property-val
  :parent ONT::religion-val
  :wordnet-sense-keys ("clean%3:00:04::" "unclean%3:00:00::" "impure%3:00:03::" "homiletic%3:01:01::" "homiletical%3:01:01::" "biblical%3:01:00::" "scriptural%3:01:00::" "evangelical%3:01:00::" "antinomian%3:01:00::" "sacramental%3:01:00::" "dogmatic%3:01:00::" "avestan%3:01:00::" "eucharistic%3:01:00::" "predestinarian%3:01:00::" "vedic%3:01:00::" "christological%3:01:00::" "universalistic%3:01:00::" "universalist%3:01:00::" "gallican%3:01:00::" "apocryphal%3:01:00::" "atheist%3:01:00::" "atheistic%3:01:00::" "atheistical%3:01:00::" "abbatial%3:01:00::" "mishnaic%3:01:00::" "pentecostal%3:01:00::" "baptismal%3:01:00::" "monophysite%3:01:00::" "monophysitic%3:01:00::" "creedal%3:01:00::" "credal%3:01:00::" "levitical%3:01:00::" "deist%3:01:00::" "deistic%3:01:00::" "consubstantial%3:01:00::" "missionary%3:01:00::" "missional%3:01:00::" "ritual%3:01:02::" "biblical%3:01:02::" "postbiblical%3:01:00::" "responsive%3:01:00::" "antiphonal%3:01:00::" "gnostic%3:01:00::" "pietistic%3:01:00::" "pietistical%3:01:00::" "evangelistic%3:01:00::" "synergistic%3:01:00::" "liturgical%3:01:00::" "agrypnotic%3:01:00::")
  )

(define-type ONT::belief-val
  :parent ONT::associated-with-belief-systems-val
  :wordnet-sense-keys ("pragmatic%3:01:00::" "pragmatical%3:01:00::" "sacerdotal%3:01:02::" "nativist%3:01:00::" "nativistic%3:01:00::" "political%3:01:01::" "rationalist%3:01:00::" "dualistic%3:01:00::" "manichaean%3:01:01::" "ideal%3:01:00::" "idealistic%3:01:00::" "deconstructionist%3:01:00::" "platonistic%3:01:00::" "animist%3:01:00::" "animistic%3:01:00::" "secular%3:01:00::" "humanitarian%3:01:00::" "existentialist%3:01:00::" "stoic%3:01:00::" "scholastic%3:01:01::" "millenary%3:01:01::" "realistic%3:01:00::" "conceptualistic%3:01:00::" "dogmatic%3:01:01::" "teleological%3:01:00::" "existential%3:01:01::" "confucian%3:01:00::" "doctrinal%3:01:00::" "monistic%3:01:00::" "humanist%3:01:02::" "humanistic%3:01:02::" "animalistic%3:01:00::" "semiotic%3:01:00::" "semiotical%3:01:00::" "autotelic%3:01:00::" "operationalist%3:01:00::" "mechanistic%3:01:00::" "rationalistic%3:01:00::" "pluralistic%3:01:00::" "intuitionist%3:01:00::" "taoist%3:01:01::" "supernaturalist%3:01:00::" "supernaturalistic%3:01:00::" "gymnosophical%3:01:00::" "relativistic%3:01:01::" "totemic%3:01:00::" "expansionist%3:01:00::" "nominalistic%3:01:00::" "postmillennial%3:01:00::" "spiritualistic%3:01:00::" "spiritualist%3:01:00::" "nihilistic%3:01:00::")
  )

(define-type ONT::astronomical-property-val
  :parent ONT::astronomy-val
  :wordnet-sense-keys ("stellar%3:01:00::" "astral%3:01:00::" "interstellar%3:01:00::" "sublunar%3:01:00::" "sublunary%3:01:00::" "cislunar%3:01:00::" "extraterrestrial%3:01:00::" "canicular%3:01:00::" "asteroidal%3:01:00::" "interplanetary%3:01:00::" "martian%3:01:00::" "intergalactic%3:01:00::" "apogean%3:01:00::" "lunisolar%3:01:00::" "zenithal%3:01:00::" "planetal%3:01:00::" "planetary%3:01:00::" "planetary%3:01:02::" "terrestrial%3:01:02::" "galactic%3:01:00::" "jovian%3:01:02::" "translunar%3:01:00::" "translunary%3:01:00::" "superlunar%3:01:00::" "superlunary%3:01:00::" "solar%3:01:00::" "mercurial%3:01:02::" "extragalactic%3:01:00::" "heliacal%3:01:00::" "heliac%3:01:00::")
  )

(define-type ONT::biological-property-val
  :parent ONT::biology-val
  :wordnet-sense-keys ("lymphatic%3:01:00::" "zymotic%3:01:00::" "homosexual%3:00:00::" "androgynous%3:00:00::" "male%3:00:00::" "cellular%3:00:00::" "asexual%3:00:00::" "nonsexual%3:00:00::" "crossbred%3:00:00::" "female%3:00:00::" "sexual%3:00:00::" "insectivorous%3:00:00::" "mesial%3:00:00::" "oral%3:00:00::" "actinomorphic%3:00:00::" "actinomorphous%3:00:00::" "polyploid%3:00:00::" "mesomorphic%3:00:00::" "muscular%3:00:04::" "heterosexual%3:00:00::" "multiparous%3:00:00::" "stomatous%3:00:00::" "distal%3:00:02::" "caducous%3:00:00::" "shed%3:00:00::" "retrorse%3:00:00::" "homologous%3:00:02::" "hemiparasitic%3:01:00::" "arawakan%3:01:00::" "botanic%3:01:00::" "botanical%3:01:00::" "acrogenic%3:01:00::" "acrogenous%3:01:00::" "sericultural%3:01:00::" "neural%3:01:00::" "neuronal%3:01:00::" "neuronic%3:01:00::" "discomycetous%3:01:00::" "hemodynamic%3:01:00::" "anamorphic%3:01:02::" "democratic%3:01:00::" "binucleate%3:00:00::" "binuclear%3:00:00::" "binucleated%3:00:00::" "autolytic%3:01:00::" "amitotic%3:01:00::" "pneumococcal%3:01:00::" "antiquarian%3:01:01::" "organismal%3:01:00::" "organismic%3:01:00::" "basophilic%3:01:00::" "basidiomycetous%3:01:00::" "prokaryotic%3:01:00::" "procaryotic%3:01:00::" "hebraic%3:01:01::" "hebraical%3:01:01::" "hebrew%3:01:01::" "bacteriophagic%3:01:00::" "bacteriophagous%3:01:00::" "suppurative%3:01:00::" "bacteriolytic%3:01:00::" "evolutionary%3:01:00::" "ancestral%3:01:00::" "biogeographic%3:01:00::" "biogeographical%3:01:00::" "cellular%3:01:00::" "astronautic%3:01:00::" "astronautical%3:01:00::" "saprophytic%3:01:00::" "commensal%3:01:00::" "mitotic%3:01:00::" "rickettsial%3:01:00::" "corpuscular%3:01:00::" "autotrophic%3:01:00::" "autophytic%3:01:00::" "bacteroidal%3:01:00::" "bacteroid%3:01:00::" "bacterioidal%3:01:00::" "bacterioid%3:01:00::" "pharaonic%3:01:00::" "artistic%3:01:00::" "dysgenic%3:01:00::" "cacogenic%3:01:00::" "leonardesque%3:01:00::" "postmenopausal%3:01:00::" "myrmecophytic%3:01:00::" "totipotent%3:01:00::" "psychomotor%3:01:00::" "coital%3:01:00::" "copulatory%3:01:00::" "propagandist%3:01:00::" "propagandistic%3:01:00::" "physiological%3:01:00::" "bacteriostatic%3:01:00::" "metabolic%3:01:00::" "childbearing%3:01:00::" "abaxial%3:00:00::" "dorsal%3:00:04::" "plutocratic%3:01:00::" "plutocratical%3:01:00::" "menopausal%3:01:00::" "ascomycetous%3:01:00::" "penicillin-resistant%3:01:00::" "accusatorial%3:01:00::" "mononuclear%3:00:00::" "mononucleate%3:00:00::" "macerative%3:01:00::" "megakaryocytic%3:01:00::" "growing%3:01:00::" "eugenic%3:01:00::" "alchemistic%3:01:00::" "alchemistical%3:01:00::" "expiratory%3:01:00::" "premenopausal%3:01:00::" "self%3:01:00::" "epiphytic%3:01:00::" "gladiatorial%3:01:00::" "bryophytic%3:01:00::" "menstrual%3:01:00::" "catamenial%3:01:00::" "bacterial%3:01:00::" "autoimmune%3:01:00::" "saxicolous%3:01:00::" "saxatile%3:01:00::" "saxicoline%3:01:00::" "genetic%3:01:00::" "genetical%3:01:00::" "congeneric%3:01:00::" "congenerical%3:01:00::" "congenerous%3:01:00::" "blastomeric%3:01:00::" "galilean%3:01:00::" "galilaean%3:01:00::" "endoparasitic%3:01:00::" "anglo-jewish%3:01:00::" "morphologic%3:01:00::" "morphological%3:01:00::" "structural%3:01:02::" "katharobic%3:01:00::" "eukaryotic%3:01:00::" "eucaryotic%3:01:00::" "allopatric%3:00:00::" "catabolic%3:01:00::" "katabolic%3:01:00::" "antibacterial%3:01:00::" "fungal%3:01:00::" "fungous%3:01:00::" "adaxial%3:00:00::" "ventral%3:00:04::" "indian%3:01:01::" "amerind%3:01:00::" "amerindic%3:01:00::" "native_american%3:01:00::" "field-crop%3:01:00::" "agnostic%3:01:00::" "zionist%3:01:00::" "neurophysiological%3:01:00::" "unicellular%3:01:00::" "lacrimatory%3:01:00::" "lachrymatory%3:01:00::" "analogous%3:00:00::" "inspiratory%3:01:00::" "semiparasitic%3:01:00::" "phagocytic%3:01:00::" "specialistic%3:01:00::" "photoemissive%3:01:00::" "planktonic%3:01:00::" "biogenous%3:01:00::" "isomorphous%3:01:00::" "isomorphic%3:01:00::" "pteridological%3:01:00::" "streptococcal%3:01:00::" "streptococcic%3:01:00::" "strep%3:01:00::" "epizoan%3:01:00::" "ectozoan%3:01:00::" "ontogenetic%3:01:00::" "parasitic%3:01:01::" "parasitical%3:01:00::" "actinal%3:00:00::" "chlamydial%3:01:00::" "palingenetic%3:01:00::" "antrorse%3:00:00::" "air-breathing%3:01:00::" "necrotic%3:01:00::" "dictatorial%3:01:00::" "in_vivo%3:00:00::" "zygotic%3:01:00::" "intracellular%3:01:00::" "antibiotic%3:01:00::" "monoclonal%3:01:00::" "urceolate%3:01:00::" "bantu%3:01:00::" "territorial%3:00:01::" "dendritic%3:01:00::" "civil%3:01:00::" "civic%3:01:02::" "erythroid%3:01:00::" "nigerian%3:01:00::" "nigerien%3:01:00::" "uniparous%3:00:00::" "trinucleate%3:00:00::" "trinuclear%3:00:00::" "trinucleated%3:00:00::" "advective%3:01:00::" "parturient%3:01:00::" "sex-linked%3:01:00::" "time-release%3:01:00::" "puerperal%3:01:00::" "psychogenetic%3:01:01::" "psychogenic%3:01:00::" "peptic%3:01:00::" "lacrimal%3:01:00::" "lachrymal%3:01:00::" "fastidious%3:00:02::" "exacting%3:00:00::" "facultative%3:00:00::" "phrenological%3:01:00::" "persistent%3:00:00::" "lasting%3:00:06::" "abiogenetic%3:01:00::" "microbial%3:01:00::" "microbic%3:01:00::" "heterologous%3:00:02::" "heterologic%3:00:00::" "heterological%3:00:00::" "osmotic%3:01:00::" "spousal%3:01:00::" "nonsuppurative%3:01:00::" "indo-european%3:01:00::" "indo-aryan%3:01:00::" "aryan%3:01:00::" "semite%3:01:00::" "semitic%3:01:00::" "essene%3:01:00::" "algoid%3:01:00::" "lithophytic%3:01:00::" "coccal%3:01:00::" "extracellular%3:01:00::" "authorial%3:01:00::" "auctorial%3:01:00::" "sympatric%3:00:00::" "excretory%3:01:00::" "thallophytic%3:01:00::" "ebionite%3:01:00::" "sociobiologic%3:01:00::" "sociobiological%3:01:00::" "cryptogamic%3:01:00::" "cryptogamous%3:01:00::" "cytokinetic%3:01:00::" "autogenetic%3:01:00::" "filial%3:01:00::" "monocarpic%3:01:00::" "plantal%3:01:00::" "meiotic%3:01:00::" "mortuary%3:01:01::" "propagative%3:01:00::" "judicial%3:01:02::" "philistine%3:01:00::" "nazarene%3:01:00::" "nocturnal%3:00:00::" "adnate%3:00:00::" "ergotic%3:01:00::" "bacillar%3:01:00::" "bacillary%3:01:00::" "intercellular%3:01:00::" "saprobic%3:01:00::" "hittite%3:01:00::" "psychogenetic%3:01:00::" "staphylococcal%3:01:00::" "phylogenetic%3:01:00::" "phyletic%3:01:00::" "obligate%3:00:00::" "algal%3:01:00::" "hematopoietic%3:01:00::" "haematopoietic%3:01:00::" "hemopoietic%3:01:00::" "haemopoietic%3:01:00::" "hematogenic%3:01:00::" "haematogenic%3:01:00::" "dominical%3:01:01::" "inhalant%3:01:00::" "geophytic%3:01:00::" "biogenetic%3:01:00::" "entozoan%3:01:00::" "endozoan%3:01:00::" "archesporial%3:01:00::" "axonal%3:01:00::" "biogenic%3:01:00::" "connate%3:00:00::" "sensorimotor%3:01:00::" "viral%3:01:00::" "diaphoretic%3:01:00::" "sudorific%3:01:00::" "mozambican%3:01:00::" "zygomorphic%3:00:00::" "bilaterally_symmetrical%3:00:00::" "zygomorphous%3:00:00::" "auxetic%3:01:00::" "serbian%3:01:00::" "respiratory%3:01:00::" "metastatic%3:01:00::" "agrobiologic%3:01:00::" "agrobiological%3:01:00::" "pleomorphic%3:01:00::" "metamorphic%3:01:00::" "metamorphous%3:01:00::" "apomictic%3:01:00::" "apomictical%3:01:00::" "spermous%3:01:00::" "spermatic%3:01:00::" "maturational%3:01:00::" "cytolytic%3:01:00::" "ecological%3:01:00::" "ecologic%3:01:00::" "bionomical%3:01:00::" "bionomic%3:01:00::" "environmental%3:01:00::" "digestive%3:01:00::" "erythropoietic%3:01:00::" "anthropogenetic%3:01:00::" "anthropogenic%3:01:00::" "taxonomic%3:01:00::" "taxonomical%3:01:00::" "systematic%3:01:00::" "astrocytic%3:01:00::" "unfastidious%3:00:02::" "sessile%3:00:02::" "trophic%3:01:00::" "anabolic%3:01:00::" "botulinal%3:01:00::" "pedunculate%3:00:00::" "stalked%3:00:00::" "premenstrual%3:01:00::" "developmental%3:01:00::" "eutrophic%3:01:00::" "colonial%3:01:01::" "nazarene%3:01:01::" "clonal%3:01:00::" "jewish%3:01:01::" "judaic%3:01:01::" "electrophoretic%3:01:00::" "cataphoretic%3:01:00::" "lysogenic%3:01:00::" "biotic%3:01:00::" "cryonic%3:01:00::" "anisogamic%3:01:00::" "anisogamous%3:01:00::" "anisogametic%3:01:00::" "diurnal%3:00:00::" "cenogenetic%3:01:00::" "neurobiological%3:01:00::" "autodidactic%3:01:00::" "suctorial%3:01:00::" "ascetic%3:01:00::" "ascetical%3:01:00::" "nonterritorial%3:00:00::" "karyokinetic%3:01:00::" "choragic%3:01:00::" "pugilistic%3:01:00::" "blastogenetic%3:01:00::" "actinomycetal%3:01:00::" "actinomycetous%3:01:00::" "polymorphic%3:01:00::" "polymorphous%3:01:00::" "septic%3:01:00::" "evaporative%3:01:00::" "blastemal%3:01:00::" "blastematic%3:01:00::" "blastemic%3:01:00::" "macrobiotic%3:01:00::" "chromatographic%3:01:00::" "chromatographical%3:01:00::" "slav%3:01:00::" "sadducean%3:01:00::" "arthrosporic%3:01:00::" "arthrosporous%3:01:00::" "nitrogen-fixing%3:01:00::" "royal%3:01:00::" "hemolytic%3:01:00::" "haemolytic%3:01:00::" "biotypic%3:01:00::" "mandaean%3:01:00::" "mandean%3:01:00::" "ovular%3:01:01::")
  )

(define-type ONT::biological-theory-val
  :parent ONT::biology-val
  :wordnet-sense-keys ("lamarckian%3:01:00::" "organicistic%3:01:00::" "darwinian%3:01:00::" "neo-darwinian%3:01:00::" "neo-lamarckian%3:01:00::")
  )

(define-type ONT::botanical-property-val
  :parent ONT::biology-val
  :wordnet-sense-keys ("simple%3:00:01::" "unsubdivided%3:00:00::" "compound%3:00:00::" "rough%3:00:02::" "caulescent%3:00:00::" "cauline%3:00:04::" "stemmed%3:00:04::" "branchy%3:00:00::" "smooth%3:00:02::" "carnivorous%3:00:00::" "evergreen%3:00:00::" "determinate%3:00:02::" "autogamous%3:00:00::" "autogamic%3:00:00::" "monoclinous%3:00:00::" "aerial%3:00:00::" "gregarious%3:00:01::" "acid-loving%3:00:00::" "ungregarious%3:00:01::" "deciduous%3:00:00::" "indeterminate%3:00:02::" "cyclic%3:00:03::" "chlamydeous%3:00:00::" "floral%3:01:00::" "self-pollinating%3:01:00::" "alternate%3:00:00::" "archegonial%3:01:00::" "archegoniate%3:01:00::" "monocotyledonous%3:00:00::" "floral%3:01:01::" "cambial%3:01:00::" "single%3:00:04::" "achenial%3:01:00::" "balsamic%3:01:00::" "balsamy%3:01:00::" "bulbaceous%3:01:00::" "axile%3:01:00::" "axial%3:01:02::" "heterotrophic%3:01:00::" "basiscopic%3:00:00::" "tendril-climbing%3:01:00::" "spicate%3:01:00::" "uniovular%3:01:00::" "uniovulate%3:01:00::" "perennial%3:00:00::" "carpellary%3:01:00::" "acyclic%3:00:01::" "double%3:00:04::" "paniculate%3:01:00::" "bicapsular%3:01:00::" "bulbed%3:01:00::" "flowery%3:01:00::" "acroscopic%3:00:00::" "apothecial%3:01:00::" "carposporous%3:01:00::" "capsular%3:01:00::" "homostylous%3:01:00::" "homostylic%3:01:00::" "homostyled%3:01:00::" "cereal%3:01:00::" "botryoid%3:01:00::" "botryoidal%3:01:00::" "boytrose%3:01:00::" "involucrate%3:01:00::" "carpellate%3:00:00::" "pistillate%3:00:02::" "acinar%3:01:00::" "foliate%3:01:02::" "foliated%3:01:00::" "amaranthine%3:01:00::" "leguminous%3:01:00::" "dicotyledonous%3:00:00::" "amygdaline%3:01:00::" "unmown%3:00:00::" "uncut%3:00:04::" "basipetal%3:00:00::" "orthotropous%3:00:00::" "exogamous%3:00:02::" "exogamic%3:00:02::" "achlamydeous%3:00:00::" "antheridial%3:01:00::" "umbellate%3:01:00::" "umbellar%3:01:00::" "acarpelous%3:00:00::" "acarpellous%3:00:00::" "acervate%3:01:00::" "ascocarpous%3:01:00::" "phyllodial%3:01:00::" "calyptrate%3:01:01::" "tuberous%3:01:00::" "veinal%3:01:00::" "foliaceous%3:01:00::" "stoloniferous%3:01:00::" "bladed%3:01:02::" "thalloid%3:01:00::" "semi-tuberous%3:01:00::" "cormous%3:01:00::" "cormose%3:01:00::" "arborical%3:01:00::" "arboreal%3:01:00::" "arborary%3:01:00::" "arborous%3:01:00::" "gemmiferous%3:01:00::" "campylotropous%3:00:00::" "endogenous%3:01:00::" "sepaloid%3:01:00::" "sepaline%3:01:00::" "amphitropous%3:00:00::" "alliaceous%3:01:00::" "carposporic%3:01:00::" "basidial%3:01:00::" "sporogenous%3:01:00::" "corymbose%3:01:00::" "alkaline-loving%3:00:00::" "annual%3:00:00::" "one-year%3:00:04::" "cauline%3:00:00::" "apogamic%3:01:00::" "apogametic%3:01:00::" "apogamous%3:01:00::" "angiocarpic%3:01:00::" "angiocarpous%3:01:00::" "ascosporic%3:01:00::" "ascosporous%3:01:00::" "radical%3:00:00::" "basal%3:00:02::" "rhizomatous%3:01:00::" "acropetal%3:00:00::" "apocarpous%3:00:00::" "syncarpous%3:00:00::" "petaloid%3:01:00::" "testaceous%3:01:00::" "calyceal%3:01:00::" "calycine%3:01:00::" "calycinal%3:01:00::" "panicled%3:01:00::" "axillary%3:01:02::" "alar%3:01:00::" "endogamous%3:00:02::" "endogamic%3:00:02::" "diclinous%3:00:00::" "umbelliform%3:01:00::" "opposite%3:00:00::" "paired%3:00:02::" "anatropous%3:00:00::" "inverted%3:00:04::" "catkinate%3:01:00::" "drupaceous%3:01:00::" "aecial%3:01:00::" "nectariferous%3:01:00::" "angiospermous%3:01:00::" "shrubby%3:01:00::" "fruticose%3:01:00::" "fruticulose%3:01:00::" "basidiosporous%3:01:00::" "perigonal%3:01:00::" "citrous%3:01:00::" "biennial%3:00:00::" "two-year%3:00:04::" "ovular%3:01:02::")
  )

(define-type ONT::genetic-property-val
  :parent ONT::biology-val
  :wordnet-sense-keys ("diabetic%3:01:00::" "parental%3:01:00::" "autosomal%3:01:00::" "dominant%3:00:02::" "bivalent%3:00:02::" "double%3:00:00::" "recessive%3:00:00::" "x-linked%3:01:00::" "haploid%3:00:00::" "haploidic%3:00:00::" "monoploid%3:00:00::" "homozygous%3:00:00::" "univalent%3:00:02::" "chromosomal%3:01:00::" "multivalent%3:00:00::" "diploid%3:00:00::" "heterozygous%3:00:00::" "achondroplastic%3:01:00::")
  )

(define-type ONT::taxonomic-property-val
  :parent ONT::biology-val
  :wordnet-sense-keys ("vibrionic%3:01:00::" "oleaceous%3:01:00::" "liliaceous%3:01:00::" "chelonian%3:01:00::" "artiodactyl%3:01:00::" "artiodactylous%3:01:00::" "even-toed%3:01:00::" "monotypic%3:01:00::" "eutherian%3:01:00::" "lobeliaceous%3:01:00::" "equine%3:01:01::" "branchiopod%3:01:00::" "branchiopodan%3:01:00::" "branchiopodous%3:01:00::" "murine%3:01:00::" "hymenopterous%3:01:00::" "archosaurian%3:01:00::" "aroid%3:01:00::" "araceous%3:01:00::" "ruminant%3:01:00::" "gymnospermous%3:01:00::" "nonruminant%3:01:00::" "rosaceous%3:01:00::" "caecilian%3:01:00::" "cyprinid%3:01:00::" "cyprinoid%3:01:00::" "canine%3:01:00::" "ordinal%3:01:00::" "arthropodal%3:01:00::" "arthropodan%3:01:00::" "arthropodous%3:01:00::" "solanaceous%3:01:00::" "filariid%3:01:00::" "specific%3:01:00::" "araneidal%3:01:00::" "araneidan%3:01:00::" "bovine%3:01:00::" "bovid%3:01:00::" "reptilian%3:01:00::" "brachyurous%3:01:00::" "anopheline%3:01:00::" "annelid%3:01:00::" "annelidan%3:01:00::" "cetacean%3:01:00::" "cetaceous%3:01:00::" "australopithecine%3:01:00::" "cephalopod%3:01:00::" "cephalopodan%3:01:00::" "iridaceous%3:01:00::" "dipterous%3:01:00::" "moraceous%3:01:00::" "orb-weaving%3:01:00::" "asclepiadaceous%3:01:00::" "accipitrine%3:01:00::" "amphibious%3:01:00::" "amphibian%3:01:00::" "moneran%3:01:00::" "dictyopteran%3:01:00::" "umbelliferous%3:01:00::" "composite%3:01:00::" "arundinaceous%3:01:00::" "protozoal%3:01:00::" "protozoan%3:01:00::" "protozoic%3:01:00::" "anserine%3:01:00::" "conspecific%3:01:00::" "buteonine%3:01:00::" "anuran%3:01:00::" "batrachian%3:01:00::" "salientian%3:01:00::" "bignoniaceous%3:01:00::" "chordate%3:01:00::" "crinoid%3:01:00::" "apocynaceous%3:01:00::" "caryophyllaceous%3:01:00::" "cruciferous%3:01:00::" "plumbaginaceous%3:01:00::" "mammalian%3:01:00::" "chaetognathan%3:01:00::" "chaetognathous%3:01:00::" "polemoniaceous%3:01:00::" "cyanobacterial%3:01:00::" "cyanophyte%3:01:00::" "cucurbitaceous%3:01:00::" "isopteran%3:01:00::" "brachiopod%3:01:00::" "brachiopodous%3:01:00::" "carangid%3:01:00::" "clamatorial%3:01:00::" "betulaceous%3:01:00::" "crustaceous%3:01:01::" "crustacean%3:01:00::")
  )

(define-type ONT::physiological-property-val
  :parent ONT::biology-val
  :wordnet-sense-keys ("efferent%3:00:00::" "motorial%3:00:00::" "involuntary%3:00:02::" "afferent%3:00:00::")
  )

(define-type ONT::zoological-property-val
  :parent ONT::biology-val
  :wordnet-sense-keys ("placental%3:00:00::" "caudate%3:00:00::" "caudated%3:00:00::" "estrous%3:00:00::" "oviparous%3:00:00::" "univalve%3:00:00::" "bivalve%3:00:00::" "bivalved%3:00:00::" "acaudate%3:00:00::" "acaudal%3:00:00::" "cold-blooded%3:00:00::" "warm-blooded%3:00:00::" "anestrous%3:00:00::" "vagile%3:00:00::" "leonine%3:01:00::" "baboonish%3:01:00::" "protozoological%3:01:00::" "ratty%3:01:00::" "oscine%3:01:00::" "zoic%3:01:00::" "plantigrade%3:00:00::" "lupine%3:01:00::" "avifaunal%3:01:00::" "avifaunistic%3:01:00::" "phocine%3:01:00::" "viviparous%3:00:00::" "live-bearing%3:00:00::" "bottom-feeding%3:01:00::" "cercarial%3:01:00::" "jumentous%3:01:00::" "metabolic%3:00:00::" "metabolous%3:00:00::" "gallinaceous%3:01:00::" "vertebrate%3:00:00::" "registered%3:00:02::" "weaned%3:00:00::" "passerine%3:01:00::" "sapiens%3:01:00::" "unregistered%3:00:02::" "ursine%3:01:00::" "avian%3:01:00::" "pedal%3:01:00::" "anthropic%3:01:00::" "anthropical%3:01:00::" "vulpine%3:01:00::" "vulpecular%3:01:00::" "bottom-dwelling%3:01:00::" "limacine%3:01:00::" "limacoid%3:01:00::" "mousy%3:01:00::" "mousey%3:01:00::" "filarial%3:01:00::" "soft-finned%3:01:00::" "ambulacral%3:01:00::" "zoological%3:01:01::" "pachydermatous%3:01:00::" "pachydermal%3:01:00::" "pachydermic%3:01:00::" "pachydermous%3:01:00::" "grubby%3:01:00::" "salamandriform%3:01:00::" "ametabolic%3:00:00::" "ametabolous%3:00:00::" "larval%3:01:00::" "fishy%3:01:00::" "anguine%3:01:00::" "mecopterous%3:01:00::" "blastospheric%3:01:00::" "blastular%3:01:00::" "medusoid%3:01:00::" "feline%3:01:00::" "alular%3:01:00::" "digitigrade%3:00:00::" "ornithological%3:01:00::" "raptorial%3:01:00::" "ungual%3:01:01::" "carnivorous%3:01:00::" "entomological%3:01:00::" "entomologic%3:01:00::" "cloven-hoofed%3:01:00::" "cloven-footed%3:01:00::" "insectan%3:01:00::" "zoological%3:01:00::" "neanderthal%3:01:00::" "neanderthalian%3:01:00::" "neandertal%3:01:00::" "boskopoid%3:01:00::" "piscine%3:01:00::" "nonpasserine%3:01:00::" "fetal%3:01:00::" "foetal%3:01:00::" "ovoviviparous%3:00:00::" "simian%3:01:00::" "spiny-finned%3:01:00::")
  )

(define-type ONT::chemistry-property-val
  :parent ONT::chemistry-val
  :wordnet-sense-keys ("membered%3:00:00::" "impure%3:00:02::" "reactive%3:00:00::" "alkaline%3:00:00::" "alkalic%3:00:00::" "unreactive%3:00:00::" "cyclic%3:00:02::" "unsaturated%3:00:00::" "acidic%3:00:00::" "bound%3:00:03::" "oleophilic%3:00:00::" "acyclic%3:00:02::" "open-chain%3:00:00::" "critical%3:00:04::" "hydrophilic%3:00:00::" "adamantine%3:01:00::" "monometallic%3:01:00::" "acidimetric%3:01:00::" "calciferous%3:01:00::" "dicarboxylic%3:01:00::" "boric%3:01:00::" "boracic%3:01:00::" "benzylic%3:01:00::" "bituminoid%3:01:00::" "bitumenoid%3:01:00::" "light%3:00:07::" "mesonic%3:01:00::" "mesic%3:01:00::" "argentic%3:01:00::" "camphoraceous%3:01:00::" "sulphuric%3:01:00::" "sulfuric%3:01:00::" "ammino%3:01:00::" "phosphorous%3:01:00::" "phosphoric%3:01:00::" "valent%3:01:00::" "ferric%3:01:00::" "ferrous%3:01:00::" "colorimetric%3:01:00::" "colorimetrical%3:01:00::" "endergonic%3:00:00::" "ceric%3:01:00::" "dimorphic%3:01:00::" "dimorphous%3:01:00::" "spectroscopic%3:01:00::" "spectroscopical%3:01:00::" "carbolated%3:01:00::" "ammoniac%3:01:00::" "ammoniacal%3:01:00::" "aurous%3:01:00::" "auric%3:01:00::" "stearic%3:01:00::" "nonenzymatic%3:01:00::" "arsenious%3:01:00::" "anhydrous%3:00:00::" "camphoric%3:01:00::" "achlorhydric%3:01:00::" "proteinaceous%3:01:00::" "polymorphous%3:01:01::" "polymorphic%3:01:01::" "iodinated%3:01:00::" "iodized%3:01:00::" "iodised%3:01:00::" "polyvalent%3:00:01::" "multivalent%3:00:04::" "biosynthetic%3:01:00::" "nonsteroidal%3:01:00::" "saturated%3:00:00::" "proteolytic%3:01:00::" "pectic%3:01:00::" "azotic%3:01:00::" "nitric%3:01:00::" "nitrous%3:01:00::" "mentholated%3:01:00::" "exergonic%3:00:00::" "calcareous%3:01:00::" "chalky%3:01:00::" "trihydroxy%3:01:00::" "autocatalytic%3:01:00::" "unmyelinated%3:01:00::" "photosynthetic%3:01:00::" "diamantine%3:01:00::" "aquatic%3:01:00::" "argentous%3:01:00::" "trivalent%3:01:00::" "electronic%3:01:01::" "carbonyl%3:01:00::" "carbonylic%3:01:00::" "antimonic%3:01:00::" "antimonious%3:01:00::" "phreatic%3:01:00::" "allylic%3:01:00::" "macromolecular%3:01:00::" "waxen%3:01:00::" "waxy%3:01:00::" "hydroxy%3:01:00::" "photochemical%3:01:00::" "zymotic%3:01:01::" "zymolytic%3:01:00::" "catalatic%3:01:00::" "bismuthal%3:01:00::" "benzenoid%3:01:00::" "nonphotosynthetic%3:01:00::" "biochemical%3:01:00::" "stannic%3:01:00::" "stannous%3:01:00::" "bichromated%3:01:00::" "chalybeate%3:01:00::" "noncritical%3:00:02::" "bivalent%3:01:01::" "divalent%3:01:00::" "azido%3:01:00::" "telluric%3:01:01::" "nitrogenous%3:01:00::" "nitrogen-bearing%3:01:00::" "hydrous%3:00:00::" "hydrated%3:00:00::" "acetonic%3:01:00::" "cherty%3:01:00::" "bismuthic%3:01:00::" "acetic%3:01:00::" "polymeric%3:01:00::" "caffeinic%3:01:00::" "bromic%3:01:00::" "molecular%3:01:00::" "transuranic%3:01:00::" "cacodylic%3:01:00::" "aleuronic%3:01:00::" "succinic%3:01:00::" "siliceous%3:01:00::" "silicious%3:01:00::" "tinny%3:01:00::" "carbocyclic%3:01:00::" "ethereal%3:01:00::" "benzoic%3:01:00::" "ceruminous%3:01:00::" "saponaceous%3:01:00::" "soapy%3:01:00::" "citric%3:01:00::" "bimolecular%3:01:00::" "monovalent%3:00:01::" "univalent%3:00:04::" "plumbic%3:01:00::" "plumbous%3:01:00::" "inorganic%3:00:01::" "amino%3:01:00::" "aminic%3:01:00::" "boronic%3:01:00::" "ionic%3:01:00::" "iodinating%3:00:00::" "ammoniated%3:01:00::" "albuminous%3:01:00::" "amphoteric%3:00:00::" "amphiprotic%3:00:00::" "amylolytic%3:01:00::" "acetylenic%3:01:00::" "allotropic%3:01:00::" "allotropical%3:01:00::" "adrenergic%3:01:00::" "sympathomimetic%3:01:00::" "physicochemical%3:01:00::" "leaden%3:01:00::" "iridic%3:01:01::" "pyrochemical%3:01:00::" "mercurial%3:01:00::" "fibrinous%3:01:00::" "hydraulic%3:01:01::" "resinated%3:01:00::" "flinty%3:01:00::" "aldehydic%3:01:00::" "formic%3:01:00::" "electrochemical%3:01:00::" "calcic%3:01:00::" "lithic%3:01:01::" "immunochemical%3:01:00::" "mass_spectroscopic%3:01:00::" "arsenical%3:01:00::" "recombinant%3:01:00::" "myelinated%3:01:00::" "medullated%3:01:00::" "zymoid%3:01:00::" "tannic%3:01:00::" "insecticidal%3:01:00::" "cupric%3:01:00::" "cuprous%3:01:00::" "heavy%3:00:07::" "catalytic%3:01:00::" "tetravalent%3:01:00::" "azo%3:01:00::" "chemical%3:01:01::" "sulphuretted%3:01:00::" "sulfurized%3:01:01::" "sulfuretted%3:01:00::" "enolic%3:01:00::" "myelinic%3:01:00::" "aluminous%3:01:00::" "glycogenic%3:01:00::" "sternutatory%3:01:00::" "intramolecular%3:01:00::" "aqueous%3:01:00::" "antiadrenergic%3:01:00::" "thermionic%3:01:00::" "pyrogallic%3:01:00::" "elemental%3:01:00::" "barytic%3:01:00::" "polarographic%3:01:00::" "nonionic%3:01:00::" "nonpolar%3:01:00::" "sulfurous%3:01:00::" "sulphurous%3:01:00::" "enzymatic%3:01:00::" "alkylic%3:01:00::" "de-iodinating%3:00:00::" "baric%3:01:00::" "acetylic%3:01:00::" "rich%3:00:04::" "bituminous%3:01:00::" "intermolecular%3:01:00::" "qualitative%3:01:00::" "pentavalent%3:01:00::" "hydrophobic%3:00:00::" "chelate%3:01:02::" "chelated%3:01:00::" "electrolytic%3:01:01::" "anionic%3:01:00::" "diazo%3:01:00::" "monocarboxylic%3:01:00::" "aromatic%3:01:00::" "tartaric%3:01:00::" "transactinide%3:01:00::" "cerous%3:01:00::" "spectrometric%3:01:00::" "cationic%3:01:00::" "carbonaceous%3:01:00::" "carbonous%3:01:00::" "carbonic%3:01:00::" "carboniferous%3:01:00::" "alkaloidal%3:01:00::" "mercuric%3:01:00::" "mercurous%3:01:00::" "chitinous%3:01:00::" "cellulosid%3:01:00::" "carboxyl%3:01:00::" "carboxylic%3:01:00::")
  )

(define-type ONT::earth-science-val
  :parent ONT::associated-with-science-val
  )

(define-type ONT::earth-science-properpty-val
  :parent ONT::earth-science-val
  :wordnet-sense-keys ("anabatic%3:00:00::" "cyclonic%3:01:01::" "cyclonal%3:01:01::" "cyclonical%3:01:01::" "aerological%3:01:00::" "hydrographic%3:01:00::" "hydrographical%3:01:00::" "paleontological%3:01:00::" "palaeontological%3:01:00::" "katabatic%3:00:00::" "catabatic%3:00:00::" "geographic%3:01:00::" "geographical%3:01:00::" "bioclimatic%3:01:00::" "paleoanthropological%3:01:00::" "meteorologic%3:01:00::" "meteorological%3:01:00::" "meteoric%3:01:02::" "anticyclonic%3:01:00::" "topographical%3:01:00::" "topographic%3:01:00::" "frontal%3:01:02::" "limnological%3:01:00::")
  )

(define-type ONT::geology-val
  :parent ONT::associated-with-science-val
  )

(define-type ONT::geological-property-val
  :parent ONT::geology-val
  :wordnet-sense-keys ("stratified%3:00:00::" "bedded%3:00:04::" "intrusive%3:00:03::" "extrusive%3:00:00::" "riparian%3:01:00::" "basinal%3:01:00::" "montane%3:01:00::" "jurassic%3:01:00::" "eonian%3:01:00::" "aeonian%3:01:00::" "igneous%3:00:00::" "eruptive%3:00:00::" "pre-jurassic%3:01:00::" "cataclinal%3:00:00::" "coastal%3:01:00::" "littoral%3:01:00::" "thermal%3:01:01::" "unstratified%3:00:00::" "cenozoic%3:01:00::" "synclinal%3:00:00::" "alpine%3:01:00::" "mesozoic%3:01:00::" "aquiferous%3:01:00::" "carboniferous%3:01:02::" "anaclinal%3:00:00::" "cretaceous%3:01:00::" "himalayan%3:01:00::" "alpine%3:01:01::" "volcanic%3:01:00::" "anticlinal%3:00:00::" "postdiluvian%3:01:00::" "andean%3:01:00::" "aqueous%3:00:00::" "sedimentary%3:00:00::" "aeolian%3:01:01::" "gibraltarian%3:01:00::" "intertidal%3:01:00::" "glacial%3:01:00::" "paleozoic%3:01:00::" "hadal%3:01:00::" "geological%3:01:00::" "geologic%3:01:00::" "triassic%3:01:00::" "postglacial%3:01:00::" "batholithic%3:01:00::" "batholitic%3:01:00::")
  )

(define-type ONT::physics-property-val
  :parent ONT::physics-val
  :wordnet-sense-keys ("physical%3:01:00::" "astrophysical%3:01:00::" "cosmologic%3:01:00::" "cosmological%3:01:00::" "cosmogonic%3:01:00::" "cosmogonical%3:01:00::" "cosmogenic%3:01:00::" "astronomic%3:01:00::" "astronomical%3:01:00::" "microelectronic%3:01:00::" "kinetic%3:01:00::" "attractive%3:00:02::" "thermal%3:01:00::" "thermic%3:01:00::" "caloric%3:01:00::" "nuclear%3:01:01::" "quantal%3:01:00::" "quantized%3:01:00::" "incoherent%3:00:01::" "mechanical%3:01:00::" "ontological%3:01:00::" "geodetic%3:01:00::" "geodesic%3:01:00::" "geodesical%3:01:00::" "rheologic%3:01:00::" "rheological%3:01:00::" "hydrostatic%3:01:00::" "ballistic%3:01:00::" "refractive%3:01:00::" "refractile%3:01:00::" "holographic%3:01:02::" "frictional%3:01:00::" "electromotive%3:01:00::" "birefringent%3:01:00::" "propulsive%3:01:00::" "self-induced%3:01:00::" "incident%3:01:00::" "fiducial%3:01:01::" "relativistic%3:01:00::" "coherent%3:00:01::" "radio%3:01:00::" "electronic%3:01:00::" "nebular%3:01:00::" "nebulous%3:01:00::" "photoconductive%3:01:00::" "thermodynamic%3:01:00::" "thermodynamical%3:01:00::" "cosmologic%3:01:01::" "cosmological%3:01:01::" "geophysical%3:01:00::" "harmonic%3:01:01::" "dynamic%3:01:02::" "pleochroic%3:01:00::" "acoustic%3:01:00::" "acoustical%3:01:00::" "repulsive%3:00:00::" "aerodynamic%3:01:00::" "geomorphologic%3:01:00::" "geomorphological%3:01:00::" "morphologic%3:01:02::" "morphological%3:01:02::" "structural%3:01:03::" "resistive%3:01:00::" "hydrokinetic%3:01:00::" "hydraulic%3:01:00::" "hydrodynamic%3:01:00::" "chaotic%3:01:00::" "aeronautical%3:01:00::" "aeronautic%3:01:00::" "metaphysical%3:01:00::" "diabatic%3:00:00::" "capacitive%3:01:00::" "photovoltaic%3:01:00::" "avionic%3:01:00::" "gravitational%3:01:00::" "gravitative%3:01:00::" "tensile%3:01:00::" "inertial%3:01:00::" "catoptric%3:01:00::" "catoptrical%3:01:00::" "nonthermal%3:01:00::" "seismological%3:01:00::" "seismologic%3:01:00::" "aeromechanic%3:01:00::" "optical%3:01:01::" "geothermal%3:01:00::" "geothermic%3:01:00::" "high-energy%3:01:00::" "adiabatic%3:00:00::" "pressor%3:01:00::" "magnetic%3:01:00::" "tectonic%3:01:00::" "plane-polarized%3:01:00::")
  )

(define-type ONT::artifact-property-val
  :parent ONT::physics-val
  :wordnet-sense-keys ("electrolytic%3:01:00::" "axial%3:01:00::" "telegraphic%3:01:00::" "forcipate%3:01:00::" "antisatellite%3:01:00::" "asat%3:01:00::" "paramagnetic%3:01:00::" "archival%3:01:00::" "closed-circuit%3:01:00::" "artifactual%3:01:00::" "artefactual%3:01:00::" "viscometric%3:01:00::" "viscosimetric%3:01:00::" "stereoscopic%3:01:01::" "tiered%3:01:00::" "prismatic%3:01:00::" "audiovisual%3:01:00::" "spring-loaded%3:01:00::" "ceramic%3:01:00::" "catapultic%3:01:00::" "catapultian%3:01:00::" "textile%3:01:00::" "interstitial%3:01:00::" "anodic%3:01:00::" "anodal%3:01:00::" "antiquarian%3:01:00::" "funicular%3:01:00::" "spermicidal%3:01:00::" "gyroscopic%3:01:00::" "woolen%3:01:00::" "woollen%3:01:00::" "bladed%3:01:01::" "electromechanical%3:01:00::" "mechanical%3:01:01::" "mechanically_skillful%3:01:00::" "anastigmatic%3:01:00::" "stigmatic%3:01:00::" "three-wheel%3:01:00::" "three-wheeled%3:01:00::" "fisheye%3:01:00::" "wide-angle%3:01:00::" "valved%3:01:00::" "servomechanical%3:01:00::" "servo%3:01:00::" "journalistic%3:01:00::" "mud-brick%3:01:00::" "ropy%3:01:00::" "ropey%3:01:00::" "vehicular%3:01:00::" "coin-operated%3:01:00::" "prosthetic%3:01:00::" "tegular%3:01:00::" "valvular%3:01:00::" "two-wheel%3:01:00::" "two-wheeled%3:01:00::" "four-wheel%3:01:00::" "four-wheeled%3:01:00::" "calico%3:01:00::" "threaded%3:01:00::" "barometric%3:01:00::" "barometrical%3:01:00::" "robotic%3:01:00::" "broadband%3:01:01::" "cathodic%3:01:00::" "cinematic%3:01:00::" "life-support%3:01:00::" "cementitious%3:01:00::" "muzzle-loading%3:01:00::" "biedermeier%3:01:00::" "bolometric%3:01:00::" "vestiary%3:01:00::")
  )

(define-type ONT::psychology-val
  :parent ONT::associated-with-science-val
  :wordnet-sense-keys ("psychological%3:01:00::")
  )

(define-type ONT::psychology-property-val
  :parent ONT::psychology-val
  :wordnet-sense-keys ("nomothetic%3:00:00::" "associational%3:01:00::" "retroactive%3:00:00::" "normal%3:00:03::" "proactive%3:00:00::" "behavioristic%3:01:00::" "behaviorist%3:01:00::" "behaviouristic%3:01:00::" "behaviourist%3:01:00::" "neuropsychological%3:01:00::" "presentational%3:01:00::" "molecular%3:00:00::" "attentional%3:01:00::" "abient%3:01:00::" "adient%3:01:00::" "ambiversive%3:00:00::" "conditioned%3:00:00::" "learned%3:00:02::" "molar%3:00:00::" "psychometric%3:01:00::" "idiographic%3:00:00::")
  )

(define-type ONT::psychological-state-val
  :parent ONT::psychology-val
  :wordnet-sense-keys ("hypnotic%3:01:00::" "anxiolytic%3:01:00::" "comatose%3:01:00::" "amnestic%3:01:00::" "amnesic%3:01:00::" "schizoid%3:01:00::" "schizophrenic%3:01:00::")
  )

(define-type ONT::cognitive-process-val
  :parent ONT::psychology-val
  :wordnet-sense-keys ("cultural%3:01:01::" "inferential%3:01:00::" "illative%3:01:00::" "optative%3:01:01::" "omissive%3:01:00::" "anamnestic%3:01:00::" "mystic%3:01:01::" "mystical%3:01:01::" "oneiric%3:01:00::" "deductive%3:01:00::" "animatistic%3:01:00::" "facultative%3:01:00::")
  )

(define-type ONT::medical-property-val
  :parent ONT::medical-val
  :wordnet-sense-keys ("epidemic%3:00:00::" "local%3:00:02::" "chronic%3:00:00::" "positive%3:00:04::" "confirming%3:00:02::" "active%3:00:02::" "general%3:00:02::" "negative%3:00:04::" "disconfirming%3:00:02::" "malign%3:00:00::" "malignant%3:00:02::" "acute%3:00:00::" "hippocratic%3:01:00::" "epidemiologic%3:01:00::" "epidemiological%3:01:00::" "dermatologic%3:01:00::" "dermatological%3:01:00::" "ophthalmic%3:01:00::" "specific%3:00:02::" "autoplastic%3:01:00::" "nonsurgical%3:01:00::" "allopathic%3:01:00::" "pathological%3:01:00::" "pathologic%3:01:00::" "electroencephalographic%3:01:00::" "periodontic%3:01:00::" "periodontal%3:01:00::" "cardiologic%3:01:00::" "anticoagulative%3:01:00::" "dental%3:01:01::" "therapeutic%3:01:00::" "therapeutical%3:01:00::" "bacteriological%3:01:00::" "bacteriologic%3:01:00::" "orthopedic%3:01:00::" "orthopaedic%3:01:00::" "orthopedical%3:01:00::" "orthodontic%3:01:00::" "catatonic%3:01:00::" "premedical%3:01:00::" "oncological%3:01:00::" "oncologic%3:01:00::" "invasive%3:00:01::" "pediatric%3:01:00::" "paediatric%3:01:00::" "cardiographic%3:01:00::" "psychotherapeutic%3:01:00::" "homeopathic%3:01:00::" "prosthetic%3:01:01::" "nonprescription%3:00:00::" "over-the-counter%3:00:00::" "pharmacological%3:01:00::" "pharmacologic%3:01:00::" "psychiatric%3:01:00::" "psychiatrical%3:01:00::" "prosthodontic%3:01:00::" "clonic%3:01:00::" "chemotherapeutic%3:01:00::" "chemotherapeutical%3:01:00::" "immunological%3:01:00::" "immunologic%3:01:00::" "hematologic%3:01:00::" "haematological%3:01:00::" "hematological%3:01:00::" "virological%3:01:00::" "exodontic%3:01:00::" "electrocardiographic%3:01:00::" "neuropsychiatric%3:01:00::" "pharmaceutical%3:01:01::" "empiric%3:01:00::" "empirical%3:01:00::" "noninvasive%3:00:00::" "antiviral%3:01:00::" "biomedical%3:01:00::" "preclinical%3:01:00::" "presymptomatic%3:01:00::" "pharmaceutical%3:01:00::" "pharmaceutic%3:01:00::" "psychopharmacological%3:01:00::" "psychoanalytical%3:01:00::" "psychoanalytic%3:01:00::" "physiotherapeutic%3:01:00::" "surgical%3:01:00::" "prescription%3:00:00::" "clinical%3:01:00::" "endodontic%3:01:00::" "obstetric%3:01:00::" "obstetrical%3:01:00::" "gynecological%3:01:00::" "gynaecological%3:01:00::" "gynecologic%3:01:00::" "nosocomial%3:01:00::" "veterinary%3:01:00::" "parenteral%3:01:01::" "serologic%3:01:00::" "serological%3:01:00::" "neurotropic%3:01:00::" "toxicological%3:01:00::" "toxicologic%3:01:00::" "antidotal%3:01:00::" "immunotherapeutic%3:01:00::" "geriatric%3:01:00::" "gerontological%3:01:00::" "bronchoscopic%3:01:00::" "medicolegal%3:01:00::" "self-limited%3:01:00::" "aeromedical%3:01:00::" "neurological%3:01:00::" "neurologic%3:01:00::")
  )

(define-type ONT::dental-property-val
  :parent ONT::medical-val
  :wordnet-sense-keys ("uncrowned%3:00:02::" "crowned%3:00:02::")
  )

(define-type ONT::mathematics-val
  :parent ONT::associated-with-science-val
  )

(define-type ONT::mathematical-property-val
  :parent ONT::mathematics-val
  :wordnet-sense-keys ("monotonic%3:00:00::" "monotone%3:00:04::" "inverse%3:00:00::" "linear%3:00:02::" "additive%3:00:04::")
  )

(define-type ONT::geometrical-property-val
  :parent ONT::mathematics-val
  :wordnet-sense-keys ("congruent%3:00:00::")
  )

(define-type ONT::computer-science-val
  :parent ONT::associated-with-science-val
  )

(define-type ONT::compsci-property-val
  :parent ONT::computer-science-val
  :wordnet-sense-keys ("on-line%3:00:00::" "online%3:00:00::" "stored-program%3:01:00::" "incompatible%3:00:02::" "real-time%3:01:00::" "off-line%3:00:00::" "wysiwyg%3:01:00::" "client-server%3:01:00::" "open-source%3:01:00::" "computational%3:01:00::" "stovepiped%3:01:00::")
  )

(define-type ONT::associated-with-academics-val
  :parent ONT::associated-with-val
  )

(define-type ONT::education-val
  :parent ONT::associated-with-academics-val
  :wordnet-sense-keys ("catechetical%3:01:00::" "catechetic%3:01:01::" "instructional%3:01:00::" "pedagogical%3:01:00::" "pedagogic%3:01:00::" "academic%3:01:00::" "curricular%3:01:00::" "acculturational%3:01:00::" "acculturative%3:01:00::" "educational%3:01:00::" "phonic%3:01:02::")
  )

(define-type ONT::academic-discipline-val
  :parent ONT::associated-with-academics-val
  :wordnet-sense-keys ("eschatological%3:01:00::" "sociological%3:01:00::" "metrological%3:01:00::" "anthropological%3:01:00::" "jurisprudential%3:01:00::" "cryptanalytic%3:01:00::" "cryptographic%3:01:00::" "cryptographical%3:01:00::" "cryptologic%3:01:00::" "cryptological%3:01:00::" "epistemic%3:01:00::" "epistemological%3:01:00::" "allometric%3:01:00::" "bionic%3:01:00::" "biosystematic%3:01:00::" "agrologic%3:01:00::" "agrological%3:01:00::" "historical%3:01:00::" "cybernetic%3:01:00::" "ergonomic%3:01:00::" "hermeneutic%3:01:00::" "metallurgical%3:01:00::" "metallurgic%3:01:00::" "philosophic%3:01:00::" "philosophical%3:01:00::" "casuistic%3:01:00::" "casuistical%3:01:00::" "classical%3:01:00::" "dialectic%3:01:00::" "dialectical%3:01:00::" "axiological%3:01:00::" "bibliotic%3:01:00::" "aesthetic%3:01:00::" "esthetic%3:01:00::" "etiological%3:01:01::" "etiologic%3:01:01::" "aetiological%3:01:01::" "aetiologic%3:01:01::" "tectonic%3:01:01::" "architectonic%3:01:00::" "disciplinary%3:01:01::" "agronomic%3:01:00::" "agronomical%3:01:00::" "disciplinary%3:01:00::" "technical%3:01:03::" "technological%3:01:00::" "juridical%3:01:02::" "juridic%3:01:02::" "numerological%3:01:00::" "interdisciplinary%3:01:00::" "eudemonic%3:01:00::" "eudaemonic%3:01:00::" "ethical%3:01:00::" "glottochronological%3:01:00::" "humanist%3:01:00::" "humanistic%3:01:00::" "humane%3:01:01::" "transcendental%3:01:00::")
  )

(define-type ONT::associated-with-spirituality-val
  :parent ONT::associated-with-val
  )

(define-type ONT::spiritual-val
  :parent ONT::associated-with-spirituality-val
  :wordnet-sense-keys ("unworldly%3:00:00::")
  )

(define-type ONT::worldly-val
  :parent ONT::associated-with-spirituality-val
  :wordnet-sense-keys ("worldly%3:00:00::" "secular%3:00:07::" "temporal%3:00:00::")
  )

(define-type ONT::associated-with-entertainment-val
  :parent ONT::associated-with-val
  )

(define-type ONT::sports-property-val
  :parent ONT::associated-with-entertainment-val
  :wordnet-sense-keys ("backhand%3:00:00::" "backhanded%3:00:00::" "forehand%3:00:00::" "forehanded%3:00:00::" "fair%3:00:01::" "foul%3:00:00::" "out%3:00:02::" "overhand%3:00:00::" "overhanded%3:00:00::" "overarm%3:00:00::" "gymnastic%3:01:00::" "agonistic%3:01:00::" "safe%3:00:02::" "onside%3:00:00::" "home%3:00:00::" "offside%3:00:00::" "offsides%3:00:00::" "sporting%3:01:00::" "away%3:00:00::" "athletic%3:01:00::" "underhand%3:00:00::" "underhanded%3:00:00::" "underarm%3:00:00::" "sportive%3:01:00::")
  )

(define-type ONT::recreation-property-val
  :parent ONT::associated-with-entertainment-val
  :wordnet-sense-keys ("aerophilatelic%3:01:00::" "terpsichorean%3:01:00::" "philatelic%3:01:00::" "philatelical%3:01:00::" "pyrrhic%3:01:02::" "recreational%3:01:00::" "avocational%3:01:00::")
  )

(define-type ONT::commercial-property-val
  :parent ONT::commercial-enterprise-val
  :wordnet-sense-keys ("noncommercial%3:00:00::" "tangible%3:00:02::" "corporate%3:01:00::" "piscatorial%3:01:00::" "piscatory%3:01:00::" "nonfinancial%3:01:00::" "hollywood%3:01:00::" "intangible%3:00:02::" "commercial%3:01:00::" "nautical%3:01:02::" "maritime%3:01:00::" "marine%3:01:02::" "apicultural%3:01:00::" "cultural%3:01:02::")
  )

(define-type ONT::economics-val
  :parent ONT::associated-with-val
  )

(define-type ONT::economics-property-val
  :parent ONT::economics-val
  :wordnet-sense-keys ("mercantile%3:01:00::" "microeconomic%3:01:00::" "bolshevik%3:01:00::" "bolshevist%3:01:00::" "bolshevistic%3:01:00::" "macroeconomic%3:01:00::" "national_socialist%3:01:00::" "nazi%3:01:00::" "capitalist%3:01:00::" "capitalistic%3:01:00::" "nazi%3:01:02::" "nominal%3:00:00::" "real%3:00:01::" "deflationary%3:00:00::" "econometric%3:01:00::" "recessionary%3:01:00::" "recessive%3:01:00::" "inflationary%3:00:00::")
  )

(define-type ONT::financial-property-val
  :parent ONT::economics-val
  :wordnet-sense-keys ("denominational%3:01:01::")
  )


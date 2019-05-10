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


;; higher-level type for evaluation
(define-type ont::evaluation-attribute-val
    :parent ont::property-val
    :arguments ((:REQUIRED ONT::FIGURE (?any (F::tangible +))))
    :comment "properties which need an observer to be recognized -- subjective to the observer"
    :sem (F::abstr-obj (F::scale ont::evaluation-scale ))
    )

;; fresh, stale
(define-type ont::freshness-val
 :parent ont::evaluation-attribute-val 
 :comment "relating to how recently an object was made or obtained"
)

(define-type ont::fresh-val
 :parent ont::freshness-val 
 :wordnet-sense-keys ("recent%5:00:00:new:00" "recent%5:00:00:past:00" "new%3:00:00" "fresh%3:00:01" "fresh%3:00:02")
)

(define-type ont::not-fresh-val
 :parent ont::freshness-val 
 :wordnet-sense-keys ("stale%3:00:00")
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
 :wordnet-sense-keys ("expensive%3:00:00" "pricy%5:00:00:expensive:00")
)

(define-type ont::not-expensive-val
 :parent ont::cost-val
 :sem (F::abstr-obj (F::scale ont::not-expensive-scale ))
 :wordnet-sense-keys ("cheap%3:00:00")
)

(define-type ont::no-cost-val
 :parent ont::cost-val
 :wordnet-sense-keys ("free%5:00:00:unpaid:00")
)

; good, bad, terrible, great
(define-type ont::acceptability-val
 :parent ont::evaluation-attribute-val 
 :comment "properties having to do with good vs. bad"
 :sem (F::abstr-obj (F::scale ont::acceptability-scale ))
)

(define-type ont::good
 :parent ont::acceptability-val 
 :wordnet-sense-keys ("adequate%5:00:00:satisfactory:00" "nice%3:00:00" "good%3:00:01" "satisfactory%5:00:00:good:01" "all_right%5:00:00:satisfactory:00" "good%5:00:00:nice:00" "satisfactory%3:00:00" "acceptable%3:00:00" "favorable%3:00:02" "alright%5:00:00:satisfactory:00")
 ; Words: (W::GOOD W::GREAT W::FINE W::NICE W::ACCEPTABLE W::ALRIGHT W::SATISFACTORY W::SUPERB W::OKAY W::OK W::PEACHY W::FAVORABLE W::BEARABLE W::TOLERABLE W::SUPPORTABLE ALL_RIGHT)
 ; Antonym: ONT::bad (W::BAD W::TERRIBLE W::AWFUL W::NASTY W::DREADFUL W::UNACCEPTABLE W::ROTTEN W::UNSUPPORTABLE W::UNBEARABLE W::INTOLERABLE W::INSUFFERABLE W::UNFAVORABLE W::MEDIOCRE W::LOUSY)
 :sem (F::abstr-obj (F::scale ont::goodness-scale))
)

(define-type ont::great-val
 :parent ont::good 
 :wordnet-sense-keys ("superb%5:00:00:good:01" "bang-up%5:00:00:good:01" "great%5:00:01:extraordinary:00" "phenomenal%5:00:00:extraordinary:00" "fantastic%5:00:00:extraordinary:00" "ideal%3:00:00:perfect:00" "perfect%3:00:00" )
)

(define-type ont::bad
 :parent ont::acceptability-val 
 :wordnet-sense-keys ("dirty%5:00:00:nasty:00" "unacceptable%5:00:00:unsatisfactory:00" "unacceptable%3:00:00" "unfavorable%3:00:02" "icky%5:00:00:bad:00" "unfavorable%5:00:00:bad:00" "mediocre%5:00:00:bad:00" "bad%3:00:00" "unsatisfactory%3:00:00")
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
)

(define-type ont::not-tolerable-val
 :parent ont::tolerability-val 
 :wordnet-sense-keys ("impossible%5:00:00:intolerable:00" "unsupportable%5:00:00:intolerable:00" "intolerable%3:00:00" )
)

(define-type ont::tolerable-val
 :parent ont::tolerability-val 
 :wordnet-sense-keys ("bearable%5:00:00:tolerable:00" "tolerable%3:00:00" )
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
 :wordnet-sense-keys ("appropriate%3:00:00" "pat%5:00:00:appropriate:00" "proper%5:00:00:appropriate:00" "suitable%5:00:00:fit:02" "fit%3:00:02")
 :sem (F::abstr-obj (F::scale ont::appropriate-scale))
)

(define-type ont::not-appropriate-val
 :parent ont::appropriateness-val 
 :wordnet-sense-keys ("inappropriate%3:00:00" "improper%5:00:00:inappropriate:00" )
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
 :wordnet-sense-keys ("inconvenient%3:00:00" "awkward%5:00:00:inconvenient:00" )
 :sem (F::abstr-obj (F::scale ont::convenient-scale))
)

;; particular, specific
(define-type ont::specificity-val
 :parent ont::evaluation-attribute-val 
)

(define-type ont::specific-val
 :parent ont::specificity-val 
 :wordnet-sense-keys ("specific%3:00:00" "particular%5:00:00:specific:00" "specified%3:00:00")
)

(define-type ont::general-val
 :parent ont::specificity-val 
 :wordnet-sense-keys ("general%3:00:00" "overall%5:00:00:general:00" "nonspecific%3:00:00" )
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
 :wordnet-sense-keys ("fortunate%3:00:00" "lucky%5:00:00:fortunate:00" )
 ; Words: (W::LUCKY W::FORTUNATE)
 ; Antonym: ONT::unlucky (W::UNFORTUNATE W::UNLUCKY)
 :sem (F::abstr-obj (F::scale ont::luckiness-scale) (F::orientation f::pos))
)

(define-type ont::unlucky
 :parent ont::luckiness-val 
 :wordnet-sense-keys ("unfortunate%3:00:00" "doomed%5:00:00:unfortunate:00" )
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
 :wordnet-sense-keys ("capable%3:00:00" "able%5:00:00:capable:00" "able%5:00:00:competent:00" "competent%3:00:00")
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
 :wordnet-sense-keys ("incompetent%3:00:00" "incapable%3:00:00" "unable%5:00:00:incapable:00" )
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
 :wordnet-sense-keys ("cozy%5:00:00:comfortable:00" "comfortable%3:00:00" )
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
)

(define-type ont::benign-val
 :parent ont::harmfulness-val 
 :wordnet-sense-keys ("harmless%3:00:00" )
)

(define-type ont::harmful-val
 :parent ont::harmfulness-val 
 :wordnet-sense-keys ("damaging%5:00:00:harmful:00" "harmful%3:00:00" "ruinous%5:00:00:harmful:00" )
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
 :wordnet-sense-keys ("secure%3:00:02" "safe%3:00:01" )
 ; Words: (W::SAFE W::SECURE)
 ; Antonym: ONT::DANGEROUS (W::DANGEROUS W::UNSAFE)
 :sem (F::abstr-obj (F::scale ont::safe-scale))
)

; noticeable, noteworthy
(define-type ont::attention-worthy-val
 :parent ont::evaluation-attribute-val 
)

(define-type ont::noticeable
 :parent ont::attention-worthy-val 
 :wordnet-sense-keys ("conspicuous%3:00:00" "noticeable%3:00:00" "marked%5:00:00:noticeable:00" "outstanding%5:00:00:conspicuous:00" "big%5:00:00:conspicuous:00" "obtrusive%3:00:00" )
 ; Words: (W::PROMINENT W::STRIKING W::NOTICEABLE W::PRONOUNCED W::OBTRUSIVE W::CONSPICUOUS)
 ; Antonym: ONT::UNOBTRUSIVE (W::UNOBTRUSIVE W::INCONSPICUOUS W::UNNOTICEABLE)
)

(define-type ont::unnoticeable
 :parent ont::attention-worthy-val 
 :wordnet-sense-keys ("unobtrusive%3:00:00" "obscure%5:00:00:inconspicuous:00" "inconspicuous%3:00:00" )
 ; Words: (W::UNOBTRUSIVE W::INCONSPICUOUS W::UNNOTICEABLE)
 ; Antonym: ONT::OUTSTANDING (W::PROMINENT W::STRIKING W::NOTICEABLE W::PRONOUNCED W::OBTRUSIVE W::CONSPICUOUS)
)

;; (un)important, (un)necessary, (in)significant, central, critical, principal
(define-type ont::importance-val
 :parent ont::evaluation-attribute-val 
 :comment "of primary (i.e., major, significant), secondary (i.e., minor), or no importance"
 :sem (F::abstr-obj (F::scale ont::importance-scale))
)

(define-type ont::primary
 :parent ont::importance-val 
 :wordnet-sense-keys ("chief%5:00:02:important:00" "important%3:00:00" "all-important%5:00:00:important:00" "major%3:00:06" "cardinal%5:00:00:important:00" "basal%5:00:00:essential:00" "crucial%3:00:00" "significant%3:00:00" "imperative%3:00:00")
 ; Words: (W::IMPORTANT W::MAIN W::MAJOR W::NECESSARY W::CENTRAL W::SERIOUS W::SIGNIFICANT W::ESSENTIAL W::PRIMARY W::SENIOR W::CRITICAL W::VITAL W::CRUCIAL W::INDISPENSABLE)
 ; Antonym: ONT::SECONDARY (W::SECONDARY W::MINOR W::JUNIOR W::UNNECESSARY W::UNIMPORTANT W::INSIGNIFICANT)
 :comment "important; of primary importance"
 :sem (F::abstr-obj (F::scale ont::important-scale))
)

(define-type ont::urgent-val
 :parent ont::primary
 :wordnet-sense-keys ("urgent%5:00:00:imperative:00" "serious%5:00:00:critical:03" "critical%3:00:03" "desperate%5:00:00:imperative:00")
 :comment "time-sensitive or critical"
)

(define-type ont::secondary
 :parent ont::importance-val 
 :wordnet-sense-keys ("minor%3:00:06" "junior%3:00:00" "insignificant%5:00:00:minor:06")
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
 :wordnet-sense-keys ("unnecessary%3:00:00" )
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
 :wordnet-sense-keys ("conventional%5:00:00:orthodox:00" "orthodox%3:00:00" "conventional%3:00:00" "traditional%5:00:00:orthodox:00" "conventional%3:00:01")
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
)

(define-type ont::familiar-val
 :parent ont::familiarity-val 
 :wordnet-sense-keys ("familiar%3:00:02" "familiar%3:00:00" "known%3:00:00")
)

(define-type ont::unfamiliar-val
 :parent ont::familiarity-val 
 :wordnet-sense-keys ("unfamiliar%3:00:00" "strange%5:00:01:unfamiliar:00")
)

;; typical (normal, usual, stereotypical); atypical (uncommon, unusual, strange, exceptional, unique)
(define-type ont::typicality-val
 :parent ont::evaluation-attribute-val 
 :comment "abiding by or breaking with customary or usual practices"
 :sem (F::abstr-obj (F::scale ont::typicality-scale))
)

(define-type ont::typical-val
 :parent ont::typicality-val 
 :wordnet-sense-keys ("customary%5:00:00:usual:00" "typical%5:00:00:normal:01" "everyday%5:00:00:ordinary:00" "run-of-the-mill%5:00:00:ordinary:00" "typical%3:00:00" "regular%5:00:00:typical:00" "regular%5:00:00:normal:01" "normal%3:00:01" "common%5:00:00:ordinary:00" "ordinary%5:00:02:common:01" "usual%3:00:00" "ordinary%3:00:00" "standard%5:00:00:common:01" "common%3:00:01" )
 :sem (F::abstr-obj (F::scale ont::typical-scale))
)

(define-type ont::stereotypical-val
 :parent ont::typical-val 
 :wordnet-sense-keys ("stereotypical%5:00:00:conventional:01" "formulaic%5:00:00:conventional:00" "generic%3:01:00" "white-bread%5:00:00:conventional:01")
)

(define-type ont::atypical-val
 :parent ont::typicality-val 
 :wordnet-sense-keys ("uncommon%3:00:00" "unconventional%3:00:01" "unorthodox%5:00:00:unconventional:00" "unusual%3:00:00" "unusual%5:00:00:uncommon:00" "unconventional%3:00:00" "nonstandard%3:00:02" "irregular%3:00:00")
 :sem (F::abstr-obj (F::scale ont::not-typical-scale))
)

(define-type ont::strange
 :parent ont::atypical-val 
 :wordnet-sense-keys ("irregular%5:00:00:abnormal:00" "weird%5:00:00:strange:00" "freaky%5:00:00:strange:00" "bizarre%5:00:00:unconventional:01" "funky%5:00:00:unconventional:01" "atypical%3:00:00" "atypical%5:00:00:abnormal:00" "abnormal%3:00:00" "odd%5:00:00:unusual:00" "curious%5:00:00:strange:00" "strange%3:00:00")
 ; Words: (W::STRANGE W::ODD W::UNUSUAL W::REMARKABLE W::EXTRAORDINARY W::EXCEPTIONAL W::PECULIAR W::WEIRD W::BIZARRE W::UNFAMILIAR W::ABNORMAL W::FUNKY W::UNCONVENTIONAL W::UNCOMMON W::SINGULAR W::FREAKY W::ATYPICAL W::OUTLANDISH W::UNORTHODOX)
 ; Antonym: ONT::COMMON (W::COMMON W::NORMAL W::USUAL W::REGULAR W::ORDINARY W::STANDARD W::FAMILIAR W::TYPICAL W::CONVENTIONAL W::ORTHODOX W::UNREMARKABLE W::UNEXCEPTIONAL W::ROUTINE)
)

(define-type ont::exceptional-val
 :parent ont::atypical-val 
 :wordnet-sense-keys ("remarkable%5:00:00:extraordinary:00" "exceptional%5:00:00:extraordinary:00" "special%5:00:00:uncommon:00" "singular%5:00:00:extraordinary:00" "extraordinary%3:00:00" )
)

(define-type ONT::uniqueness-VAL
 :parent ONT::atypical-val
 )

;; basic, fundamental, inherent
(define-type ont::fundamental-val
 :parent ont::evaluation-attribute-val 
 :comment "forming a necessary base or core"
)

(define-type ont::basic-val
 :parent ont::fundamental-val 
 :wordnet-sense-keys ("basic%3:00:00" "fundamental%5:00:00:basic:00" "elementary%5:00:00:basic:00" "essential%3:00:00")
)

(define-type ont::intrinsic-val
 :parent ont::fundamental-val 
 :wordnet-sense-keys ("built-in%5:00:00:intrinsic:00" "intrinsic%3:00:00" )
)

(define-type ont::implicit-val
 :parent ont::fundamental-val 
 :wordnet-sense-keys ("underlying%5:00:00:implicit:00" "implicit%3:00:00" )
)

;; superior, inferior
(define-type ont::quality-val
 :parent ont::evaluation-attribute-val 
)

(define-type ont::superior-val
 :parent ont::quality-val 
 :wordnet-sense-keys ("premium%5:00:00:superior:02" "superior%3:00:02" "supreme%5:00:00:superior:02" "sterling%5:00:00:superior:02" )
)

(define-type ont::substandard-val
 :parent ont::quality-val 
 :wordnet-sense-keys ("second-rate%5:00:00:inferior:02" "low-grade%5:00:00:inferior:02" "substandard%5:00:00:nonstandard:02" "inferior%3:00:02" )
)

;; dependable, reliable
(define-type ont::reliability-val
 :parent ont::evaluation-attribute-val
)

(define-type ont::reliable
 :parent ont::reliability-val 
 :wordnet-sense-keys ("trustworthy%3:00:00" "dependable%5:00:00:trustworthy:00" "reliable%3:00:00" )
 ; Words: (W::RELIABLE W::TRUSTWORTHY W::DEPENDABLE)
 ; Antonym: ONT::UNRELIABLE (W::UNCERTAIN W::UNRELIABLE)
 :sem (F::abstr-obj (F::scale ont::reliability-scale) (f::orientation f::pos))
)

(define-type ont::unreliable
 :parent ont::reliability-val 
 :wordnet-sense-keys ("uncertain%5:00:00:unreliable:00" "unreliable%3:00:00" )
 ; Words: (W::UNCERTAIN W::UNRELIABLE)
 ; Antonym: ONT::RELIABLE (W::RELIABLE W::TRUSTWORTHY W::DEPENDABLE)
 :sem (F::abstr-obj (F::scale ont::reliability-scale) (f::orientation f::neg))
)


;; worthy/valuable, unworthy/worthless
(define-type ont::worthiness-val
 :parent ont::evaluation-attribute-val 
 :comment "evaluation attributes dealing with the value or worth of something"
)

(define-type ont::worthy-val
 :parent ont::worthiness-val 
 :wordnet-sense-keys ("worthwhile%5:00:00:worthy:00" "worthy%3:00:00" "valuable%5:00:00:worthy:00" "worthy%5:00:00:fit:02")
)

(define-type ont::not-worthy-val
 :parent ont::worthiness-val 
 :wordnet-sense-keys ("unworthy%3:00:00" "worthless%3:00:00" )
)

;; plain vs. fanciful
(define-type ont::plainness-val
 :parent ont::evaluation-attribute-val 
 :comment "evaluation attributes dealing with plainess or elaborateness of something"
)

(define-type ont::not-plain-val
 :parent ont::plainness-val 
 :wordnet-sense-keys ("fanciful%5:00:00:fancy:00" "fancy%3:00:00" "elaborate%5:00:00:fancy:00" )
)

(define-type ont::plain-val
 :parent ont::plainness-val 
 :wordnet-sense-keys ("plain%3:00:01" "stark%5:00:00:plain:01" )
)

(define-type ont::mere-val
 :parent ont::plainness-val 
 :wordnet-sense-keys ("mere%5:00:00:specified:00")
 :comment "being nothing other than what's specified"
)

;; judgement
(define-type ont::judgement-val
 :parent ont::evaluation-attribute-val
)

;; morality, righteousness, virtuousness, principle, purity
(define-type ont::morality-val
 :parent ont::judgement-val
 :comment "characterized by morality, righteousness, virtuousness, and principle"
)

(define-type ont::moral-val
 :parent ont::morality-val 
 :wordnet-sense-keys ("moral%3:00:00" "virtuous%3:00:00" "chaste%3:00:00" "good%3:00:02" "incorrupt%3:00:00" "principled%3:00:00" "pure%3:00:01" "regenerate%3:00:00" "right%3:00:01" "righteous%3:00:00" "straight%3:00:04")
)

(define-type ont::not-moral-val
 :parent ont::morality-val
 :wordnet-sense-keys ( "immoral%3:00:00" "immoral%3:00:00:wrong:01" "unprincipled%3:00:00" "unregenerate%3:00:00" "wicked%3:00:00" "corrupt%3:00:00" "crooked%3:00:02" "evil%3:00:00" "illicit%3:00:00" "impure%3:00:01" "licit%3:00:00" "wrong%3:00:01" "unchaste%3:00:00")
)

;; fairness, partiality, bias, and objectivity
(define-type ont::bias-val
 :parent ont::judgement-val
 :comment "characterized by fairness, partiality, bias and objectivity"
)

(define-type ont::biased-val
 :parent ont::bias-val
 :wordnet-sense-keys ("subjective%3:00:00" "partial%3:00:01")
)

(define-type ont::prejudiced-val
 :parent ont::biased-val
 :wordnet-sense-keys ("prejudiced%3:00:00" "judgmental%3:00:00")
 :comment "bias characterized by dislike or distrust"
)

(define-type ont::not-biased-val
 :parent ont::bias-val
 :wordnet-sense-keys ("impartial%3:00:00" "objective%3:00:00" "fair%3:00:03" "unprejudiced%3:00:00" "nonjudgmental%3:00:00")
)

;; chic, classy, stylish vs. unstylish, tasteless
(define-type ont::aesthetic-judgement-val
 :parent ont::judgement-val 
)

(define-type ont::good-aesthetic-judgement-val
 :parent ont::aesthetic-judgement-val 
 :wordnet-sense-keys ("chic%5:00:00:stylish:00" "classy%5:00:00:stylish:00" "stylish%3:00:00" "tasteful%3:00:02" "elegant%3:00:00" "fashionable%3:00:00")
)

(define-type ont::bad-aesthetic-judgement-val
 :parent ont::aesthetic-judgement-val 
 :wordnet-sense-keys ("unstylish%3:00:00" "tasteless%3:00:02" "unfashionable%3:00:00")
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
)

(define-type ont::offensive-val
 :parent ont::offensiveness-val 
 :wordnet-sense-keys ("offensive%3:00:04" "offensive%3:00:02" "offensive%3:00:01" "distasteful%5:00:00:offensive:01" "wicked%5:00:00:offensive:01")
)

(define-type ont::not-offensive-val
 :parent ont::offensiveness-val 
 :wordnet-sense-keys ("inoffensive%3:00:01" )
)

;; neat, tidy, (dis)orderly
(define-type ont::orderliness-val
 :parent ont::evaluation-attribute-val 
)

(define-type ont::tidy-val
 :parent ont::orderliness-val 
 :wordnet-sense-keys ("tidy%5:00:00:groomed:00" "tidy%3:00:00" "neat%5:00:00:tidy:00" "uncluttered%5:00:00:tidy:00" "groomed%3:00:00")
)

(define-type ont::messy-val
 :parent ont::orderliness-val 
 :wordnet-sense-keys ("messy%5:00:00:untidy:00" "untidy%3:00:00" )
)

;; free vs. bound
(define-type ont::freedom-val
 :parent ont::evaluation-attribute-val 
)

(define-type ont::free-val
 :parent ont::freedom-val 
 :wordnet-sense-keys ("free%3:00:01" "loose%5:00:01:free:00" "free%3:00:00")
)

(define-type ont::not-free-val
 :parent ont::freedom-val 
 :wordnet-sense-keys ("servile%5:00:00:unfree:01" "unfree%3:00:01" "stuck%3:00:00" "bound%5:00:00:unfree:00" "unfree%3:00:00")
)

;; recommendable
(define-type ont::recommendability-val
 :parent ont::evaluation-attribute-val
)

(define-type ont::recommendable-val
 :parent ont::recommendability-val
 :wordnet-sense-keys ("advisable%3:00:00::")
)

;; real vs. fake
(define-type ont::real-vs-fake-val
 :parent ont::evaluation-attribute-val
)

(define-type ont::artificiality-val
 :parent ont::real-vs-fake-val 
 :comment "artificial vs. natural"
)

(define-type ont::artificial
 :parent ont::artificiality-val 
 :wordnet-sense-keys ("synthetic%5:00:00:artificial:00" "false%5:00:00:artificial:00" "artificial%3:00:00" "artificial%5:00:00:affected:01" "unreal%3:00:04" "faux%5:00:00:artificial:00" "imitation%5:00:02:artificial:00" "fake%5:00:00:artificial:00" "affected%3:00:01" "unnatural%3:00:00")
)

;; natural, unnatural
(define-type ont::natural
 :parent ont::artificiality-val 
 :wordnet-sense-keys ("natural%3:00:02" )
 :arguments ((:REQUIRED ONT::FIGURE ((? lof f::phys-obj f::abstr-obj f::situation )))) 
)

(define-type ont::actuality-val
 :parent ont::real-vs-fake-val 
 ;:arguments ((:REQUIRED ONT::FIGURE (F::proposition (F::information F::mental-construct )))) 
 :comment "existing or occuring in fact vs. imagined or supposed"
)

;; real, actual
(define-type ont::actual
 :parent ont::actuality-val 
 :wordnet-sense-keys ("real%3:00:00" "actual%5:00:00:real:00" "concrete%3:00:00")
)

;; fake, imaginary, hypothetical
;; see also ont::evaluation-val?
(define-type ont::nonactual
 :parent ont::actuality-val 
 :wordnet-sense-keys ("virtual%5:00:00:essential:00" "imaginary%5:00:00:unreal:00" "theoretical%3:00:00" "hypothetical%5:00:00:theoretical:00" "unreal%3:00:00" "abstract%3:00:00")
)

(define-type ont::authenticity-val
 :parent ont::real-vs-fake-val 
 :comment "truly what it is said to be vs. made as imitation"
)

(define-type ont::fake-val
 :parent ont::authenticity-val 
 :wordnet-sense-keys ("phoney%5:00:00:counterfeit:00" "fake%5:00:00:counterfeit:00" "counterfeit%3:00:00")
)

(define-type ont::authentic-val
 :parent ont::authenticity-val 
 :wordnet-sense-keys ("genuine%3:00:00" "genuine%5:00:00:true:00" "authentic%5:00:00:genuine:00" "actual%5:00:00:true:00" "real%5:00:00:true:00" "true%5:00:00:real:02" "real%3:00:02")
)

;; INFORMATION PROPERTY
(define-type ont::information-property-val
 :parent ont::property-val
 :comment "properties regarding the evaluation of information, knowledge, or understanding (e.g. credible, correct, consistent)"
 :sem (F::abstr-obj (F::scale ont::information-property-scale))
)



(define-type ont::precision-val
 :parent ont::information-property-val 
)

(define-type ont::not-precise-val
 :parent ont::precision-val 
 :wordnet-sense-keys ("imprecise%3:00:00" "vague%3:00:04" )
)

(define-type ont::precise
 :parent ont::precision-val 
 :wordnet-sense-keys ("dead%5:00:00:precise:00" "precise%3:00:00" )
 ; Words: (W::DEAD W::PRECISE)
 ; Antonym: NIL (W::IMPRECISE)
)

;; meaningful, meaningless
(define-type ont::meaningfulness-val
 :parent ont::information-property-val
)

(define-type ont::meaningful-val
 :parent ont::meaningfulness-val 
 :wordnet-sense-keys ("meaningful%3:00:00" )
)

(define-type ont::not-meaningful-val
 :parent ont::meaningfulness-val 
 :wordnet-sense-keys ("meaningless%3:00:00" )
)


;; credibility
(define-type ont::credibility-val
 :parent ont::information-property-val 
)

(define-type ont::credible-val
 :parent ont::credibility-val 
 :wordnet-sense-keys ("credible%3:00:00" "believable%3:00:04" )
)

(define-type ont::not-credible-val
 :parent ont::credibility-val 
 :wordnet-sense-keys ("unbelievable%3:00:04" "incredible%3:00:00" )
)

;; likely, unlikely, probably
(define-type ont::likelihood-val
 :parent ont::information-property-val
 :comment "likelihood to hold true"
 :sem (F::abstr-obj (F::scale ont::likelihood-scale))
)

(define-type ont::likely-val
 :parent ont::likelihood-val
 :wordnet-sense-keys ("likely%3:00:04" "probable%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::likely-scale))
 )

(define-type ont::at-risk-val
 :parent ont::likely-val
 )

(define-type ont::not-likely-val
 :parent ont::likelihood-val
 :wordnet-sense-keys ("unlikely%3:00:04" "improbable%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::not-likely-scale))
)

(define-type ont::predictability-val
 :parent ont::information-property-val 
)

(define-type ont::predictable-val
 :parent ont::predictability-val 
 :wordnet-sense-keys ("predictable%3:00:00" "foreseeable%5:00:00:predictable:00" )
)

(define-type ont::not-predictable-val
 :parent ont::predictability-val 
 :wordnet-sense-keys ("unpredictable%3:00:00" "unforeseeable%5:00:00:unpredictable:00" )
)

;; understandable
(define-type ont::comprehensibility-val
 :parent ont::information-property-val
 :comment "able to be grasped or understood (different from ont::clarity-val e.g., an explanation might be clear but it still may not be understandable because of external reasons"
)

(define-type ont::comprehensible-val
 :parent ont::comprehensibility-val
 :wordnet-sense-keys ("friendly%3:00:03" "comprehensible%3:00:00")
)

(define-type ont::not-comprehensible-val
 :wordnet-sense-keys ("incomprehensible%3:00:00" "incomprehensible%3:00:04" "unfriendly%3:00:02")
)

; obvious, obscure apparent
(define-type ont::clarity-val
 :parent ont::information-property-val 
 :comment "clear, obvious vs. unclear, obscure"
)

(define-type ont::clear
 :parent ont::clarity-val 
 :wordnet-sense-keys ("apparent%5:00:00:obvious:00" "obvious%3:00:00" "clear%3:00:00")
 ; Words: (w::clear W::OBVIOUS W::EVIDENT)
 ; Antonym: ONT::unclear (W::UNOBVIOUS)
)

(define-type ont::unclear
 :parent ont::clarity-val 
 :wordnet-sense-keys ("unclear%3:00:00" "opaque%5:00:00:incomprehensible:00" "ill-defined%3:00:00" "obscure%5:00:00:unclear:00" )
 ; Words: (w::unobvious W::UNCLEAR W::OBSCURE W::OPAQUE)
 ; Antonym: ONT::clear (W::CLEAR w::obvious w::evident)
)

;; consistent in the logical sense
;; consistent, inconsistent
(define-type ont::consistent-val
 :parent ont::information-property-val 
 :arguments ((:REQUIRED ONT::FIGURE ((? lof f::abstr-obj f::situation )))(:REQUIRED ONT::GROUND ((? vl f::abstr-obj f::situation )))) 
 :comment "marked by a (il)logical or (dis)orderly consistent relation of parts"
)

(define-type ont::consistent
 :parent ont::consistent-val 
 :wordnet-sense-keys ("consistent%3:00:00" "consistent%3:00:01" )
)

(define-type ont::inconsistent
 :parent ont::consistent-val 
 :wordnet-sense-keys ("inconsistent%3:00:00" "inconsistent%5:00:00:irreconcilable:00" )
)

;;; Myrosia 09/23/03 - a very bad name, really. This is for adjectives like "confused", "mixed up", etc - changed LF_Mistaken to LF_Correctness
(define-type ont::correctness-val
 :parent ont::information-property-val 
 :comment "describing the quality of being error-free or error-prone"
 :sem (F::abstr-obj (F::scale ont::correctness-scale))
)

(define-type ont::correct
 :parent ont::correctness-val 
 :wordnet-sense-keys ("proper%3:00:00" "correct%5:00:00:proper:00" "correct%3:00:00" "accurate%5:00:00:correct:00" )
 ; Words: (W::PROPER W::CORRECT W::ACCURATE)
 ; Antonym: ONT::incorrect (W::MISTAKEN W::INACCURATE W::INCORRECT)
 :sem (F::abstr-obj (F::scale ont::correctness-scale) (F::orientation f::pos))
)

(define-type ont::incorrect
 :parent ont::correctness-val 
 :wordnet-sense-keys ("faulty%5:00:00:inaccurate:00" "false%5:00:00:incorrect:00" "incorrect%3:00:00" "inaccurate%3:00:00" )
 ; Words: (W::MISTAKEN W::INACCURATE W::INCORRECT)
 ; Antonym: ONT::correct (W::PROPER W::CORRECT W::ACCURATE)
 :sem (F::abstr-obj (F::scale ont::correctness-scale) (F::orientation f::neg))
)

(define-type ont::validity-val
 :parent ont::information-property-val 
)

(define-type ont::invalid-val
 :parent ont::validity-val 
 :wordnet-sense-keys ("invalid%3:00:00" )
)

(define-type ont::valid-val
 :parent ont::validity-val 
 :wordnet-sense-keys ("legitimate%5:00:00:valid:00" "valid%3:00:00" "logical%5:00:00:valid:00" )
)

(define-type ont::relevance-val
 :parent ont::information-property-val 
)

(define-type ont::relevant
 :parent ont::relevance-val 
 :wordnet-sense-keys ("pertinent%5:00:00:relevant:00" "applicable%5:00:00:relevant:00" "relevant%3:00:00" )
 ; Words: (W::RELEVANT W::APPLICABLE W::PERTINENT)
 ; Antonym: NIL (W::IRRELEVANT)
)

(define-type ont::not-relevant-val
 :parent ont::relevance-val 
 :wordnet-sense-keys ("irrelevant%3:00:00" )
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
)

(define-type ont::verifiable-val
 :parent ont::verifiability-val
 :wordnet-sense-keys ("verifiable%5:00:00:objective:00" "provable%5:00:00:obvious:00")
)

(define-type ont::not-verifiable-val
 :parent ont::verifiability-val
 :wordnet-sense-keys ("unverifiable%5:00:00:subjective:00")
)

;; PHYSICAL-PROPERTY
(define-type ont::physical-property-val
 :parent ont::property-val 
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
 :wordnet-sense-keys ("flexible%3:00:01" "pliant%5:00:00:flexible:01")
)

;; loose vs. tight
(define-type ont::constricting-val
 :parent ont::configuration-property-val 
 :comment "describes how constricting something is with regards to another item (close-fitting vs. loose-fitting)"
 :sem (F::abstr-obj (F::scale ont::constriction-scale))
)

(define-type ont::loose-val
 :parent ont::constricting-val 
 :wordnet-sense-keys ("loose%3:00:01" "loose%5:00:00:coarse:00")
 :sem (F::abstr-obj (F::scale ont::looseness-scale))
)

(define-type ont::not-loose-val
 :parent ont::constricting-val 
 :wordnet-sense-keys ("snug%5:00:00:tight:01" "tight%3:00:01" "choky%5:00:00:tight:01"  "isotonic%5:00:00:tense:03")
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
 :wordnet-sense-keys ("loose%3:00:04" )
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
 :wordnet-sense-keys ("obstructed%3:00:00" )
)

(define-type ont::unobstructed
 :parent ont::flow-val 
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
 :wordnet-sense-keys ("homogeneous%3:00:00" )
)

(define-type ont::not-homogeneous-val
 :parent ont::homogeneity-val 
 :wordnet-sense-keys ("heterogeneous%3:00:00" )
)

;; clothed vs. naked
(define-type ont::clothedness-val
 :parent ont::configuration-property-val 
)

(define-type ont::clothed-val
 :parent ont::clothedness-val 
 :wordnet-sense-keys ("clothed%3:00:00" )
)

(define-type ont::not-clothed-val
 :parent ont::clothedness-val 
 :wordnet-sense-keys ("bare%5:00:00:unclothed:00" "unclothed%3:00:00" "naked%5:00:00:bare:00")
)

;; adorned vs. unadorned
(define-type ont::adornment-val
 :parent ont::configuration-property-val 
 :comment "having or missing adornment"
)

(define-type ont::adorned-val
 :parent ont::adornment-val 
 :wordnet-sense-keys ("adorned%3:00:00" )
)

(define-type ont::unadorned
 :parent ont::adornment-val 
 :wordnet-sense-keys ("bare%5:00:00:unadorned:00" "plain%5:00:00:unadorned:00" "unadorned%3:00:00" "bare%3:00:00" )
 ; Words: (W::BARE W::NAKED W::UNADORNED)
 ; Antonym: nil
)


;; substantial properties
(define-type ont::substantial-property-val
 :parent ont::physical-property-val 
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
)

;;; noisy (data, signal)
(define-type ont::interference-val
 :parent ont::substantial-property-val 
)

;; optical, magnetic, holographic
(define-type ont::medium
 :parent ont::substantial-property-val 
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
)

(define-type ont::automatic
 :parent ont::mode-of-control-val 
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
 :wordnet-sense-keys ("clement%3:00:02")
)

(define-type ont::inclement-weather-val
 :parent ont::atmospheric-val
 :wordnet-sense-keys ("stormy%3:00:00")
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
 :wordnet-sense-keys ("clear%3:00:03" "fair%5:00:00:clear:03" "liquid%5:00:00:clear:02")
 ; Words: (W::CLEAR W::FAIR)
 ; Antonym: ONT::CLOUDY (W::SMOGGY W::OVERCAST W::FOGGY W::HAZY W::CLOUDY)
)

(define-type ont::cloudy
 :parent ont::atmospheric-val 
 :wordnet-sense-keys ("cloud-covered%5:00:00:cloudy:00" "cloudy%3:00:00" "brumous%5:00:00:cloudy:00" "smoggy%5:00:00:cloudy:00" )
 ; Words: (W::SMOGGY W::OVERCAST W::FOGGY W::HAZY W::CLOUDY)
 ; Antonym: ONT::CLEAR-WEATHER (W::CLEAR W::FAIR)
)

;; hot, cold
(define-type ont::temperature-val
 :parent ont::physical-property-val 
 :sem (F::abstr-obj (F::scale ont::temperature-scale ))
 :comment "having to do with temperature"
)

(define-type ont::cold
 :parent ont::temperature-val 
 :wordnet-sense-keys ("cool%3:00:01" "cold%5:00:00:cool:03" "cool%3:00:03" )
 ; Words: (W::COLD W::COOL)
 ; Antonym: ONT::WARM (W::HOT W::WARM)
 :sem (F::abstr-obj (F::scale ont::cold-scale ))
)

(define-type ont::warm
 :parent ont::temperature-val 
 :wordnet-sense-keys ("warm%3:00:01" "hot%5:00:00:warm:03" "warm%3:00:03" )
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
 :wordnet-sense-keys ("dry%3:00:01" )
 :sem (F::abstr-obj (F::scale ont::dry-scale ))
)

(define-type ont::dehydrated-val
 :parent ont::dry-val
 :wordnet-sense-keys("withered%5:00:00:dry:01" "dehydrated%5:00:00:preserved:02")
 :sem (F::abstr-obj (F::scale ont::dehydrated-scale ))
)

(define-type ont::wet-val
 :parent ont::moisture-content-val 
 :wordnet-sense-keys ("wet%3:00:01" )
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
 :wordnet-sense-keys ("noisy%3:00:00" "loud%3:00:00" )
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
 :sem (F::Abstr-obj (F::MEasure-function F::VALUE ))
 :sem (F::abstr-obj (F::scale ont::texture-scale))
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
 :sem (F::Abstr-obj (F::scale ONT::hardness*1--07--00 ))
 :sem (F::abstr-obj (F::scale ont::tactile-hardness-scale))
)

; soft skin/cloth/body/tissue/bed/seat/chair
(define-type ont::soft-val
 :parent ont::hardness-val 
 :wordnet-sense-keys ("soft%3:00:01" "fluffy%5:00:00:soft:01" "plushy%5:00:00:coarse:00")
 :sem (F::Abstr-obj (F::scale ONT::softness*1--07--00 ))
 :sem (F::abstr-obj (F::scale ont::tactile-softness-scale))
)

(define-type ont::smoothness-val
 :parent ont::texture-val 
)

(define-type ont::not-smooth-val
 :parent ont::smoothness-val 
 :wordnet-sense-keys ("rough%3:00:00" "granular%5:00:00:coarse:00" "coarse%3:00:00")
 :sem (F::abstr-obj (F::scale ont::roughness-scale))
)

(define-type ont::smooth-val
 :parent ont::smoothness-val 
 :wordnet-sense-keys ("seamless%5:00:00:smooth:00" "smooth%3:00:00" )
; :sem (F::Abstr-obj (F::scale ONT::smoothness*1--07--00 ))
 :sem (F::abstr-obj (F::scale ont::smoothness-scale))
)

(define-type ont::sticky-val
 :parent ont::texture-val 
 :wordnet-sense-keys ("glutinous%5:00:00:adhesive:00" "gooey%5:00:00:adhesive:00" "gum-like%5:00:00:adhesive:00" "gummed%5:00:00:adhesive:00" "tarry%5:00:00:adhesive:00" "sticky%5:00:01:adhesive:00")
)

;; taste properties
(define-type ont::taste-property-val
 :parent ont::appearance-property-val 
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Phys-obj (F::OBJECT-FUNCTION F::COMESTIBLE))))
 :sem (F::abstr-obj (F::scale ont::taste-scale))
 :comment "subjective characteristics of an item perceived through gustatory input"
)

(define-type ont::bitter-val
 :parent ont::taste-property-val 
 :wordnet-sense-keys ("bitter%5:00:00:tasty:00" )
; :sem (F::Abstr-obj (F::scale ONT::bitter*1--07--00 ))
 :sem (F::abstr-obj (F::scale ont::bitterness-scale))
)

(define-type ont::spicy-val
 :parent ont::taste-property-val 
 :wordnet-sense-keys ("spicy%5:00:01:tasty:00" "spicy%5:00:00:tasty:00" )
 :sem (F::abstr-obj (F::scale ont::spiciness-scale))
)

(define-type ont::salty-val
 :parent ont::taste-property-val 
 :wordnet-sense-keys ("salty%5:00:00:tasty:00" )
)

(define-type ont::sour-val
 :parent ont::taste-property-val 
 :wordnet-sense-keys ("sour%5:00:00:tasty:00" )
; :sem (F::Abstr-obj (F::scale ONT::sourness*1--07--00 ))
 :sem (F::abstr-obj (F::scale ont::sourness-scale))
)

(define-type ont::sweet-val
 :parent ont::taste-property-val 
 :wordnet-sense-keys ("sweet%3:00:02" )
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
 :comment "subjective characteristics of an item perceived through visual input"
 :sem (F::abstr-obj (F::scale ont::visual-scale))
)

;; color saturation: dark blue, light green
(define-type ont::color-saturation-val
 :parent ont::visual-property-val 
)

(define-type ont::light-in-color-val
 :parent ont::color-saturation-val 
 :wordnet-sense-keys ("light%3:00:05" )
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
 :wordnet-sense-keys ("light%3:00:06" )
; :sem (F::abstr-obj (F::scale ont::luminosity-scale ))
 :sem (F::abstr-obj (F::scale ont::lightness-scale ))
)

(define-type ont::dark-val
 :parent ont::presense-of-light-val 
 :wordnet-sense-keys ("dark%3:00:01")
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
 :wordnet-sense-keys ("dim%5:00:00:dark:01" "dim%3:00:02")
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
 :wordnet-sense-keys ("chromatic%3:00:00" "achromatic%3:00:00")
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
 :parent ont::sensory-property-val 
 :sem (F::abstr-obj (F::scale ont::tactile-scale))
)

(define-type ont::tangible-val
 :parent ont::tangibility-val 
 :wordnet-sense-keys ("palpable%3:00:00" "tangible%3:00:04" "tangible%3:00:00" )
; :sem (F::abstr-obj (F::scale ont::tactile-scale) (F::orientation f::pos))
 :sem (F::abstr-obj (F::scale ont::tangibility-scale))
)

(define-type ont::intangible-val
 :parent ont::tangibility-val 
 :wordnet-sense-keys ("intangible%3:00:00" )
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
 :wordnet-sense-keys ("visible%3:00:00" )
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
 :wordnet-sense-keys ("concealed%3:00:00" "concealed%5:00:00:invisible:00")
 ; Words: (W::HIDDEN W::INVISIBLE W::OBSCURE)
 ; Antonym: NIL (W::VISIBLE)
)

;; properties relating to ability to perceive
(define-type ont::perceptibility-val
 :parent ont::sensory-property-val
)

(define-type ont::perceptible-val
 :parent ont::perceptibility-val 
 :wordnet-sense-keys ("perceptible%3:00:00" )
)

(define-type ont::imperceptible-val
 :parent ont::perceptibility-val 
 :wordnet-sense-keys ("imperceptible%3:00:00" )
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
)

(define-type ont::partial-incomplete
 :parent ont::completeness-val 
 :wordnet-sense-keys ("partial%5:00:00:incomplete:00" "incomplete%3:00:00" )
)

(define-type ont::whole-complete
 :parent ont::completeness-val 
 :wordnet-sense-keys ("complete%3:00:00" "whole%3:00:00" )
)

;; left over
(define-type ont::remaining-val
 :parent ont::part-whole-val 
 :wordnet-sense-keys ("left%5:00:00:unexhausted:00" "remaining%5:00:00:unexhausted:00" )
 :comment "remaining after reaching a complete whole"
)

;; divided
(define-type ont::split-val
 :parent ont::part-whole-val 
 :wordnet-sense-keys ("split%5:00:00:divided:00" "divided%3:00:00")
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
 :wordnet-sense-keys ("charged%3:00:00")
 :sem (F::abstr-obj (F::scale ont::polarity-scale))
)

(define-type ont::polarity-val-positive
 :parent ont::polarity-val 
 :wordnet-sense-keys ("positive%3:00:05" "positively%4:02:02" "positive%5:00:00:charged:00")
 :sem (F::abstr-obj (F::scale ont::polarity-scale) (f::orientation f::pos))
)

(define-type ont::polarity-val-negative
 :parent ont::polarity-val 
 :wordnet-sense-keys ("negative%3:00:05" "negatively%4:02:00" "negative%5:00:00:charged:00")
 :sem (F::abstr-obj (F::scale ont::polarity-scale) (f::orientation f::neg))
)

;; body-related-property-val
(define-type ont::body-condition-property-val
 :parent ont::physical-property-val 
 :comment "properties having to do with with conditions of the human/animal body; note, these adjectives are general purpose. for medical usages see ont::medical-condition-property-val"
 :sem (F::abstr-obj (F::scale ont::body-condition-scale) )
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Phys-obj (f::origin (? org f::human f::non-human-animal)))))
)

;; restless, relaxed
(define-type ont::restlessness-val
 :parent ont::body-condition-property-val 
 :sem (F::abstr-obj (F::scale ont::restlessness-scale))
)

(define-type ont::restless-val
 :parent ont::restlessness-val 
 :wordnet-sense-keys ("restless%3:00:00" "edgy%5:00:00:tense:03" "nervy%5:00:00:tense:03" "jumpy%5:00:00:tense:03" "jittery%5:00:00:tense:03" "antsy%5:00:00:tense:03")
 :sem (F::abstr-obj (F::scale ont::restlessness-scale) (f::orientation f::pos))
)

(define-type ont::relaxed-val
 :parent ont::restlessness-val 
 :wordnet-sense-keys ("relaxed%3:00:00" "restful%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::restlessness-scale) (f::orientation f::neg))
)

;; alert vs drowsy
(define-type ont::alertness-val
 :parent ont::body-condition-property-val 
)

(define-type ont::alert-val
 :parent ont::alertness-val 
 :wordnet-sense-keys ("alert%5:00:00:aware:00" "alert%5:00:00:energetic:00" )
)

(define-type ont::drowsy-val
 :parent ont::alertness-val 
 :wordnet-sense-keys ("drowsy%5:00:00:inattentive:00" )
 :sem (F::abstr-obj (F::scale ont::sleepiness-scale))
)

;; awake vs asleep
(define-type ont::awakeness-val
 :parent ont::body-condition-property-val 
)

(define-type ont::asleep-val
    :parent ont::awakeness-val
    :definitions (ont::not (ont::awake-val :figure ?neutral))
    :wordnet-sense-keys ("asleep%4:02:00" )
    )

(define-type ont::awake-val
    :parent ont::awakeness-val
    :definitions (ont::not (ont::asleep-val :figure ?neutral))
    :wordnet-sense-keys ("awake%3:00:00" )
    )

;; energized or fatigued
(define-type ont::energy-supply-val
 :parent ont::body-condition-property-val 
)

(define-type ont::not-energized-val
 :parent ont::energy-supply-val
)

(define-type ont::fatigued-val
 :parent ont::not-energized-val 
 :wordnet-sense-keys ("tired%3:00:00" "exhausted%5:00:00:tired:00" "weary%5:00:00:tired:00" )
)

(define-type ont::dazed-val
 :parent ont::not-energized-val
 :wordnet-sense-keys ("lethargic%3:00:00" "dazed%5:00:00:lethargic:00" "groggy%5:00:00:lethargic:00" )
)

(define-type ont::energized-val
 :parent ont::energy-supply-val 
 :wordnet-sense-keys ("energetic%3:00:00" "active%3:00:01")
)

(define-type ont::athletic-val
 :parent ont::energized-val
 :wordnet-sense-keys ("acrobatic%5:00:00:active:01" "sporty%5:00:00:active:01")
)

(define-type ont::hyperactive-val
 :parent ont::energized-val
 :wordnet-sense-keys ("hyperactive%5:00:00:active:01")
)

;; sated or hungry/thirsty
(define-type ont::satedness-val
 :parent ont::body-condition-property-val 
)

(define-type ont::satiated-val
 :parent ont::satedness-val 
 :wordnet-sense-keys ("satiated%3:00:00" )
)

(define-type ont::not-satiated-val
 :parent ont::satedness-val 
 :wordnet-sense-keys ("unsatiated%5:00:00:insatiate:00" "unsated%5:00:00:insatiate:00"  "insatiate%3:00:00")
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

;; sweaty
(define-type ont::sweaty-val
 :parent ont::body-condition-property-val 
 :wordnet-sense-keys ("clammy%5:00:00:wet:01" )
)

;; healthy or ailing
(define-type ont::healthiness-val
  :parent ont::body-condition-property-val
  :arguments ((:ESSENTIAL ONT::FIGURE (F::Phys-obj (f::origin (? org f::human f::non-human-animal)))))
)

(define-type ont::healthy-val
 :parent ont::healthiness-val
 :wordnet-sense-keys ("well%3:00:01" "healthy%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::health-scale))
)

(define-type ont::ailing-val
 :parent ont::healthiness-val
 :wordnet-sense-keys ("unwell%5:00:01:ill:01" "ailing%5:00:00:ill:01" "sick%3:00:01" "ill%3:00:01" "upset%5:00:00:ill:01" )
 ; Words: (W::SICK W::ILL W::UPSET W::GIDDY W::WOOZY W::LIGHTHEADED W::DIZZY W::UNWELL W::FEVERISH W::NAUSEOUS)
 ; Antonym: NIL (W::WELL)
 :sem (F::abstr-obj (F::scale ont::illness-scale))
 :comment "unhealthy, unwell or ailing"
)

;; parallel to ont::medical-condition 
(define-type ont::medical-condition-property-val
 :parent ont::physical-property-val
)

;; mute, deaf, blind
(define-type ont::has-medical-condition
  :parent ont::medical-condition-property-val
  :arguments ((:essential ONT::FIGURE (F::phys-obj (f::origin (? org f::human f::non-human-animal)) (F::intentional +))))
  :wordnet-sense-keys ("deaf%3:00:00" "blind%3:00:00" "mute%5:00:01:inarticulate:00")
  )

;; properties relating to an injury
(define-type ONT::injury-property-val
 :parent ONT::medical-condition-property-val
 :wordnet-sense-keys ("puffy%5:00:00:unhealthy:00")
 )

;; medical symptoms
(define-type ont::medical-symptom-val
 :parent ONT::medical-condition-property-val
)

(define-type ont::dizzy-val
 :parent ont::medical-symptom-val
 :wordnet-sense-keys ("giddy%5:00:00:ill:01" "lightheaded%5:00:00:ill:01" "woozy%5:00:00:ill:01" "nauseated%5:00:00:ill:01" "dizzy%5:00:00:ill:01" "faint%5:00:00:ill:01" )
)

(define-type ont::breathless-val
 :parent ont::medical-symptom-val
 :wordnet-sense-keys ("breathless%3:00:00" )
)

(define-type ont::pained-val
 :parent ont::medical-symptom-val
 :wordnet-sense-keys ("aching%5:00:00:painful:00" "achy%5:00:00:painful:00" "painful%3:00:00" )
)

(define-type ont::medical-dehydration-val
 :parent ont::medical-symptom-val
 :wordnet-sense-keys ("dehydrated%5:00:00:unhealthy:00" )
)

(define-type ont::feverish-val
 :parent ont::medical-symptom-val
 :wordnet-sense-keys ("feverish%5:00:00:ill:01" "feverish%3:01:00" )
)

;; life-process adjectives
(define-type ont::life-process-val
 :parent ont::property-val
 :comment "properties that describe life processes"
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
 :wordnet-sense-keys ("natal%3:01:00" "perinatal%3:00:00")
)

(define-type ont::after-birth-val
 :parent ont::of-birth-val
 :wordnet-sense-keys ("postnatal%3:00:00" "neonatal%3:01:00")
)

(define-type ont::before-birth-val
 :parent ont::of-birth-val
 :wordnet-sense-keys ("prenatal%3:00:00")
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
 :wordnet-sense-keys ("non-living%3:00:00" )
 :comment "e.g., rock, water, metal"
)


;;; process-related adjectives
(define-type ont::process-val
  :parent ont::property-val 
 :comment "properties that describe processes"
)

;; adjectives meaning "can [not] be verb'd" for some verb
(define-type ont::can-be-done-val
 :parent ont::process-val 
 :arguments ((:optional ONT::GROUND )) 
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
)

(define-type ont::accessible-val
 :parent ont::accessibility-val 
 :wordnet-sense-keys ("accessible%3:00:00" )
)

(define-type ont::not-accessible-val
 :parent ont::accessibility-val 
 :wordnet-sense-keys ("inaccessible%3:00:00" )
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
)

(define-type ont::unmanageable
 :parent ont::manageability-val 
 :wordnet-sense-keys ("indocile%5:00:00:unmanageable:00" "unmanageable%3:00:00" )
 ; Words: (W::UNCONTROLLABLE W::UNMANAGEABLE)
 ; Antonym: ONT::manageable (W::CONTROLLABLE W::MANAGEABLE)
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
)

(define-type ont::replaceable-val
 :parent ont::replaceability-val
 :wordnet-sense-keys ("interchangeable%5:00:00:replaceable:00" "replaceable%3:00:00")
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
 :wordnet-sense-keys ("portable%3:00:00" "movable%5:00:00:portable:00" "movable%5:00:00:mobile:00")
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

;; temporal occurrence
(define-type ont::temporal-occurrence-val
 :parent ont::process-val 
 :sem (F::abstr-obj (F::scale ont::temporal-occurrence-scale))
)

;; continuous, uninterrupted, can be either time or space dimensionality
(define-type ont::continuous-val
 :parent ont::temporal-occurrence-val 
)

(define-type ont::continuous
 :parent ont::continuous-val 
 :wordnet-sense-keys ("perpetual%5:00:00:continuous:01" "continuous%3:00:01" "uninterrupted%3:00:00" )
)

(define-type ont::discontinuous
 :parent ont::continuous-val 
 :wordnet-sense-keys ("discontinuous%3:00:01" )
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
 :wordnet-sense-keys ("periodic%3:00:00" )
)

(define-type ont::specified-period-val
 :parent ont::periodic-val 
 :wordnet-sense-keys ("daily%5:00:00:periodic:00" "annual%5:00:00:periodic:00" "weekly%5:00:00:periodic:00" "monthly%5:00:00:periodic:00" "seasonal%3:00:00" )
)

(define-type ont::repetitive-val
 :parent ont::frequency-val 
 :wordnet-sense-keys ("continual%3:00:00")
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
 :wordnet-sense-keys ("sporadic%3:00:00" "irregular%5:00:00:sporadic:00" "casual%5:00:00:irregular:00" )
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

;;; This is for speed values - fast, slow, etc
(define-type ont::speed-val
 :parent ont::process-val 
 :arguments ((:REQUIRED ONT::FIGURE ((? type F::phys-obj F::situation F::abstr-obj )))) ;; e.g., "rate" is an abstract object
 :sem (F::abstr-obj (F::scale ont::speed-scale ))
)

(define-type ont::slow-val
 :parent ont::speed-val 
 :wordnet-sense-keys ("slow%3:00:01" )
)

(define-type ont::instantaneous-val
 :parent ont::speed-val 
 :wordnet-sense-keys ("instantaneous%5:00:00:fast:01" "instant%5:00:00:fast:01" "sudden%3:00:00" )
)

(define-type ont::speedy
 :parent ont::speed-val 
 :wordnet-sense-keys ("quick%5:00:02:fast:01" "fleet%5:00:00:fast:01" "rapid%5:00:00:fast:01" "rapid%5:00:02:fast:01" "fast%3:00:01" "quick%5:00:00:fast:01" )
 ; Words: (W::QUICK W::FAST W::RAPID W::SWIFT W::SPEEDY)
 ; Antonym: NIL (W::SLOW)
)

;; process evaluation
(define-type ont::process-evaluation-val
 :parent ont::process-val
 :comment "evaluation properties of processes"
 :sem (F::abstr-obj (F::scale ont::process-evaluation-scale))
)

;; productive
(define-type ont::productivity-val
 :parent ont::process-evaluation-val
)

(define-type ont::productive-val
 :parent ont::productivity-val 
 :wordnet-sense-keys ("productive%3:00:00" "productive%5:00:00:fruitful:00" "fruitful%3:00:00" )
)

;; useful vs. useless
(define-type ont::usefulness-val
 :parent ont::process-evaluation-val
)

;; useful
(define-type ont::useful
 :parent ont::usefulness-val 
 :wordnet-sense-keys ("useful%3:00:00" "utilitarian%5:00:00:useful:00" "functional%5:00:00:practical:00" "practical%3:00:00" "functional%3:00:00" )
 ; Words: (W::USEFUL W::PRACTICAL W::FUNCTIONAL)
 ; Antonym: ONT::useless (W::USELESS W::IMPRACTICAL)
)

(define-type ont::useless
 :parent ont::usefulness-val 
 :wordnet-sense-keys ("impractical%3:00:00" "useless%3:00:00" )
 ; Words: (W::USELESS W::IMPRACTICAL)
 ; Antonym: ONT::USEFUL (W::USEFUL W::PRACTICAL W::FUNCTIONAL)
)

;; effective vs. ineffective
(define-type ont::effectiveness-val
 :parent ont::process-evaluation-val
 :comment "evaluation attributes dealing with the effectiveness or efficacy of something"
)

(define-type ont::effective-val
 :parent ont::effectiveness-val
 :wordnet-sense-keys ("effective%3:00:00" "effective%5:00:00:efficacious:00" "efficacious%3:00:00")
)

(define-type ont::not-effective-val
 :parent ont::effectiveness-val
 :wordnet-sense-keys ("ineffective%3:00:00" "toothless%5:00:00:ineffective:00")
)

;; (in)efficient
(define-type ont::efficiency-val
 :parent ont::process-evaluation-val 
)

(define-type ont::efficient-val
 :parent ont::efficiency-val 
 :wordnet-sense-keys ("efficient%3:00:00" )
)

(define-type ont::not-efficient-val
 :parent ont::efficiency-val 
 :wordnet-sense-keys ("inefficient%3:00:00" )
)

;; (un)steady, off balance
(define-type ont::steadiness-val
 :parent ont::process-evaluation-val 
 :sem (F::abstr-obj (F::scale ont::steadiness-scale))
)

(define-type ont::steady
 :parent ont::steadiness-val 
 :wordnet-sense-keys ("stable%3:00:00" "unchanged%3:00:04" "unchanged%3:00:00" "steady%3:00:00" "steady%5:00:00:stable:00" )
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
)

(define-type ont::successful-val
 :parent ont::success-val 
 :wordnet-sense-keys ("successful%3:00:00" )
)

(define-type ont::not-successful-val
 :parent ont::success-val 
 :wordnet-sense-keys ("unsuccessful%3:00:00" )
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
 :wordnet-sense-keys ("catchy%5:00:00:difficult:00" "complex%3:00:00" "ambitious%5:00:00:difficult:00" "arduous%5:00:00:difficult:00" "difficult%3:00:00" "rugged%5:00:00:difficult:00" "complicated%5:00:00:complex:00" )
 ; Words: (W::DIFFICULT W::HARD W::COMPLEX W::TOUGH W::COMPLICATED W::TRICKY W::CHALLENGING W::ARDUOUS)
 ; Antonym: ONT::easy (W::EASY W::SIMPLE)
 :sem (F::abstr-obj (F::scale ont::difficult-scale))
)

(define-type ont::easy
 :parent ont::difficulty-val 
 :wordnet-sense-keys ("elementary%5:00:00:easy:01" "easy%3:00:01" "simple%3:00:02" )
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
 :comment "properties relating to human or animal tendencies or inclinations to behave in a particular manner"
 :sem (F::abstr-obj (F::scale ont::behavioral-scale))
)


(define-type ont::animal-disposition-val
 :parent ont::animal-propensity-val
)

;; grumpy, cholaric, churlish, crabbed etc
(define-type ont::negative-disposition-val
 :parent ont::animal-disposition-val
 :wordnet-sense-keys ("ill-natured%3:00:00")
)

;; amiable, euqable, placid
(define-type ont::positive-disposition-val
 :parent ont::animal-disposition-val
 :wordnet-sense-keys ("good-natured%3:00:00")
)

;; (dis)honest
(define-type ont::honesty-val
 :parent ont::animal-propensity-val 
)

(define-type ont::honest-val
 :parent ont::honesty-val
 :wordnet-sense-keys ("honest%3:00:00")
)

(define-type ont::not-honest-val
 :parent ont::honesty-val
 :wordnet-sense-keys ("misleading%5:00:00:dishonest:00" "dishonest%3:00:00")
)

(define-type ont::sneaky-val
 :parent ont::not-honest-val
 :wordnet-sense-keys ("furtive%5:00:00:concealed:00")
)

;; bold, timid
(define-type ont::boldness-val
 :parent ont::animal-propensity-val 
)

(define-type ont::bold-val
 :parent ont::boldness-val 
 :wordnet-sense-keys ("bold%3:00:00" "adventurous%3:00:00" "peremptory%5:00:02:imperative:00")
)

(define-type ont::aggressive-val
 :parent ont::boldness-val 
 :wordnet-sense-keys ("aggressive%3:00:00" "violent%3:00:00" "strident%5:00:00:imperative:00")
 :sem (F::abstr-obj (F::scale ont::aggressiveness-scale) (f::orientation f::pos))
)

(define-type ont::not-bold-val
 :parent ont::boldness-val 
 :wordnet-sense-keys ("timid%3:00:00" "unaggressive%3:00:00")
 :sem (F::abstr-obj (F::scale ont::aggressiveness-scale) (f::orientation f::neg))
)

(define-type ont::docile-val
 :parent ont::not-bold-val 
 :wordnet-sense-keys ("meek%5:00:00:docile:00" "docile%3:00:00")
)

;; modest, arrogant
(define-type ont::modesty-val
 :parent ont::animal-propensity-val 
)

(define-type ont::modest-val
 :parent ont::modesty-val 
 :wordnet-sense-keys ("modest%3:00:02" "unassuming%5:00:00:modest:02" )
)

(define-type ont::arrogant-val
 :parent ont::modesty-val 
 :wordnet-sense-keys ("arrogant%5:00:00:proud:00" "immodest%3:00:02" )
)

;; friendly, affectionate, kind, mean, considerate
;; no experiencer role; currently no distinction between human and non-human ont::of
(define-type ont::social-interaction-val
 :parent ont::animal-propensity-val 
 :arguments ((:required ont::FIGURE ((? lof f::abstr-obj f::phys-obj f::situation )))) 
 :comment "properties of human behavior having to do with social interaction, e.g. friendly, kind, mean.)"
)

;; social
(define-type ont::social-val
 :parent ont::social-interaction-val
 :wordnet-sense-keys ("social%3:01:00" "social%3:00:00" "gregarious%3:00:00" "sociable%3:00:00")
)

(define-type ont::not-social-val
 :parent ont::social-interaction-val
 :wordnet-sense-keys ("ungregarious%3:00:00" "unsociable%3:00:00" "unsocial%3:00:00")
)

;; (keep) in touch
(define-type ont::in-touch-val
 :parent ont::social-interaction-val
)

;; responsible, irresponsible
(define-type ont::responsibility-val
 :parent ont::social-interaction-val 
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
 :wordnet-sense-keys ("irresponsible%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::responsibility-scale) (f::orientation f::neg))
)

;; willing, unwilling
(define-type ont::willingness-val
 :parent ont::social-interaction-val 
)

;; unwilling
(define-type ont::unwilling
 :parent ont::willingness-val 
 :wordnet-sense-keys ("unwilling%3:00:00" "unwilling%5:00:00:involuntary:01" "involuntary%3:00:01")
 :arguments ((:required ONT::FIGURE ((? lof f::phys-obj f::abstr-obj )(f::intentional + )))) 
 ; Words: (W::unwilling w::disinclined w::involuntary)
 ; Antonym: ONT::inclined (W::willing w::inclined w::voluntary)
;	      (:optional ont::action (f::situation))
)

;; willing
(define-type ont::willing
 :parent ont::willingness-val 
 :wordnet-sense-keys ("willing%3:00:00" )
 :arguments ((:required ONT::FIGURE ((? lof f::phys-obj f::abstr-obj )(f::intentional + )))) 
 ; Words: (W::willing w::inclined w::voluntary)
 ; Antonym: ONT::disinclined (W::unwilling w::disinclined w::involuntary)
;	      (:optional ont::action (f::situation))
)

;; (un)friendly, affectionate
(define-type ont::affection-val
 :parent ont::social-interaction-val 
)

(define-type ont::affectionate-val
 :parent ont::affection-val 
 :wordnet-sense-keys ("amicable%3:00:00" "warm%3:00:02" "friendly%3:00:01" "loving%3:00:00" )
)

(define-type ont::not-affectionate-val
 :parent ont::affection-val 
 :wordnet-sense-keys ("unfriendly%3:00:01" "hostile%3:00:01" "hostile%5:00:00:irreconcilable:00")
)

;; tame, wild
(define-type ont::tameness-val
 :parent ont::social-interaction-val 
)

(define-type ont::wild-val
 :parent ont::tameness-val 
 :wordnet-sense-keys ("wild%3:00:02" )
)

(define-type ont::tame-val
 :parent ont::tameness-val 
 :wordnet-sense-keys ("tame%3:00:02" )
)

;; graceful, clumsy
(define-type ont::gracefulness-val
 :parent ont::animal-propensity-val 
)

(define-type ont::awkward-val
 :parent ont::gracefulness-val 
 :wordnet-sense-keys ("awkward%3:00:00" "clumsy%5:00:00:awkward:00" "ungraceful%5:00:00:awkward:00" "graceless%5:00:00:awkward:00" )
)

(define-type ont::graceful-val
 :parent ont::gracefulness-val 
 :wordnet-sense-keys ("graceful%3:00:00" )
)

;; wise vs. foolish
(define-type ont::wiseness-val
 :parent ont::animal-propensity-val
)

(define-type ont::wise-val
 :parent ont::wiseness-val 
 :wordnet-sense-keys ("wise%3:00:00" )
)

(define-type ont::foolish-val
 :parent ont::wiseness-val 
 :wordnet-sense-keys ("ridiculous%5:00:00:foolish:00" "absurd%5:00:00:foolish:00" "foolish%3:00:00" )
)

(define-type ont::silly-val
 :parent ont::foolish-val 
 :wordnet-sense-keys ("goofy%5:00:00:foolish:00" )
)

;; dimensional-property
(define-type ont::dimensional-property-val
    :parent ont::property-val
    :sem (F::abstr-obj (F::scale ont::measure-scale))
    :comment "properties pertaining to dimensions and measurable extents"
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
    :wordnet-sense-keys ("tall%3:00:00" )
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
    :wordnet-sense-keys ("short%3:00:02" "short%3:00:01" )
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
 :wordnet-sense-keys ("broad%5:00:00:large:00" "large%3:00:00")
 ; Words: (W::LARGE W::BIG W::HUGE W::BROAD W::DOUBLE W::VAST W::MASSIVE W::ENORMOUS W::EXTENSIVE W::GIANT W::EXTENDED W::UNLIMITED W::SPACIOUS W::WHOPPING)
 ; Antonym: ONT::small (W::SMALL W::LITTLE W::LIMITED W::TINY W::TEENY)
 :comment "more in orientation on a size scale"
)

(define-type ont::substantial-val
  :parent ont::large
  :wordnet-sense-keys ("substantial%5:00:00:considerable:00" "considerable%3:00:00" "extensive%5:00:00:large:00")
 )


(define-type ont::huge-val
  :parent ont::large
  :wordnet-sense-keys ("massive%5:00:00:large:00" "enormous%5:00:00:large:00" "humongous%5:00:00:large:00" "elephantine%5:00:00:large:00" "huge%5:00:01:large:00")  
 )

;; small
(define-type ont::small
 :parent ont::size-val 
 :wordnet-sense-keys ("small%3:00:00")
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
 :wordnet-sense-keys ("heavy%3:00:01")
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
 :wordnet-sense-keys ("potent%5:00:00:powerful:00" "strong%3:00:00" "acute%5:00:00:sharp:04" "strong%5:00:00:powerful:00" "intense%5:00:00:sharp:04" "shrill%5:00:00:high:03" "powerful%3:00:00" "strong%5:00:00:intense:00" "sharp%3:00:04" "intense%3:00:00" "high%3:00:03" "deep%5:00:00:intense:00" "intensive%5:00:00:intense:00" "forceful%3:00:00")
 ; Words: (W::HIGH W::LOW W::STRONG W::DEEP W::POWERFUL W::SHARP W::INTENSE W::DULL W::SHALLOW W::POTENT)
 ; Antonym: ONT::weak (W::WEAK W::FAINT)
)

(define-type ont::weak
 :parent ont::intensity-val 
 :wordnet-sense-keys ("shallow%3:00:02" "faint%5:00:00:weak:00" "weak%3:00:00" "dull%3:00:04" )
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
 :wordnet-sense-keys ("severe%5:00:01:bad:00" "severe%5:00:00:intense:00" "extreme%5:00:00:intense:00" "drastic%5:00:00:forceful:00")
)

(define-type ont::mild-val
 :parent ont::severity-val 
 :wordnet-sense-keys ("mild%3:00:00" "slight%3:00:00" )
)

(define-type ont::moderate-val
 :parent ont::severity-val 
 :wordnet-sense-keys ("moderate%3:00:00")
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

;; robust, frail
(define-type ont::robustness-val
 :parent ont::dimensional-property-val 
 :comment "indicates relative extent on a robustness scale"
)

(define-type ont::robust-val
 :parent ont::robustness-val 
 :wordnet-sense-keys ("robust%3:00:00" "hardy%5:00:00:robust:00" )
)

(define-type ont::not-robust-val
 :parent ont::robustness-val 
 :wordnet-sense-keys ("frail%3:00:00" )
)

;; relational-attribute: describes an entity with respect to another related entity
(define-type ont::relational-attribute-val
 :parent ont::property-val 
 :comment "properties that describe an entity with respect to another related entity, an implicit second entity always present when these properties are evoked"
  :sem (F::abstr-obj (F::scale ont::relational-property-scale))
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
 :wordnet-sense-keys ("incompatible%3:00:01" "incompatible%3:00:04")
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
 :wordnet-sense-keys ("sequential%5:00:00:ordered:00" "consecutive%5:00:00:ordered:00" "sequent%5:00:00:ordered:00" "successive%5:00:00:ordered:00" "serial%5:00:00:ordered:00" "progressive%5:00:00:ordered:00")
)

(define-type ont::random-val
 :parent ont::ordered-val 
 :wordnet-sense-keys ("random%3:00:00" "randomized%5:00:00:irregular:00")
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
 :wordnet-sense-keys ("previous%5:00:00:past:00" "previous%5:00:00:preceding:00" "former%5:00:02:past:00" "late%5:00:02:past:00" "preceding%3:00:00")
)

(define-type ont::one-before-last-val
 :parent ont::sequence-val 
 :wordnet-sense-keys ("next-to-last%5:00:01:intermediate:00" "penultimate%5:00:00:intermediate:00" )
)

(define-type ont::first-val
 :parent ont::sequence-val 
 :wordnet-sense-keys ("original%5:00:01:first:00" "first%3:00:00" "initial%5:00:00:first:00" )
)

(define-type ont::last-val
 :parent ont::sequence-val 
 :wordnet-sense-keys ("ultimate%5:00:00:last:00" "latter%3:00:00" "final%5:00:00:ultimate:00" "last%3:00:00" )
)

(define-type ont::sequence-val-next
 :parent ont::sequence-val 
 :wordnet-sense-keys ("following%5:00:02:succeeding:00" "subsequent%3:00:00" "succeeding%3:00:00")
)

(define-type ont::middle-val
 :parent ont::sequence-val 
 :wordnet-sense-keys ("halfway%5:00:00:intermediate:00" "middle%5:00:00:intermediate:00" "intermediate%3:00:00" )
)

;; Relating to time
(define-type ont::temporal-val
 :parent ont::property-val 
 :comment "properties relating to time"
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
 :wordnet-sense-keys ("old%3:00:02" "old%3:00:01")
)

(define-type ont::young-val
 :parent ont::age-val 
 :wordnet-sense-keys ("young%3:00:00" "immature%3:00:03" "new%3:00:09")
)

(define-type ont::historical-era-val
 :parent ont::temporal-val 
 :comment "relating to the distinct periods in history"
)

(define-type ont::ancient-val
 :parent ont::historical-era-val 
 :wordnet-sense-keys ("ancient%5:00:00:past:00" "prehistoric%5:00:00:past:00" "prehistoric%3:01:00" )
)

(define-type ont::modern-val
 :parent ont::historical-era-val 
 :wordnet-sense-keys ("modern%3:00:00" "contemporary%5:00:00:modern:00" )
)

(define-type ont::current-val
 :parent ont::historical-era-val 
 :wordnet-sense-keys ("contemporary%5:00:00:current:00" "current%3:00:00")
)

(define-type ont::not-current-val
 :parent ont::historical-era-val
 :wordnet-sense-keys ("old-fashioned%5:00:00:unfashionable:00")
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
 :wordnet-sense-keys ("malfunctioning%3:00:00" "defective%5:00:00:malfunctioning:00" )
 :comment "not functioning as intended; malfunctioning e.g., printer is printing only in red"
)

(define-type ont::not-in-working-order-val
 :parent ont::functionality-val 
 :wordnet-sense-keys ("broken%5:00:00:damaged:00" "inoperative%3:00:00")
 :comment "broken/not-operational e.g., not switching on"
)

(define-type ont::in-working-order-val
 :parent ont::functionality-val 
 :wordnet-sense-keys ("running%5:00:00:functioning:00" "functional%5:00:00:functioning:00" "operative%5:00:00:functioning:00" "working%5:00:00:functioning:00" "functioning%3:00:00" )
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
 :wordnet-sense-keys ("unusable%5:00:00:useless:00" )
)

;; operating/active as intended?
(define-type ont::activity-val
 :parent ont::object-affordances-val 
 :arguments ((:required ONT::FIGURE ((? lof f::phys-obj )
				     ;;(f::type (? !t2 ont::location ))
				     (F::object-function F::provides-service )))) 
; :sem (F::Abstr-obj (F::gradability - ))
 :comment "properties relating to whether something is operating/active as intended"
)

(define-type ont::active
 :parent ont::activity-val 
 :wordnet-sense-keys ("busy%5:00:01:active:06" "active%3:00:06" "active%3:00:03" "busy%3:00:00" )
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
 :wordnet-sense-keys ("idle%3:00:00" "passive%3:00:01" )
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

(define-type ont::dynamic-val
 :parent ont::motion-val
 :wordnet-sense-keys ("dynamic%3:00:04")
)

(define-type ont::static-val
 :parent ont::motion-val
 :wordnet-sense-keys ("static%5:00:00:nonmoving:00" "still%5:00:01:nonmoving:00" "motionless%5:00:00:nonmoving:00" "nonmoving%3:00:00")
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
 :parent ont::status-val 
)

(define-type ont::poor-val
 :parent ont::wealthiness-val 
 :wordnet-sense-keys ("poor%3:00:03" "poor%3:00:00" )
)

(define-type ont::wealthy-val
 :parent ont::wealthiness-val 
 :wordnet-sense-keys ("rich%3:00:00" "wealthy%5:00:00:rich:00" )
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
 :wordnet-sense-keys ("public%3:00:00" )
)

;; confidential, secret
(define-type ont::secrecy-val
 :parent ont::status-val 
)

(define-type ont::secret-val
 :parent ont::secrecy-val 
 :wordnet-sense-keys ("confidential%5:00:02:private:00" "secret%5:00:00:concealed:00" "private%5:00:00:inward:00" "secret%5:00:00:inward:00" "dark%5:00:00:concealed:00" "incognito%5:00:00:concealed:00" "sealed%5:00:00:concealed:00" "sneaking%5:00:00:concealed:00")
)

(define-type ont::country-related-val
 :parent ont::status-val 
 :comment "adjectives relating to a nation or a country"
)

;; national, federal
(define-type ont::national-val
 :parent ont::country-related-val 
 :wordnet-sense-keys ("domestic%3:00:00" "national%5:00:00:domestic:00" "interior%5:00:00:domestic:00" "home%5:00:00:domestic:00" "internal%5:00:00:domestic:00" "national%3:00:01")
 :comment "having to do with a nation (or its government)"
)

(define-type ont::federal-val
 :parent ont::national-val 
 :wordnet-sense-keys ("federal%5:00:00:national:01" "federal%3:01:02" )
)

(define-type ont::foreign-val
 :parent ont::country-related-val 
 :wordnet-sense-keys ("foreign%3:00:02" "outside%5:00:00:foreign:02" "international%5:00:00:foreign:02" "external%5:00:00:foreign:02" )
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
 :wordnet-sense-keys ("rural%3:00:00" "rural%3:01:01" )
)


;; social status relating to fame
(define-type ont::fame-val
 :parent ont::status-val 
 :wordnet-sense-keys ("famous%5:00:00:known:00" "celebrated%5:00:00:known:00" "legendary%5:00:00:known:00")
 :comment "social status relating to fame"
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

;; confused, surprised, happy
(define-type ont::psychological-property-val
    :parent ont::property-val
    :arguments ((:REQUIRED ONT::FIGURE (F::phys-obj (f::intentional +) (f::origin (? org f::human f::non-human-animal))))) 
    :comment "properties pertaining to psychological, mental or emotional states"
    ;;(:optional ont::stimulus ((? stm f::phys-obj f::situation f::abstr-obj)))
    ;; the object that is involved in a situation, but which is not a stimulus directly
    ;; for example, I am afraid for her, for the project
    ;;(:optional ont::formal (f::situation f::phys-obj f::abstr-obj)))
    :sem (F::abstr-obj (F::scale ont::psychological-condition-scale) )
)

;; reasonable/sensible vs unreasonable (mental states)
(define-type ont::sensibility-val
 :parent ont::psychological-property-val
 :comment "describing mental ability or sensitivity to respond to emotional influences"
 :sem (F::abstr-obj (F::scale ont::rationality-scale) )
)

(define-type ont::sensible-val
 :parent ont::sensibility-val 
 :wordnet-sense-keys ("sensible%3:00:04" "sane%5:00:00:rational:00" "sane%3:00:00" "in_his_right_mind%5:00:00:sane:00")
 :comment "sensible, reasonable"
 :sem (F::abstr-obj (F::scale ont::rational-scale) )
)

(define-type ont::not-sensible-val
 :parent ont::sensibility-val 
 :wordnet-sense-keys ("unreasonable%3:00:00" )
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
 :wordnet-sense-keys ("careful%3:00:00" "careful%5:00:00:mindful:00" "heedful%5:00:00:mindful:00" )
 :sem (F::abstr-obj (F::scale ont::cautiousness-scale) (F::orientation f::pos))
)

(define-type ont::not-careful-val
 :parent ont::carefulness-val 
 :wordnet-sense-keys ("careless%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::cautiousness-scale) (F::orientation f::neg))
)

(define-type ont::attentiveness-val
 :parent ont::attention-val 
)

(define-type ont::attentive-val
 :parent ont::attentiveness-val 
 :wordnet-sense-keys ("attentive%3:00:00" "attentive%3:00:04" )
)

(define-type ont::not-attentive-val
 :parent ont::attentiveness-val 
 :wordnet-sense-keys ("inattentive%3:00:00" "heedless%3:00:00" )
)

;; aware (of x)
(define-type ont::awareness-val
 :parent ont::attention-val 
 :sem (F::abstr-obj (F::scale ont::awareness-scale) )
)

(define-type ont::aware-val
 :parent ont::awareness-val 
 :wordnet-sense-keys ("aware%3:00:00" "mindful%3:00:00" "aware%3:00:04" )
 :sem (F::abstr-obj (F::scale ont::awareness-scale) (F::orientation f::pos))
)

(define-type ont::not-aware-val
 :parent ont::awareness-val 
 :wordnet-sense-keys ("unaware%3:00:00" )
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
 :wordnet-sense-keys ("definite%5:00:00:certain:01" "certain%3:00:01" "certain%3:00:02" "indisputable%5:00:00:certain:01" "sealed%3:00:02" "convinced%5:00:00:certain:02" )
 ; Words: (W::CERTAIN W::SURE W::CONFIDENT)
 ; Antonym: ONT::UNCERTAIN (W::UNCERTAIN W::UNSURE)
 :sem (F::abstr-obj (F::scale ont::certainty-scale) (F::orientation f::pos))
)

(define-type ont::uncertain
 :parent ont::certainty-val 
 :wordnet-sense-keys ("unsure%3:00:00" "unsealed%3:00:02" "uncertain%3:00:02" )
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

;; (un)interested
(define-type ont::interest-val
 :parent ont::psychological-property-val 
 :sem (F::abstr-obj (F::scale ont::interest-scale) )
)

(define-type ont::interested-val
 :parent ont::interest-val 
 :wordnet-sense-keys ("interested%3:00:00" "curious%5:00:00:interested:00" )
 :sem (F::abstr-obj (F::scale ont::interested-scale) )

)

(define-type ont::eager-val
 :parent ont::interested-val 
 :wordnet-sense-keys ("eager%3:00:00" "enthusiastic%3:00:00" )
)

(define-type ont::not-interested-val
 :parent ont::interest-val 
 :wordnet-sense-keys ("uninterested%3:00:00" "disinterested%5:00:00:impartial:00" )
 :sem (F::abstr-obj (F::scale ont::not-interested-scale) )
)

;; puzzled
(define-type ont::puzzlement-val
 :parent ont::psychological-property-val 
)

(define-type ont::puzzled-val
 :parent ont::puzzlement-val 
 :wordnet-sense-keys ("perplexed%3:00:00" "baffled%5:00:00:perplexed:00" "puzzled%5:00:00:perplexed:00" "stuck%5:00:00:perplexed:00" )
)

;; worrying, disturbing, tiresome
(define-type ont::evoking-emotion-val
 :parent ont::psychological-property-val 
 :comment "attributes that indicate the evocation of a particular emotion"
)

(define-type ont::evoking-neg-emotion-val
 :parent ont::evoking-emotion-val 
)

(define-type ont::distressing-val
 :parent ont::evoking-neg-emotion-val 
 :wordnet-sense-keys ("worrying%5:00:00:heavy:02" "distressing%5:00:00:heavy:02" "perturbing%5:00:00:heavy:02" "worrisome%3:00:04" "troubling%5:00:00:heavy:02" "disturbing%5:00:00:heavy:02" "distressful%5:00:00:heavy:02" "worrisome%5:00:00:heavy:02" "appalling%5:00:00:alarming:00" "atrocious%5:00:00:alarming:00" "weighty%5:00:00:heavy:02" "heavy%3:00:02")
 :sem (F::abstr-obj (F::scale ont::distress-scale) )
)


; scary, frightening, fearsome
(define-type ont::frightening-val
 :parent ont::evoking-neg-emotion-val 
 :wordnet-sense-keys ("alarming%3:00:00")
)

(define-type ont::not-pleasing-val
 :parent ont::evoking-neg-emotion-val 
 :wordnet-sense-keys ("unwelcome%3:00:00" "disagreeable%5:00:00:uncongenial:00" "unpleasant%3:00:00" "disagreeable%3:00:00" "displeasing%3:00:00" )
)

(define-type ont::grievous-val
 :parent ont::evoking-neg-emotion-val 
 :wordnet-sense-keys ("heartrending%5:00:00:sorrowful:00" "heartbreaking%5:00:00:sorrowful:00" "grievous%5:00:00:sorrowful:00" "sorrowful%3:00:00")
 :sem (F::abstr-obj (F::scale ont::grief-scale) )
)

(define-type ont::confusing-val
 :parent ont::evoking-neg-emotion-val 
 :wordnet-sense-keys ("confusing%5:00:00:disorienting:00" )
)

(define-type ont::boring
 :parent ont::evoking-neg-emotion-val 
 :wordnet-sense-keys ("uninteresting%3:00:00" "boring%5:00:00:uninteresting:00" "wearisome%5:00:00:uninteresting:00" "tiresome%5:00:00:uninteresting:00")
 ; Words: (W::DULL W::BORING W::UNINTERESTING)
 ; Antonym: NIL (W::INTERESTING)
)

;(define-type ont::tiresome-val
; :parent ont::evoking-neg-emotion-val 
; :wordnet-sense-keys ("burdensome%5:00:00:heavy:02" "oppressive%5:00:00:heavy:02" "leaden%5:00:00:heavy:02")
;)

(define-type ont::evoking-pos-emotion-val
 :parent ont::evoking-emotion-val 
)

(define-type ont::calming-val
 :parent ont::evoking-pos-emotion-val 
 :wordnet-sense-keys ("soothing%5:00:00:reassuring:00" "reassuring%3:00:00" "assuasive%5:00:00:reassuring:00")
)

(define-type ont::enjoyable-val
 :parent ont::evoking-pos-emotion-val 
 :wordnet-sense-keys ("enjoyable%5:00:00:pleasant:00" )
)

(define-type ont::pleasing-val
 :parent ont::evoking-pos-emotion-val 
 :wordnet-sense-keys ("pleasing%3:00:00" "welcome%3:00:00" "agreeable%3:00:00" "delightful%5:00:00:pleasing:00" )
)

(define-type ont::interesting-val
 :parent ont::evoking-pos-emotion-val 
 :wordnet-sense-keys ("fascinating%5:00:00:interesting:00" "interesting%3:00:00" )
)

(define-type ont::desirable-val
 :parent ont::evoking-pos-emotion-val 
 :wordnet-sense-keys ("desirable%3:00:00" )
)

;; happy, sad, gloomy...
(define-type ont::emotional-val
 :parent ont::psychological-property-val 
 :arguments ((:ESSENTIAL ONT::FIGURE (F::Phys-obj (f::origin (? org f::human f::non-human-animal)))))
 :comment "state of having a particular emotion"
)

(define-type ont::thankfulness-val
 :parent ont::emotional-val 
)

;;;;;
(define-type ont::grateful
 :parent ont::thankfulness-val 
 :wordnet-sense-keys ("glad%5:00:00:grateful:00" "grateful%3:00:00" "glad%3:00:00" "thankful%3:00:00" )
 ; Words: (W::GLAD W::GRATEFUL W::CHEERFUL W::THANKFUL)
 ; Antonym: ONT::UNGRATEFUL (W::SAD W::MELANCHOLY W::UNGRATEFUL)
)

(define-type ont::not-grateful-val
 :parent ont::thankfulness-val 
 :wordnet-sense-keys ("ungrateful%3:00:00" "thankless%3:00:00" "unthankful%3:00:00" )
)

;;;;;
(define-type ONT::pos-emotional-val
  :parent ONT::emotional-val
 )

(define-type ONT::neg-emotional-val
  :parent ONT::emotional-val
 )

(define-type ONT::neutral-emotional-val
  :parent ONT::emotional-val
 )

(define-type ONT::surprised
  :parent ONT::emotional-val
  :wordnet-sense-keys ("surprised%3:00:00")
 )

;; ecstatic
(define-type ONT::pos-intense-emotional-val
  :parent ONT::pos-emotional-val
 )

;; glad
(define-type ONT::pos-soft-emotional-val
  :parent ONT::pos-emotional-val
 )

;; angry
(define-type ONT::neg-intense-emotional-val
  :parent ONT::neg-emotional-val
 )

;; sad
(define-type ONT::neg-soft-emotional-val
  :parent ONT::neg-emotional-val
 )

;;;;;
(define-type ont::proud-val
 :parent ont::pos-intense-emotional-val 
 :wordnet-sense-keys ("proud%3:00:00" )
 :sem (F::abstr-obj (F::scale ont::pride-scale) )
)

(define-type ONT::EUPHORIC
 :parent ONT::pos-intense-emotional-val
 ; Words: (W::HAPPY W::EUPHORIC)
 :wordnet-sense-keys ("euphoric%3:00:00" "happy%3:00:00" "cheerful%3:00:00" "beaming%5:00:00:cheerful:00" "pleased%3:00:00")
 ; Antonym: ONT::UNHAPPY (W::UNHAPPY W::MISERABLE)
 :sem (F::abstr-obj (F::scale ont::happiness-scale) ) 
)

(define-type ONT::excited
 :parent ONT::pos-intense-emotional-val
 :wordnet-sense-keys ("excited%3:00:00" "excited%5:00:00:wild:02" "pumped-up%5:00:00:tense:03")
 :sem (F::abstr-obj (F::scale ont::excitement-scale) )
 )

(define-type ONT::desirous
 :parent ONT::pos-intense-emotional-val
 :wordnet-sense-keys ("desirous%3:00:00")
 )

(define-type ONT::amused
 :parent ONT::pos-soft-emotional-val
 :wordnet-sense-keys ("amused%5:00:00:pleased:00")
 )

(define-type ONT::calm
 :parent ONT::pos-soft-emotional-val
 :wordnet-sense-keys ("calm%5:00:00:composed:00" "composed%3:00:00")
 )

(define-type ONT::PLEASANT
 :parent ONT::pos-soft-emotional-val
 :wordnet-sense-keys ("pleasant%3:00:00")
 :sem (F::abstr-obj (F::scale ont::pleasantness-scale) )
 )

;;;;;
(define-type ONT::afraid
 :parent ONT::neg-intense-emotional-val
 :wordnet-sense-keys ("afraid%3:00:00")
 )

(define-type ONT::angry
 :parent ONT::neg-intense-emotional-val
 :wordnet-sense-keys ("angry%3:00:00")
 )

(define-type ONT::agitated
 :parent ONT::neg-intense-emotional-val
 :wordnet-sense-keys ("agitated%3:00:00")
 )

(define-type ONT::envious
 :parent ONT::neg-intense-emotional-val
 :wordnet-sense-keys ("envious%5:00:00:desirous:00")
 )

(define-type ONT::disgusted
 :parent ONT::neg-intense-emotional-val
 :wordnet-sense-keys ("disgusted%5:00:00:displeased:00")
 )

(define-type ont::frantic-val
 :parent ONT::neg-intense-emotional-val
 :wordnet-sense-keys ("hectic%5:00:00:agitated:00" "frantic%5:00:00:agitated:00" )
)

;;;;;
(define-type ONT::uneasy
 :parent ONT::neg-soft-emotional-val
 :wordnet-sense-keys ("anxious%5:00:00:troubled:00" "uneasy%3:00:00" "troubled%3:00:00" "tense%3:00:03")
 )

(define-type ONT::grumpy-val
 :parent ONT::neg-soft-emotional-val
 :wordnet-sense-keys ("grumpy%5:00:00:ill-natured:00" "disagreeable%5:00:00:ill-natured:00" "sulky%5:00:00:ill-natured:00" "irritable%5:00:00:ill-natured:00")
)

(define-type ONT::UNHAPPY
 :parent ONT::neg-soft-emotional-val
 ; Words: (W::UNHAPPY W::MISERABLE)
 :wordnet-sense-keys ("dysphoric%3:00:00" "unhappy%3:00:00" "sad%3:00:00" "gloomy%5:00:00:dejected:00" "melancholy%5:00:00:sad:00" "miserable%5:00:00:unhappy:00" "dejected%3:00:00")
 ; Antonym: ONT::EUPHORIC (W::HAPPY W::EUPHORIC)
 :sem (F::abstr-obj (F::scale ont::sadness-scale) ) 
)

(define-type ONT::sorry
 :parent ONT::neg-soft-emotional-val
 ; Words: (W::sorry)
 :wordnet-sense-keys ("sorry%3:00:02")
 )

(define-type ONT::bored
 :parent ONT::neg-soft-emotional-val
 :wordnet-sense-keys ("bored%5:00:00:tired:00" "bored%5:00:00:uninterested:00")
 )


;; smart, (un)intelligent
(define-type ont::intelligence-val
 :parent ont::psychological-property-val 
)

(define-type ont::smart
 :parent ont::intelligence-val 
 :wordnet-sense-keys ("smart%3:00:00" "bright%5:00:00:intelligent:00" "cagey%5:00:00:smart:00" "intelligent%3:00:00" )
 ; Words: (W::CLEVER W::INTELLIGENT W::SMART)
 ; Antonym: ONT::STUPID (W::STUPID W::DUMB)
)

(define-type ont::clever-val
 :parent ont::smart 
 :wordnet-sense-keys ("cunning%5:00:00:adroit:00" "clever%5:00:00:adroit:00" "ingenious%5:00:00:adroit:00" "quick-witted%5:00:00:adroit:00")
)

(define-type ont::stupid
 :parent ont::intelligence-val 
 :wordnet-sense-keys ("dense%5:00:00:stupid:00" "unintelligent%3:00:00" "stupid%3:00:00" )
 ; Words: (W::STUPID W::DUMB)
 ; Antonym: ONT::smart (W::CLEVER W::INTELLIGENT W::SMART)
)

;;; deliberate, on purpose
(define-type ont::intentionality-val
 :parent ont::psychological-property-val
 )

(define-type ont::intentional-val
 :parent ont::intentionality-val
 :wordnet-sense-keys("deliberate%5:00:00:intended:00" "intentional%5:00:00:intended:00" "intended%3:00:00") 
)

(define-type ont::not-intentional-val
 :parent ont::intentionality-val
 :wordnet-sense-keys("accidental%5:00:00:unintended:00" "unintentional%5:00:00:unintended:00" "unintended%3:00:00")
 )

;; ad hoc vs premeditated process
(define-type ont::premeditation-val
 :parent ont::psychological-property-val 
 :comment "is the process planned (careful forethought and planning)?"
)

(define-type ont::premeditated-val
 :parent ont::premeditation-val
 :wordnet-sense-keys ("planned%3:00:00" "premeditated%3:00:00")
)

(define-type ont::not-premeditated-val
 :parent ont::premeditation-val
 :wordnet-sense-keys ("haphazard%5:00:00:random:00" "ad_hoc%5:00:00:unplanned:00" "impulsive%5:00:00:unpremeditated:00" "spontaneous%5:00:00:unscripted:00" "unpremeditated%3:00:00" "unscripted%3:00:00" "unplanned%3:00:00")
)

;; resulting-state-val
(define-type ont::resulting-state-val
 :parent ont::property-val
 :comment "adjectives that describe the resulting states of the verb that it pertains to" 
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
 :wordnet-sense-keys ("preserved%3:00:02" "processed%3:00:00")
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
 :wordnet-sense-keys ("prepared%3:00:00")
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
 :wordnet-sense-keys ("protected%3:00:00")
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
 :wordnet-sense-keys ("industrial%3:01:00" "industrial%3:00:00")
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
 :wordnet-sense-keys ("technical%3:00:00" "technical%3:01:00" )
 :arguments ((:required ont::FIGURE (?ft (f::information f::information-content )))) 
 :sem (f::abstr-obj (:default (f::gradability - )))
)

;; specific type defined for obtw demo
(define-type ont::medical-val
 :parent ont::associated-with-val 
 :wordnet-sense-keys ("medical%3:01:00" )
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
)

;; mental, cerebral
(define-type ont::mental-val
 :parent ont::associated-with-val 
 :wordnet-sense-keys ("mental%3:00:00" "mental%3:01:00" )
 :arguments ((:optional ONT::GROUND )) 
)

;; physical, bodily
(define-type ont::physical-val
 :parent ont::associated-with-val 
 :wordnet-sense-keys ("physical%3:00:00" "bodily%5:00:00:physical:00" )
 :arguments ((:optional ONT::GROUND )) 
)

;; relating to machines
(define-type ont::mechanical-val
 :parent ont::physical-val
 :wordnet-sense-keys ("mechanical%3:00:00")
)

;; monroe
(define-type ONT::POLITICAL
 :parent ont::associated-with-val
 :wordnet-sense-keys ("political%3:00:00")
 )

;; assoc with languages and linguistics
(define-type ont::associated-with-languages-val
 :parent ont::associated-with-val
)

;; relating to languages
(define-type ont::related-to-languages-val
 :parent ont::associated-with-languages-val
 :wordnet-sense-keys ("linguistic%3:01:00" "intralinguistic%3:01:00")
)

(define-type ont::language-specific-val
 :parent ont::related-to-languages-val
 :wordnet-sense-keys ("semitic%3:01:01" "cyrillic%3:01:00")
 :comment "associated specifically with language written or spoken"
)

;; not related to languages
(define-type ont::not-related-to-languages-val
 :parent ont::associated-with-languages-val
 :wordnet-sense-keys ( "nonlinguistic%3:01:00" "extralinguistic%3:01:00")
)

;; relating to linguistics
(define-type ont::linguistic-val
 :parent ont::associated-with-languages-val 
 :wordnet-sense-keys ("linguistic%3:01:01" "psycholinguistic%3:01:00" "sociolinguistic%3:01:00" "diachronic%3:00:00" "morphologic%3:01:01" "phonological%3:01:00" "phonetic%3:01:00" "semantic%3:01:00")
 :comment "associated with the discipline of linguistics and its various subfields"
)

(define-type ont::linguistic-property-val
 :parent ont::linguistic-val
 :wordnet-sense-keys ("strong%5:00:00:irregular:00" "stative%3:00:00" "subjunctive%3:01:00" "finite%3:00:02" "indicative%3:01:00" "interrogative%3:01:00" "infinite%3:00:02""optative%3:01:00" "unrestricted%5:00:00:unmodified:00" "grammatical%3:00:00" "radical%3:01:01" "affixal%3:01:00" "cross-linguistic%3:01:00" "singular%3:00:00" "ungrammatical%3:00:00" "coordinating%3:00:00" "endocentric%3:00:00" "exocentric%3:00:00" "feminine%3:00:02" "grammatical%3:01:00" "inflectional%3:00:00" "masculine%3:00:02" "neuter%3:00:00" "paradigmatic%3:01:01" "personal%3:01:00" "plural%3:00:00" "syncategorematic%3:00:00" "active%3:00:09" "categorematic%3:00:00" "derivational%3:00:00" "short%3:00:04""polyphonic%3:01:01" "phonetic%3:01:01")
)

(define-type ont::orthography-property-val
 :parent ont::linguistic-val
 :wordnet-sense-keys ("analphabetic%3:00:00" "separative%5:00:02:disjunctive:00" "orthographic%3:01:00")
)

;(define-type ont::semantic-val
; :parent ont::linguistic-val 
; :wordnet-sense-keys ("semantic%3:01:00" )
;)

(define-type ont::associated-with-culture-val
 :parent ont::associated-with-val
 :wordnet-sense-keys ("transcultural%3:01:00" "cross-cultural%3:01:00" "multicultural%3:01:00")
 :comment "associated with culture, people, nation, or language"
)

(define-type ont::culture-specific-val
 :parent ont::associated-with-culture-val
 :wordnet-sense-keys ("sinitic%3:01:00" "siouan%3:01:00" "somalian%3:01:00")
 :comment "associated specifically with culture or peole"
)

(define-type ont::nation-land-specific-val
 :parent ont::associated-with-culture-val
 :wordnet-sense-keys ("northern%3:00:02")
 :comment "associated with nation or land"
)

(define-type ont::associated-with-race-val
 :parent ont::associated-with-val
)

(define-type ont::race-specific-val
 :parent ont::associated-with-race-val
 :comment "associated with race"
)

(define-type ont::associated-with-religion-val
 :parent ont::associated-with-val
 :comment "having to do with religion"
)

(define-type ont::religion-specific-val
 :parent ont::associated-with-religion-val 
 :comment "identity specifically based on religious affiliation, dogma, or theology (properties referring to the culture of the practicing people or nations belong to ont::culture-specific)"
)

(define-type ont::associated-with-science-val
  :parent ont::associated-with-val   
)

;; for boudreaux
(define-type ONT::BIOLOGICAL
 :parent ONT::associated-with-science-val
 :wordnet-sense-keys ("biological%3:01:00")
 )

(define-type ONT::chemical-val
 :parent ONT::associated-with-science-val
 :wordnet-sense-keys ("chemical%3:01:00")
 )

(define-type ONT::astronomy-val
 :parent ONT::associated-with-science-val
 :wordnet-sense-keys ("lunar%3:01:00")
)

;; words relating to space
(define-type ont::spatial
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
  :wordnet-sense-keys ("straight%3:00:01" "nonstop%5:00:00:direct:00" "direct%3:00:00")
 :parent ONT::spatial
 :sem (F::abstr-obj (:required)(:default (F::gradability -)))
 :arguments ((:REQUIRED ONT::FIGURE (F::phys-obj (F::spatial-abstraction (? sab F::line F::strip))))
             )
 )

(define-type ONT::orientation-val
 :parent ONT::spatial
  :arguments ((:OPTIONAL ONT::GROUND (F::phys-obj))
             )
 )

(define-type ont::spatial-arrangement-val
 :parent ont::orientation-val
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
 :wordnet-sense-keys ("oblique%3:00:00" "inclined%3:00:01")
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
  :parent ONT::spatial
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
 :wordnet-sense-keys ("angular%3:00:00")
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
  :parent ONT::spatial
 :wordnet-sense-keys ("symmetric%3:00:00" "perpendicular%3:00:00" "asymmetrical%5:00:00:irregular:00")
 )

(define-type ONT::GEO-FEATURE-VAL
 :parent ONT::spatial
 )

;; internal, external, inner, outer, onsite facilities
(define-type ONT::LOCATION-VAL
 :parent ONT::spatial
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
 :parent ONT::spatial
 )

;; inbound, outbound
(define-type ONT::DIRECTION-VAL
 :parent ONT::spatial
 )

;; the proposal is close to done; the hotel is close to an address; the reporter got close to the riot;
;; close to, near
(define-type ONT::distance-val
 :parent ONT::spatial
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
 :parent ont::spatial
 :sem (F::abstr-obj (:required)(:default (F::gradability +)))
 :arguments ((:REQUIRED ONT::neutral)
             (:ESSENTIAL ONT::neutral1)
;	     (:OPTIONAL ONT::PROPERTY)
             )
 )||#


(define-type ONT::LEFT
 :parent ONT::LOCATION-VAL
 ; Words: (W::LEFT W::LEFTMOST)
 :wordnet-sense-keys ("left%3:00:00" "center%3:00:00" "leftmost%5:00:00:left:00")
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
 :wordnet-sense-keys ("inbound%5:00:00:incoming:00" "incoming%3:00:00")
 ; Antonym: ONT::OUTGOING (W::OUTGOING W::OUTBOUND)
 )

(define-type ONT::OUTGOING
 :parent ONT::DIRECTION-VAL
 ; Words: (W::OUTGOING W::OUTBOUND)
 :wordnet-sense-keys ("outbound%5:00:00:outgoing:00" "outgoing%3:00:00")
 ; Antonym: ONT::INCOMING (W::INCOMING W::INBOUND)
 )

(define-type ONT::near
 :parent ONT::DISTANCE-VAL
 ; Words: (W::CLOSE W::NEAR W::NEARBY)
 :wordnet-sense-keys ("near%3:00:00" "close%3:00:02" "nearby%5:00:00:near:00" "approximate%5:00:00:close:02")
 ; Antonym: ONT::REMOTE (W::FAR W::REMOTE W::DISTANT W::FARTHER)
 )

(define-type ONT::REMOTE
 :parent ONT::DISTANCE-VAL
 ; Words: (W::FAR W::REMOTE W::DISTANT W::FARTHER)
 :wordnet-sense-keys ("distant%5:00:02:far:00" "distant%3:00:02" "far%3:00:00" "distant%5:00:01:far:00" "farther%5:00:01:far:00")
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
 :arguments ((:optional ONT::GROUND)
	     (:optional ONT::STANDARD)
             )
 :wordnet-sense-keys("quantitative%3:00:00")
 )

(define-type ONT::additional-val
 :parent ONT::quantity-related-property-val
 :wordnet-sense-keys ("extra%5:00:00:additive:00" "complemental%5:00:00:additive:00" "intercalary%5:00:00:additive:00")
 )

(define-type ONT::measure-related-property-val
 :parent ONT::quantity-related-property-val
 )

;; previously numerical-property-val
(define-type ONT::math-related-property-val
    :parent ONT::quantity-related-property-val
    :sem (f::abstr-obj (f::gradability -))
    ;:arguments ((:REQUIRED ONT::FIGURE (f::abstr-obj (F::measure-function f::term))))
    :arguments ((:REQUIRED ONT::FIGURE ((? x f::abstr-obj f::situation) (F::measure-function f::term))))  ; f::situation: exponential increase
    :wordnet-sense-keys("algebraic%3:01:00" "trigonometric%3:01:00" "additive%3:00:00")
)

(define-type ONT::adequacy-VAL
;   :sem (F::Abstr-obj (F::scale ONT::adequacy-val))
   :parent  ONT::quantity-related-property-val
   :arguments ((:ESSENTIAL ONT::GROUND)
	       )
 )

(define-type ONT::number-related-property-val
 :parent ONT::quantity-related-property-val
 :wordnet-sense-keys("numerical%5:00:00:quantitative:00" "numerical%3:01:00" "decimal%5:00:01:quantitative:00" "duodecimal%5:00:00:quantitative:00" "vicenary%5:00:00:quantitative:00" "three-figure%5:00:00:quantitative:00")
 )

;; single, dual, lone, twin, only
(define-type ONT::CARDINALITY-VAL
 :parent ONT::number-related-property-val
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
  :wordnet-sense-keys ("inadequate%5:00:00:insufficient:00" "insufficient%3:00:00" "scarce%3:00:00")
					; Antonym: ONT::ADEQUATE (W::SUFFICIENT W::ADEQUATE W::ENOUGH)
  )

(define-type ONT::ADEQUATE
  :parent ONT::adequacy-VAL
					; Words: (W::SUFFICIENT W::ADEQUATE W::ENOUGH)
 :wordnet-sense-keys ("sufficient%3:00:00" "adequate%5:00:00:sufficient:00")
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

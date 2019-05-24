;;;;;;
;;;;;;; templates.lisp
;;;;;;;

(in-package :lxm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Noun templates
;;;;;
(define-templates
 ;;;;;;
 ;;;;;;; templates.lisp
 ;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;; Noun templates
 ;;;;;
 (
  ;;;;; Most common nouns - regular countable 3s forms
  ;;;;; e.g. table
  (COUNT-PRED-TEMPL
   (SYNTAX(W::SORT W::PRED) (W::AGR W::3s) (W::CASE (? cas W::sub W::obj)) (W::MASS W::COUNT))
   (ARGUMENTS
    ))
  
  (COUNT-PRED-SUBCAT-TEMPL
   (SYNTAX(W::SORT W::PRED) (W::AGR W::3s) (W::CASE (? cas W::sub W::obj)) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::pp (W::ptype W::of)))) ONT::FIGURE)
    ))

   (COUNT-PRED-SUBCAT-required-TEMPL
   (SYNTAX(W::SORT W::PRED) (W::AGR W::3s) (W::CASE (? cas W::sub W::obj)) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (% W::pp (W::ptype W::of)) ONT::FIGURE)
    ))
  (COUNT-PRED-SUBCAT-THEME-TEMPL
   (SYNTAX(W::SORT W::PRED) (W::AGR W::3s) (W::CASE (? cas W::sub W::obj)) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::pp (W::ptype W::of)))) ONT::FORMAL)
    ))
  
  (COUNT-PRED-SUBCAT-PROPERTY-TEMPL
   (SYNTAX(W::SORT W::PRED) (W::AGR W::3s) (W::CASE (? cas W::sub W::obj)) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::pp (W::ptype W::of)))) ONT::FORMAL)
    ))
  
  (COUNT-PRED-JUNCTION-TEMPL
   (SYNTAX(W::SORT W::PRED) (W::AGR W::3s) (W::CASE (? cas W::sub W::obj)) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp1 (:default (% W::pp (W::ptype W::of)))) ONT::FIGURE OPTIONAL)
    (SUBCAT2 (:parameter xp2 (:default (% W::pp (W::ptype W::with)))) ONT::FIGURE1 OPTIONAL)
    ))

  ;; subcats are restricted to numbers. At the moment we can't make this restriction via the
  ;; semantics
  (coordinate-TEMPL
   (SYNTAX(W::SORT W::PRED) (W::AGR W::3s) (W::CASE (? cas W::sub W::obj)) (W::MASS W::BARE))
   (ARGUMENTS
    (SUBCAT (:parameter xp1 (:default (% W::number))) ONT::FIGURE)
    (SUBCAT2 (:parameter xp2 (:default (% W::number ))) ONT::FIGURE1)
    ))

  ;;;;; Mass nouns - e.g. water
  (MASS-PRED-TEMPL
   (SYNTAX(W::SORT W::PRED) (W::AGR W::3s) (W::CASE (? cas W::sub W::obj)) (W::MASS W::MASS))
   (ARGUMENTS
    ))
  
  ;;;;; Bare nouns as defined in the documentation.
  ;;;;; E.g. lunch (can be a/the lunch, lunch without article, or lunches)
  (BARE-PRED-TEMPL
   (SYNTAX(W::SORT W::PRED) (W::AGR W::3s) (W::CASE (? cas W::sub W::obj)) (W::MASS W::BARE))
   (ARGUMENTS
    ))

  ;; he has difficulty/trouble doing that
  (mass-PRED-assoc-with-TEMPL
   (SYNTAX(W::SORT W::PRED) (W::AGR W::3s) (W::CASE (? cas W::sub W::obj)) (W::MASS W::mass))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::cp (W::ctype W::finite)))) ONT::assoc-with)
    ))

; nobody uses this
  #|
  ;; book by an author
   (COUNT-PRED-SUBCAT-ORIGINATOR-OPTIONAL-TEMPL
   (SYNTAX (W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT W::PRED) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::pp (W::ptype W::by)))) ONT::originator optional)
    ))
  |#
  
  ;;;;;
  ;;;;; Count nouns with plural form only, e.g. supplies
  ;;;;;
  (COUNT-PRED-3p-TEMPL
   (SYNTAX(W::SORT W::PRED) (W::AGR W::3p) (W::CASE (? cas W::sub W::obj)) (W::MASS W::COUNT))
   (ARGUMENTS
    ))

  ;; greens
   (MASS-PRED-3p-TEMPL
   (SYNTAX(W::SORT W::PRED) (W::AGR W::3p) (W::CASE (? cas W::sub W::obj)) (W::MASS W::MASS))
   (ARGUMENTS
    ))
  ;;;;; Substance units, e.g. volume-- liters, pounds
  ;;;;; participate in construction: X gallons of water, but not X-gallon water
  ;;;;; (SUBCAT (% PP (ptype of)) LF_OF)

  (substance-unit-templ
   (SYNTAX (W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::SORT W::SUBSTANCE-UNIT) (W::CASE (? cas 
     W::sub W::obj))(w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    ;; the argument can be either mass (gallons of water) or count (pounds of oranges). Both possibilities
    ;; must be included in the default specification or the one not included breaks
    (ARGUMENT (:parameter xp (:default (% W::PP (W::ptype W::of) (W::mass (? mas w::count W::MASS))))) ONT::FIGURE)
    ))

  (substance-unit-abbrev-templ
   (SYNTAX (W::AGR (? a W::3s W::3p));; (W::MORPH (:FORMS (-S-3P))) 
	   (W::SORT W::SUBSTANCE-UNIT) (W::CASE (? cas W::sub W::obj))
	   (w::allow-deleted-comp +) (w::abbrev +) (W::MASS W::COUNT))
   (ARGUMENTS
    ;; the argument can be either mass (gallons of water) or count (pounds of oranges). Both possibilities
    ;; must be included in the default specification or the one not included breaks
    (ARGUMENT (:parameter xp (:default (% W::PP (W::ptype W::of) (W::mass (? mas w::count W::MASS))))) ONT::FIGURE)
    ))

  (substance-unit-plural-templ
   (SYNTAX (W::AGR W::3p) (W::MORPH (:FORMS (-none))) (W::SORT W::SUBSTANCE-UNIT) (W::CASE (? cas 
     W::sub W::obj))(w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    ;; the argument can be either mass (gallons of water) or count (pounds of oranges). Both possibilities
    ;; must be included in the default specification or the one not included breaks
    (ARGUMENT (:parameter xp (:default (% W::PP (W::ptype W::of) (W::mass (? mas w::count W::MASS))))) ONT::FIGURE)
    ))

  ;;;;; certain measures only apply to singular lf-ofs (which are count nouns), e.g.
  ;;;;; mb of ram
  (substance-unit-lf-of-3s-templ
   (SYNTAX(W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::SORT W::SUBSTANCE-UNIT) (W::CASE (? cas 
     W::sub W::obj)) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (ARGUMENT (% W::PP (W::ptype W::of) (W::agr W::3s)) ONT::FIGURE)
    ))

  ;; for plural variants of measure term abbreviations, e.g. 5 mb
   (substance-unit-lf-of-3s-plur-templ
   (SYNTAX (W::AGR W::3p) (W::morph (:forms (-none))) (W::SORT W::SUBSTANCE-UNIT) (W::CASE (? cas 
     W::sub W::obj)) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (ARGUMENT (% W::PP (W::ptype W::of) (W::agr W::3s)) ONT::FIGURE)
    ))
  
  ;;;;; Attribute units, e.g. degree (temperature), length units: mile, foot, ...
  ;;;;; participate in construction: 90-degree water, 9-inch nails, but not X degrees of water
  ;;;;; although 9 inches of rope sounds ok to me, just not too common
  ;;;;; (ARGUMENT2 (% NP (number +)) LF_VAL)
  (attribute-unit-templ
   (SYNTAX(W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::SORT W::ATTRIBUTE-UNIT) (W::CASE (? cas 
     W::sub W::obj)) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (ARGUMENT (:parameter xp (:default (% W::PP (W::ptype W::of)))) ONT::FIGURE)
    ))

   ;;;;; a few constructions have the units first, mostly currency  e.g., $100
  (pre-unit-templ
   (SYNTAX(W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::SORT W::ATTRIBUTE-UNIT) (W::CASE (? cas 
     W::sub W::obj)) (w::Allow-before +) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (ARGUMENT (:parameter xp (:default (% W::PP (W::ptype W::of)))) ONT::FIGURE)
    ))

  ;; for plural variants of measure term abbreviations, e.g. 5 km
  (attribute-unit-plural-templ
   (SYNTAX (W::AGR W::3p) (W::MORPH (:FORMS (-none))) (W::SORT W::ATTRIBUTE-UNIT) (W::CASE (? cas 
     W::sub W::obj)) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (ARGUMENT (:parameter xp (:default (% W::PP (W::ptype W::of)))) ONT::FIGURE)
    ))

  ;; a week of work, a day of meetings, a moment of silence 
   (unit-templ
   (SYNTAX(W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::SORT W::UNIT-MEASURE) (W::CASE (? cas 
     W::sub W::obj))(w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    ;; the argument can be either mass (gallons of water) or count (pounds of oranges). Both possibilities
    ;; must be included in the default specification or the one not included breaks
    (ARGUMENT (:parameter xp (:default (% W::PP (W::ptype W::of) (W::mass (? mas w::count W::MASS))))) ONT::FIGURE)
    ))

  (unit-plural-templ
   (SYNTAX (W::AGR W::3p) (W::MORPH (:FORMS (-S-3P))) (W::SORT W::UNIT-MEASURE) (W::CASE (? cas 
     W::sub W::obj))(w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    ;; the argument can be either mass (gallons of water) or count (pounds of oranges). Both possibilities
    ;; must be included in the default specification or the one not included breaks
    (ARGUMENT (:parameter xp (:default (% W::PP (W::ptype W::of) (W::mass (? mas w::count W::MASS))))) ONT::FIGURE)
    ))

   
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;; Relational nouns
  ;;;;;
  ;;;;; for nouns that subcategorize for contents, e.g. boxcar, glass, shipment
  (pred-subcat-contents-templ
   (SYNTAX(W::sort W::pred) (W::AGR (? a W::3s W::3p)) (W::CASE (? cas W::sub W::obj)) (W::MASS W::COUNT 
    ))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of)))) ONT::Contents)
    ))
  
  ;;;;; for nouns that subcategorize for contents, e.g. boxcar, glass, shipment
  (pred-subcat-of-templ
   (SYNTAX(W::sort W::pred) (W::AGR (? a W::3s W::3p)) (W::CASE (? cas W::sub W::obj)) (W::MASS W::COUNT 
    ))
   (ARGUMENTS
    (SUBCAT (% W::PP (W::ptype W::of)) ONT::FIGURE)
    ))
  
  ;;;;; e.g., pair of trucks, set of dogs, ....
  (classifier-count-pl-templ
   (SYNTAX (W::sort W::classifier) 
	  (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (ARGUMENT (:parameter xp (:default (% W::PP (W::ptype W::of)
				       (W::AGR W::3p) (W::MASS W::COUNT)))) ONT::FIGURE)
    ))

  ;; a serving of food, shipment of water, ...
  (classifier-mass-templ
   (SYNTAX (W::sort W::classifier) 
	  (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (ARGUMENT (:parameter xp (:default (% W::PP (W::ptype W::of)
				       (W::MASS W::MASS)))) ONT::FIGURE)
    ))

  ;; those that take just about anything, e.g., a shipment of food, a shipment of trucks, ...
  (classifier-templ
   (SYNTAX (W::sort W::classifier) 
	  (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (ARGUMENT (:parameter xp (:default (% W::PP (W::ptype W::of) (w::mass (? ms w::mass w::count))))) ONT::FIGURE)
    ))
  
  (indef-classifier-templ
   (SYNTAX (W::sort W::classifier) (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +)
	  (W::INDEF-ONLY  +) (W::MASS W::COUNT))
   (ARGUMENTS
    (ARGUMENT (:parameter xp (:default (% W::PP (W::ptype W::of)(w::mass (? ms w::mass w::count))))) ONT::FIGURE)
    ))

  (indef-classifier-count-pl-templ
   (SYNTAX (W::sort W::classifier) (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +)
	  (W::INDEF-ONLY  +) (W::MASS W::COUNT))
   (ARGUMENTS
    (ARGUMENT (:parameter xp (:default (% W::PP (W::ptype W::of) (W::AGR W::3p) (w::mass w::count)))) ONT::FIGURE)
    ))
  ;;;;; Relational nouns tagged with other-reln
  ;;;;; distance, weight, speed
  (OTHER-RELN-TEMPL
   (SYNTAX(W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp 
			(:default (% W::PP (W::ptype W::of))) 
			(:required (W::sort (? !sort W::unit-measure))))
	    ONT::FIGURE)
      ))

  ; number of boxes
  (OTHER-RELN-subcat-count-TEMPL
   (SYNTAX(W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp 
			(:default (% W::PP (W::ptype W::of) (w::mass w::count))) ; boxes
			(:required (W::sort (? !sort W::unit-measure))))
	    ONT::FIGURE)
      ))  

  ; amount of water
  (OTHER-RELN-subcat-mass-TEMPL
   (SYNTAX(W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp 
			(:default (% W::PP (W::ptype W::of) (w::mass w::mass))) ; water
			(:required (W::sort (? !sort W::unit-measure))))
	    ONT::FIGURE)
      ))  
  
  
  ;; an impro that is a TIME-LOC
   (TIME-RELN-TEMPL
   (SYNTAX(W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp 
			(:default (% W::PP (W::ptype W::of))) 
			(:required (W::sort (? !sort W::unit-measure))))
	    ONT::FIGURE)
      ))
   
  ;; mass relational nouns
  (OTHER-RELN-MASS-TEMPL
   (SYNTAX(W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::MASS))
   (ARGUMENTS
    (SUBCAT (:parameter xp 
			(:default (% W::PP (W::ptype W::of))) 
			(:required (W::sort (? !sort W::unit-measure))))
	    ONT::FIGURE)
      ))
  
  (OTHER-RELN-subcat-required-TEMPL
   (SYNTAX(W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp -) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp 
			(:default (% W::PP (W::ptype W::of))) 
			(:required (W::sort (? !sort W::unit-measure))))
	    ONT::FIGURE)
      ))
  ;; relational nouns that use both of and for for the ont::of, e.g. route and path
  (OTHER-RELN-of-for-TEMPL
   (SYNTAX(W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +)(W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp 
			(:default (% W::PP (W::ptype (? p W::of w::for)))) 
			(:required (W::sort (? !sort W::unit-measure))))
	    ONT::FIGURE)
      ))

  ;; for nominalizations where the ont::of role is the ont::formal of the verb
  ;; configuration of the system
  (OTHER-RELN-formal-TEMPL
   (SYNTAX (w::agr w::3s) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of))) (:required (W::sort (? !sort W::unit-measure)))) 
     ont::formal)
    ))

(OTHER-RELN-theme-TEMPL   ;; backwards compatability
   (SYNTAX (w::agr w::3s) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of))) (:required (W::sort (? !sort W::unit-measure)))) 
     ont::FIGURE)
    ))

  (OTHER-RELN-result-TEMPL
   (SYNTAX (w::agr w::3s) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of))) (:required (W::sort (? !sort W::unit-measure)))) 
     ONT::result)
    ))

  ;; for nominalizations where the ont::of role is the ont::formal1 of the verb
  ;; indication of the problem
  (OTHER-RELN-formal1-TEMPL
   (SYNTAX (W::AGR w::3s) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of))) (:required(W::sort (? !sort W::unit-measure)))) 
     ont::formal1)
    ))
  
  ;; for nominalizations where the ont::of role is the ont::affected of the verb
  ;; configuration of the system
  (OTHER-RELN-affected-TEMPL
   (SYNTAX (W::AGR w::3s) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of))) (:required(W::sort (? !sort W::unit-measure)))) 
     ONT::affected)
    ))

; nobody uses this
#|
 (OTHER-RELN-addressee-TEMPL
   (SYNTAX (W::AGR w::3s) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of))) (:required(W::sort (? !sort W::unit-measure)))) 
     ONT::addressee)
    ))
|#

(OTHER-RELN-attribute-TEMPL
   (SYNTAX (W::AGR w::3s) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of))) (:required(W::sort (? !sort W::unit-measure)))) 
     ONT::affected)
    ))

  ;; for nominalizations where the ont::of role is the ont::experiencer of the verb
  ;; death of a salesman
  (OTHER-RELN-experiencer-TEMPL
   (SYNTAX (w::agr w::3s) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of))) (:required(W::sort (? !sort W::unit-measure)))) 
     ONT::experiencer)
    ))

  ;; for nominalizations where the ont::of role is the ont::agent of the verb
  ;; cry of the wolf
  (OTHER-RELN-agent-TEMPL
   (SYNTAX (W::AGR w::3s) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of))) (:required(W::sort (? !sort W::unit-measure)))) 
     ONT::agent)
    ))

 (OTHER-RELN-agent1-TEMPL
   (SYNTAX (W::AGR w::3s) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of))) (:required(W::sort (? !sort W::unit-measure)))) 
     ONT::agent1)
    ))

; nobody uses this
#|
(OTHER-RELN-spatial-loc-TEMPL
   (SYNTAX (W::AGR w::3s) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of))) (:required(W::sort (? !sort W::unit-measure)))) 
     ONT::agent)
    ))
|#

; nobody uses this
#|
  (OTHER-RELN-associated-info-count-TEMPL
   (SYNTAX (w::agr w::3s) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of))) (:required(W::sort (? !sort W::unit-measure)))) 
     ONT::associated-information)
    ))
|#

; nobody uses this
#|
   (OTHER-RELN-associated-info-mass-TEMPL
   (SYNTAX (w::agr w::3s) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::MASS))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of))) (:required(W::sort (? !sort W::unit-measure)))) 
     ONT::associated-information)
    ))
|#

  ;; for nominalizations where the ont::of role is the ont::agent of the verb
  (OTHER-RELN-cause-TEMPL
   (SYNTAX (W::AGR w::3s) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of))) (:required(W::sort (? !sort W::unit-measure)))) 
     ONT::agent)
    ))

   ;; for nominalizations where the ont::of role is the ont::cognizer of the verb
  ;; beliefs of the people
  (OTHER-RELN-cognizer-TEMPL
   (SYNTAX (W::AGR W::3s) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of))) (:required(W::sort (? !sort W::unit-measure)))) 
     ONT::cognizer)
    ))

  ;; for nominalizations where the ont::of role is the ont::source of the verb
  (OTHER-RELN-source-TEMPL
   (SYNTAX (W::AGR w::3s) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of))) (:required(W::sort (? !sort W::unit-measure)))) 
     ONT::source)
    ))

 #|| ;; for nominalizations where the ont::of role is the ont::goal of the verb
  (OTHER-RELN-goal-TEMPL
   (SYNTAX (W::AGR w::3s) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::mass))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of))) (:required(W::sort (? !sort W::unit-measure)))) 
     ONT::result)
    ))||#
    
  ;;; mass nouns that subcategorize an effect role
  (SUBCAT-mass-EFFECT-TEMPL
   (SYNTAX(W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::PRED) (w::allow-deleted-comp +) (W::MASS W::MASS))
   (ARGUMENTS
;    (SUBCAT (:parameter xp (:default (% W::cp (W::ctype W::s-to) (W::subj ?lsubj)))) ONT::EFFECT)
    (SUBCAT (:parameter xp (:default (% W::cp (W::ctype W::s-to) (W::subj ?lsubj)))) ONT::FORMAL)
    ))

   (SUBCAT-inf-TEMPL
    (SYNTAX (W::SORT  W::PRED) 
	    (w::allow-deleted-comp +) (W::MASS W::COUNT))
    (ARGUMENTS
     (SUBCAT (:parameter xp (:default (% W::cp (W::ctype W::s-to) (W::subj ?lsubj)))) ONT::FIGURE)
     ))

  (MASS-SUBCAT-inf-TEMPL
    (SYNTAX (W::SORT  W::PRED) 
	    (w::allow-deleted-comp +) (W::MASS W::MASS))
    (ARGUMENTS
     (SUBCAT (:parameter xp (:default (% W::cp (W::ctype W::s-to) (W::subj ?lsubj)))) ONT::FIGURE)
     )) 

  ;;; nouns that subcategorize for a "that" clause
  ;; the constraint that it must be less than 500 dollars
  (count-subcat-that-optional-templ
   (SYNTAX(W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::PRED) ;(w::allow-deleted-comp +)
(W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::cp (W::ctype W::s-finite) (W::subj ?lsubj)))) ONT::formal optional)
    ))

  (count-subcat-that-templ
   (SYNTAX(W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::PRED) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::cp (W::ctype W::s-finite) (W::subj ?lsubj)))) ONT::formal)
    ))  

  (count-subcat-that-subjunctive-optional-templ
   (SYNTAX(W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::PRED) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::cp (W::ctype W::s-that-subjunctive) (W::subj ?lsubj)))) ONT::formal optional)
    ))

  (count-subcat-that-subjunctive-templ
   (SYNTAX(W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::PRED) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::cp (W::ctype W::s-that-subjunctive) (W::subj ?lsubj)))) ONT::formal)
    ))

   (count-subcat-to-optional-templ
      (SYNTAX(W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::PRED) (w::allow-deleted-comp +) (W::MASS W::COUNT))
     (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::cp (W::ctype W::s-to) (W::subj ?lsubj)))) ONT::formal optional)
    ))

  (count-subcat-to-templ
      (SYNTAX(W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::PRED) (w::allow-deleted-comp +) (W::MASS W::COUNT))
     (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::cp (W::ctype W::s-to) (W::subj ?lsubj)))) ONT::formal)
    ))
; nobody uses this
#|
  ;; reason for the appointment
   (count-subcat-purpose-templ
   (SYNTAX(W::AGR (? a W::3s W::3p)) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::PRED) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
;    (SUBCAT (:parameter xp (:default (% W::pp (W::ptype W::for)))) ONT::purpose optional)
    (SUBCAT (:parameter xp (:default (% W::pp (W::ptype W::for)))) ONT::REASON optional)
    ))
|#

  ;; get approval (for the purchase)
  (OTHER-RELN-EFFECT-TEMPL
   (SYNTAX (W::AGR W::3s) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
;    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype (? pt w::for W::of)) (W::sort (? !sort W::unit-measure))))) ONT::EFFECT)
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype (? pt w::for W::of)) (W::sort (? !sort W::unit-measure))))) ONT::FORMAL)
    ))

(OTHER-RELN-goal-TEMPL
   (SYNTAX (W::AGR W::3s) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::OTHER-RELN) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype (? pt w::for W::of)) (W::sort (? !sort W::unit-measure))))) ONT::GOAL)
    ))

  ;;;;; for nouns that subcategorize for unit phrases, e.g. distance of 5 miles, weight of 5 lbs
  ;; this is not optional as it usually occurs in conjunction the OTHER-RELN-TEMPL 
  (reln-subcat-of-units-templ
   (SYNTAX(W::sort W::other-reln) (W::AGR (? a W::3s W::3p)) (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp -) (W::MASS W::COUNT))
   (ARGUMENTS
    (ARGUMENT (% W::NP (W::sort (? !sort W::unit-measure))) ONT::FIGURE)
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of) (W::sort (? s w::unit-measure))))) ONT::EXTENT)
    ))

(reln-subcat-of-units-mass-templ
   (SYNTAX(W::sort W::other-reln) (W::AGR (? a W::3s W::3p)) (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp -) (W::MASS W::MASS))
   (ARGUMENTS
    (ARGUMENT (% W::NP (W::sort (? !sort W::unit-measure))) ONT::FIGURE)
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of) (W::sort (? s w::unit-measure))))) ONT::EXTENT)
    ))

  ;; level of five; bare number subcat
    (reln-subcat-of-number-templ
   (SYNTAX(W::sort W::other-reln) (W::AGR (? a W::3s W::3p)) (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +) (W::MASS 
      W::COUNT))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::FIGURE)
     (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of)))) ONT::GROUND) ;optional)
     ))

  ;; these are for words like similarity, which have theme (difference between 2 points) and property (difference in charge)
  ;; The property goes as subcat, so "their difference" is likely the difference in property
  ;; And the theme as argument, which means it is possible, but not expressed as impro
  (reln-scale-neutral-templ
   (SYNTAX(W::sort W::other-reln)  (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (ARGUMENT  (:parameter xp1 (:default (% W::NP (W::sort (? !sort W::unit-measure))))) ONT::FORMAL)
    (SUBCAT (:parameter xp2 (:default (% W::PP (W::ptype w::of)))) ONT::NEUTRAL)
    ))
    
(other-reln-neutral-templ
    (SYNTAX(W::sort W::other-reln)  (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +) (W::MASS W::COUNT))
    (ARGUMENTS
     (SUBCAT (:parameter xp (:default (% W::PP (W::ptype w::of)))) ONT::neutral)
     ))

(other-reln-neutral1-templ
    (SYNTAX(W::sort W::other-reln)  (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +) (W::MASS W::COUNT))
    (ARGUMENTS
     (SUBCAT (:parameter xp (:default (% W::PP (W::ptype w::of)))) ONT::neutral1)
     ))
  
 (other-reln-cost-templ
   (SYNTAX(W::sort W::other-reln)  (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
;    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype w::of)))) ONT::cost)
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype w::of)))) ONT::EXTENT)
    ))

  ;;  ratio
  (reln-of-ground-templ
   (SYNTAX (W::sort W::other-reln)  (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp1 (:default (% W::PP (W::ptype w::of)))) ONT::FIGURE)
;    (SUBCAT2 (:parameter xp2 (:default (% W::PP (W::ptype w::to)))) ONT::FIGURE1)
    (SUBCAT2 (:parameter xp2 (:default (% W::PP (W::ptype w::to)))) ONT::GROUND)
    ))
  
  ;; effect of A on B
  
  (reln-cause-affected-templ
   (SYNTAX(W::sort W::other-reln)  (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (ARGUMENT  (:parameter xp1 (:default (% W::PP (W::ptype w::of)))) ONT::agent)
    (SUBCAT (:parameter xp2 (:default (% W::PP (W::ptype w::on)))) ONT::affected)
    ))

  (reln-agent-affected-optional-templ
   (SYNTAX(W::sort W::other-reln)  (W::CASE (? cas W::sub W::obj))
	  (w::allow-deleted-comp +) ; this is so that it can go through n1-reln1 (even though the subcat subcat2 are optional)
	  (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT  (:parameter xp1 (:default (% W::PP (W::ptype w::of)))) ONT::agent optional)
    (SUBCAT2 (:parameter xp2 (:default (% W::PP (W::ptype w::on)))) ONT::affected optional)
    ))

  ;; relationship, relation
  (reln-between-neutral-templ
   (SYNTAX(W::sort W::other-reln)  (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp1 (:default (% W::PP (W::ptype w::between)))) ont::neutral)
    ))
  ;;;;; Relational nouns tagged with kinship-reln, e.g. mother
  (KINSHIP-RELN-TEMPL
   (SYNTAX(W::AGR W::3S) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +) (W::SORT 
      W::KINSHIP-RELN) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of)))) ONT::FIGURE)
    ))
  
  ;;;;; Relational nouns tagged with body-part-reln, e.g. hand
  (BODY-PART-RELN-TEMPL
   (SYNTAX(W::AGR W::3S) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +) (W::SORT 
      W::BODY-PART-RELN) (W::MASS W::COUNT))
   (ARGUMENTS
    (subcat (:parameter xp (:default (% W::PP (W::ptype W::of)))) ONT::FIGURE)
    ))
  
  ;;;;; Relational nouns tagged with part-of-reln, e.g. wheel
  (PART-OF-RELN-TEMPL
   (SYNTAX(W::AGR W::3S) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +) (W::SORT 
      W::PART-OF-RELN) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of)))) ONT::FIGURE)
    ))
  
  ;;;;; Relational nouns tagged with generalized-part-of-reln, e.g. top
  (GEN-PART-OF-RELN-TEMPL
   (SYNTAX(W::AGR W::3S) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +) (W::SORT 
      W::GEN-PART-OF-RELN) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of)))) ONT::FIGURE)
    ))

  ; on top of
  (GEN-PART-OF-RELN-BARE-TEMPL
   (SYNTAX(W::AGR W::3S) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +) (W::SORT 
      W::GEN-PART-OF-RELN) (W::MASS W::BARE))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of)))) ONT::FIGURE)
    ))

; nobody uses this
#|
    ;;;;; Relational nouns tagged with generalized-part-of-reln, but where the "of" subcat may map to an event
  ;; e.g. the end of the meeting
  (GEN-PART-OF-RELN-ACTION-TEMPL
   (SYNTAX(W::AGR W::3S) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +) (W::SORT 
      W::GEN-PART-OF-RELN) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of)))) ONT::Action)
    ))

     ;;;;; Relational nouns tagged with generalized-part-of-reln, but where the "of" subcat may map to a time
  ;; e.g. the end of the year / 2008
  (GEN-PART-OF-RELN-INTERVAL-TEMPL
   (SYNTAX(W::AGR W::3S) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +) (W::SORT 
      W::GEN-PART-OF-RELN) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype W::of)))) ONT::interval)
    ))
|#

  ;;;;; Relational nouns which are derived nominals, e.g. driver
  (DRV-NOM-RELN-TEMPL
   (SYNTAX(W::AGR W::3S) (W::MORPH (:FORMS (-S-3P))) (W::CASE (? cas W::sub W::obj)) (W::SORT 
      W::DRV-NOM-RELN) (W::MASS W::COUNT)  (w::allow-deleted-comp +))
   (ARGUMENTS
    
    ;;;;;; !!!! Fix this - as we handle nominalizations, this will have to be split into multiple templates with appropriate roles
    (SUBCAT (% W::PP (W::ptype W::of)) ONT::NOROLE)
    ))
  
  ;;;;; letters are count nouns but have irregular plurals e.g., a's
  (LETTER-TEMPL
   (SYNTAX(W::SORT W::PRED) (W::AGR W::3s) (W::CASE (? cas W::sub W::obj)) (W::MASS W::COUNT) (
     W::^S-PLURAL +))
   (ARGUMENTS
    ))

  ;; swift 09/01/2006 this isn't used -- there is no (sort number-unit) in the grammar
  ;;;;; number units such as hundred, dozen, ...
  ;; NB "a hundred of them" *"a hundred of oranges" "a hundred oranges" "a hundred of the oranges"
  (NUMBER-UNIT-TEMPL
   (SYNTAX(W::SORT W::NUMBER-UNIT) (W::AGR ?agr) (W::NUMBER +) (W::MORPH (:forms (-S-3P))) (W::CASE (? cas 
     W::sub W::obj)) (W::MASS W::COUNT)
     (W::QOF (% W::PP (W::PTYPE W::OF) (w::agr ?nagr) (w::mass w::count))) )
   (ARGUMENTS
    (SUBCAT (% W::PP (W::ptype W::of) (w::agr ?nagr) (w::mass w::count)) ont::FIGURE)
    )
   )
 ))

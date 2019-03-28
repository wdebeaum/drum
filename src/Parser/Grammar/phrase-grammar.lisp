;;;;
;;;; phrase-grammar.lisp
;;;;

(in-package :W)

;; VC verb crossing indicates when there's a verb between a modifier and
;; a head, e.g. will be present in relative clauses like
;; the man driving the truck
;; VC + is added as a default feature for allverbs in *pos-defaults* in LexiconManager/Code/lexicon-DB.lisp

;(parser::define-foot-feature 'VC) ; 2010 -- not currently used

;; dys nil is added as a default feature for all words in *pos-defaults* in LexiconManager/Code/lexicon-DB.lisp
;; for disfluency detection
;(parser::define-foot-feature 'dys) ; 2010 -- not currently used

;; WH is a foot feature: it should not appear in the mother constituent of a rule
(parser::define-foot-feature 'WH)
(parser::define-foot-feature 'WH-VAR)

(parser::set-default-rule-probability .99)

(parser::addLexicalCat 'cv);; contraction
(parser::addLexicalCat '^);; quote
(parser::addLexicalCat '^S)
(parser::addLexicalCat 'adv)
(parser::addLexicalCat 'conj)
(parser::addLexicalCat 'ordinal)
(parser::addLexicalCat 'POSS);; possessive pronoun
(parser::addLexicalCat 'quan);; quantifer
(parser::addLexicalCat '@);; colon
(parser::addLexicalCat 'punc)
(parser::addLexicalCat 'prep)
(parser::addLexicalCat 'neg)



;;  Basic structure of NP rules

;;     NP -> SPEC N1
;;     SPEC -> DET ORDINAL CARDINAL UNIT
;;     N1 -> QUAL* N

;;   SPEC Rulest
;;     This uses a set of features to collect info on spec
;;       SEM - DEFINITE/INDEFINITE/QUANTIFIER
;;       LF - the actual determiner/quantifier
;;       ARG - the placeholder for the object described
;;       RESTR - restrictions on object described, including
;;         (SIZE  N) - cardinality
;;         (SEQUENCE N) - position in sequence (e.g., first, ...)
;;       POSS  - non-null only if possessive determiner, and set to NP that is possessor (at least fro VAR and SEM)

;; Quantifiers appear in different constructions, here's how we tell them apart
;;    Standard Bare forms:  e.g., all boys, each truck, much sand, ...  requires NOSIMPLE -, and AGR and MASS agreement
;;    OF forms: e.g., all of the boys, each of the trucks, much of the sand, ... requires PP SUBCAT agreement
;;    NP forms: e.g., all the trucks, both the trucks, ...   requires NPMOD +
;;    cardinality forms: e.g., the several trucks, the many trucks, and many more, several more,  ...  requires CARDINALITY +
;;    bare constructions: e.g., many arrived yesterday, most died, ... requires NoBareSpec -

(parser::augment-grammar
;;(cl:setq *grammar-SPEC*
      '((headfeatures
	 (DET VAR lex orig-lex headcat transform)
         (SPEC VAR lex orig-lex headcat transform SORT POSS)
	 (possessor VAR SEM lex orig-lex headcat transform)
	 (CARDINALITY lex orig-lex headcat transform nobarespec qof)
	 (N var mass agr sort lex orig-lex headcat transform)
	 (NP VAR CASE MASS NAME agr SEM PRO CLASS lex orig-lex headcat transform postadvbl)
	 (QUANP headcat lex orig-lex)
	 )

	;;  QUANTIFICATIONS/SPECIFIER STRUCTURE FOR NOUN PHRASES
	
        ;;  DEFINITE/INDEFINITE FORMS
	
	;;  e.g., the, a
	((SPEC (SEM ?def) (AGR ?agr) (MASS ?m) (ARG ?arg) (NObareSpec +)  (lex ?lex) (LF ?l) (restr ?r))
	 -spec-det1> .995
	 (head (DET (sem ?def) (ARG ?arg) (AGR ?agr) (WH -) (MASS ?m) (lex ?lex) (LF ?l) (restr ?r))))


	;;  indefinites allow negation, not a man came

	((SPEC (SEM ?def) (AGR ?agr) (MASS ?m) (ARG ?arg) (NObareSpec +)  (lex ?lex) (LF ont::indefinite) (restr ?rr))
	 -spec-det-neg>
	 (word (lex not))
	 (head (DET (sem ?def) (ARG ?arg) (AGR ?agr) (WH -) (MASS ?m) (lex ?lex) (LF ont::indefinite) (restr ?r)))
	 (add-to-conjunct (val (negation +)) (old ?r) (new ?rr)))
	
	;; e.g., which, what    -- just like spec-det1> except for adding the wh-var feature
	((SPEC (SEM ?def) (AGR ?agr) (MASS ?m) (ARG ?arg) (NObareSpec +)  (lex ?lex) (LF ?l)
	       (RESTR ?newr) (WH ?!wh) (wh-var ?arg))
	 -spec-whdet1> 1
	 (head (DET (sem ?def) (AGR ?agr) (WH ?!wh) 
		    (MASS ?m) (lex ?lex) (poss -) (RESTR ?R) (LF ?l)))
	 (add-to-conjunct (old ?r) (val (PROFORM ?lex)) (new ?newr)))

	;; e.g., which, what    -- just like spec-det1> except for adding the wh-var feature
	((SPEC (SEM ?def) (AGR ?agr) (MASS ?m) (ARG ?arg) (NObareSpec +)  (lex ?lex) (LF ?l)
	       (RESTR ?newr) (WH ?!wh) (wh-var ?wh-var))
	 -spec-whdet1-poss> 1
	 (head (DET (sem ?def) (AGR ?agr) (WH ?!wh) (wh-var ?wh-var)
		    (MASS ?m) (lex ?lex) (poss +) (RESTR ?R) (LF ?l)))
	 (add-to-conjunct (old ?r) (val (PROFORM ?lex)) (new ?newr)))
      
	;; e.g., the first
	((SPEC (SEM ?def) (AGR ?agr) (MASS ?m) (ARG ?arg) (LF ?l) (RESTR ?newr) (WH-VAR ?wh-var)
	  (SUBCAT (% PP (PTYPE of) (SEM ?anysem))))
	 -spec2>
	 (head (DET (sem ?def) (MASS ?m) (agr ?agr) 
		(LF ?l) (RESTR ?R)))
	 (ordinal (LF (NTH ?q)) (lex (? !lex w::half w::quarter)))
         (add-to-conjunct (val (ORDINAL ?q)) (old ?r) (new ?newr)))

	;; one
	((SPEC (SEM ?def) (var ?v) (AGR ?agr) (MASS count) (ARG ?arg) (lex ?lex) (LF ONT::INDEFINITE) 
	  (subcat ?xx)
	  (RESTR (& (size (% *PRO* (status ont::indefinite) (class ONT::NUMBER)
					 (constraint ?cc) (var *))))))
	 -spec-one>
	 (head (NUMBER (val 1) (var ?v) (sem ?def) (AGR ?agr) (WH -) (lex ?lex) (RESTR ?R) (LF ?l)))
	 (add-to-conjunct (val (value 1)) (old ?r) (new ?cc))
	 )

	#| ; go through -n1-card-more> instead
        ;; e.g., two more, one less, some more, ...
	;; check interpretation?? shouldn't there be an impro -- two more than what?
	;;    this allows MORE or LESS to be attached to cardinality specifiers
	((SPEC (AGR ?agr) (arg ?v) ;(ARG ?arg)
	       (LF ?status) (MASS ?mass) (status ?status)
	       (PRED ?s)
	       (RESTR ?restr1)
	       ;(SUBCAT (% PP (PTYPE of) (SEM ?subsem)))
	       (subcat ?sc)
	       (QCOMP ?qcomp)
	       (nobarescpec ?nb) 
	       )
	 -Spec-comp> .97 ;.95 ;; prefer to attach to NP
	 (head (SPEC (AGR ?agr) (ARG ?arg) (LF ?status) (MASS ?mass)
		     (PRED ?s)
		     (RESTR ?restr)
		     ;(SUBCAT (% PP (PTYPE of) (SEM ?subsem)))
		     (subcat ?sc)
		     (QCOMP -) (nobarescpec ?nb)))
	 (QUAN (COMPARATIVE +) (VAR ?av) (LF ?cmp) (QCOMP ?qcomp))
	 (add-to-conjunct (val (MOD (% *PRO* (status ont::f) (class ?cmp) (VAR *)
		       (constraint (& (figure ?v) (ground (% *PRO* (var **) (class ?c))) ))))) ;(val (quan ?cmp))
			  (old ?restr) (new ?restr1))
	 )
	|#

	;;  DETERMINERS:  articles, possessives, quantifiers

	;; e.g., the
	((DET (SEM ?def) (AGR ?a) (ARG ?arg) (MASS ?m) (LF ?l))
	 -det1> 1.0
	 (head (art (sem ?def) (DIECTIC -) (MASS ?m) (AGR ?a) (LF ?l))))

        ;; e.g., that, this, ...
	((DET (SEM ?def) (AGR ?a) (ARG ?arg) (MASS ?m) (LF ?l) (RESTR (& (PROFORM ?lex))))
	 -det-diectic> 1.0
	 (head (art (sem ?def) (DIECTIC +) (MASS ?m) (lex ?lex) (AGR ?a) (LF ?l))))
	
	;;  Possessive times
	;; e.g., today's/monday's weather report
	((DET (LF ONT::DEFINITE) (AGR ?agr) (ARG ?arg) (MASS ?m) (RESTR (& (assoc-with ?v)))
               (NObareSpec +))
	 -possessive1-time> 1.0
	 (head (NP (SEM ?sem) (VAR ?v) (sort pp-word))) (^S))

	;; possessives become determiners - we use an intermediate constit POSSESSOR to allow the modifier "own", as in "his own truck"
	((DET (LF ONT::DEFINITE) (AGR ?agr) (wh-var ?wh-var) (ARG ?arg) (mass ?m) (restr ?r) (poss +)
               (NObareSpec +))
	 -possessor1> 1
	 (head (Possessor  (AGR ?agr)  (wh-var ?wh-var) (ARG ?arg) (mass ?m) (restr ?r))))

	;;  possessive OWN construction. e.g., his own truck, john's very own house
	((Possessor  (AGR ?agr) (ARG ?arg) (mass ?m) 
	  (restr ?newr) (own +))  ;; no-bare-spec is not + to allow "his own"
	 -possessor2> 1
	 (head (Possessor  (AGR ?agr) (ARG ?arg) (mass ?m) (poss ?poss) (restr ?r) (own -)))
	 (adjp (lex W::own) (lf ?lf) (arg ?arg) (var ?av))
	 (add-to-conjunct (val (:mod ?av));;(% *PRO* (STATUS ONT::F) (CLASS ?lf) (VAR ?av) 
				;;	(CONSTRAINT (& (?subj ?poss) (?dobj ?arg))))))
	  (old ?r) (new ?newr)))
	
	;;  Possessive constructs: two versions of each rule, one as possessor, and other as argument to a relational noun
	;; e.g., the man's book
	;; Myrosia added a restriction (sort pred) to prevent wh-desc prhases appearing in this rule
	((possessor (LF ONT::DEFINITE) (AGR ?agr) (ARG ?arg) (MASS ?m) (RESTR (& (assoc-poss ?v)))
               (NObareSpec +))
	 -possessive1>  1
	 (head (NP (PRO (? xx - INDEF RECIP)) ; RECIP is for "each other"
		   (gerund -) (generated -) (time-converted -) (SEM ?sem) (VAR ?v) (sort pred))) (^S))


	
;;    DELETING THE RELN POSS rules - we will do this as an infrerence process in the IM
#||	;; possessor of a relational noun e.g. the man's hand
	;; Myrosia added a restriction (sort pred) to prevent wh-desc prhases appearing in this rule
        ((possessor (LF DEFINITE) (AGR ?agr) (ARG ?arg) (mass ?m) (poss ?v) (restr ?r)
               (NObareSpec +))
	 -possessive1reln>
	 (head (NP (PRO -) (SEM ?sem)  (VAR ?v) (sort pred) (restr ?r) (postadvbl -))) (^S))
||#	
	;;  e.g., the engines' problems
	;; Myrosia added a restriction (sort pred) to prevent wh-desc prhases appearing in this rule
	((possessor (LF ONT::DEFINITE) (AGR ?agr) (ARG ?arg) (MASS ?m) (RESTR (& (assoc-poss ?v)))
               (NObareSpec +))
	 -possessive2> 1
	 (head (NP (PRO -) (SEM ?sem) (VAR ?v) (agr 3p) (gerund -) (name -) (sort pred) (headless -))) (^))
#||
	;; plural possessor of relational noun - the engines' wheels
	;; Myrosia added a restriction (sort pred) to prevent wh-desc prhases appearing in this rule
        ((possessor (LF DEFINITE) (AGR ?agr) (ARG ?arg) (POSS (% NP (VAR ?v) (SEM ?sem)))
               (NObareSpec +))
	 -possessive2reln>
	 (head (NP (PRO -) (SEM ?sem) (VAR ?v) (sort pred))) (^))
||#	
	;;   e.g., his book
	;; Myrosia 2003/11/04 changed NP to PRO. Genitives are covered by possessive1 and possessive2 rules
	;; so this should only be pronouns
	((possessor (LF ONT::DEFINITE) (AGR ?agr) (WH-VAR ?arg) (MASS ?m) (ARG ?arg1)
	      (RESTR (& (assoc-poss ?arg)))(WH R)
			 ;;(% *PRO* (VAR ?v) (SEM ?sem)
			 ;;(STATUS ONT::PRO) (class ?lf) (constraint (& (proform ?lex)))))))
	      (NObareSpec +))	 
	 -possessive3-whose-rel-clause> 1
	 (head (PRO (CASE POSS) (WH R) 
		    (STATUS ont::PRO-DET) (SEM ?sem) (VAR ?v) (LF ?lf) (lex ?lex) (input ?i))))

	((possessor (LF ONT::DEFINITE) (AGR ?agr) (ARG ?arg) (MASS ?m)
	  (RESTR (& (assoc-poss
		     (% *PRO* (VAR ?v) (SEM ?sem)
			(STATUS *WH-TERM*) (class ?lf) (constraint (& (LEX ?lex) (proform ?lex)))))))
	  (NObareSpec +) (WH Q) (wh-var ?v))	 
	 -possessive3-Q> 1
	 (head (PRO (CASE POSS) (WH Q)
		    (STATUS ont::PRO-DET) (SEM ?sem) (VAR ?v) (LF ?lf) (lex ?lex) (input ?i))))

	((possessor (LF ONT::DEFINITE) (AGR ?agr) (ARG ?arg) (MASS ?m)
	  (RESTR (& (assoc-poss
		     (% *PRO* (VAR ?v) (SEM ?sem)
			(STATUS ont::PRO) (class ?lf) (constraint (& (LEX ?lex) (proform ?lex)))))))
	  (NObareSpec +) (WH -))
	 -possessive3> 1
	 (head (PRO (CASE POSS) (WH -)
		    (STATUS ont::PRO-DET) (SEM ?sem) (VAR ?v) (LF ?lf) (lex ?lex) (input ?i))))
#||
	((possessor (LF DEFINITE) (AGR ?agr) (ARG ?arg) (mass ?m)
	      (POSS (% *PRO* (VAR ?v) (SEM ?sem)(STATUS ONT::PRO) (class ?lf) (constraint (& (proform ?lex)))))
              (NObareSpec +))
         possessive3reln>
	 (head (PRO (CASE POSS) (SEM ?sem) (VAR ?v) (LF ?lf) (LEX ?lex) (input ?i))))
||#
	;;  thirty first, twenty fourth, etc
	((ORDINAL (LF (nth ?newval)) (NTYPE ?ntype) (AGR ?agr) (headcat ordinal) (var ?v) (complex +) (mass count))
	 -ordinal>
	 (NUMBER (VAL ?num) (var ?v))
	 (ORDINAL  (LF (NTH ?lastdigit)) (complex -))
	 (compute-val-and-ntype (expr (+ ?num ?lastdigit)) (newval ?newval) (ntype ?ntype)))

	;;  ORDINAL PHRASES  (e.g., 1st, 2nd, ...)
	((ORDINAL (LF (nth ?digit)) (NTYPE ?ntype) (AGR ?agr) (headcat ordinal) (var ?v) (mass count))
	 -ordinal1>
	 (NUMBER (NTYPE ?ntype) (VAL ?digit) (var ?v))
	 (Punc (lex W::punc-ordinal)))
        ;;  phrases that indicate CARDINALITY
	
	;;  e.g., seven trucks (this is now handled by n1-qual-set1)
	;; but this rule is still used in constructions like "3 mile wide river" (now number processed directly in adj-unit-modifier>)
	;; -- 8/27/08 no longer used, commenting out
;	((CARDINALITY
;	  (LF (% DESCRIPTION (VAR ?v) (STATUS QUANTITY-TERM) (CLASS ONT::NUMBER) (constraint ?con))) ;(CONSTRAINT (& (VALUE ?c)))))
;	  (VAR ?v)
;	  (AGR ?a) (STATUS INDEFINITE) (mass (? mass count bare))
;;	  ;; allow this to get 'greater than 5'
;;	  (nobarespec +) ;; disallow bare numbers interpreted as bare specifiers 
;	  )
;	 -cardinality1> 
;	 (head (NUMBER (val ?c) (VAR ?v) (AGR ?a) (restr ?r)))
;	 (add-to-conjunct (val (:value ?c)) (old ?r) (new ?con))
;	 )

        ;;  e.g.,  needed for phrases like (the) many (trucks), few, several
	;; NB "few/many dogs arrived" goes through -QUAN-CARD-SIMPLE-SPEC> but the few/many dogs arrived uses this rule
        
	((ADJP (ARG ?arg) (ARGUMENT (% NP))
	  (AGR ?a) (sort pred) (VAR ?v) (sem ?sem) (atype w::central) (comparative -) (set-modifier +) 
	  (LF (% DESCRIPTION (STATUS ont::indefinite) (var ?v) (CLASS ONT::NUMBER) (constraint (& (:value ?c)))))
	  (post-subcat -)
	  )
         -cardinality2>
         (head (quan (CARDINALITY +) (VAR ?v) (LF ?c) (mass (? mass count bare)) (STATUS ?status) (AGR ?a)))
	 )

	((ADJP (ARG ?arg) (ARGUMENT (% NP))
	  (AGR ?a)
	  (sort pred) (VAR ?v) (sem ?sem) (atype w::central) (comparative -) (set-modifier +) 
	  (LF (% DESCRIPTION (STATUS ont::indefinite) (var ?v) (CLASS ONT::NUMBER) (constraint ?newc)))
	  (post-subcat -)
	  )
	 -card-number>
	 (head (NUMBER (val ?c) (lf ?lf) (VAR ?v) (AGR ?a) (restr ?r) (sem ?sem))) ;;(ntype !negative)))
	 ;;(GT (arg1 ?c) (arg2 -1)) ;; negative numbers can't be cardinalities  -- I removed this as it caused "under 500 trucks" to fail
	 (add-to-conjunct (val (:value ?c)) (old ?r) (new ?newc))
	 )

        ;;  We need special treatment of number units, as they act like numbers sometimes, as in "the hundred trucks",
        ;;   but note we can't say *hundred trucks and can say "a hundred trucks", "many hundred trucks", "hundreds of trucks"
        ;;   none of which is OK for numbers like seven

        ;;  e.g., many thousand, a few hundred
        ((CARDINALITY (LF (% DESCRIPTION (VAR ?v) (status ont::indefinite) (CLASS ?unit) (CONSTRAINT (& (AMOUNT ?c)))))
		      (AGR 3p) (STATUS ?status) (VAR ?v)
	              (mass (? mass count bare))
	              )
         -cardinality-quan-number-units1>
	 (quan (CARDINALITY +) (VAR ?qv) (LF ?c) (STATUS ?status) (AGR ?a))
	 (head (NUMBER-UNIT (lf ?unit) (AGR 3s) (var ?v)))
         )
	
	 ;;  e.g., a thousand, the hundred, ...
	;;  This produces a QUANP rather than CARDINALITY since we need to pass up the determiner
	
        ((QUANP (LF (% DESCRIPTION (VAR ?v) (status ont::indefinite) (CLASS ?unit)
		       (CONSTRAINT (& (QUAN (% *PRO* (VAR *) (CLASS ONT::NUMBER)
					       (status ont::indefinite) (CONSTRAINT (& (lex ?lex) (AMOUNT 1)))))))))
		(AGR 3p) (STATUS ?l)
		(mass (? mass count bare))
		(VAR ?v)
		)
         -cardinality-quan-number-units2>
	 (det (sem ?def) (AGR 3s) (LF ?l)) 
         (head (NUMBER-UNIT (lf ?unit) (lex ?lex) (AGR 3s) (var ?v)))
         )
 
        ;; Number units may also form cardinality experssions in their bare form, but this
        ;;   cannot become a quantifier (e.g., we can't say *hundred trucks)
        ;;  e.g., (a) hundred, (the first) dozen, ...
	
        ((CARDINALITY (LF (% DESCRIPTION (VAR ?v) (status ont::indefinite) (CLASS ?unit) (CONSTRAINT (& (VALUE ?c)))))
		      (VAR ?v) (NOQUAN +)
                      (AGR ?a) (STATUS ?status) (mass (? mass count bare))		   
	              )
         -cardinality-number-unit>
          (head (NUMBER-UNIT (lf ?unit) (AGR 3s) (val ?c) (var ?v)))
         )

	;;  we need numbers as cardinality to handle headless constructions such as "the three in the corner"
	((CARDINALITY (LF (% DESCRIPTION (VAR ?v) (status ont::indefinite) (CLASS ?unit) 
			     (CONSTRAINT ?new)))
		      (VAR ?v) (NOQUAN +) ; what does NOQUAN do?
		      (cardinality +)
                      (AGR ?a) (STATUS ?status) (mass (? mass count bare))
		      (restr ?new)
	              )
         -cardinality-number> .97
          (head (NUMBER (lf ?unit) (AGR ?a) (val ?c) (var ?v) (restr ?restr)))
	 (add-to-conjunct  (val (value ?c)) (old ?restr) (new ?new))
         )


	;; special construction: hundreds, dozens, ...  - doesn't allow *hundreds trucks
        
        #||((QUANP (NOSIMPLE +)   ;; not needed anymore, subsumed by -quan-cardinality>
		(ARG ?arg) (AGR 3p) 
               (VAR (% *PRO* (VAR ?v) (status ont::indefinite) (CLASS ?unit) (CONSTRAINT (& (quan PLURAL)))))
               (MASS (? mss count bare)) (STATUS ONT::INDEFINITE) (qof ?qof)
	       (nobarespec +)
	       )
         -quan-number-unit-plur>
         (head (NUMBER-UNIT (lf ?unit) (val ?c) (AGR 3p) (var ?v) (qof ?qof)))
         )||#
        
        ;;  e.g., many thousands (of dogs)
        ((QUANP (NOSIMPLE ?ns) (status  ont::indefinite-plural) (qof ?qof) (ARG ?arg)
	  (var ?v) (agr ?a) (mass ?m) (cardinality +) (restr ?restr))
	 -quan-cardinality>
	 (head (Cardinality (nosimple ?ns) (var ?v) (agr ?a) (mass ?m) (restr ?restr))))

	;; many thousands
	((CARDINALITY (NOSIMPLE +)
                      (VAR (% *PRO* (VAR ?v) (status ont::indefinite) (class NUMBER) 
			     (CONSTRAINT (& (UNIT ?unit) (QUAN ?c)))))
		      (AGR ?a) (STATUS ?status)
	              (mass (? mss count bare))
		      )
         -cardinality-quan-number-units-plur>
         (QUAN (CARDINALITY +) (LF ?c) (STATUS ?status) (VAR ?qv) (AGR ?a))
         (head (NUMBER-UNIT (lf ?unit) (var ?v) (qof ?qof)))
         )

	((CARDINALITY (NOSIMPLE +)
                      (VAR (% *PRO* (VAR ?v) (status ont::indefinite) (class NUMBER) 
			     (CONSTRAINT (& (UNIT ?unit) (QUAN ONT::SOME)))))
		      (AGR ?a) (STATUS ?status)
	              (mass (? mss count bare))
		      )
         -cardinality-quan-number-units-plur-bare>
	 (head (NUMBER-UNIT (lf ?unit) (var ?v) (agr 3p) (qof ?qof)))
         )
    
	))


(parser::augment-grammar
 '((headfeatures
    (ADJ arg lex orig-lex headcat transform argument sort) ;; post-subcat) ; no sem    
    )

   ; get FUNCTN into SCALE
   ((ADJ (LF (:* ?lftype ?w)) ;(LF (:* ?pred ?lftype))
     (VAR ?v) (comparative +)
     (gap ?gap)
     ;(ALLOW-POST-N1-SUBCAT ?xx)
     (SUBCAT-MAP ?subcat-map)
     (subcat  ?subcat)
     (SUBCAT2-MAP ?subcat2-map)
     (subcat2  ?subcat2)
     ;(comp-ptype ?pt)
     ;(ground-oblig ?go)
     ;(ground-subcat ?ground-subcat)
     (ATYPE CENTRAL)
     (SORT PRED)
     ;(sem ($ F::ABSTR-OBJ (f::scale ?!functn)))
     (sem ?newsem)
     (allow-deleted-comp +) (allow-post-n1-subcat +)
     ;(sem ?sem)
     (transform ?transform) (argument-map ont::figure) (argument ?argument)  (arg ?arg)
     )
     -adj-compar-functn-to-scale> 1.0
    ;(ADV (compar-op +) (lf (:* ?pred ?xx)) (ground-oblig ?go) (SUBCAT ?ground-subcat))
     (head (ADJ (LF (:* ?lftype ?w)) (LF ?oldlf)
		(var ?v)  (gap ?gap)
		;(SUBCAT2 -) (post-subcat -)
		(subcat2 ?subcat2) (subcat2-map ?subcat2-map)
		(VAR ?v) (comparative (? xxx + superl))
	       (SUBCAT ?subcat) 
	       (subcat-map ?subcat-map)
	       (ATYPE central)
	       (argument ?argument) (arg ?arg)
	       (SORT PRED)
	       (FUNCTN ?!functn)
	       ;(sem ($ F::ABSTR-OBJ (f::scale ?scale)))
	       (sem ?sem)
	       (transform ?transform)
	       ))
     (change-feature-values (old ?sem) (new ?newsem) (newvalues ((f::scale ?!functn))))
    )
))
   

;;;

;; headfeatures
;; mass -- can be mass, count or bare
;; qual -- + if qualifier is present, e.g. the RED train
;; changeagr -- looks like this isn't used anywhere
;; roles -- thematic role mappings from LF. No longer used?
;; quantity -- used for commodities

(parser::augment-grammar
 '((headfeatures
    ;; (N1 VAR arg AGR MASS CASE SEM Changeagr lex quantity subcat transform)
    (N1 var arg lex orig-lex headcat transform agr mass case sem quantity argument indef-only subcat-map refl abbrev gerund nomsubjpreps nomobjpreps dobj-map dobj subj-map generated rate-activity-nom agent-nom)
    ;;(N1 var arg lex headcat transform agr mass case sem quantity argument argument-map indef-only subcat-map refl abbrev gerund nomsubjpreps nomobjpreps dobj-map dobj subj-map generated)
    (N var arg lex orig-lex headcat transform agr mass case sem quantity argument indef-only subcat-map refl abbrev gerund nomsubjpreps nomobjpreps dobj-map dobj subj-map generated rate-activity-nom agent-nom)  ; this is a copy of N1 so -N-prefix> would pass on the features
    (NAME var arg lex orig-lex headcat transform agr mass case sem quantity argument indef-only subcat-map refl abbrev gerund nomsubjpreps nomobjpreps dobj-map dobj subj-map generated name)  ; this is a copy of N1 so -NAME-prefix> would pass on the features (added name)
    (UNITMOD var arg lex orig-lex headcat transform agr mass case sem quantity subcat argument indef-only)
    (QUAL var arg lex orig-lex headcat transform ARGUMENT COMPLEX)
    ;; MD 18/04/2008 added SEM as a headfeature to handle "in full" where in subcategorizes for adjp
    ;; Other option might be to subcategorize for adj - need to consider in the future
    (ADJP arg lex orig-lex headcat transform argument sem complex) ;; post-subcat)     
    (ADJ1 arg lex orig-lex headcat transform argument sem sort lf allow-deleted-comp allow-post-n1-subcat gap)
    (ADJ arg lex orig-lex headcat transform argument sem sort) ;; post-subcat)     
    )
   
   ;; common nouns without modifiers, e.g. boxcar, juice, trains
   ((N1 (SORT (? sort PRED UNIT-MEASURE)) (CLASS ?lf) (name-or-bare +) (one ?one) ; so we can exclude (one +) in -n-sing-n1->
     (POSTADVBL -) (QUAL -) (RESTR ?r) (subcat ?subcat) (simple +))
    -N1_1>
    (head (n (SORT (? sort PRED UNIT-MEASURE)) (LF ?lf) (RESTR ?r) (punc -) (one ?one)
	     (subj-map -)   ;; we have a separate rule for nominalizations
	     (subcat ?subcat) (SEM ($ ?type (f::scale ?sc)))
	   )
     ))


   ;; special rule for NOUN prefixes that act as adjectives, with hyphen
   ((N (RESTR  ?con)
       (LF ?lf) ;(CLASS ?lf)
       (SORT ?sort) (QUAL +)
	(relc -) ;(relc ?relc)
	(sem ?nsem) (subcat ?subcat) (SET-RESTR ?sr)
	(comparative ?com)
	(complex -) ;(complex ?cmpl)
	(post-subcat -) (gap ?gap) (allow-deleted-comp ?adc)
	    (dobj ?dobj)
	    (subj ?subj)
      	    (comp3 ?comp3)
	    (subj-map ?subjmap)
	    (dobj-map ?dobjmap)  
	    (comp3-map ?comp-map)
     )
    -N-prefix-hyphen> 1.01
    (ADJ (prefix +)
     (LF ?qual) (ARG ?v) (VAR ?adjv) (WH -)
     (argument (% NP (sem ?argsem))) 
     (COMPLEX -) (comparative ?com) (Set-modifier -)
     (post-subcat -)
     )
    (word (lex w::punc-minus))
    (head (N (RESTR ?r) (VAR ?v) (SEM ?nsem) (CLASS ?c) (SET-RESTR ?sr) (gap ?gap)
	      (SORT ?sort) (relc -) ;;(relc ?relc) "-" to avoid the ambiguity "the [[red book] which I saw]" "the [red [book which I saw]]"  
	      (subcat ?subcat) (complex -) (lf ?lf)
	      (post-subcat -) (allow-deleted-comp ?adc)
	      (PRO -) (postadvbl -) ;; to avoid the ambiguity "the [[red truck] at Avon]" "the [red [truck at Avon]]"
	    (dobj ?dobj)   ; for nominalizations
	    (subj ?subj)
      	    (comp3 ?comp3)
	    (subj-map ?subjmap)
	    (dobj-map ?dobjmap)  
	    (comp3-map ?comp-map)
	      
	      )
     )
    ;;(unify (value ?nsem) (pattern ?argsem))  ;; we're doing it this way so we pass up all the sem features  06/18  removed as I c an't 
    (add-to-conjunct (val (:MOD (% *PRO* (status ont::f) (class ?qual)
				   (var *) (constraint (& (FIGURE ?v)))))) (old ?r) (new ?con)))

    ;; special rule for NOUN prefixes that act as adjectives, without hyphen
  
   ((N (RESTR  ?con)
       (LF ?lf) ;(CLASS ?lf)
       (SORT ?sort) (QUAL +)
	(relc -) ;(relc ?relc)
	(sem ?nsem) (subcat ?subcat) (SET-RESTR ?sr)
	(comparative ?com)
	(complex -) ;(complex ?cmpl)
	(post-subcat -) (gap ?gap) (allow-deleted-comp ?adc)
	    (dobj ?dobj)
	    (subj ?subj)
      	    (comp3 ?comp3)
	    (subj-map ?subjmap)
	    (dobj-map ?dobjmap)  
	    (comp3-map ?comp-map)
     )
    -N-prefix> 1.01
    (ADJ (prefix +)
     (LF ?qual) (ARG ?v) (VAR ?adjv) (WH -)
     (argument (% NP (sem ?argsem))) 
     (COMPLEX -) (comparative ?com) (Set-modifier -)
     (post-subcat -)
     )
    (head (N (RESTR ?r) (VAR ?v) (SEM ?nsem) (CLASS ?c) (SET-RESTR ?sr) (gap ?gap)
	      (SORT ?sort) (relc -) ;;(relc ?relc) "-" to avoid the ambiguity "the [[red book] which I saw]" "the [red [book which I saw]]"  
	      (subcat ?subcat) (complex -) (lf ?lf)
	      (post-subcat -) (allow-deleted-comp ?adc)
	      (PRO -) (postadvbl -) ;; to avoid the ambiguity "the [[red truck] at Avon]" "the [red [truck at Avon]]"
	    (dobj ?dobj)   ; for nominalizations
	    (subj ?subj)
      	    (comp3 ?comp3)
	    (subj-map ?subjmap)
	    (dobj-map ?dobjmap)  
	    (comp3-map ?comp-map)
	      
	      )
     )
    ;;(unify (value ?nsem) (pattern ?argsem))  ;; we're doing it this way so we pass up all the sem features
    (add-to-conjunct (val (:MOD (% *PRO* (status ont::F) (class ?qual)
				   (var *) (constraint (& (FIGURE ?v)))))) (old ?r) (new ?con)))


     ;;  example??  
   ((NAME (RESTR  ?con)
       (LF ?lf) ;(CLASS ?lf)
       (SORT ?sort) (QUAL +)
	(relc -) ;(relc ?relc)
	(sem ?nsem) (subcat ?subcat) (SET-RESTR ?sr)
	(comparative ?com)
	(complex -) ;(complex ?cmpl)
	(post-subcat -) (gap ?gap)
	    (dobj ?dobj)
	    (subj ?subj)
      	    (comp3 ?comp3)
	    (subj-map ?subjmap)
	    (dobj-map ?dobjmap)  
	    (comp3-map ?comp-map)
     )
    -Name-prefix-hyphen> 1.01
    (ADJ (prefix +)
     (LF ?qual) (ARG ?v) (VAR ?adjv) (WH -)
     (argument (% NP (sem ?argsem))) 
     (COMPLEX -) (comparative ?com) (Set-modifier -)
     (post-subcat -)
     )
    (word (lex w::punc-minus))
    (head (NAME (RESTR ?r) (VAR ?v) (SEM ?nsem) (CLASS ?c) (SET-RESTR ?sr) (gap ?gap)
	      (SORT ?sort) (relc -) ;;(relc ?relc) "-" to avoid the ambiguity "the [[red book] which I saw]" "the [red [book which I saw]]"  
	      (subcat ?subcat) (complex -) (lf ?lf)
	      (post-subcat -)
	      (PRO -) (postadvbl -) ;; to avoid the ambiguity "the [[red truck] at Avon]" "the [red [truck at Avon]]"
	    (dobj ?dobj)   ; for nominalizations
	    (subj ?subj)
      	    (comp3 ?comp3)
	    (subj-map ?subjmap)
	    (dobj-map ?dobjmap)  
	    (comp3-map ?comp-map)
	      
	      )
     )
    ;;(unify (value ?nsem) (pattern ?argsem))  ;; we're doing it this way so we pass up all the sem features
    (add-to-conjunct (val (:MOD (% *PRO* (status ont::f) (class ?qual)
				   (var *) (constraint (& (FIGURE ?v)))))) (old ?r) (new ?con)))

   ((NAME (RESTR  ?con)
       (LF ?lf) ;(CLASS ?lf)
       (SORT ?sort) (QUAL +)
	(relc -) ;(relc ?relc)
	(sem ?nsem) (subcat ?subcat) (SET-RESTR ?sr)
	(comparative ?com)
	(complex -) ;(complex ?cmpl)
	(post-subcat -) (gap ?gap)
	    (dobj ?dobj)
	    (subj ?subj)
      	    (comp3 ?comp3)
	    (subj-map ?subjmap)
	    (dobj-map ?dobjmap)  
	    (comp3-map ?comp-map)
     )
    -Name-prefix> 1.01
    (ADJ (prefix +)
     (LF ?qual) (ARG ?v) (VAR ?adjv) (WH -)
     (argument (% NP (sem ?argsem))) 
     (COMPLEX -) (comparative ?com) (Set-modifier -)
     (post-subcat -)
     )
    (head (NAME (RESTR ?r) (VAR ?v) (SEM ?nsem) (CLASS ?c) (SET-RESTR ?sr) (gap ?gap)
	      (SORT ?sort) (relc -) ;;(relc ?relc) "-" to avoid the ambiguity "the [[red book] which I saw]" "the [red [book which I saw]]"  
	      (subcat ?subcat) (complex -) (lf ?lf)
	      (post-subcat -)
	      (PRO -) (postadvbl -) ;; to avoid the ambiguity "the [[red truck] at Avon]" "the [red [truck at Avon]]"
	    (dobj ?dobj)   ; for nominalizations
	    (subj ?subj)
      	    (comp3 ?comp3)
	    (subj-map ?subjmap)
	    (dobj-map ?dobjmap)  
	    (comp3-map ?comp-map)
	      
	      )
     )
    ;;(unify (value ?nsem) (pattern ?argsem))  ;; we're doing it this way so we pass up all the sem features
    (add-to-conjunct (val (:MOD (% *PRO* (status ont::f) (class ?qual)
				   (var *) (constraint (& (FIGURE ?v))))))
		     (old ?r) (new ?con))
    )

   ;; special construction, a noun with a name
   ((N1 (CLASS ?lf) (sort PRED)
     (POSTADVBL -) (QUAL -) (RESTR ?newr) (subcat ?subcat)) 
    -N1_string>
    (head (n1 (SORT PRED) (CLASS ?lf) (RESTR ?r)
	   (subcat ?subcat) (complex -)
	   )
     )
    (name (lex ?val) (STRING +)) ;; maybe could relax this to be any name, not necessarily a string???  JFA 2/08    
    (add-to-conjunct (val (:name-of (?val))) (old ?r) (new ?newr)))
   
   ;; relational nouns without filled PP-of    
   ;; e.g., the brother, the hand, the side
     ;; NOTE: it is crucial to have (SUBCAT -) there, or the N1 will never undergo n-n modification!
   
   ((N1 (sort pred) (class ?lf) (var ?v)
     ;; (restr (& (?smap (% *PRO* (var *) (sem ?argsem) (constraint (& (ROLE-VALUE-OF ?v) (fills-ROLE ?lf)))))))  ;; to be done in IM now
      (RESTR ?con) ;(RESTR (& (scale ?sc)))
     (qual -) (postadvbl -) (subcat (% -))
     )
    -N1-reln1> .995
    (head (n  (sort reln) (lf ?lf) (allow-deleted-comp +) (RESTR ?r)
	   (sem ?ssem)  (SEM ($ ?type (f::scale ?sc)))
	   (subcat (% ?argcat (sem ?argsem)))
	   (subcat-map ?smap)
	   (subcat-map (? !smap ont::GROUND)) 
	   ))
    (add-to-conjunct (val (:scale ?sc)) (old ?r) (new ?con))

    )

    ;; relational nouns with filled PP-of complements  e.g., distance of the route
    ;; but this is not for e.g., distance of 5 miles -- filled pp-of unit measures should go through n1-reln4
    ;; NOTE: it is crucial to have (SUBCAT -) there, or the N1 will never undergo n-n modification!
    ((N1 (sort pred) (var ?v) (class ?lf) (qual -) (COMPLEX +)
	 (RESTR ?con) ;(restr (& (?smap ?v1) (scale ?sc)))
	 (gap ?gap)
      (subcat ?!subcat)
      )
     -N1-reln-scale> 
     (head (n (sort reln) (lf ?lf) (RESTR ?r)
	      (subcat ?!subcat)
	      (subcat (% ?scat (var ?v1) (sem ?ssem) (lf ?lf2) (gap ?gap) )) ;;(sort (? srt pred individual set comparative reln))))
	      (SEM ($ F::ABSTR-OBJ (f::scale ?sc)))
	      (subcat-map ?smap)))
     ?!subcat
     (add-to-conjunct (val (?smap ?v1)) (old ?r) (new ?con1))
     (add-to-conjunct (val (scale ?sc)) (old ?con1) (new ?con))
     )

   ;; relational non-scale nouns with filled PP-of complements  e.g. top of the box
   ((N1 (sort pred) (var ?v) (class ?lf) (qual -) (COMPLEX +)
     (RESTR ?con1) ;(restr (& (?smap ?v1) (scale ?sc)))
     (gap ?gap)
     (subcat ?!subcat)
     )
    -N1-reln-parts> 
    (head (n (sort reln) (lf ?lf) (RESTR ?r)
	     (subcat ?!subcat)
	     (subcat (% ?scat (var ?v1) (sem ?ssem) (lf ?lf2) (gap ?gap) )) ;;(sort (? srt pred individual set comparative reln))))
	     (SEM ($ ?sem (f::scale -))) ;(SEM ($ F::PHYS-OBJ))
	     (subcat-map ?smap)))
    ?!subcat
    (add-to-conjunct (val (?smap ?v1)) (old ?r) (new ?con1))
    )

   ;; there are a few relational nouns with two complements  e.g., ratio of the length to the height
   ;; the intersection of acorn with booth
    ;; but this is not for e.g., distance of 5 miles -- filled pp-of unit measures should go through n1-reln4
    ;; NOTE: it is crucial to have (SUBCAT -) there, or the N1 will never undergo n-n modification!
    ((N1 (sort pred) (var ?v) (class ?lf) (qual -) (COMPLEX +)
      (restr (& (?smap ?v1) (?smap2 ?v2) (scale ?sc)))
      (subcat ?!subcat)
      )
     -N1-reln-two-subcat>
     (head (n (sort reln) (lf ?lf)
	      (subcat ?!subcat)
	      (subcat (% ?scat (var ?v1) (sem ?ssem) (lf ?lf2) (sort (? srt pred individual set comparative reln))))
	      (SEM ($ ?type (f::scale ?sc)))
	      (subcat2 ?!subcat2)
	      (subcat2 (% ?scat2 (var ?v2) (sem ?ssem2) (lf ?lf3) (sort (? srt2 pred individual set comparative reln))))
	      (subcat-map ?smap)
	      (subcat2-map ?smap2)))
     ?!subcat
     ?!subcat2
     )

   ;;  alternate construction, e.g., the GTP GTD ratio, the acorn booth intersection
   ((N1 (sort pred) (var ?v) (class ?lf) (qual -) (COMPLEX +)
      (restr (& (?smap ?v1) (?smap2 ?v2) (scale ?sc)))
      (subcat ?!subcat)
      )
     -N1-reln-two-subcat-alt> 
    (np (var ?v1) (sem ?ssem) (lf ?lf2))
    (np (var ?v2) (sem ?ssem2) (lf ?lf3))
    (head (n (sort reln) (lf ?lf)
	     (subcat ?!subcat)
	     (subcat (% ?scat (var ?v1) (sem ?ssem) (lf ?lf2) (sort (? srt pred individual set comparative reln))))
	     (SEM ($ ?type (f::scale ?sc)))
	     (subcat2 ?!subcat2)
	     (subcat2 (% ?scat2 (var ?v2) (sem ?ssem2) (lf ?lf3) (sort (? srt2 pred individual set comparative reln))))
	     (subcat-map ?smap)
	     (subcat2-map ?smap2)))
    )
    
  ((N1 (sort pred) (var ?v) (class ?lf) (qual -) (COMPLEX +)
      (restr (& (?smap ?v1) (?smap2 ?v2) (scale ?sc)))
      (subcat (% -))
      )
     -N1-reln-two-subcat-colon-dash> 1
    (np (var ?v1) (sem ?ssem) (lf ?lf2))
    (word (punc (? x w::punc-minus w::punc-colon w::punc-slash))) 
    (np (var ?v2) (sem ?ssem2) (lf ?lf3))
    (head (n (sort reln) (lf ?lf)
	     (subcat ?!subcat)
	     (subcat (% ?scat (var ?v1) (sem ?ssem) (lf ?lf2) (sort (? srt pred individual set comparative reln))))
	     (SEM ($ ?type (f::scale ?sc)))
	     (subcat2 ?!subcat2)
	     (subcat2 (% ?scat2 (var ?v2) (sem ?ssem2) (lf ?lf3) (sort (? srt2 pred individual set comparative reln))))
	     (subcat-map ?smap)
	     (subcat2-map ?smap2)))
   )
    ;; simple qualifier modifiers
    ;; TEST: orange dog, the orange dogs
    ((N1 (RESTR  ?con) (CLASS ?c) (SORT ?sort) (QUAL +) (relc ?relc) (sem ?nsem) (subcat ?subcat) (SET-RESTR ?sr)
      (comparative ?com) (complex ?cmpl) (post-subcat -) (gap ?gap)
      )
     -N1-qual1>
     (ADJP (atype (? at attributive-only central ))
      (LF ?qual) (ARG ?v) (VAR ?adjv) (WH -)
      (argument (% NP (sem ?argsem))) 
      (COMPLEX -) (comparative ?com) (Set-modifier -)
      (post-subcat -)
      )
     (head (N1 (RESTR ?r) (VAR ?v) (SEM ?nsem) (CLASS ?c) (SET-RESTR ?sr) (gap ?gap)
	    (SORT ?sort) (relc -) ;;(relc ?relc) "-" to avoid the ambiguity "the [[red book] which I saw]" "the [red [book which I saw]]"  
	    (subcat ?subcat) (complex ?cmpl)
	    (post-subcat -)
	    (PRO -) (postadvbl -) ;; to avoid the ambiguity "the [[red truck] at Avon]" "the [red [truck at Avon]]"
	    )
      )
     (unify (value ?nsem) (pattern ?argsem))  ;; we're doing it this way so we pass up all the sem features
     (add-to-conjunct (val (:MODS ?adjv)) (old ?r) (new ?con)))

    ((N1 (RESTR  ?con) (CLASS ?c) (SORT ?sort) (QUAL +) (relc ?relc) (sem ?nsem) (subcat ?subcat) (SET-RESTR ?sr)
      (comparative ?com) (complex ?cmpl) (post-subcat -) (gap ?gap)
      )
     -N1-qual1-hyphen> 1
     (ADJP (atype (? at attributive-only central )) (LF ?qual) (ARG ?v) (VAR ?adjv) (WH -)
;      (argument (% NP ));;(sem ?nsem))) 
      (argument (% NP (sem ?argsem))) 
      (COMPLEX -) (comparative ?com) (Set-modifier -)
      (post-subcat -)
      )
     (word (lex w::punc-minus))
     (head (N1 (RESTR ?r) (VAR ?v) (SEM ?nsem) (CLASS ?c) (SET-RESTR ?sr) (gap ?gap)
	    (SORT ?sort) (relc -) 
	    (subcat ?subcat) (complex -)
	    (post-subcat -)
	    (PRO -) (postadvbl -) ;; to avoid the ambiguity "the [[red truck] at Avon]" "the [red [truck at Avon]]"
	    )
      )
     (unify (value ?nsem) (pattern ?argsem))  ;; we're doing it this way so we pass up all the sem features
     (add-to-conjunct (val (:MODS ?adjv)) (old ?r) (new ?con)))

    ;; allow modification of measure terms with physical modifiers as long as the scales match. The sems will be different; keep the head sem
    ;; short distance; heavy weight; hot temperature
      ((N1 (RESTR  ?con) (CLASS ?c) (SORT ?sort) (QUAL +) (relc ?relc) (sem ?nsem) (subcat ?subcat)(SET-RESTR ?sr)
      (comparative ?com) (complex ?cmpl) (post-subcat -)
      )
     -N1-measure-term-qual> .97
     (ADJP (atype (? at attributive-only central )) (LF ?qual) (ARG ?v) (VAR ?adjv) (WH -)
      (argument (% NP (sem ?nsem1))) (COMPLEX -) (comparative ?com) (Set-modifier -)
      (sem ($ F::ABSTR-OBJ (F::scale ?!sc)))
      (post-subcat -)
      )
     (head (N1 (RESTR ?r) (VAR ?v) (SEM ?nsem) (CLASS ?c) (SET-RESTR ?sr)
	       (sem ($ F::ABSTR-OBJ (F::scale ?!sc)))
	    (SORT ?sort) (relc -) ;; "-" to avoid the ambiguity "the [[red book] which I saw]" "the [red [book which I saw]]"  
	    (subcat ?subcat) (complex ?cmpl)
	    (post-subcat -)
	    (PRO -) (postadvbl -) ;; to avoid the ambiguity "the [[red truck] at Avon]" "the [red [truck at Avon]]"
	    )
      )
     (add-to-conjunct (val (:MODS ?adjv)) (old ?r) (new ?con)))

 ;; TEST: five trains
    ;; does this replace the cardinality1> rule?
    ((N1 (RESTR  ?newr) (CLASS ?c) (SORT PRED) (QUAL +) (relc ?relc) (sem ?nsem) (subcat ?subcat)
      (comparative ?com) (complex ?cmpl) (post-subcat -)
      )
     -N1-qual-set1>
     (ADJP (atype (? at attributive-only central )) (LF ?qual) (ARG ?v) (VAR ?adjv) (WH -)
      (argument (% NP (sem ?nsem))) (COMPLEX -) (comparative ?com) (Set-modifier +) (agr ?agr)
      (post-subcat -)
      )
     (head (N1 (RESTR ?r) (VAR ?v) (SEM ?nsem) (CLASS ?c) (agr ?agr)
	    (SORT PRED) (relc -) ;;(relc ?relc) "-" to avoid the ambiguity "the [[red book] which I saw]" "the [red [book which I saw]]"  
	    (subcat ?subcat) (complex ?cmpl)
	    (post-subcat -)
	    (PRO -) (postadvbl -) ;; to avoid the ambiguity "the [[red truck] at Avon]" "the [red [truck at Avon]]"
	    )
      )
     (add-to-conjunct (val (size ?adjv)) (old ?r) (new ?newr))
    )

    ;; nouns with modifiers that come after 
    ;; "Let me see the trucks available [to me]"
    ;; Myrosia 2005/06/13 restricted relc and postadvbl to avoid "the trucks in rochester available"
    ((N1 (RESTR ?con) (CLASS ?c) (SORT ?sort) (QUAL -) (COMPLEX +) (set-restr ?sr)
      (relc -) (subcat ?subcat) (gap ?gap)
      )
     -N1-post-adj>
     (head (N1 (RESTR ?r) (VAR ?v) (SEM ?s) (CLASS ?c) (SORT ?sort) (set-restr ?sr)
	    (relc -) (postadvbl -) (subcat ?subcat) (post-subcat -)
	    ))
     (ADJP (LF ?qual) (ATYPE POSTPOSITIVE) ;;(COMPLEX +) removed because it blocks adj w/ no subcat
	   (ARG ?v) (gap ?gap)
	   (VAR ?m) 
	   (argument (% NP (sem ?s)))
	   (post-subcat -)
	   )
     (add-to-conjunct (val (:MODS ?m)) (old ?r) (new ?con)))

 #||   ;; Split phrases like "a larger apple that that
    ((N1 (RESTR ?con) (CLASS ?c) (SORT ?sort) (QUAL -) (COMPLEX +)
      (relc ?relc) (subcat ?subcat)
      )
     -N1-post-subcat>
     (ADJP (atype (? at attributive-only central)) 
      (LF ?qual) 
      (ARG ?v) (VAR ?adjv)
      (argument (% NP (sem ?nsem))) 
      (COMPLEX -) (comparative ?com)
      (post-subcat ?!post-subcat)
      (psarg ?psvar)
      (post-subcat (% ?xxx (var ?psvar) (gap -)))
      )
      (head (N1 (RESTR ?r) (VAR ?v) (SEM ?nsem) (CLASS ?c)
		(SORT ?sort) (relc ?relc) (subcat ?subcat) 
		(post-subcat -)
		)
       )
     ?!post-subcat
     (UNIFY (arg1 (% ?xxx (var ?psvar))) (arg2 ?!post-subcat))
     (add-to-conjunct (val (:MODS ?adjv)) (old ?r) (new ?con)))
    ||#

    #|
    ;; A few adjectives can have their subcat after the head noun, e.e., "the same ideas as me", "a faster car than that"
     ((N1 (RESTR ?newr) (CLASS ?c) (SORT ?sort) (QUAL -) (COMPLEX +)(set-restr ?sr)
       (relc ?relc) (subcat ?subcat)
       ;;(post-subcat +)
       (no-postmodifiers +) ;; add an extra feature to say "no further postmodifiers". If we say "The bulb in 1 is in the same path as the battery in 1", we don't want "in 1" to attach to "the path"
      )
     -N1-post-subcat>
     (ADJ1 (atype (? at attributive-only central)) 
      (LF ?qual) 
      (ARG ?v) (VAR ?adjv)
      (argument (% NP (sem ?nsem))) 
      (COMPLEX -) (comparative ?com)
      (constraint ?adjcon)
      (subcat-map ?submap)
      (argument-map ?argmap)
      ;;(post-subcat ?!post-subcat)
      ;;(subcat ?!psct)  ;; just to make sure its not empty
      (subcat (% PP (var ?psvar) (ptype ?ptype) (gap -) (sem ?pssem)
		 ))
      )
     (head (N1 (RESTR ?r) (VAR ?v) (SEM ?nsem) (CLASS ?c)(set-restr ?sr)
	    (SORT ?sort) (relc ?relc) (subcat ?subcat) 
	    (post-subcat -)
	    )
      )
      (PP (ptype ?ptype) (var ?psvar) (gap -) (sem ?pssem))
      ;; ?!psct
     ;;(UNIFY (arg1 (% ?xxx (var ?psvar))) (arg2 ?!post-subcat))
      (append-conjuncts  (conj1 ?adjcon) (conj2 (& (?submap ?psvar) (?argmap ?v))) (new ?newadjcon))
      (add-to-conjunct (val (:MOD 
			     (% *pro* (var ?adjv) (status ont::f) (class ?qual)
				    (constraint ?newadjcon))))
				   
       (old ?r) (new ?newr))
      )
    |#

    ;; A few adjectives can have their subcat after the head noun, e.e., "the same ideas as me", "a faster car than that"
    ; London is a closer city to Barcelona than Avon.
     ((N1 (RESTR ?newr) (CLASS ?c) (SORT ?sort) (QUAL -) (COMPLEX +)(set-restr ?sr)
       (relc ?relc) (subcat ?nsubcat)
       ;;(post-subcat +)
       (no-postmodifiers +) ;; add an extra feature to say "no further postmodifiers". If we say "The bulb in 1 is in the same path as the battery in 1", we don't want "in 1" to attach to "the path"
      )
     -N1-post-twosubcats>
     (ADJ1 (atype (? at attributive-only central)) 
      (LF ?qual) (lex ?lex1)
      (ARG ?v) (VAR ?adjv)
      (argument (% NP (sem ?nsem))) 
      (COMPLEX -) (comparative ?com)
      (constraint ?adjcon)
      (subcat-map ?!submap) (subcat ?!subcat) (SUBCAT (% ?xx (var ?argv))) 
      (subcat2-map (? !submap2 ONT::NOROLE -)) (subcat2 ?!subcat2) (SUBCAT2 (% ?xx2 (var ?argv2)))
      (argument-map ?argmap)
      (sem ?sem) (sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))
      ;;(post-subcat ?!post-subcat)
      )
     (head (N1 (RESTR ?r) (VAR ?v) (SEM ?nsem) (CLASS ?c)(set-restr ?sr)
	    (SORT ?sort) (relc ?relc) (subcat ?nsubcat) 
	    (post-subcat -)
	    )
      )
     ?!subcat
     ?!subcat2
     (bound (arg1 ?argv))
     (bound (arg1 ?argv2))
     ;;(UNIFY (arg1 (% ?xxx (var ?psvar))) (arg2 ?!post-subcat))
     (append-conjuncts  (conj1 ?adjcon) (conj2 (& (?!submap ?argv) (?!submap2 ?argv2) (?argmap ?v)
						  (scale ?scale) (intensity ?ints) (orientation ?orient)))
			(new ?newadjcon))
      (add-to-conjunct (val (:MOD 
			     (% *pro* (var ?adjv) (status ont::f) (class ?qual) (lex ?lex1)
				    (constraint ?newadjcon))))
				   
       (old ?r) (new ?newr))
      )


    ;; A few adjectives can have their subcat after the head noun, e.e., "the same ideas as me", "a faster car than that"
    ; London is a closer city than Avon to Barcelona
     ((N1 (RESTR ?newr) (CLASS ?c) (SORT ?sort) (QUAL -) (COMPLEX +)(set-restr ?sr)
       (relc ?relc) (subcat ?nsubcat)
       ;;(post-subcat +)
       (no-postmodifiers +) ;; add an extra feature to say "no further postmodifiers". If we say "The bulb in 1 is in the same path as the battery in 1", we don't want "in 1" to attach to "the path"
      )
     -N1-post-twosubcats-rev>
     (ADJ1 (atype (? at attributive-only central)) 
      (LF ?qual) (lex ?lex1)
      (ARG ?v) (VAR ?adjv)
      (argument (% NP (sem ?nsem))) 
      (COMPLEX -) (comparative ?com)
      (constraint ?adjcon)
      (subcat-map ?!submap) (subcat ?!subcat) (SUBCAT (% ?xx (var ?argv))) 
      (subcat2-map (? !submap2 ONT::NOROLE -)) (subcat2 ?!subcat2) (SUBCAT2 (% ?xx2 (var ?argv2)))
      (argument-map ?argmap)
      (sem ?sem) (sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))
      ;;(post-subcat ?!post-subcat)
      )
     (head (N1 (RESTR ?r) (VAR ?v) (SEM ?nsem) (CLASS ?c)(set-restr ?sr)
	    (SORT ?sort) (relc ?relc) (subcat ?nsubcat) 
	    (post-subcat -)
	    )
      )
     ?!subcat2
     ?!subcat
     ;;(UNIFY (arg1 (% ?xxx (var ?psvar))) (arg2 ?!post-subcat))
     (bound (arg1 ?argv))
     (bound (arg1 ?argv2))
     (append-conjuncts  (conj1 ?adjcon) (conj2 (& (?!submap ?argv) (?!submap2 ?argv2) (?argmap ?v)
						  (scale ?scale) (intensity ?ints) (orientation ?orient)))
			(new ?newadjcon))
      (add-to-conjunct (val (:MOD 
			     (% *pro* (var ?adjv) (status ont::f) (class ?qual) (lex ?lex1)
				    (constraint ?newadjcon))))
				   
       (old ?r) (new ?newr))
      )
     
    ;; A few adjectives can have their subcat after the head noun
    ;; a larger truck than that
     ((N1 (RESTR ?newr) (CLASS ?c) (SORT ?sort) (QUAL -) (COMPLEX +)(set-restr ?sr)
       (relc ?relc) (subcat ?nsubcat)
       ;;(post-subcat +)
       (no-postmodifiers +) ;; add an extra feature to say "no further postmodifiers". If we say "The bulb in 1 is in the same path as the battery in 1", we don't want "in 1" to attach to "the path"
      )
     -N1-post-onesubcat>
     (ADJ1 (atype (? at attributive-only central)) (ALLOW-POST-N1-SUBCAT +)
      (LF ?qual) (lex ?lex1)
      (ARG ?v) (VAR ?adjv)
      (argument (% NP (sem ?nsem))) 
      (COMPLEX -) (comparative ?com)
      (constraint ?adjcon)
      (subcat-map ?!submap) (subcat ?!subcat) (SUBCAT (% ?xx (var ?argv) (ptype ?ptype) (sem ?psem))) 
      ;(subcat2-map (? !submap2 ONT::NOROLE -)) (subcat2 ?!subcat2) (SUBCAT2 (% ?xx2 (var ?argv2)))
      (SUBCAT2 (% - (W::VAR -)))
      (argument-map ?argmap)
      (sem ?sem) (sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))
      ;;(post-subcat ?!post-subcat)
      )
     (head (N1 (RESTR ?r) (VAR ?v) (SEM ?nsem) (CLASS ?c)(set-restr ?sr)
	    (SORT ?sort) (relc ?relc) (subcat ?nsubcat) 
	    (post-subcat -)
	    )
      )
     (PP (var ?argv) (ptype ?ptype) (sem ?psem) (gap -)) ;?!subcat
     (bound (arg1 ?argv)) ; exclude optional unfilled subcats (not sure why ?!subcat doesn't ensure it exists)
     ;?!subcat2
     ;;(UNIFY (arg1 (% ?xxx (var ?psvar))) (arg2 ?!post-subcat))
     (append-conjuncts  (conj1 ?adjcon) (conj2 (& (?!submap ?argv) ;(?!submap2 ?argv2)
						  (?argmap ?v)
						  (scale ?scale) (intensity ?ints) (orientation ?orient)))
			(new ?newadjcon))
      (add-to-conjunct (val (:MOD 
			     (% *pro* (var ?adjv) (status ont::f) (class ?qual) (lex ?lex1)
				    (constraint ?newadjcon))))
				   
       (old ?r) (new ?newr))
      )

     ; same as -N1-post-onesubcat> but takes adj without ALLOW-POST-N1-SUBCAT
    ;; A few adjectives can have their subcat after the head noun
    ;; a larger truck than that
     ((N1 (RESTR ?newr) (CLASS ?c) (SORT ?sort) (QUAL -) (COMPLEX +)(set-restr ?sr)
       (relc ?relc) (subcat ?nsubcat)
       ;;(post-subcat +)
       (no-postmodifiers +) ;; add an extra feature to say "no further postmodifiers". If we say "The bulb in 1 is in the same path as the battery in 1", we don't want "in 1" to attach to "the path"
      )
     -N1-post-onesubcat-b> 0.97
     (ADJ1 (atype (? at attributive-only central)) (ALLOW-POST-N1-SUBCAT -) ; "-" here so it's mutually exclusive with -N1-post-onesubcat>
      (LF ?qual) (lex ?lex1)
      (ARG ?v) (VAR ?adjv)
      (argument (% NP (sem ?nsem))) 
      (COMPLEX -) (comparative ?com)
      (constraint ?adjcon)
      (subcat-map ?!submap) (subcat ?!subcat) (SUBCAT (% ?xx (var ?argv) (ptype ?ptype) (sem ?psem))) 
      ;(subcat2-map (? !submap2 ONT::NOROLE -)) (subcat2 ?!subcat2) (SUBCAT2 (% ?xx2 (var ?argv2)))
      (SUBCAT2 (% - (W::VAR -)))
      (argument-map ?argmap)
      (sem ?sem) (sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))
      ;;(post-subcat ?!post-subcat)
      )
     (head (N1 (RESTR ?r) (VAR ?v) (SEM ?nsem) (CLASS ?c)(set-restr ?sr)
	    (SORT ?sort) (relc ?relc) (subcat ?nsubcat) 
	    (post-subcat -)
	    )
      )
     (PP (var ?argv) (ptype ?ptype) (sem ?psem) (gap -)) ;?!subcat
     (bound (arg1 ?argv)) ; exclude optional unfilled subcats (not sure why ?!subcat doesn't ensure it exists)
     ;?!subcat2
     ;;(UNIFY (arg1 (% ?xxx (var ?psvar))) (arg2 ?!post-subcat))
     (append-conjuncts  (conj1 ?adjcon) (conj2 (& (?!submap ?argv) ;(?!submap2 ?argv2)
						  (?argmap ?v)
						  (scale ?scale) (intensity ?ints) (orientation ?orient)))
			(new ?newadjcon))
      (add-to-conjunct (val (:MOD 
			     (% *pro* (var ?adjv) (status ont::f) (class ?qual) (lex ?lex1)
				    (constraint ?newadjcon))))
				   
       (old ?r) (new ?newr))
      )

     
    ;; 500 mb or greater
    ;; note that this rule doesn't handle -500 mb of ram or greater; -a 500 mb ram or greater
    ;; note also that there is a similar rule in adverbial-grammar.lisp for NP or adv-er
    ((NP (var ?v1) (spec ?spec)
      (LF (% description (status ont::indefinite) (var ?v1) (sort W::unit-measure)
			 (class ?class) (sem ?lfsem)
			 (argument ?ag)
			 (constraint  ?new)))
         (sort ?st)  (case ?case) (class ?class) (wh ?w)
	 (sem ?sem)
	 )
     -adj-or-comparative> 
     (head (NP (VAR ?v1) (lex ?nlex)
               (sort ?st)  (case ?case)
	       (SEM ?sem) (spec ?spec) (wh ?w)
	       (LF (% description (status ont::indefinite) (sort W::UNIT-measure)
		      (sem ?lfsem) (argument ?ag) (class ?class) (constraint  ?restr)))
	       )
           )
     (word (lex or))
     (adjp (comparative +) (var ?av) (post-subcat -)
      )
     (add-to-conjunct (val (& (MODS ?av))) (old ?restr) (new ?new))
     )
    
    ;; nouns with a subcat
    ;; e.g.  trucks of oranges
    ((N1 (RESTR (& (?!subcatmap ?v1))) (SORT ?sort) (COMPLEX ?complex)
      (CLASS ?LF) (GAP ?gap) (POSTADVBL -) (QUAL -) (ARGUMENT-MAP ?am)
      (subcat (% -))
      )
     -N1-subcat1>
     (Head (N (VAR ?v) (lf ?LF)
	    ;(SORT ?sort) (ARGUMENT-MAP ?am)
	    (SORT (? !sort reln)) (ARGUMENT-MAP ?am) ; reln should go through n1-reln3
	    (SUBCAT ?!subcat) 
	    (subcat-map ?!subcatmap)
	    (SUBCAT (% ?xx (var ?v1) (gap ?gap) (sem ?subcatsem) (postadvbl -)))
	    ))
     ?!subcat
     (compute-complex (arg0 ?v1) (arg1 ?complex))
     )

    ;; noun has two subcats: point/coordinate 5 4
    ;; for the rule to be general we should use vars in the restr, not lex...but the numbers
    ;; don't produce a var
    ((N1 (RESTR (& (?smap2 ?l1) (?smap ?l2))) (SORT ?sort) (COMPLEX +)
      (CLASS ?LF) (GAP ?gap) (POSTADVBL -) (QUAL -) (ARGUMENT-MAP ?am)
      (subcat (% -))
      )
     -N1-coordinates>
     (Head (N (VAR ?v) (lf ?LF)
	    (SORT ?sort) (ARGUMENT-MAP ?am)
	    (SUBCAT ?subcat) (subcat-map ?smap)
	    (SUBCAT (% number (var ?v1) (postadvbl -) (lex ?l1)))
	    (SUBCAT2 ?!subcat2) (subcat2-map ?smap2)
	    (SUBCAT2 (% number (var ?v2) (lex ?l2)(postadvbl -)))
	    	    
	    ))
     ?subcat
     ?!subcat2)
    
   ;;===========================================================================
    ;; ADJECTIVE PHRASES

    ;; adjectives that map to predicates, e.g., little
    ;; swier added attributive and predicative
    ;; 04/2008 swift adding orientation and intensity features
    ;; 10/2009 new representation using ontology types as scales, e.g. enormous -> (f v1 (:* ont::hi w::enormous) :scale ont::large)
    ((ADJP (ARG ?arg) (VAR ?v) (sem ?sem) (atype ?atype) (comparative ?cmp)
	   (gap ?gap)
      (LF (% PROP (CLASS ?lf)
	     (VAR ?v) (CONSTRAINT ?newc)
	     (transform ?transform) (sem ?sem) (premod -)
	     )))
     -adj-scalar-pred> 1
     (head (ADJ1 (LF ?lf) ;(SUBCAT -)
		 (VAR ?v) (sem ?sem) (SORT PRED) (arg ?arg) (ARGUMENT-MAP ?argmap) (pertainym -)
	    (transform ?transform) (constraint ?con) (comp-op ?dir)
	    (Atype ?atype) (comparative ?cmp) (lex ?lx) ;(lf (:* ?lftype ?lex))
	    (sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))
	    (gap ?gap)
	    (post-subcat -) (prefix -)
	    (functn -)
	    ))
     (append-conjuncts (conj1 ?con) (conj2 (& (orientation ?orient) (intensity ?ints)
					    (?argmap ?arg) (scale ?scale) 
					     ))
		       (new ?newc))
     )

    ;; prefix ADV modification of an ADJ
   ((ADJ (LF ?lf) (SUBCAT ?subcat) (VAR ?v) (sem ?sem) (SORT PRED) (ARGUMENT-MAP ?argmap)
     (transform ?transform) (constraint ?newc) (comp-op ?dir)  (argument ?argument)
     (atype ?atype) (comparative ?cmp) (lex ?lx) ; (lf (:* ?lftype ?lx))
     ;(sem ($ F::SITUATION))
     (arg ?arg)
     (prefix -)
     )
   -adj-prefix-hyphen> 1
    (adv (PREFIX +) (VAR ?advbv) 
     (argument (% ADJP (sem ?sem))) (LF ?qual)
     )
    (word (lex w::punc-minus))
    (head (ADJ (LF ?lf) (SUBCAT ?subcat) (VAR ?v) (sem ?sem) (SORT PRED) (ARGUMENT-MAP ?argmap)
	       (transform ?transform) (constraint ?con) (comp-op ?dir) (arg ?arg)
	       (atype ?atype) (comparative ?cmp) (lex ?lx) (argument ?argument)
	       ))
    (add-to-conjunct  (val (:MOD (% *PRO* (status ont::f) (class ?qual)
				    (var ?advbv) (constraint (& (FIGURE ?v))))))
     (old ?con) 
     (new ?newc))
    )

   ;; advberb prefix     
    ((ADJ (LF ?lf) (SUBCAT ?subcat) (VAR ?v) (sem ?sem) (SORT PRED) (ARGUMENT-MAP ?argmap)
     (transform ?transform) (constraint ?newc) (comp-op ?dir)  (argument ?argument)
     (atype ?atype) (comparative ?cmp) (lex ?lx) ; (lf (:* ?lftype ?lx))
     ;(sem ($ F::SITUATION))
     (arg ?arg)
     (prefix -)
     )
   -adj-prefix> 1
    (adv (PREFIX +) (VAR ?advbv) 
     (argument (% ADJP (sem ?sem))) (LF ?qual)
     )
    (head (ADJ (LF ?lf) (SUBCAT ?subcat) (VAR ?v) (sem ?sem) (SORT PRED) (ARGUMENT-MAP ?argmap)
	       (transform ?transform) (constraint ?con) (comp-op ?dir) (arg ?arg)
	       (atype ?atype) (comparative ?cmp) (lex ?lx) (argument ?argument)
	       ))
    (add-to-conjunct  (val (:MOD (% *PRO* (status ont::f) (class ?qual)
				    (var ?advbv) (constraint (& (FIGURE ?v))))))
     (old ?con) 
     (new ?newc))
    )
  
;;  Pertainyms
   ((ADJ (LF ONT::ASSOC-WITH) (SUBCAT ?subcat) (VAR ?v) (sem ?sem) (SORT PRED) (ARGUMENT-MAP ?argmap)
     (transform ?transform) (constraint ?newc) (comp-op ?dir)  (argument ?argument)
     (atype ?atype) (comparative ?cmp) (lex ?lx) ; (lf (:* ?lftype ?lx))
     ;(sem ($ F::SITUATION))
     (arg ?arg)
     (prefix -)
     )
   -adj-pertainym-construction> 1
    (head (ADJ (LF ?lf) (SUBCAT ?subcat) (VAR ?v) (sem ?sem) (SORT PRED) (ARGUMENT-MAP ?argmap) 
	       (pertainym ?!pert-type) (pert-domain-info ?pert-domain-info)
	       (transform ?transform) (constraint ?con) (comp-op ?dir) (arg ?arg)
	       (atype ?atype) (comparative ?cmp) (lex ?lx) (argument ?argument)
	       ))
    (compute-sem-features (lf ?!pert-type) (sem ?pert-sem) (domain-info ?pert-domain-info))
    (add-to-conjunct  (val (:GROUND (% *PRO* (status ont::kind) (class ?!pert-type)
				    (var *) (sem ?pert-sem) (constraint (& (drum ?pert-domain-info))))))
     (old ?con) 
     (new ?newc))
    )

   #| ; merged with -adj-scalar-pred>
   ;; non-scalar adjectives (e.g., sleeping)
   ((ADJP (ARG ?arg) (VAR ?v) (sem ?sem) (atype ?atype) (comparative ?cmp)
     (gap ?gap)
     (LF (% PROP (CLASS ?lf)
	    (VAR ?v) (CONSTRAINT ?newc)
	    
	    (transform ?transform) (sem ?sem) (premod -)
	    )))
     -adj-nonscalar-pred> 1
     (head (ADJ1 (LF ?lf) (SUBCAT -) (VAR ?v) (sem ?sem) (SORT PRED) (arg ?arg) (ARGUMENT-MAP ?argmap) (pertainym -)
	    (transform ?transform) (constraint ?con) (comp-op ?dir)
	    (atype ?atype) (comparative ?cmp) (lex ?lx) ; (lf (:* ?lftype ?lx))
	    (sem ($ F::ABSTR-OBJ
		    (f::scale -)))
	    (FUNCTN -)
	    ;;(sem ($ F::SITUATION))
	    (gap ?gap)
	    (post-subcat -) (prefix -)
	    ))
     (append-conjuncts (conj1 ?con) (conj2 (& (?argmap ?arg)))
      (new ?newc))
     )
   |#

   ;; once morphology is done, move up the attach subcats
   ((ADJ1 (LF ?lf) (SUBCAT ?subcat) (VAR ?v) (sem ?sem) (SORT PRED) (ARGUMENT-MAP ?argmap) (pertainym -)
	    (transform ?transform) (constraint ?con) (comp-op ?dir)  (SUBCAT-MAP ?subm)
	    (atype ?atype) (comparative ?cmp) (lex ?lx)
             (FUNCTN -)
	    (post-subcat ?psub) (prefix -)
	    (subcat2 ?subcat2)
	    (subcat2-map ?subcat2-map)
	    )
       
     -adj-adj1> 1
     (head (ADJ (LF ?lf) (SUBCAT ?subcat) (VAR ?v) (sem ?sem) (SORT PRED) (ARGUMENT-MAP ?argmap) (pertainym -)
	    (transform ?transform) (constraint ?con) (comp-op ?dir) (SUBCAT-MAP ?subm)
	    (atype ?atype) (comparative ?cmp) (lex ?lx)
	    (sem ?sem)
	    (FUNCTN -)
	    (post-subcat ?psub) (prefix -)
	    (subcat2 ?subcat2)
	    (subcat2-map ?subcat2-map)
	    ))

    )

    ;;  a (ten foot) high fence, a three mile wide path, .. 
    ((ADJP (ARG ?arg) (VAR ?adjv) (sem ?sem) (atype ?atype) (comparative ?cmp)
      (LF (% PROP (CLASS ont::at-scale-val) (VAR ?adjv) (CONSTRAINT ?newc)
	     (transform ?transform) (sem ?sem)))
      )
     -adj-unit-modifier> 1.0
     (ADJP (sort unit-measure) (var ?adjv) 
      (LF (% PROP (constraint (& (GROUND ?adjval)))))
      (sem ($ F::ABSTR-OBJ (F::scale (? sc ont::scale ont::domain))))) ;ont::linear-d)))))
     (head (ADJ (LF ?lf)  (VAR ?v) (SUBCAT -) (sem ($ F::ABSTR-OBJ (F::scale (? sc1 ont::scale ont::domain)))) ;ont::linear-d))))
		(SORT PRED) ;;(ARGUMENT-MAP ?argmap)
		(transform ?transform) (constraint ?con)
	    (atype ?atype) (comparative ?cmp)
	    (post-subcat -)
	    (FUNCTN -)
	    ))
     (class-greatest-lower-bound (in1 ?sc1) (in2 ?sc) (out ?finalsc))
     (append-conjuncts (conj1 ?restr) (conj2 ?r) (new ?tempcon))
     (append-conjuncts (conj1 (& (FIGURE ?arg) (GROUND ?adjval) (scale ?finalsc)))
		       (conj2 ?tempcon) (new ?newc)))

;;  a (ten foot)-high fence, a three mile wide path, .. 
    ((ADJP (ARG ?arg) (VAR ?v) (sem ?sem) (atype ?atype) (comparative ?cmp)
      (LF (% PROP (CLASS ont::at-scale-val) (VAR ?v) (CONSTRAINT ?newc)
	     (transform ?transform) (sem ?sem)))
      )
     -adj-unit-modifier-HYPHEN> 1.1
     (ADJP (sort unit-measure) (var ?adjv) 
      (LF (% PROP (constraint (& (GROUND ?adjval)))))
      (sem ($ F::ABSTR-OBJ (F::scale (? sc ont::scale ont::domain))))) ;ont::linear-d)))))
     (word (lex w::punc-minus))
     (head (ADJ (LF ?lf)  (VAR ?v) (SUBCAT -) (sem ($ F::ABSTR-OBJ (F::scale (? sc1 ont::scale ont::domain)))) ;ont::linear-d))))
		(SORT PRED) ;;(ARGUMENT-MAP ?argmap)
		(transform ?transform) (constraint ?con)
	    (atype ?atype) (comparative ?cmp)
	    (post-subcat -)
	    (FUNCTN -)
	    ))
      (class-greatest-lower-bound (in1 ?sc1) (in2 ?sc) (out ?finalsc))
     (append-conjuncts (conj1 ?restr) (conj2 ?r) (new ?tempcon))
     (append-conjuncts (conj1 (& (FIGURE ?arg) (GROUND ?adjval) (scale ?finalsc)))
		       (conj2 ?tempcon) (new ?newc)))

    #| ; subsumed by -adj-scalar-pred
   ;; adjectives with deleted complements  JFA 8/02
    ;; different (truck)    
    ((ADJP (ARG ?arg) (VAR ?v) (atype ?atype) (comparative ?comp)
      (LF (% PROP (CLASS ?lf) (VAR ?v) 
	     (sem ?sem) 
	     (CONSTRAINT ?newc)
	     (transform ?transform)
	     )))
     -adj-pred-object-deleted>
     (head (ADJ1 (LF ?lf) (VAR ?v) (sem ?sem) (atype ?atype)
		(CONSTRAINT ?con) (comparative ?comp) (prefix -) (pertainym -)
		(SUBCAT (% ?!subc (SEM ?subsem) ))
		(sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))
		(SORT PRED) (SUBCAT-MAP ?submap)
		(COMP-OP ?dir)
		(transform ?transform)
		(ARGUMENT-MAP ?argmap) (arg ?arg)
	    (allow-deleted-comp +)
	    (post-subcat -)
	    ))
     (append-conjuncts (conj1 ?con)
		       (conj2 (& (scale ?scale) (f::intensity ?ints) (f::orientation ?orient)
				 (?argmap ?arg) 
				 ;;(?submap (% *PRO* (var *) (sem ?subsem) (constraint (& (related-to ?v)))))
				 ))
				 
		       (new ?newc))
     )
    |#

     ;; more quickly
     ((ADVBL (ARG ?arg)
	 (VAR ?v) (atype ?atype) (comparative (? cc + w::superl)) (SORT PRED)
      (LF (% PROP (CLASS ?lf) (VAR ?v)
	     (sem ?sem) 
	     (CONSTRAINT ?newc)
	     (transform ?transform)
	     ))
      (sem ?sem)
      (gap -) (wh -) (argument ?argument)
      )
     -adv-compar-object-deleted>
      (head (ADV (LF ?lf) (sem ?sem) (atype ?atype)
	     (CONSTRAINT ?con) (comparative (? cc + w::superl))
	     (SUBCAT (% ?!subc (SEM ?subsem) ))
	     (SORT PRED) (var ?v)
	     (SUBCAT-MAP ?submap)
	     (comp-op ?dir)
	     (transform ?transform)
	     (ARGUMENT-MAP ?argmap) (arg ?arg)
	     (sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))
	     (ARGUMENT (% ?x (sem ?argsem) (lex ?arglex)))
	     (post-subcat -)
	     ))
      (append-conjuncts (conj1 ?con)
       (conj2 (& (scale ?scale) (orientation ?orient) (intensity ?ints)		
		 (?argmap ?arg) 
		 ;;(?submap (% *PRO* (var *) (class ?c) (sem ?argsem)))
		 ))
       (new ?newc))
      )
    

    ;; comparatives with deleted complements  JFA 8/02
     ;; bigger
#||
    ((ADJP (ARG ?arg) (VAR ?v) (atype ?atype) (comparative (? cc + w::superl))
      (LF (% PROP (CLASS ?lf) (VAR ?v)
	     (sem ?sem) 
	     (CONSTRAINT ?newc)
	     (transform ?transform)
	     ))
      )
     -adj-compar-object-deleted>
     (head (ADJ (LF ?lf) (sem ?sem) (atype ?atype)(VAR ?v) (prefix -)
		(CONSTRAINT ?con) (comparative (? cc + w::superl))
		(SUBCAT (% ?!subc (SEM ?subsem) ))
		(SORT PRED) (SUBCAT-MAP ?submap)
		(COMP-OP ?dir)
		(transform ?transform)
		(ARGUMENT-MAP ?argmap)
		(ARGUMENT (% ?x (sem ?argsem) (lex ?arglex)))
		(sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))
		(post-subcat -);;(W::allow-deleted-comp +)
		(FUNCTN -)
	    ))
     (unify (value ?argsem) (pattern ($ ?type (f::type ?c))))
     (append-conjuncts (conj1 ?con)
		       (conj2 (& 
			       (scale ?scale)
			       (orientation ?orient) (intensity ?ints)
				 (?argmap ?arg) 
				 (?submap (% *PRO* (var *) (class ?c) (sem ?argsem)))))
      (new ?newc))
     )||#

		       
  #||   ;; (a) BIGGER (computer than that) -- requiring a post-N1 subcat
    ((ADJ1 (ARG ?arg) (VAR ?v) (atype ?atype) (comparative +)
      (LF ?lf)
      (sem ?sem) 
      (CONSTRAINT ?newc)
      (transform ?transform)
      (post-subcat ?!subcat) (psarg ?psvar))
     -adj-compar-subcat-post-N1>
     (head (ADJ1 (LF ?lf) (sem ?sem) (VAR ?v) (atype ?atype)
		(CONSTRAINT ?con) (allow-post-n1-subcat +) (prefix -)
		(subcat ?!subcat)
		(SORT PRED) (SUBCAT-MAP ?submap)
		(COMP-OP ?dir)
		(transform ?transform)
		(sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))
		(ARGUMENT-MAP ?argmap)
		(ARGUMENT (% ?x (sem ?argsem) (lex ?arglex)))
		(FUNCTN -)
	    ))
     (append-conjuncts (conj1 ?con)
		       (conj2 (& 
				 (scale ?scale)
				 (orientation ?orient) (intensity ?ints)
				 (?argmap ?arg) 
				 (?submap ?psvar)))
				 
		       (new ?newc))
     )||#

   #|| ;; a house bigger than that
    ((ADJP (ARG ?arg) (VAR ?v) (COMPLEX +) (atype (? atp postpositive predicative-only)) 
      (LF (% PROP  (CLASS ?lf)
	     (VAR ?v) (CONSTRAINT (& (?argmap ?arg) (?reln ?argv)
				     (scale ?scale)
				     (intensity ?ints) (orientation ?orient)
						      ))
	     (transform ?transform) (sem ?sem) 
	     )))
     -adj-pred-compar-subcat>
     (head (ADJ (LF ?lf) (SUBCAT2 -) (post-subcat -)(VAR ?v) ;;(comparative +)
		(SUBCAT ?subcat) (SUBCAT-MAP ?reln) (SUBCAT (% ?xx (var ?argv)))
		(ARGUMENT-MAP ?argmap) 	
		
		(SORT PRED) (prefix -)
		(sem ?sem) (sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))
		(transform ?transform)
		(FUNCTN -)
		))
     ?subcat)
||#

     #| ; subsumed by adj-pred-twosubcats
    ;; adjectives that map to predicates with subcats, e.g.,  close to avon
    ;; The resulting adjective phrase is marked as predicative only, because really it cannot be used otherwise
    ;; ?? the afraid of dogs man ???
    ;; subcat is not passed up, so no "apples are larger than pears than oranges"
     ((ADJ1 (ARG ?arg) (VAR ?v) (COMPLEX +) (atype (? atp postpositive predicative-only)) (gap ?gap)
      	     (CONSTRAINT ?newc) (ARGUMENT-MAP ?argmap)
	     (transform ?transform) (sem ?sem)
	     )
     -adj-pred-subcat>
     (head (ADJ1 (LF ?lf) (post-subcat -)(VAR ?v) ;;(comparative -)
		(SUBCAT ?subcat) (SUBCAT-MAP ?reln) (SUBCAT (% ?xx (var ?argv) (gap ?gap)))
		(arg ?arg) (ARGUMENT-MAP ?argmap) (prefix -)
		(SORT PRED)
		(sem ?sem) (sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))
		(transform ?transform)
		(constraint ?con)
		))
     ?subcat
      (append-conjuncts (conj1 ?con) (conj2 (& (?argmap ?arg) (?reln ?argv) (scale ?scale) (intensity ?ints) (orientation ?orient)))					     
		       (new ?newc)))
     |#

   ;; pre adverb mod of adjective

   ((ADJ1 (ARG ?arg) (VAR ?v) (atype ?atype) (gap ?gap) (ARGUMENT-MAP ?argmap) 
     (CONSTRAINT ?newc)(SUBCAT ?subcat) (SUBCAT-MAP ?reln)
     (transform ?transform) (sem ?sem)
	     )
     -advbl-pre-adj1>
     (advbl (ATYPE PRE) (VAR ?advbv) (ARG ?v) ;;(SORT OPERATOR) 
            (argument (% ADJP (sem ?sem) (sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))))
            (gap -)
      )
     (head (ADJ1 (LF ?lf) (SUBCAT2 -) (post-subcat -)(VAR ?v)  (atype ?atype);;(comparative -)
		(SUBCAT ?subcat) (SUBCAT-MAP ?reln) (SUBCAT (% ?xx (var ?argv) (gap ?gap)))
		(ARGUMENT-MAP ?argmap) (prefix -) (arg ?arg)
		(SORT PRED)
		(sem ?sem) (sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))
		(transform ?transform)
		(constraint ?con)
		))
    (add-to-conjunct (val (MODS ?advbv)) (old ?con) (new ?newc))
    )

   ;; post adverb mod of adjective
   ;; apples are larger on the outside than pears
   ;; apples are larger than pears on the outside
   ((ADJ1 (ARG ?arg) (VAR ?v) (atype ?atype) ;;(gap ?gap) 
     (ARGUMENT-MAP ?argmap) 
     (CONSTRAINT ?newc) (SUBCAT ?subcat) (SUBCAT-MAP ?reln)
     (transform ?transform) (sem ?sem)
	     )
     -advbl-post-adj1> .98
     (head (ADJ1 (LF ?lf) (SUBCAT2 -) (post-subcat -)(VAR ?v)  (atype ?atype);;(comparative -)
		(SUBCAT ?subcat) (SUBCAT-MAP ?reln) ;;(SUBCAT (% ?xx (var ?argv) (gap ?gap)))
		(ARGUMENT-MAP ?argmap) (prefix -) (arg ?arg)
		(SORT PRED)
		;;(argument (% ?any (var ?vv) (sem ?argsem)))
		(sem ?sem) (sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))
		(transform ?transform)
		(constraint ?con)
		))
     (advbl (ATYPE POST) (VAR ?advbv) ;(ARG ?v) ;;(SORT OPERATOR) 
      (SEM ($ F::ABSTR-OBJ (f::type (? ont::position-reln ont::adequate ont::part-whole-val)) ))  ; e.g. on the outside, enough, in part
      ;;(argument (% w::ADJP (sem ?sem)))
      (gap -)
      )
     ?subcat
    (add-to-conjunct (val (MODS ?advbv)) (old ?con) (new ?newc))
    )

   
   ;;  the ADJP please "difficult to please" has an ARG that is the GAP in the clause. 
    ((ADJ1 (ARG ?arg) (VAR ?v) (COMPLEX +) (atype (? atp postpositive predicative-only)) (gap -)
      (CONSTRAINT ?newc)
						     
      (transform ?transform) (sem ?sem)
      )
     -adj-difficult-to-please>
     (head (ADJ1 (LF ?lf) (SUBCAT2 -) (post-subcat -)(VAR ?v) (comparative -)
		(SUBCAT ?subcat) (SUBCAT-MAP ?reln) (SUBCAT (% CP ))
		(ARGUMENT-MAP ?argmap) (prefix -)
		(SORT PRED)
		(sem ?sem) (sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))
		(transform ?transform)
		))
     (CP (var ?predv) (gap ?!gap) (gap (% np (var ?arg))))
     (append-conjuncts (conj1 ?con) (conj2 (& (?argmap ?arg) (?reln ?predv) (scale ?scale) (intensity ?ints) (orientation ?orient)))
		       (new ?newc)))

 #|
   ;;  special rule for COMPAR-OPS, converting an adjective to a comparative adjective

   ((ADJ (LF (:* ?pred ?lftype))
     (VAR ?v) (comparative +)
     (allow-deleted-comp +) (allow-post-n1-subcat +) ;(ALLOW-POST-N1-SUBCAT ?xx)
     ;(SUBCAT-MAP ?subcat-map)
     ;(subcat  ?subcat)
     (SUBCAT-MAP ?ground-smap)
     (subcat  ?ground-subcat)
     ;(comp-ptype ?pt)
     (ground-oblig ?go)
     (ground-subcat ?ground-subcat)
     (ATYPE CENTRAL)
     (SORT PRED)
     ;(sem ($ F::ABSTR-OBJ (f::scale ?scale)))
     (sem ?sem)
     (transform ?transform) (argument-map ont::figure) (argument ?argument)  (arg ?arg)
     )
     -more-adj-compar> 1.0
    (ADV (lf (:* ?pred ?xx)) (comparative (? cmp + superl))
     (ground-oblig ?go) (SUBCAT ?ground-subcat) (SUBCAT-MAP ?ground-smap))
    (head (ADJ (LF (:* ?lftype ?w)) (var ?v) 
	       (SUBCAT2 -) (post-subcat -)(VAR ?v) (comparative -)
	       (SUBCAT ?subcat) 
	       (subcat-map ?subcat-map)
	       (ATYPE central)
	       (argument ?argument) (arg ?arg)
	       (SORT PRED)
	       ;;(sem ($ F::ABSTR-OBJ (f::scale ?scale)))
	       (sem ?sem)
	       (transform ?transform)
	       ))
    )
|#

   ;;  special rule for COMPAR-OPS, converting an adjective to a comparative adjective (without subcats)
   ((ADJ (LF (:* ?new-pred ?w)) ;(LF (:* ?pred ?lftype))
     (VAR ?v) (comp-op ?comp-op)
     ;(allow-deleted-comp +)  ;(ALLOW-POST-N1-SUBCAT ?xx)
     ;(comp-ptype ?pt)
     (ground-oblig ?go)
     (ground-subcat ?ground-subcat)     
     (SORT PRED) (ATYPE CENTRAL) (comparative ?cmp) (arg ?arg) (allow-post-n1-subcat +)     
     (subcat  ?ground-subcat) (SUBCAT-MAP ?ground-smap)
     (subcat2 (% -)) (subcat2-map ONT::NOROLE)
     (argument ?argument) (argument-map ?arg-map) 
     (sem ?sem)
     ;(sem ($ F::ABSTR-OBJ (f::scale ?scale)))
     (transform ?transform)   
     )
     -more-adj-compar> 1.0
    (ADV (lf (:* ?pred ?xx)) (comparative (? cmp + superl)) (complex -)
     (ground-oblig ?go) (SUBCAT ?ground-subcat) (SUBCAT-MAP ?ground-smap))
    (head (ADJ (LF (:* ?lftype ?w)) (comp-op ?comp-op)
	       (SUBCAT2 -) (post-subcat -)(VAR ?v) (comparative -)
	       (SUBCAT -) 
	       (subcat-map ?subcat-map)
	       (ATYPE central)
	       (argument ?argument) (argument-map ?arg-map) (arg ?arg)
	       (SORT PRED)
	       ;;(sem ($ F::ABSTR-OBJ (f::scale ?scale)))
	       (sem ?sem)
	       (transform ?transform)
	       ))
    (recompute-more-less (adv-op ?pred) (adj-op ?comp-op) (result ?new-pred))
    )

   ;;  special rule for COMPAR-OPS, converting an adjective to a comparative adjective (with subcat)
   ((ADJ (LF (:* ?new-pred ?w)) ;(LF (:* ?pred ?lftype))
     (VAR ?v) (comp-op ?comp-op)
     ;(allow-deleted-comp +)  ;(ALLOW-POST-N1-SUBCAT ?xx)
     ;(comp-ptype ?pt)
     (ground-oblig ?go)
     (ground-subcat ?ground-subcat)     
     (SORT PRED) (ATYPE CENTRAL) (comparative ?cmp) (arg ?arg) (allow-post-n1-subcat +)     
     (subcat2  ?ground-subcat) (SUBCAT2-MAP ?ground-smap)
     (subcat ?!subcat) (subcat-map ?!subcat-map)
     (argument ?argument) (argument-map ?arg-map) 
     (sem ?sem)
     ;(sem ($ F::ABSTR-OBJ (f::scale ?scale)))
     (transform ?transform)   
     )
     -more-adj-compar2> 1.0
    (ADV (lf (:* ?pred ?xx)) (comparative (? cmp + superl)) (complex -)
     (ground-oblig ?go) (SUBCAT ?ground-subcat) (SUBCAT-MAP ?ground-smap))
    (head (ADJ (LF (:* ?lftype ?w)) (comp-op ?comp-op)
	       (SUBCAT2 (% -)) (post-subcat -)(VAR ?v) (comparative -)
	       (SUBCAT ?!subcat) 
	       (subcat-map ?!subcat-map)
	       (ATYPE central)
	       (argument ?argument) (argument-map ?arg-map) (arg ?arg)
	       (SORT PRED)
	       ;;(sem ($ F::ABSTR-OBJ (f::scale ?scale)))
	       (sem ?sem)
	       (transform ?transform)
	       ))
    (recompute-more-less (adv-op ?pred) (adj-op ?comp-op) (result ?new-pred))
    )
   
   
   ; most beautifully
   ((ADV (LF (:* ?pred ?lftype))
     (VAR ?v) (comparative +)
     (allow-deleted-comp +) ;(allow-post-n1-subcat +) ;(ALLOW-POST-N1-SUBCAT ?xx)
     ;(SUBCAT-MAP ?subcat-map)
     ;(subcat  ?subcat)
     (SUBCAT2-MAP ?ground-smap)
     (subcat2  ?ground-subcat)
     (SUBCAT ?subcat) 
     (subcat-map ?subcat-map)
     ;;(ground-oblig ?go)
     ;;(ground-subcat ?ground-subcat)
     ;(ATYPE CENTRAL)
     (ATYPE ?atype)
     (SORT PRED)
     ;(sem ($ F::ABSTR-OBJ (f::scale ?scale)))
     (sem ?sem)
     (transform ?transform) (argument-map ont::figure) (argument ?argument)
     (complex +)  ;; just stop it recursing on itself
     ;;(arg ?arg)
     )
     -more-adv-compar> 1.0
    (ADV (lf (:* ?pred ?xx))
     (comparative (? cmp + superl)) (complex -)
     (ground-oblig ?go)
     (SUBCAT ?ground-subcat)
     (SUBCAT-MAP ?ground-smap))
    (head (ADV (LF (:* ?lftype ?w)) (var ?v) 
	       (SUBCAT2 -) (post-subcat -)(VAR ?v) (comparative -)
	       (SUBCAT ?subcat) 
	       (subcat-map ?subcat-map)
	       ;(ATYPE central)
	       (ATYPE ?atype)
	       (argument ?argument) ;;(arg ?arg)
	       (SORT PRED)
	       ;;(sem ($ F::ABSTR-OBJ (f::scale ?scale)))
	       (sem ?sem)
	       (transform ?transform)
	       ))
    )

#||

   ((less-more (pred ONT::MORE-VAL) (ptype w::than))
    -less-more1> 1.0
    (head (word (lex more))))
   
   ((less-more (pred ONT::LESS-VAL) (ptype w::than))
    -less-more2> 1.0
    (head (word (lex less))))

   ((less-more (pred ONT::MAX-VAL) (ptype w::of))
    -less-more3> 1.0
    (head (word (lex most))))

   ((less-more (pred ONT::MIN-VAL) (ptype w::of))
    -less-more4> 1.0
    (head (word (lex least))))
||#

   ; $1000 cheaper
   ((ADJ1 (ARG ?arg) (VAR ?v) (COMPLEX ?complex) (atype ?newatype) 
	  (gap ?gap)
      (CONSTRAINT ?newc) (ARGUMENT-MAP ?argmap)
      (transform ?transform) (sem ?sem) (comparative +)
      (subcat ?subcat) 
	    )           
     -adj-pred-extent> 1
     (NP (SORT unit-measure) (class ont::quantity) (var ?var2)
	 (sem ($ ?s2 (f::scale ?sc)))
	 )
     (head (ADJ1 (ARG ?arg) (VAR ?v) (COMPLEX ?complex) (atype ?newatype) 
	  (gap ?gap) (sem ($ ?s (f::scale ?sc)))
      (CONSTRAINT ?con) (ARGUMENT-MAP ?argmap)
      (transform ?transform) (sem ?sem) (comparative +)
      (subcat ?subcat)))
     (append-conjuncts (conj1 ?con) (conj2 (& (extent ?var2))) (new ?newc))
     )

  
   ;; adjectives that map to predicates with two subcats, e.g.,  closer to avon than bath
   ((ADJ1 (ARG ?arg) (VAR ?v) (COMPLEX +) (atype ?newatype) ;(atype (? atp postpositive predicative-only))
	  (gap ?gap)
      (CONSTRAINT ?newc) (ARGUMENT-MAP ?argmap)
      (transform ?transform) (sem ?sem) (comparative ?cmp)
      (subcat ?subcat) ; pass up subcat so it can be used in -NP-ADJ-MISSING-HEAD-COMPAR> ; if we pass up both subcats we go into an infinite loop because this rule can be rematched!
	    )           
     -adj-pred-twosubcats> 1
     (head (ADJ1 (LF ?lf)  (VAR ?v)
	    (transform ?transform) (comparative ?cmp)
	    (SUBCAT ?subcat) (SUBCAT-MAP ?reln) (SUBCAT (% ?xx (var ?argv) (gap ?gap))) 
	    (SUBCAT2 ?subcat2)
	    (SUBCAT2-MAP (? !reln2 ONT::NOROLE -)) (SUBCAT2 (% ?xx2 (var ?argv2))) ; gap here too?
	    (post-subcat -)
	    (sem ?sem) (sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))
	    (ARGUMENT-MAP ?argmap) (arg ?arg) (prefix -)
	    (CONSTRAINT ?con) (atype ?atype)
	    (SORT PRED)))
     ?subcat
     ?subcat2
     (recompute-atype (atype ?atype) (subcat ?subcat) (subcat2 ?subcat2) (result ?newatype))
     (append-conjuncts (conj1 ?con) (conj2 (& (?argmap ?arg)
					      (?reln ?argv) (?!reln2 ?argv2)
					      (scale ?scale) (intensity ?ints) (orientation ?orient))
					     )
		       (new ?newc)))
   
   ;; adjectives that map to predicates with two subcats, e.g.,  closer than bath to avon
   ((ADJ1 (ARG ?arg) (VAR ?v) (COMPLEX +) (atype ?newatype) ;(atype (? atp postpositive predicative-only))
	  (gap ?gap)
      (CONSTRAINT ?newc) (ARGUMENT-MAP ?argmap)
      (transform ?transform) (sem ?sem)
      (subcat ?subcat) 
	    )           
     -adj-pred-twosubcats-rev>
     (head (ADJ1 (LF ?lf)  (VAR ?v)
	    (transform ?transform) 
	    (SUBCAT ?subcat) (SUBCAT-MAP ?reln)
	    (SUBCAT (% ?xx (var ?argv) (gap ?gap))) 
	    (SUBCAT2-MAP (? !reln2 ONT::NOROLE -))
	    (SUBCAT2 ?subcat2)
	    (SUBCAT2 (% ?xx2 (var ?argv2))) ; gap here too?
	    (post-subcat -)
	    (sem ?sem) (sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))
	    (ARGUMENT-MAP ?argmap) (arg ?arg) (prefix -)
	    (CONSTRAINT ?con) (atype ?atype)
	    (SORT PRED)))
    ?subcat2
    ?subcat
    (both-bound (subcat ?subcat) (subcat2 ?subcat2)) ;;  both of these must be bound, otherwise this is a duplicate to a constit produced by the twosubcats rule above
     (recompute-atype (atype ?atype) (subcat ?subcat) (subcat2 ?subcat2) (result ?newatype))
     (append-conjuncts (conj1 ?con) (conj2 (& (?argmap ?arg)
					      (?reln ?argv) (?!reln2 ?argv2)
					      (scale ?scale) (intensity ?ints) (orientation ?orient))
					     )
		       (new ?newc)))

   ;;  To handle the variety of ways that COMPARs are expressed, we use an intermediate constituent COMPAR
   ;; some of these are very unrestrictive so have to be ranked low to allow other alternatives to dominate

   ;; adjectives that map to predicates with only one subcat, e.g., X is dependent on Y
   ((ADJ1 (ARG ?arg) (VAR ?v) (COMPLEX +) (atype ?newatype) ;(atype (? atp postpositive predicative-only))
	  (gap ?gap)
      (CONSTRAINT ?newc) (ARGUMENT-MAP ?argmap)
      (transform ?transform) (sem ?sem) (comparative ?cmp)
      (subcat ?subcat) ; pass up subcat so it can be used in -NP-ADJ-MISSING-HEAD-COMPAR> ; if we pass up both subcats we go into an infinite loop because this rule can be rematched!
	    )           
     -adj-pred-onesubcat> 
     (head (ADJ1 (LF ?lf)  (VAR ?v)
	    (transform ?transform) (comparative ?cmp)
	    (SUBCAT ?subcat) (SUBCAT-MAP ?reln) (SUBCAT (% ?xx (var ?argv) (gap ?gap))) 
	    (SUBCAT2 ?subcat2) (SUBCAT2 (% - (W::VAR -)))
	    ;(SUBCAT2-MAP (? !reln2 ONT::NOROLE -)) (SUBCAT2 (% ?xx2 (var ?argv2))) ; gap here too?
	    (post-subcat -)
	    (sem ?sem) (sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))
	    (ARGUMENT-MAP ?argmap) (arg ?arg) (prefix -)
	    (CONSTRAINT ?con) (atype ?atype)
	    (SORT PRED)))
     ?subcat
     ;?subcat2
     (recompute-atype (atype ?atype) (subcat ?subcat) (subcat2 ?subcat2) (result ?newatype))
     (append-conjuncts (conj1 ?con) (conj2 (& (?argmap ?arg)
					      (?reln ?argv) ;(?!reln2 ?argv2)
					      (scale ?scale) (intensity ?ints) (orientation ?orient))
					     )
		       (new ?newc)))

   
   ;; the standard "than PP" is the typical case
   ((COMPAR (var ?v) (sem ?sem))
    -compar-than-pp> 1
    (PP (var ?v) (ptype THAN) (sem ?sem)))

   ;; e.g.,  It is larger than expected
   ((compar (Var ?v))
     -compar-vp>
     (word (lex than))
     (vp (var ?v))
    )

   ((compar (var ?v))
    -compar-s-gap> .98  ;; very productive and unconstrained!!
    (word (lex than))
    (s (var ?v)))
       
    ;;=============================================================================
    ;; NOUN-NOUN type  modification
    
    ;; e.g.,  Elmira route/train, the July 31st resolution, my 1990 tax return
    ((N1 (RESTR ?new) (CLASS ?c) (SORT PRED) (sem ?sem)
         (QUAL -) (relc -) (subcat ?subcat) (name-mod +)  ;; only allow one name modifier
     ) 
     -name-n1> .98
     (np (name +) (generated -) ;; don't allow numbers or times here
         (VAR ?v1) (gap -))
     (head (N1 (VAR ?v2) (relc -) (sem ?sem) (sem ($ (? x F::ABSTR-OBJ F::PHYS-OBJ))) ;;  F::SITUATION)))
	    (RESTR ?r) (CLASS ?c) (SORT PRED) (name-mod -)
	    (derived-from-name -)  ; the second n shouldn't be a name
	    ;;(subj-map -)  ;; nominalized verbs have their own rules
	    (subcat ?subcat)
	    (post-subcat -)
	    (postadvbl -) 
	    (generated -)
	    (gap -)
	    )
      )
     (add-to-conjunct (val (ASSOC-WITH ?v1)) (old ?r) (new ?new)))


 ;; e.g.,  specialized construction - where head N1 occurs first -- only in medical domain so far, as in MEK Y280S (where the second is a mutation modifiers)
    ((N1 (RESTR ?new) (CLASS ?c) (SORT PRED) (sem ?sem)
         (QUAL -) (relc -) (subcat ?subcat) (name-mod +)  ;; only allow one name modifier
     ) 
     -n1-mutation> .98
     
     (head (N1 (VAR ?v2) (relc -) (sem ?sem) (sem ($ (? x F::ABSTR-OBJ F::PHYS-OBJ))) ;;  F::SITUATION)))
	    (RESTR ?r) (CLASS ?c) (SORT PRED) (name-mod -)
	    (subjmap -)  ;; nominalized verbs have their own rules
	    (subcat ?subcat)
	    (post-subcat -)
	    (postadvbl -) 
	    (generated -)
	    )
      )
     (np (name +) (time-converted -) (sem ($ f::SITUATION (f::type ont::mutation))) 
      (VAR ?v1))
     (add-to-conjunct (val (ASSOC-WITH ?v1)) (old ?r) (new ?new)))
        
    ;; e.g., the mountain route, the truck plan, the security zone, ...
   ;;   such as "The small car lot"    
    ((N1 (RESTR ?new) (SORT ?sort) (sem ?sem) (class ?c) ;(class (? c ONT::REFERENTIAL-SEM))
	 (N-N-MOD +) (QUAL -) (relc -) (subcat ?subcat) (gap ?gap)
	 (complex +) ; to prevent "the X, Y and Z boxes" to make "Z boxes" one of the conjuncts in n1-conj1
	 )
      
     -n-sing-n1-> 0.98 ;; prevent this from happening too often
     (n1 ;(AGR 3s) ; agr could be 3p for "the dog and cat boxes"
	 (abbrev -) (generated -) (lex ?lex)
      (var ?v1) (restr ?modr)  (gerund -)   ;; we expect gerunds as modifiers to be adjectives, not N1
      ;;  removed this to handle things like "computing services"
      ;; we reinstated "gerund -" as "computing" should be an adjective (and we need to exclude "... via phosphorylating Raf"
      (sem ?n-sem) (derived-from-name -) ; names go through -name-n1>
      (CLASS ?modc) (PRO -) (N-N-MOD -) ;(COMPLEX -)   ;;  can't require COMPLEX - any more -- e.g., "p53 expression levels"  -- now we can!! This goes through nom-rate instead.
      ; set two-n1-conjunct to complex - so that e.g., <trade and migration> route, can go through this rule
      ; removed "COMPLEX -" again: "water treatment plant" (added complex + back to two-n1-conjunct)
      (SUBCAT (% -)) ;(SUBCAT ?ignore)
      (GAP -) (kr-type ?kr-type)
      (postadvbl -) (post-subcat -) 
      )
     (head (N1 (VAR ?v2) (QUAL -) (subcat ?subcat) (sort ?sort) (one -) ; exclude the referential-sem w::one
	       (sem ?sem)  (class ?c) ;(class (? c ONT::REFERENTIAL-SEM)) ; "monsoon season": season is not referential-sem
	       (generated -)
	       ;;(sem ($ (? x F::ABSTR-OBJ F::PHYS-OBJ))) ;;If we put this in, the SEM info doesn't get passed up!!
	       (RESTR ?r)
	       (SORT PRED) (gap ?gap) 
	    (relc -)  (postadvbl -) (post-subcat -) 
	    (subjmap -)  ;; nominalized verbs have their own rules
	    (abbrev -)
	       ))
     (add-to-conjunct 
      (val (ASSOC-WITH (% *PRO* (status ont::kind) (var ?v1) (class ?modc) (constraint ?modr) (sem ?n-sem) (kr-type ?kr-type) (lex ?lex)))) 
      (old ?r) (new ?new)))

    ;; n-n mods with hyphen  -- this allows us to override our otherwise strict requirement to attach on the right
    ((N1 (RESTR ?new) (CLASS ?c) (SORT ?sort) 
      (QUAL -) (relc -) (subcat ?subcat) (n-sing-already +)  ;; stop this happening more than once
      )
     -n-sing-hyphen-n1-> .98
     (n1 (AGR 3s) 
        (var ?v1) (sem ?sem) (restr ?modr) (derived-from-name -) ; names go through -name-n1>
	(CLASS ?modc) (PRO -) (N-N-MOD -) ;(COMPLEX -)
	(SUBCAT (% -)) (GAP -)
      (postadvbl -) (post-subcat -) (n-sing-already -)
      )
     (word (lex w::punc-minus))
     (head (N1 (VAR ?v2) (QUAL -) (subcat ?subcat) (n-sing-already -)
	       (RESTR ?r) (CLASS ?c) (SORT ?sort) (N-N-MOD -)
	       (relc -)  (postadvbl -) (post-subcat -) (name-mod -)
	       (subjmap -)  ;; nominalized verbs have their own rules
	       ))
     (add-to-conjunct (val (ASSOC-WITH (% *PRO* (status ont::kind) (var ?v1) (class ?modc) 
					  (constraint ?modr) (sem ?sem)))) (old ?r) (new ?new)))

 ;; N-N mod with relational nouns, with "the book title", the book will fill the :OF role.
    ((N1 (RESTR ?new) (CLASS ?c) (SORT PRED) 
      (N-N-MOD +) (QUAL -) (relc -) (subcat ?subcat)
      )
     -n-sing-reln1-> 
     (n1 (AGR 3s) 
        (var ?v1) (sem ?sem) (restr ?modr) 
	(CLASS ?modc) (PRO -) (N-N-MOD -) ;(COMPLEX -)
	(SUBCAT (% -)) (GAP -)
      (postadvbl -) (post-subcat -)
      )
     (head (N1 (VAR ?v2) (QUAL -) (subcat (% ?cat (sem ?sem)))
	       (RESTR ?r) (CLASS ?c) (SORT RELN) (subcat-map ?smap)
	       (relc -)  (postadvbl -) (post-subcat -)
	       ))
     (add-to-conjunct (val (?smap (% *PRO* (status ont::definite) (var ?v1) (class ?modc) (constraint ?modr) (sem ?sem))))
      (old ?r) (new ?new)))

    ;;  plural N-N is much more rare:
    ;;  the mountains route; the books link

    ((N1 (RESTR ?new) (CLASS ?c) (SORT PRED) (COMPLEX -)
      (N-N-MOD +) (QUAL -) (relc ?relc) (subcat ?subcat)
      (post-subcat -)
         )
     -n-plur-n1-> 0.95 ;; prevent this from happening too often
     (n1 (AGR 3p) (abbrev -)
	 (var ?v1) (sem ?sem) (restr ?modr) 
	 (CLASS ?modc) (PRO -) (N-N-MOD -) (COMPLEX -) (SUBCAT (% -)) (GAP -)
	 (postadvbl -) (post-subcat -)
      )
     (head (N1 (VAR ?v2) (QUAL -) 
	    (RESTR ?r) (CLASS ?c) (SORT PRED)
	    (relc ?relc) (subcat ?subcat) (postadvbl -)
	    (post-subcat -)  (subjmap -)  ;; nominalized verbs have their own rules
	       ))
      (add-to-conjunct (val (ASSOC-WITH (% *PRO* (status ont::kind) (var ?v1) (class ?modc) (constraint ?modr) (sem ?sem)))) (old ?r) (new ?new)))
    
    ;;=======================
    ;;
    ;;  RELATIVE CLAUSES
    ;;
    
	
    ;; e.g., the train that went to Avon, the train I moved, the train that is in Avon
    
    ((N1 (RESTR ?con)
      (CLASS ?c) (SORT ?sort) (QUAL ?qual) (COMPLEX +) (var ?v)
      (relc +)  (subcat (% -)) (post-subcat -)
      )
     -n1-rel>
     (head (N1 (VAR ?v) (RESTR ?r)
	       (CLASS ?c) (SORT ?sort) (QUAL ?qual)
	    (SEM ?sem) ;;(subcat -) 
	    (post-subcat -) (gap -) ;;(derived-from-name -) 
	    (no-postmodifiers -) ;; exclude "the same path as the battery I saw" and cp attaching to "path"
	    (agr ?agr) (rate-activity-nom -) (agent-nom -)
	    ))
;     (cp (ctype relc) (VAR ?relv) (ARG ?v) (ARGSEM ?argsem) (agr ?agr)
     (cp (ctype relc) (VAR ?relv) (ARG ?v) (ARGSEM ?sem) (agr ?agr)
      (LF ?lf))
     (add-to-conjunct (val (MODS ?relv)) (old ?r) (new ?con)))

    ;;  Great construction!:   All he saw (was mountains)

  #|| ((NP (LF (% description (status ont::definite) (VAR *)
		    (CLASS ?c) (CONSTRAINT (& (mod ?relv)))
		    (sem ?sem)  (transform ?transform)
		    ))
     (sem ?sem)
     (SORT PRED) (VAR *) (AGR (? agr 3s 3p)) (CASE (? case SUB OBJ)))
     -all-he-saw> .98
    (head (word (lex (? x w::all w::what))))
    (cp (ctype relc) (VAR ?relv) (ARG *) (ARGSEM ?argsem) 
     (LF ?lf))
    )
   ||#
   
   ;; somewhat rare construction allows qualifications: the man though who saw the party, lied about it.
   ((N1 (RESTR ?con)
      (CLASS ?c) (SORT ?sort) (QUAL ?qual) (COMPLEX +) (var ?v)
      (relc +)  (subcat (% -)) (post-subcat -)
      )
    -n1-qual-rel> .96
    (head (N1 (VAR ?v) (RESTR ?r)
	      (CLASS ?c) (SORT ?sort) (QUAL ?qual)
	      (SEM ?sem) ;;(subcat -) 
	      (post-subcat -) (gap -)
	      (no-postmodifiers -) 
	      ))
    (advbl (var ?advv) (sort w::disc) (sem ($ f::abstr-obj (f::type ont::qualification))))
    (cp (ctype relc) (VAR ?relv) (ARG ?v) (ARGSEM ?argsem) 
     (LF ?lf))
    (add-to-conjunct (val (MODS (?relv ?advv))) (old ?r) (new ?con)))


    ;; the man whose dog barked
    ((N1 (RESTR ?con)
      (CLASS ?c) (SORT ?sort)
      (QUAL ?qual) (COMPLEX +) (wh -) ;;(wh-var ?whv)
      (relc +)  (subcat (% -)) (post-subcat -)
      )
     -n1-rel-whose>
     (head (N1 (VAR ?v) (RESTR ?r) (SEM ?sem) (CLASS ?c) (SORT ?sort) (QUAL ?qual)
	    (sem ?argsem)
	    (post-subcat -)
	    (no-postmodifiers -) ;; exclude "the same path as the battery I saw" and cp attaching to "path"
	    ))
     (cp (ctype rel-whose) (VAR ?relv) (wh R) (wh-var ?v) ;(arg ?v) (argsem ?sem)
      (LF ?lf))
    (add-to-conjunct (val (MODS ?relv)) (old ?r) (new ?con)))

    ;; attach an s-to to a noun:
    ;; I have a job to do, an option to suggest
  ((N1 (RESTR ?con) (gap -)
      (CLASS ?c) (SORT ?sort) (QUAL ?qual) (COMPLEX +)
      (subcat (% - (W::VAR -))) ;(subcat -)
      (post-subcat -)
      )
     -n1-inf> .92
     (head (N1 (VAR ?v) (RESTR ?r) (SEM ?sem) (CLASS ?c) (SORT ?sort) (QUAL ?qual)
	       (subcat (% - (W::VAR -))) ;(subcat -)
	       (post-subcat -)
	    (no-postmodifiers -) ;; exclude "the same path as the battery I saw" and cp attaching to "path"
	    ))
     (cp (ctype s-to) (VAR ?tov) (subj ?subj)   (gap (% np (sem ?sem) (var ?v)))
	 (dobj ?!dobj) (dobj (% np (sem ?sem)))
	  (LF ?lf)) 
     (add-to-conjunct (val (MODS ?tov)) (old ?r) (new ?con)))
  
  ;; e.g., anything else, what else
    ((NP (SORT PRED)
         (VAR ?v) (SEM ?sem) (lex ?hl) (headcat ?hc) (Class ?c) (AGR ?agr) (WH ?wh) (PRO INDEF)(case ?case)
	 (wh-var ?v) ; "anything" shouldn't really have wh-var, but its WH would be - (wh-var is needed to instantiate :focus in questions)
         (LF (% Description (status ?status) (var ?v) (Class ?c) (SORT individual)
                (Lex ?lex)
                (sem ?sem) 
		(constraint (& (MODS ?else-v) (proform ?hl)))
		(transform ?transform)
                ))
      )
     -np-anything-else>
     (head (pro (SEM ?sem) (AGR ?agr) (VAR ?v) (headcat ?hc) (lex ?hl)
	        (PRO INDEF) (status ?status) (case ?case)
	        (VAR ?v) (WH ?wh) (LF ?c)
	        (transform ?transform)
	        ))
      (Advbl (sort else) (var ?else-v) (arg ?v))
     )

    ;; e.g., something nice, anything like it
    ;; the LF built in the same way as in n1-qual1
    ;; postpositive adjps alowed to parse "something like a dog"
    ((NP (SORT PRED)
      (VAR ?v) (SEM ?sem) (lex ?hl) (headcat ?hc) (Class ?c) (AGR ?agr)
      ;;(WH ?wh)
      (PRO INDEF)(case ?case)
         (LF (% Description (status ?status) (var ?v) (Class ?c) (SORT individual)
                (Lex ?lex)
                (sem ?sem) 
		(constraint (& (MODS ?adjv) (proform ?hl)))
		(transform ?transform)
                ))
      )
     -np-anything-adj> .96 ; only use when needed
     (head (pro (SEM ?sem) (AGR ?agr) (VAR ?v) (headcat ?hc) (lex ?hl)
	        (PRO INDEF) (status ?status) (case ?case)
	        (VAR ?v)
		(WH -) ;;  (WH ?wh)   I can't think of a WH pro in this context
		(LF ?c)
	        (transform ?transform)
	    ))
     (ADJP (atype (? at attributive-only central postpositive)) (LF ?qual) (ARG ?v) (VAR ?adjv)
      (argument (% NP (sem ?sem))) (comparative ?com)
      (post-subcat -)
      )
     )
     
     
         ;; e.g., something nice, anything like it
    ;; the LF built in the same way as in n1-qual1
    ;; postpositive adjps alowed to parse "something like a dog"
    ((NP (SORT PRED)
         (VAR ?v) (SEM ?sem) (lex ?hl) (headcat ?hc) (Class ?c) (AGR ?agr) (WH ?wh) (PRO INDEF)(case ?case)
         (LF (% Description (status ?status) (var ?v) (Class ?c) (SORT individual)
                (Lex ?lex)
                (sem ?sem) 
		(constraint (& (MODS ?advvar) (proform ?hl)))
		(transform ?transform)
                ))
      )
     -np-pro-pred> .96			; only use when needed
     (head (pro (SEM ?sem) (AGR ?agr) (VAR ?v) (headcat ?hc) (lex ?hl)
		(status ?status) (case ?case)
	        (VAR ?v) (WH ?wh) (LF ?c)
	        (transform ?transform)		
		(lex (? lxx something everything nothing anything someone anyone somebody anybody somewhere anywhere these those))
		))
     (PRED (LF ?l1) (ARG ?v) ;;SORT SETTING) 
      (var ?advvar) (ARGUMENT (% NP (sem ?sem))))
     )
   
   
   ((np (sort PRED)  (gap -) (mass bare) (case (? case SUB OBJ))
     (sem ?s-sem) (var ?npvar) (WH -) (agr ?a)
     (lf (% description (status ?status) (VAR ?npvar) 
	    (constraint ?constraint) (sort ?npsort)
	    (sem ?npsem)  (class ?npclass) (transform ?transform)
	    )))
    -np-pro-cp> .96
    (head (np (var ?npvar) (sem ?npsem) 
	   (PRO INDEF) ;; (? prp INDEF +))  can't allow definite pros here!
	   (WH -)
	   (agr ?a) (case ?case)
	   ;; Myrosia 2009/04/10 Cases like "anything you saw" are handled by indef-pro-desc
	   ;; they are sort wh-desc and cannot come up as fragments
	   ;; "those in a box" is a valid fragment
	   ;; We may want to consolidate the handling later
	   ;;(lex (? lxx these those))
	   (lf (% description (class ?npclass) (status ?status) (constraint ?cons) (sort ?npsort)
		  (transform ?transform)
		  ))))
    (cp (ctype relc) (reduced -)
     (ARG ?npvar) (ARGSEM ?npsem)  (VAR ?CP-V)
     (LF ?lf) 
     )
    (add-to-conjunct (val (suchthat ?cp-v)) (old ?cons) (new ?constraint)))
   
    ;;   simple appositives,
    ;;  e.g., city avon, as in "the city avon"
    ;;
    ((N1 (RESTR ?con) (CLASS ?c) (SORT ?sort) (QUAL ?qual) ;(COMPLEX +)  ; took out complex + so this will go through two-np-conjunct
	 (subcat (% - (W::VAR -))) ;(subcat -)
	 (post-subcat -)
      )
     -N1-appos1> .98
     (head (N1 (VAR ?v1) (RESTR ?r) (CLASS ?c) (sort (? !sort unit-measure)) ;(SORT ?sort) 
	       (QUAL ?qual) (relc -) (sem ?sem)
	       (subcat (% - (W::VAR -))) ;(subcat -)
	       (post-subcat -) (complex -) (derived-from-name -) (time-converted -)
	    )      
      )
     (np (name +) (generated -) (sem ?sem) (class ?lf) (VAR ?v2) (time-converted -))
     (add-to-conjunct (val (IDENTIFIED-AS ?v2)) (old ?r) (new ?con)))
	
   ;; same with comma  the city, avon
    ((N1 (RESTR ?con) (CLASS ?c) (SORT ?sort) (QUAL ?qual) ;(COMPLEX +) 
      (subcat (% - (W::VAR -)))) ;(subcat -))
     -N1-appos2>
     (head (N1 (VAR ?v1) (RESTR ?r) (CLASS ?c) (SORT ?sort) (QUAL ?qual) (relc -)
	       (subcat (% - (W::VAR -))) ;(subcat -)
	       (post-subcat -) (sem ?sem)
	    ))
     (punc (lex w::punc-comma))
     (np (name +) (generated -) (CLASS ?c) (sem ?sem) (VAR ?v2))
     (add-to-conjunct (val (EQ ?v2)) (old ?r) (new ?con)))
		
    ))


;;; MOre complex relational nouns: the difference in states between the terminals

(parser::augment-grammar
  '((headfeatures
     (N1 var arg lex orig-lex headcat transform agr mass case sem quantity indef-only refl abbrev nomobjpreps)
     (ADJ1 arg lex orig-lex headcat transform argument subcat allow-deleted-comp subcat-map)); constraint))

     ;; adjectives with an explicit scale, e.g., red in color, high in temperature, apples are larger in size (than oranges)
   ((ADJ1 (ARG ?arg) (VAR ?v) (COMPLEX +) (LF ?lf)
     (atype (? atp postpositive predicative-only)) (gap ?gap) (SORT PRED)
     (comparative ?comp) (ARGUMENT-MAP ?argmap) (constraint ?newcon)
     (sem ($ F::ABSTR-OBJ (f::scale ?newscale) (F::intensity ?ints) (F::orientation ?orient)))
     (subcat2 ?subcat2) (subcat2-map ?subm2)
     ;;(LF (% PROP  (CLASS ?lf)
     ;;(VAR ?v) (CONSTRAINT (& (?argmap ?arg) (?reln ?argv) (FUNCTN ?fn) (scale ?newscale) (intensity ?ints) (orientation ?orient)
     ;;))
     (transform ?transform) 
     ;(sem ?sem)
     )
    -adj-pred-scale> 
    (head (ADJ1 (LF ?lf) (post-subcat -)(VAR ?v) (comparative ?comp)
	       (ARGUMENT-MAP ?argmap) (prefix -) 
	       (subcat ?subcat) (subcat-map ?subm) (subcat2 ?subcat2) (SUBCAT2 (% - (W::VAR -))) (subcat2-map ?subm2)
	       (SORT PRED) (constraint ?con)
	       (sem ?sem) (sem ($ F::ABSTR-OBJ (f::scale ?scale1) (F::intensity ?ints) (F::orientation ?orient)))
	       (transform ?transform)
	       (FUNCTN -)
	       ))
    (pp (ptype w::in) (var ?sc-var) (sem  ($ F::ABSTR-OBJ (f::type ont::domain)
					     (f::scale ?!scale2))) (gap ?gap))
    (class-greatest-lower-bound (in1 ?scale1) (in2 ?!scale2) (out ?newscale))
    (add-to-conjunct (val (scale ?sc-var)) (old ?con) (new ?newcon))
    )

   ;; adjectives with a standard, e.g., He is tall for a midget.
   ((ADJ1 (ARG ?arg) (VAR ?v) (COMPLEX +) (LF ?lf)
     (atype (? atp postpositive predicative-only)) (gap ?gap) (SORT PRED)
     (comparative -) (ARGUMENT-MAP ?argmap) (constraint ?newcon)
     ;(sem ($ F::ABSTR-OBJ (f::scale ?newscale) (F::intensity ?ints) (F::orientation ?orient)))
     (subcat -) (subcat-map -)
     (transform ?transform) 
     (sem ?sem)
     )
    -adj-standard> 
    (head (ADJ1 (LF ?lf) (post-subcat -)(VAR ?v) (comparative -) ;(comparative ?comp)
		(ARGUMENT-MAP ?argmap) (prefix -)
		(ARGUMENT (% ?x (sem ?argsem) (lex ?arglex)))
	       ;(subcat ?subcat) (subcat-map ?subm)
		(subcat2 ?subcat2) (SUBCAT2 (% - (W::VAR -)))
		(SORT PRED) (constraint ?con)
	       (sem ?sem) ;(sem ($ F::ABSTR-OBJ (f::scale ?scale1) (F::intensity ?ints) (F::orientation ?orient)))
	       (transform ?transform)
	       (FUNCTN -)
	       ))
    (pp (ptype w::for) (var ?!pp-var) (sem ?argsem) (gap ?gap))
    (append-conjuncts (conj1 ?con) (conj2 (& (standard ?!pp-var))) (new ?newcon))
    )

   
    ((N1 (sort pred) (var ?v) (class ?lf) (qual -) (COMPLEX +)
      (restr (& (?smap ?v1) (?amap ?v2)))
      (subcat ?!subcat) (argument -)
      )
     -N1-reln-arg-subcat1>
     (head (n (sort reln) (lf ?lf)
	    (subcat ?!subcat)
	    (subcat (% ?scat (var ?v1) (sem ?ssem) (lf ?lf2) (sort (? ssrt pred individual set comparative reln unit-measure))))
	    (subcat-map ?smap)
	    (argument ?!argument)
	    (argument (% ?arg (var ?v2) (sem ?asem) (lf ?lf3) (sort (? asrt pred individual set comparative reln))))
	    (argument-map ?amap)))
     ?!subcat
     ?!argument
     )
    ;; difference in states, with "states" being mapped consistently to the same role as in other cases
    ((N1 (sort pred) (var ?v) (class ?lf) (qual -) (COMPLEX +)
      (restr (& (?amap ?v2)))
      (subcat -) (argument -)
      )
     -N1-reln-arg-subcat-deleted>
     (head (n (sort reln) (lf ?lf)
	    (allow-deleted-comp +)
	    (subcat ?!subcat)	    
	    (argument ?!argument)
	    (argument (% ?arg (var ?v2) (sem ?asem) (lf ?lf3) (sort (? srt pred individual set comparative reln))))
	    (argument-map ?amap)))     
     ?!argument
     )
))


;;(cl:setq *grammar-n1-aux*
(parser::augment-grammar
  '((headfeatures
     (NP headcat lex orig-lex postadvbl)
      (NAME headcat lex orig-lex)
      (N1 VAR AGR Changeagr case lex orig-lex headcat transform subcat set-restr refl abbrev nomobjpreps) ;;  excludes MASS as a head feature
      (N lex orig-lex headcat refl))
  
  #||    I THINK THIS HAS OUTLIVED ITS USEFULNESS!  JFA 6/14
    ;; container loads of commodities, e.g., boxcars of oranges    ==> should probably be redone as a coercion rule JFA 12/02
    ;;   we allow any container to be a unit-measure term
    ((N1 (VAR ?v) (SORT UNIT-MEASURE) (MASS ?m) (CLASS (:* ont::VOLUME-UNIT ?lf)) (sem ?!sem) (QUAL -)
      )
     -n1-container-commodity> 0.95
     (head (N (SEM ($ F::PHYS-OBJ (f::CONTAINER f::+)
		       )) (sem ?!sem) (sort pred) 
	    (PRED ?pred) (MASS ?m) (LF (:* ?lf ?w)) (ARGSEM ?argsem)))
     )||#

 ;;  COERCION RULE FOR NOUNS e.g., (take my) prescription
    
    ((N (VAR ?v) (MORPH ?m) (SORT ?sort) (CLASS ?kr) (LF ?lf-new)
	(RESTR (& (MODS (% *PRO* (Var **) (status ont::f)
			   (class ?op)
			   (constraint (& (is ?v)
					  (FIGURE (% *PRO* (var *) (class ?lf-old) (SEM ?oldsem) (constraint (& (related-to **)))))))))))
	(lex ?l) (sem ?newsem) (AGR ?agr)
      (COERCE -) (TRANSFORM ?t) (MASS ?mass) (CASE ?case) 
      (argument ?argument) (subcat ?subcat) 
      )
     -n1-coerce> .97
     (head (N (COERCE ((% coerce (KR-TYPE ?kr) (Operator ?op) (sem ?newsem) (LF ?lf-new)) ?cc))
              (VAR ?v) (sem ?oldsem) (MORPH ?m) (SORT ?sort) (LF ?lf-old) (lex ?l) (agr ?agr)
	    (TRANSFORM ?t) (MASS ?mass) (CASE ?case) 
	    (argument ?argument) (subcat ?subcat)
	    )
      )
     )
    
       ;; coercing a region into an agent, Italy

    ((NP (VAR *) (MORPH ?m) (SORT ?sort)
         (lf (% description (STATUS ONT::DEFINITE) (VAR *) (CLASS ont::political-region)
                (CONSTRAINT (& (assoc-with ?v)))))
         (SEM ($ F::phys-obj (f::information ?inf)(F::MOBILITY F::MOVABLE) (f::intentional f::+) (F::spatial-abstraction F::spatial-point)))
         (AGR ?agr)
         (COERCE -) (TRANSFORM ?t) (MASS ?mass) (CASE ?case) 
         (argument ?argument) (subcat ?subcat) (coerced +)
         )
     -n1-region-to-actor-coerce> 0.97
     (head (NP (VAR ?v) (LF (CLASS ont::political-region)) (unit-spec -)
               (SEM ($ F::phys-obj (F::spatial-abstraction F::spatial-region) (f::intentional -) (f::object-function f::spatial-object)))
               (MORPH ?m) (SORT ?sort) (agr ?agr) (generated -)
               (TRANSFORM ?t) (MASS ?mass) (CASE ?case) 
               (argument ?argument) (subcat ?subcat)
	       (coerced -)
              )
           )
    )
    
    
  ;;  COERCION RULE FOR Names
    ((Name (name +) (VAR ?v) (CLASS ?kr) (LF ?lf-new)
	   (RESTR (& (MODS (% *PRO* (var **) (status ont::f) (class ?op)
				  (constraint (& (is ?v) (FIGURE (% *PRO* (var *) (STATUS ONT::DEFINITE)
								(constraint (& (name-of ?l)))  ;; NAME-OF used to be ?fname JFA 5/04
								(class ?lf-old)))))))))
	   (lex ?l) (sem ?newsem) (AGR ?agr) (full-name ?fname)
	   (COERCE -) (TRANSFORM ?t) )
     -name-coerce>
     (head (name (name +) (COERCE ((% coerce (KR-TYPE ?kr) (Operator ?op) (sem ?newsem) (LF ?lf-new)) ?cc)) ;; gets lf any-sem
		 (VAR ?v) (class ?class-old) (full-name ?fname) (LF ?lf-old) (lex ?l) (agr ?agr)
		 (TRANSFORM ?t)))     
     )      
  ))



;;=========================================================================
;;   NP rules build description structures, which consist of
;;         STATUS - definite/indefinite/quantifiers
;;         VAR - as usual
;;         SORT - a complex expression combine the AGR and MASS features,
;;                that is simplified by an attachment as follows:
;;                (1s/2s/3s -) -> Individual
;;                (1p/2p/3p -) -> Set
;;                (1s/2s/3s +) -> Stuff
;;                (1p/2p/3p +) -> Stuff (quantities of stuff)
;;        CONSTRAINT - the restrictions from N1
;;        SET-CONSTRAINT - the restrictions from SPEC
;;        In singular NPs, the two constraints are simply combined
;;        In plural NPs, the SET-CONSTRAINT apply to the set, and 
;;        the CONSTRAINT to individuals in the set


(parser::augment-grammar 
      '((headfeatures
         (NP CASE NAME agr SEM PRO CLASS Changeagr ARGUMENT argument-map SUBCAT role lex orig-lex headcat transform postadvbl refl gerund abbrev derived-from-name
	  subj dobj subcat-map comp3-map)) ; no gap/mass

	;;  special rule for proteins that are tagged as common nouns but used as names
	((NP (LF (% Description (STATUS ONT::definite) (VAR ?v) (SORT INDIVIDUAL)
	            (CLASS ?c) (CONSTRAINT ?constraint) (sem ?sem) (transform ?transform)))
             (SORT PRED) (VAR ?v)
             (BARE-NP +) (name-or-bare ?nob)
	     (simple +) (mass ?mass) ; amount of Ras (mass)
	     )
         -protein-name-constructor> 0.995
         (head (N (SORT PRED) ;(MASS  count)
		  (gerund -) (complex -) 
		   (name-or-bare ?nob) (lex ?lex)
		   (derived-from-name -)  ;; names already can become NPs by simpler derivations
		   (AGR 3s) (VAR ?v)
		   (LF (? c ONT::GENE-PROTEIN)) ;;(CLASS (? !c ONT::REFERENTIAL-SEM))
		   (RESTR ?r) (rate-activity-nom -) (agent-nom -)
		(sem ?sem) (transform ?transform) (headless -) ; exclude missing-heads
		(sem ($ (? x F::PHYS-OBJ) (F::KR-TYPE ?kr)))
		))
	 (unify (pattern ?!xx) (value ?kr))  ;; we do this because checking this in the SEM, even though it fails, would be ignored!
	 (add-to-conjunct (val (:name-of ?lex)) (old ?r) (new ?constraint)))
))

;;(cl:setq *grammar-NP*
(parser::augment-grammar 
      '((headfeatures
         (NP CASE MASS NAME agr SEM PRO CLASS Changeagr GAP ARGUMENT argument-map SUBCAT role lex orig-lex headcat transform postadvbl refl gerund abbrev derived-from-name
	  subj dobj subcat-map comp3-map))

; new plural treatment proposed by James May 10 2010
;Basically, we'd have two new quantifiers, say, THE-SET and INDEF-SET,
;and these LF expressions can have a new special role (size or card or whatever
;we call it), attached at the top level.
;
;My guess is that we could eliminate all the plural rules in the grammar and
;slightly generalize the sing version so they handle all the cases. Presumably we'd
;introduce a new sense of THE (of the 3p), and maybe some others.like SOME,
;although I think that's already done.

	;; count and mass NPs, singular and plural:
	;; TEST: a dog, the dog, the dogs, some water, the five dogs, what dog


        ((NP (LF (% description (STATUS ?newspec) (VAR ?v)   ;;(SORT individual)
		    (CLASS ?c) (CONSTRAINT ?con1)
		    (sem ?sem)  (transform ?transform) 
		    ))
             (SORT PRED) (VAR ?v) (CASE (? case SUB OBJ)) (complex ?complex)
	    (wh ?w) (wh-var ?whv)
	  )
         -np-indv> 1.0    ;; because determiners are such a closed class, they provide strong evidence for an NP - hence the 1.0 to help with large search spaces
         (SPEC (LF ?spec) (ARG ?v) (mass ?m) ;;(POSS -)
	       (wh ?w) (WH-VAR ?whv) 
	       (agr ?agr) (RESTR ?spec-restr) (NOSIMPLE -))   ;; NOSIMPLE prevents this rule for a few cases
         (head (N1 (VAR ?v) (SORT PRED) (CLASS ?c) (MASS ?m)
		   ;;(status ?spec)
		(KIND -) (agr ?agr) (RESTR ?r) (rate-activity-nom -)
		(agent-nom -)   ;;  this rule can't apply to agent nominalizations directly (they must modified first using rule -agentnom1>
		(sem ?sem) (transform ?transform) (complex ?complex) ; need complex for two-np-conjunct
		(subcat-map ?subcat-map) (subcat (% ?xx (var ?sc-var)))
		))
	 (recompute-spec (spec ?spec) (agr ?agr) (result ?newspec))
	 (add-to-conjunct (val (?subcat-map ?sc-var)) (old ?r) (new ?newr))
	 (append-conjuncts (conj1 ?spec-restr) (conj2 ?newr) (new ?con1))
	 )
#||	
        ;; e.g., (two/some) more/less eggs
	((N1 (VAR ?v) (SORT PRED) (CLASS ?c) (MASS ?m)
	     (KIND -) (agr ?agr) (RESTR ?restr1) (rate-activity-nom -)
	     (agent-nom -)
	     (sem ?sem) (transform ?transform) (subcat ?subcat) (subcat-map ?smap) (lex ?lex)
             (CASE (? case SUB OBJ))
	  )
	 -n1-card-more> ;.97 ;.95 ;; prefer to attach to NP
	 (QUAN (COMPARATIVE +) (VAR ?av) (LF ?cmp) (QCOMP ?qcomp) (lex ?qlex))
	 (head (N1 (VAR ?v) (SORT PRED) (CLASS ?c) (MASS ?m)
		(KIND -) (agr ?agr) (RESTR ?r) (rate-activity-nom -)
		(agent-nom -)   ;;  this rule can't apply to agent nominalizations directly (they must modified first using rule -agentnom1>
		(sem ?sem) (transform ?transform) (subcat ?subcat) (subcat-map ?smap) (lex ?lex)
		))
	 (add-to-conjunct (val (MOD (% *PRO* (status ont::f) (class ?cmp) (VAR *) (lex ?qlex)
		       (constraint (& (figure ?v) (ground (% *PRO* (var **) (class ?c))) ))))) ;(val (quan ?cmp))
			  (old ?r) (new ?restr1))
	 )
||#
	
	;; quantifier with post N1 complement , e.g. more trains than that,
	
        ((NP (LF (% description (STATUS ?spec) (VAR *) ;(SORT set)   ;; use the N var as the new var
		    (CLASS ?c) (CONSTRAINT ?newr)
		    (sem ?sem)  (transform ?transform)
		    ))
             (SORT PRED) (VAR *) (CASE (? case SUB OBJ))
	     (WH ?w)  (WH-VAR ?whv));; must move WH feature up by hand here as it is explicitly specified in a daughter.
         -np-quan-X-comp>
         (SPEC (LF ?spec) (VAR ?specv)
	       ;; (ARG ?v)  
	  (WH-VAR ?whv)
	  (name-spec -) ;;(mass ?m)
	  (POSS -)
	  (WH ?w) (agr 3p) (NOSIMPLE -) (pred ?pred)
	  (QCOMP (% ?!cat (VAR ?psvar)))
	  (QCOMP ?qcomp))
         (head (N1 (VAR ?v) (SORT PRED) (CLASS ?c) (MASS ?m) 
		   (KIND -) (agr 3p) (RESTR ?r) (sem ?sem) (rate-activity-nom -) (agent-nom -)
		   (transform ?transform)
		))
	 ?qcomp ;;(PP (ptype W::THAN) (var ?psvar) (gap -))
	 (add-to-conjunct
	  (val (SIZE (% *PRO* (STATUS ONT::INDEFINITE)
			 (VAR **)
			 (CLASS ONT::NUMBER)
			 (CONSTRAINT
			  (& (MODS (% *PRO* (STATUS ONT::F)
				      (VAR ***)
				      (CLASS ?pred)
				      (CONSTRAINT (& (figure **) (GROUND ?psvar) (DIFF ?card))))))))))
	  (old ?r) (new ?newr))
	 )

	;; MASS NPs
        ;;  e.g., sand, sand in the corner
        ((NP (MASS (? xx MASS bare))
             (LF (% Description (STATUS ONT::BARE) (VAR ?v) (SORT STUFF) (sem ?sem)
	            (CLASS ?c) (CONSTRAINT ?newr)
		    (sem ?sem) (transform ?transform)
		    ))	      
             (SORT PRED) (VAR ?v) (simple ?x)

	  )
         -mass>
         (head (N1 (MASS (? xx MASS bare)) (AGR 3s) (VAR ?v) (CLASS ?c) (RESTR ?r) (sem ?sem)
		(transform ?transform) (post-subcat -) (simple ?x) (agent-nom -) (rate-activity-nom -)
		(derived-from-name -) ;; this feature is + only if we have a base N1 derived from a NAME (so no need to build a competing NP!)
		(subcat-map ?subcat-map) (subcat (% ?sc1 (var ?sc-var)))))
	  (add-to-conjunct (val (?subcat-map ?sc-var)) (old ?r) (new ?newr)))
        
	;; ANOTHER seems distinct in its behavior so we do it in some grammar rules

	((NP (LF (% description (status ont::indefinite) (VAR ?v)   ;;(SORT individual)
		    (CLASS ?c) (CONSTRAINT ?con1)
		    (sem ?sem)  (transform ?transform)
		    ))
	  (SORT PRED) (VAR ?v) (CASE (? case SUB OBJ))
	  (wh -) (wh-var -);; must move WH feature up by hand here as it is explicitly specified in a daughter
	  )
         -np-another> 1.0   
	 (word (lex w::another))
         (head (N1 (VAR ?v) (SORT PRED) (CLASS ?c) (MASS count)
		   (KIND -) (agr ?agr) (RESTR ?r) (rate-activity-nom -) (agent-nom -)
		   (sem ?sem) (transform ?transform)
		   ))
	 ;;(add-to-conjunct (val (SIZE ?card)) (old ?setr) (new ?setr1))
	 (add-to-conjunct 
	  (val (MOD (% *PRO* (status ont::f) (class ont::OTHER) (VAR *)
		       (constraint (& (figure ?v) (ground (% *PRO* (var **) (class ?c))) )))))
	  (old ?r) (new ?con1)))
	 
	
        ;; QUANTITY PHRASES

        ;;  UNIT NP PHRASES

	  ;; the / those pounds
	  ((NP (LF (% description (STATUS ?speclf) (VAR ?v) 
		      (CLASS ont::quantity) (CONSTRAINT ?constr) (argument ?argument)
		      (sem ?sem)  (transform ?transform) (unit-spec +)
		      ))
	       (spec ont::definite) (class ?c) (VAR ?v) (SORT unit-measure) (WH ?w)  (WH-VAR ?whv))
	   -unit-np>  .98    ;; should the NP produced only be used as a SPEC? (possible headless?)
	   (SPEC (LF (? speclf ont::indefinite ont::indefinite-plural ont::definite ont::definite-plural)) ;; only articles in this rule -- no quans
	    (VAR ?specv)
	    (ARG ?v)  
	    (WH-VAR ?whv) (RESTR ?restr)
	    (name-spec -) (mass ?m)
	    (POSS -)
	    (WH ?w) (agr ?agr) ;;(restr (& (quan -)))   - doesn't match THE or A!!!
	    (NOSIMPLE -)
	       )
	   (head (N1 (VAR ?v) (SORT unit-measure) (INDEF-ONLY -) (CLASS ?c) (MASS ?m)
		     (KIND -) (agr ?agr) (sem ?sem) (sem ($ f::abstr-obj (f::scale ?sc)))
		     (argument ?argument) (RESTR ?restr1) (transform ?transform) (post-subcat -) (rate-activity-nom -) (agent-nom -)
		     ))
	   (append-conjuncts (conj1 ?rest1) (conj2 ?restr) (new ?restr2))
	   (add-to-conjunct (val (& (unit ?c) (scale ?sc))) (old ?restr2) (new ?constr))
	   )
	  
	;; several/many pounds
	  ((NP (LF (% description (STATUS ONT::INDEFINITE) (VAR ?v)
		      (CLASS ont::quantity) (CONSTRAINT ?constr) (argument ?argument)
		      (sem ?sem)  (transform ?transform) (unit-spec +)
		      ))
	       (SPEC ont::indefinite) (VAR ?v) (SORT unit-measure) (WH ?w))
	   -unit-quan-card-np>
	   (SPEC (LF ?spec) (ARG ?v) (name-spec -) (mass count)
	    (POSS -) (WH ?w) (agr ?agr)
	    (cardinality +) (QUAN -) ;; cardinality quantifiers only -- others use unit-np-quan
	    (RESTR (& (SIZE ?amt))) (NOSIMPLE -))
	   (head (N1 (VAR ?v) (SORT unit-measure) (INDEF-ONLY -) (CLASS ?c) (MASS ?m)
		     (KIND -) (agr ?agr) (sem ?sem) (sem ($ f::abstr-obj (f::scale ?sc)))
		     (argument ?argument) (RESTR ?restr) (transform ?transform) (post-subcat -)
		     ))
	   ;;   (append-conjuncts (conj1 ?restr) (conj2 ?r) (new ?constr1))
	   (add-to-conjunct (val (& (amount ?amt) (unit ?c) (scale ?sc))) (old ?restr) (new ?constr))
	   )

	;; quantified unit nps: every three gallons
        ((NP (LF (% description (STATUS ont::quantifier) (VAR ?v) (SORT PRED)   ;; use the N var as the new var
                    (CLASS ONT::quantity) (CONSTRAINT ?constr) 
                    (Sem ?sem)
                    ))
             (SORT PRED) (VAR ?v) (CASE (? case SUB OBJ))
             (WH ?w)  (WH-VAR ?whv));; must move WH feature up by hand here as it is explicitly specified in a daughter.
         -np-quan-plur-units>
         (SPEC (LF ?spec) (VAR ?specv)
               (ARG *)  
               (WH-VAR ?whv)
               (name-spec -) (mass ?m) 
               (POSS -)
               (WH ?w) (agr ?agr) (RESTR ?r) (NOSIMPLE -) ;(nobarespec +)
               (RESTR (& (QUAN ?!quan)))  ;; spec must be a quantifier
               )   
         (head (N1 (VAR ?v) (SORT unit-measure) (CLASS ?c) (MASS ?m) 
                   (KIND -) (agr ?agr) (RESTR ?restr) (sem ?sem)  (sem ($ f::abstr-obj (f::scale ?sc)))
		   (argument ?argument)
                 (post-subcat -)
                ))
	 (append-conjuncts (conj1 ?restr) (conj2 ?r) (new ?constr1))
         (add-to-conjunct (val (& (unit ?c) (scale ?sc))) (old ?constr1) (new ?constr))
         )	
	
       
        ;;  NP with SPECS that subcategorize for NP's
        ;;   all/both/half the boys
        
        ((NP (LF (% description (STATUS ?spec) (VAR ?specvar) (CLASS ?c) (constraint ?r1) ;(CONSTRAINT ?newr) ; don't pass up ?r from the NP so that the mods stay with the refset.  We need this to be the case for -can-indirect-request-b2> in IMRules (e.g., Can you tell me all the cats chased by the dog?)
		    (sem ?sem)  (transform ?transform)
		    ))
             (SORT PRED) (VAR ?specvar) (WH ?w));; must move WH feature up by hand here as it is explicitly specified in a daughter.
         -np-spec-npplural>
         (SPEC (LF ?spec) (ARG ?v) (name-spec -) (mass ?m) (POSS -) (NPMOD +) (var ?specvar)
               (WH ?w) (agr ?agr) (RESTR ?restr) (SUBCAT (% ?n (sem ?subcatsem))))
	 (head (NP  (VAR ?v) (MASS ?m) 
		    (KIND -) (agr ?agr) 
                    (LF (% DESCRIPTION (CLASS ?c) (STATUS ONT::DEFINITE-plural) 
			   (CONSTRAINT ?r) (sem ?sem)
		           (transform ?transform)
		           ))))
	 (add-to-conjunct (val (refset ?v)) (old ?restr) (new ?r1))
	 ;(append-conjuncts (conj1 ?r1) (conj2 ?r) (new ?newr))
	 )

	((NP (LF (% description (STATUS ?spec) (VAR ?specvar) (CLASS ?c) (constraint ?r1) ;(CONSTRAINT ?newr)
		    (sem ?sem)  (transform ?transform)
		    ))
             (SORT PRED) (VAR ?specvar) (WH ?w));; must move WH feature up by hand here as it is explicitly specified in a daughter.
         -np-spec-npmass>
         (SPEC (LF ?spec) (ARG ?v) (name-spec -) (MASS MASS) (POSS -) (NPMOD +) (var ?specvar)
               (WH ?w) (agr ?agr) (RESTR ?restr) (SUBCAT (% ?n (sem ?subcatsem))))
	 (head (NP  (VAR ?v)
		    (KIND -)  (MASS MASS)
                    (LF (% DESCRIPTION (CLASS ?c) (STATUS ONT::DEFINITE) 
			   (CONSTRAINT ?r) (sem ?sem)
		           (transform ?transform)
		           ))))
	 (add-to-conjunct (val (refset ?v)) (old ?restr) (new ?r1))
	 ;(append-conjuncts (conj1 ?r1) (conj2 ?r) (new ?newr))
	 )

        ;;  BARE PLURALS  ---> KINDS

	;;  bare plural count; the set-restr can be a cardinality
	;; TEST: dogs, five dogs
        ((NP (var ?v) (LF (% Description (STATUS ONT::INDEFINITE-PLURAL)
;			     (constraint (& ?setr))
			     (CONSTRAINT ?newr) (sem ?sem)
			     (VAR ?v) (CLASS ?c)))
	     (simple +)
	     (sem ?sem) (transform ?transform)
             (SORT PRED))
         -bare-plural-count> 
         ;; Myrosia 10/13/03 added a possibility of (mass bare) -- e.g. for "lunches" undergoing this rule
         (head (N1 (SORT PRED) (mass (? mass count bare)) (mass ?m)
		   (AGR 3p) (VAR ?v) (CLASS ?c) (RESTR ?r) (rate-activity-nom -) (agent-nom -)
		   (sem ?sem) (transform ?transform)
		   (subcat-map ?subcat-map)
		   (subcat (% ?xx (var ?sc-var)))
		   (post-subcat -)
		   ))
	  
	 (add-to-conjunct (old ?r) (val (?subcat-map ?sc-var)) (new ?newr))   ;; we add the subcat to the LF in cases its added later
	 )
		
	;;  bare plural substance unit nouns are indefinite measures
	;;  gallons, bunches, ...

        ((NP (LF (% Description (status ont::indefinite) (VAR ?v)
	            (CLASS ont::quantity) (CONSTRAINT ?newr)
                    (sem ?sem)))
             (SORT AGGREGATE-UNIT) (SPEC ont::INDEFINITE) (VAR ?v))
         -bare-measure-count> .98
         (head (N1 (SORT AGGREGATE-UNIT) (mass count) (mass ?m)
		(AGR 3p) (VAR ?v) (CLASS ?c) (RESTR ?r) (rate-activity-nom -) (agent-nom -)
		(sem ?sem)  (sem ($ f::abstr-obj (f::scale ?sc)))
		(post-subcat -)
		))
	 (add-to-conjunct (val (& (amount PLURAL) (unit ?c) (scale ?sc))) (old ?r) (new ?newr)))
  ;       (add-to-conjunct (val (:QUANTITY PLURAL)) (old ?r) (new ?newr)))

	;;  bare plural attribute unit nouns are indefinite measures
        ;;  length (inches, miles), degrees ...
	;; (ONT::A V451423 ONT::QUANTITY  :UNIT (:* ONT::LENGTH-UNIT W::METER) :QUANTITY  W::PLURAL :SCALE ONT::LENGTH-SCALE)
	   ((NP (LF (% Description (status ont::indefinite) (VAR ?v)
	            (CLASS ont::quantity) (CONSTRAINT ?newr)
                    (sem ?sem)))
             (SORT UNIT-MEASURE) (SPEC ont::INDEFINITE) (BARE +) (VAR ?v))
	    -bare-measure-attribute> .98
	    (head (N1 (SORT ATTRIBUTE-UNIT) (mass count) (mass ?m) (abbrev -) ;; don't allow bare form with abbreviations
		      (AGR 3p) (VAR ?v) (CLASS ?c) (RESTR ?r) (rate-activity-nom -) (agent-nom -)
		      (sem ?sem)  (sem ($ f::abstr-obj (f::scale ?sc)))
		      (post-subcat -) (abbrev -)
		      ))
	    (add-to-conjunct (val (& (:amount PLURAL) (unit ?c) (scale ?sc))) (old ?r) (new ?newr)))
	   
        ;;  bare plural mass nouns get SORT STUFF    ;;;  I don't think this is right JFA 12/02  "waters" is a count, isn't it

        ((NP (LF (% Description (STATUS ONT::INDEFINITE) (VAR ?v) (SORT STUFF) 
	            (CLASS ?c) (CONSTRAINT ?r) (sem ?sem) (transform ?transform)))
             (SORT PRED) (VAR ?v)
             )
         -bare-plural-mass> .988   ;; just a hair less than the COUNT for cases where ther 
         (head (N1 (SORT PRED) (mass mass) (MASS ?m) 
		(AGR 3p) (VAR ?v) (CLASS ?c) (RESTR ?r) (rate-activity-nom -) (agent-nom -)
		(sem ?sem) (transform ?transform)
		(post-subcat -)
		)))

        ;;  Bare singular - rare forms/telegraphic speech e.g., status report.
	;;  Also used for N1 conjunction "the truck and train"
        ((NP (LF (% Description (STATUS ONT::BARE) (VAR ?v) (SORT INDIVIDUAL)
	            (CLASS ?c) (CONSTRAINT ?newr) (sem ?sem) (transform ?transform)))
					;(SORT PRED)
	  (VAR ?v) (SORT ?!sort)
	  (BARE-NP +) (name-or-bare ?nob)
	  (simple +)
	  )
         -bare-singular> .98
         (head (N1 (SORT (? !sort substance-unit)) (MASS  count) (gerund -) ;;(complex -) 
		   (name-or-bare ?nob) 
		   (derived-from-name -)  ;; names already can become NPs by simpler derivations
		   (AGR 3s) (VAR ?v) (CLASS ?c) (RESTR ?r) (rate-activity-nom -) (agent-nom -)
		   (sem ?sem) (transform ?transform)
		(subcat-map ?subcat-map) (subcat (% ?xx (var ?sc-var))
		)))
	 (add-to-conjunct (old ?r) (val (?subcat-map ?sc-var)) (new ?newr)))

	#|
        ;;  COMMAS
        ;;  e.g., the train ,
        ((NP (LF ?r) (SORT ?sort) (VAR ?v) (comma +))
         -np-comma>
         (head (NP (LF ?r) (SORT ?sort) (VAR ?v) (COMPLEX -))) (punc (Lex w::punc-comma)))
	|#
	
	;; reference/citations
	((NP (LF (% description (STATUS ?spec) (VAR ?v) 
		    (CLASS ?c) (CONSTRAINT ?newc)
		    (sem ?sem)  (transform ?transform)
		    ))
             (SORT PRED) (VAR ?v) (paren +)
	    (wh ?w) (wh-var ?whv);; must move WH feature up by hand here as it is explicitly specified in a daughter
	     )
         -np-parenthetical> 1
	 (head (NP (paren -)
		   (LF (% description (STATUS ?spec) (VAR ?v)
			  (CLASS ?c) (CONSTRAINT ?constr)
			  (sem ?sem)  (transform ?transform)
			  ))
		   (SORT PRED) (VAR ?v)
		   (wh ?w) (wh-var ?whv);; must move WH feature up by hand here as it is explicitly specified in a dauhter
		   ))
	 (parenthetical (var ?pv) (arg ?v) (role ?role))
	 (add-to-conjunct (val (parenthetical ?pv)) (old ?constr) (new ?newc)))

#||	((parenthetical (var ?cc) (arg ?arg) (role :identified-as))
	 -paren-np> 1
	 (punc (lex (? x W::START-SQUARE-PAREN W::START-PAREN w::punc-comma)))
	 (head (np (var ?cc)))
	 (punc (lex  (? y W::END-SQUARE-PAREN W::END-PAREN w::punc-comma))))||#

	((parenthetical (var ?cc) (arg ?arg) (role :parenthetical))
	 -paren1> 1
	 (punc (lex (? x W::START-SQUARE-PAREN W::START-PAREN w::punc-comma)))
	 (head (Utt (LF (% W::SPEECHACT (constraint (& (content ?cc))))) (headcat (? !hc w::name))))  ; for names use appositive rules
	 (punc (lex  (? y W::END-SQUARE-PAREN W::END-PAREN w::punc-comma))))

	((parenthetical (var ?cc) (arg ?arg) (role :parenthetical))
	 -paren2> 1
	 (punc (lex (? x W::START-SQUARE-PAREN W::START-PAREN W::punc-COMMA)))
	 (head (pred (var ?cc) (arg ?arg)))
	 (punc (lex  (? y W::END-SQUARE-PAREN W::END-PAREN w::punc-comma))))
	 
	 
        ;; NP -> WH-PRO
        ;;  e.g., who, what, ...
        
        ((NP (SORT PRED)
             (VAR ?v) 
	     (sem ?sem)
	     (lex ?lex) (WH Q) (WH-VAR ?v)
             (LF (% Description (status ?newspec) (var ?v) (Class ?s) (SORT (?agr -))
	            (Lex ?lex) (sem ?sem) (transform ?transform)
		    (constraint (& (proform ?lex)))
		    )))
         -wh-pro1> .995
         (head (pro (PP-WORD -) (AGR ?agr) (LEX ?lex) (LF ?s)
		    (sem ?sem) (transform ?transform)
	            (VAR ?v) (WH Q)))    ;; removed R as NP
	 (recompute-spec (spec ONT::WH) (agr ?agr) (result ?newspec))
	 )
	
	
        ))

(parser::augment-grammar 
      '((headfeatures
         (NP CASE MASS NAME agr SEM PRO Changeagr GAP ARGUMENT argument-map SUBCAT role lex orig-lex headcat transform postadvbl refl gerund abbrev derived-from-name
	  subj dobj subcat-map comp3-map)) ; CLASS not headfeatures

	;; Some units can appear before the number, 
	; $10
	((NP (LF (% description (STATUS ONT::INDEFINITE)
		    (VAR ?v) (SORT unit-measure)
		    (CLASS ONT::quantity)
		    (CONSTRAINT ?constr) (argument ?argument)
		    (sem ?sem) 
		    ))
	     (class ont::quantity) 
	      (SPEC ont::INDEFINITE) (unit-spec +) (VAR ?v) (SORT unit-measure))
         -pre-unit-np-number-indef>
	  (head (N (VAR ?v) (SORT attribute-unit) (Allow-before +) (LF ?unit)
		   (KIND -) (agr ?agr) (sem ?sem) (sem ($ f::abstr-obj (f::type ont::unit)
							  (f::scale ?sc)))
		   (argument ?argument) (RESTR ?restr)
		   (post-subcat -)
		   ))
	 (NUMBER (val ?num) (VAR ?nv) (AGR ?agr) (restr ?r))
	 (append-conjuncts (conj1 ?restr) (conj2 ?r) (new ?constr1))
	 (add-to-conjunct (val (& (amount ?num) (unit ?unit) (scale ?sc))) (old ?constr1) (new ?constr))
	 )
	))

	
;;  special rule for another as a bare NP  
;;  veentually I hope to replace this when we extend the capability of Lex definitions
(parser::augment-grammar 
 '((headfeatures (noop noop))
   	((NP (LF (% description (status ont::indefinite) (VAR *)   ;;(SORT individual)
		    (CLASS ONT::REFERENTIAL-SEM) 
		    (CONSTRAINT (& (MOD (% *PRO* (status ont::f) (class ont::OTHER) (VAR **) (constraint (& (figure *) (ground (% *PRO* (var ***) (class ONT::REFERENTIAL-SEM))) ))))))
		    (sem ($ ?s (f::type ONT::REFERENTIAL-SEM))) ;(sem ?sem)
		    (transform ?transform)
		    ))
	  (sem ($ ?s (f::type ONT::REFERENTIAL-SEM))) ;(sem ?sem)
	  (SORT PRED) (VAR *) (CASE (? case SUB OBJ))
	  (wh -) (wh-var -)
	  )
         -np-another-bare>  .97
	 (head (word (lex w::another) 
	 )))))


(parser::augment-grammar 
 '((headfeatures
    (NP SEM ARGUMENT SUBCAT role lex orig-lex headcat transform postadvbl refl abbrev))
    
	;;  five pounds, thirty feet  -- because of the sort UNIT-MEASURE, these generally end up a specifiers, not main NPS
	((NP (LF (% description (STATUS ONT::INDEFINITE)
		    (VAR ?v)
		    (SORT unit-measure) 
		    (CLASS ONT::quantity)
		    (CONSTRAINT ?constr) (argument ?argument)
		    (sem ?sem) 
		    ))
	  (class ont::quantity)
	  (SPEC ont::INDEFINITE) (AGR 3s) (unit-spec +) (VAR ?v) (SORT unit-measure))
         -unit-np-number-indef>
	 (NUMBER (val ?num) (VAR ?nv) (AGR ?agr) (restr ?r))
 	 (head (N1 (VAR ?v) (SORT unit-measure) (INDEF-ONLY -) (CLASS ?c) (MASS ?m)
		   (KIND -) (sem ?sem) (sem ($ f::abstr-obj  (f::type (? xx ont::unit ont::multiple)) (f::scale ?sc))) ; ont::multiple: increase 100 fold
		   (argument ?argument) (RESTR ?restr)
		   (post-subcat -)
		))
         (add-to-conjunct (val (& (value ?num))) (old ?r) (new ?newr))
	 (add-to-conjunct (val (& (amount (% *PRO* (status ont::indefinite) (class ont::NUMBER) (VAR ?nv) (constraint ?newr)))
				  (unit ?c)
				  (scale ?sc))) (old ?restr) (new ?constr))
	 )

  
   ;;  special case: "a mile"

	((NP (LF (% description (STATUS ONT::INDEFINITE)
		    (VAR ?v)
		    (SORT unit-measure) 
		    (CLASS ONT::quantity)
		    (CONSTRAINT ?constr) (argument ?argument)
		    (sem ?sem) 
		    ))
	  (class ont::quantity)
	  (Mass count)
	  (SPEC ont::INDEFINITE) (AGR 3s) (unit-spec +) (VAR ?v) (SORT unit-measure))
         -unit-np-number-indef-special-case>
	 (ART (VAR ?nv) (LEX w::a) )
 	 (head (N1 (VAR ?v) (SORT unit-measure) (INDEF-ONLY -) (CLASS ?c) (MASS ?m)
		   (KIND -) (sem ?sem) (sem ($ f::abstr-obj  (f::scale ?sc)))
		   (argument ?argument) (RESTR ?restr)
		   (post-subcat -)
		))
         (add-to-conjunct (val (& (value 1))) (old ?r) (new ?newr))
	 (add-to-conjunct (val (& (amount (% *PRO* (status ont::indefinite) (class ont::NUMBER) (VAR ?nv) (constraint ?newr)))
				  (unit ?c)
				  (scale ?sc))) (old ?restr) (new ?constr))
	 )


   ;;  thirty feet in height 
	((NP (LF (% description (STATUS ONT::INDEFINITE)
		    (VAR ?v) (SORT unit-measure) 
		    (CLASS ONT::quantity)
		    (CONSTRAINT ?constr) (argument ?argument)
		    (sem ?sem) 
		    ))
	  (class ont::quantity)
	  (SPEC ont::INDEFINITE) (AGR 3s) (unit-spec +) (VAR ?v) (SORT unit-measure))
         -unit-np-number-indef-explicit-scale>
	 (NUMBER (val ?num) (VAR ?nv) (AGR ?agr) (restr ?r))
 	 (head (N1 (VAR ?v) (SORT unit-measure) (INDEF-ONLY -) (CLASS ?c) (MASS ?m)
		   (KIND -) (sem ?sem) (sem ($ f::abstr-obj  (f::scale ?!unit-sc)))
		   (argument ?argument) (RESTR ?restr)
		   (post-subcat -)
		))
	 (word (lex in))
	 (NP (gap -) (class (? !c2 ont::number)) (sem ($ f::abstr-obj (F::type (? xx ont::domain)) (f::scale ?!unit-sc))))
	 ;(pp (ptype in) (gap -)
	  ;(sem ($ f::abstr-obj (F::type (? xx ont::domain)) (f::scale ?!sc2))))

	 ;;(class-greatest-lower-bound (in1 ?unit-sc) (in2 ?explicit-sc) (out ?sc))
         (add-to-conjunct (val (& (value ?num))) (old ?r) (new ?newr))
	 (add-to-conjunct (val (& (amount (% *PRO* (status ont::indefinite) (class ont::NUMBER) (VAR ?nv) (constraint ?newr)))
				  (unit ?c)
				  (scale ?!unit-sc)))
	  (old ?restr) (new ?constr))
	 )

   ;;  NP with SPECS that subcategorize for "of" PP's that are count and definite
   ;;    all of the boys, a bunch of the people, most of the trucks, some of them, which of/among the cats
   ;;  Its has its own set of head features because the AGR feature comes from the SPEC, not the head
   
	((NP (LF (% description (status ?newspec) ;(STATUS ?spec)
		    (VAR *) (CLASS ?c) (CONSTRAINT ?newr)
                (sem ?sem)  (transform ?transform) 
                ))
	 (case ?case)
         (SORT PRED) (AGR ?agr)
	 (MASS count)
	 (VAR *) (WH ?w) (wh-var *));; must move WH feature up by hand here as it is explicitly specified in a daughter.
     -np-spec-of-count-def-pp>
     (SPEC (LF ?spec) (ARG ?v) (VAR ?specvar) (name-spec -) (mass count) 
      (POSS -) (AGR ?agr)
      (WH ?w) (wh-var ?whvar)
      (RESTR ?restr)
      (SUBCAT (% PP (Ptype ?ptp) (agr |3P|) (SEM ?sem))))
     (head 
      (PP  (VAR ?v) (MASS count) (ptype ?ptp)
       (KIND -) (AGR |3P|) (GAP -)
       (LF (% DESCRIPTION (CLASS ?c)
	      (status (? st ONT::definite-plural)) ; allows "dozens of *the* rotten eggs but excludes "dozens of rotten eggs" (the latter uses -NP-SPEC-QUANTITY-OF-DEF-PP> )
	      (sem ?sem) (transform ?transform) (constraint ?con)
	      ))))
     (recompute-spec (spec ?spec) (agr 3p) (result ?newspec))
     (append-conjuncts (conj1 (& (REFSET ?v))) ;;(quan ?card)
		       (conj2 ?restr) (new ?newr)))
     

   ;;  NP with SPECS that subcategorize for "of" PP's that are definite singular/mass
   ;; all of the water, most of the truck
   ((NP (LF (% description (STATUS ?spec) (VAR *) (CLASS ?c) (CONSTRAINT ?newr)
	       (sem ?sem)  (transform ?transform) 
                ))
     (case ?case)
     (SORT PRED)
     (MASS mass) (agr 3s)
     (VAR *) (WH ?w) (wh-var *));; must move WH feature up by hand here as it is explicitly specified in a daughter.
     -np-spec-of-def-sing-pp>
    (SPEC (LF ?spec) (ARG ?v) (VAR ?specvar) (name-spec -) (mass mass) (POSS -);;myrosia 12/27/01 added mass restriction to spec
     (WH ?w)
     (RESTR ?restr)
     (SUBCAT (% PP (Ptype ?ptp) (agr |3S|) (SEM ?sem))))
    (head 
     (PP  (VAR ?v) (MASS ?mass) (ptype ?ptp)
	  (KIND -) (GAP -) (agr |3S|)
	  (LF (% DESCRIPTION (CLASS ?c) (sem ?sem) 
		 (transform ?transform) (status (? xx ont::definite ont::pro)))
		  )))
    
    (append-conjuncts (conj1 (& (REFOBJECT ?v) (size ?card))) (conj2 ?restr) (new ?newr))
    )

   ;;  NP with SPECS that subcategorize for "of" PP's that are plural
   ;; 25% of the trucks, most of the people
   ((NP (LF (% description (status ?newspec) ;(status ont::indefinite-plural)
	       (VAR *) (CLASS ?c) (CONSTRAINT ?newr)
	       (sem ?sem)  (transform ?transform) 
                ))
     (case ?case) (agr (? agr 3s 3p)) ; none/which of the trucks *is* red
     (SORT PRED)
     (MASS count)
     (VAR *) (WH ?w) (wh-var *));; must move WH feature up by hand here as it is explicitly specified in a daughter. ; ?wh-var is set to the arg, which is "the trucks" for "which of the trucks".  We want * here ("which")
     -np-spec-of-def-plur-pp>
    (SPEC (LF ?spec) (ARG ?v) (VAR ?specvar) (name-spec -) (POSS -);;myrosia 12/27/01 added mass restriction to spec
     (WH ?w) (mass count)
     (RESTR ?restr)
     (SUBCAT (% PP (Ptype ?ptp) (agr |3P|) (SEM ?sem))))
    (head 
     (PP  (VAR ?v) (mass count) (ptype ?ptp)
	  (KIND -) (GAP -) (agr 3p)
	  (LF (% DESCRIPTION (CLASS ?c) (sem ?sem) 
		 (transform ?transform) (status (? xx ont::definite-plural ont::pro-set)))
		  )))
    
    (append-conjuncts (conj1 (& (REFSET ?v) (size ?card))) (conj2 ?restr) (new ?newr))
    (recompute-spec (spec ?spec) (agr 3p) (result ?newspec))

    )
#||
   ;;  NP with SPECS that subcategorize for "of" PP's that are plural
   ;; which of the cats is/are red?
   ((NP (LF (% description (status WH) ;(status ont::indefinite-plural)
	       (VAR *) (CLASS ?c) (CONSTRAINT ?newr)
	       (sem ?sem)  (transform ?transform) 
                ))
     (case ?case) (agr (? agr2 3s 3p))
     (SORT PRED)
     (MASS count)
     (VAR *) (WH Q) (wh-var *)) ; wh-var set to the new var ;; must move WH feature up by hand here as it is explicitly specified in a daughter.
     -np-spec-of-def-plur-pp-which>
    (SPEC (LF ?spec) (ARG ?v) (VAR ?specvar) (name-spec -) (POSS -);;myrosia 12/27/01 added mass restriction to spec
     (WH Q) (wh-var ?whvar)
     (RESTR ?restr)
     ;(SUBCAT (% PP (Ptype ?ptp) (agr |3S|) (SEM ?sem)))
     )
    (head 
     (PP  (VAR ?v) (mass count) (ptype w::of)
	  (KIND -) (GAP -) (agr 3p)
	  (LF (% DESCRIPTION (CLASS ?c) (sem ?sem) 
		 (transform ?transform) (status (? xx ont::definite-plural ont::pro-set))) 
		  )))
    
    (append-conjuncts (conj1 (& (REFOBJECT ?v) (size ?card))) (conj2 ?restr) (new ?newr))
    )
||#
   ;;  NP with SPECS that subcategorize for "of" PP's that are plural
   ;; Of the cats that is/are red, which ate the mouse?

   #|
   ((NP (LF (% description (status WH) ;(status ont::indefinite-plural)
	       (VAR *) (CLASS ?c) (CONSTRAINT ?newr)
	       (sem ?sem)  (transform ?transform) 
                ))
     (case ?case) (agr (? agr2 3s 3p))
     (SORT PRED)
     (MASS count)
     (VAR *) (WH Q) (wh-var *)) ; wh-var set to the new var ;; must move WH feature up by hand here as it is explicitly specified in a daughter.
     -np-spec-of-def-plur-pp-which-rev>
    (head 
     (PP  (VAR ?v) (mass count) (ptype w::of)
	  (KIND -) (GAP -) (agr 3p)
	  (LF (% DESCRIPTION (CLASS ?c) (sem ?sem) 
		 (transform ?transform) (status (? xx ont::definite-plural ont::pro-set)))
		  )))
    (SPEC (LF ?spec) (ARG ?v) (VAR ?specvar) (name-spec -) (POSS -);;myrosia 12/27/01 added mass restriction to spec
     (WH Q) (wh-var ?whvar)
     (RESTR ?restr)
     ;(SUBCAT (% PP (Ptype ?ptp) (agr |3S|) (SEM ?sem)))
     )
    
    (append-conjuncts (conj1 (& (REFOBJECT ?v) (size ?card))) (conj2 ?restr) (new ?newr))
    )
   |#

   
   ;; Of/Among the cats that is/are red, which/some/all ate the mouse
	((NP (LF (% description (status ?newspec) ;(STATUS ?spec)
		    (VAR *) (CLASS ?c) (CONSTRAINT ?newr)
                (sem ?sem)  (transform ?transform) 
                ))
	 (case ?case)
         (SORT PRED) (AGR ?agr)
	 (MASS count)
	 (VAR *) (WH ?w) (wh-var *));; must move WH feature up by hand here as it is explicitly specified in a daughter.
     -np-spec-of-count-def-pp-rev>
     (head 
      (PP  (VAR ?v) (MASS count) (ptype ?ptp)
       (KIND -) (AGR |3P|) (GAP -)
       (LF (% DESCRIPTION (CLASS ?c)
	      (sem ?sem) (transform ?transform) (constraint ?con)
	      ))))
     (SPEC (LF ?spec) (ARG ?v) (VAR ?specvar) (name-spec -) (mass count) 
      (POSS -) (AGR ?agr)
      (WH ?w) (wh-var ?whvar)
      (RESTR ?restr)
      (SUBCAT (% PP (Ptype ?ptp) (agr |3P|) (SEM ?sem))))
     
     (recompute-spec (spec ?spec) (agr 3p) (result ?newspec))
     (append-conjuncts (conj1 (& (REFSET ?v))) ;;(quan ?card)
      (conj2 ?restr) (new ?newr)))


   ;;  CLASSIFIER Constructions
   ;;   a bunch of people, a set of numbers, a bunch of sand
      ((NP (LF (% description (STATUS ?spec) (VAR ?v) (CLASS ?c) (CONSTRAINT ?newr)
                (sem ?sem)  (transform ?transform) 
                ))
	 (case ?case)
         (SORT PRED) (AGR ?agr)
	 (MASS count)
	 (VAR ?v) (WH ?w));; must move WH feature up by hand here as it is explicitly specified in a daughter.
     -np-classifier-of-pp>
       (NP  (LF (% description (STATUS ?spec) (VAR ?v) (CLASS ?c) (CONSTRAINT ?restr)
                (sem ?sem)  (transform ?transform) 
                ))
	(case ?case)
	(SORT CLASSIFIER) (AGR ?agr)
	(MASS count)
	(VAR ?v) (WH ?w)
	(argument-map ?!argmap)
	(argument (% PP (ptype ?ptp) (agr ?ppagr) (mass ?mass))))
       (head 
	(PP  (VAR ?ppv) (MASS ?mass) (ptype ?ptp)
	     (KIND -) (AGR ?ppagr) (GAP -) 
	     ))
       (append-conjuncts (conj1 (& (?!argmap ?ppv))) (conj2 ?restr) (new ?newr))
       )
      
   ;;  NP with SPECS that subcategorize for "of" PP's that are mass
    ;; e.g., three gallons of water
      ((NP (LF (% description (STATUS ?spec) (var ?v) ;(VAR *)
		  (CLASS ?c) (CONSTRAINT ?newr)
	       (sem ?sem)  (transform ?transform) 
	       ))
     (case ?case)
     (SORT PRED)
     (MASS mass) (AGR ?agr)
     (var ?v) ;(VAR *)
     (WH ?w));; must move WH feature up by hand here as it is explicitly specified in a daughter.
    -np-spec-of-mass-indef-pp>
    (SPEC (LF ?spec) (ARG ?v) (VAR ?specvar) (name-spec -) (mass mass) 
     (POSS -)
     (WH ?w)
     (RESTR ?restr)
     (SUBCAT (% PP (Ptype ?ptp) (SEM ?sem))))
    (head 
     (PP  (VAR ?v) (MASS mass) 
	  (ptype ?ptp)
	  (KIND -) (GAP -)
	  (LF (% DESCRIPTION (CLASS ?c) (sem ?sem) (constraint ?constr)
		 (transform ?transform) (status (? st ont::bare ont::indefinite-plural))
		 ))))
                 
    (append-conjuncts (conj1 ?constr) (conj2 ?restr) (new ?newr))
    )

   ;;  NP with SPECS that subcategorize for "of" PP's that are plural
    ;; e.g., three gallons of beans
   ((NP (LF (% description (STATUS ?spec) (var ?v) ;(VAR *)
	       (CLASS ?c) (CONSTRAINT ?newr)
	       (sem ?sem)  (transform ?transform) 
	       ))
     (case ?case)
     (SORT PRED)
     (MASS count) (agr ?agr)
     (var ?v) ;(VAR *)
     (WH ?w));; must move WH feature up by hand here as it is explicitly specified in a daughter.
    -np-spec-quantity-of-def-pp>
    (SPEC (LF ?spec) (ARG ?v) (VAR ?specvar) (name-spec -)
     (POSS -)
     (WH ?w)
     (RESTR ?restr)
     (SUBCAT (% PP (Ptype ?ptp) (SEM ?sem))))
    (head 
     (PP  (VAR ?v) (MASS count) 
	  (ptype ?ptp)
	  (KIND -) (GAP -)
	  (LF (% DESCRIPTION (CLASS ?c) (sem ?sem) (constraint ?constr)
		 (transform ?transform) (status (? x ont::indefinite-plural)) ; definite-plural uses -np-spec-of-count-def-pp>
		 ))))
                 
    (append-conjuncts (conj1 ?constr) (conj2 ?restr) (new ?newr))
    )
   ))

(parser::augment-grammar 
  '((headfeatures
     (NP VAR SEM agr ARGUMENT SUBCAT role lex orig-lex headcat transform)
     (SPEC POSS POSS-VAR POSS-SEM comparative lex orig-lex transform headcat) ;; NObareSpec
     (QUANP CARDINALITY VAR AGR comparative headcat)
     (ADJP headcat lex orig-lex)
     )

    ;;  a four cycle engine, a two-trick pony, a one horse town, ...
    ((ADJP (ARG ?arg) (VAR *) (sem ?sem) (atype central) (comparative -) (argument ?aa)
      (LF (% PROP (CLASS ONT::ASSOC-WITH) (VAR *) 
	     (CONSTRAINT (& (FIGURE ?arg) 
			    (GROUND (% *PRO* (status ont::kind) (var ?nv) 
				    (CLASS ?c) (CONSTRAINT ?con)))))
				    
	     (Sem ?sem)))
      (transform ?transform))
     -adj-number-noun> .98    ;; this is very rare 
     (NUMBER  (val ?sz) (VAR ?nv) (restr -))
     (Gt (arg1 ?sz) (arg2 0))   ;; negative numbers don't work as cardinailty adjectives!
     (head (N1 (VAR ?v) (class ?c) ;(LF ?c) ; changed N to N1 because some N's need to change to pred (via N1-RELN1)
	       (Mass count) (sort PRED)  
	      (KIND -) (agr 3s) (one -) ;; don't allow "one" as the N!
	      (RESTR ?restr) (sem ($ (? ss  F::PHYS-OBJ F::SITUATION-ROOT  F::ABSTR-OBJ)))
	      (transform ?transform) (postadvbl -)
	      (post-subcat -) (gap -)
	      ))
    (add-to-conjunct (val (amount ?sz)) (old ?restr) (new ?con)))

      ;; version of adj-number-noun with units -- creates quantities, not sets
    ;; a 10 foot fence, 2 week vacation
    ((ADJP (ARG ?arg) (VAR *) (sem ?sem) (atype attributive-only) (comparative -)
	   (argument (% ?aa (sem ?argsem)))
      (SORT unit-measure)
      (LF (% PROP (CLASS ONT::ASSOC-WITH) (VAR *) 
	     (CONSTRAINT (& (FIGURE ?arg) 
			    (GROUND (% *PRO* (status ont::inDEFINITE) (var ?nv) 
				    (CLASS ont::quantity)
				    (CONSTRAINT ?constr)))))
	     (Sem ?sem)))					
      (transform ?transform))
     -adj-number-unit-modifier> .98
     (NUMBER  (val ?sz) (VAR ?nv) (restr -))
     (head (N1 (VAR ?v) (SORT unit-measure) (INDEF-ONLY -) (CLASS ?c) (MASS ?m)
	       (KIND -) ;;(agr 3s)   we allow either 61 year old or 61 years old
	       (sem ?sem)  (sem ($ f::abstr-obj (f::scale ?sc))) ;(sem ($ f::abstr-obj (f::scale ont::linear-d)))
	       (RESTR ?restr) (transform ?transform)
	       (postadvbl -) (post-subcat -)
	       (argument (% ?aa2 (sem ?argsem))) ; ?aa2 is a PP; not the same as ?aa in the LHS, which is an NP.  But they have the same sem
	       ))
     (add-to-conjunct (val (& (amount ?sz) (unit ?c))) (old ?restr) (new ?constr))
     )

    ;; and often has a hyphen
    ; two-step (pitch) interval, but not a two-step staircase
    ((ADJP (ARG ?arg) (VAR *) (sem ?sem) (atype attributive-only) (comparative -)
	   (argument (% ?aa (sem ?argsem)))
      (LF (% PROP (CLASS ONT::ASSOC-WITH) (VAR *) 
	     (CONSTRAINT (& (FIGURE ?arg) 
			    (GROUND (% *PRO* (status ont::inDEFINITE) (var ?nv) 
				    (CLASS ont::quantity)
				    (CONSTRAINT ?constr)))))
	     (Sem ?sem)))
      (SORT unit-measure)
      (transform ?transform))
     -adj-number-unit-modifier-hyphen> 0.98
     (NUMBER  (val ?sz) (VAR ?nv) (restr -))
     (Punc (lex W::punc-minus))
     (head (N1 (VAR ?v) (SORT unit-measure) (INDEF-ONLY -) (CLASS ?c) (MASS ?m) 
	       (KIND -) (agr 3s)
	       (sem ?sem)  (sem ($ f::abstr-obj (f::scale ?sc)))
	       (RESTR ?restr) (transform ?transform)
	       (postadvbl -) (post-subcat -)
	       (argument (% ?aa2 (sem ?argsem))) ; ?aa2 is a PP; not the same as ?aa in the LHS, which is an NP.  But they should have the same sem
	       ))
     (add-to-conjunct (val (& (amount ?sz) (unit ?c) (scale ?sc))) (old ?restr) (new ?constr))
     )

    ;; version of adj-number-noun with unit before number, e.g., (a) $ 10 (watch)
    ((ADJP (ARG ?arg) (VAR *) (sem ?sem) (atype ?atype) (comparative -) (argument ?aa)
      (SORT unit-measure)
      (LF (% PROP (CLASS ONT::ASSOC-WITH) (VAR *) 
	     (CONSTRAINT (& (FIGURE ?arg) 
			    (GROUND (% *PRO* (status ont::inDEFINITE) (var ?nv) 
				    (CLASS ont::quantity)
				    (CONSTRAINT ?constr)))))
	     (Sem ?sem)))					
      (transform ?transform))
     -adj-number-pre-unit-modifier>
      (head (N (VAR ?v) (SORT attribute-unit) (Allow-before +) (LF (:* ?x ?unit))
		   (KIND -) (agr ?agr) (sem ?sem) (sem ($ f::abstr-obj (f::scale ?sc)))
		   (argument ?argument) (RESTR ?restr)
		   (post-subcat -)
		   ))
     (NUMBER  (val ?sz) (var ?nv) (restr -))
     (add-to-conjunct (val (& (amount ?sz) (unit ?c) (scale ?sc))) (old ?restr) (new ?constr))
     )

      ;; turn unit NPs into specs
    ;;  e.g., a gallon (of water)
       
    ((SPEC (AGR ?agr)
      (VAR *) 
      (ARG ?arg) (lex ?lex) (LF ont::SM) (SUBCAT ?subcat) (Mass MASS)
      (unit-spec +)
      (restr (& (quantity ?unit-v)))) ;; mass nouns get QUANTITY in the restriction
     -spec-indef-unit-mass>
     (head (NP (sort unit-measure) (LF (% DESCRIPTION (status (? status ont::indefinite ont::indefinite-plural))))
	     (ARGUMENT ?subcat) (ARGUMENT (% ?xx (MASS MASS)))
	    (var ?unit-v) (lex ?unit-lex) )))

    ;;  e.g., the gallon (of water)
       
    ((SPEC (AGR ?agr)
      (VAR *) 
      (ARG ?arg) (lex ?lex) (LF ont::DEFINITE) (SUBCAT ?subcat) (Mass MASS)
      (unit-spec +)
      (restr (& (quantity ?unit-v)))) ;; mass nouns get QUANTITY in the restriction
     -spec-def-unit-mass>
     (head (NP (sort unit-measure) (LF (% DESCRIPTION (status (? status ont::definite ont::definite-plural))))
	     (ARGUMENT ?subcat) (ARGUMENT (% ?xx (MASS MASS)))
	    (var ?unit-v) (lex ?unit-lex) )))


    ;;  e.g., the three gallons, all three gallons, also each three gallons (quantifying over sets of quantities)

    ((SPEC (AGR 3s)
      (VAR *) 
      (ARG ?arg) (lex ?lex) (LF ?speclf) (SUBCAT ?subcat) (Mass MASS)
      ;; (NOSIMPLE +)  disabled to allow three gallons water.
      (unit-spec +)
      (restr ?new))
     -spec-quan-unit-mass>
     (spec (LF ?speclf) (agr ?agr) (complex -) (restr ?restr) (agr 3s))  ;; singular AGR as its a mass term we'll be constructing
     (head (NP (sort unit-measure)
	     (ARGUMENT ?subcat) (ARGUMENT (% ?xx (MASS MASS)))
	    (var ?unit-v) (lex ?unit-lex)))
     (add-to-conjunct (val (quantity ?unit-v)) (old ?restr) (new ?new)))

    ;; e.g., a bunch of trucks
    
    ((SPEC (SEM ?def) (AGR ?agr) (status ?spec)
      (VAR *) (card ?unit-v)     
      (ARG ?arg) (lex ?lex) (LF ?spec) (SUBCAT ?subcat) (Mass COUNT)
      (NOSIMPLE +) (unit-spec +)
      (restr (& (size ?unit-v)))) ;; count nouns get SIZE in the restriction
     -spec-unit-count>
     (head (NP (sort classifier)
	    (SPEC ?spec) (ARGUMENT ?subcat) (ARGUMENT (% ?xx (MASS COUNT) (AGR ?agr)))
	    (var ?unit-v) (lex ?unit-lex))))


     ;;  QUANTIFIERS
    
    ;; e.g., basic quantification with no-cardinality quantifiers: all trucks, no luck, every person ...
    ;; e.g., which cat
    ((SPEC (ARG ?arg) (VAR *) (agr ?agr) (MASS ?m) (LF ?status) (Nobarespec ?nbs)  (comparative ?cmp)
      (RESTR (& (QUAN ?s) (negation ?neg) )) (NoSimple ?ns) (npmod ?npm) (STATUS ?status)
      (SUBCAT ?qof) (QCOMP ?Qcomp) (PRED ?s)
      (wh ?wh) (wh-var ?arg)
      )
     -quan-simple-spec>
     (head (quan (CARDINALITY -) (SEM ?sem) (VAR ?v) (agr ?agr) (comparative ?cmp) (QOF ?qof) (QCOMP ?Qcomp)
		 (MASS ?m) (STATUS ?status) (Nobarespec ?nbs) (NoSimple ?ns) (npmod ?npm) (negation ?neg)
		 (LF ?s)
		 (wh ?wh) (wh-var ?wh-var) (comparative -) ; exclude "more"
		 )))

 #|| ;;  this is not right -- "more than 20 trucks" parses as a SPEC in a headless NP!
  ;; quantifier with complement , e.g. more than that, enough to weigh it down, ...
    ((SPEC (ARG ?arg) (VAR *) (MASS ?m) (agr ?agr) (LF W::indefinite-plural) (Nobarespec ?nbs)  (comparative ?cmp)
	   ;;(NoSimple +)
	   (npmod ?npm)
	   (SUBCAT ?qof) (QCOMP ?Qcomp) (PRED ?s)
	   (RESTR ?restr)
#||(& (% *PRO* (STATUS ONT::INDEFINITE)
			 (VAR **)
			 (CLASS ONT::NUMBER)
			 (CONSTRAINT
			  (& (MODS (% *PRO* (STATUS ONT::F)
				      (VAR ***)
				      (CLASS ?pred)
				      (CONSTRAINT (& (figure **) (GROUND ?psvar) (DIFF ?card))))))))))||#
      )
     -quan-than-X>
     (head (SPEC (LF ?spec) (VAR ?specv) (agr ?agr)
		 (WH-VAR ?whv) (MASS ?m)
		 (name-spec -)
		 (POSS -)
		 (WH ?w) (RESTR ?set-restr) (NOSIMPLE -) (pred ?pred)
		 (QCOMP (% ?!cat (VAR ?psvar)))
		 (QCOMP ?qcomp)))
     ?qcomp
     (add-to-conjunct (val (size (% *PRO* (STATUS ONT::INDEFINITE)
				      (VAR **)
				      (CLASS ONT::NUMBER)
				      (CONSTRAINT
				       (& (MODS (% *PRO* (STATUS ONT::F)
						   (VAR ***)
						   (CLASS ?pred)
						   (CONSTRAINT (& (figure **) (GROUND ?psvar) (DIFF ?card))))))))))
      (old ?set-restr) (new ?restr))
			     
     )
||#

    ;; cardinality quantifiers goes to SIZE rather than OP
    ;; TEST: some dogs
    ((SPEC (ARG ?arg) (VAR *) (agr ?agr) (MASS  (? m count bare)) (LF ?status) (Nobarespec ?nbs)
      (STATUS ?status)
      ;; (SUBCAT (% N1 (SEM ?subsem) (agr ?agr))) ;; The subcat isn't being used for the simple form yet - if we need it, we'll have to 
      ;;  modifier the ordinal/cardinal rules
      (subcat ?qof) (qcomp ?qcomp) (cardinality +)
      (restr (& (size ?s)))
      (NoSimple ?ns) (npmod ?npm))
     -quan-card-def-simple-spec>
     (head (quan (CARDINALITY +) (SEM ?sem) (VAR ?v) (agr ?agr) (MASS (? m COUNT BARE)) (STATUS ?status)
		 (Nobarespec ?nbs) (NoSimple ?ns) (npmod ?npm)
		 (qof ?qof) (qcomp ?qcomp)
		 (LF ?s))))

    ; two more dogs (two is quanp)
    ((SPEC (ARG ?arg) (VAR *) (agr ?agr) (MASS  (? m count bare)) (LF ?status) (Nobarespec ?nbs)
      (STATUS ?status)
      ;; (SUBCAT (% N1 (SEM ?subsem) (agr ?agr))) ;; The subcat isn't being used for the simple form yet - if we need it, we'll have to 
      ;;  modifier the ordinal/cardinal rules
      (subcat ?qof) (qcomp ?qcomp) (cardinality +)
      ;(restr (& (size ?s)))
      (restr (& (size ?v)))
      (NoSimple ?ns) (npmod ?npm))
     -quanp-card-def-simple-spec>
     (head (quanp (CARDINALITY +) (SEM ?sem) (VAR ?v) (agr ?agr) (MASS (? m COUNT BARE)) (STATUS ?status)
		 (Nobarespec ?nbs) (NoSimple ?ns) (npmod ?npm) (restr ?restr)
		 (qof ?qof) (qcomp ?qcomp)
		 (LF ?s))))
    
    ;;  building quans with "not", e.g., not all trucks
    ;; not too/so much??
    ((quan (SEM ?sem) (VAR ?v) (MASS ?m) (AGR ?agr) (STATUS ?status) (Nobarespec ?nbs) (LF ?s) (negation +)
           (Nosimple ?ns) (NPmod ?nm) )
     -not-quan>
     (word (lex not))
     (head (quan  (negatable +) (SEM ?sem) (VAR ?v) (agr ?agr) (MASS ?m) (AGR ?agr) 
                  (STATUS ?status) (Nobarespec ?nbs) 
                  (Nosimple ?ns) (NPmod ?nm) (LF ?s))))

    
    ; a fifth (of the pizza)
    ((quan (ntype fraction) (val ?val) (agr ?a)
	   (var ?var) (lex ?lex) (sem ?sem) (mass ?mass)
	   (status sm)
	   (QOF (% PP (PTYPE OF) (AGR (? a1 3s 3p)) (MASS ?mass))) (lf ?val)  ; note: ?a1 is 3s/3p regardless of what ?a is (e.g., two thirds of the pizza *was* eaten)
	   )
     -fraction-to-quan>
     (head (number (ntype fraction) (val ?val) (agr (? a 3s 3p)) (var ?var) (lex ?lex) (sem ?sem)
		   )))

    
     ;;  Special rule for every third, every fifth, ...
    
    ((SPEC (ARG ?arg) (VAR ?v) (agr ?agr) (MASS ?m) (LF ONT::QUANTIFIER)
           (SUBCAT (% PP (ptype of))) (RESTR (& (QUAN EVERY) (ORDINAL ?q)))
           )
     -quan-every-nth>
     (head (quan (VAR ?v) (agr 3s) (MASS ?m) (LF ONT::EVERY) (STATUS ONT::QUANTIFIER)))
     (ordinal (LF (W::NTH ?q))))

    ;; every five ....
     ((SPEC (ARG ?arg) (VAR ?v) (agr ?agr) (MASS ?m) (LF ONT::QUANTIFIER)
           (SUBCAT (% PP (ptype of))) (RESTR (& (QUAN EVERY) (AMOUNT ?num)))
           )
     -quan-every-num>
     (head (quan (VAR ?v) (agr 3s) (MASS ?m) (LF ONT::EVERY) (STATUS ONT::QUANTIFIER)))
     (number (val ?num)))
    
    ;; Infinitive NPS -- to be is divine
    ((np (SORT PRED)
         (gap -) (var ?v)  (agr 3s)
         (sem ?sem) (mass bare)
         (case sub)      
         (lf (% description (status ont::kinD) (VAR ?v) 
	        (class ?class) 
	        (constraint ?con) (sort individual)
	        (sem ?sem) (transform ?transform)
	        )))
     -infinitive-np> 0.96 ;; don't want to consider it unless there are no other interpretations
     (head (cp (ctype s-to) 
	       (var ?v)  
               (gap -)
               (sem ?sem)
               (lf (% prop (class ?class) (constraint ?con) (transform ?transform)))))
     )


    ; TEST: the quickly loaded truck ; the quickly computer generated truck
    ;; Myrosia 11/26/01 we only allow those phrases before the verbs. After the verbs, they should be treated as reduced relative clauses
    
     ((ADJP (ARG ?arg) (VAR ?v) (sem ?sem) (class ?lf)
	    (subcatmap ?subjmap) ;(SUBCATMAP (? x ont::affected ont::affected-result ont::neutral))
	    (ARGUMENT ?subj)
	    (atype attributive-only) ;(atype central) 
	    (LF (% PROP (class ?lf) (VAR ?v) (constraint ?newc)))
      )
     -vp-pastprt-adjp>
     (head
      (vp- (class ?lf) (constraint ?cons) (var ?v) (sem ?sem)
	   (subj-map ?subjmap) ;(SUBJ-MAP (? x ont::affected ont::affected-result))
	   (SUBJ ?subj) ;; more general to ask for SUBJ to be AFFECTED role, includes
 	                                         ;; the passive as well as unaccusative cases
	                ;; also neutral/formal: the expected result
	   (subjvar ?arg)
	   (gap -) ;;  no gap in the VP
	   (vform (? pp passive pastpart))
	   (complex -)
           (advbl-needed -)
	   (dobj (% -))  ;; we can't say "the cooked the steak meat" but "the cooked meat" is fine.
	   ;(comp3 (% -))  ;; sacrificing "the broken by the hammer window"
	   (comp3 (% ?comp3 (var ?compvar)))  
           ))
     (not-bound (arg1 ?compvar)) ; optional but unbound
     ;(append-conjuncts (conj1 ?cons) (conj2 (& (ont::affected ?arg))) (new ?newc))
     (append-conjuncts (conj1 ?cons) (conj2 (& (?subjmap ?arg))) (new ?newc))
     )

     ; may have been subsumed by -VP-PASTPRT-ADJP>
     ; If using this rule, need to make part "-" or check for matching part
     ; Also note that VP- doesn't have headfeatures here
     #|
    ;; TEST: the loaded truck
    ;; bare passive form as an adjective
    ;; Verb must be passive, and require no complement  
    ((VP- (VAR ?v)  (arg ?arg) (class ?lf)
	  (subj-map ?!reln) (subj ?subj) (VFORM (? vf PASSIVE))
	  (LF (% prop (constraint ?cons) (class ?lf) (VAR ?v)))
      )
     -simple-v-passive-for-adj> 0.97
     (head (V (VFORM (? vf PASSIVE)) (COMP3 (% - )) (DOBJ (% -)) (GAP -) (LF ?lf)
              (SUBJ-MAP ?!reln) (SUBJ ?subj)
              (VAR ?v)
              )
      ))
     |#

    ;; TEST: the computer-generated dog
     ((ADJP (VAR ?v)  (arg ?dobj) (class ?lf) (atype attributive-only) ;(atype w::central)
	    (argument (% NP (var ?dobj)))
      (vform passive) (constraint ?constraint) (sem ?sem2) ;(sem ?sem)
      (LF (% prop (class ?lf) (var ?v)
	     (constraint 
	      (& (?!reln (% *PRO* (status ont::kind) (var ?v-n) (class ?nc) (constraint ?nr) (sem ?sem)))
		 (?dobj-map ?dobj))))))
     -adj-passive+subj-hyphen> 1
     (n1 (sort ?sort) (CLASS ?nc) (RESTR ?nr) (status ?status) (complex -) (gerund -) (var ?v-n) 
      (sem ?sem) (relc -) (abbrev -) (gap -) (agr 3s)
	 )
     (punc (lex w::punc-minus))
     (head (V (var ?v) (VFORM pastpart) (DOBJ (% NP (var ?dobj))) (sem ?sem2)
      (GAP -) (LF ?lf)  (part (% -)) ;; no particle forms
      (SUBJ-MAP ?!reln) (dobj-map ?dobj-map)
      (dobj-map (? !dmap ONT::NOROLE))  ; to prevent "RAS-induced phosphorylation of ERK and AKT is compromised" from giving a NOROLE to phosphorylation (using a template for "induce")
      ))
     )

    ;; TEST: the computer generated dog
     ((ADJP (VAR ?v)  (arg ?dobj) (class ?lf) (atype attributive-only) ;(atype w::central)
	    (argument (% NP (var ?dobj)))
      (vform passive) (constraint ?constraint) (sem ?sem2) ;(sem ?sem)
      (LF (% prop (class ?lf) (var ?v)
	     (constraint 
	      (& (?!reln (% *PRO* (status ont::kind) (var ?v-n) (class ?nc) (constraint ?nr) (sem ?sem)))
		 (?dobj-map ?dobj))))))
     -adj-passive+subj> 
     (n1 (sort ?sort) (CLASS ?nc) (RESTR ?nr) (status ?status) (complex -) (gerund -) (var ?v-n) 
      (sem ?sem) (relc -) (abbrev -) (gap -) (agr 3s)
	 )
     (head (V (var ?v) (VFORM pastpart) (DOBJ (% NP (var ?dobj))) (sem ?sem2)
      (GAP -) (LF ?lf) (Part (% -))
      (SUBJ-MAP ?!reln) (dobj-map ?dobj-map)
      (dobj-map (? !dmap ONT::NOROLE))  ; to prevent "RAS-induced phosphorylation of ERK and AKT is compromised" from giving a NOROLE to phosphorylation (using a template for "induce")
      ))
     )

    
    ;; TEST: the Ras-dependent activation
    ((ADJP (VAR ?v) (arg ?arg) (class ?lf) (atype w::central) (argument ?argument)
      (constraint ?constraint)
      (LF (% prop (class ?lf) (var ?v)
	     (constraint 
	      (& (?sc-map (% *PRO* (status ont::BARE) (var ?v-n) (class ?nc) (constraint ?nr) (sem ?sem)))
		  (?arg-map ?arg))))))
     -adj-subcat-hyphen> 1
     (n1 (sort ?sort) (CLASS ?nc) (RESTR ?nr) (status ?status) (complex -) (var ?v-n) 
      (sem ?sem) (relc -) (abbrev -)
	 )
     (punc (lex w::punc-minus))
     (head (ADJ (var ?v) (SUBCAT (% PP (var ?sc)))
      (GAP -) (comparative -)
      (LF ?lf)
      (SUBCAT-MAP ?sc-map)
      (ARGUMENT-MAP ?arg-map)
      (ARGUMENT ?argument)
      ))
     )
    
    ;; TEST: the Ras dependent activation
    ((ADJP (VAR ?v) (arg ?arg) (class ?lf) (atype w::central) (argument ?argument)
      (constraint ?constraint) 
      (LF (% prop (class ?lf) (var ?v)
	     (constraint 
	      (& (?sc-map (% *PRO* (status ont::BARE) (var ?v-n) (class ?nc) (constraint ?nr) (sem ?sem)))
		  (?arg-map ?arg))))))
     -adj-subcat-nohyphen> 1
     (n1 (sort ?sort) (CLASS ?nc) (RESTR ?nr) (status ?status) (complex -) (var ?v-n) 
      (sem ?sem) (relc -) (abbrev -) (gerund -)
	 )
     (head (ADJ (var ?v) 
		(SEM ($ ?xx (f::type (? x ONT::event-of-action ONT::PREDICATE ONT::PROPERTY-VAL))))
		(SUBCAT (% PP (var ?sc) (sem ?sem)))
		(GAP -) (comparative -)  ; excludes "the city closer to..."
		(LF ?lf)
		(SUBCAT-MAP ?sc-map)
		(ARGUMENT-MAP ?arg-map)
		(ARGUMENT ?argument)
		))
     )
    
    ;;#||  This doesn't really work as the COMP3 for "hire" is the THEME, and DOBJ is the RECIPIENT
    ;;   so we get the RECIPIENT role for "A hired employee"
;     ((ADJP (ARG ?arg) (VAR ?v)  (SUBCATMAP ?!reln) (atype attributive-only)
;           (ARGUMENT ?subj) (sem ?sem)
;           (LF (% PROP (class ?lf) (VAR ?v)
;                  (CONSTRAINT (& (?!reln ?arg)))
;                  (transform ?transform)
;                  ))           
;      )
;     -adj-passive-optional> 0.97
;     (head (V (VFORM PASSIVE) (COMP3 (% ?x (optional +))) (DOBJ (% -)) (GAP -) (LF ?lf) 
;              (SUBJ-MAP ?!reln) (SUBJ ?subj)
;              (VAR ?v) (transform ?transform)
;              )
;      ))
;    
    ;;  bare ing form as an adjective (e.g., the running truck)
    ((ADJP (ARG ?arg) (VAR ?v)  (SUBCATMAP ?!reln) (atype attributive-only)
           (ARGUMENT ?subj) (sem ?sem)
           (LF (% PROP (class ?lf) (VAR ?v) 
;                  (CONSTRAINT (& (?!reln ?arg) (mod ?prefix)))
                  (CONSTRAINT ?newc)
                  (transform ?transform)
		  ))
           )
     -adj-ing>
     (head (V (VFORM (? vf ING)) (COMP3 (% - )) ;;(DOBJ (% -)) 
	      (GAP -) (LF ?lf) (sem ?sem)
              (SUBJ-MAP ?!reln) (SUBJ ?subj)
              (VAR ?v) (transform ?transform)
	      (SEM ($ F::situation (f::type ont::event-of-action)))
	      (COMP3-map -)
	      (dobj-map -)  ;; seems right, but maybe too strong?   JFA 11/18
;	      (prefix ?prefix)
	      (restr ?prefix)
	      (part (% -))
              )
           )
     (append-conjuncts (conj1 ?prefix) (conj2 (& (?!reln ?arg)))
		       (new ?newc))

     )


  ;; the phosphorylating Ras (phosphorylating has an optional LOC role)
 ((ADJP (ARG ?arg) (VAR ?v)  (SUBCATMAP ?!reln) (atype attributive-only)
           (ARGUMENT ?subj) (sem ?sem)
           (LF (% PROP (class ?lf) (VAR ?v) 
;                  (CONSTRAINT (& (?!reln ?arg) (mod ?prefix)))
                  (CONSTRAINT ?newc)
                  (transform ?transform)
		  ))
           )
     -adj-ing-opt-comp3> 0.97
     (head (V (VFORM (? vf ING)) (COMP3 (% ?!xx (w::optional +)))
	      (GAP -) (LF ?lf) (sem ?sem)
              (SUBJ-MAP ?!reln) (SUBJ ?subj)
              (VAR ?v) (transform ?transform)
;	      (prefix ?prefix)
	      (restr ?prefix)
	      (part (% -))
              )
           )
     (append-conjuncts (conj1 ?prefix) (conj2 (& (?!reln ?arg)))
		       (new ?newc))

     )
 
 ))



;; PP-WORDS

(parser::augment-grammar 
  '((headfeatures
     (NP VAR SEM LEX wh lex orig-lex headcat transform postadvbl)
     (SPEC POSS POSS-VAR POSS-SEM  transform) 
     (ADVBL VAR SEM LEX ATYPE lex orig-lex headcat transform neg)
     (ADVBL-R VAR SEM LEX ATYPE argument wh lex orig-lex headcat transform)
     (QUANP CARDINALITY AGR)
     (N1 lex orig-lex headcat set-restr refl abbrev nomobjpreps nomsubjpreps agent-nom rate-activity-nom result subcat subcat-map) ; result for nominalizations
     (ADJP lex orig-lex headcat argument transform)
     )

;;;     ;; lexicalized quantifier phrases, e.g., all, several, ...
;;;    ;; currentl we simply make the LF into the VAR to allow simple atoms in the :QUAN slot.
;;;    ;;  may need to change later 
;;;    ((QUANP (ARG ?arg) (AGR ?agr) (VAR ?s) (SEM ?sem)
;;;	    (MASS (? m count bare)) (STATUS ?status) 
;;;	  (Nobarespec ?nbs)
;;;	  )
;;;     -lex-quan>
;;;     (head (quan (SEM ?sem) (VAR ?v) (agr ?agr) (MASS ?m) (STATUS ?status) (Nobarespec ?nbs) (NoSimple ?ns) (npmod ?npm)
;;;	    (LF ?s)))
;;;     )

    ;;   HOW much, many, few   - I haven't found a generalization yet  that excludes "How several", so we enumerate - sorry! JFA 02/03
    ;; its in its own rule cluster here because I don't want the LEX to be a head feature
    
    ((SPEC (LF ONT::INDEFINITE) ;(LF wh-quantity)
	   (Lex (How ?l)) (headcat QUAN) (STATUS WH) (AGR ?a)
          (ARG ?arg) (MASS ?m) (WH Q) (WH-VAR *) (QUANT +) (VAR ?v)
          (RESTR (& (SIZE (% *PRO* (STATUS WH) (VAR *) (CLASS ont::QUANTITY)
			     (CONSTRAINT (& (QUAN ?lf)
					    ;(suchthat ?arg)
					    ))))))
          (SUBCAT (% PP ))
          )
     -how-many-etc>
     (word (lex how))
     (head (quan (sem ?def) (LF ?lf) (MASS (? mss COUNT bare)) (AGR ?a) (VAR ?v) (lex (? x MANY FEW)))))

    ((SPEC (LF ONT::SM) ;(LF wh-quantity)
	   (Lex (How ?l)) (headcat QUAN) (STATUS WH) (AGR ?a)
          (ARG ?arg) (MASS ?m) (WH Q) (WH-VAR *) (QUANT +) (VAR ?v)
          (RESTR (& (QUANTITY (% *PRO* (STATUS WH) (VAR *) (CLASS ont::QUANTITY)
				 (CONSTRAINT (& (QUAN ?lf)
						;(suchthat ?arg)
						))))))
          (SUBCAT (% PP ))
          )
     -how-much>
     (word (lex how))
     (head (quan (sem ?def) (LF ?lf) (MASS MASS) (AGR ?a) (VAR ?v) (lex MUCH))))


    ;; This constructs an NP to stand for the implicit object introduced
    ;;  by the pp-word, e.g., for "why" it is a REASON, for "HOW" it is a METHOD, etc
    ((NP (PP-WORD +) (SORT PRED) (VAR ?v) (SEM ?sem) (lex ?lex) 
         (WH Q) (WH-VAR ?v) (CASE ?case) (class ?lf) ;(pred ?lf)
         (LF (% Description (status WH) (var ?v) (Class ?lf) (SORT PRED) ;(SORT (?agr -))
                (Lex ?lex) (sem ?sem) (transform ?transform) (constraint (% & (proform ?lex)))
                )))
     -np-pp-word1>
     (head (n (SORT PP-WORD) (AGR ?agr) (SORT ?sort)
              (LF ?lf) (sem ?sem)
              (LEX ?lex) (VAR ?v) (WH Q)
              (transform ?transform)
              )))
    
    ;; why not e.g., I don't know why not
    ((NP (PP-WORD +) (SORT PRED) (VAR ?v) (SEM ?s) (lex why) 
         (WH Q) (CASE ?case) (pred ?lf)
         (LF (% Description (status WH) (var ?v) (Class ?lf) (SORT PRED) ;(SORT (?agr -))
                (lex why) (sem ?sem) (transform ?transform)
                )))
     -np-pp-word-why-not>
     (head (n (SORT PP-WORD) (lex why) (SEM ?s) (AGR ?agr) (SORT ?sort)
              (LF ?lf) (sem ?sem)
              (VAR ?v) (WH Q)
              (transform ?transform)
              ))
     (word (lex not))
     )
    
    ;;  e.g., what else
    ((NP (PP-WORD +) (SORT pred) (VAR ?v) (SEM ?s) (lex ?lex) 
         (WH Q) (WH-VAR ?v) (CASE ?case) (pred ?lf)
         (LF (% Description (status WH) (var ?v) (Class ?lf) (SORT PRED) ;(SORT (?agr -))
                (Lex ?lex) (sem ?sem) 
		(constraint (& (MODS ?else-v)))
			     ;;(?ELSE-LF (% *PRO* (var *) (class ?lf) (sem ?sem) (constraint (& (proform ?else-lex)))))))
                (transform ?transform) 
                ))
         )
     -np-pp-word-else1>
     (head (n (SEM ?s) (AGR ?agr) (SORT PP-WORD)
              (LF ?lf)
              (sem ?sem) 
	    (LEX ?lex) (VAR ?v) (WH Q) (transform ?transform)))
      (Advbl (sort else) (var ?else-v) (arg ?v))
      ;; (cv (lex ?else-lex) (lex else) (lf ?else-lf))
     )
    
    ;;  Words like there, here, tomorrow (no WH terms) are treated as PRO forms

    ((NP ;(PP-WORD +) ; so that this can be the np in -s1>
	 (PRO +) (SORT (? srt pred set)) (VAR ?v) (SEM ?sem) (lex ?lex)
         (role ?lf) (agr (? agr 3s 3p -)) (case ?case) (class ?lf)
         (LF (% Description (status ont::pRO) (var ?v) (Class ?lf) (SORT (? srt pred set)) ;(SORT (?agr -))
                (Lex ?lex) (sem ?sem) (transform ?transform) (constraint (% & (proform ?lex)))
                )))
     -np-pp-word2>
     (head (N (AGR ?agr) (SORT PP-WORD)
              (LEX ?lex) (VAR ?v) (WH -)
              (lf ?lf)
              (sem ?sem) (transform ?transform)
              )
           ))
    
    ;; adverbials with pp-words have a default pro subcat, expressed as an argument in the LF
    
    ;; wh-terms   
    ((ADVBL  (ARG ?argvar) (SUBCATSEM ?subcatsem)
      ;; MD 06/11/06
      ;; note that the explicit propagation up is required, even though argument is head feature
      ;; otherwise all the relevant features do not propagate up correctly
      (argument ?argument)
      (sort pp-word)
      (wh-var *)  (WH Q)
      (var ?v) 
      (IMPRO-CLASS ?pro-class)
      (GROUNDVAR *)
      (LF (% PROP (VAR ?v) (CLASS ?reln) 
	     (CONSTRAINT (& (?!submap (% *PRO* (status *wh-term*) (VAR *) (CLASS ?pro-class)
					 (SEM ?subcatsem) (CONSTRAINT (& (proform ?lex)
									 (suchthat ?v)))))
			    (?!argmap ?argvar)))
	     (sem ?sem) (transform ?trans)))
      (gap -) (pp-word +)
      (role ?reln) (how ?how) 
      )
     -advbl-wh-word> 
     (head (adv (SORT PP-WORD) (wh Q) (IMPRO-CLASS ?pro-class) (how ?how)
	    (argument ?argument)
	    (ARGUMENT (% ?argcat (var ?argvar)))
	    (SUBCAT (% ?x (SEM ?subcatsem))) 
	    (subcat-map ?!submap)
	    (argument-map ?!argmap)
	    (LF ?reln) (lex ?lex)
	    (sem ?sem) (transform ?trans)
	    ))
     )

#||  Doesn't seem to be a useful rule - there's not even an ADJ on the RHS
    
 ;; how adj is it
    ((ADJP (ARG ?arg) (VAR ?v) (sem ?sem) (atype ?atype) (comparative ?cmp)
      (LF (% PROP (CLASS ?lf) (VAR ?v) (CONSTRAINT ?newc)
	     (transform ?transform) (sem ?sem) 
	     )))
     -degree-pred>
     (head (ADVBL (LF ?lf) (SUBCAT -) (sem ?sem) (SORT PRED) (ARGUMENT-MAP ?argmap)
	   (constraint ?con) (functn ?fn) (comp-op ?dir)
	    ))
     (append-conjuncts (conj1 ?con) (conj2 (& (?argmap ?arg) (functn ?fn)))
		       (new ?newc))
     )
      ||#

    ;; how adj   e.g., how red
    ((ADJP (ARG ?argvar) (SUBCATSEM ?subcatsem)
      (wh-var *) (WH Q) (SORT PP-WORD) (how +)
      (var ?adjv) (atype ?atype)
      (LF (% PROP (VAR ?adjv) (CLASS ?reln) 
	     (CONSTRAINT ?newc)
	     (sem ?sem) (transform ?trans)))
     ;; (argument (% S (var ?argvar) (sem ?sem1)))
      (gap -) (pp-word +)
      (role ?reln)
      (sem ?sem)
      )
     -how-adj>     
     (adv (SORT PP-WORD) (wh Q) (IMPRO-CLASS ONT::DEGREE) (lex how))
     (head (adjp (var ?adjv) (atype ?atype) (arg ?argvar) (sem ?sem) (LF (% PROP (class ?reln) (constraint ?con)))))
     (append-conjuncts (conj1 ?con) 
      (conj2 (& (degree (% *PRO* (status *wh-term*) (VAR *) (CLASS ont::degree)
		   (SEM ?subcatsem) (CONSTRAINT (& (proform ?lex) (suchthat ?adjv)))))))
      (new ?newc))
     )

     ;; how adv  e.g., how quickly
    ((ADVBL  (ARG ?argvar) (SUBCATSEM ?subcatsem)
      (wh-var *) (WH Q) (SORT PP-WORD) (how-advbl +)
      (var ?adjv) (atype ?atype) (IMPRO-CLASS ont::degree)
      (LF (% PROP (VAR ?adjv) (CLASS ?reln) 
	     (CONSTRAINT ?newc)
	     (sem ?sem) (transform ?trans)))
      (gap -) (pp-word +) (argument ?argu)
      (role ?reln)
      )
     -how-advbl>     
     (adv (SORT PP-WORD) (wh Q) (IMPRO-CLASS ont::degree) (lex how))
     (head (advbl (var ?adjv) (atype ?atype) (arg ?argvar)  (argument ?argu) (sort pred) ; to rule out "how about..."
		  (LF (% PROP (class ?reln) (constraint ?con)))))
     (append-conjuncts (conj1 ?con) 
      (conj2 (& (degree (% *PRO* (status *wh-term*) (VAR *) (CLASS ont::degree)
		   (SEM ?subcatsem) (CONSTRAINT (& (proform ?lex) (suchthat ?adjv)))))))
      (new ?newc))
     )
    
    ;; pp adverbials, here, there, home 
    ((ADVBL  (ARG ?argvar) (SUBCATSEM ?subcatsem)
      (argument ?argument)
      (sort pp-word)
      (var ?v) 
      (LF (% PROP (VAR ?v) (CLASS ?reln) 
	     (CONSTRAINT (& (?!submap (% *PRO*
					 (VAR *) (CLASS ?!pro-class)
					 (SEM ?subcatsem) (CONSTRAINT (& (proform ?lex)))))
			    ;;(suchthat ?v)))))
			    (?!argmap ?argvar)))
	     (sem ?sem) (transform ?trans)))
      (gap -) (pp-word +)
      (role ?reln)
             )
     -advbl-pp-word>     
     (head (adv (SORT PP-WORD) (wh -) (IMPRO-CLASS ?!pro-class)
		(argument ?argument)
	        (ARGUMENT (% ?argcat (var ?argvar)))
	        (SUBCAT (% ?x (SEM ?subcatsem))) 
	        (subcat-map ?!submap)
	        (argument-map ?!argmap)
	        (LF ?reln) (lex ?lex)
	        (sem ?sem) (transform ?trans)
	        ))
     )
     
    
    ;;  ELSE modifier on pp advbls
    ((ADVBL (ARG ?argvar) (SUBCATSEM ?subcatsem)
      (FOCUS ?var) (var ?v) (wh-var *) 	(argument ?argument)
      (LF (% PROP (VAR ?v) (CLASS ?reln) 
	     (CONSTRAINT (& (?!submap (% *PRO* (VAR *) (SEM ?subcatsem) 
					 (CONSTRAINT (& (proform ?lex)
							(mods ?else-v)
							(suchthat ?v)))))
			    (?!argmap ?argvar)))
	     (sem ?sem) (transform ?trans)))
      (gap -) (pp-word +)
      (role ?reln)
      )
     -advbl-pp-word-else>     
     (Head (adv (SORT PP-WORD) (wh -)
	        (ARGUMENT (% ?argcat (var ?argvar)))
	    (SUBCAT (% ?x (SEM ?subcatsem))) 
	    (subcat-map ?!submap)
	    (argument-map ?!argmap)
	    (argument ?argument)
	    (LF ?reln) (lex ?lex)
	    (sem ?sem) (transform ?trans)
	    (else-word +)
	    ))
     ;;     (cv (lex ?else-lex) (lex else) (lf ?else-lf))
     (Advbl (sort else) (var ?else-v) (arg ?v))
     )

    ;;   ELSE modifier of WH adverbials..  where else ...

     ((ADVBL (ARG ?argvar) (SUBCATSEM ?subcatsem)
      (FOCUS ?var) (var ?v) (wh-var *) (WH Q)
      (LF (% PROP (VAR ?v) (CLASS ?reln) 
	     (CONSTRAINT (& (?!submap (% *PRO* (class ?proclass) (status *wh-term*)
					 (VAR *) (SEM ?subcatsem) 
					 (CONSTRAINT (& (proform ?lex)
							(mods ?else-v)
							(suchthat ?v)))))
			    (?!argmap ?argvar)))
	     (sem ?sem) (transform ?trans)))
      (gap -) (pp-word +)
      (role ?reln)
      )
     -advbl-wh-pp-word-else>     
     (Head (adv (SORT PP-WORD) (wh Q) (impro-class ?proclass)
	        (ARGUMENT (% ?argcat (var ?argvar)))
	    (SUBCAT (% ?x (SEM ?subcatsem))) 
	    (subcat-map ?!submap)
	    (argument-map ?!argmap)
	    (LF ?reln) (lex ?lex)
	    (sem ?sem) (transform ?trans)
	    (else-word +)
	    ))
     ;;     (cv (lex ?else-lex) (lex else) (lf ?else-lf))
     (Advbl (sort else) (var ?else-v) (arg ?v)))
     
    ;; Special construction only for relative clause advbls - we need this only to build the right semantic form
    ;;  e.g., the box WHERE we stood
    ((ADVBL-R  (ARG ?argvar) (SUBCATSEM ?subcatsem) (ARG2 ?arg2var)
             (FOCUS *) 
             (var ?v) 
             (LF (% PROP (VAR ?v) (CLASS ?reln) 
	            (CONSTRAINT (& (?!submap ?arg2var)
			           (?!argmap ?argvar)))
	            (sem ?sem) (transform ?trans)))
             (gap -) (pp-word +)
             (role ?reln)
             )
     -advbl-rel-pro>     
     (head (adv (SORT PP-WORD) (wh R)
	        (ARGUMENT (% ?argcat (var ?argvar)))
	        (SUBCAT (% ?x (SEM ?subcatsem))) 
	        (subcat-map ?!submap)
	        (argument-map ?!argmap)
	        (LF ?reln) (lex ?lex)
	        (sem ?sem) (transform ?trans)
	        ))
     )
    ))

(parser::augment-grammar 
  '((headfeatures
     (ADVBL-R VAR SEM LEX ATYPE argument lex orig-lex headcat transform))
    ;;   e.g., the box ON WHICH we stood
    ((ADVBL-R  (ARG ?argvar) (SUBCATSEM ?subcatsem) (ARG2 ?arg2var)
             (FOCUS *) 
             (var ?v) (WH R)
             (LF (% PROP (VAR ?v) (CLASS ?reln) 
	            (CONSTRAINT (& (?!submap ?arg2var)
			           (?!argmap ?argvar)))
	            (sem ?sem) (transform ?trans)))
             (gap -) (pp-word +)
             (role ?reln)
             )
     -advbl-rel-advbl-which>     
     (head (adv (SORT BINARY-CONSTRAINT)
	        (ARGUMENT (% ?argcat (var ?argvar)))
	        (SUBCAT (% ?x (SEM ?subcatsem))) 
	        (subcat-map ?!submap)
	        (argument-map ?!argmap)
	        (LF (? reln ont::position-reln ont::temporal-relation)) (lex ?lex)
	        (sem ?sem) (transform ?trans)
	        ))
     (pro (lex w::which) (wh R))
     )
    ))
    
; nominalizations
; pass up subcat
(parser::augment-grammar 
  '((headfeatures
     (NP VAR SEM LEX wh lex orig-lex headcat transform postadvbl subcat subcat-map)
     (N1 lex orig-lex headcat set-restr refl abbrev nomobjpreps nomsubjpreps agent-nom rate-activity-nom result subcat subcat-map) ; result for nominalizations
     )    
    
    ;; VPs as gerund-NPS
    ((NP (SORT PRED)
         (gap -) (var ?v) (agr 3s)
         (sem ?sem) (mass mass) (gerund +) (class ?class)
         (case (? case sub obj -)) ;; gerunds aren't case marked, allow any value except posessive
         (lf (% description (status ont::bare) (VAR ?v) 
                (class ?class) 
                (constraint ?con) (sort individual)
                (sem ?sem) (transform ?transform)
                ))
	 )
     -gerund> .97 ;;.97
     (head (vp (vform ing) (var ?v) (gap -) (aux -)
               (sem ?sem) 
	       (class ?class)  (constraint ?con)  (transform ?transform)
	       ))
     )

    
#||   THis is replace by new nominlaization handling
    ((NP (SORT PRED)
      (gap -) (var ?v) (agr 3s)
      (sem ?sem) (mass mass) (gerund +) (class ?class)
      (case (? case sub obj -)) ;; gerunds aren't case marked, allow any value except posessive
      (lf (% description (status bare) (VAR ?v) 
	     (class ?class) 
	     (constraint ?con) (sort individual)
	     (sem ?sem) (transform ?transform)
	     ))
      )
     -gerund-w-subj>
     (np (sem ?npsem) (var ?subjvar) (agr ?a) (case (? casesubj sub -)) (lex ?lex) ;; lex needed for expletives?
      (pp-word -) (changeagr -))
     (head (vp (vform ing) (var ?v) (gap -) (aux -)
               (sem ?sem) (subjvar ?subjvar)
	       (subj (% np (sem ?npsem) (var ?subjvar) (lex ?lex)))
	       (class ?class)  (constraint ?con)  (transform ?transform)
	       ))
     )||#
    
;;   NEW RULES FOR HANDLING NOMINALIZATIONS

    ;; and we have explicit nominalizations (current any N of type EVENT-OF-CHANGE)
    ((N1 (SORT PRED)
      (gap -) (var ?v) (agr ?agr)
      (sem ?sem) (mass ?mass)
      (case (? case sub obj -)) ;; noms aren't case marked, allow any value except posessive
      (class ?class)
      (dobj ?dobj)
      (subj ?subj)
      (comp3 ?comp3)
      (subj-map ?subjmap)
      (dobj-map (? !dmap ONT::NOROLE))
      (comp3-map ?comp-map)
      (restr ?newr)
      
      )
     -n1-nom-with-obj> 1
      (head (n  (var ?v) (gap -) (aux -) (agr ?agr) (sort pred) (headless -)
		(sem ?sem)  (sem ($ F::SITUATION)) ; (f::type ont::event-of-change)))
		(LF ?class) (transform ?transform)
            ;; these are dummy vars for trips-lcflex conversion, please don't delete
            ;;(subj ?subj) (dobj ?dobj) (comp3 ?comp3) (iobj ?iobj) (part ?part)
	    (dobj ?dobj)
	    (dobj (% NP (var ?dobjvar)))
	    (subj ?subj)
	    (subj (% NP (var ?subjvar)))
      	    (comp3 ?comp3)
	    (subj-map ?subjmap)
	    (dobj-map (? !dmap ONT::NOROLE))  ; this is so "Ras causes the inhibition of MMP-9 activation." won't be able to use this template to assign NOROLE to MMP-9
	    (comp3-map ?comp-map)
	    (generated -)
	    (restr ?r)
	    ))
      (add-to-conjunct (val (& (?subjmap ?subjvar) ((? !dmap ONT::NOROLE) ?dobjvar))) (old ?r) (new ?newr))
      )
    
 ;; and we have explicit nominalizations (current any N of type EVENT-OF-CHANGE)
    ((N1 (SORT PRED)
      (gap -) (var ?v) (agr ?agr)
      (sem ?sem) (mass ?mass)
      (case (? case sub obj -)) ;; noms aren't case marked, allow any value except posessive
      (class ?class)
      (dobj -)
      (subj ?subj)
      (comp3 ?comp3)
      (subj-map ?subjmap)
      (dobj-map ?dmap)
;      (dobj-map -)
      (comp3-map ?comp-map)
      (restr ?newr)
      
      )
     -n1-nom-without-obj> 1
      (head (n  (var ?v) (gap -) (aux -) (agr ?agr) (sort pred) (headless -)
		(sem ?sem)  (sem ($ F::SITUATION)) ; (f::type ont::event-of-change)))
		(LF ?class) (transform ?transform)
            ;; these are dummy vars for trips-lcflex conversion, please don't delete
            ;;(subj ?subj) (dobj ?dobj) (comp3 ?comp3) (iobj ?iobj) (part ?part)
	    (dobj -)
	    (subj ?subj)
	    (subj (% NP (var ?subjvar)))
      	    (comp3 ?comp3)
	    (subj-map ?subjmap)
	    (dobj-map ?dmap)
;	    (dobj-map -)
	    (comp3-map ?comp-map)
	    (generated -)
	    (restr ?r)
	    (agent-nom -) ;; this rule can't apply to agentive nominalizations
	    ))
      (add-to-conjunct (val (& (?subjmap ?subjvar))) (old ?r) (new ?newr))
      )
     

    ;;  of-PP is typically the DOBJ role   
 ;;   e.g., The eradication of the trucks
    ((N1 (SORT PRED) (COMPLEX +)
      (gap -) (var ?v) (agr ?agr) (gerund ?ger)
      (sem ?sem) (mass ?mass) (pre-arg-already ?npay)
      (case ?case)
      (class ?class)
      (restr ?newrestr)
      (subj ?subj)
      (subj-map ?subjmap)
      (dobj ?!dobj)
      (dobj-map -) ;; eliminate the dobj-map so we can't assign another
      (comp3 ?comp3)
      (comp3-map ?comp-map)
      )
     -nom-of-obj1> 1.0
     (head (n1  (var ?v) (gap -) (aux -)(case ?case) (gerund ?ger)(agr ?agr)
		(nomobjpreps ?nompreps) (pre-arg-already ?npay)
		(dobj ?!dobj) 
;		(dobj (% ?s3 (case (? dcase obj -)) (agr ?agr) (var ?dv) (sem ?dobjsem) (gap -)))
		(dobj (% ?s3 (case (? dcase obj -)) (var ?dv) (sem ?dobjsem) (gap -)))
		(dobj-map ?!dmap)
		(sem ?sem) (sem ($ F::SITUATION)) ; (f::type ont::event-of-change)))
		(class ?class) (transform ?transform)
		;; these are dummy vars for trips-lcflex conversion, please don't delete
		;;(subj ?subj) (comp3 ?comp3) (iobj ?iobj) (part ?part)
		(restr ?restr)
		(subj ?subj)
		(subj-map ?subjmap)
		(comp3 ?comp3)
		(comp3-map ?comp-map)
		(generated -)
		))
;     (pp (ptype ?nompreps) (sem ?dobjsem) (agr ?agr) (gap -) (var ?dv))
     (pp (ptype ?nompreps) (sem ?dobjsem) (gap -) (var ?dv))
     (add-to-conjunct (val (& (?!dmap ?dv))) (old ?restr) (new ?newrestr))
     )

    ;;  of PP with accusative/intransitive verbs (mapping affected to the subj)
 ((N1 (SORT PRED) (COMPLEX +)
      (gap -) (var ?v) (agr ?agr) (gerund ?ger)
      (sem ?sem) (mass ?mass) (pre-arg-already ?npay)
      (case ?case)
      (class ?class)
      (restr ?newrestr)
      (subj -)
      (subj-map -)
      (dobj-map -) ;; eliminate the dobj-map so we can't assign another
      ;(comp3 ?comp3)
      ;(comp3-map ?comp-map)
      (comp3-map -)
      )
     -nom-of-subj1> 1.0
     (head (n1  (var ?v) (gap -) (aux -)(case ?case) (gerund ?ger) (agr ?agr)
		(pre-arg-already ?npay) 
		(dobj-map -)
		(sem ?sem) (sem ($ F::SITUATION)) ; (f::type ont::event-of-change)))
		(class ?class) (transform ?transform)
	    ;; these are dummy vars for trips-lcflex conversion, please don't delete
	    ;;(subj ?subj) (comp3 ?comp3) (iobj ?iobj) (part ?part)
		(restr ?restr)
		(subj ?subj)
		(subj-map ont::affected)
		(subj (% ?s3 (case (? dcase obj -)) (var ?dv) (sem ?subjsem) (gap -)))
		;(comp3 ?comp3)
		;(comp3-map ?comp-map)
		(comp3-map -)
		(generated -)
		))
     (pp (ptype of) (gap -) (sem ?subjsem) (var ?dv))
     (add-to-conjunct (val (& (ont::affected ?dv))) (old ?restr) (new ?newrestr))
     )
  

  ;;  by-PP is the SUBJ role   
 ;;   e.g., The eradication by the army
    ((N1 (SORT PRED) (COMPLEX +)
      (gap -) (var ?v) (agr ?agr) (gerund ?ger)
      (sem ?sem) (mass ?mass) (pre-arg-already ?npay)
      (case ?case)
      (class ?class)
      (restr ?newrestr)
      (dobj ?dobj)
      (dobj-map ?dmap)
      (subj ?subj)
      (subj-map -) ;; we eliminate the SUBJ-MAP so we can't assign another
      (comp3 ?comp3)
      (comp3-map ?comp-map)
      )
     -nom-by-subj1> 1
     (head (n1  (var ?v) (gap -) (aux -)(case ?case)  (gerund ?ger) (nomsubjpreps ?subjpreps)
	      (dobj ?dobj) (pre-arg-already ?npay)(agr ?agr)
	      (subj (% ?s3 (case (? dcase obj -)) (var ?dv) (sem ?subjsem) (gap -)))
	      (dobj-map ?dmap)
	      (sem ?sem) (sem ($ F::SITUATION)) ; (f::type ont::event-of-change)))
	      (class ?class) (transform ?transform)
	    ;; these are dummy vars for trips-lcflex conversion, please don't delete
	    ;;(subj ?subj) (comp3 ?comp3) (iobj ?iobj) (part ?part)
	      (restr ?restr)
	    (subj ?subj)
	    (subj-map ?!subjmap)
	    (comp3 ?comp3)
	    (comp3-map ?comp-map)
	    (generated -)
	    (agent-nom -) ;; no agent in agentivenominalizations
	    ))
     (pp (ptype ?subjpreps) (sem ?subjsem) (gap -) (var ?dv) (gerund -))  ;; gerund PP is probably BY-MEANS-OF
     (add-to-conjunct (val (& (?!subjmap ?dv))) (old ?restr) (new ?newrestr)))


 ;; N-N modification on a nominalization yields DOBJ (if nomobjpreps contains "of")
    ((N1 (SORT PRED) (COMPLEX +)
      (gap -) (var ?v) (agr ?agr) (gerund ?ger)
      (sem ?sem) (mass ?mass) (pre-arg-already +)
      (case ?case)
      (class ?class)
      (restr ?newrestr)
      (subj ?subj)
      (subj-map ?subjmap)
      (dobj ?!dobj)
      (dobj-map -) ;; eliminate the dobj-map so we can't assign another
      (comp3 ?comp3)
      (comp3-map ?comp-map)
      )
     -nom-n-n>
     (np (AGR 3s) (abbrev -) (sort pred) (headless -)
      (var ?v1) 
      (PRO -) (N-N-MOD -) (COMPLEX -) (GAP -)
      (postadvbl -) (post-subcat -) (sem ?dobjsem)
      )
     
     (head (n1  (var ?v) (gap -) (aux -)(case ?case) (gerund ?ger)(agr ?agr)
		(dobj ?!dobj) 
		(dobj (% ?s3 (case (? dcase obj -)) (var ?dv) (sem ?dobjsem) (gap -)))
		(dobj-map ?!dmap) (pre-arg-already -)
		(nomobjpreps w::of)
		(sem ?sem) (sem ($ F::SITUATION)) ; (f::type ont::event-of-change)))
		(class ?class) (transform ?transform)
		;; these are dummy vars for trips-lcflex conversion, please don't delete
		;;(subj ?subj) (comp3 ?comp3) (iobj ?iobj) (part ?part)
		(restr ?restr)
		(subj ?subj)
		(subj-map ?subjmap)
		(comp3 ?comp3)
		(comp3-map ?comp-map)
		))
     (add-to-conjunct (val (& (?!dmap ?v1))) (old ?restr) (new ?newrestr))
     )

    ((N1 (SORT PRED) (COMPLEX +)
      (gap -) (var ?v) (agr ?agr) (gerund ?ger)
      (sem ?sem) (mass ?mass) (pre-arg-already +)
      (case ?case)
      (class ?class)
      (restr ?newrestr)
      (subj ?subj)
      (subj-map ?subjmap)
      (dobj ?!dobj)
      (dobj-map -) ;; eliminate the dobj-map so we can't assign another
      (comp3 ?comp3)
      (comp3-map ?comp-map)
      )
     -nom-n-n-hyphen> 1
     (np (AGR 3s) (abbrev -)(agr ?agr) (sort pred) (headless -)
      (var ?v1) 
      (PRO -) (N-N-MOD -) (COMPLEX -) (GAP -)
      (postadvbl -) (post-subcat -) (sem ?dobjsem)
      )
      (Punc (lex W::punc-minus))
     (head (n1  (var ?v) (gap -) (aux -)(case ?case) (gerund ?ger)
		(dobj ?!dobj) 
		(dobj (% ?s3 (case (? dcase obj -)) (var ?dv) (sem ?dobjsem) (gap -)))
		(dobj-map ?!dmap) (pre-arg-already -)
		(nomobjpreps w::of)
		(sem ?sem) (sem ($ F::SITUATION)) ; (f::type ont::event-of-change)))
		(class ?class) (transform ?transform)
		;; these are dummy vars for trips-lcflex conversion, please don't delete
		;;(subj ?subj) (comp3 ?comp3) (iobj ?iobj) (part ?part)
		(restr ?restr)
		(subj ?subj)
		(subj-map ?subjmap)
		(comp3 ?comp3)
		(comp3-map ?comp-map)
		))
     (add-to-conjunct (val (& (?!dmap ?v1))) (old ?restr) (new ?newrestr))
     )

;; N-N modification on a nominalization yields SUBJ if DOBJ is not available 
    ((N1 (SORT PRED)
      (gap -) (var ?v) (agr ?agr) (gerund ?ger)
      (sem ?sem) (mass ?mass) (pre-arg-already +)
      (case ?case)
      (class ?class)
      (restr ?newrestr)
      (subj ?subj)
;      (subj-map ?subjmap)
      (subj-map -)  ; eliminate subj-map too
      (dobj ?!dobj)
      (dobj-map -) ;; eliminate the dobj-map 
      ;(comp3 ?comp3)
      ;(comp3-map ?comp-map)
      (comp3-map -)
      )
     -nom-n-n-subj> 
     (np (AGR 3s) (abbrev -) (sort pred) (headless -)
      (var ?v1) 
      (PRO -) (N-N-MOD -) (COMPLEX -) (GAP -)
      (postadvbl -) (post-subcat -) (sem ?subjsem) (lex ?subjlex) (agr ?subjagr)
      )
     
     (head (n1  (var ?v) (gap -) (aux -)(case ?case)  (gerund ?ger) (agr ?agr)
		(dobj-map -) 
;		(nomobjpreps w::of)
		(pre-arg-already -)
		(sem ?sem) (sem ($ F::SITUATION)) ; (f::type ont::event-of-change)))
		(class ?class) (transform ?transform)
		;; these are dummy vars for trips-lcflex conversion, please don't delete
		;;(subj ?subj) (comp3 ?comp3) (iobj ?iobj) (part ?part)
		(restr ?restr)
		(subj ?subj) (subj (% ?s1 (lex ?subjlex) (agr ?subjagr) (var ?v1) (sem ?subjsem) (gap -)))
		(subj-map ?!subjmap)
		;(comp3 ?comp3)
		;(comp3-map ?comp-map)
		(comp3-map -)
		(AGENT-NOM -) ;; can't apply to agentive nominalizations
		))
     (add-to-conjunct (val (& (?!subjmap ?v1))) (old ?restr) (new ?newrestr))
     )

;; N-N modification on a nominalization yields SUBJ if "of" is in nomsubjpreps
    ((N1 (SORT PRED)
      (gap -) (var ?v) (agr ?agr) (gerund ?ger)
      (sem ?sem) (mass ?mass) (pre-arg-already +)
      (case ?case)
      (class ?class)
      (restr ?newrestr)
      (subj ?subj)
;      (subj-map ?subjmap)
      (subj-map -)  ; eliminate subj-map too
      (dobj ?!dobj)
      (dobj-map -) ;; eliminate the dobj-map 
      (comp3 ?comp3)
      (comp3-map ?comp-map)
      )
     -nom-n-n-subj1> 
     (np (AGR 3s) (abbrev -) (sort pred) (headless -)
      (var ?v1) 
      (PRO -) (N-N-MOD -) (COMPLEX -) (GAP -)
      (postadvbl -) (post-subcat -) (sem ?subjsem) (lex ?subjlex) (agr ?subjagr)
      )
     
     (head (n1  (var ?v) (gap -) (aux -)(case ?case)  (gerund ?ger) (agr ?agr)
		;;(dobj-map -) can't do this!  cf "ras attack"
		(pre-arg-already -)
		(nomsubjpreps w::of)
		(sem ?sem) (sem ($ F::SITUATION)) ; (f::type ont::event-of-change)))
		(class ?class) (transform ?transform)
		;; these are dummy vars for trips-lcflex conversion, please don't delete
		;;(subj ?subj) (comp3 ?comp3) (iobj ?iobj) (part ?part)
		(restr ?restr)
		(subj ?subj) (subj (% ?s1 (lex ?subjlex) (agr ?subjagr) (var ?v1) (sem ?subjsem) (gap -)))
		(subj-map ?!subjmap)
		(comp3 ?comp3)
		(comp3-map ?comp-map)
		(agent-nom -)
		))
     (add-to-conjunct (val (& (?!subjmap ?v1))) (old ?restr) (new ?newrestr))
     )

;; N-N modification on a nominalization yields DOBJ if SUBJ is not available (e.g., Raf attack by Ras)
    ((N1 (SORT PRED)
      (gap -) (var ?v) (agr ?agr) (gerund ?ger)
      (sem ?sem) (mass ?mass) (pre-arg-already +)
      (case ?case)
      (class ?class)
      (restr ?newrestr)
      (subj ?!subj)
;      (subj-map ?subjmap)
      (subj-map -)  ; eliminate subj-map too
      (dobj ?dobj)
      (dobj-map -) ;; eliminate the dobj-map 
      (comp3 ?comp3)
      (comp3-map ?comp-map)
      )
     -nom-n-n-dobj> 
     (np (AGR 3s) (abbrev -) (sort pred) (headless -)
      (var ?v1) 
      (PRO -) (N-N-MOD -) (COMPLEX -) (GAP -)
      (postadvbl -) (post-subcat -) (sem ?subjsem) (lex ?subjlex) (agr ?subjagr) 
      )
     
     (head (n1  (var ?v) (gap -) (aux -)(case ?case)  (gerund ?ger) (agr ?agr)
		(subj-map -) 
;		(nomsubjpreps w::of)
		(pre-arg-already -)
		(sem ?sem) (sem ($ F::SITUATION)) ; (f::type ont::event-of-change)))
		(class ?class) (transform ?transform)
		;; these are dummy vars for trips-lcflex conversion, please don't delete
		;;(subj ?subj) (comp3 ?comp3) (iobj ?iobj) (part ?part)
		(restr ?restr)
		(dobj ?dobj) (dobj (% ?s1 (lex ?subjlex) (agr ?subjagr) (var ?v1) (sem ?subjsem) (gap -)))
		(dobj-map ?!dobjmap)
		(comp3 ?comp3)
		(comp3-map ?comp-map)
		))
     (add-to-conjunct (val (& (?!dobjmap ?v1))) (old ?restr) (new ?newrestr))
     )
    
    ;; Possessive modification on a nominalization yields SUBJ 
    ;;  this only applies to N's whose dobj-map is - -- because its filled or doesn't exist!
    ((NP (SORT PRED)
      (gap -) (var ?v) (agr ?agr) (gerund ?ger)
      (sem ?sem) (mass ?mass) (pre-arg-already +)
      (case ?case)
      (lf (% description (status ont::definite) (var ?v) (sort INDIVIDUAL)
		    (Class ?class) (constraint ?newrestr)
		    (sem ?sem)))
      ;;(class ?class)
      ;;(restr ?newrestr)
      (subj ?subj)
      (subjvar ?v1)
      ;;(subj-map -)
      ;;(dobj ?!dobj)
      ;;(dobj-map -)
      ;;(comp3 ?comp3)
      ;;(comp3-map ?comp-map)
      )
     -nom-poss-n-subj> 1
     (Possessor (restr (& (assoc-poss ?v1))))
     (head (n1  (var ?v) (gap -) (aux -)(case ?case)  (gerund ?ger)(agr ?agr)
		(complex ?complex)
		(sem ?sem) (sem ($ F::SITUATION)) ; (f::type ont::event-of-change)))
		(class ?class) (transform ?transform)
		;; these are dummy vars for trips-lcflex conversion, please don't delete
		;;(subj ?subj) (comp3 ?comp3) (iobj ?iobj) (part ?part)
		(restr ?restr)
		(subj (% NP (var ?v1)))
		(subj-map ?!subjmap)
		(dobj-map -)
		(comp3 ?comp3)
		(comp3-map ?comp-map)
		(agent-nom -)
		))
     (add-to-conjunct (val (& (?!subjmap ?v1))) (old ?restr) (new ?newrestr))
     )

    ;; possessive can be DOBJ if DOBJ is not otherwise specified
    ((NP (SORT PRED) (COMPLEX ?complex)
      (gap -) (var ?v) (agr ?agr) (gerund ?ger)
      (sem ?sem) (mass ?mass) (pre-arg-already +)
       (lf (% description (status ont::definite) (var ?v) (sort INDIVIDUAL)
		    (Class ?class) (constraint ?newrestr)
		    (sem ?sem) 
		    ))
      (case ?case)
      (subj ?subj)
      )
     -nom-poss-n-obj> 1
     (Possessor (restr (& (assoc-poss ?v1))))
     (head (n1  (var ?v) (gap -) (aux -)(case ?case)  (gerund ?ger) (complex ?complex) (agr ?agr)
		(dobj-map (? dobjmap ONT::AFFECTED ONT::NEUTRAL ONT::AFFECTED1 ONT::NEUTRAL1 ONT::AGENT1))
		(dobj ?!dobj) 
		(sem ?sem) (sem ($ F::SITUATION)) ; (f::type ont::event-of-change)))
		(class ?class) (transform ?transform)
		;; these are dummy vars for trips-lcflex conversion, please don't delete
		;;(subj ?subj) (comp3 ?comp3) (iobj ?iobj) (part ?part)
		(restr ?restr)
		(subj ?subj)
		(subj-map ?subj-map)
		(comp3 ?comp3)
		(comp3-map ?comp-map)
		))
     (add-to-conjunct (val (& (?dobjmap ?v1))) (old ?restr) (new ?newrestr))
     )

    ;;   nominalization with verbal complements
    ((N1 (SORT PRED) (COMPLEX +)
      (gap -) (var ?v) (agr ?agr) (gerund ?ger)
      (sem ?sem) (mass ?mass) (pre-arg-already ?npay)
      (case ?case)
      (class ?class)
      (restr ?newrestr)
      (subj ?subj)
      (subj-map ?subjmap)
      ;;(dobj-map ?dobjmap) 
      (dobj-map -)
      (dobj ?dobj)
      (comp3 -)
      (comp3-map -)
      (subcat (% -))
      )
     -nom-compln> 1
     (head (n1  (var ?v) (gap -) (aux -)(case ?case) (agr ?agr)
		(dobj ?dobj)
		(pre-arg-already ?npay)  (gerund ?ger)
		;;(dobj-map ?dobjmap)
		(dobj-map -)
		(sem ?sem) (sem ($ F::SITUATION)) ; (f::type ont::event-of-change)))
		(class ?class) (transform ?transform)
		;; these are dummy vars for trips-lcflex conversion, please don't delete
		;;(subj ?subj) (comp3 ?comp3) (iobj ?iobj) (part ?part)
		(restr ?restr)
		(subj ?subj)
		(subj-map ?subjmap)
		(comp3 (% ?s3 (case (? dcase obj -)) (var ?dv) (sem ?subjsem) (gap -)))
		(comp3 ?!comp3) 
		(comp3-map ?!compmap)
		))
     ?!comp3
     (add-to-conjunct (val (& (?!compmap ?dv))) (old ?restr) (new ?newrestr))
     )
    


    ))


(parser::augment-grammar	 
  '((headfeatures
     (N1 lf lex orig-lex headcat transform set-restr refl abbrev rate-activity-nom subcat subcat-map); agent-nom)
     )

    ;; this rule handles agentive nominalizations, wraps an "agent" around the event nominalization
    ((N1 (SORT PRED)
      (gap -) (var *) (agr ?agr)
      (sem ?subjsem) (mass count)
      (case (? case sub obj -)) ;; noms aren't case marked, allow any value except posessive
      (class ont::referential-sem) ;;?lf)
      (agent-nom -) (complex ?complex)
      (restr (& (suchthat
		 (% *pro* (status F) (class ?class) (constraint ?restr) (var ?v))))
      
	     ))
     -agentnom1> 1
     (head (n1  (agent-nom +) (complex ?complex)
		(var ?v) (gap -) (aux -)
	     (agr ?agr) (sort pred)
		(sem ?sem)  (sem ($ F::SITUATION)) ; (f::type ont::event-of-change)))
		(class ?class) (transform ?transform)
		(restr ?restr) 
		(subj (% NP (var *) (sem ?subjsem)))
		))
     ;;(compute-ont-type-from-sem (sem ?subjsem) (lf ?lf))     need to fix bug in SEMS that are variables
     )
))
    
    
;; NAMES and complex WH-DESC NPs
;; allows changing of SEM and VAR features

;;(cl:setq *grammar-CONJ*
(parser::augment-grammar	 
  '((headfeatures
     (NP ;;NAME   -- putting it in the NP-NAME rule 
      PRO Changeagr lex orig-lex headcat transform refl)
     (NPSEQ CASE MASS NAME PRO lex orig-lex headcat transform)
     (NSEQ CASE MASS NAME lex orig-lex headcat transform)
     (N1 sem lf lex orig-lex headcat transform set-restr refl abbrev rate-activity-nom); agent-nom)
     (N sem lf mass sort lex orig-lex headcat transform refl  rate-activity-nom agent-nom)
     )
    
  ;;  ing forms can serve as nominalizations e.g., The loading  note: it goes here as nomobjpreps can't be a head feature!
    ((N1 (SORT PRED)
      (gap -) (var ?v) (agr 3s) (gerund +)
      (sem ?sem) (mass ?mass)
      (case (? case sub obj -)) ;; gerunds aren't case marked, allow any value except posessive
      (class ?class)
      (dobj ?dobj)
      (subj ?subj)
      (comp3 ?comp3)
      (subj-map ?!subjmap)
      (dobj-map ?dmap)
      (comp3-map ?comp-map)
      (nomobjpreps ?nop)
      (nomsubjpreps ?nsp)
      (subcat (% -))
      )
     -gerund2> ;;0.98
     (head (v (vform ing) (var ?v) (gap -) (aux -) 
	      (sem ?sem) 
	      (LF ?class) (transform ?transform)
            ;; these are dummy vars for trips-lcflex conversion, please don't delete
            ;;(subj ?subj) (dobj ?dobj) (comp3 ?comp3) (iobj ?iobj) (part ?part)
	    (dobj ?dobj)
	    (subj ?subj)
	    (comp3 ?comp3)
	    (subj-map ?!subjmap)
	    (dobj-map ?dmap)
	    (comp3-map ?comp-map)
	    (nomobjpreps ?nop)
	    (nomsubjpreps ?nsp)
	    (part ?part)
	    ))
     ?part
     )

    ;; swift 11/28/2007 there is no more gname status
    ;; Myrosia 2/12/99: changed the rule so that class in LF comes from class
    ;; Added "postadvbl -" to handle things like "elmwood at genesee"
    ;; NP -> NAME
    ;; Myrosia 5/19/00 Changed the rule to apply only to "true" names
    ;; "generated" names get status "GNAME" in the next rule
    ((NP (SORT ?sort)
         (var ?v) (Class ?lf) (sem ?sem) (agr ?agr) (case (? cas sub obj -))
         (LF (% Description (Status ?newspec) (var ?v) (Sort Individual)
                (class ?lf) (lex ?l) (sem ?sem) 
                (transform ?transform)  (generated ?gen)
		(constraint ?con)
                ))
	 (mass ?mass) ; amount of Ras (mass)
	 (name +) (simple +) (time-converted ?tc) (generated ?gen)
      (postadvbl ?gen) ;; swift -- setting postadvl to gen as part of eliminating gname rule but still allowing e.g. truck 1
      )
     -np-name> 0.995
     (head (name (lex ?l) (sem ?sem) (var ?v) (agr ?agr) (lf ?lf) (class ?class)
		 (sort sort) (sem ($ (? !s F::time)))
		 (full-name ?fname) (time-converted ?tc)
		 ;; swift 11/28/2007 removing gname rule & passing up generated feature (instead of restriction (generated -))
		 (generated ?gen)  (transform ?transform) (title -)
		 (restr ?restr)
		 ))
     (add-to-conjunct (val (:name-of ?l)) (old ?restr) (new ?con))
     (recompute-spec (spec ont::definite) (agr ?agr) (result ?newspec)))

    
    ;; number or number-and-letter sequences
    ((NP (SORT PRED)
      (var ?v) (Class ONT::ANY-SEM) (agr ?agr) (case (? cas sub obj -))
      (LF (% Description (status ont::definite) (var ?v) (Sort Individual) (lex ?lf)
                (Class ONT::SEQUENCE) (constraint (& (NAME-OF ?lf) (val ?val)))
		(lex ?l) (sem ?sem) (transform ?transform)
                ))
      (sem ($ (? ft f::phys-obj f::abstr-obj)))
      (postadvbl +) (generated +)
      (mass bare)
      (constraint ?restr) (bare-sequence ?bare-sequence)
      )
     -np-sequence-num> 
     (head (rnumber (val ?lf) (lex ?l) (val ?val) (bare-number -) (bare-sequence ?bare-sequence)
			    (restr ?restr) (var ?v)))
     )


	;;   Headless adjective phrases
	;;  The green, the largest, the largest of the lions
    ((NP (SORT PRED) (class ont::referential-sem) ;(CLASS ?c)
      (VAR *) (sem ?s) (case (? case SUB OBJ))  (headless +) (agr (? agr 3s 3p))
      (wh-var ?whv)
      (lf (% description (status (? st definite definite-plural)) ;(status ?spec)
	     (var *) ;(sort SET) 
	     (class ont::referential-sem) ;(Class ont::Any-sem)
	     (agr (? agr 3s 3p))
	     (constraint ?con)
	     (sem ?s)
	     ))
      (postadvbl +)
      )
     -NP-adj-missing-head> .981  ;; just a hair above the PLUR forms to set the defaulr
     (head (spec  (poss -) (restr ?restr) (wh-var ?whv)
		  (WH -)   ;;tentatively eliminating WH terms  -- is there an example like "show me which large?"
		  ;(restr (& (proform -)))  ;; prevent this and that, which should be pronouns
		  (lf ?spec) (arg *) (agr (? agr 3s 3p)) ;(agr |3P|)
		  (var ?v)))
     (ADJP (LF ?l1) (ARG *) (set-modifier -)
      (var ?advvar)
      (class ?class)
      (ARGUMENT (% NP (sem ?s))))
     ;;(cardinality (var ?card))
     (append-conjuncts (conj1 (& (mods ?advvar))) (conj2 ?restr) (new ?con))
     )

	; Avon is the closer of the cities to London
	((NP (SORT PRED) (CLASS ?c) (VAR *) (sem ?s) (case (? case SUB OBJ))  (headless +) (agr (? agr 3s 3p))
	     (lf (% description (status (? st definite definite-plural)) ;(status ?spec)
		    (var *) ;(sort SET) 
		    (class ?c) ;(class ont::referential-sem) ;(Class ont::Any-sem)
		    (agr (? agr 3s 3p))
		    (constraint ?con2)
		    (sem ?s)
		    ))
	  (postadvbl +)
	  )
	 -NP-adj-missing-head-compar> .97 ; .96
	 (head (spec  (poss -) (restr ?restr)
                      (lf ?spec) (arg *) (agr (? agr 3s 3p)) ;(agr |3P|)
		      (var ?v)))
	 (ADJ1 (VAR ?advvar) (arg *) (ARGUMENT (% NP (sem ?s)))
		     (subcat2 -) (comparative +) ; comparative with no "than" clause (subcat2)
		     (LF ?lf) (subcat ?subcat) ;(SUBCAT -)
		     (sem ?sem) (SORT PRED)  (ARGUMENT-MAP ?argmap) (pertainym -)
		     (transform ?transform) (constraint ?con) (constraint (% & (ont::compar ?compar)))
		     (comp-op ?dir)
		     (Atype ?atype) (lex ?lx) ;(lf (:* ?lftype ?lex))
		     (sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))
		     (gap ?gap)
		     (post-subcat -) (prefix -)
		     (functn -)
		     )
	 (pp (ptype w::of) (var ?!pp-var) (sem ?s) (class ?c))
	 ?subcat
	 (not-bound (arg1 ?compar))
	 (append-conjuncts (conj1 ?con) (conj2 (& (orientation ?orient) (intensity ?ints)
						  (?argmap *) (scale ?scale) (refset ?!pp-var)
						  ))
			   (new ?newc))	 
	 (add-to-conjunct (val (mod (% *PRO* (CLASS ?lf) (status PROP)
					(VAR ?advvar) (CONSTRAINT ?newc)
					(transform ?transform) (sem ?sem) (premod -)
					)))
			  (old ?restr) (new ?con2))
	 )

	
    ;; WH-DESC form
    ;; The Wh-DESC forms map structures that allow relative clause-like modification to indefinite pronoun forms
    ;;   and wh-adverbials

    ;;  The pronoun forms:
    ;;    currently indicate by the feature PRO having value INDEF
    
    ;;  WH-term as gap in an S structure
    ;; e.g., (I know) what the man said, (I know) what else the man said, (i know) what city it is in
    ((np (sort wh-desc)  (gap -) (mass bare) (case (? case SUB OBJ))
	 (sem ?npsem) ;(sem ?s-sem)
	 (var ?npvar) (WH -) (agr ?a)
         (lf (% description (status ?status) (VAR ?npvar) 
                (constraint ?constraint) (sort ?npsort)
                (sem ?npsem)  (class ?npclass) (transform ?transform)
                )))
     -wh-desc1>
     (head (np (var ?npvar) (sem ?npsem) (PRO (? xx INDEF -)) (WH Q)
	       (agr ?a) (case ?case)
            (lf (% description (class ?npclass) (status ?status) (constraint ?cons) (sort ?npsort)
                   (transform ?transform)
                   ))))
     (s (stype decl) (main -) (lf ?lf-s) (var ?s-v) (sem ?s-sem)
      (gap (% np (sem ?npsem) (var ?npvar) (case ?case) (agr ?a))))
     (add-to-conjunct (val (suchthat ?s-v)) (Old ?cons) (new ?constraint)))

    ;; WH-ADJ e.g., "(I know) how fun it is?"

    ((np (sort wh-desc)  (gap -) (mass bare) (case (? case SUB OBJ))
	 (sem ?s-sem) (var ?!whvar) (WH -) (agr ?a)
         (lf (% description (status ont::definite) (class ONT::DEGREE) (VAR ?!whvar) 
                (constraint (& (:suchthat ?s-v))))))
                
     -wh-desc-how-pred>
     (head (pred (var ?var) (arg ?arg) (sem ?npsem) (PRO (? xx INDEF -)) (WH Q) (WH-VAR ?!whvar) (HOW +)
		 (lf (% prop (class ?npclass) (status ?status) (constraint ?cons) (sort ?npsort)
                   (transform ?transform)
                   ))))
     (s (stype decl) (main -) (lf ?lf-s) (var ?s-v) (sem ?s-sem)
      (gap (% pred (sem ?npsem) (var ?var) (arg ?arg)))
    ))

 ;; WH-ADVBL e.g., "(I know) how quickly he ran"

    ((np (sort wh-desc)  (gap -) (mass bare) (case (? case SUB OBJ))
	 (sem ?s-sem) (var ?!whvar) (WH -) (agr ?a)
         (lf (% description (status ont::definite) (class ONT::DEGREE) (VAR ?!whvar) 
                (constraint (& (:suchthat ?var)))))
      )
                
     -wh-desc-how-advbl-pred>
     (head (advbl (var ?var) (PRO (? xx INDEF -)) (WH Q) (WH-VAR ?!whvar) (HOW-ADVBL +)
		 (arg ?s-v) 
		 ))
     (s (stype decl) (main -) (lf ?lf-s) (var ?s-v) (sem ?s-sem)
      (gap -))
    )
     

    ;; other indefinite pronouns may allow any relative clause
    ;; e.g., (tell) anybody you saw, (was there) anyone else that you saw
    
    ((np (gap -) (mass bare) (case (? case SUB OBJ)) (sort ?npsort)
	 (sem ?s-sem) (var ?npvar) (WH -) (agr ?a)
         (lf (% description (status ?status) (VAR ?npvar) 
                (constraint ?constraint)
                (sme ?npsem)  (class ?npclass) (transform ?transform)
                )))
     -indef-pro-desc>
     (head (np (var ?npvar) (sem ?npsem) 
	    (PRO (? prp INDEF +))
	    (WH -)  (sort ?npsort)
	    (agr ?a) (case ?case)
	    ;; Myrosia's hack to make it work - need a feature later
	    ;; This is because SOMETHING is pro +, but anything is PRO indef
	    (lex (? lxx something everything nothing anything someone anyone somebody anybody somewhere anywhere everybody everyone))
            (lf (% description (class ?npclass) (status ?status) (constraint ?cons);; (sort ?npsort)
                   (transform ?transform)
                   ))))
     (cp (ctype relc)
	 (ARG ?npvar) (ARGSEM ?npsem)  (VAR ?CP-V)
	 (LF ?lf) 
	 )
     (add-to-conjunct (val (suchthat ?cp-v)) (old ?cons) (new ?constraint)))

    ;;  Indef Pro's as subjects of a VP
    ;;  e.g., (show me) who/what arrived, I know who else arrived
   
    ((np (sort wh-desc)  (gap -)  (mass bare) (agr ?a)
      (sem ?npsem) ;(sem ?s-sem) ;;(sem ?npsem)  ; should be npsem: e.g., I know *which dog* ate the pizza
      (var ?npvar) (case (? case SUB OBJ))  ;; I think SUB is also OK? isn't it? JFA 3/03
      (lf (% description (status ?status) (VAR ?npvar) (class ?npclass) 
             (constraint ?constraint) (sort ?sort) (WH -)
             (sem ?npsem) (transform ?transform)
             )))
     -wh-desc2> .98  ;; it sometimes interferes with better readings
     (head (np (var ?npvar) (sem ?npsem) (wh Q) (agr ?a) ;;(PRO INDEF)
            (lf (% description  (class ?npclass) (status ?status)
                   (constraint ?cons) (sort ?sort) (transform ?transform)
                   ))))
     (vp (var ?vpvar) (lf ?lf-s) (subjvar ?npvar) 
      (gap -)  ;;(CLASS (? !class ont::IN-RELATION)) ; ont::HAVE-PROPERTY) )     ;;  "what is aspirin" is not a good NP   ;; "can you tell be what is aspirin" is fine!
      (CLASS (? !class ont::EXISTS))
      (vform fin)
      (advbl-needed -) (agr ?a)
      (subj (% np (sem ?npsem) (var ?npvar) (agr ?a)))
      (sem ?s-sem)
      )
     (add-to-conjunct (val ;;(mod ?vpvar))
		       (suchthat ?vpvar))
		      (old ?cons) (new ?constraint))
     )
    
    ;; wh-term as gap
    ;; (tell me) what to do in avon, (I have) nothing to do    
 ((np (sort wh-desc)  (gap -) (mass bare) (case (? case SUB OBJ))
	 ;(sem ?s-sem) (var ?npvar) (WH -) (agr ?a)
	 (sem ?npsem) (var ?npvar) (WH -) (agr ?a)
         (lf (% description (status ?status) (VAR ?npvar) 
                (constraint ?constraint) (sort ?npsort)
                (sem ?npsem)  (class ?npclass) (transform ?transform)
                )))
  -wh-desc3>
  ;; myrosia 2007/22/02 commented out (pro indef) restriction
  ;; because "I don't know which rule to apply / what person to see" are valid sentences
  (head (np (var ?npvar) (sem ?npsem) ;;;(PRO INDEF) 
	 (WH Q)
	 (agr ?a) (case ?case)
	 (lf (% description (class ?npclass) (status ?status) (constraint ?cons) (sort ?npsort)
		(transform ?transform)
		))))
  (cp (ctype s-to) (lf ?lf-s) (var ?s-v) (sem ?s-sem)
   (gap (% np (sem ?npsem) (var ?npvar) (case ?case) (agr ?a))))
  (add-to-conjunct (val (suchthat ?s-v)) (old ?cons) (new ?constraint)))
    
    ;;  WH-term as setting in an S structure 

    ;; e.g., (I know) when/where the train arrived
    ((np (sort wh-desc)  (gap -) (mass bare) (case (? case SUB OBJ))
            (sem ?s-sem) ;; (sem ?advsem)
         (var ?xx) 
         (lf (% description (status *wh-term*) (var ?xx)
                (class ?impro-class)
		(constraint (& (suchthat ?newlf))) (sort individual)
                (sem ?advsem)
                )
             ))
     -wh-desc1a-norole> 0.98
     (head (advbl (pp-word +) 
                  (var ?advblvar) 
		  (how -) (how-advbl -) ;; how ADJ/ADVBL constructions need their own rule
		  ;;(argument (% S (sem ($ f::situation (f::type F::EVENTUALITY)))))
	    (argument (% S (sem ?argsem)))
	    (impro-class ?impro-class)
	    (wh-var ?xx) ;; this is here to disable the foot feature proposition
            (subcatsem ?advsem)
            (focus ?foc) (arg ?s-v) (wh Q) (lf ?lf1)
            ))
     (s (stype decl) (sem ?argsem) (var ?s-v)
      (lf ?lf-s)
      (gap -) ;; no gap here because locations are treated as adjuncts in grammar (except for pred BE!)
      (advbl-needed -) 
      (preadvbl -)   ;; we eliminate preadvbl constructs as they are at best awkward -- e.g., where quickly did you run, and lead to bad parses for how quickly did you run
      )
     (add-constraints-to-lf (lf ?lf-s) (new ((MOD ?advblvar))) (result ?newlf)
     ))
    
    ;;    e.g., (I know) where the dogs are.
    ;; this is the only case where we have a gap
    ;;   its complicated, but we promote the ground value in the advbl
    ;; by overwriting the original implicit ground term (by using the same variable!)
    ((np (sort wh-desc)  (gap -) (mass bare) (case (? case SUB OBJ))
      (sem ?sem) ;; (sem ?advsem)
      (var ?!gv) 
      (lf (% description (status *wh-term*) (VAR ?!gv) 
	     (class ?impro-class) (constraint (& (suchthat ?s-v))) (sort individual)
	     (sem ?advsem)
	     )
	  ))
     -wh-desc1a-be> 
     (head (advbl (pp-word +) 
                  (var ?advar)  (sem ?sem)
		  (argument (% S (sem ?argsem)))
		  (impro-class ?impro-class)
		  (role ?advrole) (subcatsem ?advsem)
		  (focus ?foc) (arg ?s-v) (wh Q) 
		  (groundvar ?!gv)
		  ))
     (s (stype decl) (sem ?argsem) (var ?s-v) (ellipsis -)
      (lf (% prop (class ONT::HAVE-PROPERTY)))
      (gap (% W::PRED (var ?advar) (sem ?sem)))
      (advbl-needed -)
      )
     )
#||  ;; what does this cover that is not covered 
    ;;  special rule for BE form -- unlike the others, we need a GAP in the S
     ;; e.g., (I know) when/where the train is
    
    ((np (sort wh-desc)  (gap -) (mass bare) (case (? case SUB OBJ))
            (sem ?s-sem) ;; (sem ?advsem)
         (var ?npvar) 
         (lf (% description (status *wh-term* ) (VAR ?npvar) 
                (class ?advrole) (constraint (& (suchthat ?s-v))) (sort individual)
                (sem ?advsem)
                )
             ))
     -wh-desc-be-verb> .98
     (head (advbl (pp-word +) 
                  (var ?npvar) 
            ;;(argument (% S (sem ($ f::situation (f::type F::EVENTUALITY)))))
	    ;;(argument (% S (sem ?argsem)))
            (role ?advrole) (subcatsem ?advsem)
            (focus ?foc) (arg ?s-v) (wh Q) (lf ?lf1)
            ))
     (s (stype decl) (var ?s-v)
	(lf (% prop (class ONT::BE))) (gap (% NP (var ?npvar)))
      (ellipsis -) (advbl-needed -)
      )
     )
||#
   
    ;;  WH-term as setting in an S structure 
    ;; e.g., (I know) when/where to go
    ((np (sort wh-desc)  (gap -) (mass bare) (case (? case SUB OBJ))
         (sem ?sem) ;;(sem ?advsem)
         (var ?whvar)
	 (lf ?gv)
	 )
     ;; 11/21/2008 swift raising preference here to improve processing of "let me teach you how to X"
     -wh-desc4-norole> 0.98
     (head (advbl (pp-word +) (var ?advvar)  (sem ?sem)
            (argument (% S (sem ($ f::situation (f::type ont::situation-root)))))
            (role ?advrole) (subcatsem ?advsem)
            (focus ?foc) (arg ?s-v)
	    (wh Q) (wh-var ?whvar)
	    (groundvar ?gv)
	     (lf ?wh-lf)
            ))
     (cp (ctype s-to) (sem ?argsem) (var ?s-v) (lf ?lf-s) (gap -)
      (lf (% Prop (transform ?transform) (sem ?argsem) (class ?c) (constraint ?con)))
      )
     (add-to-conjunct (val (suchthat ?advvar)) (old ?con) (new ?constraint))
     )
    

    ;; e.g. "It depends on whether he is happy"
    ;; We do this as a PP because "whether he is happy" already has an NP-like description
    ;; which supports "I don't know whether he is happy"
    ((pp (sort wh-desc)  (gap -) (mass bare) (case (? case SUB OBJ))
      (sem ?s-sem) 
      (var ?s-v) 
      (lf ?lf-s)
      (ptype ?ptp)
      (headcat ?hc)
      )
     -wh-desc-if> 0.97 ;; low probability so that s-if is taken first where it is subcategorized for
     (prep (lex ?ptp) (headcat ?hc))
     (head (cp (ctype s-if) (clex whether)
	    (sem ?s-sem) (var ?s-v) (lf ?lf-s) (gap -)	    
	    (advbl-needed -)
	    )
      )
     )

    ;; NP -> pronoun
    
    ;; june 2010 singular and plural collapsed with new plural representation: LF identifier is ont::pro for singular, ont::pro-set for plural, triggered on the status ont::feature
    ((NP (SORT PRED) (case ?case)
         (VAR ?v) (SEM ?sem) (lex ?lex) (Class ?c) (AGR ?agr)
         (LF (% Description (status ?st) (var ?v) (Class ?c)
                (Lex ?lex) (constraint (& (proform ?lex)))
                (sem ?sem)))
	 (mass ?m) (expletive ?exp)
         )
     -np-pro>
     (head (pro (SEM ?sem) (VAR ?v) (case ?case) (AGR ?agr)
                (LEX ?lex) (VAR ?v) (WH -) (lf ?c)
		(mass ?m) ;(sing-lf-only -) ; "this" is sing-lf-only + (used to use np-pro-noagr)
		(expletive ?exp)
	    (status (? st ont::PRO ont::pro-set))   ;; this excludes this rule applying to pro's like "everything"
	    (poss -) ;; Added by myrosia 2003/11/02 to avoid "our" as NP
                )))

    ;; indefinite pronouns allow negation, e.g., not everyone, not one, ...

 ((NP (SORT PRED) (case ?case)
         (VAR ?v) (SEM ?sem) (lex ?lex) (Class ?c) (AGR ?agr)
         (LF (% Description (status ?st) (var ?v) (Class ?c)
                (Lex ?lex) (constraint (& (proform ?lex) (negation +)))
                (sem ?sem)))
	 (mass ?m) (expletive ?exp)
         )
     -np-pro-neg>
  (word (lex not))
  (head (pro (pro w::indef) (SEM ?sem) (VAR ?v) (case ?case) (AGR ?agr)
                (LEX ?lex) (VAR ?v) (WH -) (lf ?c)
	    (mass ?m) (sing-lf-only -) (expletive ?exp)
	    (status ?st)
	    (poss -) ;; Added by myrosia 2003/11/02 to avoid "our" as NP
	    )))


   ;; possessive pronouns: it is yours, mine, his, hers
    ((NP (SORT PRED) (case ?case)
         (VAR ?v) (SEM ?sem) (lex ?lex) (Class ont::REFERENTIAL-SEM) (AGR ?agr)
         (LF (% Description (status ont::pro) 
		(var ?v) (Class ont::REFERENTIAL-SEM) 
		(SORT (?agr -))
                (Lex ?lex) 
		(constraint (& (assoc-poss (% *PRO* (status ont::PRO) (var *)
					      (class ?c) 
					      (constraint (& (proform ?lex)
					  ))))))
                (sem ?sem)))
	 (mass ?m)
         )
     -np-pro-poss-sing>
     (head (pro (SEM ?sem) (VAR ?v) (case w::poss) (AGR (? agr 1s 2s 3s))
                (LEX ?lex) (VAR ?v) (WH -) (lf ?c)
	    (mass ?m) (sing-lf-only -)
	    (status ont::PRO)   ;; this excludes this rule applying to pro's like "everything"
	    (poss +) 
                )))

    ;;   my own, your own, ....
     ((NP (SORT PRED) (case ?case)
         (VAR *) (SEM ?sem) (lex ?lex) (Class ?c) (AGR ?agr)
         (LF (% Description (status ont::indefinite) (var *) (Class ?c) (SORT (?agr -))
                (Lex ?lex) (constraint ?restr)
                (sem ?sem)))
	 (mass ?m)
         )
     -np-pro-own-bare>
     (head (possessor (SEM ?sem) (VAR ?v) (restr ?restr) (class ?class)
                (LEX ?lex) (VAR ?v) (WH -) (lf ?lf)(own +) (arg *)
	    (mass w::count)
	    ))
      )
     

    ;; possessive pronouns: it is yours, ours, theirs
    ((NP (SORT PRED) (case ?case)
         (VAR ?v) (SEM ?sem) (lex ?lex) (Class ?c) (AGR ?agr)
         (LF (% Description (status ont::pRO) (var ?v) (Class ?c) (SORT (?agr -))
                (Lex ?lex) (constraint (& (proform ?lex)))
                (sem ?sem)))
	 (mass ?m)
         )
     -np-pro-poss-plural>
     (head (pro (SEM ?sem) (VAR ?v) (case w::poss) (AGR (? agr 1p 2p 3p))
                (LEX ?lex) (VAR ?v) (WH -) (lf ?c)
	    (mass ?m) (sing-lf-only -)
	    (status ont::PRO)   ;; this excludes this rule applying to pro's like "everything"
	    (poss +) 
                )))

   ;;  QUANTIFIER PRO's e.g., EVERYTHING, anything
   ((NP (SORT PRED) (case ?case)
         (VAR ?v) (SEM ?sem) (lex ?lex) (Class ?c) (AGR ?agr)
         (LF (% Description (status (? status ont::quantifier ont::indefinite))  (var ?v) (Class ?c) (SORT (?agr -))
                (Lex ?lex) (constraint (& (quan ?lex)))
                (sem ?sem)))
         (mass ?m)
         )
     -np-quan-sing>
     (head (pro (SEM ?sem) (VAR ?v) (case ?case) (AGR (? agr 1s 2s 3s))
                (LEX ?lex) (VAR ?v) (WH -) (lf ?c) (status (? status ont::quantifier ONt::indefinite ))
	    (mass ?m) (sing-lf-only -)
	    (poss -) 
                )))

   ;; now use np-pro
    ;;  plural pronouns, e.g. US
;    ((NP (SORT PRED)
;         (VAR ?v) (SEM ?sem) (lex ?lex) (Class ?c) (AGR ?agr) (WH -) (case ?case)
;         (LF (% Description (status ont::pro) (var ?v) 
;                (Class (SET-OF (% *PRO* (var *) (status ont::kind) (class ?c))))
;                (SORT set) (constraint (& (proform ?lex)))
;                (sem ?sem)))
;         (mass ?m)
;         )
;     -np-pro-plur>
;     (head (pro (SEM ?sem) (VAR ?v) (AGR (? agr 1p 2p 3p)) (case ?case)
;                (LEX ?lex) (VAR ?v) (WH -) (lf ?c)
;            (mass ?m) (sing-lf-only -)
;            (poss -) ;; Added by myrosia 2003/11/02 to avoid "our" as NP
;            )))

   #| now uses -np-pro>
    ;; Added by Myrosia to cover the cases where there's a pronoun
    ;; with either singular or plural agreement (e.g. "what" in what
    ;; is this/what are these), but where it does not matter in most
    ;; cases and we need to avoid needless ambiguity
    ((NP (SORT PRED) (case ?case)
      (VAR ?v) (SEM ?sem) (lex ?lex) (Class ?c) (AGR ?agr)
      (LF (% Description (status ?status) (var ?v) (Class ?c) (SORT PRED) ;(SORT (?agr -))
	     (Lex ?lex)
	     (constraint (& (proform ?lex)))
	     (sem ?sem)))
      (mass ?m) (expletive ?exp)
      )
     -np-pro-noagr>
     (head (pro (SEM ?sem) (AGR ?agr) (VAR ?v) (case ?case)
	    (LEX ?lex) (VAR ?v) (WH -) (lf ?c)
	    (mass ?m) ;(sing-lf-only +) 
	    (status ?status) (expletive ?exp)
	    (poss -) ;; Added by myrosia 2003/11/02 to avoid "our" as NP
	    )))
   |#
    
    ;; THIS HERE needs a special rule as the AT-LOC modifer from here
    ;;    would normally only modifer AN N1 constituent
    ;;  e.g.,  this here,  this in the lake, this here in the lake
    
    ((NP (SORT PRED)
         (VAR ?v) (SEM ?sem) (lex ?lex) (Class ?c) (AGR ?agr) (case ?case)
         (LF (% Description (status ont::pro) (var ?v) (Class ?c) (SORT (?agr -))
                (constraint ?lf)
                (Lex ?lex) (sem ?sem)
                (mass bare)
                )))
     -this-here>
     (head (pro (POINTER +) (SEM ?sem) (AGR ?agr) (VAR ?v) (case ?case) ;;(POSS -)
                (LEX ?lex) (VAR ?v) (class ?c)))
     (advbls (argument (% ?argcat (sem ?s))) (arg ?v) (LF ?lf) (WH -)))
    
    ;;  CONJUNCTIONS
    ;;  use sequence constructions (SEQ +),  e.g., X, Y and/or Z.
    ;;  we control the types carefully to restrict the possibilities
    
    ;; allow mixing of class and sem, so '490 and inner loop' can combine
    ;;  X and Y,  A, Y and Z
    ((NP (ATTACH ?a) (var ?v) (agr 3p) (SEM ?sem)  
      (LF (% Description (Status ?status) (var ?v) 
	     (class ?class)
	     (constraint (& (operator ?op) (sequence ?members)))
	     (sem ?sem) (CASE ?case1)
	     (mass ?m1) 
	     ))
      (COMPLEX +) (SORT PRED)
      (generated ?generated)
      )
     np-conj1> 
     (NPSEQ (var ?v1) (SEM ?s1) (lf ?lf1) (class ?c1) (CASE ?case) (mass ?m1)
      (generated ?generated1) (separator W::punc-comma)
      (time-converted ?tc1) ;; MD 2008/03/06 Introduced restriction that only items with the same time-converted status can combine - i.e. don't mix number notation for times or non-times. 
      )
     (conj (SEQ +) (LF ?op) (SUBCAT NP) (var ?v)) ;; (status ?status))
     (head (NP (VAR ?v2) (SEM ?s2) (ATTACH ?a) (lf ?lf2) (LF (% ?d (class ?c2) (status ?status))) (CASE ?case2) (mass ?m2) (constraint ?con)
	    (generated ?generated2)
	    (sort (? !sort unit-measure)) ;; no unit-measure here since they form sub-NPs & we want the whole one
	    (time-converted ?tc1) 
	    ))
     (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
     (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
     (simple-cons (in1 ?v2) (in2 ?lf1) (out ?members))
     (logical-and (in1 ?generated1) (in2 ?generated2) (out ?generated))
     )
    
    ;; same construction with exceptions (we have an explicit rule to prefer attachment to the top sequence rather than
    ;;   the last embedded item

     ((NP (ATTACH ?a) (var ?v) (agr 3p) (SEM ?sem)  
      (LF (% Description (Status ?status) (var ?v) 
	     (class ?class)
	     (constraint (& (operator ?op) (sequence ?members) (except ?exception)))
	     (sem ?sem) (CASE ?case1)
	     (mass ?m1) 
	     ))
      (COMPLEX +) (SORT PRED)
      (generated ?generated)
      )
     np-conj1-with-exception> 
      (NPSEQ (var ?v1) (SEM ?s1) (lf ?lf1) (class ?c1) (CASE ?case) (mass ?m1)
       (generated ?generated) (separator W::punc-comma)
       (time-converted ?tc1) ;; MD 2008/03/06 Introduced restriction that only items with the same time-converted status can combine - i.e. don't mix number notation for times or non-times. 
       )
      (conj (SEQ +) (LF ?op) (SUBCAT NP) (var ?v)) ;; (status ?status))
      (head (NP (VAR ?v2) (SEM ?s2) (ATTACH ?a) (lf ?lf2) (LF (% ?d (class ?c2) (status ?status))) (CASE ?case2) (mass ?m2) (constraint ?con)
		(generated ?generated2)
		(sort (? !sort unit-measure)) ;; no unit-measure here since they form sub-NPs & we want the whole one
		(time-converted ?tc1) 
		))
      (conj (but-not +))
      (np (var ?exception))
      (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
      (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
      (simple-cons (in1 ?v2) (in2 ?lf1) (out ?members))
      (logical-and (in1 ?generated1) (in2 ?generated2) (out ?generated))
      )

    ;; same rule with exceptions + comma

     ((NP (ATTACH ?a) (var ?v) (agr 3p) (SEM ?sem)  
      (LF (% Description (Status ?status) (var ?v) 
	     (class ?class)
	     (constraint (& (operator ?op) (sequence ?members) (except ?exception)))
	     (sem ?sem) (CASE ?case1)
	     (mass ?m1) 
	     ))
      (COMPLEX +) (SORT PRED)
      (generated ?generated)
      )
     np-conj1-with-exception-with-comma> 
      (NPSEQ (var ?v1) (SEM ?s1) (lf ?lf1) (class ?c1) (CASE ?case) (mass ?m1)
       (generated ?generated) (separator W::punc-comma)
       (time-converted ?tc1) ;; MD 2008/03/06 Introduced restriction that only items with the same time-converted status can combine - i.e. don't mix number notation for times or non-times. 
       )
      (punc (lex punc-comma))
      (head (conj (SEQ +) (LF ?op) (SUBCAT NP) (var ?v))) ;; (status ?status))
      (NP (VAR ?v2) (SEM ?s2) (ATTACH ?a) (lf ?lf2) (LF (% ?d (class ?c2) (status ?status))) (CASE ?case2) (mass ?m2) (constraint ?con)
       (generated ?generated2)
       (sort (? !sort unit-measure)) ;; no unit-measure here since they form sub-NPs & we want the whole one
		(time-converted ?tc1) 
		)
      (punc (lex punc-comma))      
      (conj (but-not +))
      (np (var ?exception))
      (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
      (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
      (simple-cons (in1 ?v2) (in2 ?lf1) (out ?members))
      (logical-and (in1 ?generated1) (in2 ?generated2) (out ?generated))
      )
    
    ;; sugar, salt, and dill.

    ((NP (ATTACH ?a) (var ?v) (agr 3p) (SEM ?sem) (mass ?m1)
      (LF (% Description (Status ?status) (var ?v) 
	     (class ?class)
	     (constraint (& (operator ?op) (sequence ?members)))
	     (sem ?sem) (CASE ?case1)
	     (mass ?m1) 
	     ))
      (COMPLEX +) (SORT PRED)
      (generated ?generated)
      )
     np-comma-conj> 
     (NPSEQ (var ?v1) (SEM ?s1) (lf ?lf1) (class ?c1) (CASE ?case) (mass ?m1)
      (generated ?generated1) (separator w::punc-comma)
      (time-converted ?tc1) ;; MD 2008/03/06 Introduced restriction that only items with the same time-converted status can combine - i.e. don't mix number notation for times or non-times. 
      )
     (punc (lex punc-comma))
     (head (conj (SEQ +) (LF ?op) (SUBCAT NP) (var ?v) (status ?status)))
    
      (NP (VAR ?v2) (SEM ?s2) (ATTACH ?a) (lf ?lf2) (LF (% ?d (class ?c2))) (CASE ?case2) (mass ?m1) ;(mass ?m2)
	       (constraint ?con)
	    (generated ?generated2) 
	    (sort (? !sort unit-measure)) ;; no unit-measure here since they form sub-NPs & we want the whole one
	    (time-converted ?tc1) 
       )
     (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
     (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
     (simple-cons (in1 ?v2) (in2 ?lf1) (out ?members))
     (logical-and (in1 ?generated1) (in2 ?generated2) (out ?generated))
     )
   

    ((NPSEQ  (SEM ?sem) (LF (?v1 ?v2)) (AGR ?agr) (mass ?m) (class ?class) (case ?c)
      (generated ?gen)  (time-converted ?tc1) (separator w::punc-comma)
      )
     -npseq-initial-sequence-comma> 1.01
     (head (NP (SEM ?s1) (VAR ?v1) ;;(agr ?agr)   ;; AGR is not reliably determined for proper names
	       (complex -) (headless -) (expletive -) ;;(bare-np ?bnp)
	    (generated ?gen1)  (time-converted ?tc1)
	    ;; (bare-sequence -)
	    (LF (% ?sort (class ?c1))) (CASE ?c) (constraint ?con) (mass ?m)
	    (sort (? !sort unit-measure)) ;; no unit measure here since they form sub-NPs [500 mb] & we want the top-level [500 mb of ram] 	    
	    ))
     (punc (lex w::punc-comma))
     (NP (SEM ?s2) (VAR ?v2) ;;(agr ?agr)  
      (complex -) (headless -) (expletive -) ;;(bare-np ?bnp)
	    (generated ?gen2)  (time-converted ?tc1)
	    ;; (bare-sequence -)
	    (LF (% ?sort (class ?c2))) (CASE ?c) (constraint ?con2) (mass ?m)
	    (sort (? !sort unit-measure)))
     (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
     (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
     (logical-and (in1 ?gen1) (in2 ?gen2) (out ?gen))
     )

     ((NPSEQ  (SEM ?sem) (LF (?v1 ?v2)) (AGR ?agr) (mass ?m) (class ?class) (case ?c)
      (generated ?gen)  (time-converted ?tc1) (separator (? p w::punc-slash w::punc-colon w::punc-minus w::punc-en-dash  w::punc-minus))
      )
     -npseq-initial-sequence> 1.01
     (head (NP (SEM ?s1) (VAR ?v1) ;;(agr ?agr)   ;; AGR is not reliably determined for proper names
	       (complex -) (headless -) (simple +)
	       (expletive -) ;;(bare-np ?bnp)
	    (generated ?gen1)  (time-converted ?tc1)
	    ;; (bare-sequence -)
	    (LF (% ?sort (class ?c1))) (CASE ?c) (constraint ?con) (mass ?m)
	    (sort (? !sort unit-measure)) ;; no unit measure here since they form sub-NPs [500 mb] & we want the top-level [500 mb of ram] 	    
	    ))
     (punc (lex (? p w::punc-slash w::punc-colon w::punc-minus w::punc-en-dash w::punc-minus)))
     (NP (SEM ?s2) (VAR ?v2) ;;(agr ?agr)  
      (complex -) (expletive -) (simple +) 
      (headless -)
	    (generated ?gen2)  (time-converted ?tc1)
	    ;; (bare-sequence -)
	    (LF (% ?sort (class ?c2))) (CASE ?c) (constraint ?con2) (mass ?m)
	    (sort (? !sort unit-measure)))
     (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
     (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
     (logical-and (in1 ?gen1) (in2 ?gen2) (out ?gen))
     )

     ; 20181213: changed head from the first NP to conj in the next four rules so that we can pass on its lex
     ;;  simple conjuncts/disjunct of NPS, e.g., the dog and the cat, the horse or the cow
     ((NP (ATTACH ?a) (var ?v) (agr ?agr-out) ;(agr 3p) ; the ice and the fire could be 3s or 3p
	  (SEM ?sem) (gerund ?ger) (mass ?m1) ; should really be some combination of m1 and m2
      (LF (% Description (Status ?status-out) (var ?v) 
	     (class ?class)
	     (constraint (& (operator ?op) (sequence (?v1 ?v2))))
	     (sem ?sem) (CASE ?c)
	     (mass ?m1) 
	     ))
      (COMPLEX +)
      (SORT PRED) (wh ?wh)
      (generated ?generated)
       )
     -two-np-conjunct> 
     (NP (SEM ?s1) (VAR ?v1) (agr ?agr)  (complex -) (expletive -) ;;(bare-np ?bnp)
	    (generated ?gen1)  (time-converted ?tc1) (gerund ?ger) (wh ?wh); ok: which cats and which dogs; ok: which <cats and dogs> (use -two-n1-conjunct>); not ok: <which cat> and dog 
	    ;; (bare-sequence -)
	    (LF (% ?sort (class ?c1) (status ?status))) (CASE ?c) (constraint ?con) (mass ?m1) ;; allowing mismatch on mass
	    (sort (? !sort unit-measure)) ;; no unit measure here since they form sub-NPs [500 mb] & we want the top-level [500 mb of ram] 	    
	    )
      (head (conj (SEQ +) (LF ?op) (var ?v) )) ;;(status ?status))
      (NP (SEM ?s2) (VAR ?v2) (agr ?agr1)  (complex -) (expletive -)
       (bare-np -)  ;; bare-NP should go through N!-conjunct, not NP-conjunct
       (generated ?gen2)  (time-converted ?tc1)  (gerund ?ger) (wh ?wh)
       ;; (bare-sequence -)
       (LF (% ?sort (class ?c2) (status ?status2))) (CASE ?c) (constraint ?con2) (mass ?m2) ;; allowing mismatch on mass -- e.g. "fatigue and weakness"
       (sort (? !sort unit-measure)))
      (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
      (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
      (logical-and (in1 ?gen1) (in2 ?gen2) (out ?generated))
      (combine-status (in1 ?status) (in2 ?status2) (out ?status-out))
      (recompute-agr (in1 ?agr) (in2 ?agr1) (out ?agr-out))
      )

    
    ;;  But not construction, e,g,. apples but not pears, apples not pears, 
     ((NP (ATTACH ?a) (var ?v) (agr ?agr) (SEM ?sem) (gerund ?ger) 
      (LF (% Description (Status ?status) (var ?v) 
	     (class ?c1)
	     (constraint (& (operator but-not) (sequence (?v1)) (except ?exception)))
	     (sem ?sem) (CASE ?c)
	     (mass ?m1) 
	     ))
      (COMPLEX +) (SORT PRED)
      (generated ?generated)  (time-converted ?tc1) 
       )
     -np-but-not-conjunct> 
     (NP (SEM ?s1) (VAR ?v1) (agr ?agr)  (complex -) (expletive -) ;;(bare-np ?bnp)
	    (generated ?generated)  (time-converted ?tc1) (gerund ?ger)
	    ;; (bare-sequence -)
	    (LF (% ?sort (class ?c1) (status ?status))) (CASE ?c) (constraint ?con) (mass ?m2) ;; allowing mismatch on mass
	    (sort (? !sort unit-measure)) ;; no unit measure here since they form sub-NPs [500 mb] & we want the top-level [500 mb of ram] 	    
	    )
      (head (conj (but-not +) (var ?v)))
      (np (var ?exception))
      )
;;  But not construction, e,g,. apples but not pears, apples not pears, 
     ((NP (ATTACH ?a) (var ?v) (agr ?agr) (SEM ?s1) (gerund ?ger) 
      (LF (% Description (Status ?status) (var ?v) 
	     (class ?c1)
	     (constraint (& (operator ont::and) (sequence (?v1)) (except ?exception)))
	     (sem ?s1) (CASE ?c)
	     (mass ?m1) 
	     ))
      (COMPLEX +) (SORT PRED)
      (generated ?generated)  (time-converted ?tc1) 
       )
     -np-but-not-conjunct-with-comma> 
     (NP (SEM ?s1) (VAR ?v1) (agr ?agr)  (complex -) (expletive -) ;;(bare-np ?bnp)
	    (generated ?generated)  (time-converted ?tc1) (gerund ?ger)
	    ;; (bare-sequence -)
	    (LF (% ?sort (class ?c1) (status ?status))) (CASE ?c) (constraint ?con) (mass ?m2) ;; allowing mismatch on mass
	    (sort (? !sort unit-measure)) ;; no unit measure here since they form sub-NPs [500 mb] & we want the top-level [500 mb of ram] 	    
	    )
      (punc  (lex w::punc-comma))
      (head (conj (but-not +) (var ?v)))
      (np (var ?exception))
      )

        ;;  simple conjuncts/disjunct of NPS with exception + comma, e.g., the dog, but not the horse
     ((NP (ATTACH ?a) (var ?v) (agr 3p) (SEM ?sem) (gerund ?ger) 
      (LF (% Description (Status ?status) (var ?v) 
	     (class ?class)
	     (constraint (& (operator ?op) (sequence (?v1 ?v2)) (except ?exception)))
	     (sem ?sem) (CASE ?c)
	     (mass ?m1) 
	     ))
      (COMPLEX +) (SORT PRED)
      (generated ?generated)
       )
     -two-np-conjunct-with-exceptions> 
      (NP (SEM ?s1) (VAR ?v1) (agr ?agr)  (complex -) (expletive -) ;;(bare-np ?bnp)
	       (generated ?gen1)  (time-converted ?tc1) (gerund ?ger)
	    ;; (bare-sequence -)
	    (LF (% ?sort (class ?c1) (status ?status))) (CASE ?c) (constraint ?con) (mass ?m2) ;; allowing mismatch on mass
	    (sort (? !sort unit-measure)) ;; no unit measure here since they form sub-NPs [500 mb] & we want the top-level [500 mb of ram] 	    
	    )
      
      (head (conj (SEQ +) (LF ?op) (var ?v) )) ;;(status ?status))
      (NP (SEM ?s2) (VAR ?v2) (agr ?agr1)  (complex -) (expletive -) ;;(bare-np ?bnp)
	    (generated ?gen2)  (time-converted ?tc1)  (gerund ?ger)
	    ;; (bare-sequence -)
	    (LF (% ?sort (class ?c2))) (CASE ?c) (constraint ?con2) (mass ?m1) ;; allowing mismatch on mass -- e.g. "fatigue and weakness"
       (sort (? !sort unit-measure)))
      (conj (but-not +))
      (np (var ?exception))
      (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
      (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
      (logical-and (in1 ?gen1) (in2 ?gen2) (out ?generated))
    )
    
        ;; sequences in the bio domain especially can become an NP
     ((NP (ATTACH ?a) (var *) (agr 3p) (SEM ?sem) (class ?c1)
      (LF (% Description (status ont::definite) (var *) 
	     (class ?c1)
	     (constraint (& (sequence ?lf1)))
	     (sem ?sem) (CASE ?case1)
	     (mass ?m1) 
	     ))
      (COMPLEX +) (SORT PRED)
      (generated ?generated)
      )
     np-sequence> 1
      (head (NPSEQ (var ?v) (SEM ?sem) (lf ?lf1) (class ?c1) (CASE ?case) (mass ?m1)
		   (generated ?generated1) (separator (? p w::punc-slash w::punc-colon w::punc-minus w::punc-en-dash w::punc-minus))
		   (time-converted ?rule))))

    #|| ;;  tc1 without any separator -- needed for speech, should add there
    ((NPSEQ  (SEM  ?sem) (LF ?newlf) (AGR 3s) (CASE ?case) (mass ?m) (class ?class)
      (generated ?gen) (time-converted ?tc1)
      )
     np1-3-3> 0.98 ;; myrosia lowered the preference - we should not do it unless there's a good reason to, so that we don't infer stupid things in lattices
     (head (NPSEQ  (SEM ?s1) (LF ?lf) (MASS ?m) (class ?cl) (CASE ?case)
	    (generated ?gen1) (time-converted ?tc1)
	    ))      
     (NP (SEM ?s2) (VAR ?v2) (MASS ?m) (COMPLEX -) (bare-sequence -) (class ?c2) (CASE ?case) (expletive -)
      (generated ?gen2)  (time-converted ?tc1)) ;; MD 2008/03/06 Introduced restriction that only items with the same time-converted status can combine - i.e. don't mix number notation for times or non-times. 
     (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
     (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
     (simple-cons (in1 ?v2) (in2 ?lf) (out ?newlf))
     (logical-and (in1 ?gen1) (in2 ?gen2) (out ?gen))
     )||#

    ((NPSEQ  (SEM  ?sem) (LF ?newlf) (AGR 3s) (CASE ?case) (mass ?m) (class ?class)
      (generated ?gen) (time-converted ?tc1) (separator (? p w::punc-comma w::punc-slash w::punc-colon w::punc-minus w::punc-en-dash w::punc-minus))
      )
     npseq-add-next-comma> 1.02 
     (head (NPSEQ  (SEM ?s1) (LF ?lf) (MASS ?m) (class ?c1) (CASE ?case)
	    (generated ?gen1) (time-converted ?tc1) (separator (? p w::punc-comma w::punc-slash w::punc-colon w::punc-minus w::punc-en-dash w::punc-minus))
	    )) 
     (punc  (lex w::punc-comma))
     (NP (SEM ?s2) (VAR ?v2) (MASS ?m) (COMPLEX -) (name-mod -) (bare-sequence -) (class ?c2) (CASE ?case) (expletive -)
      (generated ?gen2)  (time-converted ?tc1)) ;; MD 2008/03/06 Introduced restriction that only items with the same time-converted status can combine - i.e. don't mix number notation for times or non-times. 
     (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
     (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
     (simple-cons (in1 ?v2) (in2 ?lf) (out ?newlf))
     (logical-and (in1 ?gen1) (in2 ?gen2) (out ?gen))
     )


    ((NPSEQ  (SEM  ?sem) (LF ?newlf) (AGR 3s) (CASE ?case) (mass ?m) (class ?class)
      (generated ?gen) (time-converted ?tc1) (separator (? p w::punc-slash w::punc-colon w::punc-minus w::punc-en-dash  w::punc-minus))
      )
     npseq-add-next> 1.02 
     (head (NPSEQ  (SEM ?s1) (LF ?lf) (MASS ?m) (class ?c1) (CASE ?case)
	    (generated ?gen1) (time-converted ?tc1) (separator (? p w::punc-slash w::punc-colon w::punc-minus w::punc-en-dash  w::punc-minus))
	    )) 
     (punc  (lex (? p w::punc-slash w::punc-colon  w::punc-minus w::punc-en-dash  w::punc-minus)))
     (NP (SEM ?s2) (VAR ?v2) (MASS ?m) (COMPLEX -) (simple +)   ;; simple is a bare-NP or name
      (name-mod -) (bare-sequence -) (class ?c2) (CASE ?case) (expletive -)
      (generated ?gen2)  (time-converted ?tc1)) ;; MD 2008/03/06 Introduced restriction that only items with the same time-converted status can combine - i.e. don't mix number notation for times or non-times. 
     (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
     (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
     (simple-cons (in1 ?v2) (in2 ?lf) (out ?newlf))
     (logical-and (in1 ?gen1) (in2 ?gen2) (out ?gen))
     )
    
    
    ;;  Conjunctions with double conjuncts:: either ... or, neither ... nor, both ... and
    ;;  use sequence constructions (SEQ +),  e.g., X, Y and/or Z.
    ;;  we control the types carefully to restrict the possibilities
    
    ;; Note the difference from the main conj rule: we enforce case
    ;; agreement. We really should enforce it in the other case, too
    ;; the rule makes a simplification that the conjunction should have the agreement feature
    ;; the real rules are kind of complicated and therefore difficult to enforce carefully
    ; TEST: both dogs and cats
    ; TEST: neither dogs nor cats
    ; TEST: either dogs or cats
    ((NP (ATTACH ?a) (var ?v) (agr ?cagr) (SEM ?sem)  
         (LF (% Description (Status ?cstat) (var ?v) 
                (class ?class)
                (constraint (& (operator ?cop) (sequence (?v1 ?v2))))
                (sem ?sem) (CASE ?case1)
                (mass ?m1) 
                )) 
         (COMPLEX +) (SORT PRED))
     -np-double-conj1> 
     (conj (SEQ +) (SUBCAT1 NP) (SUBCAT2 ?wlex) (SUBCAT3 NP) 
      (var ?v) 
      (operator ?cop) (status ?cstat) (agr ?cagr))      
     (NP (var ?v1) (SEM ?s1) (lf ?lf1) (class ?c1) (CASE ?case) (mass ?m1) (constraint ?con1))
     (word (lex ?wlex))
     (head (NP (VAR ?v2) (SEM ?s2) (ATTACH ?a) (lf ?lf2) 
	       (LF (% ?d (class ?c2))) (CASE ?case) 
	       (mass ?m2) (constraint ?con2)
	       (sort (? !sort unit-measure)) ;; no unit-measure here since they form sub-NPs & we want the whole one
	       ))
     (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
     (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
     ;;     (simple-cons (in1 ?lf2) (in2 ?lf1) (out ?members))
     )
 
    ;; TEST: such dogs as collies
    ;; note that the agr can differ in the 2 NPs: such great issues as the federal budget deficit
    ((NP (var ?v1) (agr ?agr1) (SEM ?s1)
	 (LF (% Description (Status ?st1)
		(var ?v1) (class ?c1)
		(constraint ?new)
                )) 
         (COMPLEX +) (SORT PRED))
     -such-X-as-y>
     (word (lex such))
     (head (NP (var ?v1) (SEM ?s1) (CASE ?case) (agr ?agr1) (mass ?m1) (constraint ?con1)
	       (lf (% ?typ (class ?c1) (status (? st1 ont::indefinite ont::indefinite-plural))))
	       ))
     (pp (lex as))
     (NP (VAR ?v2) (SEM ?s2) (agr ?agr2) (lf ?lf2) 
            (LF (% ?d (class ?c2))) (CASE ?case) 
            (mass ?m2) (constraint ?con2)
            (sort (? !sort unit-measure)) ;; no unit-measure here since they form sub-NPs & we want the whole one
            )
    (add-to-conjunct (val (MODS (% *PRO* (status ont::f) (var *) (class (:* ont::exemplifies w::such-as)) (constraint (& (FIGURE ?v1) (GROUND ?v2)))))) (old ?con1) (new ?new))
     )

    ;; Myrosia added atype central restriction. This may be a little bit too harsh, but it is farily reasonable
    ;; and should be changed only if there are compelling counterexamples for predicative-only or attributive-only adjectives
    ;; which I could not find
    ;; Now handles conjunctions and disjunctions
    ;; predicative-only: The cat is alone and afraid
    ;; attributive-only: The main and only reason
    ;; postpositive: the cars available and unavailable
    ((ADJP (ARG ?arg) (argument ?a) (sem ?sem) (atype ?atype1) ;(atype central)
	   (VAR *) ;(COMPLEX +) -- removed to allow complex adj prenominal modification, e.g. "a natural and periodic state of rest"
	   (SORT PRED)
      (LF (% PROP (CLASS ?class) (VAR *) (sem ?sem) (CONSTRAINT (& (sequence (?v1 ?v2)) (operator ?conj))) ;;?members)))
	     (transform ?transform) (sem ?sem)
	     )))
          
     -adj-conj1>
     (ADJP (arg ?arg) (argument ?a) (VAR ?v1) 
      ;(lf (% PROP (class ?c1))) (sem ?s1) (atype central) (post-subcat -)
      (lf (% PROP (class ?c1))) (sem ?s1) (atype ?atype1) (post-subcat -)
      (set-modifier -)
      )
     (CONJ (LF ?conj) (but-not -) (but -))
     (ADJP (arg ?arg)  (argument ?a) (VAR ?v2) 
      ;(LF (% PROP (class ?c2))) (sem ?s2) (atype central) (post-subcat -)
      (LF (% PROP (class ?c2))) (sem ?s2) (atype ?atype2) (post-subcat -)
      (set-modifier -)
      )
     (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
     (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
     ;;(simple-cons (in1 ?v2) (in2 ?lf1) (out ?members))
     )

    ((ADJP (ARG ?arg) (argument ?a) (sem ?sem) (atype ?atype1) ;(atype central)
	   (VAR *) ;(COMPLEX +) -- removed to allow complex adj prenominal modification, e.g. "a natural and periodic state of rest"
	   (SORT PRED)
      (LF (% PROP (CLASS ?class) (VAR *) (sem ?sem) (CONSTRAINT (& (sequence (?v1 ?v2 ?v3)) (operator ?conj))) ;;?members)))
	     (transform ?transform) (sem ?sem)
	     )))
          
     -adj-triple-conj1> 1
     (ADJP (arg ?arg) (argument ?a) (VAR ?v1) 
      ;(lf (% PROP (class ?c1))) (sem ?s1) (atype central) (post-subcat -)
      (lf (% PROP (class ?c1))) (sem ?s1) (atype ?atype1) (post-subcat -)
      (set-modifier -)
      )
     (punc (lex w::punc-comma))
     (ADJP (arg ?arg) (argument ?a) (VAR ?v2) 
      ;(lf (% PROP (class ?c1))) (sem ?s1) (atype central) (post-subcat -)
      (lf (% PROP (class ?c2))) (sem ?s2) (atype ?atype2) (post-subcat -)
      (set-modifier -)
      )
     (CONJ (LF ?conj) (but-not -) (but -))
     (ADJP (arg ?arg)  (argument ?a) (VAR ?v3) 
      ;(LF (% PROP (class ?c2))) (sem ?s2) (atype central) (post-subcat -)
      (LF (% PROP (class ?c3))) (sem ?s3) (atype ?atype3) (post-subcat -)
      (set-modifier -)
      )
     (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?s4))
     (sem-least-upper-bound (in1 ?s3) (in2 ?s4) (out ?sem))
     (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?c4))
     (class-least-upper-bound (in1 ?c3) (in2 ?c4) (out ?class))
     ;;(simple-cons (in1 ?v2) (in2 ?lf1) (out ?members))
     )

    ((ADJP (ARG ?arg) (argument ?a) (sem ?sem) (atype ?atype1) ;(atype central)
	   (VAR *) ;(COMPLEX +) -- removed to allow complex adj prenominal modification, e.g. "a natural and periodic state of rest"
	   (SORT PRED)
      (LF (% PROP (CLASS ?class) (VAR *) (sem ?sem) (CONSTRAINT (& (sequence (?v1 ?v2 ?v3)) (operator ?conj))) ;;?members)))
	     (transform ?transform) (sem ?sem)
	     )))
          
     -adj-triple-conj-w-comma> 1
     (ADJP (arg ?arg) (argument ?a) (VAR ?v1) 
      ;(lf (% PROP (class ?c1))) (sem ?s1) (atype central) (post-subcat -)
      (lf (% PROP (class ?c1))) (sem ?s1) (atype ?atype1) (post-subcat -)
      (set-modifier -)
      )
     (punc (lex w::punc-comma))
     (ADJP (arg ?arg) (argument ?a) (VAR ?v2) 
      ;(lf (% PROP (class ?c1))) (sem ?s1) (atype central) (post-subcat -)
      (lf (% PROP (class ?c2))) (sem ?s2) (atype ?atype2) (post-subcat -)
      (set-modifier -)
      )
     (punc (lex w::punc-comma))
     (CONJ (LF ?conj) (but-not -) (but -))
     (ADJP (arg ?arg)  (argument ?a) (VAR ?v3) 
      ;(LF (% PROP (class ?c2))) (sem ?s2) (atype central) (post-subcat -)
      (LF (% PROP (class ?c3))) (sem ?s3) (atype ?atype3) (post-subcat -)
      (set-modifier -)
      )
     (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?s4))
     (sem-least-upper-bound (in1 ?s3) (in2 ?s4) (out ?sem))
     (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?c4))
     (class-least-upper-bound (in1 ?c3) (in2 ?c4) (out ?class))
     ;;(simple-cons (in1 ?v2) (in2 ?lf1) (out ?members))
     )

    ;; hungry but not angry, happy but sad, ...
    ((ADJP (ARG ?arg) (argument ?a) (sem ?sem) (atype central)
	   (VAR ?v1) ;(COMPLEX +) -- removed to allow complex adj prenominal modification, e.g. "a natural and periodic state of rest"
	   (SORT PRED)
      (LF (% PROP (CLASS ?class) (VAR ?v1) (sem ?sem) (constraint ?newcon)
	     (transform ?transform) 
	     )))
          
     -adj-but-conj1>
     (ADJP (arg ?arg) (argument ?a) (VAR ?v1) (sem ?sem) 
      (lf (% PROP (class ?class) (var ?v1) (sem ?sem) (constraint ?con)))
      (atype central) (post-subcat -)
      (set-modifier -)
      )
     (CONJ (LF ?conj) (but +) (var ?vc))
     (ADJP (arg ?arg)  (argument ?a) (VAR ?v2) (sem ?s2) (atype central) (post-subcat -)
      (set-modifier -)
      )
     (add-to-conjunct (val (:qualification (% *PRO* (status ont::F) (var ?vc) (class ?conj) (constraint (& (Figure ?v1) (ground ?v2)))
						  )))
      (old ?con) (new ?newcon))
     
     )

    ;; either happy or sad
    ((ADJP (arg ?arg) (argument ?a) (sem ?sem) (var *) (atype central)
      (sort pred)
      (lex ?hlex) (headcat ?hcat) (fake-head 0) ;; aug-trips
      (LF (% PROP (CLASS ?clf) (VAR *) (sem ?sem) 
	     (constraint (& (sequence (?v1 ?v2)))) 
	     (transform ?transform) (sem ?sem))
	  ))
     
     -adj-double-conj1> 
     (conj (SEQ +) (SUBCAT1 ADJP) (SUBCAT2 ?wlex) (SUBCAT3 ADJP) 
      (var ?v) (lf ?clf)
      (operator ?cop)
      )                
     (adjp (var ?v1) (arg ?arg) (argument ?a) (SEM ?s1) (lf ?lf1) (atype central) (post-subcat -)
      (set-modifier -)
      (lex ?hlex) (headcat ?hcat) ) ;; aug-trips
     (word (lex ?wlex))
     (adjp (var ?v2) (arg ?arg) (argument ?a) (SEM ?s2) (lf ?lf2) (atype central) (post-subcat -)
      (set-modifier -)
      )
     (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
     ;;(simple-cons (in1 ?v2) (in2 ?lf1) (out ?members))
     )            
    ))

;;;  ((ADJP (ARG ?arg) (VAR ?v) (sem ?sem)
;;;      (LF (% PROP (CLASS ?lf) (VAR ?v) (CONSTRAINT ?newc)
;;;	     (transform ?transform) (sem ?sem)
;;;	     )))
;;;     -adj-pred>
;;;     (head (ADJ (LF ?lf) (SUBCAT -) (sem ?sem) (SORT PRED) (ARGUMENT-MAP ?argmap)
;;;		(transform ?transform) (constraint ?con)))
;;;     (append-conjuncts (conj1 ?con) (conj2 (& (?argmap ?arg)))
;;;		       (new ?newc))
;;;     )

(parser::augment-grammar	 
  '((headfeatures
     (N1 lf headcat transform set-restr refl abbrev subcat subcat-map)
     )
;;  simple conjuncts/disjunct of N1, e.g., the (dog and cat)
    ((N1 (ATTACH ?a) (var ?v) (agr ?agr-out) ;(agr 3p) ; ice and fire could be 3s or 3p
	 (SEM ?sem) (gerund ?ger) 
      (Status ?status-out)
      (class ?class)
      (restr (& (operator ?op) 
		(sequence ((% *PRO* (status ?status-out) (var ?v1) (class ?c1) (constraint ?con) (sem ?s1) (lex ?lex1))
			   (% *PRO* (status ?status-out) (var ?v2) (class ?c2) (constraint ?con2) (sem ?s2) (lex ?lex2))))))
      (CASE ?c)
      (mass ?m1) 
      (COMPLEX +) 
      (sort (? !sort unit-measure)) ;(SORT PRED)
      (generated ?generated)
      (lex ?lex) ;(lex ?op)
      )
     -two-n1-conjunct> 
     (head (N1 (SEM ?s1) (VAR ?v1) (agr ?agr)  (complex -) (expletive -) ;;(bare-np ?bnp)
	    (generated ?gen1)  (time-converted ?tc1) (gerund ?ger)
	    (lex ?lex1)
	    (class ?c1) (status ?status) (CASE ?c) (restr ?con) (mass ?m2) ;; allowing mismatch on mass
	    (sort (? !sort unit-measure)) ;; no unit measure here since they form sub-NPs [500 mb] & we want the top-level [500 mb of ram]
	    ))
     (conj (SEQ +) (LF ?op) (lex ?lex) (var ?v) ) ;;(status ?status))
     (N1 (SEM ?s2) (VAR ?v2) (agr ?agr1)  (complex -) (expletive -) ;;(bare-np ?bnp)
	    (generated ?gen2)  (time-converted ?tc1)  (gerund ?ger)
	     (lex ?lex2)
	    (class ?c2) (status ?status2) (CASE ?c) (RESTR ?con2) (mass ?m1) ;; allowing mismatch on mass -- e.g. "fatigue and weakness"
	    (sort (? !sort unit-measure)))
     (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
     (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
     (logical-and (in1 ?gen1) (in2 ?gen2) (out ?generated))
     (combine-status (in1 ?status) (in2 ?status2) (out ?status-out))
     (recompute-agr (in1 ?agr) (in2 ?agr1) (out ?agr-out))
     )
    
    ;; 
    ((N1SEQ  (SEM ?sem) ;(LF (?v1 ?v2))
	     (restr (& ;(operator ?op)
		(sequence ((% *PRO* (status ?status-out) (var ?v1) (class ?c1) (constraint ?con) (sem ?s1) (lex ?lex1))
			   (% *PRO* (status ?status-out) (var ?v2) (class ?c2) (constraint ?con2) (sem ?s2) (lex ?lex2))))))
	     (AGR ?agr-out) (mass ?m) (class ?class) (case ?c) (status ?status-out)
      (generated ?gen)  (time-converted ?tc1) (separator w::punc-comma)
      )
     -n1seq-initial-sequence-comma> 1.01
     (head (N1 (SEM ?s1) (VAR ?v1) (agr ?agr) ;;(agr ?agr)   ;; AGR is not reliably determined for proper names
	       (complex -) (headless -) (expletive -) ;;(bare-np ?bnp)
	    (generated ?gen1)  (time-converted ?tc1)
	    ;; (bare-sequence -)
	    (class ?c1) ;(LF (% ?sort (class ?c1)))
	    (CASE ?c) (restr ?con) (mass ?m) (status ?status) (lex ?lex1)
	    (sort (? !sort unit-measure)) ;; no unit measure here since they form sub-NPs [500 mb] & we want the top-level [500 mb of ram] 	    
	    ))
     (punc (lex w::punc-comma))
     (N1 (SEM ?s2) (VAR ?v2) (agr ?agr1) ;;(agr ?agr)  
      (complex -) (headless -) (expletive -) ;;(bare-np ?bnp)
	    (generated ?gen2)  (time-converted ?tc1)
	    ;; (bare-sequence -)
	    (class ?c2) ;(LF (% ?sort (class ?c2)))
	    (CASE ?c) (restr ?con2) (mass ?m) (status ?status2) (lex ?lex2)
	    (sort (? !sort unit-measure)))
     (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
     (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
     (logical-and (in1 ?gen1) (in2 ?gen2) (out ?gen))
     (combine-status (in1 ?status) (in2 ?status2) (out ?status-out))
     (recompute-agr (in1 ?agr) (in2 ?agr1) (out ?agr-out))
     )

    ;;
    ((N1SEQ  (SEM  ?sem) ;(LF ?newlf)
	     (agr ?agr-out) ;(AGR 3s)
	     (CASE ?case) (mass ?m) (class ?class) (status ?status-out)
	     (generated ?gen) (time-coverted ?tc1) (separator (? p w::punc-comma w::punc-slash w::punc-colon w::punc-minus w::punc-en-dash w::punc-minus))
	     (restr (& (sequence ?members)))
	     (COMPLEX +) (SORT PRED)	     
      )
     n1seq-add-next-comma> 1.02 
     (head (N1SEQ  (SEM ?s1) (restr (& (sequence ?seq))) ;(LF ?lf)
		   (MASS ?m) (class ?c1) (CASE ?case) (agr ?agr) (status ?status)
	    (generated ?gen1) (time-converted ?tc1) (separator (? p w::punc-comma w::punc-slash w::punc-colon w::punc-minus w::punc-en-dash w::punc-minus))
	    )) 
     (punc  (lex w::punc-comma))
     (N1 (SEM ?s2) (VAR ?v2) (MASS ?m) (COMPLEX -) (name-mod -) (bare-sequence -) (class ?c2) (CASE ?case) (expletive -)
	 (agr ?agr1) (status ?status2) (restr ?con) (lex ?lex2) (sort (? !sort unit-measure)) (complex -)
      (generated ?gen2)  (time-converted ?tc1)) ;; MD 2008/03/06 Introduced restriction that only items with the same time-converted status can combine - i.e. don't mix number notation for times or non-times. 
     (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
     (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
     (logical-and (in1 ?gen1) (in2 ?gen2) (out ?gen))
     (combine-status (in1 ?status) (in2 ?status2) (out ?status-out))
     (recompute-agr (in1 ?agr) (in2 ?agr1) (out ?agr-out))
     (simple-cons1 (in1 (% *PRO* (status ?status-out) (var ?v2) (class ?c2) (constraint ?con) (sem ?s2) (lex ?lex2)))
		   (in2 ?seq) (out ?members))
     )
    
    ;;  X and Y,  A, Y and Z
    ((N1 (ATTACH ?a) (var ?v) (agr ?agr-out) ;(agr 3p)
	 (SEM ?sem) (class ?class) (status status-out) (mass ?m1) (case ?case) (lex ?op)
	 #|
      (LF (% Description (Status ?status) (var ?v) 
	     (class ?class)
	     (constraint (& (operator ?op) (sequence ?members)))
	     (sem ?sem) (CASE ?case1)
	     (mass ?m1) 
	     ))
	 |#
	 (restr (& (operator ?op)
		(sequence ?members)))
	 (COMPLEX +) (SORT PRED)
      (generated ?generated)
      )
     n1-conj1> 
     (N1SEQ (var ?v1) (SEM ?s1) ;(lf ?lf1)
	    (class ?c1) (CASE ?case) (mass ?m1) (status ?status) (restr (& (sequence ?seq)))
      (generated ?generated1) (separator W::punc-comma) (agr ?agr)
      (time-converted ?tc1) ;; MD 2008/03/06 Introduced restriction that only items with the same time-converted status can combine - i.e. don't mix number notation for times or non-times. 
      )
     (conj (SEQ +) (LF ?op) (SUBCAT NP) (var ?v)) ;; (status ?status))
     (head (N1 (VAR ?v2) (SEM ?s2) (ATTACH ?a) (agr ?agr1) ;(lf ?lf2)
	       (class ?c2) (status ?status2) ;(LF (% ?d (class ?c2) (status ?status)))
	       (CASE ?case2) (mass ?m2) (restr ?con)
	    (generated ?generated2) (lex ?lex2)
	    (sort (? !sort unit-measure)) ;; no unit-measure here since they form sub-NPs & we want the whole one
	    (time-converted ?tc1)
	    (complex -)
	    ))
     (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
     (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
     (logical-and (in1 ?generated1) (in2 ?generated2) (out ?generated))
     (combine-status (in1 ?status) (in2 ?status2) (out ?status-out))
     (recompute-agr (in1 ?agr) (in2 ?agr1) (out ?agr-out))
     (simple-cons1 (in1 (% *PRO* (status ?status-out) (var ?v2) (class ?c2) (constraint ?con) (sem ?s2) (lex ?lex2)))
		   (in2 ?seq) (out ?members))
     )    
    
     
    ))
;; the rate/activity construction
;;  these rules handle complex constructions like "the binding rate/activity of Ras" where
;;   the arguments attach to the gerund rather than the rate/activity.  It works by storing
;;   the rate/activity predicate in a new feature RATE-ACTIVITY-NOM. All of the normal NP
;;   rules require this to be empty. The second rule below "consumes" the feature and builds
;;   the required LF 

(parser::augment-grammar	 
  '((headfeatures
     (N1 lf headcat transform set-restr refl abbrev subcat subcat-map) ; no lex/orig-lex
     )

    ;; this rule handles rate/activity constructions - e.g., the binding rate of ras on raf
    ;;  we basically store away the rate.activity predicate and continue parsing as though it
    ;; wasn't there
        ((N1 (SORT PRED) (COMPLEX +)
	  (gap -) (var ?v) (agr ?agr) (gerund ?ger)
	  (sem ?sem) (mass ?mass) (pre-arg-already ?npay)
	  (case ?case)
	  (class ?class) (lex ?lex) (orig-lex ?orig-lex)
	  (restr ?restr)
	  (subj ?subj)
	  (subj-map ?subjmap)
	  (dobj ?dobj)
	  (dobj-map ?dobjmap) ;; eliminate the dobj-map so we can't assign another
	  (comp3 ?comp3)
	  (comp3-map ?comp-map)
	  (rate-activity-nom ?ratelf) ; not passing up the orig-lex of this
	  (nomobjpreps ?nop)
	  (nomsubjpreps ?nsp)
      )
     -nom-rate> 1.0
	 (head (n1  (var ?v) (gap -) (aux -)(case ?case) (gerund ?ger) (agr ?agr)
		    (pre-arg-already ?npay)
		    (headless -)
		    (sem ?sem) (sem ($ F::SITUATION)) ; (f::type ont::event-of-change)))
		    (class ?class) (transform ?transform) (lex ?lex) (orig-lex ?orig-lex)
		    ;; these are dummy vars for trips-lcflex conversion, please don't delete
		    ;;(subj ?subj) (comp3 ?comp3) (iobj ?iobj) (part ?part)
		    (restr ?restr)
		    (subj ?subj)
		    (subj-map ?subjmap)
		    (dobj ?dobj)
		    (dobj-map ?dobjmap)
		    (comp3 ?comp3)
		    (comp3-map ?comp-map)
		    (generated -)
		    (rate-activity-nom -)
		    (agent-nom -)
		    (nomobjpreps ?nop)
		    (nomsubjpreps ?nsp)
		    ))
	 (n (lf ?ratelf) (sem ($ (? t F::ABSTR-OBJ F::SITUATION F::TIME)  ; (planting) date
				 ;(f::type (? x ONT::DOMAIN ONT::ACTING))))) ; ACTING: activity
				 (f::type (? x ONT::DOMAIN ONT::ACTIVITY-EVENT ONT::ABILITY-EVENT ONT::LEVEL ONT::QUANTITY
					     ONT::time-object))))) ; rate, height, activity, level, amount, (planting) date
	 )

    ;; this rule then inserts the rate/activity predicate once the arguments have been attached

         ((N1 (SORT PRED) (COMPLEX +)
	   (gap -) (var *) (agr ?agr) (gerund ?ger)
	   (sem ?newsem) (mass ?mass) (pre-arg-already ?npay)
	   (case ?case)
	   (class ?!pred) (lex ?!pred-w)
	   (restr (& (figure (% *PRO* (status ont::F) (var ?v) (class ?class)
				(lex ?lex) (orig-lex ?orig-lex)
				    (constraint ?restr) (sem ?sem)))))
	   (subj ?subj)
	   (subj-map ?subjmap)
	   ;;(dobj ?dobj)
	   ;;(dobj-map ?dobjmap) 
	   (rate-activity-nom -)
	   )
	  -insert-rate-pred>
	  (head (n1  (var ?v) (gap -) (aux -)(case ?case) (gerund ?ger) (agr ?agr)
		     (pre-arg-already ?npay) 
		     (sem ?sem) (sem ($ F::SITUATION)) ; (f::type ont::event-of-change)))
		     (class ?class) (transform ?transform) (lex ?lex) (orig-lex ?orig-lex)
		     ;; these are dummy vars for trips-lcflex conversion, please don't delete
		     ;;(subj ?subj) (comp3 ?comp3) (iobj ?iobj) (part ?part)
		     (restr ?restr)
		     (subj ?subj)
		     (subj-map ?subjmap)
		     (generated -)
		     (rate-activity-nom ?!pred)
		     (rate-activity-nom (:* ?!pred-t ?!pred-w)) ; this assumes the rate always has a w
		     ))
	  (compute-sem-features (lf ?!pred) (sem ?newsem))
	  )
    ))

;; GRAMMAR 4
;; allows changing of LF, agr, LF SEM and QUANT feature
;;
;;(cl:setq *grammar4*
(parser::augment-grammar
 '((headfeatures
    (NP SPEC QUANT VAR PRO Changeagr lex orig-lex headcat transform wh)
    ;;(PP headcat) ;; non-aug-trips settings
    (PREP headcat) ;; aug-trips
    ;;(PP VAR agr SEM KIND VAR2)
    )

 #||  ;; PP GAP    ;;;  8/13 I deleted this rule, it seems redundant with -pp1-gap> in adverbial-grammar.lisp
   
   ((PP (PTYPE ?pt) (LF ?lf)
     (lex ?pt) (headcat ?hc) ;; aug-trips
     (gap (% np (lf ?lf) (sem ?sem) (mass ?m)			   
	     (agr ?gapagr) 
	     (case (? case obj -))))
     )
    -pp-gap> 0.96
    (head (prep (LEX ?pt) (headcat ?hc))))||#
   
   ;; numbers (only -- number sequences use np-sequenc1e>
   ((NP (SORT PRED)
     (var ?v) (class ONT::NUMBER) ;(Class ONT::ANY-SEM) 
     (sem ($ (? ft f::abstr-obj) (f::container -) (F::scale ont::number-scale)
	     (F::tangible -) (f::type ont::number))) ;;(sem ($ (? ft f::phys-obj f::abstr-obj))) 
     (case (? cas sub obj -))
     (LF (% Description (status ont::number) (var ?v) (Sort Individual) ;(lex ?lf)
	    (CLASS ONT::NUMBER) ;;(Class ONT::REFERENTIAL-SEM) 
	    (constraint ?restr) 
	    (lex ?l) (val ?val)
	    (sem ($ (? ft f::abstr-obj) (f::container -) (F::scale ont::number-scale)
		    (F::tangible -) (f::type ont::number))) ; "container -" to discourage it from being used by IN-LOC for "in 2019"
	    ;; (sem ($ (? ft f::phys-obj f::abstr-obj))) 
	    (transform ?transform)
	    ))
     (postadvbl +) 
     (generated +) ;; do we need this for numbers? MD 2008/06/03 yes, because we need to have "bulbs 1 and 3", and only generated NP conjunctions go there
     (simple +)
     (mass ?mass)
     (constraint ?restr) 
     (nobarespec +) ;; bare numbers can't be specifiers     
     (agr 3s)  ; The answer *is* 5.
     )
    -np-number> 0.98
    (head (number (val ?lf) (lex ?l) (val ?val) (range -) (agr (? a 3s 3p));(number-only +)
		  (mass ?mass) (sem ?sem1) (restr ?restr) (var ?v)
		  ;(headcat (? !x ordinal))
		  ))
    )
   
   ;; a seven 
   ((NP (SORT PRED)
     (var ?v) (Class ONT::ANY-SEM) (sem ($ f::abstr-obj (f::intentional -) (f::information f::information-content))) (case (? cas sub obj -))
     (LF (% Description (status ont::indefinite) (var ?v) (Sort Individual) (lex ?lf)
	    (Class ONT::ORDERING) (constraint (& (:value ?val))) 
	    (lex ?l) 
	    (sem ($ f::abstr-obj))
	    (transform ?transform)
	    ))
     (postadvbl +) 
     (generated +) ;; do we need this for numbers? MD 2008/06/03 yes, because we need to have "bulbs 1 and 3", and only generated NP conjunctions go there
     (mass count)
     (nobarespec +) ;; bare numbers can't be specifiers     
     (agr (? a 3s 3p))
     )
    -np-score> 0.97
    (art (lex w::a))
    (head (number (lex ?l) (val ?val) (agr (? a 3s 3p));(number-only +)
		  (restr ?restr) (var ?v)))
    )

   ;;  seven out of ten
   ((NP (SORT PRED)
     (var ?v) (Class ONT::ANY-SEM) (sem ($ f::abstr-obj (f::intentional -) (f::information f::information-content))) (case (? cas sub obj -))
     (LF (% Description (status ont::indefinite) (var ?v) (Sort Individual) (lex ?lf)
	    (Class ONT::ORDERING) (constraint (& (:value ?val) (:range ?range)))
	    (lex ?l)
	    (sem ($ f::abstr-obj))
	    (transform ?transform)
	    ))
     (postadvbl +) 
     (generated +) ;; do we need this for numbers? MD 2008/06/03 yes, because we need to have "bulbs 1 and 3", and only generated NP conjunctions go there
     (mass count)
     (nobarespec +) ;; bare numbers can't be specifiers     
     (agr (? a 3s 3p))
     )
    -np-score-outof> 0.98
    ;;(art (lex w::a))
    (head (number (val ?val) (agr (? a 3s 3p));(number-only +)
	   (mass ?mass) (sem ?sem1) (restr ?restr) (var ?v)))
    (word (lex w::out))
    (word (lex W::of))
    (number (val ?range) (range -))
    )

   ((NP (SORT PRED)
     (var ?v) (Class ONT::ANY-SEM) (sem ($ (? ft f::phys-obj f::abstr-obj))) (case (? cas sub obj -))
     (LF (% Description (status ont::indefinite) (var ?v) (Sort Individual) (lex ?lf)
	    (Class ONT::ORDERING) (constraint (& (:value ?val) (:range ?range)))
	    (lex ?l)
	    (sem ($ f::abstr-obj))
	    (transform ?transform)
	    ))
     (postadvbl +) 
     (generated +) ;; do we need this for numbers? MD 2008/06/03 yes, because we need to have "bulbs 1 and 3", and only generated NP conjunctions go there
     (mass count)
     (nobarespec +) ;; bare numbers can't be specifiers     
     (agr (? a 3s 3p))
     )
    -np-a-score-outof> 0.98
    (art (lex w::a))
    (head (number (val ?val) (agr (? a 3s 3p));(number-only +)
	   (mass ?mass) (sem ?sem1) (restr ?restr) (var ?v)))
    (word (lex w::out))
    (word (lex W::of))
    (number (val ?range) (range -))
    )
   

	         
   ))

;; GRAMMAR 6 
;;
;; KIND/AMOUNT/QUAN OF ANY-SEM
;;
;;(cl:setq *grammar6*
(parser::augment-grammar
      '((headfeatures
	 (NP SPEC QUANT VAR agr PRO Changeagr lex orig-lex headcat transform wh)
	 (N1 lf lex orig-lex headcat transform set-restr refl abbrev nomobjpreps kr-type)) ;sem and var not headfeatures
    
   ;; certains NAMES (esp in the biology domain) are really treat like mass nouns
	;;   we need this for constructions with modifiers, like "phosphorylated HER3"
    ((n1 (SORT PRED)
      (var ?v) (Class ?lf) (sem ?sem) (agr ?agr)
      ;;(agr 3s) 
      (case (? cas sub obj -))
      (derived-from-name +)  ;; we do this so that this N1 doesn't go through the bare-np rule, since we have the name-np already. But this N1 does allow relative clauses, as in "Ras that is bound to Raf"
      (status ont::name) (lex ?l) (restr ?con) ;(restr (& (w::name-of ?l)))
      (mass MASS) (subcat ?subcat)
      )
     -n1-from-name> 1
     (head (name (lex ?l) (sem ?sem) 
		 (sem ($ (? type f::PHYS-OBJ f::situation) (f::type (? x ont::molecular-part ont::cell-part ont::chemical ont::physical-process ont::organization))))
		 (var ?v) (agr ?agr) ;;(agr 3s) 
		 (lf ?lf) (class ?class)
	    (full-name ?fname) (time-converted ?tc)
	    ;; swift 11/28/2007 removing gname rule & passing up generated feature (instead of restriction (generated -))
	    (generated -)  (transform ?transform) (title -)
	    (restr ?restr) (subcat ?subcat)
	    ))
     (add-to-conjunct (val (w::name-of ?l)) (old ?restr) (new ?con))
     )

	;;  HEADLESS CONSTRUCTIONS

	;; e.g., the first three, the three,
	((NP (SORT PRED) (CLASS ?c) (VAR ?v) (sem ?subcatsem) (case (? case SUB OBJ)) (N-N-MOD +) (AGR 3p) (Headless +)
	    (lf (% description (status ?status) (var ?v) (sort SET)
		    (class ont::referential-sem) ;(Class ont::ANY-SEM)
		    (constraint ?con)
		    (sem ?subcatsem) 
		    ))
	  (postadvbl +)
	  )
	 -NP-missing-head-plur> .98 
	 (head (spec (poss -)  (restr ?restr) (mass count)(status ?status)
		     (LF ?spec) (arg ?v) (agr 3p) (var ?v) ;;(NObareSpec -)       removed to handle "the three (arrived)"  jfa 5/10
		     ))
	 (CARDINALITY (var ?card) (AGR 3p))
	 (add-to-conjunct (val (size ?card)) (old ?restr) (new ?con))
	 )

	;; e.g.,  many, a few, ...
	((NP (SORT PRED) (CLASS ?c) (VAR ?v) (sem ?subcatsem) (case (? case SUB OBJ)) (N-N-MOD +) (AGR 3p) 
	     (lf (% description (status ?newspec) ;(status ?status)
		    (var ?v) (sort SET)
		    (class ont::referential-sem) ;(Class ont::ANY-SEM)
		    (constraint ?restr)
		    (sem ?subcatsem) 
		    ))
	  (postadvbl +) (headless +)
	  )
	 -NP-missing-head-plur2> .98 ;.96 ;; Myrosia lowered the preference to be lower than wh-setting1-role, with which this competes on "be" questions
	 (head (spec (poss -) (restr ?restr) (mass count) (status ?status)
		     (LF ?spec) (arg ?v) (agr 3p) (var ?v) (nobarespec -)
		     ))
	 (recompute-spec (spec ?status) (agr 3p) (result ?newspec))
	 )
        
	;;  e.g., some (as in some pain)  -- we treat these as pre-referential
	((NP (SORT PRED) (CLASS ?c) (VAR ?v) (sem ?subcatsem) (case (? case SUB OBJ))
	     (lf (% description (status ?spec) (var ?v) (sort INDIVIDUAL)
		    (class ont::referential-sem) ;(Class ont::Any-sem)
		    (constraint ?restr)
		    (sem ?subcatsem) 
		    ))
	  (headless +)
	  )
	 -NP-missing-head-mass> .96
	 (head (spec (poss -) (restr ?restr) (LF ?spec) (arg ?v) (agr 3s)
		     (var ?v) (mass mass) (NObareSpec -)
		     (subcat (% ?x (SEM (? subcatsem ($ (? ss F::PHYS-OBJ F::SITUATION F::ABSTR-OBJ))))))))
         )
        
	;;  e.g., the two at avon
	;;  only allowed with determiners that involve a SIZE
	((NP (SORT PRED) (CLASS ?c) (VAR ?v) (sem ?s) (case (? case SUB OBJ))
	     (lf (% description (status ?spec) (var ?v) (sort SET)
		    (class ont::referential-sem) ;(Class ont::Any-sem)
		    (constraint ?con)
		    (sem ?s)
		    ))
	  (postadvbl +) (headless +)
	  )
	 -NP-missing-head-mod-plur> .96
	 (head (spec  (poss -) (restr ?restr) ;;(SUBCAT (% ?x (SEM ?s)))
		(lf ?spec) (arg ?v) (agr 3p) (var ?v)
		;;(postadvbl +)
		)) ;; (NObareSpec -)))
	 (CARDINALITY (var ?card) (AGR 3p))
         (PRED (LF ?l1) (ARG ?v) ;(ARG *) ;;SORT SETTING) 
	       (var ?advvar) (ARGUMENT (% NP (sem ?s))))
	 (append-conjuncts (conj1 (& (size ?card) (mods ?advvar))) (conj2 ?restr) (new ?con))
	 )

#||	;;  e.g., some at avon
	;;  only allowed with determiners that involve a CARD
	((NP (SORT PRED) (CLASS ?c) (VAR ?v) (sem ?s) (case (? case SUB OBJ))
	     (lf (% description (status ont::indefinite-plural) (var ?v) (sort SET)
		    (Class ont::Any-sem)
		    (constraint ?con)
		    (sem ?s)
		    ))
	  ;;(postadvbl +)
	  )
	 -NP-missing-head-mod-plur2> .96
	 (head (spec  (poss -) (card ?!card) (restr ?restr) ;;(SUBCAT (% ?x (SEM ?s)))
		(lf ?spec) (arg ?v) (agr |3P|) (var ?v)
		)) ;; (NObareSpec -)))
         (PRED (LF ?l1) (ARG ?v) ;;SORT SETTING) 
	  (var ?advvar) (ARGUMENT (% NP (sem ?s))))
	 (append-conjuncts (conj1 (& (size ?!card) (mods ?advvar))) (conj2 ?restr) (new ?con))
	 )
||#
	;;  The green two, the largest three, ...
	((NP (SORT PRED) (CLASS ?c) (VAR ?v) (sem ?s) (case (? case SUB OBJ))  (headless +)
	     (lf (% description (status ?spec) (var ?v) (sort SET) 
		    (class ont::referential-sem) ;(Class ont::Any-sem) 
		    (constraint ?con)
		    (sem ?s)
		    ))
	  (postadvbl +)
	  )
	 -NP-missing-head-number> .96
	 (head (spec  (poss -) (restr ?restr)
                      (lf ?spec) (arg ?v) (agr |3P|) (var ?v)))
	 (ADJP (LF ?l1) (ARG ?v) ;(ARG *)
	       (set-modifier -)
	  (var ?advvar) (ARGUMENT (% NP (sem ?s))))
	 (cardinality (var ?card) (agr |3P|))
	 (append-conjuncts (conj1 (& (size ?card) (mods ?advvar))) (conj2 ?restr) (new ?con))
	 )

	;; Eat (some/two) more.
	((N1 (SORT PRED) (class ont::referential-sem) ;(CLASS ONT::ANY-SEM)
	     (VAR *) (sem ?sem2)
	     (case (? case SUB OBJ))  (headless +) ; sem and var not headfeature
	     (restr (& (MOD (% *PRO* (status ont::f) (class ?s) (VAR **) (constraint (& (figure *) (ground (% *PRO* (var ***) (class ont::referential-sem) ;(class ONT::ANY-SEM)
													      )) ))))))
	  (postadvbl +) (agr ?agr) (mass ?mass) 
	  )
	 -N1-missing-head-more> .96
	 (head (quan (CARDINALITY -) (VAR ?v) (agr ?agr) (comparative +) (QOF ?qof) (QCOMP ?Qcomp)
		 (MASS ?mass) (Nobarespec ?nbs) (NoSimple ?ns) (npmod ?npm) (negation ?neg)
		 (LF ?s)))
	 )
	

	;;  The green two in the corner, the largest three of the houses, ...
	((NP (SORT PRED) (CLASS ?c) (VAR ?v) (sem ?s) (case (? case SUB OBJ)) (headless +)
	     (lf (% description (status ?spec) (var ?v) (sort SET)
		    (class ont::referential-sem) ;(Class ont::Any-sem) 
		    (constraint ?con)
		    (sem ?s)
		    ))
	  (postadvbl +)
	  )
	 -NP-missing-head-number-mod> .96
	 (head (spec  (poss -) (restr ?restr)
                      (lf ?spec) (arg ?v) (agr |3P|) (var ?v)))
	 (ADJP (ARG ?v) ;(ARG *) 
	  (var ?advvar1) (ARGUMENT (% NP (sem ?s))))
	 (cardinality (var ?card) (agr |3P|))
	 (PRED (ARG ?v) ;(ARG *) 
	  (var ?advvar2) (ARGUMENT (% NP (sem ?s))))
	 (append-conjuncts (conj1 (& (size ?card) (mods (?advvar1 ?advvar2)))) (conj2 ?restr) (new ?con))
	 )
	
	    

	))


;; various misc rules for collocations

;;(cl:setq *grammar8*
(parser::augment-grammar
      '((headfeatures
	 (NAME VAR NAME lex orig-lex transform headcat)
	 (ADV var transform headcat)
	 (conj var transform headcat)
	 (quan var transform headcat)
	 (adj var transform headcat)
	 (pause var lex orig-lex transform headcat)
	 (fp headcat)
	 )		

	;; rule that allows us to skip over filled pauses
	((pause (skip +))
	 -fp1>
	 (head (fp)))

	((puncpause (skip +) (lex ?lex)) ;; (? lex w::punc-comma w::punc-colon W::semi-colon W::punc-hashmark)))
	 -skip-punc1>
	 (head (W::punc (skip -) (lex (? lex w::punc-comma w::punc-colon W::punc-semicolon W::punc-hashmark w::punc-quotemark)))))

	))



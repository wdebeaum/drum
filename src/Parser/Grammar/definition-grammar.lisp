;;;;
;;;; robust.lisp
;;;;
;;;; Time-stamp: <Wed May 29 13:06:38 EDT 2019 james>
;;;;

(in-package :W)


(parser::augment-grammar
  '((headfeatures
     (NP CASE MASS NAME agr SEM PRO headcat)
    (vp vform agr sem lex headcat template)
    (cp vform var neg sem subjvar dobjvar lex headcat)
     (definition  vform neg sem subjvar dobjvar lex headcat)
    (utt sem setting subjvar dobjvar lex headcat))
    
    ;; CP with no GAP to have a dream
    ((definition (var ?v)  (lf ?lf))
      -defn-no-gap> .97
     (head (cp (var ?v) (lf ?lf) (subj-map (? !s ONT::NOROLE)) (ctype (? x w::s-to)) ;; make sure there's a subj-map (avoid parses for "there is" etc)
	       (subj (% ?xx (sem ?subjsem)))
      (subjvar (% *PRO* (var *) (class ont::ROLE-REF) (sem ?subjsem) (constraint (& (:context-rel :lsubj)))))
      (gap -)
      )))

    ;; CP with gap, preferred e.g., to push
    ((definition (var ?v) (lf ?lf))
      -defn-gap> 1
     (head (cp (var ?v) (lf ?lf)
	       (subj (% ?xx (sem ?subjsem)))
	       (dobj (% ?yy (sem ?dobjsem)))
	       (subjvar (% *PRO* (var *) (class ont::ROLE-REF) (sem ?subjsem) (constraint (& (:context-rel :lsubj)))))
	       (gap (% NP (gap -)
			(var (% *PRO* (var **) (class ont::ROLE-REF) (sem ?dobjsem)
				(constraint (& (:context-rel :dobj)))))))
		)))

    ;; CP passive with gap, e.g., to be eaten by
    ((definition (var ?v) (lf ?lf))
      -defn-passive-gap> 
     (head (cp (var ?v) (lf ?lf)
	       (subj (% ?xx (sem ?subjsem)))
	       (dobjvar -)
	       (subjvar (% *PRO* (var *) (class ont::ROLE-REF) (sem ?subjsem) (constraint (& (:context-rel :lsubj)))))
	       (comp3 (% ?comp3 (sem ?compsem)))
	       (gap (% NP (gap -)
			(var (% *PRO* (var **) (class ont::ROLE-REF) (sem ?compsem)
				(constraint (& (:context-rel :lcomp)))))))
	       (passive +)
		)))
    
    
    ;;  making bread, make bread   (no gap in VP)
    ((definition (var ?v) (lf ?lf))
      -defn-ing>
     (head (vp (var ?v) (lf ?lf) (vform (? vf w::ing w::base)) (gap -)
	       (subj (% ?xx (sem ?subjsem)))
	       (subjvar (% *PRO* (var *) (class ont::ROLE-REF) (sem ?subjsem)
			   (constraint (& (:context-rel :lsubj)))))
	       )))

    ;; ING with gap, preferred e.g., go into
    ((definition (var ?v) (lf ?lf))
      -defn-ing-GAP>
     (head (vp (var ?v) (lf ?lf) (vform (? vf w::ing w::base))
	       (subj (% ?xx (sem ?subjsem)))
	       ;(dobj (% ?yy (sem ?dobjsem)))
	       (subjvar (% *PRO* (var *) (class ont::ROLE-REF) (sem ?subjsem)
			   (constraint (& (:context-rel :lsubj)))))
	       (gap (% NP (gap -) ;(sem ?dobjsem)
		       (var (% *PRO* (var **) (class ont::ROLE-REF) (constraint (& (:context-rel :dobj))))))))
	       ))

    #|| ;; ING with VP gap, e.g. "to do well"
    ((definition (var ?v) (lf ?lf))
      -defn-ing-GAP-vp> .98
     (head (vp (var ?v) (lf ?lf) (vform (? vf w::ing w::base))
	       (subjvar (% *PRO* (var *) (class ont::ROLE-REF) (constraint (& (:context-rel :lsubj)))))
	       (gap (% VP (gap -)
		       (var (% *PRO* (var **) (class ont::ROLE-REF) (constraint (& (:context-rel :dobj))))))))
	       ))||#
    
    
    ;;complex definitions
    ((definition (var ?v1) 
      (complex +)
      (lf ?lf))
      -advbl-defn> .99
      (advbl (sort DISC))
      (head (definition (var ?v1) (lex ?lf))
     ))
    
    ((definition (var *) 
      (complex +)
      (lf (% prop (var *) (class ont::s-conjoined) (constraint (&  (OPERATOR ont::OR) (SEQUENCE (?v1 ?v2 ?v3)))))))
     -defn+2> 1.01
     (head (definition (complex -) (var ?v1)))
     (punc (lex w::punc-comma))
     (definition (complex -) (var ?v2))
     (punc (lex w::punc-comma))
     (definition (complex -) (var ?v3)))

    ;; ADJ definitions
    ((definition (var ?v) 
      (complex +)
      (lf ?lf))
      -defn-adj> 1
     (head (adjp (lf ?lf) (var ?v)
		 (arg (% *PRO* (var *) (class ont::ROLE-REF) (gap -) (constraint (& (:context-rel :figure))))))))

     ;; ADVBL definitions
    ((definition (var ?v) 
      (complex +)
      (lf ?lf))
      -defn-advbl> .96    ;; want to make sure VP interpretations are preferred
     (head (advbl (lf ?lf) (var ?v) (gap -)
		 (arg (% *PRO* (var *) (class ont::ROLE-REF) (gap -) (constraint (& (:context-rel :figure))))))))

    ;; rule to handle explicit argument typing e.g., "(of leaves) green"

    ((definition (var ?v) 
      (complex +)
      (lf (% prop (var ?v) (class ?class) (constraint ?newconstraint))))
     -defn-with-explicit-argdelcare>    
     (argdeclare (argrestriction ?x))
     (head (definition (lf (% prop (var ?v) (class ?class) (constraint ?constraint)))))
     (append-conjuncts (conj1 ?constraint) (conj2 (& (argrestriction ?x))) (new ?newconstraint)))
     

    ;; arguments in definitions using parens

    ((NP (SORT PRED) (generated +) (var *) (class ?class) (agr ?agr)
      (LF (% Description (Status PRO) (var *) (Sort Individual)
	     (class ONT::REFERENTIAL-SEM) ;(class ?class)
	     (lex ?l)   ;; LEX used to be ?fname, but this isn't used any more  JFA 5/04
	     (sem ($ ?s (f::type ONT::REFERENTIAL-SEM))) ;(sem ?sem)
	     (constraint (& (:proform w::something) (:like ?v)))
	     )))
     -arg-in-paren>
     (punc (lex w::start-paren))
     (head (NP (var ?v) (sem ?sem) (LF (% description (class ?class))) (agr ?agr) (lex ?l)))
     (punc (lex W::end-paren)))
      
    ;; phrases that identify arguments in definitions
    ((argdeclare (argrestriction ?v))
     -argdeclare1>
     (punc (lex w::start-paren))
     (advbl (lex w::of) (var ?v))
     (punc (lex W::end-paren)))

    ((argdeclare (argrestriction ?v))
     -argdeclare2>
     (punc (lex w::start-paren))
     (word (lex w::used)) 
     (advbl (lex w::of) (var ?v))
     (punc (lex W::end-paren)))

    ;; explicit metatalk rules
    
    ;; e.g., a quantifier meaning less than one
    ((definition (var ?v) (lf ?lf))
     -quantifier> 1.0
     (head (np (var ?v)
      (lex w::quantifier)
      )
     ))

    ;; quantifier meaning ...
     ((definition (var ?v) (lf (*pro* (var ?v) (status bare) (class ?class) (constraint ?con))))
     -quantifier-bare> 1.0
      (head (n1 (var ?v)
	     (lex w::quantifier) (constraint ?con) (class ?class)))
      )
    
    ;;  Here is a robust rule for ADJ+PP constructs - it overgenerates but is useful to starting to identify subcategoriztion patterns
    
     ((ADJP (ARG ?arg) (VAR ?v) (COMPLEX +) (atype (? atp postpositive predicative-only)) (gap ?gap)
       (argument ?argument)
      (LF (% PROP  (CLASS ?lf)
	     (VAR ?v) (CONSTRAINT (& (subcat ?subcat) (:figure ?arg) (?reln ?argv) (FUNCTN ?fn) (scale ?scale) (intensity ?ints) (orientation ?orient) 
						      ))
	     (transform ?transform) (sem ?sem)
	     )))
     -adj-pred-subcat-missing-in-defn> .96
      (head (ADJ (LF ?lf) (SUBCAT2 -) (post-subcat -)(VAR ?v) (comparative -)
		(SUBCAT -) (SUBCAT-MAP - )
		(ARGUMENT-MAP ?argmap) (argument ?argument)
		(functn ?fn)
		(SORT PRED)
		(sem ?sem) (sem ($ F::ABSTR-OBJ (f::scale ?scale) (F::intensity ?ints) (F::orientation ?orient)))
		(transform ?transform)
		))
      (PP (Ptype ?ptp) (var ?subcat))
      )

    ;; and a rule to pick up NP+PP ellipsis, e.g., thwart the passage of
    
     ((NP (ARG ?arg) (VAR ?v) (COMPLEX +) 
       (gap (% np (lf ?lf) (var ?gapvar) (gap -) (sort (? sort pred descr wh-desc)) 
	       (case (? case obj -)) (agr ?agr)))
       (LF (% description  (CLASS ?c) (status ?s) (sem ?sem)
	      (VAR ?v) (CONSTRAINT ?new)))
       (transform ?transform) (sem ?sem)
       )
      -NP-pp-ellipsis> .98
      (head (NP (LF (% description (var ?v) (status ?s) (class ?c) (sem ?sem) (constraint ?con)))
		(VAR ?v) (gap -)
		(SUBCAT -)
		(transform ?transform)
		))
      (prep (lex ?ptp))
      (add-to-conjunct  (val (:figure ?gapvar)) (old ?con) (new ?new))
      )

   #|| ;;  prescription rules
     ((NP (SORT PRED)
         (var ?v) (Class (:* ONT::PHARMACOLOGIC-SUBSTANCE ?lex)) (sem ?sem) (agr ?agr) (case (? cas sub obj -))
         (LF (% Description (Status Name) (var ?v) (Sort Individual)
                (class (:* ONT::PHARMACOLOGIC-SUBSTANCE ?lex)) (lex ?l) (sem ?sem)  ;; LEX used to be ?fname, but this isn't used any more  JFA 5/04
                (transform ?transform)
		(constraint (& (:amount ?val)))
                ))
         (mass mass) (time-converted -)
	 (postadvbl ?gen) ;; swift -- setting postadvl to gen as part of eliminating gname rule but still allowing e.g. truck 1
         )
     -np-presciption>
     (head (name (lex ?l) (sem ?sem) (var ?v) (agr ?agr) (lf (:* ONT::PHARMACOLOGIC-SUBSTANCE ?lex))
		 ))
     (adjp (sort w::unit-measure) (arg ?v) (lf (% prop (constraint (& (val ?val)))))))||#
     ))

;; From np-grammar.lisp

(parser::augment-grammar 
  '((headfeatures
     (NP VAR CASE MASS NAME agr SEM PRO CLASS headcat)
     (N VAR SEM SORT LF LEX INPUT PRED ARGSEM headcat)
     (ART VAR INPUT headcat)
     (SPEC POSS VAR LEX POSS-VAR POSS-SEM INPUT headcat)
     )
    
    

    ;; Commented out by Myrosia temporarily to aid with debugging mass/count distinction    
;;;     ;; plural to the chart for each singular noun.
;;;     ;;
;;;     
;;;     ((N (agr 3p) (MASS -) (CHANGEAGR +))
;;;      -corrected-agr> .7
;;;       (head (n (agr 3s) (MASS -) (CHANGEAGR -))))
    ;;  to handle cases like The US or "The Saudi Academy" where the NER doesn't get the "the"
     ((NP (SORT PRED) (generated +)  ;; we set generated to prevent this being used as a NAME-N modification
         (var ?v) (Class ?lf) (sem ?sem) (agr ?agr) (case (? cas sub obj -))
         (LF (% Description (Status Name) (var ?v) (Sort Individual)
                (class ?lf) (lex ?l) (sem ?sem)  ;; LEX used to be ?fname, but this isn't used any more  JFA 5/04
                (transform ?transform)
		(constraint ?restr)
                ))
       (postadvbl ?gen) ;; swift -- setting postadvl to gen as part of eliminating gname rule but still allowing e.g. truck 1
         )
     -np-the-name> .98      ;; this is not as rare as we once thought - e.g.,  "the affinitor" the affinitor tagged as NAME
      (art (lex the))
      (head (name (time-converted -)
		  (lex ?l) (sem ?sem) (var ?v) (agr ?agr) (lf ?lf) (class ?class)
		   (full-name ?fname)
	    ;; swift 11/28/2007 removing gname rule & passing up generated feature (instead of restriction (generated -))
	    (generated ?gen)  (transform ?transform)
	    (restr ?restr)
	    )))


     ;; this is another to catch mixed a and the. Adds the 
     ;; for every "a" encountered

;;     ((SPEC (SEM DEFINITE) (ARG ?arg) (BAREDET +) (mass ?m) (AGR ?a) (LF THE))
;;      -spec-det1-robust>
;;     (head (DET (sem INDEFINITE) (POSS -) (LF A))))

))

  ;; Commented out by Myrosia temporarily to aid with debugging mass/count distinction

;;;;; Added by Myrosia. Those are variants of original NP rules with relaxed agreement
;;;;; Don't bother with posessives, they are treated incorrectly anyway
;;;;;
;;;
;;;(parser::augment-grammar 
;;;  '((headfeatures
;;;     (NP VAR CASE MASS NAME agr SEM PRO CLASS))
;;;    
;;;    ;;   Basic noun phrases: all combinations of AGR and MASS
;;;    ;;  e.g., the train, some trains, the sand in the corner, five pounds of sand
;;;    ;; with non-possessive SPEC
;;;
;;;    ((NP (LF (% description (STATUS ?spec) (VAR ?v) (SORT (?agr ?m))
;;;		(CLASS ?c) (CONSTRAINT ?r) (SET-CONSTRAINT ?restr)))
;;;      (SORT DESCR) (QTYPE ?w) (WH ?w)) ;; must move WH feature up by hand here as it is explicitly specified in a daughter.
;;;     -np-indv-robust-agr> 0.7
;;;     (SPEC (MASS ?m1) (LF ?spec) (ARG ?v) (POSS -)
;;;           (WH ?w) (agr ?agr1) (SEM ?ref-type) (RESTR ?restr))
;;;     (head (N1 (VAR ?v) (SORT PRED) (CLASS ?c) (MASS ?m) 
;;;	    (KIND -) (agr ?agr) (RESTR ?r)))
;;;     (NOT-UNIFY (arg1 ?agr1) (arg2 ?agr))   ;; disable if number agreement was OK
;;;     )
;;;    
;;;    ((NP (LF (% description (STATUS ?ref-type) (VAR ?v) (SORT (?agr ?m))
;;;		(CLASS ?c) (CONSTRAINT ?r) (SET-CONSTRAINT ?restr)))
;;;      (SORT DESCR) (QTYPE ?w) (WH ?w)) ;; must move WH feature up by hand here as it is explicitly specified in a daughter.
;;;     -np-indv-robust-mass> 0.7
;;;     (SPEC (MASS ?m1) (LF ?spec) (ARG ?v) (POSS -)
;;;           (WH ?w) (agr ?agr1) (SEM ?ref-type) (RESTR ?restr))
;;;     (head (N1 (VAR ?v) (SORT PRED) (CLASS ?c) (MASS ?m) 
;;;	    (KIND -) (agr ?agr) (RESTR ?r)))
;;;     (UNIFY (arg1 ?agr1) (arg2 ?agr)) ;; disable if number agreement was bad as rule above handled
;;;     (NOT-UNIFY (arg1 ?m1) (arg2 ?m))   
;;;     )
;;;)) 




























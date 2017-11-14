;;;;
;;;; adverbial-grammar.lisp
;;;;
;;;;
(in-package :W)


(parser::augment-grammar
  '((headfeatures
	 ;;lex headcat removed --me
     (PP KIND MASS NAME agr SEM SORT PRO SPEC CLASS transform gap gerund)
     ;;(ADVBLS FOCUS VAR SEM SORT ATYPE ARG SEM ARGUMENT NEG TO QTYPE lex transform)
     (ADVBL VAR SORT ARGSORT ATYPE SEM ARGUMENT lex headcat transform neg result-only)
     (ADV SORT ATYPE CONSTRAINT SA-ID PRED NEG TO LEX HEADCAT SEM ARGUMENT SUBCAT IGNORE transform)
     )	       	       

    ;;   GENERAL RULES FOR ADVERBIAL PHRASES
    
    ;;  simple adverbials
    ;; TEST: quickly
    ((ADVBL (ARG ?arg) (LF (% PROP (CLASS ?lf) (VAR ?v) (constraint ?newc);(CONSTRAINT (& (?!argmap ?arg)))
                              (sem ?sem) (transform ?transform)))
            ;;(SORT CONSTRAINT)
      (role ?lf) (particle ?p) (particle-role-map ?prm)
      ;; Myrosia 10/25/03 - this is just a stupid trick to make these go into vp-post-role rules
      ;; which ask for subcatsem
      ;; needs to be cleaned up with better role handling in the grammar
      (subcatsem ?!newsem) (comparative -)
      )
     -advbl-simple>
     (head (adv (WH -) (sem ?sem) (ARGUMENT-MAP ?!argmap) ;;(Sort PRED) 
                (VAR ?v) (SUBCAT -) (LF ?lf) (implicit-arg -) (constraint ?con)
		(sem ($ F::ABSTR-OBJ (F::intensity ?ints) (F::orientation ?orient) (F::Scale ?scale)))
		(comparative -) (prefix -)
		(particle ?p) (particle-role-map ?prm)
		)
      )
     (append-conjuncts (conj1 ?con) (conj2 (& (?!argmap ?arg)  (scale ?scale)
					      (orientation ?orient) (intensity ?ints)))
		       (new ?newc))
     )
   
     ;;  simple adverbials- used is the lexical entry does not specify an argument-map
    ((ADVBL (ARG ?arg) (LF (% PROP (CLASS ?lf) (VAR ?v) (CONSTRAINT (& (FIGURE ?arg)))
                              (sem ?sem) (transform ?transform)))
            ;;(SORT CONSTRAINT)
      (role ?lf)
      (comparative -)
      )
     -advbl-simple-no-argmap>
     (head (adv (WH -) (sem ?sem) (ARGUMENT-MAP -) ;;(Sort PRED) 
	    (VAR ?v) (SUBCAT -) (LF ?lf) (implicit-arg -) (prefix -)
	    (comparative -)
	    ))      
     )
     
    
    ;; BINARY-CONSTRAINT adverbials (with a complement)
    ;;   e.g., instead of avon, in the town
    ;;   and gap forms for complex subcats e.g., (what did you go to the store) to buy
    ;; Myrosia 04/18/03: having a subcat written out explicitly was causing all sorts of problems in the rules
    ;; Many adverbials place restrictions on their argument (vform base), (stype decl) etc.
    ;; These weren't picked up and the whole system prone to failure
    ;; Instead, I re-wrote it so that now pp adverbials place restriction on case (which is the only reason I can see it here)
    ;; TEST: instead of the house, from the store
    ((ADVBL (ARG ?arg) (WH ?wh) (FOCUS ?subv)
            (LF (% PROP (VAR ?v) (CLASS ?lf) (CONSTRAINT (& (?submap ?subv) (?argmap ?arg)))
                   (sem ?sem) (transform ?trans)))
      (sem ?sem) (VAR ?v) (role ?lf) (subcatsem ?subcatsem) (gap ?gap)
            )
     -advbl-binary> ;;.98 ;; rank slightly lower than a subcategorized complements
     (head (adv (lf ?lf) ;;(SORT BINARY-CONSTRAINT) 
            ;; make sure pp-word is not a sort here
            (sort (? !sort pp-word double-subcat))
	    (argument (% ?cat2 (var ?arg) (lex ?arglex) (sem ?argsem) (subjvar ?subjvar)))
            (subcat ?!sub) (SUBCAT (% ?cat (var ?subv) (sem ?subcatsem) (stype ?stype) (vform ?vform) (gap ?gap))) 
            (subcat-map ?submap) (ARGUMENT-MAP ?argmap)
	    (subcat2 -) ; to prevent e.g., if... then... to be used here (BINARY-CONSTRAINT-S-DECL-MIDDLE-WORD-SUBCAT-TEMPL)
            (IGNORE -)
	    (comparative -) (prefix -)
            (sem ?sem))
           )
     ?!sub
     (unify (pattern (% ?xx (WH ?wh))) (value ?!sub))    ;; fixes problem that WH don't seem to move up through variable constituents
     ;; ((? cat) (stype ?stype) (var ?subv) (sem ?subcatsem) (case (? case obj -)) (gap -) (vform ?vform))
     )

    
    ((ADVBL (ARG ?arg) (gap -) (WH ?wh) (FOCUS ?subv)
            (LF (% PROP (VAR ?v) (CLASS ?lf) (CONSTRAINT (& (?submap ?subv) (?argmap ?arg) (?sub2map ?sub2)))
                   (sem ?sem) (transform ?trans)))
      (sem ?sem) (VAR ?v) (role ?lf) (subcatsem ?subcatsem)
            )
     -advbl-double-subcat> ;;.98 ;; rank slightly lower than a subcategorized complements
     (head (adv (lf ?lf) ;;(SORT BINARY-CONSTRAINT) 
            ;; make sure pp-word is not a sort here
            (sort (? !sort pp-word))
            (subcat ?!sub) (SUBCAT (% ?cat (var ?subv) (sem ?subcatsem) (stype ?stype) (vform ?vform) (gap -))) 
	    (subcat2 ?!sub2) (SUBCAT2 (% ?cat2 (var ?sub2) (sem ?subcatsem2) (stype ?stype2) (vform ?vform2) (gap -))) 
            (subcat-map ?submap) (ARGUMENT-MAP ?argmap) (SUBCAT2-MAP ?sub2map)
            (IGNORE -)
            (sem ?sem))
           )
     ?!sub
     ?!sub2
     (unify (pattern (% ?xx (WH ?wh))) (value ?!sub))    ;; fixes problem that WH don't seem to move up through variable constituents
     ;; ((? cat) (stype ?stype) (var ?subv) (sem ?subcatsem) (case (? case obj -)) (gap -) (vform ?vform))
     )
    
   #||   This now can be handled compositionally
    ;; the meeting from 5 to 10 pm; the flight from here to there
    ((ADVBL (ARG ?arg) (gap -) (WH ?wh) (FOCUS ?subv)
         (LF (% PROP (VAR ?v) (CLASS ont::FROM-TO) (CONSTRAINT (& (TO ?v3) (FROM ?subv) (?argmap ?arg)))
                (sem ?sem2) (transform ?trans)))
         (sem ?sem2) (VAR ?v) (role ?lf) (subcatsem ?subcatsem)
         )
     -advbl-from-to>
     (head (adv (lf ?lf) (lex from)
                (sort (? !sort pp-word double-subcat))
                (subcat ?!sub) (SUBCAT (% ?cat (var ?subv) (sem ?subcatsem) (sem ($ f::situation))
					  (stype ?stype) (vform ?vform) (gap -))) 
                (subcat-map ?submap)(ARGUMENT-MAP ?argmap)
                (IGNORE -)
                (sem ?sem1))
           )
     ?!sub
     (unify (pattern (% ?xx (WH ?wh))) (value ?!sub))
     (adv (lex to))
     (np (var ?v3) (sem ?sem2))
     )||#

    ;; to the left/right
    ;; handled by rule to produce adverbial, same lf as "right/left"
    ;; ont::direction is compatible with verbs that are f::trajectory +, like move
    ;; TEST: to the left
    ;; TEST: to the right
    ((ADVBL (ARG ?arg) (gap -) (WH -)
            (LF (% PROP (VAR ?v) (CLASS (:* ONT::direction ?lx))  (CONSTRAINT (& (?argmap ?arg)))
                   (sem ?sem) (transform ?trans)))
            (sem ?sem) (role ont::direction) (VAR ?v)
         )
     -advbl-left-right>
     (word (lex to))
     (word (lex the))
     (head (adv (lex (? lx left right)) (var ?v) (lf ont::direction)
                (WH -) (sem ?sem) (subcat -) (implicit-arg -)
                (ARGUMENT-MAP ?argmap)
                )
           )
     )

    
    ;;; THREE PART ADVERBIALS: if ... then, as ... as, for X to Y
    
    ((ADVBL (ARG ?arg) (gap -) (WH ?wh) (FOCUS ?subv)
            (LF (% PROP (VAR ?v) (CLASS ?lf) (CONSTRAINT (& (?submap ?subv) (?argmap ?arg) (?submap2 ?subv2)))
                   (sem ?sem) (transform ?trans)))
      (sem ?sem) (VAR ?v) (role ?lf) (subcatsem ?subcatsem)
            )
     -advbl-binary-3part> ;;.98 ;; rank slightly lower than a subcategorized complements
     (head (adv (lf ?lf) ;;(SORT BINARY-CONSTRAINT) 
            ;; make sure pp-word is not a sort here
            (sort (? !sort pp-word))
	    (subcat ?!sub) (SUBCAT (% ?cat (var ?subv) (sem ?subcatsem) (stype ?stype) (vform ?vform) (gap -) (lex ?slex))) 
            (subcat-map ?submap) (ARGUMENT-MAP ?argmap)
            (subcat2 ?!sub2) (SUBCAT2 (% ?cat2 (var ?subv2) (sem ?subcatsem2) (stype ?stype2) (vform ?vform2) (gap -) (lex ?slex2))) 
            (subcat2-map ?submap2) (ARGUMENT-MAP ?argmap2)
            (IGNORE -)
            (sem ?sem))
           )
     ?!sub
     ?!sub2
     (unify (pattern (% ?xx (WH ?wh))) (value ?!sub2))    ;; fixes problem that WH don't seem to move up through variable constituents     
     ;; ((? cat) (stype ?stype) (var ?subv) (sem ?subcatsem) (case (? case obj -)) (gap -) (vform ?vform))
     )

    
    
    ((ADVBL (ARG ?arg) (gap ?!gap)
      (LF (% PROP (VAR ?v) (CLASS ?lf) (CONSTRAINT (& (?submap ?subv) (?argmap ?arg)))
                   (sem ?sem) (transform ?trans)))
      (sem ?sem) (role ?lf) (subcatsem ?subcatsem)
      )
     -advbl-binary-gap> .9 ;; we really don't want gaps here if we can avoid them
     (head (adv (lf ?lf) (var ?v) ;;(SORT BINARY-CONSTRAINT) 
	    (sort (? !sort pp-word double-subcat))
	    (SUBCAT (% ?!cat (var ?!subv) (sem ?subcatsem)))
	    (subcat-map ?submap) (ARGUMENT-MAP ?argmap)
            (Ignore -)
	    (comparative -)
	    (sem ?sem))
      )
     ;;     ?!sub
     (?!cat (var ?!subv) (sem ?subcatsem) (case (? case obj -)) (gap ?!gap))
     )


    ;;  BINARY-CONSTRAINT adverbials that allow omitted objects, e.g., nearby, near, below, about, ...
    ;; TEST: The dog chased the cat below.
    ((ADVBL (ARG ?arg) (QTYPE ?wh) (FOCUS ?var)
	    (LF (% PROP (VAR ?v) (CLASS ?lf) (CONSTRAINT (& (?submap (% *PRO* (var *) (CLASS ONT::ANY-SEM)
									(SEM ?subcatsem))) (?argmap ?arg)))
                   (sem ?sem) (transform ?trans)))
      ;;(SORT CONSTRAINT)
      (role ?lf) (subcatsem ?subcatsem)
      )
     -advbl-binary-deleted> .97
     (head (adv (SUBCAT (% ?x (SEM ?subcatsem))) (VAR ?v)
	    (sort (? !sort pp-word double-subcat))
	    (subcat-map ?submap) (ARGUMENT-MAP ?argmap)
	    (ALLOW-DELETED-COMP +)
	    (comparative -)
	    (LF ?lf) (AGR ?a))))
    
    ;;  Gapped ADVBLs with entire subcat gone, this one for NP subcats only
    ;; TEST: The store the cat was from.
    ((ADVBL (ARG ?arg) 
            (GAP (% NP (var ?gapv) (SEM ?gapsem) (gap -)
	            (agr ?gapagr) (case (? case obj -))))       
            (FOCUS ?var)
            (LF (% PROP (VAR ?v) (CLASS ?lf) (CONSTRAINT (& (?submap ?gapv) (?argmap ?arg)))
                   (sem ?sem) (transform ?trans)))
            (role ?lf) (subcatsem ?gapsem) (subcat ?subcat)
            )
     -advbl-subcat-gap> .92
     (head (adv (var ?v) ;;(SORT BINARY-CONSTRAINT) 
	    (sort (? !sort pp-word double-subcat))
            (lf ?lf) (IGNORE -)  (sem ?sem)
            (subcat-map ?submap) (ARGUMENT-MAP ?argmap)
	    (comparative -) 
            (SUBCAT (% NP (var ?gapv) (sem ?gapsem) 
                       (agr ?gapagr) (case (? case obj -))))))
     )
    
;;;    ;;  DISC adverbials: e.g., anyway
;;;     ((ADVBL (ARG ?arg) (LF (?lf ?arg)) (SORT DISC))
;;;      -advbl-disc> 
;;;      (head (adv (WH -) (Sort DISC) ;;(PRED ?pred) 
;;;	     (SUBCAT -) (LF ?lf)))
;;;      )

;;;     ;;  COMPARATIVE ADVERBIALS
;;;    
;;;    ;; e.g., comparatives e.g., more quickly, less slowly
;;;    ((ADVBL (ARG ?arg) (LF ((?mod ?lf) ?arg (% *pro* (VAR *) (SEM ?argsem)))) (COMPARATIVE +))
;;;     -comp-adv>
;;;     (adv (SORT COMPARATIVE) (LF ?mod))
;;;     (head (ADVBL (ARG (% ?argcat (sem ?argsem))) (COMP-OP ?comp) (LF ?lf))))
    
    
    
    ;; NEED to handle "more quickly than that" etc    
    
    ;; =====================================
    ;;  REMAINING MISCELLANEOUS STUFF
    
    ;; COMMA preceding POST adverbials
    ;;  even though the parser will skip commas, this avoids the small penalty and allows
    ;;  the comma to possibly affect the preferences between parses.
    ((ADV (var ?v) (argument ?arg) (argument-map ?argmap) (subcat ?sc) (subcat-map ?sc-map) (LF ?lf))
     -comma-adv>
     (punc (lex w::punc-comma))
     (head (adv (var ?v) (argument ?arg) (argument-map ?argmap) (subcat ?sc) (subcat-map ?sc-map) (LF ?lf) (ATYPE POST))))
    
    
    ;;  the PP rule is used for syntactic PPs that act as subcategorized arguments
    ;;   Note that the subcatsem of the prep is ignored here, as it is specified
    ;;   by the verb as a SEM restriction on the PP, and SEM is a head feature.
    ;;  e.g., on the top
    
    ((PP (PTYPE ?pt) (var ?v) (lf ?lf) (case ?c) (sem ?sem) 
		 (lex ?pt) (headcat ?hc) (fake-head 0) ;;me
		 )   ; I set the case here to a var, in order to allow -np-spec-of-pp> to work. Otherwise, CASE is not used in PPs
     -pp1> 1  ;; since PPs are only used if subcategorized, we set this to 1
     (prep (LEX ?pt) (headcat ?hc))
     (head (np (lf ?lf) (sem ?sem)  (var ?v)
	       ;; 02/07/08 allow bare numbers here! 
	      ;; (LF (% ?cat (STATUS (? !st number)))) ; disallowing bare numbers here to prevent 'more than 5' w/ bare (referential-sem) interp
	       (lf (% ?cat (status (? !st ONT::PRO)))) ;; disallowing proforms here -- use pp1-pro>
	       (sort (? sort pred descr wh-desc unit-measure)) (case (? case obj -)))
      ))

    ;; PP conjunction rules: e.g., it associates with X and with Y -- both X and Y fill the role signalled by with
    ;; this rule requires the prep to the identicial, and merges the NP's into a conjunction
     ((PP (PTYPE ?pt) (var ?vc) (lf (% PROP (class ?class) (var ?vc) (sem ?sem) (constraint (& (operator ont::and) (sequence (?v1 ?v2))))))
       (case ?c) (sem ?sem)
		 (lex ?pt) (headcat ?hc) (fake-head 0) ;;me
       )
      -pp-conj1>
      (head (PP (PTYPE ?pt) (var ?v1) (case ?c) (sem ?s1)
		(gap ?gap)
		(LF (% ?sort1 (class ?c1) (status ?status)))
		))
      (CONJ (LF ?conj) (var ?vc) (but-not -) (but -))
      (PP (PTYPE ?pt) (var ?v2) (case ?c) (sem ?s2)
       (gap ?gap)
       (LF (% ?sort2 (class ?c2)))
       )
      (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
      (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class)))

    ;; I ate a dollop of the ice cream but not the ice
    ((PP (PTYPE ?pt) (var ?vc) 
      (lf (% PROP (class ?class) (var ?vc) (sem ?sem) 
	     (constraint (& (operator ont::and) (sequence ?v1) (except ?v2)))))
      (case ?c) (sem ?sem)
      (lex ?pt) (headcat ?hc) (fake-head 0) ;;me
      )
     -pp-conj-but-not1>
     (head (PP (PTYPE ?pt) (var ?v1) (case ?c) (sem ?s1)
	       (gap ?gap)
	       (LF (% ?sort1 (class ?c1) (status ?status)))
	       ))
     (CONJ (LF ?conj) (var ?vc) (but-not +))
     (PP (PTYPE ?pt) (var ?v2) (case ?c) (sem ?s2)
      (gap ?gap)
      (LF (% ?sort2 (class ?c2)))
      )
     (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
     (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class)))

    ;;PP but-not with comma
    ((PP (PTYPE ?pt) (var ?vc) 
      (lf (% PROP (class ?class) (var ?vc) (sem ?sem) 
	     (constraint (& (operator ont::and) (sequence ?v1) (except ?v2)))))
      (case ?c) (sem ?sem)
      (lex ?pt) (headcat ?hc) (fake-head 0) ;;me
      )
     -pp-conj-but-not-comma>
     (head (PP (PTYPE ?pt) (var ?v1) (case ?c) (sem ?s1)
	       (gap ?gap)
	       (LF (% ?sort1 (class ?c1) (status ?status)))
	       ))
     (punc (lex w::punc-comma))
     (CONJ (LF ?conj) (var ?vc) (but-not +))
     (PP (PTYPE ?pt) (var ?v2) (case ?c) (sem ?s2)
      (gap ?gap)
      (LF (% ?sort2 (class ?c2)))
      )
     (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
     (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class)))


    ;; for subcat pp'w with proforms -- but not w::one
    ((PP (PTYPE ?pt) (lf ?lf) (case ?c) (sem ?sem)  (var ?v)
      (lex ?pt) (headcat ?hc) (fake-head 0) ;;me
      )   ; I set the case here to a var, in order to allow -np-spec-of-pp> to work. Otherwise, CASE is not used in PPs
     -pp1-pro>
     (prep (LEX ?pt) (headcat ?hc))
     (head (np (lex (? !nlx w::one)) (lf ?lf) (sem ?sem) (var ?v)
	       (lf (% ?cat (status ONT::PRO)))
	       (sort (? sort pred descr wh-desc unit-measure)) (case (? case obj -)))
      ))

    ;; subcat pp with proform one has lower preference to prefer number subcats
    ((PP (PTYPE ?pt) (lf ?lf) (case ?c) (sem ?sem)  (var ?v)
      (lex ?pt) (headcat ?hc) (fake-head 0) ;;me
      )   ; I set the case here to a var, in order to allow -np-spec-of-pp> to work. Otherwise, CASE is not used in PPs
     -pp1-pro-one> .9
     (prep (LEX ?pt) (headcat ?hc))
     (head (np (lex w::one) (lf ?lf) (sem ?sem)  (var ?v)
	       (lf (% ?cat (status ont::PRO)))
	       (sort (? sort pred descr wh-desc unit-measure)) (case (? case obj -)))
      ))

       
     ;; more than five (trucks) 
    ((PP (PTYPE ?pt) (lf ?lf) (case ?c) (mass count)  (var ?v)
		 (lex ?pt) (headcat ?hc) (fake-head 0) ;;me
		 ) 
     -pp1-number> .9
     (prep (LEX ?pt) (headcat ?hc))
     (head (cardinality (lf ?lf)  (var ?v)))
      )

    ; "in" already has a sense that takes an adjective
    #|
    ;; rule for prepositional subcats with adjectives, such as
    ;; TEST: classify this as benign
    ;; I'd like one in red
    ((PP (PTYPE ?pt) (lf ?lf) (case ?c) (adjpp +) (arg ?arg)  (var ?v)
		 (lex ?pt) (headcat ?hc) (fake-head 0) ;;me
		 )
     -pp1-adj> .97
     (prep (LEX ?pt) (headcat ?hc))
     (head (adjp (lf ?lf) (case (? case obj -))  (var ?v) (arg ?arg) (set-modifier -)))  ;; set-modifier - excludes numbers
      )
    |#

    ))  ;; end ADVERBIALS



;;=======================================
;;
;;  ADVERBIAL MODIFICATION RULES
;;
;;   In a true unification grammar, I could do this with two rules. But I need to
;;    explode out each case to make the feature inheritance work.
;;   We use the POSTADVBL feature 

(parser::augment-grammar
  '((headfeatures
     (ADJ VAR ATYPE SORT ARG PRED ARGUMENT lex headcat transform)
     ;; MD 2008/07/17 added post-subcat as a head feature so that it doesn't lead to overgeneration
     (ADJP VAR ATYPE SORT ARG COMP-OP PRED ARGUMENT lex headcat transform post-subcat sem) 
     (NUMBER VAR AGR lex headcat transform)
     (VP vform var agr neg sem subj iobj dobj comp3 part cont gap class subjvar lex headcat transform subj-map tma aux template)
     (VP- vform var agr neg sem subj iobj dobj dobjvar comp3 part cont gap class subjvar lex headcat transform subj-map tma aux passive passive-map template result)
     (S vform neg cont stype gap sem subjvar dobjvar var  lex headcat transform subj)
     (N1 case VAR AGR MASS SEM Changeagr class reln sort lex headcat transform set-restr result rate-activity-nom agent-nom) ;added features for nominalizations
     (NP case VAR AGR MASS SEM Changeagr class reln sort lex headcat transform status)
     (ADVBL SORT ATYPE CONSTRAINT SA-ID PRED NEG TO LEX HEADCAT SEM ARGUMENT SUBCAT IGNORE transform neg)
     (ADV VAR ATYPE SORT ARG PRED ARGUMENT lex headcat transform)
     (UTT headcat lex ended subjvar)
     (quanp headcat lex)
     (cardinality headcat lex nobarespec)
     (number headcat lex nobarespec)
     )	       		   
    
    ;;   PRED-VAL and BINARY-CONSTRAINT adverbs become CONSTRAINT advbls.
    ;;   These rules add the modification to the CONSTRAINT slot

    ;; Myrosia added a rule for pre-vp adverbials to appear after "be"
    ((vp- (constraint ?new) ;;(tma ?tma)
      (advbl-needed ?avn)
      )
     -adv-vp-pre-be> 
     (head (vp- (VAR ?v) (SEM ?argsem) (constraint ?lf) (constraint (& (lobj -))) 
	    (aux -)
	    (CLASS (:* ONT::EXISTS BE))
	    (gap -) ;;(tma ?tma)
	    (advbl-needed ?avn)
	    )
      )
     (advbl (ATYPE PRE-VP) (GAP -)
      (ARGUMENT (% S (sem ?argsem)))
      (ARG ?v) (VAR ?mod) 
      (role ?advrole) (subcat -)
      )
     (add-to-conjunct (val (MODS ?mod)) (old ?lf) (new ?new)))
 
  
    ;; she subsequently was fired
   ((vp- (constraint ?new)
     (tma ?tma) (sem ?argsem)
     (advbl-needed ?avn)
      )
     -adv-vp-pre-s> 
     (advbl (SORT PRED) (ATYPE (? x PRE-VP)) (ARGUMENT (% S (SEM ($ f::situation))))  
      (GAP -) 
      (ARG ?v) (VAR ?mod)
      )
     (head (vp- (VAR ?v) (constraint ?con) (SEM ?argsem) ;;(aux -) 
		(tma ?tma)
		(advbl-needed ?avn)
	    )
      )
     (add-to-conjunct (val (MODS ?mod)) (old ?con) (new ?new)))

    ;; another version of adv-vp-pre-s> for occasional cases, and rarer
    ;; she in this case was fired
   ((vp- (constraint ?new)
     (tma ?tma) (sem ?argsem)
     (advbl-needed ?avn) (complex +)
      )
     -adv-vp-pre-complex-s>  .97
     (advbl (SORT BINARY-CONSTRAINT) 
;      (ATYPE (? x PRE PRE-VP)) 
      (ATYPE (? x PRE-VP)) 
      (ARGUMENT (% S (SEM ?argsem))) ;;($ f::situation))))  
      (GAP -) 
      (ARG ?v) (VAR ?mod)
      )
     (head (vp- (VAR ?v) (constraint ?con) (SEM ?argsem) ;;(aux -) 
		(tma ?tma)
		(advbl-needed ?avn)
	    )
      )
     (add-to-conjunct (val (MODS ?mod)) (old ?con) (new ?new)))
   
    

    ((vp- (constraint ?new) (tma ?tma) (class ?class) (sem ?sem) (var ?v)
      (advbl-needed -) (complex +) (subjvar ?subjvar)(GAP ?gap)
      )
     -adv-vp-post> .98   ;;  want to prefer explicitly subcategorized attachments
     (head (vp- (VAR ?v) 
		(seq -)  ;;  post mods to conjoined VPs is very rare
		(SEM ?sem) 
		(constraint ?lf) (tma ?tma)
		(subjvar ?subjvar)
		(aux -) (gap ?gap)
		(ellipsis -)
		))

     (advbl (particle -) (ATYPE POST) (ARGUMENT (% S (sem ?sem) (var ?v))); (subjvar ?subjvar)))
						   (GAP -)
      ;;(subjvar ?subjvar)   ;Not sure why this was here - maybe for purpose clauses. Leaving it in causes many parses to fail as the SUBJVAR in the new VP is wrecked
     ;; the SUBJVAR is required in the argument to be able to pass in the subject for things like "the dog walked barking".
      (ARG ?v) (VAR ?mod)
      (role ?advrole)
      ;(SEM ($ f::abstr-obj (F::type (? !ttt ont::position-reln))))
      (SEM ($ f::abstr-obj (F::type (? !ttt ont::path ont::conventional-position-reln ont::direction ont::complex-ground-reln ont::back ont::front ont::left-of ont::off ont::orients-to ont::right-of ;ont::pos-as-containment-reln
				       ont::pos-directional-reln ont::pos-distance ont::pos-wrt-speaker-reln ont::resulting-object))))
      )
     (add-to-conjunct (val (MODS ?mod)) (old ?lf) (new ?new))
     )

    ; I dusted the room up.
    ((vp- (constraint ?new) (tma ?tma) (class ?class) (sem ?sem) (var ?v)
      (advbl-needed -) (complex +) (subjvar ?subjvar)(GAP ?gap)
      )
     -adv-vp-post-particle> 0.98 ;.98   ;;  want to prefer explicitly subcategorized attachments
     (head (vp- (VAR ?v) 
		(seq -)  ;;  post mods to conjoined VPs is very rare
		(SEM ?sem) 
		(constraint ?lf) (tma ?tma)
		(subjvar ?subjvar)
		(aux -) (gap ?gap)
		(ellipsis -)
		; this works too, but I lowered the weight of the rule instead
		;(result ($ f::abstr-obj (F::type (? !ttt1 ont::position-reln ont::path)))) ; to exclude the verbs which can take DIRECTION
		))

     (advbl (particle +) (particle-role-map manner)
	    (ATYPE POST) (ARGUMENT (% S (sem ?sem) (var ?v) (subjvar ?subjvar)))
						   (GAP -)
      ;;(subjvar ?subjvar)   ;Not sure why this was here - maybe for purpose clauses. Leaving it in causes many parses to fail as the SUBJVAR in the new VP is wrecked
     ;; the SUBJVAR is required in the argument to be able to pass in the subject for things like "the dog walked barking".
      (ARG ?v) (VAR ?mod)
      (role ?advrole)
      (SEM ($ f::abstr-obj (F::type (? !ttt ont::position-reln))))
      )
     (add-to-conjunct (val (MANNER ?mod)) (old ?lf) (new ?new))
     )

    
    ;;  resultative construction using adjectives with transitives: e.g., wipe the table clean
    ((vp- (constraint ?new) (tma ?tma) (class (? class ONT::EVENT-OF-CAUSATION)) (var ?v)
         ;;(LF (% PROP (constraint ?new) (class ?class) (sem ?sem) (var ?v) (tma ?tma)))
      (advbl-needed -) (complex +) (result-present +) (subjvar ?subjvar)(GAP ?gap)
      )
     -vp-result-withtransitive> 0.97 ; lower than -adv-vp-post-particle> ;.98   ;;  want to prefer explicitly subcategorized attachments
     (head (vp- (VAR ?v) 
		(seq -)  ;;  post mods to conjoined VPs is very rare
		(DOBJ (% NP (Var ?npvar) (sem ?sem)))
		(COMP3 (% -))
		(constraint ?con) (tma ?tma) (result-present -)
		;;(subjvar ?subjvar)
		;;(aux -)   c.f., It had gone bad
		(gap ?gap)
		(ellipsis -)
		))
     (adjp (ARGUMENT (% NP (sem ?sem))) 
      ;(SEM ($ f::abstr-obj (F::type (? ttt ONT::position-reln ont::domain-property)))) ; not sure why we have position-reln here
      (SEM ($ f::abstr-obj (F::type (? ttt ont::domain-property)))) 
      (GAP -)
      ;; (subjvar ?subjvar)
      (SET-MODIFIER -)  ;; mainly eliminate numbers 
      (ARG ?npvar) (VAR ?mod)
      ;;(role ?advrole) 
      )
     (add-to-conjunct (val (RESULT ?mod)) (old ?con) (new ?new))
     )

    ;;  resultative construction using adjectives with intransitives: e.g., the water froze solid
    ((vp- (constraint ?new) (tma ?tma) (class (? class ONT::EVENT-OF-CAUSATION)) (var ?v)
         ;;(LF (% PROP (constraint ?new) (class ?class) (sem ?sem) (var ?v) (tma ?tma)))
      (SUBJ (% NP (Var ?npvar) (LEX ?LEX) (agr ?agr) (sem ?sem)))
      (advbl-needed -) (complex +) (result-present +) (subjvar ?subjvar)(GAP ?gap)
      )
     -vp-result-with-intransitive> .98   ;;  want to prefer explicitly subcategorized attachments
     (head (vp- (VAR ?v) 
		(seq -)  ;;  post mods to conjoined VPs is very rare
		;(DOBJVAR -)  ; This doesn't work because it could unify with a dobjvar not yet instantiated
		(dobj (% -)) ; cannot use (dobj -) because dobj is (% - (W::VAR -))
		(SUBJ (% NP (Var ?npvar) (LEX ?LEX)  (agr ?agr)(sem ?sem)))
		(constraint ?con) (tma ?tma) (result-present -)
		;;(subjvar ?subjvar)
		;;(aux -)   c.f., It had gone bad
		(gap ?gap)
		(ellipsis -)
		))
     (adjp (ARGUMENT (% NP (sem ?sem))) 
;      (SEM ($ f::abstr-obj (F::type (? ttt ONT::path))))
      ;(SEM ($ f::abstr-obj (F::type (? ttt ont::position-reln ont::domain-property))))
      (SEM ($ f::abstr-obj (F::type (? ttt ont::domain-property))))
      (GAP -)
      ;; (subjvar ?subjvar)
      (SET-MODIFIER -)  ;; mainly eliminate numbers 
      (ARG ?npvar) (VAR ?mod)
      ;;(role ?advrole) 
      )
     (add-to-conjunct (val (RESULT ?mod)) (old ?con) (new ?new))
     )

     
    ;;  resultative construction using adverbs: e.g., I walked to the store
    ((vp- (constraint ?new) (tma ?tma) (class (? class ONT::EVENT-OF-CAUSATION)) (var ?v)
         ;;(LF (% PROP (constraint ?new) (class ?class) (sem ?sem) (var ?v) (tma ?tma)))
;      (advbl-needed -) (complex +) (result-present +) (GAP ?gap)
      (SUBJ (% NP (Var ?npvar) (sem ?sem) (agr ?agr) (lex ?lex) (case ?case)))
      (subjvar ?npvar) (result-present +)
      (advbl-needed -) (complex +) (GAP ?gap)
      )
     -vp-result-advbl-intransitive>  
     (head (vp- (VAR ?v) 
		(seq -)  ;;  post mods to conjoined VPs is very rare
		;(DOBJVAR -)  ; This doesn't work because it could unify with a dobjvar not yet instantiated
		(dobj (% -)) ; cannot use (dobj -) because dobj is (% - (W::VAR -))
		(SUBJ (% NP (Var ?npvar) (agr ?agr) (sem ?sem) (lex ?lex) (case ?case)))  
		(subjvar ?npvar)
		(constraint ?con) (tma ?tma) (result-present -)
		;;(aux -) 
		(gap ?gap)
		(ellipsis -)
		(result ?asem)
		))

     (advbl (ARGUMENT (% NP ;; (? xxx NP S)  ;; we want to eliminate V adverbials, he move quickly  vs he moved into the dorm
			 (sem ?sem) (var ?npvar)))
      (GAP -)
      ;; (subjvar ?subjvar)
      (sem ?asem)
      (SEM ($ f::abstr-obj (F::type (? ttt ont::path ont::position-reln)))) ;(F::type (? !ttt1 ont::position-as-extent-reln ont::position-w-trajectory-reln ))))
;      (SEM ($ f::abstr-obj (F::type (? ttt ont::position-reln ont::goal-reln ont::direction-reln))))
      (SET-MODIFIER -)  ;; mainly eliminate numbers 
      (ARG ?npvar) (VAR ?mod)
      ;;(role ?advrole) 
      )
     (add-to-conjunct (val (result ?mod)) (old ?con) (new ?new))  ; The RESULT will be remapped to TRANSIENT-RESULT
     )

    
    ;;  resultative construction using adverbs: e.g., sweep the dust into the corner
    ;; only allow one of these
    ((vp- (constraint ?new) (tma ?tma) (class (? class ONT::EVENT-OF-CAUSATION)) (var ?v)
         ;;(LF (% PROP (constraint ?new) (class ?class) (sem ?sem) (var ?v) (tma ?tma)))
;      (advbl-needed -) (complex +) (result-present +) (GAP ?gap)
      (advbl-needed -) (complex +) (GAP ?gap) (result-present +)
      )
     -vp-result-advbl-no-particle>  
     (head (vp- (VAR ?v) 
		(seq -)  ;;  post mods to conjoined VPs is very rare
		(DOBJ (% NP (Var ?npvar) (sem ?sem)))
		(COMP3 (% -))
		(constraint ?con) (tma ?tma) (result-present -)
		;;(subjvar ?subjvar)
		;;(aux -) 
		(gap ?gap)
		(ellipsis -)
		(result ?asem)
		))

     (advbl (ARGUMENT (% NP ;; (? xxx NP S)  ;; we want to eliminate V adverbials, he move quickly  vs he moved into the dorm
			 (sem ?sem))) (GAP -) (particle -)
      ;; (subjvar ?subjvar)
			 (sem ?asem)
			 (SEM ($ f::abstr-obj
				 (F::type (? ttt ont::path ont::conventional-position-reln ont::direction ont::complex-ground-reln ont::back ont::front ont::left-of ont::off ont::orients-to ont::right-of ont::pos-as-containment-reln ont::pos-directional-reln ont::pos-distance ont::pos-wrt-speaker-reln ont::resulting-object))))
					;(F::type (? ttt ont::path ont::position-reln))))
	      ;;(F::type (? !ttt1 ont::position-as-extent-reln ont::position-w-trajectory-reln ont::on ont::at-loc )))) ; take the trajectory senses instead of the position-as-extent-reln senses of words such as "across"
;      (SEM ($ f::abstr-obj (F::type (? ttt ont::position-reln ont::goal-reln ont::direction-reln))))
      (SET-MODIFIER -)  ;; mainly eliminate numbers 
      (ARG ?npvar) (VAR ?mod)
      ;;(role ?advrole) 
      )
     (add-to-conjunct (val (result ?mod)) (old ?con) (new ?new))  ; The RESULT will be remapped to TRANSIENT-RESULT
     )

    ;; put the box down in the corner
    ;;  we can allow two RESULTS if the first one is a particle
     ((vp- (constraint ?new) (tma ?tma) (class (? class ONT::EVENT-OF-CAUSATION)) (var ?v)
         ;;(LF (% PROP (constraint ?new) (class ?class) (sem ?sem) (var ?v) (tma ?tma)))
;      (advbl-needed -) (complex +) (result-present +) (GAP ?gap)
      (advbl-needed -) (complex +) (GAP ?gap) ;(result-present +)
      )
     -vp-result-advbl-particle>  
     (head (vp- (VAR ?v) 
		(seq -)  ;;  post mods to conjoined VPs is very rare
		(DOBJ (% NP (Var ?npvar) (sem ?sem)))
		(constraint ?con) (tma ?tma) (result-present -)
		;;(subjvar ?subjvar)
		;;(aux -) 
		(gap ?gap)
		(ellipsis -)
		(result ?asem)
		))

     (advbl (ARGUMENT (% NP ;; (? xxx NP S)  ;; we want to eliminate V adverbials, he move quickly  vs he moved into the dorm
			 (sem ?sem))) (GAP -) (particle +)
      ;; (subjvar ?subjvar)
      (SEM ($ f::abstr-obj (F::type (? ttt ont::path ont::position-reln))))
;      (SEM ($ f::abstr-obj (F::type (? ttt ont::position-reln ont::goal-reln ont::direction-reln))))
      (sem ?asem)
      (SET-MODIFIER -)  ;; mainly eliminate numbers 
      (ARG ?npvar) (VAR ?mod)
      ;;(role ?advrole) 
      )
     (add-to-conjunct (val (RESULT ?mod)) (old ?con) (new ?new))
     )

    ;; to kill by immersing in water
    ((vp- (constraint ?new) (tma ?tma) (class ?class) (sem ?sem) (var ?v)
      ;;(LF (% PROP (constraint ?new) (class ?class) (sem ?sem) (var ?v) (tma ?tma)))
      (advbl-needed -) (complex +) (GAP ?!gap)
      )
     -adv-ganged-gap-vp-post> 
     (head (vp- (VAR ?v) 
		(seq -)  ;;  post mods to conjoined VPs is very rare
		(SEM ?sem) 
		(constraint ?lf) (tma ?tma)
		(aux -) (gap ?!gap)
		))

     (advbl (ATYPE POST) (ARGUMENT (% S (sem ?sem))) (GAP ?!gap)
      (ARG ?v) (VAR ?mod)
      (role ?advrole) 
      )
     (add-to-conjunct (val (MODS ?mod)) (old ?lf) (new ?new))
     )

   ;; adverbial conjunctions/disjunctions
   ;; TEST: up and to the right
   ;; TEST: up and down
   ((ADVBL (ARG ?arg) (sem ?sem) (VAR *) (gap ?g)
	   (LF (% PROP (CLASS ?class) (VAR *) (sem ?sem) (CONSTRAINT (& (sequence (?v1 ?v2)) (operator ?conj))
		  ))))
     -advbl-conj1>
     (ADVBL (ARG ?arg) (LF (% PROP (CLASS ?lf1) (VAR ?v1)(sem ?sem1))) (gap ?g) (argument ?argmt))
     (CONJ (LF ?conj) (but-not -) (but -))
     (head (ADVBL (ARG ?arg) (LF (% PROP (CLASS ?lf2) (VAR ?v2) (sem ?sem2))) (gap ?g) (argument ?argmt)))
    (sem-least-upper-bound (in1 ?sem1) (in2 ?sem2) (out ?sem))
    (class-least-upper-bound (in1 ?lf1) (in2 ?lf2) (out ?class))
    )

    ;; down the hill but to the right
     ((ADVBL (ARG ?arg) (sem ?sem) (VAR ?v1) (gap ?g)
	   (LF (% PROP (CLASS ?lf1) (VAR ?v1) (sem ?sem) (CONSTRAINT ?newcon)))
		  )
     -advbl-but-conj1>
      (head (ADVBL (ARG ?arg) (LF (% PROP (CLASS ?lf1) (VAR ?v1)(sem ?sem) (constraint ?con) ))
		   (gap ?g) (argument ?argmt)))
      (CONJ (LF ?conj) (but-not -) (but +) (var ?vc))
      (ADVBL (ARG ?arg) (LF (% PROP (CLASS ?lf2) (VAR ?v2) (sem ?sem2))) (gap ?g) (argument ?argmt))
      (add-to-conjunct (val (:qualification (% *PRO* (status ont::F) (var ?vc) (class ?conj) (constraint (& (Figure ?v1) (ground ?v2)))
					       )))
       (old ?con) (new ?newcon))
      )

    ((ADVBL (ARG ?arg) (sem ?sem) (VAR *)
      (LF (% PROP (CLASS ?clf) (VAR *) (sem ?sem) 
	     (constraint (& (?cop (?v1 ?v2))))
	     ))
      (headcat ?hcat) (fake-head 0) ;; aug-trips
      )
     -advbl-double-coord>
     (conj (SEQ +) (SUBCAT1 ADVBL) (SUBCAT2 ?wlex) (SUBCAT3 ADVBL) 
      (var ?v) (lf ?clf)
      (operator ?cop))                
     (ADVBL (ARG ?arg) (LF (% PROP (CLASS ?lf1) (VAR ?v1)(sem ?sem1))))
     (word (lex ?wlex))
     (head (ADVBL (ARG ?arg) (LF (% PROP (CLASS ?lf2) (VAR ?v2) (sem ?sem2)))))
     (sem-least-upper-bound (in1 ?sem1) (in2 ?sem2) (out ?sem))
     )
    
    ;; declarative:
    ;; TEST: After he left, the dog arrived.
    ;; imperative:
    ;; TEST: After you get approval, purchase it.
    ;; after I get a raise, how about a seventeen inch flat screen (how-about)
    ;; if the dog barks, will the cat run away
    ((S (LF ?newlf) (PREADVBL +)
       (tma ?tma)
        )
     -adv-S-pre> 
     (advbl (ATYPE PRE) (ARGUMENT (% S (sem ?argsem))) 
            (GAP -) (WH -)   ;; wh-terms have their own special rule
            (ARG ?v) (VAR ?mod)
            )
     (head (S (VAR ?v) (LF ?lf) (LF (% prop (constraint ?con))) (SEM ?argsem) (aux -)
	      (wh -) ;; while possible, its very unlikely
	      (tma ?tma) ;;(stype (? stp decl imp how-about))
	      )
           )
     (add-to-conjunct (val (MODS ?mod)) (old ?con) (new ?newcon))
     (change-feature-values (OLD ?lf) (NEW ?newlf) (newvalues ((CONSTRAINT ?newcon)))))

    ((S (LF ?newlf) (PREADVBL +)
       (tma ?tma)
        )
     -adv-S-pre-comma> 1
     (advbl (ATYPE PRE) (ARGUMENT (% S (sem ?argsem))) 
            (GAP -) (WH -)   ;; wh-terms have their own special rule
            (ARG ?v) (VAR ?mod)
            )
     (punc (lex w::punc-Comma))
     (head (S (VAR ?v) (LF ?lf) (LF (% prop (constraint ?con))) (SEM ?argsem) (aux -)
	      (wh -) ;; while possible, its very unlikely
	      (tma ?tma) (stype (? stp decl imp how-about))
	      )
           )
     (add-to-conjunct (val (MODS ?mod)) (old ?con) (new ?newcon))
     (change-feature-values (OLD ?lf) (NEW ?newlf) (newvalues ((CONSTRAINT ?newcon)))))

    ;; Terrified, he ran.
    ((S (LF ?newlf) ;(PREADVBL +)
       (tma ?tma) 
      (adj-s-prepost +)  ;;  we use this to stop such S's from being relative clauses etc
        )
     -adj-S-pre-comma> .97    ; requires a comma here so "Activated Ras..." doesn't use this rule
     (adjp (ARGUMENT (% NP)) 
	 (gap -) 
	 (ARG ?subjvar) (VAR ?mod)
	 )
     (punc (lex w::punc-Comma))
     (head (S (VAR ?v) (LF ?lf) (LF (% prop (constraint ?con))) (aux -)
	      (wh -) ;; while possible, its very unlikely
	      (tma ?tma)
	      ;(stype (? stp decl imp how-about))
	      (stype (? stp decl how-about))
	      (subjvar ?subjvar) 
	      )
           )
     (add-to-conjunct (val (TIME (% *pro* (var *) (status ont::F) (class ont::EVENT-TIME-REL) 
				    (constraint (& (FIGURE ?v) (GROUND ?mod))))))
      (old ?con) (new ?newcon))
     (change-feature-values (OLD ?lf) (NEW ?newlf) (newvalues ((CONSTRAINT ?newcon)))))

    ;; He ran away, terrified.  (added comma)
    ((S (LF ?newlf) ;(PREADVBL +)
       (tma ?tma) 
      (adj-s-prepost +)  ;;  we use this to stop such S's from being relative clauses etc
        )
     -adj-S-post> .97    
     (head (S (VAR ?v) (LF ?lf) (LF (% prop (constraint ?con))) (aux -)
	      (wh -) ;; while possible, its very unlikely
	      (tma ?tma) (stype (? stp decl imp how-about))
	      (subjvar ?subjvar) (adj-s-prepost -)
	      )
           )
     (punc (lex w::punc-Comma))
     (adjp (ARGUMENT (% NP)) (set-modifier -)
	 (gap -) 
	 (ARG ?subjvar) (VAR ?mod)
	 )
     (add-to-conjunct (val (TIME (% *pro* (var *) (status ont::F) (class ont::EVENT-TIME-REL) 
				    (constraint (& (FIGURE ?v) (GROUND ?mod))))))
      (old ?con) (new ?newcon))
     (change-feature-values (OLD ?lf) (NEW ?newlf) (newvalues ((CONSTRAINT ?newcon)))))

    ;;   N1 modification
    ;; Adverbial modifiers - locations, routes and objectives
    ;; e.g., the train at avon, the route from Avon to Bath, the goal to get to Bath
    ;; TEST: The dog at the store.
    ;; TEST: The route from the house to the store.
    ((N1 (RESTR ?new) (POSTADVBL +) (COMPLEX +)) 
     -adv-np-post> 
     (head (N1 (VAR ?v1) ;; (POSTADVBL -) 
	    (SEM ?argsem) 
	    (RESTR ?restr) ;;(gerund -)   Have to allow gerunds e.g., the debating at the house.
	    (post-subcat -) (SORT PRED)
	    (no-postmodifiers -) ;; exclude "the same path as the battery" and advbl attaching to "path"
	    (rate-activity-nom -)
	    (agent-nom -)
	    ))
     (advbl (ATYPE POST) 
      (result-only -)  ;; only allow adverbials that may be interpreted as something other than a result
      (ARGUMENT (% NP (sem ?argsem) (constraint ?c)  ))
      (SEM ($ f::abstr-obj (F::type (? ttt ont::predicate ont::position-reln))))
      (arg ?v1) (VAR ?mod) (WH -) (GAP -)
      (particle -)  ;; exclude particles as they should attach to the verb
      )
     (add-to-conjunct (val (MODS ?mod)) (old ?restr) (new ?new))
     )

    ; the lifting of the ball into the box
    ((N1 (RESTR ?new) (POSTADVBL +) (COMPLEX +)) 
     -adv-np-event-post>   ;; event nominals allows result adverbials
     (head (N1 (VAR ?v1) ;; (POSTADVBL -)
	       (dobj (% NP (sem ?argsem) (var ?argv)))
	       (SEM ($ f::situation (F::type ont::event-of-change)))
	       (RESTR ?restr) ;;(gerund -)   Have to allow gerunds e.g., the debating at the house.
	       (post-subcat -) (SORT PRED)
	       (no-postmodifiers -) ;; exclude "the same path as the battery" and advbl attaching to "path"
	       (result ?asem)
	       ))
     (advbl (ATYPE POST)
      (result-only +)  ;; allows result adverbials (others already handled by -adv-np-post>
      (ARGUMENT (% NP (sem ?argsem) (constraint ?c) ))
      (sem ?asem)
      (SEM ($ f::abstr-obj (F::type (? ttt ont::predicate ont::position-reln))))
      (arg ?argv) (VAR ?mod) (WH -) (GAP -)
      (particle -)  ;; exclude particles as they should attach to the verb
      )
     (add-to-conjunct (val (MODS ?mod)) (old ?restr) (new ?new))
     )
    
    
   #||
 ((N1 (RESTR ?new) (POSTADVBL +) (COMPLEX +)) 
     -adv-np-post-role> .999 ;; above  verb subcat (.99) and vp-adv mod (.95)
     (head (N1 (VAR ?v1) ;; (POSTADVBL -) 
	    (SEM ?argsem)  (RESTR ?restr)
	    ;; Myrosia added roles ?!roles restriction
	    ;; role-unify couldn't possibly work if there is an empty roles field on the noun
	    ;; but the early restriction cuts off such unreasonable constituents early on
	    (roles ?!roles)
	    (post-subcat -)
	    (no-postmodifiers -) ;; exclude "the same path as the battery" and advbl attaching to "path"
	    )
      )
     (advbl (ATYPE POST)  
      (ARGUMENT (% NP (sem ?argsem)))
      (arg ?v1) (VAR ?mod) (WH -) (GAP -)
      (role ?advrole) (subcatsem ?advsem)
      )
     (role-UNIFY (arg0 ?advrole) (arg1 ?advsem) (arg2 ?!roles) (arg3 ?restr))
     (add-to-conjunct (val (MODS ?mod)) (old ?restr) (new ?new))
     )
||#
    ;; 500 mb or more
    ;; note that this rule doesn't handle -500 mb of ram or more; -a 500 mb ram or more
    ;; note also that there is a similar rule in phrase-grammar.lisp for NP or adj-er
    ((NP (var ?v1) (spec ?spec)
      (LF (% description (status quantity-term) (var ?v1) (sort ?lfst)
			 (class ?class) (sem ?lfsem)
			 (argument ?ag)
			 (constraint  ?new)))
         (sort ?st)  (case ?case) (class ?class) (qtype ?qt) (wh ?w)
	 )
     -adv-or-comparative> 
     (head (NP (VAR ?v1) (lex ?nlex)
               (sort ?st)  (case ?case)
	       (SEM ?sem) (spec ?spec) (qtype ?qt) (wh ?w)
	       (LF (% description (status quantity-term) (sort ?lfst)
		      (sem ?lfsem) (argument ?ag) (class ?class) (constraint  ?restr)))
	       )
           )
     (word (lex or))
     (advbl (comparative +) (var ?av))
     (add-to-conjunct (val (& (MODS ?av))) (old ?restr) (new ?new))
     )
    
    ((NP (name +) (LF (% description (status ont::definite)
			 (var ?v1) (class ?class) (sem ?lfsem)
			 (constraint  ?new)))
         (sort ?sort)  (case ?case))
     -adv-route-post> 0.97 
     (head (NP (VAR ?v1) (name +) (lex ?nlex)
               (sort ?sort)  (case ?case)
	       (SEM ?argsem)  (LF (% description (sem ?lfsem) (class ?class) (constraint  ?restr)))
	       )
           )
     (advbl (ATYPE POST)
      (ARGUMENT (% NP (sem ?argsem)))
      (ROLE (? role ONT::Spatial-loc))
      (subcatsem ($ f::phys-obj (f::form f::geographical-object) (f::object-function f::spatial-object )))
      (arg ?v1) (VAR ?mod) (WH -) (GAP -)
      )
     (add-to-conjunct (val (& (MODS ?mod) (NAME-OF ?nlex))) (old ?restr) (new ?new))
     )
    
    ;; route 252 at marketplace mall
    ((NP (name +) (LF (% description (constraint  ?new)))
         (sort ?sort)  (case ?case))
     -adv-post-route> 0.96
     (head (NP (VAR ?v1) (name +) 
               (sort ?sort)  (case ?case)
	    (SEM ?argsem)  (LF (% description (constraint  ?restr)))
	    (sem ($ f::phys-obj (f::form f::geographical-object) (f::spatial-abstraction (? sa f::line f::strip f::spatial-region))))
	    )
      )
     (advbl (ATYPE POST)
            (ARGUMENT (% NP (sem ?argsem)))
            (ROLE (? role ONT::Spatial-loc))
            (subcatsem ($ f::phys-obj (f::form f::geographical-object) (f::object-function f::spatial-object)))
            (arg ?v1) (VAR ?mod) (WH -) (GAP -)
            )
     (add-to-conjunct (val (MODS mod)) (old ?restr) (new ?new))
     )
    
    ;;  UTT modification  -- we need a better treatment here
    ;;  
    ;;  e.g., There I saw the train
    
    ((Utt (sa ?sa) (ended -)
          (setting ?set) 
          (LF (% prop (var ?v) (class ?c) (constraint ?new)
		 (sem ?argsem)
		 )))
     -pre-utt-advbl-decl> .98
     (advbl (LF ?con) (WH -) (ATYPE PRE) (ARG ?v) (GAP -) 
      (ARGUMENT (% UTT (sem ?argsem)))
      )
     (head (utt (ended -) (stype decl) (sa ?sa) (lf (% prop (sem ?sem) (var ?v) (class ?c) (constraint ?lf1)))))
     (add-to-conjunct (val (MODS ?con)) (old ?lf1) (new ?new)))

    ;; Less likely for whq, although consider Previously, had you seen the train?
    ;;            and Carefully open the door.

    ((Utt (sa ?sa) (ended -)
          (setting ?set) 
          (LF (% prop (var ?v) (class ?c) (constraint ?new)
		 (sem ?argsem)
		 )))
     -pre-utt-advbl-other> .90
     (advbl (LF ?con) (WH -) (ATYPE PRE) (ARG ?v) (GAP -) 
      (ARGUMENT (% UTT (sem ?argsem)))
      )
     (head (utt (ended -) (stype (? x ynq whq imp)) (sa ?sa) (lf (% prop (sem ?sem) (var ?v) (class ?c) (constraint ?lf1)))))
     (add-to-conjunct (val (MODS ?con)) (old ?lf1) (new ?new)))

    ((Utt (sa ?sa) (ended -)
          (setting ?set) 
          (LF (% prop (sem ?argsem) (var ?v) (class ?c) (constraint ?new))))
     -post-utt-advbl> .98
     (head (utt (ended -) (sa ?sa) (lf (% prop (sem ?argsem) (var ?v) (class ?c) (constraint ?lf1) ))))
     (advbl (VAR ?mod) (WH -) (ATYPE POST) (ARG ?v) (GAP -) 
      (ARGUMENT (% UTT (sem ?argsem)))
      )
      (add-to-conjunct (val (MODS ?mod)) (old ?lf1) (new ?new))
      )

    ;;============================
    ;; 
    ;; The next two rules could in principle be combined, but then we would have to define
    ;;  all the head features by hand
    
    ;;  ADJ modification.
    ;;  TEST: very red
    
    ((ADJP (LF (% PROP (CLASS ?c) (VAR ?v) (CONSTRAINT ?newc) (sem ?sem)))
           (val ?val) (agr ?agr) (mass ?mass) (var ?v) (ARG ?arg) (gap ?gap) 
      (argument ?argmt) (premod +) (sem ?sem)
      )
     -advbl-adj-pre>
     (advbl (ATYPE PRE) (VAR ?advbv) (ARG ?v) ;;(SORT OPERATOR) 
            (argument (% ADJP (sem ?sem)))
            (gap -)
      )
     (head (ADJP (lf (% PROP (CLASS ?c) (VAR ?v) (CONSTRAINT ?con) (sem ?sem))) 
	    (val ?val) (agr ?agr) (mass ?mass) (argument ?argmt) (arg ?arg)
	    (gap ?gap) (premod -) (sem ?sem)
                 ))
     (add-to-conjunct (val (MODS ?advbv)) (old ?con) (new ?newc))
     
     )

    ;; nonprefix adverbial modification of ADJ with hyphen

     ((ADJP (LF (% PROP (CLASS ?c) (VAR ?v) (CONSTRAINT ?newc) (sem ?sem)))
           (val ?val) (agr ?agr) (mass ?mass) (var ?v) (ARG ?arg) (gap ?gap) 
      (argument ?argmt) (premod +) 
      )
     -advbl-hyphen-adj-pre> 1 
     (advbl (ATYPE PRE) (VAR ?advbv) (ARG ?v) ;;(SORT OPERATOR) 
            (argument (% ADJP (sem ?sem)))
            (gap -) (complex -)
      )
      (word (lex w::punc-minus))
      (head (ADJP (lf (% PROP (CLASS ?c) (VAR ?v) (CONSTRAINT ?con) (sem ?sem))) 
		  (val ?val) (agr ?agr) (mass ?mass) (argument ?argmt) (arg ?arg)
		  (gap ?gap) (premod -) 
		  ))
      (add-to-conjunct (val (MODS ?advbv)) (old ?con) (new ?newc))
     
      )
#||
    ;;   more/as/so ADJP than/as/that 
    ;;   e.g., more sensitive than that
    ((ADJP (LF (% PROP (CLASS ?lf) (VAR ?v) (CONSTRAINT ?newc) (sem ?sem)))
      (val ?val) (agr ?agr) (mass ?mass) (var ?v) (ARG ?arg) (gap ?gap) 
      (argument ?argmt) (premod +) 
      )
     -comp-with-ground>
     (head (ADJ (comparative +) (lf ?lf) (ground-subcat ?!gsub)
		(ground-subcat (% ?cat (var ?vg)))
		(val ?val) (agr ?agr) (mass ?mass) (argument ?argmt) (arg ?arg)
		(gap ?gap) (premod -) 
		))
     ?!gsub
     ;;(word (lex ?pt))
     ;;(np (var ?vnp))
     (add-to-conjunct (val (& (figure ?arg) (ground ?vg))) (old ?con) (new ?newc))
     )

    ;; e.g.,  more sensitive to heat than that
    ((ADJP (LF (% PROP (CLASS ?lf) (VAR ?v) (CONSTRAINT ?newc) (sem ?sem)))
      (val ?val) (agr ?agr) (mass ?mass) (var ?v) (ARG ?arg) (gap ?gap) 
      (argument ?argmt) (premod +) 
      )
     -comp-with-subcat+ground>
     (head (ADJ (comparative +) (lf ?lf) (subcat ?!subcat)
		(subcat (% ?subcatcat (var ?vsc)))
		(subcat-map ?sc-map)
		(ground-subcat ?!gsub)
		(ground-subcat (% ?cat (var ?vg)))
		(val ?val) (agr ?agr) (mass ?mass) (argument ?argmt) (arg ?arg)
		(gap ?gap) (premod -) 
		))
     ?!subcat
     ?!gsub
     ;;(word (lex ?pt))
     ;;(np (var ?vnp))
     (add-to-conjunct (val (& (figure ?arg) (ground ?vg) (?sc-map ?vsc))) (old ?con) (new ?newc))
     )
||#
    

    ;;  as ADJ as-PP
    ((ADJP (LF (% PROP (CLASS (:* ONT::AS-MUCH-AS ?w)) (VAR ?v) (CONSTRAINT ?newc) (sem ?sem)))
      (val ?val) (agr ?agr) (mass ?mass) (var ?v) (ARG ?arg) (gap ?gap) 
      (argument ?argmt) (premod +) 
      )
     -as-adj-as>
     (word (lex W::as))
     (head (ADJP (lf (% PROP (CLASS (:* ?c ?w)) (VAR ?v) (CONSTRAINT ?con) (sem ?sem)))
	    (val ?val) (agr ?agr) (mass ?mass) (argument ?argmt) (arg ?arg)
	    (gap ?gap) (premod -) 
                 ))
     (word (lex w::as))
     (np (var ?vnp) (gap -))
     (add-to-conjunct (val (ground ?vnp)) (old ?con) (new ?newc))
     )

     ;;  as ADJ as-PP
    ((ADJP (LF (% PROP (CLASS (:* ONT::AS-MUCH-AS ?w)) (VAR ?v) (CONSTRAINT ?newc) (sem ?sem)))
      (val ?val) (agr ?agr) (mass ?mass) (var ?v) (ARG ?arg) (gap ?gap) 
      (argument ?argmt) (premod +) 
      )
     -as-adj-as-s>
     (word (lex W::as))
     (head (ADJP (lf (% PROP (CLASS (:* ?c ?w)) (VAR ?v) (CONSTRAINT ?con) (sem ?sem)))
	    (val ?val) (agr ?agr) (mass ?mass) (argument ?argmt) (arg ?arg)
	    (gap ?gap) (premod -) 
                 ))
     (word (lex w::as))
     (S (var ?sv) (gap -))
     (add-to-conjunct (val (standard ?sv)) (old ?con) (new ?newc))
     )

    ;;  as quickly as my dog
    ((ADVBL (LF (% PROP (CLASS (:* ONT::AS-MUCH-AS ?w)) (VAR ?v) (CONSTRAINT ?newc) (sem ?sem)))
      (val ?val) (agr ?agr) (mass ?mass) (var ?v) (ARG ?arg) (gap ?gap) 
      (argument ?argmt) (premod +) 
      )
     -as-adv-as-np>
     (word (lex W::as))
     (head (ADVBL (lf (% PROP (CLASS (:* ?c ?w)) (VAR ?v) (CONSTRAINT ?con) (sem ?sem)))
	    (val ?val) (agr ?agr) (mass ?mass) (argument ?argmt) (arg ?arg)
	    (gap ?gap) (premod -) 
                 ))
     (word (lex w::as))
     (np (var ?vnp) (gap -))
     (add-to-conjunct (val (standard ?vnp)) (old ?con) (new ?newc))
     )

    ;;  as ADV as he can be
    ((ADVBL (LF (% PROP (CLASS (:* ONT::AS-MUCH-AS ?w)) (VAR ?v) (CONSTRAINT ?newc) (sem ?sem)))
      (val ?val) (agr ?agr) (mass ?mass) (var ?v) (ARG ?arg) (gap ?gap) 
      (argument ?argmt) (premod +) 
      )
     -as-adv-as-s>
     (word (lex W::as))
     (head (ADVBL (lf (% PROP (CLASS (:* ?c ?w)) (VAR ?v) (CONSTRAINT ?con) (sem ?sem)))
	    (val ?val) (agr ?agr) (mass ?mass) (argument ?argmt) (arg ?arg)
	    (gap ?gap) (premod -) 
                 ))
     (word (lex w::as))
     (S (var ?sv) (gap -))
     (add-to-conjunct (val (standard ?sv)) (old ?con) (new ?newc))
     )

    ;; so ADJ/ADVBL that ...
     ((ADJP (LF (% PROP (CLASS (:* ONT::SO-MUCH-THAT ?w)) (VAR ?v) (CONSTRAINT ?newc) (sem ?sem)))
           (val ?val) (agr ?agr) (mass ?mass) (var ?v) (ARG ?arg) (gap ?gap) 
      (argument ?argmt) (premod +) 
      )
     -so-adj-that>
     (word (lex W::so))
     (head ((? cat ADJP ADVBL)
	    (lf (% PROP (CLASS (:* ?c ?w)) (VAR ?v) (CONSTRAINT ?con) (sem ?sem))) 
	    (val ?val) (agr ?agr) (mass ?mass) (argument ?argmt) (arg ?arg)
	    (gap ?gap) (premod -) 
                 ))
     ;;(word (lex w::that))
     (cp (var ?vs) (gap -) (ctype (? ctype W::S-THAT-MISSING W::S-THAT-OVERT)))
     (add-to-conjunct (val (standard ?vs)) (old ?con) (new ?newc))
     )
#||
    ;; so ADV that ...
     ((ADVBL (LF (% PROP (CLASS (:* ONT::SO-MUCH-THAT ?w)) (VAR ?v) (CONSTRAINT ?newc) (sem ?sem)))
           (val ?val) (agr ?agr) (mass ?mass) (var ?v) (ARG ?arg) (gap ?gap) 
      (argument ?argmt) (premod +) 
      )
     -so-adv-that>
     (word (lex W::so))
     (head (ADVBL (lf (% PROP (CLASS (:* ?c ?w)) (VAR ?v) (CONSTRAINT ?con) (sem ?sem))) 
	    (val ?val) (agr ?agr) (mass ?mass) (argument ?argmt) (arg ?arg)
	    (gap ?gap) (premod -) 
                 ))
     (word (lex w::that))
     (S (var ?vs))
     (add-to-conjunct (val (ground ?vs)) (old ?con) (new ?newc))
     )
     ||#

     #|
    ;; TEST: red enough
    ((ADJP (LF (% PROP (CLASS ?c) (VAR ?v) (CONSTRAINT ?newc) (sem ?sem)))
           (val ?val) (agr ?agr) (mass ?mass) (var ?v) (ARG ?arg) (gap ?gap)
           (argument ?argmt))
     -advbl-adj-post>
   
     (head (ADJP (lf (% PROP (CLASS ?c) (VAR ?v) (CONSTRAINT ?con) (sem ?sem))) 
	    (val ?val) (agr ?agr) (mass ?mass) (argument ?argmt)
	    (gap ?gap)
                 ))
     (advbl (ATYPE postpositive) (VAR ?advbv) (ARG ?v)
            (argument (% ADJP (sem ?sem) ))
            (gap -)
            )
     (add-to-conjunct (val (MODS ?advbv)) (old ?con) (new ?newc))
     
     )
|#

    ;;  ADV modification
    ;; TEST: very quickly 
    ((ADVBL (LF (% PROP (CLASS ?c) (VAR ?v) (CONSTRAINT ?newc) (sem ?sem)))
            (val ?val) (agr ?agr) (mass ?mass) (var ?v) (ARG ?arg)
      (argument ?argmt) (role ?role) (subcatsem ?subcatsem)
      (gap ?gap)
      )
     -advbl-adv-pre>
     (advbl (ATYPE PRE) (VAR ?advbv) (ARG ?v) ;;(SORT OPERATOR) 
            (argument (% ADVBL (sem ?sem) (sort ?sort)(comparative ?cmp) ))
            (gap -)
            )
     (head (ADVBL (lf (% PROP (CLASS ?c) (VAR ?v) (CONSTRAINT ?con) (sem ?sem))) 
	    (val ?val) (agr ?agr) (mass ?mass) (argument ?argmt) (sort ?sort)
	    (role ?role) (subcatsem ?subcatsem) (arg ?arg)(comparative ?cmp)
	    (gap ?gap)
	    ))
     (add-to-conjunct (val (MODS ?advbv)) (old ?con) (new ?newc))
     
     )

      ;;  ADV modification
      ;; TEST: quickly enough 
    ((ADVBL (LF (% PROP (CLASS ?c) (VAR ?v) (CONSTRAINT ?newc) (sem ?sem)))
            (val ?val) (agr ?agr) (mass ?mass) (var ?v) (ARG ?arg)
      (argument ?argmt) (role ?role) (subcatsem ?subcatsem)
      (gap ?gap)
      )
     -advbl-adv-post>
     (head (ADVBL (lf (% PROP (CLASS ?c) (VAR ?v) (CONSTRAINT ?con) (sem ?sem))) 
	    (val ?val) (agr ?agr) (mass ?mass) (argument ?argmt) (sort ?sort)
	    (role ?role) (subcatsem ?subcatsem) (arg ?arg)
	    (gap ?gap)(comparative ?cmp)
	    ))
       (advbl (ATYPE postpositive) (VAR ?advbv) (ARG ?v)
            (argument (% ADVBL (sem ?sem) (sort ?sort)(comparative ?cmp) ))
            (gap -)
            )
     (add-to-conjunct (val (MODS ?advbv)) (old ?con) (new ?newc))
     )
      


    ;;  COMPARATIVE CONSTRUCTIONS

  #||  ;; TEST: more efficiently
    ((ADV (LF ONT::MORE-VAL) (COMPARATIVE +) (ALLOW-POST-N1-SUBCAT +) (ARGUMENT-MAP FIGURE) (functn ?pred)
      (SUBCAT (% PP (PTYPE W::THAN) (VAR ?v))) 
      (subcat-map GROUND) (argument ?argument) (sort ?s) (arg ?a) (atype ?at) (var ?v)
      (sem ?sem)
      )
     -compar-more-adv>
     (word (lex more))
     (head (adv (LF ?pred) (sort ?s) (arg ?a) (atype ?at) (var ?v) (sem ?sem)))
     )
    ;; TEST: most efficiently
    ((ADV (LF ONT::MAX-VAL) (COMPARATIVE w::SUPERL) (ARGUMENT-MAP FIGURE) (functn ?pred) (functn-map ?argmap)
      (SUBCAT (% PP (PTYPE W::OF) (VAR ?v))) ;; MD 03/04/2009 we need a subcat or otherwise adv-compar-obj-deleted doesn't work
      (argument ?argument) (sort ?s) (arg ?a) (atype ?at) (var ?v)
      (sem ?sem)
      )
     -compar-most-adv>
     (word (lex most))
     (head (adv (LF ?pred) 
		(sort ?s) (arg ?a) 
		(atype ?at) (var ?v) 
		(sem ?sem)
		(argument-map ?argmap)
		))
     )
   
    ;; TEST: less efficiently
   ((ADV (LF ONT::LESS-VAL) (COMPARATIVE +) (ALLOW-POST-N1-SUBCAT +) (ARGUMENT-MAP FIGURE) (functn ?pred)
     (SUBCAT (% PP (PTYPE W::THAN) (VAR ?v))) (subcat-map GROUND) (argument ?argument) (sort ?s) (arg ?a) (atype ?at) (var ?v)
     (sem ?sem)
     )
    -compar-less-adv>
    (word (lex less))
    (head (adv (LF ?pred) (sort ?s) (arg ?a) (atype ?at) (var ?v) (sem ?sem)))
    )
    ;; TEST: least efficiently
    ((ADV (LF ONT::MIN-VAL) (COMPARATIVE w::SUPERL) (ARGUMENT-MAP FIGURE) (functn ?pred)
      (argument ?argument) (sort ?s) (arg ?a) (atype ?at) (var ?v)
      (sem ?sem)
      )
     -compar-least-adv>
     (word (lex least))
     (head (adv (LF ?pred) (sort ?s) (arg ?a) (atype ?at) (var ?v) (sem ?sem)))
     )
  ||#
    ;; qmodifiers with bare numbers
    ;; TEST: exactly five
    ((number (agr ?agr) (VAR ?v) (MASS ?mn) (lf ?lf) (sem ?sem) (premod +) ;;(val ?val)
	     (nobarespec +) ; this can't be a specifier -- that goes through the cardinality rules
	     (restr (& (mods (% *PRO* (status ont::F) (class ?lfa) (var ?v1) 
				(constraint (& (FIGURE ?v) 
					       (GROUND (% *PRO* (status ont::indefinite) (var *) (class ont::number) (constraint (& (value ?val)))))))))))
	     )
     -advbl-bare-number-pre>
     (adv (ATYPE PRE) (VAR ?v1) (argument (% number)) (Mass ?m) (lf ?lfa))
     (head (number (VAR ?v) (lf ?lf) (lex ?l) (agr ?agr) (MASS ?mn) (sem ?sem) (val ?val) (premod -)
		      ))
     )

    ;; TEST: more than five (trucks)
    ((number (agr ?agr) (VAR ?v) (MASS ?mn) (lf ?lf) (sem ?sem) (premod +) ;;(val ?val)
	     (nobarespec +) ; this can't be a specifier -- that goes through the cardinality rules
	     (restr (& (mods (% *PRO* (status ont::F) (class ?lfa) (var ?v1) 
				(constraint (& (FIGURE ?v) 
					       (GROUND (% *PRO* (status ont::indefinite) (var *) (class ont::number) (constraint (& (value ?val)))))))))))
	     )
     -advbl-bare-number-pre-than>
     (adv (VAR ?v1) (argument (% number)) (Mass ?m) (lf ?lfa))
     (word (lex w::than))
     (head (number (VAR ?v) (lf ?lf) (lex ?l) (agr ?agr) (MASS ?mn) (sem ?sem) (val ?val) (premod -)
		      ))
     )
    
    ;; TEST: eight or so
    ((number (agr ?agr) (VAR ?v) (MASS ?mn) (lf ?lf) (sem ?sem) (premod +) ;;(val ?val)
	     (nobarespec +) ; this can't be a specifier -- that goes through the cardinality rules
	     (restr (& (mods (% *PRO* (status ont::F) (class (:* ont::precision-val w::approximate)) (var *) 
				(constraint (& (FIGURE ?v) 
					       (GROUND (% *PRO* (status ont::indefinite) (var **) (class ont::number) (constraint (& (value ?val)))))))))))
	     )
     -number-or-so>
     (head (number (VAR ?v) (lf ?lf) (lex ?l) (agr ?agr) (MASS ?mn) (sem ?sem) (val ?val) (premod -)
		   ))
     (word (lex w::or))
     (word (lex w::so))
     )
    
    ;; and the special case for "a" -- e.g., only a week, just a candy, ..
   
    ((NP (LF (% description (status ont::indefinite)
			 (var ?v1) (class ?class) (sem ?lfsem)
			 (constraint ?new)))
      (sort ?sort) (case ?case))
     -only-a> 
     (adv (ATYPE PRE) (VAR ?v) (argument (% number)) (Mass ?m) (lf ?lfa))
     (head (NP (VAR ?v1) (lex ?nlex) (agr 3s)
               (sort ?sort) (case ?case)
	       (LF (% description (status ont::indefinite) (sem ?lfsem) (class ?class) (constraint  ?restr)))
	       )
      )
     (add-to-conjunct (val (MODS (% *PRO* (status ont::F) (class ?lfa) (var ?v) 
				    (constraint (& (FIGURE ?v1))))))
      (old ?restr) (new ?new))
     )
    
     ;; a little/a lot/slightly more than five degrees
     ((QUANP (agr ?agr) (VAR ?v) (MASS COUNT) (STATUS ?status)
            (LF (% DESCRIPTION (VAR ?v) (STATUS QUANTITY-TERM) (CLASS ONT::NUMBER) ;;(CONSTRAINT (& (?mod ?v)))
                   (constraint ?newc)
                   )))
     -advbl-number-pre2>
     (advbl (ATYPE PRE) (VAR ?advbv) (argument (% cardinality)) (Mass ?m) (LF  ?mod))
     (head (cardinality (VAR ?v) (agr ?agr) (MASS COUNT) (STATUS ?status) (LF ?lf)
                      (LF (% DESCRIPTION (CONSTRAINT (& (VALUE ?val)))))
                        (constraint ?con)
                                        ))
     (add-to-conjunct (val (MODS ?advbv)) (old ?con) (new ?newc))
     )

   ;; under a thousand
     ((QUANP (agr ?agr) (VAR ?v1) (MASS COUNT) (STATUS ?status)
	    (LF (% DESCRIPTION (VAR ?v1) (STATUS QUANTITY-TERM) (CLASS ONT::NUMBER) (CONSTRAINT (& (?mod ?v))))))
     -advbl-number-quan-pre>
     (adv (ATYPE PRE) (VAR ?v1) (argument (% cardinality)) (Mass ?m) (LF  ?mod))
     (head (quanp (VAR ?v) (agr ?agr) (MASS COUNT) (STATUS ?status) (LF ?lf)
		  (LF (% DESCRIPTION (CONSTRAINT (& (VALUE ?val)))))))
     )

    ;; DISC Adverbs

    ;;  Discourse adverbials and conjuncts
    ;;    these set the CONSTRAINT feature

     ;; Myrosia slightly lowered the preferences so adverbials that can fit into slots don't get treated as discourse adverbials
    ;;   I set them back to normal, I think the discourse interp is usaully the right one. JFA 12/02
    ;; TEST: then the dog barked
    ;; TEST: now chase the cat
    ((Utt (LF (% SPEECHACT (var ?v) (class ?sa) (constraint ?adv))) (var ?v) (punctype ?pn))
     -disc1>
     ;; swift -- adding check for subjvar since for topic advbls this must be filled
     (advbl (sort DISC) (ATYPE PRE) (arg ?v) (SA-ID -) (VAR ?advv) (gap -) (argument (% UTT (subjvar ?sv))))
     (head (Utt (ended -) (LF (% SPEECHACT (class ?sa) (constraint ?adv1))) (var ?v) (punc -) (subjvar ?sv)))
     (add-to-conjunct (val (MODS ?advv)) (old ?adv1) (new ?adv)))

    ((Utt (LF (% SPEECHACT (class ?sa) (var ?v) (constraint ?adv))) (var ?v) (punctype ?pn))
     -disc-comma> 
     (advbl (sort DISC) (ATYPE PRE) (arg ?v) (SA-ID -) (VAR ?advv) (gap -) (argument (% UTT)))
     (punc (lex w::punc-comma))
     (head (Utt (ended -) (var ?v) (LF (% SPEECHACT (class ?sa) (constraint ?adv1))) (punc -) (punctype ?pn)))
     (add-to-conjunct (val (MODS ?advv)) (old ?adv1) (new ?adv)))

    ; TEST: The dog barked then
    ((Utt (LF (% SPEECHACT (var ?v) (class ?sa) (constraint ?adv))) (var ?v)
	  (punctype ?pn)
          ;;(POSTADVBL +)
          )
     -disc2> 0.96
     
     (head (Utt (ended -) (no-post-adv -) (LF (% SPEECHACT (class ?sa) (constraint ?adv1))) (var ?v) (punctype ?pn) ))
     ;; beetle fix -- Myrosia moved (Argument (% UTT)) where it should be for discourse adverbials
     ;; Potential ambiguity here because these days discourse adverbials also allowed for post-vp positions
     ;; but this is necessary for making things work in cases like "is this true as well"?
     (advbl (sort DISC) (ATYPE POST) (SA-ID -) (arg ?v) (var ?advv) (gap -) (ARGUMENT (% UTT)))
     (add-to-conjunct (val (MODS ?advv)) (old ?adv1) (new ?adv)))
     
    ))

(parser::augment-grammar
  
 '((headfeatures
    (ADVBL SEM ARGSORT TO transform neg)
    (NP VAR CASE MASS NAME agr Changeagr argument subcat argument-map transform)
    )
   ))
   ;; Myrosia 03/08/02: made a fix to "how" rules. Now they gram whatever SEM is set for an adj
   ;; and use it to produce their own sem
   ;;  HOW X questions. make "how x" a PP-WORD to create WH object for questions or NP complements
    
 ;;  ((NP (PP-WORD +) (SORT (? x QUALITY)) (VAR ?v) (lex HOW-X) ; NB: to change this HOW-X, we must fix rule wh-question2>
 ;;    (WH Q) (SEM ?npsem) 
 ;;    (LF (% Description (status how-much) (var ?v) (Class ?lf) (SORT STUFF)
;;	    (Lex ?lex) (sem ?npsem) (transform ?transform)
;;	    ))
 ;;    ;;(sort CONSTRAINT)
  ;;   )
 ;;   -how-adj> .96 
 ;;   (word (LEX how))
 ;;   (head (adj (LF ?lf) (ARGUMENT-MAP ?argmap) (premod (% np (sem ?npsem))) (lex ?lex) (transform ?transform)))
 ;;   )

(parser::augment-grammar
 '((headfeatures
    (VP vform var agr neg sem subj iobj dobj comp3 part cont class subjvar lex headcat transform tma subj-map template)
    (VP- vform var agr neg sem subj iobj dobj comp3 part cont class subjvar lex headcat transform subj-map tma aux passive passive-map template result)
    (pp headcat lex)
    (advbl gap headcat lex neg)
    )

((vp- (constraint ?new)
     (tma ?tma)
     (gap ?!gap) (var ?v)
     (class ?class) (sem ?argsem) (vform ?vf)      
     (advbl-needed -)
     )
    -adv-gap-vp-post> .98
    (head (vp- (VAR ?v) 
	      (SEM ?argsem) 
	      (constraint ?lf)
	      (class ?class) (vform ?vf)
	      (tma ?tma)
	      (gap -) (aux -)
	      ))
    (advbl (ATYPE POST) ;;(SORT CONSTRAINT) 
           (ARGUMENT (% S (sem ?argsem))) 
           (gap ?!gap)
           (ARG ?v) (VAR ?mod)
           (role ?advrole) 
           )
    (add-to-conjunct (val (MODS ?mod)) (old ?lf) (new ?new))
    )

   ((PP (PTYPE ?pt) (lf ?lf) (case ?c)
		(lex ?pt) (headcat ?hc)
     (sem ($ ?somesem)) (var ?gapvar) (agr ?agr)     
     (gap (% np (lf ?lf) (var ?gapvar) (gap -) (sort (? sort pred descr wh-desc)) (case (? case obj -)) (agr ?agr)))
     )					; I set the case here to a var, in order to allow -np-spec-of-pp> to work. Otherwise, CASE is not used in PPs
    -pp1-gap> 0.98
    (head (prep (LEX ?pt) (headcat ?hc)))
    )

   ;; distance NPs as adverbials
   ;; TEST: move it one inch
   ;; TEST: move it two degrees
   ;; TEST: walk a short distance
   ;; TEST: The market fell three percent
   ((advbl (arg ?arg) ;;(role (:* ONT::distance W::quantity)) 
     (var *) (subj ?anysubj)
	   (sort binary-constraint)
	   (LF (% PROP (VAR *) (CLASS ONT::extent-predicate) (sem ?sem)
		  (CONSTRAINT (& (FIGURE ?arg) (scale ?scale) (GROUND ?v)))))
	   (atype (? x W::PRE W::POST))
     (argument (% W::S (subjvar ?anysubj)
                          ;; W::NP
			  ;; W::VP)
		  (SEM ($ F::situation (f::type (? xx ont::event-of-action ont::event-of-state)))))) ;;SITUATION (F::trajectory +)))))))
     )
    -distance-np-advbl> 1.0 ;.97
    (head (np (var ?v) (sort unit-measure) (sem ?sem)  
	      (bare -) ;; we suppress this rule for distances without a specific amount (e.g., "miles")
	      ;; the semantic restriction is not sufficient to prevent measure-unit phrases such as "a bit" or "a set" as distances so using the lfs to restrict
	      (lf (% description (constraint (& (scale ?scale)))))
	      (sem ($ f::abstr-obj (f::scale (? sc ont::scale ont::measure-domain))))
	      (class  ont::quantity);;(? lft ont::angle-unit ont::length-unit ont::percent ont::distance))
	      ;; well, 'he walked miles before he reached water'; 'he crawled inches to the next exit' ...; and this restriction prevents the non-unit NPs so if it's reinstated we need two rules
;	      (lf (% description (sort set))) ;; this restriction is needed to prevent bare measure units as adverbials
	      ))
    )

      ;; ing VPs as adverbials
      ;; TEST: Barking, the dog chased the cat.
   ;; TEST: The dog chased the cat barking.
   ((advbl (arg ?arg) (sem ($ f::abstr-obj (f::information -) (f::intentional -)))
     (argument (% S (sem ($ f::situation (f::aspect f::dynamic))) (subjvar ?!subjvar) (subj ?!subj))) 
     (sort pred) (gap -) (atype (? atp pre post))
     (role ONT::MANNER) (var **)
     (LF (% PROP (CLASS ONT::IMPLICIT-OVERLAP) (VAR **) 
	    (CONSTRAINT (& (FIGURE ?arg) (GROUND ?v)))
	    (sem ($ f::abstr-obj (f::information -) (f::intentional -)))))
     )
    -vp-ing-advbl> .98
    (head (vp (vform ing) (var ?v) (gap -) (aux -) (advbl-necessary -)
	   (constraint ?con)  (transform ?transform) (class ?class)
	   (subj (% np (var ?subjvar) (agr ?subjagr) (sem ?subjsem) (gap -)))
	   ;(subjvar (% *PRO* (VAR *) (gap -) (sem ?subjsem)))
	   (subjvar ?!subjvar)
	   (subj ?!subj)
	   ))
    )
#||   I don't think we can distinguish RESULT well from temporal overlap
 ((advbl (arg ?arg) (sem ($ f::abstr-obj (f::information -) (f::intentional -)))
     (argument (% S (sem ($ f::situation (f::aspect f::dynamic))))) 
     (sort pred) (gap -) (atype (? atp pre post))
     (role ONT::MANNER) (var **)
     (LF (% PROP (CLASS ONT::result) (VAR **) 
	    (CONSTRAINT (& (FIGURE ?arg) (GROUND ?v)))
	    (sem ($ f::abstr-obj (f::information -) (f::intentional -)))))
     )
    -vp-ing-as-result-advbl> 0.93
    (head (vp (vform ing) (var ?v) (gap -) (aux -) (advbl-necessary -)
	   (constraint ?con)  (transform ?transform) (class ?class)
	   (subj (% np (sem ?subjsem)))
	   (subjvar (% *PRO* (VAR *) (gap -) (sem ?subjsem)))
	   ))
    )||#

   ;; the following rule could be handled lexically, with a new sense of IN, but
   ;; its seems idiosyncratic enough to warrant a special rule.
   
   ;; in a harsh way, in a certain manner, --> manner adverbial
   ((advbl (arg ?arg) (sem ($ f::abstr-obj (f::information -) (f::intentional -)))
     (argument (% S (sem ($ f::situation (f::aspect f::dynamic))))) 
     (subjvar ?subjvar) (subj ?subj)
     (sort pred) (gap -) (atype (? atp pre post))
     (var *)
     (LF (% PROP (CLASS ONT::Manner) (VAR *) 
	    (CONSTRAINT (& (FIGURE ?arg) (GROUND ?v)))
	    (sem ($ f::abstr-obj (f::information -) (f::intentional -)))))
     )
    -manner-advbl> 
    (prep (lex in))
    (head (np (var ?v) (lex (? lex W::manner w::way w::fashion))))
    )

   ;;   Myself as an adverbial, I opened the door myself
    ((ADVBL (ARG ?arg) 
      (SORT BINARY-CONSTRAINT) (var *)
       (LF (% PROP (var *) (CLASS ONT::EXCLUSIVE) 
	        (Constraint (& (FIGURE ?arg) (GROUND ?v)))))
      (ATYPE w::post) (focus ?v)
      (ARGUMENT (% (? x W::VP W::S) (subjvar ?subjv)))
      (SEM ?sem))
     -myself-as-advbl> .98
     (head (np  (var ?v) (REFL +) (PRO +)
		(sem ($ (? aa  f::situation f::abstr-obj f::phys-obj)))
		)))
      
    ;;  by myself
    ((ADVBL (ARG ?arg) 
      (SORT BINARY-CONSTRAINT) (var *)
       (LF (% PROP (var *) (CLASS ONT::EXCLUSIVE) 
	        (Constraint (& (FIGURE ?arg) (GROUND ?v)))))
      (ATYPE (? xx w::post w::pre w::pre-vp)) (focus ?v)
      (lex ?hlex) (headcat ?hcat)
      (ARGUMENT (% (? x W::VP W::S) (subjvar ?subjv)))
      (SEM ?sem))
     -by-myself-as-advbl> .98
     (word (lex by))
     (head (np  (var ?v) (REFL +) (PRO +)
		(sem ($ (? aa  f::situation f::abstr-obj f::phys-obj)))
		)))
     ;;  all by myself
    ((ADVBL (ARG ?arg) 
      (SORT BINARY-CONSTRAINT) (var *)
       (LF (% PROP (var *) (CLASS ONT::EXCLUSIVE) 
	        (Constraint (& (FIGURE ?arg) (GROUND ?v)))))
      (ATYPE (? xx w::post w::pre w::pre-vp)) (focus ?v)
      (lex ?hlex) (headcat ?hcat) 
      
      (ARGUMENT (% (? x W::VP W::S) (subjvar ?subjv)))
      (SEM ?sem))
     -all-by-myself-as-advbl> .98
     (word (lex all))
     (word (lex by))
     (head (np  (var ?v) (REFL +) (PRO +)
		(sem ($ (? aa  f::situation f::abstr-obj f::phys-obj)))
		)))
  

   ))

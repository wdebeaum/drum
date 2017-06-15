;; THIS file contains term extraction rules for DRUM. It is intended to be loaded in conjunction after DRUMrules.lisp so does not have the usual initialization codes

#||
(setq *compute-force-from-tmas* nil) ;; disable interpretation of force (which eliminates modality and tense information)

(setq *extractor-oblig-features* '(:tense :progr :modality :perf :negation :passive :coref :while :until :manner :purpose))

(setq *extraction-rules* '(drum))

(reset-im-rules 'drum)  ;; this allows you to edit this file and reload it without having to reload the entire system
||#

(in-package "IM")

;(setq *term-extraction-rules* '(drumterms))
(reset-im-rules 'drumterms)

(mapcar #'(lambda (x) (add-im-rule x 'drumterms))  ;; sets of rules are tagged so they can be managed independently 
	'(

;; ** temporarily comment out this rule so it doesn't get used for extracting the :BASE in explicit-ref1 **
#|	  
	  ;; robust rule for explicit constructions, e.g., "RasGEF domain", when we want to return the variable (?!name) pointing to RasGEF but modify its type
	  ;; lower priority than -explicit-ref1, which returns a TERM pointing to ?!obj instead of ?!name
	  ;; used for dangling variables, e.g., in "SOS1's RasGEF domain"
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
	    (:* ?!type (? word W::PROTEIN W::GENE W::DRUG W::KINASE W::SITE W::POSITION W::MOLECULE W::DOMAIN W::PROMOTER W::MRNA W::TRANSCRIPT W::PROTEINS W::GENES W::DRUGS W::KINASES W::SITES W::POSITIONS W::MOLECULES W::DOMAINS W::PROMOTERS W::MRNAS W::TRANSCRIPTS))  ; mutation comes out as a verb in LIFE-TRANSFORMATION with AFFECTED protein (we get a TRANSFORM event for this)
	    :ASSOC-WITHS (?!name))
	   ((? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name 
;	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART) ?!w) :DRUM ?code)
	    ; this is to avoid mapping to "kinase domain"
	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART) (? !w W::PROTEIN W::GENE W::DRUG W::KINASE W::SITE W::POSITION W::MOLECULE W::DOMAIN W::PROMOTER W::MRNA W::TRANSCRIPT W::PROTEINS W::GENES W::DRUGS W::KINASES W::SITES W::POSITIONS W::MOLECULES W::DOMAINS W::PROMOTERS W::MRNAS W::TRANSCRIPTS)) :DRUM ?code)
	   -explicit-ref1x>
	   90
	   (ONT::TERM ?!name ?!type
	    :name ?!w
	    :drum ?code    
	    :rule -explicit-ref1x
	    )
	   )
|#
	  
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ; basic terms (not conjunctions)
	  ; phys-object: fragment, we, mutant
	  ; mental-construction (supertype of signaling-pathway): process, procedure, pathway, theory
	  ; awareness: hypothesis
	  ; information-function-object: result
	  ;
	  ; MUTATION and BIOLOGICAL-PROCESS are in EVENT-OF-CHANGE (the latter is matched in drumrules_ev)
	  ; PHYS-OBJECT contains MUTANT-OBJ and WILDTYPE-OBJ (the latter two are matched in drumrules_ev)
	  
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
;	    (:* (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT) ?w) :SEQUENCE - :DRUM ?code)

; took out EVENT-OF-CHANGE: it doesn't work well with term substitution (e.g., phosphorylation would lose all the AGENT/AFFECTED/etc roles when not returned with the TERM)
; but put back ONT::MUTATION and ONT::BIOLOGICAL-PROCESS (e.g., tumorigenesis) which were in EVENT-OF-CHANGE
	    (:* (? t1 
;ONT::EVENT-OF-CHANGE 
		   ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MEDICAL-DISORDERS-AND-CONDITIONS ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT) ?!w) :SEQUENCE - :DRUM ?code)
	   -simple-ref>
	   90
	   (ONT::TERM ?!obj ?t1
	    :name ?!w
	    :drum ?code
	    :rule -simple-ref
	    ))

	  ; the first protein
	((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	  ?reln ?!obj
;	    (:* (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT) ?w) :SEQUENCE - :DRUM ?code)

; took out EVENT-OF-CHANGE: it doesn't work well with term substitution (e.g., phosphorylation would lose all the AGENT/AFFECTED/etc roles when not returned with the TERM)
; but put back ONT::MUTATION and ONT::BIOLOGICAL-PROCESS (e.g., tumorigenesis) which were in EVENT-OF-CHANGE
	    (:* (? t1 
;ONT::EVENT-OF-CHANGE 
		   ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MEDICAL-DISORDERS-AND-CONDITIONS ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT) ?!w) :SEQUENCE (ONT::NTH ?x) :DRUM ?code)
	   -simple-ref-2>
	   90
	   (ONT::TERM ?!obj ?t1
	    :name ?!w
	    :drum ?code
	    :rule -simple-ref-2
	    ))
	  

#|
; matches the speech act: hack to return mutation/cell location/cell line that got covered by another term (e.g., G12V Ras; Ras in the nucleus)
; also return other entities now (e.g., Ras in "Ras-Raf complex")
; excludes certain words so protein in "Ras protein" will not match this rule (but the "Ras" in "Ras protein" still will)
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
;	    (:* (? t1 ONT::CELL-PART ONT::CELL-LINE ONT::MUTATION ONT::CHEMICAL ONT::MOLECULAR-PART) ?w) 
;	    (:* (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT) ?w)
	    (:* (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT) 
		(? !w W::PROTEIN W::GENE W::DRUG W::KINASE
		      W::FRAGMENT
		      W::WILDTYPE W::WILD-TYPE W::WILD-PUNC-MINUS-TYPE W::WT
		      W::MUTANT
		      W::COMPLEX W::PATHWAY
		   ))  
	    :SEQUENCE - :DRUM ?code)
;	   (ONT::SPEECHACT ?sa ?sa1)
	   (?spec-dummy ?v-dummy ?type-dummy :TENSE ?!tense)
	   -explicit-ref-part1>
	   100
	   (term ?!obj ?t1
	    :name ?!w
	    :drum ?code
	    :rule -explicit-ref-part1
	    ))

; does not match the speech act 
; match only the words that are excluded in -explicit-ref-part1
; this rule is applicable only for standalone "the protein", not "the Ras protein" (covered by -explicit-ref1)
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
	    (:* (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT) 
		(? w W::PROTEIN W::GENE W::DRUG W::KINASE
		      W::FRAGMENT
		      W::WILDTYPE W::WILD-TYPE W::WILD-PUNC-MINUS-TYPE W::WT
		      W::MUTANT
		      W::COMPLEX W::PATHWAY
		   ))  
	    :SEQUENCE - :DRUM ?code)
	   -explicit-ref-part2>
	   100
	   (term ?!obj ?t1
	    :name ?w
	    :drum ?code
	    :rule -explicit-ref-part2
	    ))

; matching the speech act: hack to return mods that are subsumed by another extraction rule (e.g., trans-phosporylate)
	  ((ONT::F ?!mod (:* (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY
				    ONT::MANNER-UNDO ONT::NEG
				    ONT::DEGREE-MODIFIER
				    ) ?w) :DRUM ?code)
;	   (ONT::SPEECHACT ?sa ?sa1)
	   (?spec-dummy ?v-dummy ?type-dummy :TENSE ?!tense)
	   -explicit-ref-part3>
	   100
	   (term ?!mod ?tmp
	    :name ?w
	    :drum ?code
	    :rule -explicit-ref-part3
	    ))
|#



	  ;; just the word "wildtype"
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
	    ?reln ?!obj
	    (:* (? type ONT::WILDTYPE-OBJ) ?!w)
	    :DRUM ?code)
	   -explicit-ref-mutation7>
	   100
	   (ONT::TERM ?!obj ?type
	    :name ?!w   
	    :mutation ONT::FALSE
	    :drum ?code
	    :rule -simple-ref-mutation7
	    )
	   )

	  ;; just the word "mutant"
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
	    ?reln ?!obj
	    (:* (? type ONT::MUTANT-OBJ) ?!w)
	    :DRUM ?code)
	   -explicit-ref-mutation8>
	   100
	   (ONT::TERM ?!obj ?type
	    :name ?!w   
	    :mutation ONT::TRUE
	    :drum ?code
	    :rule -simple-ref-mutation8
	    )
	   )


	  ;; chemicals, e.g., drugs (not conjunctions)
;	  (((? reln ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj (? t1 ont::CHEMICAL) :SEQUENCE - :DRUM ?code)
;	   
;	   -simple-ref-chemical>
;	   100
;	   (term ?!obj (? t1 ont::CHEMICAL)
;	    :drum ?code
;	    :rule -simple-ref-chemical
;	    ))

	  ;; logical sequences (e.g. conjunctions)
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
	    (? t1 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::MEDICAL-DISORDERS-AND-CONDITIONS) ;  ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ instead of ONT::PHYS-OBJECT.  Is this good?
	    :SEQUENCE ?!sequence  :operator ?!op )
	   -logicalOp-ref>
	   100
	   (ONT::TERM ?!obj ?t1
	    :logicalOp-sequence ?!sequence
	    :operator ?!op
	    :rule -logicalOp-ref
	    ))

; for paper1
; this one doesn't work any more
#|
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
	    (:* (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ) ?w) :ASSOC-WITHS (?!v1))
	   ;(ONT::F ?!v1 (:* ONT::ASSOC-WITH W::FOR) :VAL ?!v2 :MODS (?!m1))
	   (ONT::F ?!v1 (:* ONT::ASSOC-WITH W::FOR) :VAL ?!v2 :MOD ?!m1)
	   ((? reln2 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!v2 
	    (? t2 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ))
	   (ONT::F ?!m1 (:* ONT::PARENTHETICAL W::BUT-NOT)) 
	   -logicalOp-refA>
	   100
	   (term ?!obj ?t1   ; should be the union of t1 and t2
	    :logicalOp-sequence (?!obj ?!v2) ; note: ?!obj is re-used
	    :operator ONT::BUT-NOT
	    :rule -logicalOp-refA
	    ))

; for paper1
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
	    (:* (? t1 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ) ?!w))
	   (?!spec ?!ev ?!t :AFFECTED ?!obj :ASSOC-WITHS (?!v1))
	   ;(ONT::F ?!v1 (:* ONT::ASSOC-WITH W::FOR) :VAL ?!v2 :MODS (?!m1))
	   (ONT::F ?!v1 (:* ONT::ASSOC-WITH W::FOR) :VAL ?!v2 :MOD ?!m1)
	   ((? reln2 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!v2 
	    (? t2 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ))
	   (ONT::F ?!m1 (:* ONT::PARENTHETICAL W::BUT-NOT)) 
	   -logicalOp-refA>
	   100
	   (ONT::TERM ?!obj ?t1   ; should be the union of t1 and t2
	    :logicalOp-sequence (?!obj ?!v2) ; note: ?!obj is re-used
	    :operator ONT::BUT-NOT
	    :rule -logicalOp-refA
	    ))
|#


;	  ;; logical operations for chemicals (e.g. conjunctions)
;	  (((? reln ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj (? t1 ont::CHEMICAL) :SEQUENCE ?!sequence  :operator ?op :DRUM ?code)
;	   -logicalOp-ref-chemical>
;	   100
;	   (term ?!obj (? t1 ont::CHEMICAL)
;	    :logicalOp-sequence ?!sequence
;	    :operator ?op
;	    :DRUM ?code
;	    :rule -logicalOp-ref-chemical
;	    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; we need to return :drum explicitly in the robust rules because we want to replace the default :drum in ?type with the one in ?type1
;; [not any more] see the -explicit-ref-part rule (we don't want to return both the orignal protein term and the modified "Ras protein" term)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  ;; robust rule for explicit constructions, e.g., "ras protein" --- protein :ASSOC-WITH ras
	  ;; "pathway" uses -explicit-ref-pathway1/2 rules
	  ;; "complex" should uses another rule
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
	    (:* ?!type (? word W::PROTEIN W::GENE W::DRUG W::KINASE W::SITE W::POSITION W::MOLECULE W::DOMAIN W::PROMOTER W::MRNA W::TRANSCRIPT W::RECEPTOR-TYROSINE-KINASE W::PATHWAY W::COMPLEX W::PROTEINS W::GENES W::DRUGS W::KINASES W::SITES W::POSITIONS W::MOLECULES W::DOMAINS W::PROMOTERS W::MRNAS W::TRANSCRIPTS W::RECEPTOR-TYROSINE-KINASES W::PATHWAYS W::COMPLEXES))  ; mutation comes out as a verb in LIFE-TRANSFORMATION with AFFECTED protein (we get a TRANSFORM event for this)
	    :ASSOC-WITHS (?!name))
	   (;(? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name
	    ?reln1 ?!name
;	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART) ?!w) :DRUM ?code)
	    ; this is to avoid mapping to "kinase domain"
	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::MUTATION) (? !w W::PROTEIN W::GENE W::DRUG W::KINASE W::SITE W::POSITION W::MOLECULE W::DOMAIN W::PROMOTER W::MRNA W::TRANSCRIPT W::RECEPTOR-TYROSINE-KINASE W::PATHWAY W::COMPLEX W::PROTEINS W::GENES W::DRUGS W::KINASES W::SITES W::POSITIONS W::MOLECULES W::DOMAINS W::PROMOTERS W::MRNAS W::TRANSCRIPTS W::RECEPTOR-TYROSINE-KINASES W::PATHWAYS W::COMPLEXES)) :DRUM ?code)
	   -explicit-ref1>
	   100
	   (ONT::TERM ?!obj ?!type
	    :name ?!w
	    :BASE ?!name  
	    :drum ?code    
	    :rule -explicit-ref1
	    ; we don't zero out :ASSOC-WITH here because there might be other :ASSOC-WITH in there
	    )
	   #|
	   ; this extraction has to go first because otherwise for some reason -EXPLICIT-REF1X is fired (to extract the :BASE ?!name)
	   ; but if this extraction goes first, ?!obj is not substituted at the next level.  The second extraction here is just appended and it results in two ?!obj TERMs and duplicate event extractions
	   ; if we leave out this extraction, the system correctly tries to extract ?!name, but the rule with the largest cover is -EXPLICIT-REF1X
	   ; so we commented out -EXPLICIT-REF1X temporarily
	   (ONT::TERM ?!name ?type1   
	    :name ?!w
	    :drum ?code    
	    :rule -explicit-ref1-base
	    )
	   |#
	   )

	  ; Ras/Raf proteins
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
	    (:* ?!type (? word W::PROTEIN W::GENE W::DRUG W::KINASE W::SITE W::POSITION W::MOLECULE W::DOMAIN W::PROMOTER W::MRNA W::TRANSCRIPT W::RECEPTOR-TYROSINE-KINASE W::PATHWAY W::COMPLEX W::PROTEINS W::GENES W::DRUGS W::KINASES W::SITES W::POSITIONS W::MOLECULES W::DOMAINS W::PROMOTERS W::MRNAS W::TRANSCRIPTS W::RECEPTOR-TYROSINE-KINASES W::PATHWAYS W::COMPLEXES))  ; mutation comes out as a verb in LIFE-TRANSFORMATION with AFFECTED protein (we get a TRANSFORM event for this)
	    :ASSOC-WITHS (?!name))
	   (;(? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name
	    ?reln1 ?!name
;	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART) ?!w) :DRUM ?code)
	    ; this is to avoid mapping to "kinase domain"
	    (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::MUTATION) :SEQUENCE ?!seq :OPERATOR -)  ; only type, no w
	   -explicit-ref1-mseq>
	   100
	   (ONT::TERM ?!obj ?!type
	    ;:name ?!w
	    ;:BASE ?!name  
	    ;:drum ?code    
	    :M-SEQUENCE ?!seq
	    :rule -explicit-ref1-mseq
	    ; we don't zero out :ASSOC-WITH here because there might be other :ASSOC-WITH in there
	    )
	   #|
	   ; this extraction has to go first because otherwise for some reason -EXPLICIT-REF1X is fired (to extract the :BASE ?!name)
	   ; but if this extraction goes first, ?!obj is not substituted at the next level.  The second extraction here is just appended and it results in two ?!obj TERMs and duplicate event extractions
	   ; if we leave out this extraction, the system correctly tries to extract ?!name, but the rule with the largest cover is -EXPLICIT-REF1X
	   ; so we commented out -EXPLICIT-REF1X temporarily
	   (ONT::TERM ?!name ?type1   
	    :name ?!w
	    :drum ?code    
	    :rule -explicit-ref1-base
	    )
	   |#
	   )

	  ; Ras and Raf proteins
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
	    (:* ?!type (? word W::PROTEIN W::GENE W::DRUG W::KINASE W::SITE W::POSITION W::MOLECULE W::DOMAIN W::PROMOTER W::MRNA W::TRANSCRIPT W::RECEPTOR-TYROSINE-KINASE W::PATHWAY W::COMPLEX W::PROTEINS W::GENES W::DRUGS W::KINASES W::SITES W::POSITIONS W::MOLECULES W::DOMAINS W::PROMOTERS W::MRNAS W::TRANSCRIPTS W::RECEPTOR-TYROSINE-KINASES W::PATHWAYS W::COMPLEXES))  ; mutation comes out as a verb in LIFE-TRANSFORMATION with AFFECTED protein (we get a TRANSFORM event for this)
	    :ASSOC-WITHS (?!name))
	   (;(? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name
	    ?reln1 ?!name
;	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART) ?!w) :DRUM ?code)
	    ; this is to avoid mapping to "kinase domain"
	    (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::MUTATION) :SEQUENCE ?!seq :OPERATOR ?!op)
	   -explicit-ref1-lseq>
	   100
	   (ONT::TERM ?!obj ?!type
	    ;:name ?!w
	    ;:BASE ?!name  
	    ;:drum ?code    
	    :LOGICALOP-SEQUENCE ?!seq
	    :OPERATOR ?!op
	    :rule -explicit-ref1-lseq
	    ; we don't zero out :ASSOC-WITH here because there might be other :ASSOC-WITH in there
	    )
	   #|
	   ; this extraction has to go first because otherwise for some reason -EXPLICIT-REF1X is fired (to extract the :BASE ?!name)
	   ; but if this extraction goes first, ?!obj is not substituted at the next level.  The second extraction here is just appended and it results in two ?!obj TERMs and duplicate event extractions
	   ; if we leave out this extraction, the system correctly tries to extract ?!name, but the rule with the largest cover is -EXPLICIT-REF1X
	   ; so we commented out -EXPLICIT-REF1X temporarily
	   (ONT::TERM ?!name ?type1   
	    :name ?!w
	    :drum ?code    
	    :rule -explicit-ref1-base
	    )
	   |#
	   )

	  
	  ;; robust rule for explicit constructions, e.g., "the protein Erk" --- Erk :ASSOC-WITH protein
	  ;; "the pathway Ras/Raf parses the same as the Ras/Raf pathway", so no need for another rule?
	  ;; "the complex Raf-Raf" doesn't parse correctly
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
;	    (:* (? type ONT::CHEMICAL ONT::MOLECULAR-PART) ?!w) :DRUM ?code
	    ; this is to avoid mapping to "kinase domain"
	    (:* (? type ONT::CHEMICAL ONT::MOLECULAR-PART ONT::MUTATION) (? !w W::PROTEIN W::GENE W::DRUG W::KINASE W::SITE W::POSITION W::MOLECULE W::DOMAIN W::PROMOTER W::MRNA W::TRANSCRIPT W::RECEPTOR-TYROSINE-KINASE W::PATHWAY W::COMPLEX W::PROTEINS W::GENES W::DRUGS W::KINASES W::SITES W::POSITIONS W::MOLECULES W::DOMAINS W::PROMOTERS W::MRNAS W::TRANSCRIPTS W::RECEPTOR-TYROSINE-KINASES W::PATHWAYS W::COMPLEXES)) :DRUM ?code
	    :ASSOC-WITHS (?!name))	    
	   (;(? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name
	    ?reln1 ?!name
	    (:* ?!type1 (? word W::PROTEIN W::GENE W::DRUG W::KINASE W::SITE W::POSITION W::MOLECULE W::DOMAIN W::PROMOTER W::MRNA W::TRANSCRIPT W::RECEPTOR-TYROSINE-KINASE W::PATHWAY W::COMPLEX W::PROTEINS W::GENES W::DRUGS W::KINASES W::SITES W::POSITIONS W::MOLECULES W::DOMAINS W::PROMOTERS W::MRNAS W::TRANSCRIPTS W::RECEPTOR-TYROSINE-KINASES W::PATHWAYS W::COMPLEXES))  ; mutation comes out as a verb in LIFE-TRANSFORMATION with AFFECTED protein (we get a TRANSFORM event for this)
	    )
	   -explicit-ref1-rev>
	   100
	   (ONT::TERM ?!obj ?!type1
	    :name ?!w
	    :BASE *1
	    :drum ?code    
	    :rule -explicit-ref1-rev
	    )
	   (ONT::TERM *1 ?type   ; this rule has to go first because otherwise for some reason -EXPLICIT-REF1X is fired (to extract the :ASSOC ?!name  ??)
	    :name ?!w
	    :drum ?code    
	    :rule -explicit-ref1-rev-base
	    )
	   )

	  ;; robust rule for explicit constructions, e.g., "the protein Erk"  protein :APPOS-EQ Erk
	  ;; "the pathway Ras/Raf parses the same as the Ras/Raf pathway", so no need for another rule?
	  ;; "the complex Raf-Raf" doesn't parse correctly
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
;	    (:* (? type ONT::CHEMICAL ONT::MOLECULAR-PART) ?!w) :DRUM ?code
	    ; this is to avoid mapping to "kinase domain"
	    (:* (? type ONT::CHEMICAL ONT::MOLECULAR-PART ONT::MUTATION) (? !w W::PROTEIN W::GENE W::DRUG W::KINASE W::SITE W::POSITION W::MOLECULE W::DOMAIN W::PROMOTER W::MRNA W::TRANSCRIPT W::RECEPTOR-TYROSINE-KINASE W::PATHWAY W::COMPLEX W::PROTEINS W::GENES W::DRUGS W::KINASES W::SITES W::POSITIONS W::MOLECULES W::DOMAINS W::PROMOTERS W::MRNAS W::TRANSCRIPTS W::RECEPTOR-TYROSINE-KINASES W::PATHWAYS W::COMPLEXES)) :DRUM ?code
	    )	    
	   (;(? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name
	    ?reln1 ?!name
	    (:* ?!type1 (? word W::PROTEIN W::GENE W::DRUG W::KINASE W::SITE W::POSITION W::MOLECULE W::DOMAIN W::PROMOTER W::MRNA W::TRANSCRIPT W::RECEPTOR-TYROSINE-KINASE W::PATHWAY W::COMPLEX W::PROTEINS W::GENES W::DRUGS W::KINASES W::SITES W::POSITIONS W::MOLECULES W::DOMAINS W::PROMOTERS W::MRNAS W::TRANSCRIPTS W::RECEPTOR-TYROSINE-KINASES W::PATHWAYS W::COMPLEXES))  ; mutation comes out as a verb in LIFE-TRANSFORMATION with AFFECTED protein (we get a TRANSFORM event for this)
	    :APPOS-EQ ?!obj
	    )
	   -explicit-ref1-appos>
	   100
	   (ONT::TERM ?!name ?!type1
	    :name ?!w
	    :BASE ?!obj
	    :drum ?code    
	    :rule -explicit-ref1-appos
	    )
	   )

	  ;; robust rule for explicit constructions, e.g., "the proteins Ras/Raf"  protein :APPOS-EQ a sequence
	  ;; "the pathway Ras/Raf parses the same as the Ras/Raf pathway", so no need for another rule?
	  ;; "the complex Raf-Raf" doesn't parse correctly
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
;	    (:* (? type ONT::CHEMICAL ONT::MOLECULAR-PART) ?!w) :DRUM ?code
	    ; this is to avoid mapping to "kinase domain"
	    (? type ONT::CHEMICAL ONT::MOLECULAR-PART ONT::MUTATION) :SEQUENCE ?!seq :OPERATOR -
	    )	    
	   (;(? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name
	    ?reln1 ?!name
	    (:* ?!type1 (? word W::PROTEIN W::GENE W::DRUG W::KINASE W::SITE W::POSITION W::MOLECULE W::DOMAIN W::PROMOTER W::MRNA W::TRANSCRIPT W::RECEPTOR-TYROSINE-KINASE W::PATHWAY W::COMPLEX W::PROTEINS W::GENES W::DRUGS W::KINASES W::SITES W::POSITIONS W::MOLECULES W::DOMAINS W::PROMOTERS W::MRNAS W::TRANSCRIPTS W::RECEPTOR-TYROSINE-KINASES W::PATHWAYS W::COMPLEXES))  ; mutation comes out as a verb in LIFE-TRANSFORMATION with AFFECTED protein (we get a TRANSFORM event for this)
	    :APPOS-EQ ?!obj
	    )
	   -explicit-ref1-appos-mseq>
	   100
	   (ONT::TERM ?!name ?!type1
	    :M-SEQUENCE ?!seq
	    ;:name ?!w
	    ;:BASE ?!obj
	    ;:drum ?code    
	    :rule -explicit-ref1-appos-mseq
	    )
	   )

	  ;; robust rule for explicit constructions, e.g., "the proteins Ras and Raf"  protein :APPOS-EQ a sequence
	  ;; "the pathway Ras/Raf parses the same as the Ras/Raf pathway", so no need for another rule?
	  ;; "the complex Raf-Raf" doesn't parse correctly
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
;	    (:* (? type ONT::CHEMICAL ONT::MOLECULAR-PART) ?!w) :DRUM ?code
	    ; this is to avoid mapping to "kinase domain"
	    (? type ONT::CHEMICAL ONT::MOLECULAR-PART ONT::MUTATION) :SEQUENCE ?!seq :OPERATOR ?!op
	    )	    
	   (;(? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name
	    ?reln1 ?!name
	    (:* ?!type1 (? word W::PROTEIN W::GENE W::DRUG W::KINASE W::SITE W::POSITION W::MOLECULE W::DOMAIN W::PROMOTER W::MRNA W::TRANSCRIPT W::RECEPTOR-TYROSINE-KINASE W::PATHWAY W::COMPLEX W::PROTEINS W::GENES W::DRUGS W::KINASES W::SITES W::POSITIONS W::MOLECULES W::DOMAINS W::PROMOTERS W::MRNAS W::TRANSCRIPTS W::RECEPTOR-TYROSINE-KINASES W::PATHWAYS W::COMPLEXES))  ; mutation comes out as a verb in LIFE-TRANSFORMATION with AFFECTED protein (we get a TRANSFORM event for this)
	    :APPOS-EQ ?!obj
	    )
	   -explicit-ref1-appos-lseq>
	   100
	   (ONT::TERM ?!name ?!type1
	    :LOGICALOP-SEQUENCE ?!seq
	    :OPERATOR ?!op
	    ;:name ?!w
	    ;:BASE ?!obj
	    ;:drum ?code    
	    :rule -explicit-ref1-appos-lseq
	    )
	   )
	  
	  ;; robust rule for "SOS1's RasGEF domain"
	  ;; "domain" :ASSOC-WITH RasGEF :ASSOC-POSS SOS1  (Note: the :ASSOC-WITH is the domain but could be tagged as CELL-LINE (e.g., SH2) or PROTEIN (e.g., RASGEF, RBD))
	  ;; added MUTATION to ?type1 (SOS1's G12V site)
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
	    (:* ?!type (? word W::SITE W::POSITION W::DOMAIN))  ; mutation comes out as a verb in LIFE-TRANSFORMATION with AFFECTED protein (we get a TRANSFORM event for this)
	    :ASSOC-WITHS (?!name) :ASSOC-POSS ?!name2)
	   (;(? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name
	    ?reln1 ?!name
	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::MUTATION) ?!w) :DRUM ?code)
	   (;(? reln2 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name2
	    ?reln2 ?!name2
	    (:* (? type2 ONT::CHEMICAL ONT::MOLECULAR-PART) ?!w2) :DRUM ?code2)
	   -explicit-ref1b>
	   100
	   (ONT::TERM ?!obj ?type2
	    :name ?!w2
	    :site ?!name
	    :drum ?code2  
	    :rule -explicit-ref1b
	    )
#|
	   (?reln1 ?!name ONT::MOLECULAR-DOMAIN 
	    :name ?!w
	    :drum ?code
	    :rule -explicit-ref1b
	    )
|#
	   )


	  ;; robust rule for "The RasGEF domain of SOS1"
	  ;; "domain" :ASSOC-WITH RasGEF :MOD of ... :VAL SOS1  (Note: the :ASSOC-WITH is the domain but could be tagged as CELL-LINE (e.g., SH2) or PROTEIN (e.g., RASGEF, RBD))
	  ;; added MUTATION to ?type1 (The G12V site)
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
	    (:* ?!type (? word W::SITE W::POSITION W::DOMAIN))  ; mutation comes out as a verb in LIFE-TRANSFORMATION with AFFECTED protein (we get a TRANSFORM event for this)
	    :ASSOC-WITHS (?!name) :MODS (?!mod))
	   (;(? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name
	    ?reln1 ?!name
	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::MUTATION) ?!w) )
	   (ONT::F ?!mod (:* ONT::ASSOC-WITH W::OF) :GROUND ?!name2)
	   (;(? reln2 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name2
	    ?reln2 ?!name2
	    (:* (? type2 ONT::CHEMICAL ONT::MOLECULAR-PART) ?!w2) :DRUM ?code)
	   -explicit-ref1c>
	   100
	   (ONT::TERM ?!obj ?type2
	    :name ?!w2
	    :site ?!name
	    :drum ?code    
	    :rule -explicit-ref1c
	    )
	   )

	  ;; robust rule for "The RasGEF domain on SOS1"
	  ;; "domain" :ASSOC-WITH RasGEF :MOD of ... :VAL SOS1  (Note: the :ASSOC-WITH is the domain but could be tagged as CELL-LINE (e.g., SH2) or PROTEIN (e.g., RASGEF, RBD))
	  ;; added MUTATION to ?type1 (The G12V site)
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
	    (:* ?!type (? word W::SITE W::POSITION W::DOMAIN))  ; mutation comes out as a verb in LIFE-TRANSFORMATION with AFFECTED protein (we get a TRANSFORM event for this)
	    :ASSOC-WITHS (?!name) :LOC ?!loc)
	   (;(? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name
	    ?reln1 ?!name
	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::MUTATION) ?!w) )
	   (ONT::F ?!loc ONT::ON :GROUND ?!name2)
	   (;(? reln2 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name2
	    ?reln2 ?!name2
	    (:* (? type2 ONT::CHEMICAL ONT::MOLECULAR-PART) ?!w2) :DRUM ?code)
	   -explicit-ref1d>
	   100
	   (ONT::TERM ?!obj ?type2
	    :name ?!w2
	    :site ?!name
	    :drum ?code    
	    :rule -explicit-ref1d
	    )
	   )

	  ;; robust rule for explicit constructions, returning both words, e.g., "ASPP2 fragment"
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
	    (:* ?!type (? word W::FRAGMENT))
	    :ASSOC-WITHS (?!name))
	   (;(? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name
	    ?reln1 ?!name
	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART) ?!w) :DRUM ?code)
	   -explicit-ref2>
	   100
	   (ONT::TERM ?!obj ?type1
	    :name (?!w ?word)   ;; e.g., "ASPP2 fragment"
	    :drum ?code
	    :rule -explicit-ref2
	    )
	   )

	  ;; robust rule for explicit constructions, e.g., "EGFR wildtype"
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
	    (:* ?!type (? word W::WILDTYPE W::WILD-TYPE W::WILD-PUNC-MINUS-TYPE W::WT))
	    :ASSOC-WITHS (?!name))
	   (;(? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name
	    ?reln1 ?!name
	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART) ?!w) :DRUM ?code)
	   -explicit-ref3>
	   100
	   (ONT::TERM ?!obj ?type1
	    :name ?!w   
	    :mutation ONT::FALSE
	    :drum ?code
	    :rule -explicit-ref3
	    )
	   )

	  ;; robust rule for explicit constructions, e.g., "EGFR mutant"
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
	    (:* ?!type (? word W::MUTANT))
	    :ASSOC-WITHS (?!name))
	   (;(? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name
	    ?reln1 ?!name
	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART) ?!w) :DRUM ?code)
	   -explicit-ref4>
	   100
	   (ONT::TERM ?!obj ?type1
	    :name ?!w
	    :mutation ONT::TRUE
	    :drum ?code
	    :rule -explicit-ref4
	    )
	   )

	  ;; robust rule for explicit constructions, with both a mutation and the word "mutant" e.g., "D770_N771insNPG EGFR mutant"
	  ;; we don't need a similar rule for wildtype
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
	    (:* ?!type (? word W::MUTANT))
	    :ASSOC-WITHS (?!name1 ?!name2))
	   (;(? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name1
	    ?reln1 ?!name1
	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART) ?!w1) :DRUM ?code)
            ((? spec ONT::THE) ?!name2 (:* (? tmp ONT::MUTATION) ?!w2))
	   -explicit-ref5>
	   100
	   (ONT::TERM ?!obj ?type1
	    :name ?!w1
	    :mutation ?!name2
	    :drum ?code
	    :rule -explicit-ref5
	    )
	   )

	  ;; basic terms (not conjunctions) with mutation :ASSOC-WITH protein, e.g., Ras G12V
	  ;; Note: this is circular
	  ;; Note: this might not fire anymore "Ras G12V" is now Ras :ASSOC-WITH G12V, not the other way
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
	    (:* (? t1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY) ?!w) :SEQUENCE - :DRUM ?code)
            ((? spec ONT::THE ONT::BARE) ?!m (:* (? tmp ONT::MUTATION) ?!m2) :ASSOC-WITHS (?!obj))
	   -explicit-ref6>
	   100
;	   (?spec ?!m ?t1   ; note: ?!m here, not ?!obj (because others will be referring to ?!m as the argument)
	   (ONT::TERM ?!m ?t1   ; note: ?!m here, not ?!obj (because others will be referring to ?!m as the argument)
	    :name ?!w
	    :mutation ?!m  ; note: circular ?!m
	    :drum ?code
	    :rule -explicit-ref6
	    ))

	  ;; robust rule for explicit constructions, e.g., "the Rasaeo protein" where "Rasaeo" is unknown
	  ;; Note: I think the unknown word must be capitalized and be preceded by "the"
	  ;; works somewhat for, e.g., Ib-V-IX complex
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::WH-TERM) ?!obj
	    ?reln ?!obj
	    (:* ?!type (? word W::PROTEIN W::GENE W::DRUG W::KINASE W::SITE W::POSITION W::MOLECULE W::DOMAIN W::PROMOTER W::MRNA W::TRANSCRIPT W::RECEPTOR-TYROSINE-KINASE W::PATHWAY W::COMPLEX W::PROTEINS W::GENES W::DRUGS W::KINASES W::SITES W::POSITIONS W::MOLECULES W::DOMAINS W::PROMOTERS W::MRNAS W::TRANSCRIPTS W::RECEPTOR-TYROSINE-KINASES W::PATHWAYS W::COMPLEXES))
	    :ASSOC-WITHS (?!name))
;	   (?reln1 ?!name (:* (? type1 ONT::REFERENTIAL-SEM) ?w) :NAME-OF ?!name1 :DRUM ?code)
	   (?reln1 ?!name (:* (? type1 ONT::REFERENTIAL-SEM) ?!w)
		   ;:NAME-OF ?!name1
		   :DRUM -)   ;; make sure it is not a known term; no :DRUM info
	   -explicit-ref-robust>
	   90 ; lower priority than -explicit-ref-pathway2
	   (ONT::TERM ?!obj ?!type
	    :name ?!w
;	    :drum ?code    ; note: with new scheme the :drum slot for ?!obj will be retained if there is one
	    :rule -explicit-ref-robust
	    )
	   )

	  ;; robust rule for pronouns, e.g., it, itself, we
	  (((? reln ONT::PRO ONT::PRO-SET) ?!obj (:* ?!type ?!w) :PROFORM ?!pro
	    )
	   -robustPro>
	   100
	   (ONT::TERM ?!obj ?!type
	    :pro ?!pro
;	    :drum ?code
	    :rule -robustPro
	    )
	   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; pathways/complexes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 2016/01/29: reinstating a modified version of this rule and -explicit-ref-seq0-x2 (for two :ASSOC-WITHs)
;;;	  
;;; removing this rule so as not to return multiple complexes with the same ID (e.g., the PI3KC2-beta RBD-Ras complex)
;;; two firing of this rule or one firing of this rule with one firing of -explicit-ref-seq0a
;;; but now we won't get "MAPK pathway"
	  ;; robust rule for explicit constructions for one element pathways/complexes, e.g., "MAPK pathway", "XYZ complex"
	  ;; higher priority than rule EXPLICIT-REF-ROBUST
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
	    (:* ?type (? word W::PATHWAY W::COMPLEX))
	    :ASSOC-WITHS (?!name1))
	   (;(? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name1
	    ?reln1 ?!name1
;	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY) ?w) :DRUM ?code)
;	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULE ONT::GENE) ?!w) :DRUM ?code)  ; to avoid "Ras S338" (S338 :ASSOC-WITH Ras) being extracted as an m-sequence (Ras S338)
	    ; replaced MOLECULE and GENE by MOLECULAR-PART; the "Ras S338" comment is probably not applicable any more
	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART) ?!w) :DRUM ?code)  ; to avoid "Ras S338" (S338 :ASSOC-WITH Ras) being extracted as an m-sequence (Ras S338)
	   -explicit-ref-seq0>
	   100
	   (ONT::TERM ?!obj ?type
;	    :name ?w
	    :m-sequence (?!name1)
;	    :drum ?code
	    :drum -   ; zero out :drum explicitly
	    :rule -explicit-ref-seq0
	    )
	   (ONT::TERM ?!name1 ?type1    ; this is otherwise not extracted because it is subsumed
	    :name ?!w
	    :drum ?code
	    :rule -explicit-ref-seq0-term
	    )
	   )

	  ;; the PI3KC2-beta RBD-Ras complex
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
	    (:* ?type (? word W::PATHWAY W::COMPLEX))
	    :ASSOC-WITHS (?!name1 ?!name2))
	   (;(? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name1
	    ?reln1 ?!name1
;	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY) ?w) :DRUM ?code)
;	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULE ONT::GENE) ?!w) :DRUM ?code)  ; to avoid "Ras S338" (S338 :ASSOC-WITH Ras) being extracted as an m-sequence (Ras S338)
	    ; replaced MOLECULE and GENE by MOLECULAR-PART; the "Ras S338" comment is probably not applicable any more
	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART) ?!w) :DRUM ?code)  ; to avoid "Ras S338" (S338 :ASSOC-WITH Ras) being extracted as an m-sequence (Ras S338)
	   (;(? reln2 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name2
	    ?reln2 ?!name2
;	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY) ?w) :DRUM ?code)
;	    (:* (? type2 ONT::CHEMICAL ONT::MOLECULE ONT::GENE) ?!w2) :DRUM ?code2)  ; to avoid "Ras S338" (S338 :ASSOC-WITH Ras) being extracted as an m-sequence (Ras S338)
	    ; replaced MOLECULE and GENE by MOLECULAR-PART; the "Ras S338" comment is probably not applicable any more
	    (:* (? type2 ONT::CHEMICAL ONT::MOLECULAR-PART) ?!w2) :DRUM ?code2)  ; to avoid "Ras S338" (S338 :ASSOC-WITH Ras) being extracted as an m-sequence (Ras S338)
	   -explicit-ref-seq0-x2>
	   100
	   (ONT::TERM ?!obj ?type
;	    :name ?w
	    :m-sequence (?!name1 ?!name2)
;	    :drum ?code
	    :drum -   ; zero out :drum explicitly
	    :rule -explicit-ref-seq0-x2
	    )
	   (ONT::TERM ?!name1 ?type1    ; this is otherwise not extracted because it is subsumed
	    :name ?!w
	    :drum ?code
	    :rule -explicit-ref-seq0-x2-term
	    )
	   (ONT::TERM ?!name2 ?type2    ; this is otherwise not extracted because it is subsumed
	    :name ?!w2
	    :drum ?code2
	    :rule -explicit-ref-seq0-x2-term2
	    )
	   )

	  ;; robust rule for explicit constructions for pathways/complexes, e.g., "MEK/ERK pathway", "Ras/Raf complex"
	  ;; new construction; might have replaced -explicit-ref-seq0
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
	    (:* (? type ONT::SIGNALING-PATHWAY ONT::MACROMOLECULAR-COMPLEX) ?!w)
	    :ASSOC-WITHS (?!name))
	   (;(? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name
	    ?reln1 ?!name
;	    (? type1 ONT::SEQUENCE) :SEQUENCE ?!sequence)
	    (? type1 ONT::MUTATION ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART)
	    :SEQUENCE ?!sequence
 	    :SEQUENCE (?!s1 ?s2 ?s3)  ; this is so to match a superset of -explicit-ref-seq0b
	    :operator -)  ; ":operator -" is here to exclude logical sequences (e.g., Ras and Raf)
	   (;(? reln1s ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!s1
	    ?reln1s ?!s1
	    (:* (? type1s ONT::MUTATION ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART) ?!w1s)
	    :SEQUENCE - :drum ?code1s)
	   -explicit-ref-seq0a>
	   100
	   (ONT::TERM ?!obj ?type
	    :m-sequence ?!sequence
;	    :operator ONT::AND
;	    :drum ?code  ; there is no :drum information for :SEQUENCE (so this will be empty)
	    :drum -   ; zero out :drum explicitly
	    :rule -explicit-ref-seq0a
	    )
	   (ONT::TERM ?!s1 ?type1s  ; emit !?s1 also which otherwise would not be extracted by -simple-ref
	    :name ?!w1s
	    :drum ?code1s
	    :rule -explicit-ref-seq0a-term
	    )	   	   
	   )

	  ;; robust rule for explicit constructions for pathways/complexes, e.g., "a complex of Ras/Raf"
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
	    (:* (? type ONT::SIGNALING-PATHWAY ONT::MACROMOLECULAR-COMPLEX) ?!w)
	    :ASSOC-WITHS (?!name))
	   (ONT::F ?!name ONT::ASSOC-WITH :GROUND ?!nVal)
	   (;(? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!nVal
	    ?reln1 ?!nVal
;	    (? type1 ONT::SEQUENCE) :SEQUENCE ?!sequence)
	    (? type1 ONT::MUTATION ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART)
	    :SEQUENCE ?!sequence
 	    :SEQUENCE (?!s1 ?s2 ?s3)  ; this is so to match a superset of -explicit-ref-seq0b
	    :operator -)
	   (;(? reln1s ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!s1
	    ?reln1s ?!s1
	    (:* (? type1s ONT::MUTATION ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART) ?!w1s)
	    :SEQUENCE - :drum ?code1s)
	   -explicit-ref-seq0a2>
	   100
	   (ONT::TERM ?!obj ?type
	    :m-sequence ?!sequence
;	    :operator ONT::AND
;	    :drum ?code  ; there is no :drum information for :SEQUENCE (so this will be empty)
	    :drum -   ; zero out :drum explicitly
	    :rule -explicit-ref-seq0a2
	    )
	   (ONT::TERM ?!s1 ?type1s  ; emit !?s1 also which otherwise would not be extracted by -simple-ref
	    :name ?!w1s
	    :drum ?code1s
	    :rule -explicit-ref-seq0a2-term
	    )	   	   
	   )

	  ;; robust rule for explicit constructions for pathways/complexes (without the word "pathway" or "complex"), e.g., "Ras/Raf/Mek"
	  ;; We do not make the combined type PATHWAY or MACROMOLECULAR-COMPLEX because it could be either
	  ; works only up to three elements because of :SEQUENCE (?!s1 ?s2 ?s3)
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
;	    ONT::SEQUENCE :ELEMENT-TYPE ?!type :SEQUENCE ?!sequence)
	    (? type ONT::MUTATION ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART)
	    :SEQUENCE ?!sequence
	    :SEQUENCE (?!s1 ?s2 ?s3)  ; this is to stop "the first protein" from matching ("first" is :sequence (nth 1))
	    :operator -)
	   (;(? reln1s ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!s1
	    ?reln1s ?!s1
	    (:* (? type1s ONT::MUTATION ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART) ?!w1s)
	    :SEQUENCE - :drum ?code1s)
	   -explicit-ref-seq0b>
	   100
	   (ONT::TERM ?!obj ?type
	    :m-sequence ?!sequence
;	    :operator ONT::AND
;	    :drum ?code  ; there is no :drum information for :SEQUENCE (so this will be empty)
	    :drum -   ; zero out :drum explicitly
	    :rule -explicit-ref-seq0b
	    )
	   (ONT::TERM ?!s1 ?type1s  ; emit !?s1 also which otherwise would not be extracted by -simple-ref
	    :name ?!w1s
	    :drum ?code1s
	    :rule -explicit-ref-seq0b-term
	    )	   
	   )
	  

	  ;; robust rule for explicit constructions for molecules with mutations that look like pathways/complexes, e.g., BRAF-V600E; (but not Ras-V12)
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
;	    ONT::SEQUENCE :ELEMENT-TYPE ?!type :SEQUENCE (?!s1 ?!s2))
	    (? type ONT::REFERENTIAL-SEM) :SEQUENCE (?!s1 ?!s2) :operator -)  ; gene + mutation = REFERENTIAL-SEM
	   ((? reln1 ONT::THE) ?!s1 
;	    (:* (? t1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY) ?!w) :SEQUENCE - :DRUM ?code)
	    (:* (? t1 ONT::GENE-PROTEIN) ?!w) :SEQUENCE - :DRUM ?code)
;            (ONT::THE ?!s2 (:* (? tmp ONT::MUTATION ONT::MOLECULAR-SITE) ?!m2))
            (ONT::THE ?!s2 (:* (? tmp ONT::MUTATION) ?!m2))
	   -explicit-ref-seq0c>
	   100
	   (ONT::TERM ?!obj ?t1
	    :name ?!w
	    :mutation ?!s2
;	    :m-sequence ?!sequence
	    :drum ?code  ; drum code from t1
	    :rule -explicit-ref-seq0c
	    )
	   )

	  ;; same as above -explicit-ref-seq0c, but with reversed sequence elements
	  ;; robust rule for explicit constructions for molecules with mutations that look like pathways/complexes, e.g., V600E-BRAF; (but not V12-Ras)
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
;	    ONT::SEQUENCE :ELEMENT-TYPE ?!type :SEQUENCE (?!s2 ?!s1))
	    (? type ONT::REFERENTIAL-SEM) :SEQUENCE (?!s2 ?!s1) :operator -)  ; gene + mutation = REFERENTIAL-SEM
	   ((? reln1 ONT::THE) ?!s1 
;	    (:* (? t1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY) ?!w) :SEQUENCE - :DRUM ?code)
	    (:* (? t1 ONT::GENE-PROTEIN) ?!w) :SEQUENCE - :DRUM ?code)
;            (ONT::THE ?!s2 (:* (? tmp ONT::MUTATION ONT::MOLECULAR-SITE) ?!m2))
            (ONT::THE ?!s2 (:* (? tmp ONT::MUTATION) ?!m2))
	   -explicit-ref-seq0d>
	   100
	   (ONT::TERM ?!obj ?t1
	    :name ?!w
	    :mutation ?!s2
;	    :m-sequence ?!sequence
	    :drum ?code  ; drum code from t1
	    :rule -explicit-ref-seq0d
	    )
	   )

	  ;; robust rule for explicit constructions for molecules with sits that look like pathways/complexes, e.g., Ras-V12 (or Ras/V12 if this happens)
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
;	    ONT::SEQUENCE :ELEMENT-TYPE ?!type :SEQUENCE (?!s1 ?!s2))
	    (? type ONT::REFERENTIAL-SEM) :SEQUENCE (?!s1 ?!s2) :operator -)  ; gene + mutation = REFERENTIAL-SEM
	   ((? reln1 ONT::THE) ?!s1 
;	    (:* (? t1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY) ?!w) :SEQUENCE - :DRUM ?code)
	    (:* (? t1 ONT::GENE-PROTEIN) ?!w) :SEQUENCE - :DRUM ?code)
;            (ONT::THE ?!s2 (:* (? tmp ONT::MUTATION ONT::MOLECULAR-SITE) ?!m2))
            (ONT::THE ?!s2 (:* (? tmp ONT::MOLECULAR-SITE) ?!m2))
	   -explicit-ref-seq0e>
	   100
	   (ONT::TERM ?!obj ?t1
	    :name ?!w
	    :site ?!s2
;	    :m-sequence ?!sequence
	    :drum ?code  ; drum code from t1
	    :rule -explicit-ref-seq0e
	    )
	   )

	  ;; same as above -explicit-ref-seq0e, but with reversed sequence elements
	  ;; robust rule for explicit constructions for molecules with mutations that look like pathways/complexes, e.g., V12-Ras (or V12/Ras if this happens)
	  ((;(? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ?reln ?!obj
;	    ONT::SEQUENCE :ELEMENT-TYPE ?!type :SEQUENCE (?!s2 ?!s1))
	    (? type ONT::REFERENTIAL-SEM) :SEQUENCE (?!s2 ?!s1) :operator -)  ; gene + mutation = REFERENTIAL-SEM
	   ((? reln1 ONT::THE) ?!s1 
;	    (:* (? t1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY) ?!w) :SEQUENCE - :DRUM ?code)
	    (:* (? t1 ONT::GENE-PROTEIN) ?!w) :SEQUENCE - :DRUM ?code)
;            (ONT::THE ?!s2 (:* (? tmp ONT::MUTATION ONT::MOLECULAR-SITE) ?!m2))
            (ONT::THE ?!s2 (:* (? tmp ONT::MOLECULAR-SITE) ?!m2))
	   -explicit-ref-seq0f>
	   100
	   (ONT::TERM ?!obj ?t1
	    :name ?!w
	    :site ?!s2
;	    :m-sequence ?!sequence
	    :drum ?code  ; drum code from t1
	    :rule -explicit-ref-seq0f
	    )
	   )
	  
	  
#|
	  ;; robust rule for explicit constructions for pathways/complexes, e.g., "the Ras-Raf complex/pathway" 
	  ;; (can only deal with two components because of the limitation of :ASSOC-WITHS)
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
	    (:* ?type (? word W::PATHWAY W::COMPLEX))
	    :ASSOC-WITHS (?!name1 ?!name2))
	   ((? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name1 
;	    (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY) :SEQUENCE -)
	    (? type1 ONT::CHEMICAL ONT::MOLECULE ONT::GENE) :SEQUENCE -)
	   ((? reln2 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name2 
;	    (? type2 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY) :SEQUENCE -)
	    (? type2 ONT::CHEMICAL ONT::MOLECULE ONT::GENE) :SEQUENCE -)
	   -explicit-ref-seq2>
	   100
	   (term ?!obj ?type
	    :m-sequence (?!name1 ?!name2)
;	    :operator ONT::AND
;	    :drum ?code
	    :rule -explicit-ref-seq2
	    )
	   )

	  ;; robust rule for explicit constructions for pathways/complexes, e.g., "Ras-Raf-MEK" (three components)
	  ;; (up to three because of limitation of :ASSOC-WITHS)
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
;	    (:* (? type ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY) ?word)
	    (:* (? type ONT::CHEMICAL ONT::MOLECULE ONT::GENE) ?word)
	    :ASSOC-WITHS (?!name1 ?!name2) :SEQUENCE -)
	   ((? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name1 
;	    (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY) :SEQUENCE -)
	    (? type1 ONT::CHEMICAL ONT::MOLECULE ONT::GENE) :SEQUENCE -)
	   ((? reln2 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name2 
;	    (? type2 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY) :SEQUENCE -)
	    (? type2 ONT::CHEMICAL ONT::MOLECULE ONT::GENE) :SEQUENCE -)
	   -explicit-ref-seq3>
	   100
	   (term ?!obj ?type  ; takes the type ?type of the first entity (ideally should be the type of the unification of the sequence)
			      ; takes the variable ID ?!obj of the first entity too (ideally should keep ?!obj for the sequence and make a new id for the member ?!obj)
	    :m-sequence (?!obj ?!name1 ?!name2)
;	    :operator ONT::AND
;	    :drum -    ; we don't want to pass on the drum-info from ?!obj
	    :rule -explicit-ref-seq3
	    )
	   )

	  ;; robust rule for explicit constructions for pathways/complexes, e.g., "Ras-Raf" (two components)
	  ;; (two here, but only up to three because of limitation of :ASSOC-WITHS)
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
;	    (:* (? type ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY) ?word)
	    (:* (? type ONT::CHEMICAL ONT::MOLECULE ONT::GENE) ?word)
	    :ASSOC-WITHS (?!name1) :SEQUENCE -)
	   ((? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name1 
;	    (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY) :SEQUENCE -)
	    (? type1 ONT::CHEMICAL ONT::MOLECULE ONT::GENE) :SEQUENCE -)
	   -explicit-ref-seq4>
	   100
	   (term ?!obj ?type  ; take the type of the first entity (ideally should be the type of the unification of the sequence)
	    :m-sequence (?!obj ?!name1)
;	    :operator ONT::AND
;	    :drum -    ; we don't want to pass on the drum-info from ?!obj
	    :rule -explicit-ref-seq4
	    )
	   )
|#


#||||
	  ;; activity, e.g., "activity of Ras"
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
	    (:* ONT::ACTING W::ACTIVITY)
	    :AGENT ?!name)
	   ((? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name 
	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY) ?w) :DRUM ?code)
	   -activity1>
	   100
	   (term ?!obj ONT::ACTIVITY
	    :name ?w
	    :drum ?code
	    :rule -activity1
	    )
	   )

	  ;; activity, e.g., "Ras activity"
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
	    (:* ONT::ACTING W::ACTIVITY)
	    :ASSOC-WITH ?!name)
	   ((? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name 
	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY ) ?w) :DRUM ?code)
	   -activity2>
	   100
	   (term ?!obj ONT::ACTIVITY
	    :name ?w
	    :drum ?code
	    :rule -activity2
	    )
	   )


	  ;; activity, e.g., "phosphorylation activity"
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
	    (:* ONT::ACTING W::ACTIVITY)
	    :ASSOC-WITH ?!name)
	   ((? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name 
	    (:* (? type1 ONT::EVENT-OF-CHANGE ) ?w) :DRUM ?code)
	   -activity3>
	   100
	   (term ?!obj ONT::ACTIVITY
	    :name ?w
	    :drum ?code
	    :rule -activity3
	    )
	   )
||||#


#||

; This matches any sentence with the word gene and another thing in it, e.g., "the gene activates the protein": protein will match the second condition
;	  ;; robust rule for explicit construction for genes and proteins, e.g., "ras protein"
;	  (((? reln ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj (:* ?type (? word W::PROTEIN W::GENE)))
;	   ((? reln1 ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name ?type1 :DRUM ?code)
;	   -explicit-ref-molecule>
;	   120
;	   (term ?!obj ont::molecular-part
;	    :name ?!name
;	    :drum ?code
;	    :rule -explicit-ref-molecule
;	    )
;	   )

	  ;; robust rule for explicit construction for drugs, e.g., "PLX4032 drug"
	  (((? reln ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj (:* ?type (? word W::DRUG)) 
	    :ASSOC-WITH ?!name)
	   ((? reln1 ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name ?type1 :DRUM ?code)
	   -explicit-ref-drug>
	   100
	   (term ?!obj ont::CHEMICAL
	    :name ?!name
	    :drum ?code
	    :rule -explicit-ref-drug
	    )
	   )

;	  ;; robust rule for explicit construction for drugs, e.g., "PLX4032 drug"
;	  (((? reln ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj (:* ?type (? word W::DRUG)))
;	   ((? reln1 ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name ?type1 :DRUM ?code)
;	   -explicit-ref-drug>
;	   120
;	   (term ?!obj ont::CHEMICAL
;	    :name ?!name
;	    :drum ?code
;	    :rule -explicit-ref-drug
;	    )
;	   )

||#

	  )
	)

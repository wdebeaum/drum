(in-package "IM")

;(setq *term-extraction-rules* '(drumtermsAdd))
(reset-im-rules 'drumtermsAdd)

(mapcar #'(lambda (x) (add-im-rule x 'drumtermsAdd))  ;; sets of rules are tagged so they can be managed independently 
	'(

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;
; took out :SEQUENCE - from the rules below 
;;;;;;;;;;;;;;;;;;;;;;;;;

	  
#|
	  ;; basic terms (not conjunctions) with selected mod (e.g., prefixes auto-, mono-, trans-)
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
	    (:* (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT) ?w) :SEQUENCE - :MOD ?!modA)
            (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
	   -simple-ref-modA1>
	   100
	   (term ?!obj ?t1
	    :name ?w
	    :modA ?!modA
	    :rule -simple-ref-modA1
	    ))
|#
	  ;; basic terms (not conjunctions) with selected mod (e.g., prefixes auto-, mono-, trans-)
	  ((ONT::TERM ?!obj 
	    ;(? t1 ONT::MUTATION ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT) 
	    ?t1
	    :MODS (?!modA) )
	   ; leave out ONT::INCLUSIVE (for co-)  (by the way, co- is now :MANNER, but it does not need to be returned)
            (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))  
	   -simple-ref-modA2>
	   100
	   (ONT::TERM ?!obj ?t1
	    :modA ?tmp ;?!modA
	    :rule -simple-ref-modA2
	    ))
#|
	  ;; basic terms (not conjunctions) with selected mods that negates/reverses the meaning
	  ;; (e.g., prefixes de-, non-, un-)
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
	    (:* (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT) ?w) :SEQUENCE - :MOD ?!modN)
            (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
	   -simple-ref-modN1>
	   100
	   (term ?!obj ?t1
	    :name ?w
	    :modN ?!modN
	    :rule -simple-ref-modN1
	    ))
|#

	  ;; basic terms (not conjunctions) with selected mods that negates/reverses the meaning
	  ;; (e.g., prefixes de-, non-, un-)
	  ((ONT::TERM ?!obj 
	    ;(? t1 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT) 
	    ?t1
	    :MODS (?!modN) )
            (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
	   -simple-ref-modN2>
	   100
	   (ONT::TERM ?!obj ?t1
	    :modN ?tmp ;?!modN
	    :rule -simple-ref-modN2
	    ))


;; this will return for example ONT::AT-LOC as a MODL for MEK (not good)
;; don't need this for now because extra- etc are not fixed yet.
#||
	  ;; basic terms (not conjunctions) with selected LOC (e.g., extra-)
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
	    (:* (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT) ?w) :SEQUENCE - :LOC ?!modL)
            (ONT::F ?!modL (? tmp ONT::position-as-point-reln))
	   -simple-ref-modL1>
	   100
	   (term ?!obj ?t1
	    :name ?w
	    :modL ?!modL
	    :rule -simple-ref-modL1
	    ))
||#
	  
	  ;; basic terms (not conjunctions) with selected mod (only ONT::ACTIVE)
	  ((ONT::TERM ?!obj 
	    ;(? t1 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT)
            ?t1
	    :MODS (?!modActive) )
            (ONT::F ?!modActive (? tmp ONT::ACTIVE))
	   -simple-ref-modActive1>
	   100
	   (ONT::TERM ?!obj ?t1
	    :active ONT::TRUE
	    :rule -simple-ref-modActive1
	    ))

	  ;; basic terms (not conjunctions) with selected mod (only ONT::INACTIVE)
	  ((ONT::TERM ?!obj 
	    ;(? t1 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT)
            ?t1
	    :MODS (?!modActive))
            (ONT::F ?!modActive (? tmp ONT::INACTIVE))
	   -simple-ref-modActive2>
	   100
	   (ONT::TERM ?!obj ?t1
	    :active ONT::FALSE
	    :rule -simple-ref-modActive2
	    ))

#|
	  ; merged into simple-ref-modEvent
	  ;; basic terms (not conjunctions) with :mod pointing to ACTIVATE events
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
	    (:* (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT) ?w) :SEQUENCE - :MODS (?!ev))
            (ONT::F ?!ev (? tmp ONT::START ONT::START-OBJECT))
	   -simple-ref-modActive3>
	   100
	   (term ?!obj ?t1
	    :name ?w
	    :active +
	    :rule -simple-ref-modActive3
	    ))
|#



	  ;; basic terms (not conjunctions) with selected mods that indicate degree
	  ;; (e.g., hyper-)
	  ;; :degree is automatically extracted but for terms it is :MOD, not :DEGREE
	  ((ONT::TERM ?!obj 
	    ;(? t1 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT) 
            ?t1
	    :MODS (?!modD) )
            (ONT::F ?!modD (? tmp ONT::DEGREE-MODIFIER))
	   -simple-ref-modD2>
	   100
	   (ONT::TERM ?!obj ?t1
	    :degree ?!modD
	    :rule -simple-ref-modD2
	    ))


; INEVENT has moved to ruleFileTop_mod	  
#|
	  ; need to straighten this out!
	  ; also need to fix: it is possible that ?!ev is not extracted as an EVENT (e.g., if the arguments don't match)
	  ;; basic terms (not conjunctions) with :mod pointing to events (BIND, UNBIND, PTM, ACTIVATE, DEACTIVATE, INCREASE, DECREASE, STIMULATE, INHIBIT)
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
	    (:* (? t1 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT) ?!w) :SEQUENCE - :MODS (?!ev) :DRUM ?code)
            (ONT::F ?!ev (? tmp ONT::JOINING ONT::ATTRACT 
ONT::BREAK-OBJECT ONT::SEPARATION 
ONT::POST-TRANSLATIONAL-MODIFICATION 
ONT::START ONT::START-OBJECT 
ONT::STOP 
ONT::INCREASE ONT::ADD-INCLUDE ONT::ACQUIRE ONT::FILLING ONT::START ONT::START-OBJECT
ONT::DECREASE ONT::EXTINGUISH
ONT::ENCOURAGE ONT::CAUSE-STIMULATE ONT::HELP ONT::ENABLE ONT::ALLOW ONT::IMPROVE
ONT::INHIBIT-EFFECT ONT::CAUSE-COME-FROM ONT::REMOVE-FROM ONT::RENDER-INEFFECTIVE
))
	   -simple-ref-modEvent>
	   100
	   (?reln ?!obj ?t1
	    :name ?!w
	    :inevent ?!ev
	    :drum ?code
	    :rule -simple-ref-modEvent
	    ))

|#
	  
	  ;; basic terms (not conjunctions) with mutation (:ASSOC-WITH)
	  ;; G12V Ras
	  ;; Ras G12V (normalized by -n1-mutation>)
	  ((ONT::TERM ?!obj 
;	    (? t1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY) :SEQUENCE - :ASSOC-WITHS (?!m))
	    (? t1 ONT::GENE-PROTEIN) :ASSOC-WITHS (?!m))
            (ONT::TERM ?!m (? tmp ONT::MUTATION))
	   -simple-ref-mutation1>
	   100
	   (ONT::TERM ?!obj ?t1
	    :mutation ?!m
	    :rule -simple-ref-mutation1
	    ))

	  ;; basic terms (not conjunctions) with mutation (in parentheses)
	  ;; Ras (G12V)
	  ((ONT::TERM ?!obj 
;	    (? t1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY) :SEQUENCE - :PARENTHETICAL ?!m )
	    (? t1 ONT::GENE-PROTEIN) :PARENTHETICAL ?!m )
            (ONT::TERM ?!m (? tmp ONT::MUTATION))
	   -simple-ref-mutation2>
	   100
	   (ONT::TERM ?!obj ?t1
	    :mutation ?!m
	    :rule -simple-ref-mutation2
	    ))

	  ;; basic terms (not conjunctions) with molecular site (:ASSOC-WITH)
	  ;; V12 Ras
	  ((ONT::TERM ?!obj 
;	    (? t1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY) :SEQUENCE - :ASSOC-WITHS (?!m))
	    (? t1 ONT::GENE-PROTEIN) :ASSOC-WITHS (?!m))
            (ONT::TERM ?!m (? tmp ONT::MOLECULAR-SITE))
	   -simple-ref-site1>
	   100
	   (ONT::TERM ?!obj ?t1
	    :site ?!m
	    :rule -simple-ref-site1
	    ))

	  ;; basic terms (not conjunctions) with molecular site (in parentheses)
	  ;; Ras (V12)
	  ((ONT::TERM ?!obj 
;	    (? t1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY) :SEQUENCE - :PARENTHETICAL ?!m )
	    (? t1 ONT::GENE-PROTEIN) :PARENTHETICAL ?!m )
            (ONT::TERM ?!m (? tmp ONT::MOLECULAR-SITE))
	   -simple-ref-site2>
	   100
	   (ONT::TERM ?!obj ?t1
	    :site ?!m
	    :rule -simple-ref-site2
	    ))

	  ;; basic terms (not conjunctions) with molecular site (:ASSOC-WITH)
	  ;; Ras V12
	  ((ONT::TERM ?!obj 
	    (? t1 ONT::MOLECULAR-SITE) :ASSOC-WITHS (?!m))
;            (ONT::TERM ?!m (? tmp ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY))
            (ONT::TERM ?!m (? tmp ONT::GENE-PROTEIN))
	   -simple-ref-site1a>
	   100
	   (ONT::TERM ?!m ?tmp   ; note ?!m, not ?!obj
	    :site ?!obj
	    :rule -simple-ref-site1a
	    ))

	  ;; basic terms (not conjunctions) with wild type (:MOD)
	  ;; wild type Ras
	  ((ONT::TERM ?!obj 
;	    (? t1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY) :SEQUENCE - :MODS (?!m) )
	    (? t1 ONT::GENE-PROTEIN) :MODS (?!m) )
            (ONT::F ?!m (:* (? tmp ONT::NATURAL) ?!m2))
	   -simple-ref-mutation3>
	   100
	   (ONT::TERM ?!obj ?t1
	    :mutation ONT::FALSE
	    :rule -simple-ref-mutation3
	    ))

	  ;; basic terms (not conjunctions) with wild type (in parentheses)
	  ;; Ras (wildtype)
	  ((ONT::TERM ?!obj 
;	    (? t1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY) :SEQUENCE - :PARENTHETICAL ?!m )
	    (? t1 ONT::GENE-PROTEIN) :PARENTHETICAL ?!m )
            ((? spec ONT::F ONT::BARE) ?!m (:* (? tmp ONT::NATURAL ONT::WILDTYPE-OBJ) ?!m2))  ; "WILDTYPE-OBJ" added for good measure (see -simple-ref-mutation6)
	   -simple-ref-mutation4>
	   100
	   (ONT::TERM ?!obj ?t1
	    :mutation ONT::FALSE
	    :rule -simple-ref-mutation4
	    ))

	  ;; TERM substitution: WT is extracted as a TERM
	  ;; basic terms (not conjunctions) with wild type (in parentheses)
	  ;; Ras (WT)
	  ((ONT::TERM ?!obj 
;	    (? t1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY) :SEQUENCE - :PARENTHETICAL ?!m )
	    (? t1 ONT::GENE-PROTEIN) :PARENTHETICAL ?!m )
            (ONT::TERM ?!m (? tmp ONT::NATURAL ONT::WILDTYPE-OBJ))  ; "WILDTYPE-OBJ" added for good measure (see -simple-ref-mutation6)
	   -simple-ref-mutation4b>
	   100
	   (ONT::TERM ?!obj ?t1
	    :mutation ONT::FALSE
	    :rule -simple-ref-mutation4b
	    ))

	  ;; basic terms (not conjunctions) with mutant (:MOD)
	  ;; mutant Ras (Note that "Ras mutant" is extracted by -EXPLICIT-REF4>)
	  ((ONT::TERM ?!obj 
;	    (? t1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY) :SEQUENCE - :MODS (?!m) )
	    (? t1 ONT::GENE-PROTEIN) :MODS (?!m) )
            (ONT::F ?!m (:* (? tmp ONT::MUTANT) ?!m2))
	   -simple-ref-mutation5>
	   100
	   (ONT::TERM ?!obj ?t1
	    :mutation ONT::TRUE
	    :rule -simple-ref-mutation5
	    ))

	  ;; basic terms (not conjunctions) with mutant (in parentheses)
	  ;; Ras (mutant)
	  ((ONT::TERM ?!obj 
;	    (? t1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY) :SEQUENCE - :PARENTHETICAL ?!m )
	    (? t1 ONT::GENE-PROTEIN) :PARENTHETICAL ?!m )
            ((? spec ONT::F ONT::BARE ONT::TERM) ?!m (:* (? tmp ONT::MUTANT ONT::MUTANT-OBJ) ?!m2))  ; "mutant" sometimes come out as a noun, which then needs ONT::BARE
	   -simple-ref-mutation6>
	   100
	   (ONT::TERM ?!obj ?t1
	    :mutation ONT::TRUE
	    :rule -simple-ref-mutation6
	    ))

#|	  
	  ;; basic terms (not conjunctions) with mutant (in parentheses)
	  ((ONT::TERM ?!obj 
;	    (? t1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY) :SEQUENCE - :PARENTHETICAL ?!m )
	    (? t1 ONT::GENE-PROTEIN) :SEQUENCE - :PARENTHETICAL ?!m )
            (ONT::TERM ?!m (? tmp ONT::MUTANT ONT::MUTANT-OBJ))  ; "mutant" sometimes come out as a noun, which then needs ONT::BARE
	   -simple-ref-mutation6b>
	   100
	   (ONT::TERM ?!obj ?t1
	    :mutation ONT::TRUE
	    :rule -simple-ref-mutation6b
	    ))
|#
	  
	  ;; basic terms (not conjunctions) for BODY-PART-VAL (e.g., cytoplasmic)
	  ;; *** cytoplasmic doesn't work now because it has been removed from the lexicon (waiting for pertainyms to work)
	  ((ONT::TERM ?!obj 
	    (? t1 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::MEDICAL-DISORDERS-AND-CONDITIONS) :MODS (?!modBP) )
            (ONT::F ?!modBP (? tmp ONT::BODY-PART-VAL))
	   -simple-ref-modBP>
	   100
	   (ONT::TERM ?!obj ?t1
	    :LOC ?!modBP
	    :rule -simple-ref-modBP
	    ))

	  ;; basic terms (not conjunctions) for BODY-PART (e.g., in the nucleus)  *maybe this parse doesn't come out any more
	  ((ONT::TERM ?!obj 
	    (? t1 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::MEDICAL-DISORDERS-AND-CONDITIONS) :ASSOC-WITHS (?!modBP) )
            (ONT::TERM ?!modBP (? tmp ONT::BODY-PART))
	   -simple-ref-modBP2>
	   100
	   (ONT::TERM ?!obj ?t1
	    :LOC ?!modBP
	    :rule -simple-ref-modBP2
	    ))

	  ;; basic terms (not conjunctions) for BODY-PART (e.g., in the nucleus) 
	  ;; *maybe this doesn't work any more either* :LOC has changed to :LOCATION (see -simple-ref-modBP4)  *** seem to have changed back to :LOC! ***

	  #|
	  ((ONT::TERM ?!obj 
	    (? t1 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::MEDICAL-DISORDERS-AND-CONDITIONS) :LOC ?!modL )
	   (ONT::F ?!modL (? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON) :GROUND ?!modBP)	   
            (ONT::TERM ?!modBP (? tmp2 ONT::BODY-PART))
	   -simple-ref-modBP3>
	   100
	   (ONT::TERM ?!obj ?t1
	    :LOC ?!modBP
	    :rule -simple-ref-modBP3
	    ))
	  |#

	  ((ONT::TERM ?!obj 
	    (? t1 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::MEDICAL-DISORDERS-AND-CONDITIONS) :LOC ?!modL )
	   (ONT::F ?!modL (? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON) :GROUND ?!modBP)	   
            (ONT::TERM ?!modBP (? tmp2 ONT::BODY-PART))
	   -simple-ref-modBP3-gd>
	   100
	   (ONT::TERM ?!obj ?t1
	    :LOC ?!modBP
	    :rule -simple-ref-modBP3-gd
	    ))

	  ;; basic terms (not conjunctions) for BODY-PART (e.g., in the nucleus)

	  #|
	  ((ONT::TERM ?!obj 
	    (? t1 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::MEDICAL-DISORDERS-AND-CONDITIONS) :LOCATION ?!modL )
	   (ONT::F ?!modL (? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON) :GROUND ?!modBP)	   
            (ONT::TERM ?!modBP (? tmp2 ONT::BODY-PART))
	   -simple-ref-modBP4>
	   100
	   (ONT::TERM ?!obj ?t1
	    :LOC ?!modBP
	    :rule -simple-ref-modBP4
	    ))
	  |#

	  ((ONT::TERM ?!obj 
	    (? t1 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::MEDICAL-DISORDERS-AND-CONDITIONS) :LOCATION ?!modL )
	   (ONT::F ?!modL (? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON) :GROUND ?!modBP)	   
            (ONT::TERM ?!modBP (? tmp2 ONT::BODY-PART))
	   -simple-ref-modBP4-gd>
	   100
	   (ONT::TERM ?!obj ?t1
	    :LOC ?!modBP
	    :rule -simple-ref-modBP4-gd
	    ))

	  ;; basic terms (not conjunctions) for cell lines

	  #|
	  ((ONT::TERM ?!obj 
	    (? t1 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::MEDICAL-DISORDERS-AND-CONDITIONS) :LOC ?!modL )
	   (ONT::F ?!modL (? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON) :GROUND ?!modBP)	   
            (ONT::TERM ?!modBP (? tmp2 ONT::CELL-LINE))
	   -simple-ref-modCL>
	   100
	   (ONT::TERM ?!obj ?t1
	    :CELL-LINE ?!modBP
	    :LOC -   ; zero out :LOC so it wouldn't be emitted
	    :rule -simple-ref-modCL
	    ))
	  |#

	  ((ONT::TERM ?!obj 
	    (? t1 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::MEDICAL-DISORDERS-AND-CONDITIONS) :LOC ?!modL )
	   (ONT::F ?!modL (? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON) :GROUND ?!modBP)	   
            (ONT::TERM ?!modBP (? tmp2 ONT::CELL-LINE))
	   -simple-ref-modCL-gd>
	   100
	   (ONT::TERM ?!obj ?t1
	    :CELL-LINE ?!modBP
	    :LOC -   ; zero out :LOC so it wouldn't be emitted
	    :rule -simple-ref-modCL-gd
	    ))
	  
	  ; Ras localizes to/in the nucleus
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::BE-AT-LOC) ?!w) :NEUTRAL ?!obj :DRUM ?code :LOCATION ?!modL)
;           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::TERM ?!obj ?t1)
           (ONT::TERM ?!modL (? tmp ONT::BODY-PART))
           -location1>
	   100
	   (ONT::TERM ?!obj ?t1
	    :LOC ?!modL
	    :rule -location1
	    ))

	  ; Ras is localized to/in the nucleus  (Can you say this?)
	  ; Ras and Raf are colocalized in the nucleus
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::BE-AT-LOC) ?!w) :NEUTRAL1 ?!obj :DRUM ?code :LOCATION ?!modL)
;           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::TERM ?!obj ?t1)
           (ONT::TERM ?!modL (? tmp ONT::BODY-PART))
           -location1b>
	   100
	   (ONT::TERM ?!obj ?t1
	    :LOC ?!modL
	    :rule -location1b
	    ))
	  
	  ; Ras is located in the nucleus

	  #|
	  (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::EXISTS) ?!w) :NEUTRAL ?!obj :DRUM ?code :LOCATION ?!modL)
;           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::TERM ?!obj ?t1)
	   (ONT::F ?!modL (? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON) :GROUND ?!modBP)	   
           (ONT::TERM ?!modBP (? tmp2 ONT::BODY-PART))
           -location2>
	   100
	   (ONT::TERM ?!obj ?t1
	    :LOC ?!modBP
	    :rule -location2
	    ))
	  |#

          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::EXISTS) ?!w) :NEUTRAL ?!obj :DRUM ?code :LOCATION ?!modL)
;           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::TERM ?!obj ?t1)
	   (ONT::F ?!modL (? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON) :GROUND ?!modBP)	   
           (ONT::TERM ?!modBP (? tmp2 ONT::BODY-PART))
           -location2-gd>
	   100
	   (ONT::TERM ?!obj ?t1
	    :LOC ?!modBP
	    :rule -location2-gd
	    ))

	  ; Ras is in the nucleus

	  #|
	  (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::HAVE-PROPERTY) ?!w) :NEUTRAL ?!obj :DRUM ?code :FORMAL ?!modL)
;           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::TERM ?!obj ?t1)
	   (ONT::F ?!modL (? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON) :GROUND ?!modBP)	   
           (ONT::TERM ?!modBP (? tmp2 ONT::BODY-PART))
           -location3>
	   100
	   (ONT::TERM ?!obj ?t1
	    :LOC ?!modBP
	    :rule -location3
	    ))
	  |#

          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::HAVE-PROPERTY) ?!w) :NEUTRAL ?!obj :DRUM ?code :FORMAL ?!modL)
;           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::TERM ?!obj ?t1)
	   (ONT::F ?!modL (? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON) :GROUND ?!modBP)	   
           (ONT::TERM ?!modBP (? tmp2 ONT::BODY-PART))
           -location3-gd>
	   100
	   (ONT::TERM ?!obj ?t1
	    :LOC ?!modBP
	    :rule -location3-gd
	    ))
	  
	  ; Ras resides/hangs out in the nucleus
	  ; Ras residency in the nucleus

	  #|
	  (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::APPEAR ONT::STAY ONT::STOP-MOVE ONT::WAIT) ?!w) :AGENT ?!obj :DRUM ?code :LOCATION ?!modL)
;           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::TERM ?!obj ?t1)
	   (ONT::F ?!modL (? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON) :GROUND ?!modBP)	   
           (ONT::TERM ?!modBP (? tmp2 ONT::BODY-PART))
           -location4>
	   100
	   (ONT::TERM ?!obj ?t1
	    :LOC ?!modBP
	    :rule -location4
	    ))
	  |#

          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::APPEAR ONT::STAY ONT::STOP-MOVE ONT::WAIT) ?!w) :AGENT ?!obj :DRUM ?code :LOCATION ?!modL)
;           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::TERM ?!obj ?t1)
	   (ONT::F ?!modL (? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON) :GROUND ?!modBP)	   
           (ONT::TERM ?!modBP (? tmp2 ONT::BODY-PART))
           -location4-gd>
	   100
	   (ONT::TERM ?!obj ?t1
	    :LOC ?!modBP
	    :rule -location4-gd
	    ))
	  
	  ; Ras lives in the nucleus

	  #|
	  (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::LIVE) ?!w) :AFFECTED ?!obj :DRUM ?code :LOCATION ?!modL)
;           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::TERM ?!obj ?t1)
	   (ONT::F ?!modL (? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON) :GROUND ?!modBP)	   
           (ONT::TERM ?!modBP (? tmp2 ONT::BODY-PART))
           -location5>
	   100
	   (ONT::TERM ?!obj ?t1
	    :LOC ?!modBP
	    :rule -location5
	    ))
	  |#

          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::LIVE) ?!w) :AFFECTED ?!obj :DRUM ?code :LOCATION ?!modL)
;           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::TERM ?!obj ?t1)
	   (ONT::F ?!modL (? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON) :GROUND ?!modBP)	   
           (ONT::TERM ?!modBP (? tmp2 ONT::BODY-PART))
           -location5-gd>
	   100
	   (ONT::TERM ?!obj ?t1
	    :LOC ?!modBP
	    :rule -location5-gd
	    ))
	  
	  ; Ras lives in the nucleus

	  #|
          ((ONT::TERM ?ev
            (? type ONT::LOCATION) :FIGURE ?!obj :DRUM ?code :LOC ?!modL)
;           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::TERM ?!obj ?t1)
	   (ONT::F ?!modL (? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON) :GROUND ?!modBP)	   
           (ONT::TERM ?!modBP (? tmp2 ONT::BODY-PART))
           -location6>
	   100
	   (ONT::TERM ?!obj ?t1
	    :LOC ?!modBP
	    :rule -location6
	    ))
	  |#

          ((ONT::TERM ?ev
            (? type ONT::LOCATION) :FIGURE ?!obj :DRUM ?code :LOC ?!modL)
;           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::TERM ?!obj ?t1)
	   (ONT::F ?!modL (? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON) :GROUND ?!modBP)	   
           (ONT::TERM ?!modBP (? tmp2 ONT::BODY-PART))
           -location6-gd>
	   100
	   (ONT::TERM ?!obj ?t1
	    :LOC ?!modBP
	    :rule -location6-gd
	    ))
	  
	  ; Ras is active
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::HAVE-PROPERTY) ?!w) :NEUTRAL ?!obj :DRUM ?code :FORMAL ?!modActive)
;           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::TERM ?!obj ?t1)
           (ONT::F ?!modActive (? tmp2 ONT::ACTIVE))
           -active3>
	   100
	   (ONT::TERM ?!obj ?t1
	    :active ONT::TRUE
	    :rule -active3
	    ))

	  ; Ras is inactive
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::HAVE-PROPERTY) ?!w) :NEUTRAL ?!obj :DRUM ?code :FORMAL ?!modActive)
;           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::TERM ?!obj ?t1)
           (ONT::F ?!modActive (? tmp2 ONT::INACTIVE))
           -active4>
	   100
	   (ONT::TERM ?!obj ?t1
	    :active ONT::FALSE
	    :rule -active4
	    ))

	  #|
	  ; conserved domain
	  ;; basic terms (not conjunctions) with selected mod (only ONT::ACTIVE)
	  ((ONT::TERM ?!obj 
	    (? t1 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT) :SEQUENCE - :MODS (?!modC) )
            (ONT::F ?!modC (:* ONT::RETAIN W::CONSERVE) :AFFECTED ?!obj)
	   -simple-ref-modConserved>
	   100
	   (ONT::TERM ?!obj ?t1
	    :conserved ONT::TRUE
	    :rule -simple-ref-modConserved
	    ))
	  |#

	  ;;;;;;;;;;;;;; phosphorylation ;;;;;;;;;;;;;;

	  ; phosphoRaf; pAck1
	  ((ONT::TERM ?!name (? t ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ) :MODS (?!m)) 
	   (ONT::F ?!m ONT::PHOSPHORILATED)
	   -phospho1>
	   100
	   (ONT::event ?!m ONT::PHOSPHORYLATION
	    :rule -phospho1
	    :AFFECTED ?!name
	    :type ONT::PHOSPHORILATED
;	    :drum ?code    
	    )
	   )

	  ; pAck1-Tyr176; pAck1-Tyr
	  ((ONT::TERM ?!name (? t ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ) :MODS (?!m)) 
	   (ONT::F ?!m ONT::PHOSPHORILATED)
	   (ONT::TERM ?!loc (? locType ONT::MOLECULAR-SITE ONT::MOLECULAR-DOMAIN ONT::AMINO-ACID ONT::RESIDUE ONT::TERMINUS ONT::MUTATION) :ASSOC-WITHS (?!name)) ; note: it doesn't work to make ?name and ?loc optional, probably because ?name gets instantiated
	   -phospho1b>
	   100
	   (ONT::event ?!m ONT::PHOSPHORYLATION
	    :rule -phospho1b
	    :AFFECTED ?!name
	    :SITE ?!loc
	    :type ONT::PHOSPHORILATED
;	    :drum ?code    
	    )
	   )

	  ; pTyr; pTyr176
	  ((ONT::TERM ?!name (? locType ONT::MOLECULAR-SITE ONT::MOLECULAR-DOMAIN ONT::AMINO-ACID ONT::RESIDUE ONT::TERMINUS ONT::MUTATION) :MODS (?!m)) 
	   (ONT::F ?!m ONT::PHOSPHORILATED)
	   -phospho2>
	   101  ; higher priority than -phospho1> because MOLECULAR-SITE would also match MOLECULAR-PART
	   (ONT::event ?!m ONT::PHOSPHORYLATION
	    :rule -phospho2
	    :SITE ?!name
	    :type ONT::PHOSPHORILATED
;	    :drum ?code    
	    )
	   )

	  ; pTyr176-AKT
	  ((ONT::TERM ?!loc (? locType ONT::MOLECULAR-SITE ONT::MOLECULAR-DOMAIN ONT::AMINO-ACID ONT::RESIDUE ONT::TERMINUS ONT::MUTATION) :MODS (?!m)) 
	   (ONT::F ?!m ONT::PHOSPHORILATED)
	   (ONT::TERM ?!name (? t ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ) :ASSOC-WITHS (?!loc))
	   -phospho2b>
	   100
	   (ONT::event ?!m ONT::PHOSPHORYLATION
	    :rule -phospho2b
	    :AFFECTED ?!name
	    :SITE ?!loc
	    :type ONT::PHOSPHORILATED
;	    :drum ?code    
	    )
	   (ONT::TERM ?!name ?t
	    :rule -phospho2b-term
	    :INEVENT ?!m
	    )
	   )

	  ; pAck1(Tyr); pAck1(Tyr176)
	  ((ONT::TERM ?!name (? t ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ) :MODS (?!m) :PARENTHETICAL ?!loc) 
	   (ONT::F ?!m ONT::PHOSPHORILATED)
	   (ONT::TERM ?!loc (? locType ONT::MOLECULAR-SITE ONT::MOLECULAR-DOMAIN ONT::AMINO-ACID ONT::RESIDUE ONT::TERMINUS ONT::MUTATION)) 
	   -phospho3>
	   100
	   (ONT::event ?!m ONT::PHOSPHORYLATION
	    :rule -phospho3
	    :AFFECTED ?!name
	    :SITE ?!loc
	    :type ONT::PHOSPHORILATED
;	    :drum ?code    
	    )
	   )

	  ; pTyr(Ack1)   ; this notation might not happen but is here just in case
	  ((ONT::TERM ?!name (? t ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ)) 
	   (ONT::F ?!m ONT::PHOSPHORILATED)
	   (ONT::TERM ?!loc (? locType ONT::MOLECULAR-SITE ONT::MOLECULAR-DOMAIN ONT::AMINO-ACID ONT::RESIDUE ONT::TERMINUS ONT::MUTATION) :MODS (?!m) :PARENTHETICAL ?!name) 
	   -phospho3b>
	   100
	   (ONT::event ?!m ONT::PHOSPHORYLATION
	    :rule -phospho3b
	    :AFFECTED ?!name
	    :SITE ?!loc
	    :type ONT::PHOSPHORILATED
;	    :drum ?code    
	    )
	   )

	  ; pRasV12
	  ((ONT::TERM ?!name (? t ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ) :MODS (?!m) :ASSOC-WITHS (?!loc)) 
	   (ONT::F ?!m ONT::PHOSPHORILATED)
	   (ONT::TERM ?!loc (? locType ONT::MOLECULAR-SITE ONT::MOLECULAR-DOMAIN ONT::AMINO-ACID ONT::RESIDUE ONT::TERMINUS ONT::MUTATION)) 
	   -phospho4>
	   100
	   (ONT::event ?!m ONT::PHOSPHORYLATION
	    :rule -phospho4
	    :AFFECTED ?!name
	    :SITE ?!loc
	    :type ONT::PHOSPHORILATED
;	    :drum ?code    
	    )
	   )
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  
	  )
	)



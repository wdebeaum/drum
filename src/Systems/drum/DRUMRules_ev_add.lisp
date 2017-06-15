(in-package "IM")

(reset-im-rules 'drum_ev_add)

(mapcar #'(lambda (x) (add-im-rule x 'drum_ev_add))  
	'(
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  ; RAS converts GTP into/to GDP. 
	  ((ONT::EVENT ?ev ?type :RESULT ?!res)
	   (ONT::F ?!res (? type1 ont::goal-reln) :GROUND ?!res1)
	   (;(? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM)
	    ?reln1
	    ?!res1  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART
;ONT::CELL-PART ONT::BODY-PART ONT::MEDICAL-DISORDERS-AND-CONDITIONS
ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ))
           -result1>
           100
	   (ONT::EVENT ?ev ?type
	    :rule -result1
	    :RES ?!res1
	    )
          )
	  
	  ; RAS converts GTP to form GDP. 
	  ((ONT::EVENT ?ev ?type :REASON ?!res)
	   (ONT::F ?!res ONT::PURPOSE :GROUND ?!res1)
	   (ONT::EVENT ?!res1 ONT::PRODUCE :AFFECTED-RESULT ?!res2)
	   (;(? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM)
	    ?reln1
	    ?!res2  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART
;ONT::CELL-PART ONT::BODY-PART ONT::MEDICAL-DISORDERS-AND-CONDITIONS
ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ))
           -result2>
           100
	   (ONT::EVENT ?ev ?type
	    :rule -result2
	    :RES ?!res2
	    :REASON -  ; get rid of this; otherwise we get a CC BY-MEANS-OF using -PURPOSE-GD>
	    )
          )

	  ; RAS phosphorylates GTP into/to GDP. 
	  ((ONT::EVENT ?ev ?type :MODS (?!res))
	   (ONT::F ?!res (? type1 ont::goal-reln) :GROUND ?!res1)
	   (;(? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM)
	    ?reln1
	    ?!res1  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART
;ONT::CELL-PART ONT::BODY-PART ONT::MEDICAL-DISORDERS-AND-CONDITIONS
ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ))
           -result3>
           100
	   (ONT::EVENT ?ev ?type
	    :rule -result3
	    :RES ?!res1
	    )
          )
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  )
	)

;;; See DRUMRules.lisp for comments

(in-package "IM")

;(setq *post-extraction-rules* '(drumCC))
(reset-im-rules 'drumCC)

(mapcar #'(lambda (x) (add-im-rule x 'drumCC))  
	'(

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INEVENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
          ((TERM ?ev ?type :MODS (?!ev2))
	   (EVENT ?!ev2 ?!type2)
           -inevent1>
           100
	   (TERM ?ev ?type
	    :rule -inevent1
	    :INEVENT ?!ev2
	    )
          )
|#

	  ; the preexisting INEVENT could be from, e.g., pTyr176-AKT (-phospho2b>)
          (((? reln ONT::EVENT ONT::TERM) ?ev ?type :MODS (?!ev2) :INEVENT ?!ev3)  
	   ((? reln2 ONT::EVENT ONT::CC) ?!ev2 ?type2)
	   ((? reln2b ONT::EVENT ONT::CC) ?ev2b ?type2b)
           -inevent4>
           100
	   (?reln ?ev ?type
	    :rule -inevent4
	    :INEVENT ?!ev2
	    :INEVENT ?!ev3
	    )
          )

	  
	  ; NRAS, expressed by GTP, binds BRAF.	  	  
	  ; RAF-bound BRAF that is not bound to Vemurafenib phosphorylates MAP2K1. (need to return *two* INEVENT slots)
          (((? reln ONT::EVENT ONT::TERM) ?ev ?type :MODS (?!ev2 ?ev2b))  ; note optional ?ev2b
	   ((? reln2 ONT::EVENT ONT::CC) ?!ev2 ?type2)
	   ((? reln2b ONT::EVENT ONT::CC) ?ev2b ?type2b)
           -inevent2>
           100
	   (?reln ?ev ?type
	    :rule -inevent2
	    :INEVENT ?!ev2
	    :INEVENT ?ev2b
	    )
          )

	  ; Need this rule in addition to -inevent2 because some MODs (e.g., "active") are not EVENT/CC
	  ; Active MAP2K1 that is not bound to PP2A-alpha phosphorylates MAPK1. 
          (((? reln ONT::EVENT ONT::TERM) ?ev ?type :MODS (?!ev2))  
	   ((? reln2 ONT::EVENT ONT::CC) ?!ev2 ?type2)
;	   ((? reln2b ONT::EVENT ONT::CC) ?ev2b ?type2b)
           -inevent2b>
           100
	   (?reln ?ev ?type
	    :rule -inevent2b
	    :INEVENT ?!ev2
;	    :INEVENT ?ev2b
	    )
	   )

	  
	  ; This doesn't fire anymore?  But if it does, we need to consider two INEVENTs here (e.g., Raf-bound NRAS, bound to GTP, binds BRAF, but this is now parsed as two :MODs)
	  ; NRAS, bound to GTP, binds BRAF.
          (((? reln ONT::EVENT ONT::TERM) ?ev ?type :PARENTHETICAL ?!ev2)
	   (ONT::EVENT ?!ev2 ?!type2 :AFFECTED ?ev)
           -inevent3a>
           100
	   (?reln ?ev ?type
	    :rule -inevent2
	    :INEVENT ?!ev2
	    )
          )

	  ; these following -inevent3* rules might not fire.  They are here for completeness
          (((? reln ONT::EVENT ONT::TERM) ?ev ?type :PARENTHETICAL ?!ev2)
	   (ONT::EVENT ?!ev2 ?!type2 :AGENT ?ev)
           -inevent3b>
           100
	   (?reln ?ev ?type
	    :rule -inevent2
	    :INEVENT ?!ev2
	    )
          )

          (((? reln ONT::EVENT ONT::TERM) ?ev ?type :PARENTHETICAL ?!ev2)
	   (ONT::EVENT ?!ev2 ?!type2 :AFFECTED-RESULT ?ev)
           -inevent3c>
           100
	   (?reln ?ev ?type
	    :rule -inevent2
	    :INEVENT ?!ev2
	    )
          )

          (((? reln ONT::EVENT ONT::TERM) ?ev ?type :PARENTHETICAL ?!ev2)
	   (ONT::EVENT ?!ev2 ?!type2 :NEUTRAL ?ev)
           -inevent3d>
           100
	   (?reln ?ev ?type
	    :rule -inevent2
	    :INEVENT ?!ev2
	    )
          )

          (((? reln ONT::EVENT ONT::TERM) ?ev ?type :PARENTHETICAL ?!ev2)
	   (ONT::EVENT ?!ev2 ?!type2 :FORMAL ?ev)
           -inevent3e>
           100
	   (?reln ?ev ?type
	    :rule -inevent2
	    :INEVENT ?!ev2
	    )
          )

          (((? reln ONT::EVENT ONT::TERM) ?ev ?type :PARENTHETICAL ?!ev2)
	   (ONT::EVENT ?!ev2 ?!type2 :AFFECTED1 ?ev)
           -inevent3f>
           100
	   (?reln ?ev ?type
	    :rule -inevent2
	    :INEVENT ?!ev2
	    )
          )

          (((? reln ONT::EVENT ONT::TERM) ?ev ?type :PARENTHETICAL ?!ev2)
	   (ONT::EVENT ?!ev2 ?!type2 :NEUTRAL1 ?ev)
           -inevent3g>
           100
	   (?reln ?ev ?type
	    :rule -inevent2
	    :INEVENT ?!ev2
	    )
          )

	  
	  
#|
; subsumed by -inevent2

	  ;; basic terms (not conjunctions) with :mod pointing to events
	  ;; This extracts :MODs pointing to all EVENTs, as opposed to previously we only extract BIND, BREAK, and a few others.
;; Fix: note that ?t1 and ?!w could have been changed by explict-ref rules
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj
	    ; remove ?!w because this is not there after TERM substitution 
;	    (:* (? t1 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT) ?!w) :SEQUENCE - :MODS (?!ev) :DRUM ?code)
	    (? t1 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT) :SEQUENCE - :MODS (?!ev) :DRUM ?code)
	   (EVENT ?!ev ?!type2)
	   -simple-ref-modEvent>
	   100
	   (?reln ?!obj ?t1
;	    :name ?!w   ; this would have already been returned in the main term extraction
	    :inevent ?!ev
;	    :drum ?code   ; this would have already been returned in the main term extraction
	    :rule -simple-ref-modEvent
	    ))
|#

	  
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; via ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  
	  ; Ras activates Erk via Raf.
          ((ONT::EVENT ?ev ?type :AGENT ?!ag :AFFECTED ?!obj :MODS (?!m))
	   (ONT::F ?!m ONT::OBJ-IN-PATH :VAL ?!mVal)
	   (ONT::TERM ?!mVal ?!type)   
           -via>
           100
           (ONT::EVENT *1 ONT::MODULATE
            :rule -via-A
	    :AGENT ?!ag
            :AFFECTED ?!mVal
	    :TYPE ONT::CONTROL-MANAGE ; this is invented---there is nothing corresponding to this event in the LF
            )
	   (ONT::EVENT *2 ONT::MODULATE
	    :rule -via-B
	    :AGENT ?!mVal
	    :AFFECTED ?!obj
	    :TYPE ONT::CONTROL-MANAGE ; this is invented---there is nothing corresponding to this event in the LF
	    )
           (ONT::CC *3 ONT::BY-MEANS-OF
            :rule -via-C
	    :factor-sequence (*1 *2)
            :OUTCOME ?ev
            )
	   #|
           (CC *3 ONT::VIA
            :rule -via
	    :cause ?!mVal
            :effect ?ev
            )
	   |#
          )

          ((ONT::EVENT ?ev ?type :AGENT ?!ag :AFFECTED ?!obj :MODS (?!m))
	   (ONT::F ?!m ONT::OBJ-IN-PATH :GROUND ?!mVal)
	   (ONT::TERM ?!mVal ?!type)   
           -via-gb>
           100
           (ONT::EVENT *1 ONT::MODULATE
            :rule -via-A-gb
	    :AGENT ?!ag
            :AFFECTED ?!mVal
	    :TYPE ONT::CONTROL-MANAGE ; this is invented---there is nothing corresponding to this event in the LF
            )
	   (ONT::EVENT *2 ONT::MODULATE
	    :rule -via-B-gb
	    :AGENT ?!mVal
	    :AFFECTED ?!obj
	    :TYPE ONT::CONTROL-MANAGE ; this is invented---there is nothing corresponding to this event in the LF
	    )
           (ONT::CC *3 ONT::BY-MEANS-OF
            :rule -via-C-gb
	    :factor-sequence (*1 *2)
            :OUTCOME ?ev
            )
	   #|
           (CC *3 ONT::VIA
            :rule -via
	    :cause ?!mVal
            :effect ?ev
            )
	   |#
          )

	  
	  ; Ras binds to Raf via S338.  
          ((ONT::EVENT ?ev ONT::BIND :AGENT ?!ag :AFFECTED ?!obj :MODS (?!m))
	   (ONT::F ?!m (:* ONT::OBJ-IN-PATH ?!w) :VAL ?!locVal)
           (ONT::TERM ?!locVal (? locType ONT::MOLECULAR-SITE ONT::MOLECULAR-DOMAIN ONT::AMINO-ACID ONT::RESIDUE ONT::TERMINUS))
           -via2>
           100
           (ONT::EVENT ?ev ONT::BIND
            :rule -via2
	    :sitemod (:* ONT::OBJ-IN-PATH ?!w)
	    :site ?!locVal
            )
	   )

          ((ONT::EVENT ?ev ONT::BIND :AGENT ?!ag :AFFECTED ?!obj :MODS (?!m))
	   (ONT::F ?!m (:* ONT::OBJ-IN-PATH ?!w) :GROUND ?!locVal)
           (ONT::TERM ?!locVal (? locType ONT::MOLECULAR-SITE ONT::MOLECULAR-DOMAIN ONT::AMINO-ACID ONT::RESIDUE ONT::TERMINUS))
           -via2-gd>
           100
           (ONT::EVENT ?ev ONT::BIND
            :rule -via2-gd
	    :sitemod (:* ONT::OBJ-IN-PATH ?!w)
	    :site ?!locVal
            )
	   )
	  


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bmo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  
	  ; Ras activates Erk by binding to Raf.
          ((ONT::EVENT ?ev ?type :AGENT ?!ag :METHOD ?!bmo :PASSIVE -)
	   (ONT::F ?!bmo ONT::BY-MEANS-OF :VAL ?!bmoVal)
	   (ONT::EVENT ?!bmoVal ?!bmoValType :AGENT -)
           -bmo1>
           100
           (ONT::CC *1 ONT::BY-MEANS-OF
            :rule -bmo1
	    :FACTOR ?!bmoVal
            :OUTCOME ?ev
	    :TYPE ONT::BY-MEANS-OF
            )
	   (ONT::EVENT ?!bmoVal ?!bmoValType
	    :rule -bmo1-event
	    :AGENT ?!ag
	    )
	   )

          ((ONT::EVENT ?ev ?type :AGENT ?!ag :METHOD ?!bmo :PASSIVE -)
	   (ONT::F ?!bmo ONT::BY-MEANS-OF :GROUND ?!bmoVal)
	   (ONT::EVENT ?!bmoVal ?!bmoValType :AGENT -)
           -bmo1-gd>
           100
           (ONT::CC *1 ONT::BY-MEANS-OF
            :rule -bmo1-gd
	    :FACTOR ?!bmoVal
            :OUTCOME ?ev
	    :TYPE ONT::BY-MEANS-OF
            )
	   (ONT::EVENT ?!bmoVal ?!bmoValType
	    :rule -bmo1-event-gd
	    :AGENT ?!ag
	    )
	   )
	  
	  ; Our results show that Ras activates Erk by binding to Raf.
#|
          ((CC ?ev ONT::BY-MEANS-OF :OUTCOME ?!outcomeV)
	   (EVENT ?!outcomeV ?!type1 :epi ?!epiV)
	   (EPI ?!epiV ?!type2 :FORMAL ?!outcomeV)
           -cc-epi1>
           100
           (CC ?ev ?!type
            :rule -cc-epi1
	    :epi ?!epiV
            )
	   (EPI ?!epiV ?!type2
	    :rule -cc-epi1-epi
	    :FORMAL ?ev
	    )
	   )
|#

	  #|   ********  commenting this out for now  ***********
          ((EVENT ?ev ?type :AGENT ?!ag :METHOD ?!bmo :PASSIVE - :epi ?!epiV)
	   (EVENT ?ev2 ?type :AGENT ?!ag :METHOD ?!bmo :PASSIVE - :epi -)  ; dummy to prevent -bmo1 from also firing.  Should be removed when we are merging LF substitutions
	   (ONT::F ?!bmo ONT::BY-MEANS-OF :VAL ?!bmoVal)
	   (EVENT ?!bmoVal ?!bmoValType :AGENT -)
	   (EPI ?!epiV ?!type2 :FORMAL ?ev)
           -bmo-epi1>
           100
           (CC *1 ONT::BY-MEANS-OF
            :rule -bmo-epi1
	    :FACTOR ?!bmoVal
            :OUTCOME ?ev
	    :epi* ?!epiV
            )
	   (EVENT ?!bmoVal ?!bmoValType
	    :rule -bmo-epi1-event
	    :AGENT ?!ag
	    )
	   (EPI ?!epiV ?!type2
	    :rule -bmo-epi1-epi
	    :FORMAL* *1
	    )
	   )

	  |#
	  
	  ; Ras activates Erk by inducing the binding to Raf.
          ((ONT::EVENT ?ev ?type :AGENT ?!ag :METHOD ?!bmo :PASSIVE -)
	   (ONT::F ?!bmo ONT::BY-MEANS-OF :VAL ?!bmoVal)
	   (ONT::CC ?!bmoVal ?!bmoValType :FACTOR -)
           -bmo1-cc>
           100
           (ONT::CC *1 ONT::BY-MEANS-OF
            :rule -bmo1-cc
	    :FACTOR ?!bmoVal
            :OUTCOME ?ev
	    :TYPE ONT::BY-MEANS-OF
            )
	   (ONT::CC ?!bmoVal ?!bmoValType
	    :rule -bmo1-cc-event
	    :FACTOR ?!ag
	    )
	   )

          ((ONT::EVENT ?ev ?type :AGENT ?!ag :METHOD ?!bmo :PASSIVE -)
	   (ONT::F ?!bmo ONT::BY-MEANS-OF :GROUND ?!bmoVal)
	   (ONT::CC ?!bmoVal ?!bmoValType :FACTOR -)
           -bmo1-cc-gd>
           100
           (ONT::CC *1 ONT::BY-MEANS-OF
            :rule -bmo1-cc-gd
	    :FACTOR ?!bmoVal
            :OUTCOME ?ev
	    :TYPE ONT::BY-MEANS-OF
            )
	   (ONT::CC ?!bmoVal ?!bmoValType
	    :rule -bmo1-cc-event-gd
	    :FACTOR ?!ag
	    )
	   )


	  
	  #|   ********  commenting this out for now  ***********
          ((EVENT ?ev ?type :AGENT ?!ag :METHOD ?!bmo :PASSIVE - :epi ?!epiV)
	   (EVENT ?ev2 ?type :AGENT ?!ag :METHOD ?!bmo :PASSIVE - :epi -)  ; dummy to prevent -bmo1 from also firing.  Should be removed when we are merging LF substitutions
	   (ONT::F ?!bmo ONT::BY-MEANS-OF :VAL ?!bmoVal)
	   (CC ?!bmoVal ?!bmoValType :FACTOR -)
	   (EPI ?!epiV ?!type2 :FORMAL ?ev)
           -bmo-cc-epi1>
           100
           (CC *1 ONT::BY-MEANS-OF
            :rule -bmo-cc-epi1
	    :FACTOR ?!bmoVal
            :OUTCOME ?ev
	    :epi* ?!epiV
            )
	   (CC ?!bmoVal ?!bmoValType
	    :rule -bmo-cc-epi1-event
	    :FACTOR ?!ag
	    )
	   (EPI ?!epiV ?!type2
	    :rule -bmo-cc-epi1-epi
	    :FORMAL* *1
	    )
	   )
	  |#
	  
					; should have a rule that generates AFFECTED-RESULT too

	  ; Note: We are passing on the AFFECTED only for these events.  In general we shouldn't.
	  ; For example: I scared him by jumping off the bridge.
	  ; Ras activates Erk by phosphorylation.
          ((ONT::EVENT ?ev ?type :AGENT ?!ag :AFFECTED ?!obj :METHOD ?!bmo :PASSIVE -)
	   (ONT::F ?!bmo ONT::BY-MEANS-OF :VAL ?!bmoVal)
	   (ONT::EVENT ?!bmoVal ?!bmoValType :AGENT - :AFFECTED -)
           -bmo1b>
           120  ; takes precedence to -bmo1
           (ONT::CC *1 ONT::BY-MEANS-OF
            :rule -bmo1b
	    :FACTOR ?!bmoVal
            :OUTCOME ?ev
	    :TYPE ONT::BY-MEANS-OF
            )
	   (ONT::EVENT ?!bmoVal ?!bmoValType
	    :rule -bmo1b-event
	    :AGENT ?!ag
	    :AFFECTED ?!obj
	    )
	   )

          ((ONT::EVENT ?ev ?type :AGENT ?!ag :AFFECTED ?!obj :METHOD ?!bmo :PASSIVE -)
	   (ONT::F ?!bmo ONT::BY-MEANS-OF :GROUND ?!bmoVal)
	   (ONT::EVENT ?!bmoVal ?!bmoValType :AGENT - :AFFECTED -)
           -bmo1b-gd>
           120  ; takes precedence to -bmo1
           (ONT::CC *1 ONT::BY-MEANS-OF
            :rule -bmo1b-gd
	    :FACTOR ?!bmoVal
            :OUTCOME ?ev
	    :TYPE ONT::BY-MEANS-OF
            )
	   (ONT::EVENT ?!bmoVal ?!bmoValType
	    :rule -bmo1b-event-gd
	    :AGENT ?!ag
	    :AFFECTED ?!obj
	    )
	   )

	  
					; should have a rule that generates AFFECTED-RESULT too
	  ; Raf activates Erk by being stimulated by Ras.
          ((ONT::EVENT ?ev ?type :AGENT ?!ag :METHOD ?!bmo :PASSIVE -)
	   (ONT::F ?!bmo ONT::BY-MEANS-OF :VAL ?!bmoVal)
	   (ONT::EVENT ?!bmoVal ?!bmoValType :PASSIVE +)
           -bmo1a>
           100
           (ONT::CC *1 ONT::BY-MEANS-OF
            :rule -bmo1a
	    :FACTOR ?!bmoVal
            :OUTCOME ?ev
	    :TYPE ONT::BY-MEANS-OF
            )
	   (ONT::EVENT ?!bmoVal ?!bmoValType
	    :rule -bmo1a-event
	    :AFFECTED ?!ag
	    )
          )

          ((ONT::EVENT ?ev ?type :AGENT ?!ag :METHOD ?!bmo :PASSIVE -)
	   (ONT::F ?!bmo ONT::BY-MEANS-OF :GROUND ?!bmoVal)
	   (ONT::EVENT ?!bmoVal ?!bmoValType :PASSIVE +)
           -bmo1a-gd>
           100
           (ONT::CC *1 ONT::BY-MEANS-OF
            :rule -bmo1a-gd
	    :FACTOR ?!bmoVal
            :OUTCOME ?ev
	    :TYPE ONT::BY-MEANS-OF
            )
	   (ONT::EVENT ?!bmoVal ?!bmoValType
	    :rule -bmo1a-event-gd
	    :AFFECTED ?!ag
	    )
          )

	  
	  ; By binding to Ras, Raf is activated.
	  ((ONT::EVENT ?ev ?type :AFFECTED ?!obj :METHOD ?!bmo :PASSIVE +)
	   (ONT::F ?!bmo ONT::BY-MEANS-OF :VAL ?!bmoVal)
	   (ONT::EVENT ?!bmoVal ?!bmoValType :AGENT - :AFFECTED ?!bmoAff)
           -bmo2>
           100
           (ONT::CC *1 ONT::BY-MEANS-OF
            :rule -bmo2
	    :FACTOR ?!bmoVal
            :OUTCOME ?ev
	    :TYPE ONT::BY-MEANS-OF
            )
	   (ONT::EVENT ?!bmoVal ?!bmoValType
	    :rule -bmo2-event
	    :AGENT ?!obj
	    )
          )

	  ((ONT::EVENT ?ev ?type :AFFECTED ?!obj :METHOD ?!bmo :PASSIVE +)
	   (ONT::F ?!bmo ONT::BY-MEANS-OF :GROUND ?!bmoVal)
	   (ONT::EVENT ?!bmoVal ?!bmoValType :AGENT - :AFFECTED ?!bmoAff)
           -bmo2-gd>
           100
           (ONT::CC *1 ONT::BY-MEANS-OF
            :rule -bmo2-gd
	    :FACTOR ?!bmoVal
            :OUTCOME ?ev
	    :TYPE ONT::BY-MEANS-OF
            )
	   (ONT::EVENT ?!bmoVal ?!bmoValType
	    :rule -bmo2-event-gd
	    :AGENT ?!obj
	    )
          )

	  
					; should have a rule that generates AFFECTED-RESULT too
	  ; By phosphorylation, Raf is activated.
	  ; By phosphorylation by Ras, Raf is activated.
	  ((ONT::EVENT ?ev ?type :AFFECTED ?!obj :METHOD ?!bmo :PASSIVE +)
	   (ONT::F ?!bmo ONT::BY-MEANS-OF :VAL ?!bmoVal)
	   (ONT::EVENT ?!bmoVal ?!bmoValType :AFFECTED - :AFFECTED-RESULT -)
           -bmo3>
           100
           (ONT::CC *1 ONT::BY-MEANS-OF
            :rule -bmo3
	    :FACTOR ?!bmoVal
            :OUTCOME ?ev
	    :TYPE ONT::BY-MEANS-OF
            )
	   (ONT::EVENT ?!bmoVal ?!bmoValType
	    :rule -bmo3-event
	    :AFFECTED ?!obj
	    )
          )

	  ((ONT::EVENT ?ev ?type :AFFECTED ?!obj :METHOD ?!bmo :PASSIVE +)
	   (ONT::F ?!bmo ONT::BY-MEANS-OF :GROUND ?!bmoVal)
	   (ONT::EVENT ?!bmoVal ?!bmoValType :AFFECTED - :AFFECTED-RESULT -)
           -bmo3-gd>
           100
           (ONT::CC *1 ONT::BY-MEANS-OF
            :rule -bmo3-gd
	    :FACTOR ?!bmoVal
            :OUTCOME ?ev
	    :TYPE ONT::BY-MEANS-OF
            )
	   (ONT::EVENT ?!bmoVal ?!bmoValType
	    :rule -bmo3-event-gd
	    :AFFECTED ?!obj
	    )
          )

	  
					; should have a rule that generates AFFECTED-RESULT too
	  ; Raf is activated by phosphorylation.
	  ; Raf is activated by phosphorylation by Ras.
	  ((ONT::EVENT ?ev ?type :AFFECTED ?!obj :AGENT ?!bmo :PASSIVE +)
	   (ONT::EVENT ?!bmo ?!bmoValType :AFFECTED - :AFFECTED-RESULT -)
           -bmo4>
           100
	   (ONT::EVENT ?!bmo ?!bmoValType
	    :rule -bmo4
	    :AFFECTED ?!obj
	    )
          )

	  #| ; don't want this rule because it fires for "Raf is activated by Ras phosphorylation." and make Raf the AGENT of phosphorylation
	  ; Raf is activated by binding to Ras.  ("by" goes to :AGENT instead of :METHOD; no need to extract a CC)
	  ((EVENT ?ev ?type :AFFECTED ?!obj :AGENT ?!bmoVal :PASSIVE +)
	   (EVENT ?!bmoVal ?!bmoValType :AGENT - :AFFECTED ?!bmoAff)
           -bmo5>
           100
	   (EVENT ?!bmoVal ?!bmoValType
	    :rule -bmo5
	    :AGENT ?!obj
	    )
          )
	  |#

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; because ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  ; Because Ras activates Raf, Erk is stimulated.
          ((ONT::EVENT ?ev ?type :REASON ?!r)
	   (ONT::F ?!r ONT::REASON :VAL ?!rVal)
	   (ONT::EVENT ?!rVal ?!rValType)
           -reason>
           100
           (ONT::CC *1 ONT::CAUSE
            :rule -reason
	    :FACTOR ?!rVal
            :OUTCOME ?ev
	    :TYPE ONT::REASON
            )
          )

          ((ONT::EVENT ?ev ?type :REASON ?!r)   ; "because" is not FIGURE/GROUND yet, but it would be if it is to match "due to" etc below
	   (ONT::F ?!r ONT::REASON :GROUND ?!rVal)
	   (ONT::EVENT ?!rVal ?!rValType)
           -reason-gd>
           100
           (ONT::CC *1 ONT::CAUSE
            :rule -reason-gd
	    :FACTOR ?!rVal
            :OUTCOME ?ev
	    :TYPE ONT::REASON
            )
          )
	  
	  ; (DUE-TO is subtype of REASON; both -reason and -reason2 will fire in some cases)
	  ; The activation of Raf is due to the stimulation of Ras.  
          ((ONT::EVENT ?ev ?type)  ; note: all free variables
	   (ONT::F ?!r ONT::DUE-TO :OF ?ev :VAL ?!rVal)
	   (ONT::EVENT ?!rVal ?!rValType)
           -reason2>
           100
           (ONT::CC *1 ONT::CAUSE
            :rule -reason2
	    :FACTOR ?!rVal
            :OUTCOME ?ev
	    :TYPE ONT::DUE-TO
            )
          )

          ((ONT::EVENT ?ev ?type)  ; note: all free variables
	   (ONT::F ?!r ONT::DUE-TO :FIGURE ?ev :GROUND ?!rVal)
	   (ONT::EVENT ?!rVal ?!rValType)
           -reason2-gd>
           100
           (ONT::CC *1 ONT::CAUSE
            :rule -reason2-gd
	    :FACTOR ?!rVal
            :OUTCOME ?ev
	    :TYPE ONT::DUE-TO
            )
          )
	  
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; purpose ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  
	                             ;; Not true in general?  I sat down to watch tv.  I went to the store to buy bread.
	  ; Ras activates Raf to stimulate Erk.
          ((ONT::EVENT ?ev ?type :REASON ?!r)
	   (ONT::F ?!r ONT::PURPOSE :VAL ?!rVal)
	   (ONT::EVENT ?!rVal ?!rValType)
           -purpose>
           100
           (ONT::CC *1 ONT::BY-MEANS-OF
            :rule -purpose
	    :FACTOR ?ev
            :OUTCOME ?!rVal
	    :TYPE ONT::PURPOSE
            )
	   )

          ((ONT::EVENT ?ev ?type :REASON ?!r)
	   (ONT::F ?!r ONT::PURPOSE :GROUND ?!rVal)
	   (ONT::EVENT ?!rVal ?!rValType)
           -purpose-gd>
           100
           (ONT::CC *1 ONT::BY-MEANS-OF
            :rule -purpose-gd
	    :FACTOR ?ev
            :OUTCOME ?!rVal
	    :TYPE ONT::PURPOSE
            )
	   )

	  
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; therefore ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  ; Ras activates Raf, thus stimulating Erk.
	  ; Ras activates Raf so Erk is stimulated.
	  
	  #|
          ((EVENT ?ev ?type :AGENT ?!ag :REASON ?!r)
	   (ONT::F ?!r ONT::THEREFORE :VAL ?!rVal)
	   (EVENT ?!rVal ?!rValType :AGENT -)
           -therefore>
           100
           (CC *1 ONT::REASON
            :rule -therefore
	    :cause ?ev
            :effect ?!rVal
            )
	   (EVENT ?!rVal ?!rValType
	    :rule -therefore-event
	    :AGENT ?!ag
	    )	   
	   )
	  |#

          ((ONT::EVENT ?ev ?type :REASON ?!r)
	   (ONT::F ?!r ONT::THEREFORE :VAL ?!rVal)
	   (ONT::EVENT ?!rVal ?!rValType)
           -therefore>
           100
           (ONT::CC *1 ONT::CAUSE
            :rule -therefore
	    :FACTOR ?ev
            :OUTCOME ?!rVal
	    :TYPE ONT::THEREFORE
            )
	   )

          ((ONT::EVENT ?ev ?type :REASON ?!r)
	   (ONT::F ?!r ONT::THEREFORE :GROUND ?!rVal)
	   (ONT::EVENT ?!rVal ?!rValType)
           -therefore-gd>
           100
           (ONT::CC *1 ONT::CAUSE
            :rule -therefore-gd
	    :FACTOR ?ev
            :OUTCOME ?!rVal
	    :TYPE ONT::THEREFORE
            )
	   )
	  
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; result ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  
	  ; Raf is activated as a result of Ras stimulation.
          ((ONT::EVENT ?ev ?type :MANNER ?!r1)
	   (ONT::F ?!r1 ONT::MANNER :VAL ?!r2)
	   (ONT::TERM ?!r2 ONT::OUTCOME :OF ?!r3)   ; term substitution
	   (ONT::EVENT ?!r3 ?!r3Type)
           -result>
           100
           (ONT::CC *1 ONT::CAUSE    ; used to be ONT::RESULT
            :rule -result
	    :FACTOR ?!r3
            :OUTCOME ?ev
	    :TYPE ONT::OUTCOME
            )
	   )

          ((ONT::EVENT ?ev ?type :MANNER ?!r1)
	   (ONT::F ?!r1 ONT::MANNER :GROUND ?!r2)
	   (ONT::TERM ?!r2 ONT::OUTCOME :FIGURE ?!r3)   ; term substitution  ; (2016/06/13 :OF not :FIGURE) (2016/06/22: changed :OF to :FIGURE too)
	   (ONT::EVENT ?!r3 ?!r3Type)
           -result-gd>
           100
           (ONT::CC *1 ONT::CAUSE    ; used to be ONT::RESULT
            :rule -result-gd
	    :FACTOR ?!r3
            :OUTCOME ?ev
	    :TYPE ONT::OUTCOME
            )
	   )

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cause ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  ; Ras causes Raf to activate.
	  ; Ras causes Raf to be activated.
	  ; Ras causes Raf to activate Erk.
#|
          ((EVENT ?ev ONT::CAUSE :AGENT ?!ag :FORMAL ?!obj)
	   (EVENT ?!obj ?!objType :AGENT -)
           -cause>
           100
           (EVENT ?!obj ?!objType
            :rule -cause
	    :AGENT ?!ag
            )
          )
|#

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; in a Ras-(in)dependent manner ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  ; Raf activates Erk in a Ras-dependent manner
          ((ONT::EVENT ?ev ?type :MANNER ?!r1)
	   (ONT::F ?!r1 ONT::MANNER :VAL ?!r2)
	   (ONT::TERM ?!r2 ?t2 :MODS (?!r3))            ; need to fix ?t2 for "way", "fashion"
;	   (ONT::CC ?!r3 (? t3 ONT::DEPENDENT ONT::INDEPENDENT))     ; lost the NEUTRAL due to event substitution
	   (ONT::F ?!r3 (:* (? t3 ONT::DEPENDENT ONT::INDEPENDENT) ?!w3) :NEUTRAL1 ?!r4 :NEUTRAL ?!r2)
	   (ONT::TERM ?!r4 ?t4)
           (ONT::EVAL (symbolmap ?t3 ?!eventName -rule5b_NEUTRAL1_NEUTRAL))	  ; Note: there is a rule in drumrules_ev that generates this CC too.  Need to make sure the name of the CC stays the same. 
           -depend>
           100
           (ONT::CC ?!r3 ?!eventName
            :rule -depend
	    :FACTOR ?!r4
	    :OUTCOME ?ev
	    :NEUTRAL -
	    :NEUTRAL1 -
            )
	   )

          ((ONT::EVENT ?ev ?type :MANNER ?!r1)
	   (ONT::F ?!r1 ONT::MANNER :GROUND ?!r2)
	   (ONT::TERM ?!r2 ?t2 :MODS (?!r3))            ; need to fix ?t2 for "way", "fashion"
;	   (ONT::CC ?!r3 (? t3 ONT::DEPENDENT ONT::INDEPENDENT))     ; lost the NEUTRAL due to event substitution
	   (ONT::F ?!r3 (:* (? t3 ONT::DEPENDENT ONT::INDEPENDENT) ?!w3) :NEUTRAL1 ?!r4 :NEUTRAL ?!r2)
	   (ONT::TERM ?!r4 ?t4)
           (ONT::EVAL (symbolmap ?t3 ?!eventName -rule5b_NEUTRAL1_NEUTRAL))	  ; Note: there is a rule in drumrules_ev that generates this CC too.  Need to make sure the name of the CC stays the same. 
           -depend-gd>
           100
           (ONT::CC ?!r3 ?!eventName
            :rule -depend-gd
	    :FACTOR ?!r4
	    :OUTCOME ?ev
	    :NEUTRAL -
	    :NEUTRAL1 -
            )
	   )

	  
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dependent (was: necessary) and ensure (sufficient) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ; "Ras-dependent activation" is in DRUMRules_mod.lisp
	  
	  ; Ras activation is necessary for Raf stimulation.
	  ; Ras is necessary for Raf stimulation.
          ((ONT::F ?ev (:* (? type ONT::NECESSARY ONT::ADEQUATE) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj)  ; need the "(:* ...)" here because otherwise ?type gets maps to (:* ONT::NECESSARY W::NECESSARY)
	   ((? reln ONT::TERM ONT::EVENT) ?!ag ?!type1)
	   (ONT::EVENT ?!obj ?!type2)
	   (ONT::EVAL (symbolmap ?type ?!newtype))
           -necessary1>
           100
           (;ONT::CC *1 ?!newtype
	    ONT::CC ?ev ?!newtype    
            :rule -necessary1
	    :FACTOR ?!ag
            :OUTCOME ?!obj
	    :NEUTRAL -    ; zero out NEUTRAL/FORMAL so they wouldn't be emitted 
	    :FORMAL -
	    :TYPE ?type
            )
	   )

	  ;;;;;; note NECESSARY vs. NECESSITY

	  ; ??? Raf stimulation requires/necessitates Ras activation: Does this mean Ras activation of Raf, or Raf activation of itself?
	  ; The stimulation of Raf requires the activation of Ras
	  ; Raf stimulation requires/necessitates Ras.  (Note: "necessitate" is sometimes CAUSE-PRODUCE-REPRODUCE)
          ((ONT::F ?ev ONT::NECESSITY :NEUTRAL ?!ag :FORMAL ?!obj)
	   (ONT::EVENT ?!ag ?!type1)
	   ((? reln ONT::TERM ONT::EVENT) ?!obj ?!type2)
           -necessary2>
           100
           (;ONT::CC *1 ONT::DEPENDENT    
	    ONT::CC ?ev ONT::DEPENDENT    
            :rule -necessary2
	    :FACTOR ?!obj
            :OUTCOME ?!ag
	    :NEUTRAL -    ; zero out NEUTRAL/FORMAL so they wouldn't be emitted 
	    :FORMAL -
	    :TYPE ONT::NECESSITY
            )
	   )

#|
					; Ras requires Raf to activate
          ((ONT::F ?ev ONT::NECESSITY :NEUTRAL ?!v1 :FORMAL ?!v2)
	   ((? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!v1 (? t1 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT))
	   ((? reln2 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!v2 (? t2 ONT::MUTATION ONT::BIOLOGICAL-PROCESS ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::PHYS-OBJECT ONT::MENTAL-CONSTRUCTION ONT::AWARENESS ONT::INFORMATION-FUNCTION-OBJECT) :MODS (?!ev2))
	   (EVENT ?!ev2 ?!type2)
           -necessary2b>
           100
           (CC *1 ONT::ENABLE    
            :rule -necessary2b
	    :NEUTRAL ?!ev2
            :NEUTRAL1 ?!v1
            )
	   )
|#

	  
	  ; Ras activation is required for Raf stimulation.
	  ; Ras is required for Raf stimulation.
          ((ONT::F ?ev ONT::NECESSITY :FORMAL ?!obj :REASON ?!r)
	   ((? sp ONT::EVENT ONT::TERM) ?!obj ?!type2)
	   (ONT::F ?!r ONT::PURPOSE :VAL ?!rVal)
	   (ONT::EVENT ?!rVal ?!rValType)
           -necessary3>
           100
           (;ONT::CC *1 ONT::DEPENDENT
	    ONT::CC ?ev ONT::DEPENDENT    
            :rule -necessary3
	    :FACTOR ?!obj
            :OUTCOME ?!rVal
	    :FORMAL -
	    :REASON -
	    :TYPE ONT::NECESSITY
            )
	   )

          ((ONT::F ?ev ONT::NECESSITY :FORMAL ?!obj :REASON ?!r)  ; This doesn't parse but let's assume it would be GROUND
	   ((? sp ONT::EVENT ONT::TERM) ?!obj ?!type2)
	   (ONT::F ?!r ONT::PURPOSE :GROUND ?!rVal)
	   (ONT::EVENT ?!rVal ?!rValType)
           -necessary3-gd>
           100
           (;ONT::CC *1 ONT::DEPENDENT
	    ONT::CC ?ev ONT::DEPENDENT    
            :rule -necessary3-gd
	    :FACTOR ?!obj
            :OUTCOME ?!rVal
	    :FORMAL -
	    :REASON -
	    :TYPE ONT::NECESSITY
            )
	   )
	  
	  ;;;;;
	  ; "Ras activation is required by Raf for Erk stimulation." doesn't parse very well---which is good!  We don't know what we want to extract
	  ;;;;;
	  
	  ; Ras/Ras stimulation suffices for the activation of Raf.
	  ; Ras/Ras stimulation suffices to activate Raf.
          ((ONT::F ?ev ONT::SUFFICIENCY :NEUTRAL ?!obj :REASON ?!r)
	   ((? reln ONT::TERM ONT::EVENT) ?!obj ?!type2)
	   (ONT::F ?!r ONT::PURPOSE :VAL ?!rVal)
	   (ONT::EVENT ?!rVal ?!rValType)
           -sufficient>
           100
           (;ONT::CC *1 ONT::ENSURE
	    ONT::CC ?ev ONT::ENSURE    
            :rule -sufficent
	    :FACTOR ?!obj
            :OUTCOME ?!rVal
	    :NEUTRAL -
	    :REASON -
	    :TYPE ONT::SUFFICIENCY
            )
	   )
	  
          ((ONT::F ?ev ONT::SUFFICIENCY :NEUTRAL ?!obj :REASON ?!r)
	   ((? reln ONT::TERM ONT::EVENT) ?!obj ?!type2)
	   (ONT::F ?!r ONT::PURPOSE :GROUND ?!rVal)
	   (ONT::EVENT ?!rVal ?!rValType)
           -sufficient-gd>
           100
           (;ONT::CC *1 ONT::ENSURE
	    ONT::CC ?ev ONT::ENSURE    
            :rule -sufficent-gd
	    :FACTOR ?!obj
            :OUTCOME ?!rVal
	    :NEUTRAL -
	    :REASON -
	    :TYPE ONT::SUFFICIENCY
            )
	   )
	  
	  )
	)

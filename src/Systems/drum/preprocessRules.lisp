(in-package "IM")

(reset-im-rules 'preprocessRules)

(mapcar #'(lambda (x) (add-im-rule x 'preprocessRules))  ;; sets of rules are tagged so they can be managed independently 
	'(
	  ((?reln ?!obj ?t
	    )
	   -add-spec>
	   100
	   (?reln ?!obj ?t
	    :spec ?reln
	    :ONT ?t  ; TRIPS type
	    :rule -add-spec
	    ))

	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ; bypass STATE-RESULTING-FROM
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  ((?reln ?!obj ?t :MODS (?!mod ?mod2))
	   (?reln1 ?mod ONT::STATE-RESULTING-FROM :FIGURE ?!obj :GROUND ?!ev)
	   (?reln1b ?!ev ?t1b)
	   -state-resulting-from>
	   100
	   (?reln ?!obj ?t
	    :MOD ?!ev
	    :MOD ?mod2
	    :ONT ?t
	    :spec ?reln ; since -add-spec> wouldn't be used
	    :rule -state-resulting-from
	    ))
	  
))

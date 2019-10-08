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
	    :rule -state-resulting-from
	    ))
	  
))

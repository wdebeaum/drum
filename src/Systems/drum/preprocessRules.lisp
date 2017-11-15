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
))

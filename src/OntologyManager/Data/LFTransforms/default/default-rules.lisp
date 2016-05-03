;; swift -- still need these to pass through roles and modifiers for main terms. This should be handled in the code the way main type transforms are, but the existing transform mapping code is complicated enough to make keeping this file simpler for now...

(in-package :om)

;; ----------------------------------------

(define-transform roles
  :abstract t
  :typevar ?vv
  :typetransform (ONT::SITUATION-ROOT -> ?)
  :argtransforms (((:agent ?a) -> (ont::agent ?vv ?a))
		  ((:cognizer ?cg) -> (ont::cognizer ?vv ?cg))
		  ((:experiencer ?ex) -> (ont::experiencer ?vv ?ex))
		  ((:theme ?t) -> (ont::theme ?vv ?t))
		  ((:co-theme ?c) -> (ont::co-theme ?vv ?c))
		  ((:source ?s) -> (ont::source ?vv ?s))
		  ((:goal ?g) -> (ont::goal ?vv ?g))
		  ((:recipient ?r) -> (ont::recipient ?vv ?r))
		  ((:beneficiary ?b) -> (ont::beneficiary ?vv ?b))
		  ((:mods ?m) -> (ont::mods ?vv ?m))
		  ((:assoc-with ?aw) -> (ont::assoc-with ?vv ?aw))
		  )
  )

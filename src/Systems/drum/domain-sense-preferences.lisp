;;  the domain specific sense preferences


(setf lxm::*domain-sense-preferences* '((w::expression ont::gene-expression)
				       (W::TRANSCRIPT ONT::RNA)
				       (W::SITE ONT::MOLECULAR-SITE)
				       (W::POSITION ONT::MOLECULAR-SITE)
				       (W::complex ONT::MACROMOLECULAR-COMPLEX)
				       ;(W::RAS ONT::PROTEIN-FAMILY )
				       (W::ASSOCIATE ONT::ATTACH)
				       (W::ASSOCIATION ONT::ATTACH)
				       (W::INTERACT ONT::BIND-INTERACT)
				       (W::interaction ONT::BIND-INTERACT)
				       (W::FDA ONT::PROFESSIONAL-ORGANIZATION) ; as opposed to PROTEIN
				       ))



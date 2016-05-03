(in-package W)

(parser::add-ontology-hierarchical-features '(SUBCATSEM SEM role class))

(parser::add-syntax-hierarchical-features '(VFORM SA TYPE ctype sort))

(parser::add-syntax-to-type-hierarchy
 '(DUMMY))

(parser::add-syntax-to-type-hierarchy
 '(any-vform
   (fin (pres) (past) (fut))
   (nonfin (base) (inf) (pastpart) (ing) (passive))
   ))

(parser::add-syntax-to-type-hierarchy
 '(any-status
   (definite
     (definite-plural))
   (indefinite
     (indefinite-plural))
   (quantifier)
   (pro
     (pro-set))
   (wh
    (how-many) (how-much))
   )
 )

(parser::add-syntax-to-type-hierarchy
 '(any-sort
   (individual) (set) (pred) (kind)
   (adj) (adj-val) (descr) (wh-desc)
   (unit-measure 
    (attribute-unit)
    (aggregate-unit 
     (substance-unit) (classifier))
    ))
 )

(parser::add-syntax-to-type-hierarchy
 '(clause-type
   (s-finite
    (rel-whose)
    (s-that (s-that-overt) (s-that-missing))
    (s-if))
   (s-infinite
    (s-to) (s-and))
   (s-relc)
   (s-from-ing)
   (s-that-subjunctive)
   ))
    
(parser::add-syntax-to-type-hierarchy
 '(reln
   (other-reln)
   (kinship-reln)
   (body-part-reln)
   (part-of-reln)
   (gen-part-of-reln)
   (drv-nom-reln)
   ))

(parser::add-syntax-to-type-hierarchy
 '(any-atype
   (attributive-only)
   (predicative-only)
   (central)
   (postpositive)
   ))


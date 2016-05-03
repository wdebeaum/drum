# aggregates all the lisp ontologies into a single file (in the right order)
ONT=../OntologyManager/Data/LFdata

cat $ONT/feature-declarations.lisp $ONT/feature-types.lisp $ONT/root-types.lisp $ONT/speech-acts.lisp $ONT/predicates.lisp $ONT/time-location-types.lisp $ONT/physobj.lisp $ONT/situation-types.lisp $ONT/abstract-types.lisp $ONT/specific-situation-types.lisp > ontology.lisp

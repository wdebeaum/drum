#
# Makefile for OWL for Trips ontology
#

MODULE	= OWL
PACKAGE = TRIPS.OWL

SRCS = AkrlAsserter.java \
	Duration.java \
	InvalidFeatureValueException.java \
	NotAnOntObjectException.java \
	OntFeature.java \
	OntFeatureListType.java \
	OntFeatureValue.java \
	OntIndividual.java \
	OntNameClashException.java \
	OntNamedFeatureListType.java \
	OntNamedFeatureValue.java \
	OntObject.java \
	OntProperty.java \
	OntType.java \
	OntUnionFeatureListType.java \
	OntUnionFeatureValue.java \
	OwlUtils.java \
	Stopwatch.java \
	TripsOntology.java \
	Utils.java \
	testAssert.java \
	lispont/DefineFeature.java \
	lispont/DefineFeatureArgument.java \
	lispont/DefineFeatureListType.java \
	lispont/DefineFeatureRule.java \
	lispont/DefineType.java \
	lispont/FeatureVector.java \
	lispont/LispOntology.java \
	lispont/OntFeatureListTypeRef.java \
	lispont/OntFeatureRef.java \
	lispont/OntFeatureValueRef.java \
	lispont/OntNamedFeatureListTypeRef.java \
	lispont/OntNamedFeatureValueRef.java \
	lispont/OntObjectRef.java \
	lispont/OntPropertyRef.java \
	lispont/OntTypeRef.java \
	lispont/OntUnionFeatureListTypeRef.java \
	lispont/OntUnionFeatureValueRef.java \
	lispont/TripsOntologyExporter.java \
	lispont/TypeArgument.java \
	lispont/convertTripsOntology.java \
	sexp/SExpression.java \
	sexp/SExpressionAtom.java \
	sexp/SExpressionList.java \
	sexp/SExpressionPrettyPrinter.java \
	sexp/SExpressionReader.java \
	sexp/SExpressionSymbol.java 

CLASSES = $(SRCS:.java=.class) \
	TripsOntology$$StaticResources.class \
	sexp/SExpressionSymbol$$1.class \
	sexp/SExpressionSymbol$$PackageReferenceType.class

CONFIGDIR=../config
include defs.mk

JFLAGS = -classpath .:$(PROTEGE_BASE_CP):$(PROTEGE_LIB_CP):$(PROTEGE_INFERENCE_CP)

include $(CONFIGDIR)/java/lib.mk

# stuff to compile the TRIPS ontology to OWL and install the file to etcdir
LISPONT:=../OntologyManager/Data/LFdata

ontology.lisp:: $(LISPONT)/feature-declarations.lisp $(LISPONT)/feature-types.lisp $(LISPONT)/root-types.lisp $(LISPONT)/speech-acts.lisp $(LISPONT)/predicates.lisp $(LISPONT)/time-location-types.lisp $(LISPONT)/physobj.lisp $(LISPONT)/situation-types.lisp $(LISPONT)/abstract-types.lisp $(LISPONT)/specific-situation-types.lisp
	cat $(LISPONT)/feature-declarations.lisp $(LISPONT)/feature-types.lisp $(LISPONT)/root-types.lisp $(LISPONT)/speech-acts.lisp $(LISPONT)/predicates.lisp $(LISPONT)/time-location-types.lisp $(LISPONT)/physobj.lisp $(LISPONT)/situation-types.lisp $(LISPONT)/abstract-types.lisp $(LISPONT)/specific-situation-types.lisp \
	| fgrep -i -v ':comment "' \
	> ontology.lisp
	# ^^^ hack around the fact that sexp lib can't read :comment strings
	# properly

# we don't make the jar a dependency, because that forces a recompile of ontology.owl
# every time some java file changes.  with the make jar statement, we at least make sure
# that if we compile the owl file, we are doing it with the latest code
ontology.owl:: ontology.lisp coreOntology.owl
	make jar
	./convertTripsOntology.sh ontology.lisp ontology.owl

all:: ontology.owl

install:: ontology.owl
	$(MKINSTALLDIRS) $(etcdir)/OWL
	$(INSTALL_DATA) ontology.owl $(etcdir)/OWL
	$(INSTALL_DATA) ontology.pprj $(etcdir)/OWL
	$(INSTALL_DATA) log4j.properties $(etcdir)/OWL
	
clean::
	rm -f ontology.lisp ontology.owl

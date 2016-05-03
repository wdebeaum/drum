
CONFIGDIR=../config
include $(CONFIGDIR)/protege/defs.mk

PROTEGE_BASE_JARS = \
	protege.jar \
	looks.jar

PROTEGE_LIB = $(PROTEGE_BASE)/plugins/edu.stanford.smi.protegex.owl
PROTEGE_LIB_JARS = \
	antlr-2.7.5.jar \
	arq-extra.jar \
	arq.jar \
	axis.jar \
	commons-discovery-0.4.jar \
	commons-lang-2.0.jar \
	commons-logging-1.1.1.jar \
	concurrent.jar \
	edtftpj-1.5.2.jar \
	ekitspell.jar \
	icu4j_3_4.jar \
	iri.jar \
	jcalendar.jar \
	jdom.jar \
	jena.jar \
	jep-2.4.0.jar \
	json.jar \
	junit.jar \
	kazuki.jar \
	log4j-1.2.12.jar \
	lucene-core-2.3.1.jar \
	orphanNodesAlg.jar \
	owlsyntax.jar \
	protege-owl.jar \
	stax-api-1.0.jar \
	swrl-jess-bridge.jar \
	wstx-asl-3.0.0.jar \
	xercesImpl.jar \
	xml-apis.jar

PROTEGE_INFERENCE = $(PROTEGE_BASE)/plugins/edu.stanford.smi.protegex.owl.inference.pellet
PROTEGE_INFERENCE_JARS = \
	aterm-java-1.6.jar \
	commons-lang-2.2.jar \
	owlapi-api.jar \
	owlapi-apibinding.jar \
	owlapi-change.jar \
	owlapi-debugging.jar \
	owlapi-dig1_1.jar \
	owlapi-functionalparser.jar \
	owlapi-functionalrenderer.jar \
	owlapi-impl.jar \
	owlapi-krssparser.jar \
	owlapi-metrics.jar \
	owlapi-oboparser.jar \
	owlapi-owlxmlparser.jar \
	owlapi-owlxmlrenderer.jar \
	owlapi-rdfapi.jar \
	owlapi-rdfxmlparser.jar \
	owlapi-rdfxmlrenderer.jar \
	owlapi-util.jar \
	pellet.jar \
	protege-pellet.jar \
	relaxngDatatype.jar \
	xsdlib.jar

empty:=
space:= $(empty) $(empty)

# add path prefixes to classpaths
PROTEGE_BASE_CP = $(subst $(space),:,$(addprefix $(PROTEGE_BASE)/,$(PROTEGE_BASE_JARS)))
PROTEGE_LIB_CP = $(subst $(space),:,$(addprefix $(PROTEGE_LIB)/,$(PROTEGE_LIB_JARS)))
PROTEGE_INFERENCE_CP = $(subst $(space),:,$(addprefix $(PROTEGE_INFERENCE)/,$(PROTEGE_INFERENCE_JARS)))


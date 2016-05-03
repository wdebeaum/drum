#!/bin/sh

# Get PROTEGE_BASE from configure
. ../config/protege/defs.sh

PLIB=${PROTEGE_BASE}/plugins/edu.stanford.smi.protegex.owl
PRLIB=${PLIB}.inference.pellet

pelletcp=${PRLIB}/aterm-java-1.6.jar:${PRLIB}/commons-lang-2.2.jar:${PRLIB}/owlapi-api.jar:${PRLIB}/owlapi-apibinding.jar:${PRLIB}/owlapi-change.jar:${PRLIB}/owlapi-debugging.jar:${PRLIB}/owlapi-dig1_1.jar:${PRLIB}/owlapi-functionalparser.jar:${PRLIB}/owlapi-functionalrenderer.jar:${PRLIB}/owlapi-impl.jar:${PRLIB}/owlapi-krssparser.jar:${PRLIB}/owlapi-metrics.jar:${PRLIB}/owlapi-oboparser.jar:${PRLIB}/owlapi-owlxmlparser.jar:${PRLIB}/owlapi-owlxmlrenderer.jar:${PRLIB}/owlapi-rdfapi.jar:${PRLIB}/owlapi-rdfxmlparser.jar:${PRLIB}/owlapi-rdfxmlrenderer.jar:${PRLIB}/owlapi-util.jar:${PRLIB}/pellet.jar:${PRLIB}/plugin.properties:${PRLIB}/protege-pellet.jar:${PRLIB}/relaxngDatatype.jar:${PRLIB}/versions.txt:${PRLIB}/xsdlib.jar

owlcp=${PLIB}/antlr-2.7.5.jar:${PLIB}/edtftpj-1.5.2.jar:${PLIB}/jena.jar:${PLIB}/lucene-core-2.3.1.jar:${PLIB}/wstx-asl-3.0.0.jar:${PLIB}/arq-extra.jar:${PLIB}/ekitspell.jar:${PLIB}/jep-2.4.0.jar:${PLIB}/orphanNodesAlg.jar:${PLIB}/xercesImpl.jar:${PLIB}/arq.jar:${PLIB}/icu4j_3_4.jar:${PLIB}/json.jar:${PLIB}/owlsyntax.jar:${PLIB}/xml-apis.jar:${PLIB}/commons-lang-2.0.jar:${PLIB}/iri.jar:${PLIB}/junit.jar:${PLIB}/protege-owl.jar:${PLIB}/commons-logging-1.1.1.jar:${PLIB}/jcalendar.jar:${PLIB}/kazuki.jar:${PLIB}/stax-api-1.0.jar:${PLIB}/concurrent.jar:${PLIB}/jdom.jar:${PLIB}/log4j-1.2.12.jar:${PLIB}/swrl-jess-bridge.jar

cp=.:${PROTEGE_BASE}/protege.jar:${PROTEGE_BASE}/looks.jar:${owlcp}:${pelletcp}

java -Xmx1g -cp ${cp} -Dprotege.dir=${PROTEGE_BASE} -Djava.util.logging.config.file=log4j.properties TRIPS.OWL.lispont.convertTripsOntology $@

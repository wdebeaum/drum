#
# Makefile
#
# Time-stamp: <Fri Jan  6 18:56:06 CST 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>,  9 Feb 2010
#

MAIN    = DrumGUI
PACKAGE = TRIPS.DrumGUI
USES    = TRIPS.TripsModule TRIPS.KQML TRIPS.util

SUBDIRS = 

SRCS =  DrumGUI.java \
	Log.java \
	Debug.java \
	DataSet.java \
	Display.java \
	SelectorPanel.java \
	TextPanel.java \
	ExtractorPanel.java \
	ExtractionTree.java \
	TagOptionsInput.java \
	DrumKB.java \
	Extraction.java \
	ExtractionFactory.java \
	TermExtraction.java \
	EventExtraction.java \
	ModalityExtraction.java \
	EpistemicModalityExtraction.java \
	CausalityExtraction.java

CLASSES = $(SRCS:.java=.class) \
	DrumGUI$$Mode.class \
	DrumGUI$$PubPullReplyHandler.class \
	DrumGUI$$TagReplyHandler.class \
	DrumGUI$$RunTask.class \
	DrumGUI$$TaskScheduler.class \
	Log$$TimeStamp.class \
	Debug$$Level.class \
	Display$$1.class \
	Display$$State.class \
	SelectorPanel$$1.class \
	SelectorPanel$$2.class \
	TextPanel$$1.class \
	ExtractorPanel$$Mode.class \
	ExtractionTree$$1.class \
	DrumKB$$Paragraph.class \
	DrumKB$$Sentence.class \
	TermExtraction$$Attribute.class \
	TermExtraction$$PolyAttribute.class \
	EventExtraction$$Feature.class \
	EventExtraction$$Modifier.class \
	EventExtraction$$PolyModifier.class \
	EventExtraction$$Role.class \
	ModalityExtraction$$Modifier.class \
	ModalityExtraction$$PolyModifier.class \
	ModalityExtraction$$Role.class \
	EpistemicModalityExtraction$$Modifier.class \
	EpistemicModalityExtraction$$PolyModifier.class \
	EpistemicModalityExtraction$$Role.class \
	CausalityExtraction$$Modifier.class \
	CausalityExtraction$$PolyModifier.class \
	CausalityExtraction$$Role.class

XTRA =  images/ihmcLogo.png \
	images/folder.png \
	images/file.png \
	images/zoomin.png \
	images/zoomout.png \
	images/delete.png \
	images/edit.png \
	images/gear.png \
	images/tag.png \
	images/list.png \
	images/load.png \
	images/plus.png \
	images/x.png \
	images/play.png

CONFIGDIR=../config
include $(CONFIGDIR)/java/prog.mk

all default install clean::
	@for d in $(SUBDIRS); do \
	  if test -d $$d; then \
	    (cd $$d; $(MAKE) $@); \
	  fi; \
	done

etags:
	etags --members ${SRCS}

## APP
APP_DIR = gui_app
APP-JARS = $(addprefix ${APP_DIR}/etc/java/,$(addsuffix .jar,${USES})) ${APP_DIR}/etc/java/${PACKAGE}.jar

app: ${APP_DIR}/${MAIN} ${APP-JARS}

${APP_DIR}/${MAIN}: all ${APP_DIR}
	cat ${MAIN} \
	| sed 's,\(TRIPS_BASE_DEFAULT=\).*,\1`pwd`,' \
	| sed 's,\($$JFLAGS\),\"\1\",' \
	> ${APP_DIR}/${MAIN}
	chmod +x ${APP_DIR}/${MAIN}

${APP_DIR}/etc/java/${PACKAGE}.jar: ${PACKAGE}.jar ${APP_DIR}
	cp $< $@

${APP_DIR}/etc/java/%.jar: TRIPS/../etc/java/%.jar ${APP_DIR}/etc/java
	cp $< $@

${APP_DIR}/etc/java: ${APP_DIR}
	-mkdir -p $@

${APP_DIR}:
	-mkdir -p $@


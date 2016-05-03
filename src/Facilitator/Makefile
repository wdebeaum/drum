#
# Makefile for Java Facilitator (formerly Input Manager)
#
# David Costello, costello@cs.rochester.edu,  8 Jul 1999
# $Id: Makefile,v 1.2 2012/08/11 03:31:22 iperera Exp $
#

MAIN    = Facilitator
PACKAGE = TRIPS.Facilitator
USES    = TRIPS.KQML TRIPS.util

SRCS = ClientGroup.java \
	ColumnLayout.java \
	Debug.java \
	Facilitator.java \
	FacilitatorDisplay.java \
	FlashLED.java \
	Launcher.java \
	LauncherException.java \
	LED.java \
	Log.java \
	ModemDisplay.java \
	NullDisplay.java \
	Orientation.java \
	ProcessClient.java \
	Registry.java \
	Sendable.java \
	SocketClient.java \
	StderrReader.java \
	StdinClient.java \
	Subscription.java \
	SubscriptionManager.java \
	SubscriptionPattern.java \
	TrafficViewer.java \
	TTYDisplay.java
CLASSES = $(SRCS:.java=.class) \
	Assert.class \
	ClientRecord.class \
	HistoryEntry.class \
	Launcher$$1.class \
	MatchAnythingPattern.class \
	MatchConjunctionPattern.class \
	MatchDisjunctionPattern.class \
	MatchGeneralPattern.class \
	MatchNothingPattern.class \
	MatchParameterExistsPattern.class \
	MatchParameterHeadPattern.class \
	MatchParameterStringPattern.class \
	MatchSenderPattern.class \
	MatchVerbPattern.class \
	ModemDisplayEntry.class \
	RegistryEntry.class \
	Subscription.class \
	SubscriptionPattern.class \
	TimeStamper.class
XTRA = images/LEDgreen.gif \
	images/LEDoff.gif \
	images/LEDred.gif \
	images/LEDyellow.gif

CONFIGDIR=../config
include $(CONFIGDIR)/java/prog.mk

test::
	(sleep 8; cd ../KeyboardManager; make run) &
	(sleep 8; cd ../SpeechController; make run) &
	make run

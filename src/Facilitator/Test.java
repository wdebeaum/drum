/*
 * Test.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 11 Jun 2001
 * $Id: Test.java,v 1.1.1.1 2005/01/14 19:48:08 ferguson Exp $
 */

import TRIPS.TripsModule.StandardTripsModule;

class TestModule extends StandardTripsModule {
    public TestModule(String[] argv, boolean isApplication) {
	super(argv, isApplication);
    }
}

public class Test {
    public static void main(String[] argv) {
	System.err.println("Test.main: launching modules");
	for (int i=0; i < 3; i++) {
	    String[] moduleArgv = new String[2];
	    moduleArgv[0] = "-name";
	    moduleArgv[1] = "MODULE-" + i;
	    System.err.println("Test.main: starting module: " + moduleArgv[1]);
	    new Thread(new TestModule(moduleArgv, false)).start();
	}
	System.err.println("Test.main: done");
    }
}

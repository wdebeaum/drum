package TRIPS.OWL.lispont;

import java.util.*;
import java.io.*;

import TRIPS.OWL.*;

/**
 * this converts an ontology from Lisp in Trips to OWL
 */
public class convertTripsOntology {

    protected void mainProgram(List<String> args) {

	String defsFilename = args.remove(0);
	String outputFilename = args.remove(0);

	TripsOntology ontology = TripsOntologyExporter.fromLispOntologyFile(defsFilename);
	ontology.writeOWL(outputFilename);
	System.out.println("wrote file " + outputFilename);

    } // end mainProgram

    public static void main(String[] args) {
	try {
	if (args.length != 2) {
	    System.err.println("convertTripsOntology <deftype file> <output file>");
	    System.exit(1);
	}

	convertTripsOntology bob = new convertTripsOntology();
	bob.mainProgram(new ArrayList<String>(Arrays.asList(args)));

	} catch (Throwable e) {
	    System.err.println("exception percolated to the top");
	    e.printStackTrace();
	}
	
    } // end main

} // end convertTripsOntology
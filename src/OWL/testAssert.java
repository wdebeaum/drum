package TRIPS.OWL;

import java.util.*;
import java.io.*;

import TRIPS.KQML.*;
import TRIPS.AKRL.*;

import edu.stanford.smi.protegex.owl.model.*;
import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
import edu.stanford.smi.protegex.owl.*;
import edu.stanford.smi.protegex.owl.inference.reasoner.ProtegeReasoner;
import edu.stanford.smi.protegex.owl.inference.protegeowl.ReasonerManager;
import edu.stanford.smi.protegex.owl.inference.pellet.ProtegePelletJenaReasoner;
import com.hp.hpl.jena.util.FileUtils;


/**
 * this converts an ontology from Lisp in Trips to OWL
 */
public class testAssert {

    protected void mainProgram(List<String> args) {
	try {
	String inputFilename = args.remove(0);
	String parsedFilename = args.remove(0);
	String outputFilename = args.remove(0);

	System.err.println("loading ontology....");
	TripsOntology ont = TripsOntology.fromFile(inputFilename);
	System.err.println("ontology loaded");

	System.err.println("loading parsed file....");
	List<AkrlBundle> bundles = readInBundles(parsedFilename);
	System.err.println("parsed file loaded");

	// assert each bundle
	for (AkrlBundle bundle : bundles) {
	    AkrlAsserter.assertAkrlBundle(bundle);
	}
	ont.writeOWL(outputFilename);

	List<String> nameClashes = new ArrayList<String>(AkrlAsserter.nameClashes);
	Collections.sort(nameClashes);

	System.out.println("\nProperties that didn't exist in the ontology: " 
			   + AkrlAsserter.createdProperties);
	System.out.println("\n(Major) types that didn't exist in the ontology: "
			   + AkrlAsserter.createdTypes);
	System.out.println("\nIndividuals that didn't exist in the ontology: " 
			   + AkrlAsserter.createdIndividuals);
	System.out.println("\nName clashes (both individuals and types): " 
			   + nameClashes);
	System.out.println("\nPossible name clashes. Non-gensym types used as inline slot values: "
			   + nameClashes);
	System.out.println("\nGensyms that were never defined: "
			   + AkrlAsserter.createdGensyms);

	} catch (Exception e) {
	    throw new RuntimeException("top",e);
	}
    } // end mainProgram

    // stuff for reading in an NGA parsed file for testing

    /**
     * used to separate recors in the text file
     */
    public static final String SEPARATOR_LINE = "====";


    /**
     * reads in a group of bundles from a parsed file
     */
    public List<AkrlBundle> readInBundles(String filename) {
	List<AkrlBundle> retval;
	BufferedReader in;
	try {
        in = new BufferedReader(new FileReader(filename));

	String line = null;

	// read header info
	String header = in.readLine();
	// 	    String[] headerList = header.split(" ");
	// 	    String sessionName = headerList[0];
	// 	    PursuitCorpus.CarPosition carPosition =
	// 		PursuitCorpus.CarPosition.getValue(headerList[1]);
	// 	    int start = Integer.parseInt(headerList[2]);

	retval = new ArrayList<AkrlBundle>();

	// read in one utterance at a time
	while ((line = in.readLine()) != null) {
	    String id = line;

	    line = in.readLine();
	    String words = line;

	    line = in.readLine();
	    String startTime = line;

	    line = in.readLine();
	    String endTime = line;

	    line = in.readLine();
	    String logicalFormStr = line;

	    line = in.readLine(); // the separator line
	    if (!line.equals(SEPARATOR_LINE)) {
		throw new RuntimeException("error reading in file: " + filename +
					   ", expected separator line but got: " + line);
	    }

	    KQMLList kqml;
	    try {
		kqml = KQMLList.fromString(logicalFormStr);
	    } catch (IOException e) {
		throw new RuntimeException("error trying to read in KQML string for utt " +
					   id + "\n" + logicalFormStr,e);
	    }

	    AkrlBundle thisBundle = AkrlBundle.fromLFParagraphList(kqml);
	    retval.add(thisBundle);		
	} // end while 

	in.close();

	} catch(Exception e) {
	    throw new RuntimeException("problem reading in file: " + filename,e);
	}

	//System.err.println("read:\n" + retval);

	return retval;
    } // end fromTextFile


    public static void main(String[] args) {
	if (args.length != 3) {
	    System.err.println("testAssert <input ontology> <parsed file> <output ontology>");
	    System.exit(1);
	}
	new testAssert().mainProgram(new ArrayList<String>(Arrays.asList(args)));
    } // end main



} // end testAssert
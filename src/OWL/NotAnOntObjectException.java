package TRIPS.OWL;

/**
 * this signals that an RDFResource is not a type (such as OWLSomeValuesFrom)
 * that we represent in the TripsOntology directly (as a first class object that
 * is a descendant of OntObject)
 */
public class NotAnOntObjectException extends RuntimeException {
    public static final long serialVersionUID = 1;

    public NotAnOntObjectException() {
	super();
    }

    public NotAnOntObjectException(String mesg) {
	super(mesg);
    }

}
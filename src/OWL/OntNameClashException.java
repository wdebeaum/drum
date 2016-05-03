package TRIPS.OWL;

import edu.stanford.smi.protegex.owl.model.*;

/**
 * this signals that we've tried to get or create a resource of one type (e.g., individual
 * but that it clashed with the name already existing as another type (e.g., Type
 */
public class OntNameClashException extends RuntimeException {
    public static final long serialVersionUID = 1;

    String name;
    RDFResource res;

    public OntNameClashException(String name, RDFResource res) {
	super();
	this.name = name;
	this.res = res;
    }

    public OntNameClashException(String name, RDFResource res, String mesg) {
	super(mesg);
	this.name = name;
	this.res = res;
    }

    public OntNameClashException(String name, OntObject obj, String mesg) {
	this(name,obj.getResource(),mesg);
    }

}
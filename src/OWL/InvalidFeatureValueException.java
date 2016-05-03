package TRIPS.OWL;

/**
 * this signals something wrong with a feature value
 */
public class InvalidFeatureValueException extends RuntimeException {
    public static final long serialVersionUID = 1;

    public InvalidFeatureValueException() {
	super();
    }

    public InvalidFeatureValueException(String mesg) {
	super(mesg);
    }

}
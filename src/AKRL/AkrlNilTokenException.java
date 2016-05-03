package TRIPS.AKRL;

/**
 * this helps with error reporting when we encounter NIL somewhere in something we're
 * converting to AKRL (which is an error)
 */
public class AkrlNilTokenException extends RuntimeException {
    public static final long serialVersionUID = 1;

    public AkrlNilTokenException() {
	super();
    }

    public AkrlNilTokenException(String mesg) {
	super(mesg);
    }
}
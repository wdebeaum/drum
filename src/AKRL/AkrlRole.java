package TRIPS.AKRL;

import TRIPS.KQML.*;
import java.util.*;
import java.io.*;

/**
 * this encapsulates a role for an AKRL term (i.e., the slot name)
*/
public class AkrlRole implements Serializable {
    protected static final long serialVersionUID = 2;

    /**
     * the bare name (without the keyword indicator ":")
     */
    protected String bareName;
    public String getBareName() {return bareName;}

    /**
     * create from a KQMLToken
     */
    public AkrlRole(KQMLToken roleName) {
	if (roleName.equalsIgnoreCase(KQMLToken.NIL))
	    throw new AkrlNilTokenException("attempt to create a NIL role");
	if (!roleName.isKeyword())
	    throw new RuntimeException("attempt to create a role with a token that isn't a keyword "
				       + roleName);
	bareName = roleName.getName().toLowerCase();
    } 

    public AkrlRole(String roleName) {
	if (roleName.startsWith(":"))
	    bareName = roleName.substring(1);
	else
	    bareName = roleName;
    }

    /**
     * returns the name with the ":" on the front
     */
    public String getKeywordName() {
	return ":" + getBareName();
    }

    @Override
    public boolean equals(Object o) {
	if (!(o instanceof AkrlRole))
	    return false;
	return getBareName().equalsIgnoreCase(((AkrlRole)o).getBareName());
    }

    @Override
    public int hashCode() {
	return getBareName().toUpperCase().hashCode();
    }

    @Override
    public String toString() {
	return getKeywordName();
    }

} // end class AkrlRole
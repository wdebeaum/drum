package TRIPS.util.cwc;

import TRIPS.KQML.*;

/** Static methods for dealing with KQML arguments.
 */
public interface Args {
  /** Turn the content Object given to a receive*() method into a
   * KQMLPerformative. Throw InvalidArgument if it can't be converted.
   */
  static KQMLPerformative getContentPerformative(KQMLPerformative msg, Object contentObj) throws InvalidArgument {
    if (!(contentObj instanceof KQMLList)) {
      throw new InvalidArgument(msg, ":content", "list");
    }
    try {
      return new KQMLPerformative((KQMLList)contentObj);
    } catch (KQMLBadPerformativeException e) {
      throw new InvalidArgument(msg, ":content", "list");
    }
  }

  // NOTE: Java won't let me genericize the following pairs of methods into one,
  // even though the implementations are (almost) identical, because it can't
  // figure out that KQMLPerformative goes with String and KQMLList goes with
  // int, and no other combinations are valid (I think).

  /** Get an argument from a performative that is required to be present and of
   * a specific type, and return it as that type. If it's missing, throw
   * MissingArgument. If it's of the wrong type, throw InvalidArgument.
   */
  static <T> T getTypedArgument(KQMLPerformative perf, String key, Class<T> t) throws MissingArgument, InvalidArgument {
    KQMLObject val = perf.getParameter(key);
    if (val == null) {
      throw new MissingArgument(perf.getVerb(), key);
    }
    if (t.equals(String.class)) { // special case for KQMLString->String
      if (val instanceof KQMLString) {
	return t.cast(((KQMLString)val).stringValue());
      } else {
	throw new InvalidArgument(perf, key, "KQMLString");
      }
    } else if (t.equals(KQMLPerformative.class)) {// special case for list->perf
      if (val instanceof KQMLList) {
	try {
	  return t.cast(new KQMLPerformative((KQMLList)val));
	} catch (KQMLBadPerformativeException e) {
	  throw new InvalidArgument(perf, key, "KQMLPerformative");
	}
      } else {
	throw new InvalidArgument(perf, key, "KQMLPerformative");
      }
    } else if (t.equals(Integer.class)) {// special case for KQMLToken->Integer
      if (val instanceof KQMLToken) {
	try {
	  return t.cast(new Integer(val.toString()));
	} catch (NumberFormatException e) {
	  throw new InvalidArgument(perf, key, "Integer");
	}
      } else {
	throw new InvalidArgument(perf, key, "Integer");
      }
    } else if (t.equals(Double.class)) {// special case for KQMLToken->Double
      if (val instanceof KQMLToken) {
	try {
	  return t.cast(new Double(val.toString()));
	} catch (NumberFormatException e) {
	  throw new InvalidArgument(perf, key, "Double");
	}
      } else {
	throw new InvalidArgument(perf, key, "Double");
      }
    } else if (t.equals(Boolean.class)) {// special case for KQMLToken->Boolean
      if (val instanceof KQMLToken) {
	String str = val.toString().toLowerCase();
	if (str.equals("nil")) {
	  return t.cast(false);
	} else if (str.equals("t")) {
	  return t.cast(true);
	}
      }
      throw new InvalidArgument(perf, key, "Boolean");
    } else { // general case for KQMLObjects
      if (t.isInstance(val)) {
	return t.cast(val);
      } else {
	throw new InvalidArgument(perf, key, t.getName());
      }
    }
  }

  /** Get an element from a list that is required to be present and of a
   * specific type, and return it as that type. If it's missing, throw
   * MissingArgument. If it's of the wrong type, throw InvalidArgument.
   */
  static <T> T getTypedArgument(KQMLList perf, int key, Class<T> t) throws MissingArgument, InvalidArgument {
    if (key >= perf.size()) {
      throw new MissingArgument(perf.get(0).toString(), key);
    }
    KQMLObject val = perf.get(key);
    if (t.equals(String.class)) { // special case for KQMLString->String
      if (val instanceof KQMLString) {
	return t.cast(((KQMLString)val).stringValue());
      } else {
	throw new InvalidArgument(perf, key, "KQMLString");
      }
    } else if (t.equals(KQMLPerformative.class)) {// special case for list->perf
      if (val instanceof KQMLList) {
	try {
	  return t.cast(new KQMLPerformative((KQMLList)val));
	} catch (KQMLBadPerformativeException e) {
	  throw new InvalidArgument(perf, key, "KQMLPerformative");
	}
      } else {
	throw new InvalidArgument(perf, key, "KQMLPerformative");
      }
    } else if (t.equals(Integer.class)) {// special case for KQMLToken->Integer
      if (val instanceof KQMLToken) {
	try {
	  return t.cast(new Integer(val.toString()));
	} catch (NumberFormatException e) {
	  throw new InvalidArgument(perf, key, "Integer");
	}
      } else {
	throw new InvalidArgument(perf, key, "Integer");
      }
    } else if (t.equals(Double.class)) {// special case for KQMLToken->Double
      if (val instanceof KQMLToken) {
	try {
	  return t.cast(new Double(val.toString()));
	} catch (NumberFormatException e) {
	  throw new InvalidArgument(perf, key, "Double");
	}
      } else {
	throw new InvalidArgument(perf, key, "Double");
      }
    } else if (t.equals(Boolean.class)) {// special case for KQMLToken->Boolean
      if (val instanceof KQMLToken) {
	String str = val.toString().toLowerCase();
	if (str.equals("nil")) {
	  return t.cast(false);
	} else if (str.equals("t")) {
	  return t.cast(true);
	}
      }
      throw new InvalidArgument(perf, key, "Boolean");
    } else {
      if (t.isInstance(val)) {
	return t.cast(val);
      } else {
	throw new InvalidArgument(perf, key, t.getName());
      }
    }
  }

  /** Get an argument from a performative that is not required to be present,
   * but is required to be of a specific type, and return it (or the given
   * default value) as that type. If it's present and of the wrong type, throw
   * InvalidArgument.
   */
  static <T> T getTypedArgument(KQMLPerformative perf, String key, Class<T> t, T defalt) throws InvalidArgument {
    try {
      return getTypedArgument(perf, key, t);
    } catch (MissingArgument e) {
      return defalt;
    }
  }

  /** Get an element from a list that is not required to be present, but is
   * required to be of a specific type, and return it (or the given default
   * value) as that type. If it's present and of the wrong type, throw
   * InvalidArgument.
   */
  static <T> T getTypedArgument(KQMLList perf, int key, Class<T> t, T defalt) throws InvalidArgument {
    try {
      return getTypedArgument(perf, key, t);
    } catch (MissingArgument e) {
      return defalt;
    }
  }
}

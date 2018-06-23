package TRIPS.util.cwc;

import TRIPS.KQML.*;

public class InvalidArgument extends CWCException {
  public InvalidArgument(String operator, String argument, String expected, KQMLObject got) {
    super("failed-to-interpret", new KQMLPerformative("invalid-argument"));
    failureReason.setParameter(":operator", new KQMLToken(operator));
    failureReason.setParameter(":argument", new KQMLToken(argument));
    failureReason.setParameter(":expected", new KQMLString(expected));
    failureReason.setParameter(":got", got);
  }

  public InvalidArgument(KQMLPerformative operation, String argument, String expected) {
    super("failed-to-interpret", new KQMLPerformative("invalid-argument"));
    failureReason.setParameter(":operator", operation.getVerb());
    failureReason.setParameter(":argument", new KQMLToken(argument));
    failureReason.setParameter(":expected", new KQMLString(expected));
    failureReason.setParameter(":got", operation.getParameter(argument));
  }

  public InvalidArgument(KQMLList operation, int argument, String expected) {
    super("failed-to-interpret", new KQMLPerformative("invalid-argument"));
    failureReason.setParameter(":operator", operation.get(0));
    failureReason.setParameter(":argument", new KQMLToken(Integer.toString(argument)));
    failureReason.setParameter(":expected", new KQMLString(expected));
    failureReason.setParameter(":got", operation.get(argument));
  }
}


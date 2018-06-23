package TRIPS.util.cwc;

import TRIPS.KQML.*;

public class MissingArgument extends CWCException {
  public MissingArgument(String operator, String argument) {
    super("failed-to-interpret", new KQMLPerformative("missing-argument"));
    failureReason.setParameter(":operator", new KQMLToken(operator));
    failureReason.setParameter(":argument", new KQMLToken(argument));
  }

  public MissingArgument(String operator, int argument) {
    super("failed-to-interpret", new KQMLPerformative("missing-argument"));
    failureReason.setParameter(":operator", new KQMLToken(operator));
    failureReason.setParameter(":argument", new KQMLToken(Integer.toString(argument)));
  }
}


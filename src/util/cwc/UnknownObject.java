package TRIPS.util.cwc;

import TRIPS.KQML.*;

public class UnknownObject extends CWCException {
  public UnknownObject(String what) {
    super("failed-to-interpret", new KQMLPerformative("unknown-object"));
    failureReason.setParameter(":what", new KQMLToken(what));
  }
}


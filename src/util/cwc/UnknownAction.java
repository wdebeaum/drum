package TRIPS.util.cwc;

import TRIPS.KQML.*;

public class UnknownAction extends CWCException {
  public UnknownAction(String what) {
    super("failed-to-interpret", new KQMLPerformative("unknown-action"));
    failureReason.setParameter(":what", new KQMLToken(what));
  }
}


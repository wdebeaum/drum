package TRIPS.util.cwc;

import TRIPS.KQML.*;

public class CWCException extends Exception {
  public final String failureType;
  public final KQMLPerformative failureReason;
  public CWCException(String type, KQMLPerformative reason) {
    this.failureType = type;
    this.failureReason = reason;
  }

  public KQMLPerformative toFailure() {
    KQMLPerformative failure = new KQMLPerformative("failure");
    failure.setParameter(":type", new KQMLToken(failureType));
    failure.setParameter(":reason", failureReason);
    return failure;
  }
}


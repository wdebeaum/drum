package TRIPS.util.cwc;

import TRIPS.KQML.*;

public class InvalidArgumentCombo extends CWCException {
  public InvalidArgumentCombo(String comment) {
    super("failed-to-interpret", new KQMLPerformative("invalid-argument-combo"));
    failureReason.setParameter(":comment", new KQMLString(comment));
  }
}


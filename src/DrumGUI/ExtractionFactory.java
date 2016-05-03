/*
 * ExtractionFactory.java
 *
 * $Id: ExtractionFactory.java,v 1.9 2016/04/25 23:01:29 lgalescu Exp $
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>, 8 Jan 2015
 */

package TRIPS.DrumGUI;

import java.util.ArrayList;
import java.util.List;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;

/**
 * Factory for handling extractions.
 * 
 * @author lgalescu
 * @see Extraction
 */
public class ExtractionFactory {
    public static List<Extraction> buildExtraction(DrumKB ekb, KQMLList extractionResult) {
        KQMLList kqmlValueList = (KQMLList) extractionResult.getKeywordArg(":VALUE");
        KQMLList context = (KQMLList) extractionResult.getKeywordArg(":CONTEXT");
        KQMLObject uttnumObj = extractionResult.getKeywordArg(":UTTNUM");
        int uttnum = Integer.parseInt(uttnumObj.toString());

        ArrayList<Extraction> extractions = new ArrayList<Extraction>();
        for (KQMLObject valueObj : kqmlValueList) {
            KQMLList value = (KQMLList) valueObj;
            Extraction x = buildExtraction(ekb, value, context, uttnum);
            extractions.add(x);
        }
        return extractions;
    }

    public static Extraction buildExtraction(DrumKB ekb, KQMLList value, KQMLList context, int uttnum) {
        Extraction extraction = null;
        String extractionType = null;

        if (value != null) {
            // FIXME: temporarily, we handle both bare and ONT-prefixed types; eventually we'll use ONT
            extractionType = value.get(0).toString().replaceFirst("ONT::", "");
        }

        if (extractionType == null) {
            // TODO
        }

        if (extractionType.equalsIgnoreCase("EVENT")) {
            extraction = new EventExtraction(ekb, value, context, uttnum);
        } else if (extractionType.equalsIgnoreCase("TERM")) {
            extraction = new TermExtraction(ekb, value, context, uttnum);
        } else if (extractionType.equalsIgnoreCase("MODALITY")) {
            extraction = new ModalityExtraction(ekb, value, context, uttnum);
        } else if (extractionType.equalsIgnoreCase("EPI")) {
            extraction = new EpistemicModalityExtraction(ekb, value, context, uttnum);
        } else if (extractionType.equalsIgnoreCase("CC")) {
            extraction = new CausalityExtraction(ekb, value, context, uttnum);
        } else {
            extraction = new Extraction(ekb, value, context, uttnum);
        }

        return extraction;
    }

}

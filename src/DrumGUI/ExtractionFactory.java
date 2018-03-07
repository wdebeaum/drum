/*
 * ExtractionFactory.java
 *
 * $Id: ExtractionFactory.java,v 1.10 2018/03/06 15:53:28 lgalescu Exp $
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>, 8 Jan 2015
 */

package TRIPS.DrumGUI;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.Hashtable;
import java.util.List;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLToken;

/**
 * Factory for handling extractions.
 * 
 * @author lgalescu
 * @see Extraction
 */
public class ExtractionFactory {
    
    private static Hashtable<String, String> typeMap;
    
    private static void setMap (boolean useMap) {
        typeMap = new Hashtable<String, String>();
        if (useMap) {
            typeMap.put("ONT::BARE", "ONT::TERM");
            typeMap.put("ONT::A", "ONT::TERM");
            typeMap.put("ONT::THE", "ONT::TERM");
            typeMap.put("ONT::INDEF-SET", "ONT::TERM");
            typeMap.put("ONT::KIND", "ONT::TERM");
        }   
    }

    public static List<Extraction> buildExtraction(DrumKB ekb, KQMLList extractionResult) {
        return buildExtraction(ekb, extractionResult, false);
    }
    
    public static List<Extraction> buildExtraction(DrumKB ekb, KQMLList extractionResult, boolean useMap) {
        setMap(useMap);

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
            extractionType = value.get(0).toString();
            Debug.warn("Got extraction type: " + extractionType);
            if (!typeMap.isEmpty() && typeMap.containsKey(extractionType)) {
                String newType = typeMap.get(extractionType);
                value.remove(0);
                value.add(0, new KQMLToken(newType));
                extractionType = newType;
                Debug.warn("Mapped to: " + extractionType);
            }
            extractionType = extractionType.replaceFirst("ONT::", "");
        }

        if (extractionType == null) {
            // TODO
        }

        if (extractionType.equalsIgnoreCase("EVENT")) 
        {
            extraction = new EventExtraction(ekb, value, context, uttnum);
        } else if (extractionType.equalsIgnoreCase("TERM"))
        {
            extraction = new TermExtraction(ekb, value, context, uttnum);
        } else if (extractionType.equalsIgnoreCase("MODALITY")) 
        {
            extraction = new ModalityExtraction(ekb, value, context, uttnum);
        } else if (extractionType.equalsIgnoreCase("EPI")) 
        {
            extraction = new EpistemicModalityExtraction(ekb, value, context, uttnum);
        } else if (extractionType.equalsIgnoreCase("CC")) 
        {
            extraction = new CausalityExtraction(ekb, value, context, uttnum);
        } else 
        {
            extraction = new Extraction(ekb, value, context, uttnum);
        }

        return extraction;
    }

}

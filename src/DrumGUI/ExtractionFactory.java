/*
 * ExtractionFactory.java
 *
 * $Id: ExtractionFactory.java,v 1.11 2018/06/22 16:41:53 lgalescu Exp $
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>, 8 Jan 2015
 */

package TRIPS.DrumGUI;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.Hashtable;
import java.util.List;
import java.util.Properties;
import java.util.regex.Pattern;

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
    
    private static Hashtable<String, String> specMap;
    
    private static void setSpecifierMap (Properties props) {
        specMap = new Hashtable<String, String>();
        final Pattern listPattern = Pattern.compile(", *");
        final Pattern mapPattern = Pattern.compile(" *=> *");
        if (props == null) {
            return;
        }
        String specMapString = props.getProperty("extractions.specifiers.map");
        if (specMapString == null) {
            return;
        }
        String[] mappings = listPattern.split(specMapString);
        for (String mapping : mappings) {
            String[] pair = mapPattern.split(mapping);
            specMap.put(pair[0], pair[1]);
        }
    }

    public static List<Extraction> buildExtraction(DrumKB ekb, KQMLList extractionResult) {
        return buildExtraction(ekb, extractionResult, null);
    }
    
    public static List<Extraction> buildExtraction(DrumKB ekb, KQMLList extractionResult, Properties props) {
        setSpecifierMap(props);

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
            if (!specMap.isEmpty() && specMap.containsKey(extractionType)) {
                String newType = specMap.get(extractionType);
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

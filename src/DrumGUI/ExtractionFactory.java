/*
 * ExtractionFactory.java
 *
 * $Id: ExtractionFactory.java,v 1.13 2018/11/08 21:25:42 lgalescu Exp $
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

    /**
     * Properties
     */
    private static Properties properties;

    /**
     * Specifier mappings
     */
    private static Hashtable<String, String> specMap;

    /**
     * Sets properties.
     * <p>
     * Note: Extraction properties start with "ekb." and "extractions.".
     * 
     * @param props
     */
    public static void setProperties(Properties props) {
        properties = props;
        setSpecifierMap();
    }

    /**
     * Obtains the value of a property. Returns {@code null} if the property is not found.
     * 
     * @param key
     * @return
     */
    public static String getProperty(String key) {
        return properties.getProperty(key);
    }

    /**
     * Obtains the value of a list property. A list property value is given as "X, Y, Z, ...". The result is the list of
     * these values, i.e, (X, Y, Z, ...).
     * 
     * @param key
     * @return
     */
    public static List<String> getPropertyList(String key) {
        final Pattern listPattern = Pattern.compile(", *");

        String prop = properties.getProperty(key);
        if (prop == null) {
            return null;
        }
        String[] list = listPattern.split(prop);
        return Arrays.asList(list);
    }

    private static void setSpecifierMap() {
        final Pattern mapPattern = Pattern.compile(" *=> *");

        specMap = new Hashtable<String, String>();
        if (properties == null) {
            return;
        }
        List<String> mappings = getPropertyList("extractions.specifiers.map");
        if (mappings == null) {
            return;
        }
        for (String mapping : mappings) {
            String[] pair = mapPattern.split(mapping);
            specMap.put(pair[0], pair[1]);
        }
    }

    /**
     * Builds an extraction of the appropriate type from a KQMLList object.
     * 
     * @param ekb
     * @param extractionResult
     * @return
     */
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

    private static Extraction buildExtraction(DrumKB ekb, KQMLList value, KQMLList context, int uttnum) {
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

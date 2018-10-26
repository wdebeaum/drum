/*
 * DrumKB.java
 *
 * $Id: DrumKB.java,v 1.33 2018/10/26 01:33:42 lgalescu Exp $
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>,  9 May 2015
 */

package TRIPS.DrumGUI;

import java.io.File;
import java.io.StringReader;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Properties;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Attr;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.InputSource;

import TRIPS.KQML.KQMLList;

/**
 * Class for holding and manipulating DRUM knowledge derived from extractions (aka the EKB).
 * 
 * @author lgalescu
 *
 */
public class DrumKB {

    /**
     * Utility class for encapsulating the bits of information the EKB needs to know about sentences.
     * 
     * @author lgalescu
     *
     */
    private class Sentence {
        private int uttnum;
        private String pid;
        private String text;
        private int offset; // from start of paragraph

        public Sentence(int uttnum, String pid, String text) {
            this.uttnum = uttnum;
            this.pid = pid;
            this.text = normalize(text);
            offset = 0;
        }

        /**
         * @return the uttnum
         */
        protected int getUttnum() {
            return uttnum;
        }

        /**
         * @return the pid
         */
        protected String getPID() {
            return pid;
        }

        /**
         * @return the text
         */
        protected String getText() {
            return text;
        }

        /**
         * Sets the text for this sentence.
         * <p>
         * N.B.: The text is normalized for readability.
         * 
         * @param text
         *            the text to set
         */
        protected void setText(String text) {
            this.text = normalize(text);
        }

        /**
         * Adds clause to existing text.
         * 
         * @param clause
         *            a new clause
         */
        protected void addText(String clause) {
            if (!clause.isEmpty()) {
                text += " " + normalize(clause);
            } else {
                Debug.error("Found empty text when attempting to add clause at uttnum " + uttnum);
            }
        }

        /**
         * Normalizes some text.
         * 
         * @param text
         * @return
         */
        private String normalize(String text) {
            return text.replaceAll("\\s+", " ");
        }

        /**
         * Sets offset.
         */
        protected void setOffset(int offset) {
            this.offset = offset;
        }

        /**
         * Gets offset.
         */
        protected int getOffset() {
            return offset;
        }

    }

    private class Paragraph {
        private String pid;
        private String file;
        private String text;

        public Paragraph(String pid, String file, String text) {
            this.pid = pid;
            this.file = file;
            this.text = text;
        }

        /**
         * @return the pid
         */
        protected String getPID() {
            return pid;
        }

        /**
         * @return the file
         */
        protected String getFile() {
            return file;
        }

        /**
         * @return the text
         */
        protected String getText() {
            return text;
        }

    }

    /** An identifier. */
    private String id;

    /** A timestamp for when this EKB was created */
    private String timestamp;

    private ArrayList<Paragraph> paragraphs;
    /**
     * List of sentences in the input text (in all paragraphs), keyed by their utterance number.
     * <p>
     * N.B.: In this structure sentences have their spaces normalized, therefore they should not be used for obtaining
     * extraction spans!
     */
    private ArrayList<Sentence> sentences;

    /** 
     * List containing the extractions.
     * There should be one entry for each unique extraction (thus, logically,
     * this is a set). 
     * @see TRIPS.DrumGUI.Extraction#equals(Extraction) equals
     */ 
    ArrayList<Extraction> extractions;

    /**
     * The EKB in XML format.
     */
    Document ekb;

    /**
     * The document type (currently: "text" or "article").
     */
    String docType;

    /**
     * A flag signifying that the {@link #ekb} needs to be re-made.
     */
    private boolean _ekb_needs_update;

    /**
     * Completion status: {@code true} if whole dataset was processed, {@code false} otherwise.
     */
    private boolean completionStatus;

    /**
     * Folder where the EKB is saved.
     */
    private String ekbFolder;

    /**
     * File where the EKB is saved.
     */
    private File ekbFile;
    
    /**
     * Properties
     */
    private Properties properties;

    /**
     * Constructor.
     */
    public DrumKB() {
        id = null;
        timestamp = null;
        ekb = null;
        ekbFile = null;
        ekbFolder = null;
        _ekb_needs_update = false;
        completionStatus = false;
        docType = "text";
        paragraphs = new ArrayList<Paragraph>();
        sentences = new ArrayList<Sentence>();
        extractions = new ArrayList<Extraction>();
        properties = new Properties();
    }

    /**
     * Clears data.
     */
    protected void clear() {
        id = null;
        timestamp = null;
        ekb = null;
        ekbFile = null;
        ekbFolder = null;
        _ekb_needs_update = false;
        completionStatus = false;
        docType = "text";
        paragraphs.clear();
        sentences.clear();
        extractions.clear();
        Debug.debug("The EKB was cleared!");
    }

    /**
     * Initializes the KB, with a new id. As a side effect, {@link #timestamp} gets reset.
     * 
     * @param id
     */
    protected void init() {
        clear();
        setTimestamp();
        Debug.debug("The EKB was initialized (ts=" + timestamp + ")");
    }

    /**
     * Initializes the KB and sets its properties. 
     * <p>
     * Note: only properties with a prefix of "extractions" or "ekb" are set!
     * 
     * @param id
     * 
     * @see #init()
     */
    protected void init(Properties props) {
        for (String key : props.stringPropertyNames()) {
            if (key.startsWith("extractions") || key.startsWith("ekb")) {
                properties.put(key, props.getProperty(key));
            }
        }
        init();
        ExtractionFactory.setProperties(properties);
    }

    /**
     * Sets the {@link #id} for this EKB.
     * 
     * @param id
     *            the id to set
     */
    protected void setID(String id) {
        this.id = id;
        _ekb_needs_update = true;
    }

    /**
     * Gets the {@link #id} for this EKB.
     */
    protected String getID() {
        return id;
    }

    /**
     * @return the docType
     */
    protected String getDocType() {
        return docType;
    }

    /**
     * @param docType
     *            the docType to set
     */
    protected void setDocType(String docType) {
        this.docType = docType;
        _ekb_needs_update = true;
    }

    /**
     * Generates a timestamp recording the time when this EKB was created. The format for the timestamp is:
     * {@literal "yyyyMMdd'T'HHmmss"}.
     * 
     * @see java.text.SimpleDateFormat
     */
    private void setTimestamp() {
        // format: "yyyyMMdd'T'HHmmss"
        String pattern = "yyyyMMdd'T'HHmmss";
        SimpleDateFormat format = new SimpleDateFormat(pattern);
        timestamp = format.format(new Date());
        _ekb_needs_update = true;
    }

    /**
     * Sets completion status.
     * 
     * @param completionStatus
     *            the completionStatus to set
     */
    protected void setCompletionStatus(boolean completionStatus) {
        this.completionStatus = completionStatus;
        _ekb_needs_update = true;
    }

    /**
     * Sets the output folder, where the EKB gets saved.
     * 
     * @param outputFolder
     *            the outputFolder to set
     */
    protected void setEKBFolder(String outputFolder) {
        ekbFolder = outputFolder;
    }

    /**
     * Makes the standard name for the EKB file. Generally, the filename is composed by concatenating the values of
     * {@link #id} and {@link #timestamp}, separated by {@literal _}, and then adding the {@literal .ekb} extension. If
     * the id is {@code null}, the filename is constituted solely by the timestamp.
     * <p>
     * Examples: {@literal 213775_20151029T132849.ekb}, {@literal test_20151029T133409.ekb},
     * {@literal 20151029T133433.ekb}
     * 
     * @return the filename
     */
    private String makeEKBFileName() {
        return ((id == null) ? "" : (id + "_")) + timestamp + ".ekb";
    }

    /**
     * Sets {@link #ekbFile} to a new file, with the standard name.
     * 
     * @throws NullPointerException
     * @see {@link #makeEKBFileName()}
     */
    private void setEKBFile()
            throws NullPointerException
    {
        // don't set it if it is already set!
        if (ekbFile == null) {
            String filename = makeEKBFileName();
            setEKBFile(new File(ekbFolder, filename));
        } else {
            Debug.info("ekbFile not changed; current value: " + ekbFile);
        }
    }

    /**
     * Sets EKB file path.
     * 
     * @param file
     *            The file path
     * @throws NullPointerException
     */
    protected void setEKBFile(File file)
            throws NullPointerException
    {
        ekbFile = file;
        ekbFolder = ekbFile.getParent();
        Debug.info("ekbFile set to: " + ekbFile);
    }

    /**
     * Returns the absolute path to the EKB file.
     * 
     * @throws RuntimeException
     */
    protected String getEKBFile()
            throws RuntimeException
    {
        return ekbFile.getAbsolutePath();
    }

    /**
     * Sets current paragraph.
     * 
     * @param file
     *            the input file
     * @param text
     *            the content of the input file (a paragraph)
     * @param pid
     *            the paragraph id
     */
    protected void setParagraph(String pid, String file, String text) {
        Debug.info("DrumKB: setting paragraph: \n\tid: " + pid + "\n\tfile: " + file + "\n\ttext: " + text);
        paragraphs.add(new Paragraph(pid, file, text));
        _ekb_needs_update = true;
    }

    /**
     * Gets the current paragraph; this is the last one added to {@link #paragraphs}.
     * 
     * @return the paragraph
     * @throws RuntimeException
     *             if there are no paragraphs
     */
    private Paragraph getCurrentParagraph()
            throws RuntimeException
    {
        try {
            Paragraph last = paragraphs.get(paragraphs.size() - 1);
            return last;
        } catch (IndexOutOfBoundsException e) {
            Debug.error("No paragraphs processed: " + e);
            throw new RuntimeException("No paragraphs have been processed.");
        }
    }

    /**
     * Returns the current paragraph ID.
     * 
     */
    protected String getCurrentPID() {
        try {
            return getCurrentParagraph().getPID();
        } catch (Exception e) {
            Debug.error("Cannot get paragraph ID: " + e);
            return null;
        }
    }

    /**
     * Returns the text of the current paragraph.
     */
    protected String getCurrentText() {
        try {
            return getCurrentParagraph().getText();
        } catch (Exception e) {
            Debug.error("Cannot get paragraph ID: " + e);
            return null;
        }
    }

    /**
     * Gets list of utterance numbers.
     * 
     * @see #sentences
     */
    protected KQMLList getUttnums() {
        KQMLList uttnums = new KQMLList();
        for (Sentence s : sentences) {
            uttnums.add(Integer.toString(s.getUttnum()));
        }
        return uttnums;
    }

    /**
     * Looks up sentence in the list of sentences, by the sentences's uttnum.
     * 
     * @param uttnum
     *            the sentence number (uttnum)
     * @return the sentence, if one exists; otherwise {@code null}
     * 
     * @see #sentences
     */
    private Sentence getSentence(int uttnum) {
        for (Sentence s : sentences) {
            if (s.getUttnum() == uttnum) {
                return s;
            }
        }
        return null;
    }

    /**
     * Looks up paragraph in the list of paragraphs, by the paragraph's pid.
     * 
     * @param pid
     *            the paragraph id
     * @return the paragraph, if one exists; otherwise {@code null}
     */
    private Paragraph getParagraph(String pid) {
        for (Paragraph p : paragraphs) {
            if (p.getPID().equals(pid)) {
                return p;
            }
        }
        return null;

    }

    /**
     * Sets utterance at index {@code n} to {@code utt}. However, if the utterance at that index is set already,
     * {@code utt} is appended to it (with a space as a separator).
     * <p>
     * N.B.: Utterances are normalized; therefore they should not be used for obtaining extraction spans!
     * 
     * @param uttnum
     * @param utt
     */
    protected void addUtterance(int uttnum, String utt) {
        Sentence sentence = getSentence(uttnum);
        if (sentence == null) { // new utterance: we normalize it
            sentences.add(new Sentence(uttnum, getCurrentPID(), utt));
        } else { // additional clause of an already existing utterance
            sentence.addText(utt);
        }
        _ekb_needs_update = true;
    }

    /**
     * Sets sentence offset from the start of the paragraph.
     * 
     * @param uttnum
     * @param offset
     */
    protected void setOffset(int uttnum, int offset) {
        Sentence sentence = getSentence(uttnum);
        sentence.setOffset(offset);
    }

    /**
     * Adds extraction.
     */
    protected void add(Extraction e) {
        int index = indexOf(e);
        if (index < 0) { // not found
            extractions.add(e);
        } else { // found
            update(index, e);
        }
        _ekb_needs_update = true;
    }

    /**
     * Adds extraction from KQML message.
     * 
     * @param content
     */
    protected List<Extraction> add(KQMLList content) {
        try {
            List<Extraction> newExtractions = ExtractionFactory.buildExtraction(this, content);
            for (Extraction x : newExtractions) {
                add(x);
            }
            return (newExtractions);
        } catch (Exception e1) {
            Debug.error("Extraction interpretation failed: " + e1);
            e1.printStackTrace();
            return null;
        }
    }

    /**
     * Searches for extraction.
     */
    protected int indexOf(Extraction e) {
        for (Extraction ke : extractions) {
            if (ke.equals(e))
                return extractions.indexOf(ke);
        }
        return -1;
    }

    /**
     * Find EKB assertions by id. If none are found the result is an empty list.
     */
    protected ArrayList<Extraction> lookupByID(String id) {
        ArrayList<Extraction> result = new ArrayList<Extraction>();
        for (Extraction ke : extractions) {
            if (id.equals(ke.getID())) {
                result.add(ke);
            }
        }
        if (result.isEmpty()) {
            Debug.warn("EKB: no extraction with id=" + id);
        } else if (result.size() > 1) { // should not happen!
            Debug.error("EKB: multiple (raw) extractions with id=" + id + ": " + result);
        } else {
            Debug.debug("EKB: found (raw) extraction with id=" + id + ": " + result.get(0));
        }
        return result;
    }

    /**
     * Updates extraction with new information.
     */
    protected void update(int index, Extraction e) {
        Extraction ke = extractions.get(index);
        ke.combineWith(e);
        _ekb_needs_update = true;
    }

    /**
     * Searches for extractions by utterance number.
     */
    protected ArrayList<Extraction> getItemsByUttnum(int uttnum) {
        ArrayList<Extraction> result = new ArrayList<Extraction>();
        for (Extraction e : extractions) {
            if (uttnum == e.getUttnum()) {
                result.add(e);
            }
        }
        return result;
    }

    /**
     * Obtains the paragraph id for a given sentence.
     * 
     * @param uttnum
     *            the uttnum for the given sentence
     * @return the paragraph id
     */
    protected String getPID(int uttnum) {
        Sentence s = getSentence(uttnum);
        return s.getPID();
    }

    /**
     * Get offset from start of paragraph for a given character frame in an utterance (identified by its {@code uttnum}
     * ).
     * 
     * @param uttnum
     * @param frame
     * @return
     */
    protected int getOffset(int uttnum, int frame) {
        return getSentence(uttnum).getOffset() + frame;
    }

    /**
     * Returns a span of text for a given sentence, identified by its {@code uttnum} value.
     * 
     */
    protected String getTextSpan(int uttnum, int start, int end) {
        Sentence s = getSentence(uttnum);
        Paragraph p = getParagraph(s.getPID());
        String text = p.getText();
        if (text == null) {
            Debug.error("No text?!?");
            return null;
        }
        String span;
        try {
            span = text.substring(getOffset(uttnum, start), getOffset(uttnum, end));
        } catch (IndexOutOfBoundsException x) {
            Debug.error("Bad text span: " + uttnum + "[" + start + ", " + end + "]");
            span = ""; // TODO: why empty instead of null???
        }
        return span;
    }

    /**
     * Creates the EKB as an XML Document.
     * 
     * @see #ekb
     */
    private void makeEKB()
            throws RuntimeException
    {
        DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder docBuilder;
        try {
            docBuilder = docFactory.newDocumentBuilder();
        } catch (ParserConfigurationException e) {
            Debug.debug("Cannot create XML Document");
            e.printStackTrace();
            throw new RuntimeException(e);
        }
        ekb = docBuilder.newDocument();
        Element ekbRoot = null;
        try {
            ekbRoot = makeRootNode(ekb);
        } catch (DOMException e) {
            Debug.debug("Cannot create XML Document");
            e.printStackTrace();
            throw new RuntimeException(e);
        }
        for (Extraction ke: extractions) {
            String x = null;
            try {
                x = ke.toXML();
            } catch (Exception e) {
                Debug.debug("Error generating XML for: " + ke);
                e.printStackTrace();
            }
            if (x == null) continue; // FIXME: eventually all extractions should be output!
            try {
                // FIXME: currently, Extraction#toXML() can generate multiple
                // elements; the following is a temporary fix:
                String wrappedX = "<extractions>" + x + "</extractions>";
                Node xNode = docBuilder.parse(new InputSource(new StringReader(wrappedX))).getDocumentElement();
                xNode = ekb.importNode(xNode, true);
                NodeList xNodes = xNode.getChildNodes();
                for (int i = 0; i < xNodes.getLength(); i++) {
                    ekbRoot.appendChild(xNodes.item(i));
                }
            } catch (Exception e) {
                Debug.debug("Error generating XML for: " + x);
                e.printStackTrace();
            }
        }
    }

    /**
     * Creates {@code ekb} root element. It attaches it to the {@code doc}
     * document, then returns the new element.
     * 
     * @param doc
     * @throws DOMException
     */
    private Element makeRootNode(Document doc) throws DOMException {
        Element root = doc.createElement("ekb");
        // id
        Attr idAttr = doc.createAttribute("id");
        idAttr.setValue(id);
        root.setAttributeNode(idAttr);
        // timestamp
        Attr tsAttr = doc.createAttribute("timestamp");
        tsAttr.setValue(timestamp);
        root.setAttributeNode(tsAttr);
        // status
        Attr csAttr = doc.createAttribute("complete");
        csAttr.setValue(String.valueOf(completionStatus));
        root.setAttributeNode(csAttr);
        // domain 
        Attr emAttr = doc.createAttribute("domain");
        emAttr.setValue(String.valueOf(properties.get("extractions.mode")));
        root.setAttributeNode(emAttr);
        // done
        root.appendChild(makeInputElement(doc));
        doc.appendChild(root);
        return root;
    }

    /**
     * Creates {@code input} element.
     * 
     * @param doc
     * @throws DOMException
     */
    private Element makeInputElement(Document doc) throws DOMException {
        Element input = doc.createElement("input");
        // @id
        Attr typeAttr = doc.createAttribute("type");
        typeAttr.setValue(docType);
        input.setAttributeNode(typeAttr);
        // paragraphs
        Element parasElement = doc.createElement("paragraphs");
        for (Paragraph p : paragraphs) {
            Element pElement = makeParagraphElement(p, doc);
            parasElement.appendChild(pElement);
        }
        input.appendChild(parasElement);
        // utterances
        Element sentsElem = doc.createElement("sentences");
        for (Sentence s : sentences) {
            Element sElement = makeSentenceElement(s, doc);
            sentsElem.appendChild(sElement);
        }
        input.appendChild(sentsElem);
        return input;
    }

    /**
     * Creates a {@code paragraph} element in {@code doc}.
     * 
     * @param pid
     *            a paragraph id
     * @param doc
     *            a document
     * @return the element
     */
    private Element makeParagraphElement(Paragraph para, Document doc) {
        Element element = doc.createElement("paragraph");
        Attr id = doc.createAttribute("id");
        id.setValue(para.getPID());
        element.setAttributeNode(id);
        Attr attr = doc.createAttribute("file");
        attr.setValue(para.getFile());
        element.setAttributeNode(attr);
        Text textContent = doc.createTextNode(para.getText());
        element.appendChild(textContent);
        return element;
    }

    /**
     * Creates a {@code sentence} element in {@code doc}.
     * 
     * @param n
     *            a sentence number
     * @param doc
     *            a document
     * @return the element
     */
    private Element makeSentenceElement(Sentence sentence, Document doc) {
        Element element = doc.createElement("sentence");
        Attr id = doc.createAttribute("id");
        id.setValue(Integer.toString(sentence.getUttnum()));
        element.setAttributeNode(id);
        Attr pid = doc.createAttribute("pid");
        pid.setValue(sentence.getPID());
        element.setAttributeNode(pid);
        Text uttContent = doc.createTextNode(sentence.getText());
        element.appendChild(uttContent);
        return element;
    }

    /**
     * Outputs a compact XML serialization for a {@code Node}.
     * 
     * @param node
     *            a node
     */
    private String nodeToString(Node node) {
        StringWriter sw = new StringWriter();
        try {
            Transformer t = TransformerFactory.newInstance().newTransformer();
            t.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
            t.transform(new DOMSource(node), new StreamResult(sw));
        } catch (TransformerException te) {
            Debug.warn("nodeToString Transformer Exception");
            te.printStackTrace();
        }
        return sw.toString();
    }

    /**
     * Saves EKB to a file in {@code outputFolder}. On success, returns the full path to the saved file.
     * 
     * @return the absolute path to the saved file
     * 
     * @see #makeEKBFileName()
     * @see #ekbFolder
     */
    protected String saveEKB()
            throws RuntimeException
    {
        // create file, if it hasn't been created already
        setEKBFile();
        // make the EKB, if we don't have it already
        if (ekb == null || _ekb_needs_update) {
            try {
                makeEKB();
            } catch (Exception e) {
                Debug.debug("Error making eKB");
                e.printStackTrace();
                throw new RuntimeException(e);
            }
        }
        // save to file
        try {
            Transformer t = TransformerFactory.newInstance().newTransformer();
            t.setOutputProperty(OutputKeys.INDENT, "yes");
            t.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4");

            DOMSource source = new DOMSource(ekb.getDocumentElement());
            t.transform(source, new StreamResult(ekbFile));
            Debug.debug("eKB written to: " + ekbFile.getName());
        } catch (Exception e) {
            Debug.debug("Error writing eKB to: " + ekbFile.getName());
            throw new RuntimeException(e);
        }
        // return
        return ekbFile.getAbsolutePath();
    }

    /**
     * Returns contents of KB in XML, without XML declaration.
     * 
     * @return KB in XML format.
     */
    protected String toXML() {
        if (ekb == null || _ekb_needs_update) {
            try {
                makeEKB();
            } catch (Exception e) {
                return null;
            }
        }
        return nodeToString(ekb.getDocumentElement());
    }

}

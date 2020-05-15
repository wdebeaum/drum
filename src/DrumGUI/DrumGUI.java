/*
 * DrumGUI.java
 *
 * $Id: DrumGUI.java,v 1.97 2020/05/14 13:58:19 lgalescu Exp $
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>,  8 Feb 2010
 */

/* History
 * 20141021 lgalescu - Adapted from the CernlGUI module.
 */

package TRIPS.DrumGUI;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.EOFException;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Timer;
import java.util.TimerTask;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import TRIPS.KQML.KQMLContinuation;
import TRIPS.KQML.KQMLExpectedListException;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.KQML.KQMLReader;
import TRIPS.KQML.KQMLString;
import TRIPS.KQML.KQMLToken;
import TRIPS.TripsModule.StandardTripsModule;
import TRIPS.util.StringUtils;

/**
 * GUI module for data manipulation and visualization in the DRUM system.
 * 
 * @author lgalescu
 *
 */
// TODO: make this into a DrumManager and split off all the EKB stuff into a separate module
public class DrumGUI extends StandardTripsModule {

    /** Modes of operation. */
    public enum Mode {
        /**
         * In this mode, the module is connected to the TRIPS system. Its operation is driven by the user or some other
         * module. This is the normal mode of operation.
         */
        CONNECTED,
        /**
         * In this mode, the module is connected to the TRIPS system. On startup it goes automatically into processing a
         * set of files from a batch list; when the processing is over it exits (shutting down the whole system).
         */
        BATCH,
        /**
         * In this mode, the module is not connected to TRIPS. It reads messages from trace files and simulates
         * execution. Trace files contain KQML messages that this module should receive during its processing;
         * typically, these are obtained from logs obtained from runs using any of the other two modes of operation.
         * <p>
         * This mode can be useful for re-running the system on a dataset if the only changes are in this module,
         * whereas other TRIPS components' behavior as well as the interaction between those components and this module
         * has not changed.
         * <p>
         * This mode can be triggered via the command line in two ways, leading to slightly different modes of
         * operation:
         * <li><code>-cache DIR</code>: for each data file being processed the module will look in <code>DIR</code> for
         * a file with the same name as the data file name holding the KQML messages it needs to process that one data
         * item. NOTE: This mode of operation has not been tested recently, so it might not work at all! --lg, 20150701
         * <li><code>-trace FILE</code>: the trace file contains the list of all KQML messages for replaying a session
         * (or a portion of it).
         * 
         * @see #processCache(String)
         * @see #processTrace()
         */
        STANDALONE
    }

    // TODO: add documentation

    // properties
    static Properties properties;

    // mode
    private Mode mode = Mode.CONNECTED;

    // display
    protected Display display = null;
    private boolean showDisplay = true;

    // dataset (sets of files)
    // the files to be processed during a run are those in the dataset that are selected
    protected DataSet dataset = new DataSet();
    /** Input file currently being processed. */
    protected String currentInputFile;
    /** String form of data currently being processed. */
    protected String currentInputData;

    // useful stuff for sync-ing with other TRIPS modules
    private boolean isSysReady = false;
    /** When true, we can do whatever to close off the current document, then can move on to a new one */
    private boolean documentDone = true;
    /** ID for the paragraph currently being processed. */
    protected String paragraphId = null;
    /** Counter for external {@code run-text} requests. */
    protected int runTextCounter = 0;
    /** Utterance number (uttnum) of the last utterance of the current fragment. */
    private int lastUttnumInFragment = 0;
    /** Number of clauses for the last utterance of the current fragment that remain to be processed. */
    private int clausesRemaining = 0;
    /** True iff fragment sent to TextTagger was OKed for processing. */
    private boolean gotOK = false;
    /**
     * Utterances we're waiting on. Each utterance, represented by its {@code uttnum}, maps to a list of clauses. The
     * clause is represented via the list of words specified in the speech act from the Parser.
     */
    private LinkedHashMap<Integer, ArrayList<KQMLList>> waitingList = new LinkedHashMap<Integer, ArrayList<KQMLList>>();

    /**
     * List of failed utterances we get before the full paragraph was OKed. 
     * 
     * @see {@link #gotOK}
     */
    private ArrayList<Integer> earlyFailures = new ArrayList<Integer>();
    
    /**
     * List of run requests that haven't been processed yet. If we're working on a dataset and another request comes
     * in, it is queued up here. When a dataset is finished processing, the first request from this list is taken up.
     */
    private List<RunTask> taskQueue = Collections.synchronizedList(new ArrayList<RunTask>());
    /** Current task request (not {@code null} iff it requires a reply) */
    private KQMLPerformative taskRequest = null;
    /** If {@code true}, exit after current task is completed */
    private boolean exitWhenDone = false;
    /** Reply to the current task upon completion (if {@code false} reply at start */
    private boolean replyWhenDone = false;
    /** Send EKB in reply? (can be {@code true} only if {@link #replyWhenDone} is {@code true}) */
    private boolean replyWithEKB = false;
    /** If {@code true}, call EKBAgent to do inference on the raw EKB for current task */
    private boolean ekbInferenceRequested = false;
    /** Inferred EKB as KQML string, received from EKBAgent */
    private KQMLString inferredEKBAsKQMLString = null;
    /** Inferred EKB file, received from EKBAgent */
    private String inferredEKBFileName = null;

    /**
     * A timer that checks every now and then whether we're idle and have tasks to do. If so, it will pick the first
     * task and execute it
     */
    private final Timer taskMonitor = new Timer(true);
    /** How often to check for activity status */
    private static final int TS_PERIOD = 2000; // two seconds
    /** Time of last activity seen on the rest of the system */
    private long timeOfLastSystemActivity;

    /**
     * The KB.
     */
    private DrumKB kb;

    // logging
    protected Log log = null;
    protected boolean logging = false;

    // things relevant for STANDALONE mode
    protected String cacheDir; // cached input directory
    protected String traceFile; // msg input for rerunning a full session
    // things relevant for BATCH mode
    protected String batchFile; // lists data files to be processed in batch mode

    // LG 20110710: ideally the processing model should allow for documents to be structured in more complex
    // hierarchies.
    /**
     * If {@code true}, a document will be split into paragraphs at two consecutive linebreaks. If {@code false} (the
     * default), the document is processed in a single piece.
     * <p>
     * Note: Other TRIPS components may impose limits on the length of text they can process at once. It is advisable
     * that very long text be pre-processed in smaller, paragraph-size fragments, or that this setting is turned on
     * (assuming the two-linebreaks convention is followed in the input text).
     * 
     * @see #splitOnNewlines
     */
    private boolean splitParagraphsMode = false;
    /**
     * If {@code true}, paragraphs that don't look like sentences are skipped. For this option to be on,
     * {@link #splitParagraphsMode} must also be on.
     * 
     * @see #splitParagraphsMode
     */
    private boolean validateParagraphs = false;
    /**
     * If {@code true}, the document is considered pre-processed to contain one sentence per line. For the moment this
     * turns off {@link #splitParagraphsMode}.
     * If {@code false} (the default) no such assumption is made.
     * <p>
     * Note: There is no check that the input actually conforms to the one-sentence-per-line convention; this setting
     * only controls the behavior of this component, but has no effect on subsequent decisions by other TRIPS
     * components.
     * 
     * @see #splitParagraphsMode
     */
    private boolean splitOnNewlines = false;
    /**
     * Text fragments (paragraphs or lines) in the current document. Used only if a splitting mode is used, i.e., either
     * {@link #splitParagraphsMode} or {@link #splitOnNewlines} is turned on.
     * 
     * @see #splitParagraphsMode
     * @see #splitOnNewlines
     */
    String[] fragments;
    /**
     * Fragment offsets relative to document. Used in conjunction with {@link #fragments}.
     * 
     * @see #fragments
     */
    int[] fragmentOffsets;
    /**
     * Maps each uttnum to a fragment index (starting at 0). Uttnums are utterance numbers coming out of the sentence
     * splitter. This object holds a mapping between those numbers and the text fragment they belong to.
     */
    HashMap<Integer, Integer> docMap;
    /** Number of text fragments (paragraphs or lines) processed */
    int fragmentsDone = 0;

    int currentDatasetIndex = 0;

    // TT options, etc.
    /** Default tag options, set during initialization. */
    private KQMLList defaultTagOptions;
    /** Tag options for processing current data item. May be changed via GUI or message. */
    private KQMLList tagOptions;
    /** Input terms */
    private KQMLList inputTerms = new KQMLList();
    private String inputTermsFolder = null;
    private String inputTermsFileExtension = "its";

    /**
     * main()
     */
    public static void main(String argv[]) {
        new DrumGUI(argv, true).run();
    }

    /**
     * Constructor
     */
    public DrumGUI(String argv[], boolean isApplication) {
        super(argv, isApplication);
    }

    /**
     * Constructor
     */
    public DrumGUI(String argv[]) {
        this(argv, false);
    }

    /**
     * Performs initializations.
     */
    public void init() {
        name = "READER"; // we are a reading manager
        super.init();
        handleParameters();

        // debugging
        if (debuggingEnabled) {
            Debug.setDebugLevel(Debug.Level.DEBUG);
        }

        // TT options
        String tagOptProp = getProperty("tag.options.default");
        if (tagOptProp != null) {
            try {
                defaultTagOptions = KQMLList.fromString(tagOptProp);
            } catch (IOException e) {
                Debug.fatal("Badly formed default tag options string!");
            }
        } else {
            defaultTagOptions = new KQMLList();
        }
        tagOptions = defaultTagOptions;

        // break document into lines?
        splitOnNewlines = propertyValueBoolean("input.split-on-newlines");
        // break document into paragraphs?
        splitParagraphsMode = propertyValueBoolean("input.split-into-paragraphs") && !splitOnNewlines;
        // break document into paragraphs?
        validateParagraphs =propertyValueBoolean("input.validate-paragraphs") && splitParagraphsMode;
                
        // ready
        initLog();
        dumpProperties();
        sendSubscriptions();
        ready();
        setTimeOfSystemActivity();

        // get dataset
        if (mode == Mode.CONNECTED) {
            // nop
        } else if (mode == Mode.BATCH) {
            if (batchFile == null) {
                Debug.fatal("No batch file specified!");
            }
            dataset.addFromFile(batchFile);
            dataset.selectAll();
            if (isSysReady) { // normally we'd have to wait for this...
                sendGetTTParameters();
                initiateProcessing(true);
            }
        } else if (mode == Mode.STANDALONE) {
            if ((cacheDir == null) && (traceFile == null)) {
                Debug.fatal("A cache directory or a trace file must be specified!");
            }
            isSysReady = true; // not sure this is actually useful
            if (traceFile != null)
                processTrace();
        }

        // display
        Debug.warn("showDisplay = " + showDisplay);
        if (showDisplay) {
            display = new Display(this, name);
            display.setSelectorMode(propertyValueInt("Display.SelectorPanel.mode"));
            display.setExtractorMode(getProperty("Display.ExtractorPanel.mode"));
            display.showDataset(false);
        }

        // should we wait?
        if (display != null) {
            display.setState(isSysReady ? Display.State.READY : Display.State.WAITING);
        }

        // task scheduler
        taskMonitor.schedule(new TaskScheduler(), TS_PERIOD, TS_PERIOD);

        // fragmentsMap initialization
        docMap = new HashMap<Integer, Integer>();
    }

    /**
     * Handles command-line parameters.
     */
    protected void handleParameters() {
        String value;
        // properties
        properties = new Properties();
        setDefaultProperties();
        if ((value = getParameter("-config")) != null) {
            loadProperties(value);
        }

        if ((value = getParameter("-display")) != null) {
            showDisplay = StringUtils.stringToBoolean(value);
        }
        if ((value = getParameter("-data")) != null) {
            try {
                dataset.setFolder(value, true);
            } catch (IOException e) {
                Debug.fatal("Cannot set folder to " + value);
            }
        }
        if ((value = getParameter("-cache")) != null) {
            cacheDir = value;
            mode = Mode.STANDALONE;
        }
        if ((value = getParameter("-trace")) != null) {
            traceFile = value;
            mode = Mode.STANDALONE;
        }
        if ((value = getParameter("-batch")) != null) {
            batchFile = value;
            mode = Mode.BATCH;
        }
        if ((value = getParameter("-log")) != null) {
            logging = StringUtils.stringToBoolean(value);
        }
        if ((value = getParameter("-wait")) != null) {
            isSysReady = !StringUtils.stringToBoolean(value);
        }
    }

    /**
     * Sets default properties.
     */
    private void setDefaultProperties() {
        properties.put("Display.ExtractorPanel.mode", "TREE");
        properties.put("Display.SelectorPanel.mode", "2");
        properties.put("tag.options.default", "(:split-clauses true :split-sentences true)");
        properties.put("input.split-into-paragraphs", "false");
        properties.put("input.split-on-newlines", "false");
        properties.put("input.validate-paragraphs", "false");
        properties.put("extractions.mode", "DRUM");
        properties.put("ekb.reasoner", "DRUM");
    }

    /**
     * Loads properties from a configuration file.
     * 
     * @param file
     */
    private void loadProperties(String file) {
        Debug.warn("Using configuration file: " + file);
        BufferedReader in = null;
        try {
            in = new BufferedReader(new FileReader(file));
        } catch (FileNotFoundException e) {
            Debug.error("Cannot open file: " + file);
            e.printStackTrace();
            return;
        }
        try {
            properties.load(in);
        } catch (IOException e) {
            Debug.error("Cannot read file: " + file);
            e.printStackTrace();
        }
        try {
            if (in != null)
                in.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Dumps properties to logfile or STDERR.
     */
    private void dumpProperties() {
        StringWriter writer = new StringWriter();
        properties.list(new PrintWriter(writer));
        log("properties", writer.getBuffer().toString());
    }

    /**
     * Returns a property value.
     * 
     * @param prop
     * @return
     */
    protected String getProperty(String prop) {
        return properties.getProperty(prop);
    }

    /**
     * Returns a property value as {@code int}.
     * 
     * @param prop
     * @return
     */
    protected int propertyValueInt(String prop) {
        return Integer.parseInt(properties.getProperty(prop));
    }

    /**
     * Returns a property value as {@code boolean}.
     * 
     * @param prop
     * @return
     */
    protected boolean propertyValueBoolean(String prop) {
        return Boolean.parseBoolean(properties.getProperty(prop, "false"));
    }

    /**
     * Sends message subscriptions.
     */
    protected void sendSubscriptions() {
        if (mode == Mode.STANDALONE) {
            return;
        }
        KQMLPerformative perf = null;
        try {
            perf = KQMLPerformative.fromString("(subscribe :content (tell &key :content (extraction-result . *)))");
            send(perf);
            perf = KQMLPerformative
                    .fromString("(subscribe :content (tell &key :content (module-status . *) :sender TEXTTAGGER))");
            send(perf);
            perf = KQMLPerformative.fromString("(subscribe :content (tell &key :content (start-paragraph . *)))");
            send(perf);
            perf = KQMLPerformative
                    .fromString("(subscribe :content (tell &key :content (utterance . *) :sender TEXTTAGGER))");
            send(perf);
            // from Parser: useful only to tell when it rejects the input (therefore no need to expect IM messages)
            perf = KQMLPerformative.fromString("(subscribe :content (tell &key :content (paragraph-completed . *)))");
            send(perf);
            // from parser, to keep track of clauses IM is working on
            perf = KQMLPerformative.fromString("(subscribe :content (tell &key :content (new-speech-act . *)))");
            send(perf);
            // from parser: parse failures (so we know nothing else is coming)
            perf = KQMLPerformative.fromString("(subscribe :content (tell &key :content (failed-to-parse . *)))");
            send(perf);
            // from IM: utterance finished
            perf = KQMLPerformative.fromString("(subscribe :content (tell &key :content (end-of-turn . *)))");
            send(perf);
            // from IM: fragment or full utterance failed interpretation
            perf = KQMLPerformative.fromString("(subscribe :content (tell &key :content (interpretation-failed . *)))");
            send(perf);
            // from IM: paragrapd done
            perf = KQMLPerformative.fromString("(subscribe :content (tell &key :content (paragraph-done . *)))");
            send(perf);
            // tag requests to TextTagger initiated by other modules
            perf = KQMLPerformative.fromString("(subscribe :content (request &key :content (tag . *)))");
            send(perf);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    /**
     * Requests that the {@code TextTagger} component send back its
     * configuration. Currently this information is not processed in any way,
     * but is logged so it can be inspected.
     */
    protected void sendGetTTParameters() {
        if (mode == Mode.STANDALONE) {
            return;
        }
        KQMLPerformative perf = null;
        try {
            perf = KQMLPerformative.fromString("(request :receiver TextTagger :content (get-parameters))");
            send(perf);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    /**
     * Handles {@code tell} messages.
     */
    public void receiveTell(KQMLPerformative msg, Object contentobj) {
        log("received", msg.toString());

        if (contentobj.toString().equals("NIL")) {
            errorReply(msg, "NIL content in tell");
            return;
        }

        KQMLList content = (KQMLList) contentobj;
        String verb = content.get(0).toString();

        this.setTimeOfSystemActivity();

        if (verb.equalsIgnoreCase("module-status")) {
            handleModuleStatus(msg, content);
        } else if (verb.equalsIgnoreCase("start-paragraph")) {
            handleStartParagraph(content);
        } else if (verb.equalsIgnoreCase("utterance")) {
            handleUtterance(msg, content);
        } else if (verb.equalsIgnoreCase("extraction-result")) {
            handleExtraction(content);
        } else if (verb.equalsIgnoreCase("paragraph-completed")) {
            handleParagraphCompleted(content);
        } else if (verb.equalsIgnoreCase("new-speech-act")) {
            handleNewSpeechAct(content);
        } else if (verb.equalsIgnoreCase("failed-to-parse")) {
            handleUtteranceFailed(content);
        } else if (verb.equalsIgnoreCase("end-of-turn")) {
            handleUtteranceDone(content);
        } else if (verb.equalsIgnoreCase("interpretation-failed")) {
            handleUtteranceDone(content);
        } else if (verb.equalsIgnoreCase("paragraph-done")) {
            handleParagraphDone(content);
        } else if (verb.equalsIgnoreCase("reject")) { // temporary (FIXME)
            handleReplyReject(content);
        } else {
            // ignore
            error("Can't handle tell: " + verb);
        }
    }

    /**
     * Handles {@code request} messages.
     */
    public void receiveRequest(KQMLPerformative msg, Object contentObject) {
        log("received", msg.toString());

        if (contentObject.toString().equals("NIL")) {
            errorReply(msg, "NIL content in request");
            return;
        }

        KQMLList content = (KQMLList) contentObject;
        String verb = content.get(0).toString();

        if (verb.equalsIgnoreCase("tag")) {
            // we only overhear this!
            handleTag(msg, content);
        } else if (verb.equalsIgnoreCase("set-tag-options")) {
            // set options to send to TextTagger
            handleSetTagOptions(msg, content);
        } else if (verb.equalsIgnoreCase("set-terms-folder")) {
            // set options to send to TextTagger
            handleSetTermsFolder(msg, content);
        } else if (verb.equalsIgnoreCase("run-text") || verb.equalsIgnoreCase("load-text")
                || verb.equalsIgnoreCase("run-file") || verb.equalsIgnoreCase("load-file")
                || verb.equalsIgnoreCase("run-all-files")
                || verb.equalsIgnoreCase("run-pmcid")) {
            handleTaskRequest(msg, content);
        } else if (verb.equalsIgnoreCase("get-status")) {
            // initiate processing of files from folder
            try {
                handleGetStatus(msg);
            } catch (Exception e) {
                errorReply(msg, e.toString());
            }
        } else if (verb.equalsIgnoreCase("set-debug")) {
            // set debugging level
            try {
                Debug.setDebugLevel(content);
            } catch (Exception e) {
                errorReply(msg, e.toString());
            }
        } else {
            error("Can't handle request: " + verb);
            errorReply(msg, "Sorry, I don't know how to handle your request.");
        }
    }

    /**
     * Handles run requests.
     */
    protected void handleTaskRequest(KQMLPerformative msg, KQMLList content) {
        String verb = content.get(0).toString();

        // if we're busy, we queue up this task
        if (!dataset.isSelectionEmpty()) {
            synchronized (taskQueue) {
                taskQueue.add(new RunTask(msg, content));
                Debug.debug("Added task to queue: " + content);
            }
            return;
        }

        // otherwise, we proceed
        Debug.debug("Started working on task: " + content);
        try {
            if (verb.equalsIgnoreCase("load-text") || verb.equalsIgnoreCase("run-text")) {
                // initiate processing of text
                handleRunText(msg, content);
            } else if (verb.equalsIgnoreCase("load-file") || verb.equalsIgnoreCase("run-file")) {
                // initiate processing of file
                handleRunFile(msg, content);
            } else if (verb.equalsIgnoreCase("run-all-files")) {
                // initiate processing of files from folder
                handleRunAllFiles(msg, content);
            } else if (verb.equalsIgnoreCase("run-pmcid")) {
                // initiate processing of files from folder
                handleRunPMCID(msg, content);
            }
        } catch (Exception e) {
            errorReply(msg, e.toString());
        }
    }

    /**
     * Handles {@code reply} messages.
     */
    public void receiveReply(KQMLPerformative msg, Object contentObject) {
        log("received", msg.toString());

        KQMLList content = (KQMLList) contentObject;
        String verb = content.get(0).toString();

        if (verb.equalsIgnoreCase("ok")) {
            handleReplyOK(content);
        } else if (verb.equalsIgnoreCase("parameters")) {
            // ignore: we only want to log these
        } else if (verb.equalsIgnoreCase("reject")) {
            handleReplyReject(content);
        } else {
            // ignore but complain softly
            error("Can't handle reply: " + verb);
        }
    }

    /**
     * Handler for 'overheard' {@code tag} requests to TextTagger
     * (<em>i.e.</em>, messages sent from some other TRIPS component).
     */
    private void handleTag(KQMLPerformative msg, KQMLList content) {
        KQMLToken sender = (KQMLToken) msg.getParameter(":sender");
        if (sender == null) {
            errorReply(msg, "No :sender... Who *are* you?");
            return;
        }
        // if we sent this, and we're not in standalone mode, we don't do anything
        if ((mode != Mode.STANDALONE) && sender.toString().equalsIgnoreCase(name)) {
            // ignore
            return;
        }
        currentInputData = ((KQMLString) content.getKeywordArg(":text")).stringValue();
        Debug.warn("current para length: " + currentInputData.length());

        if (display != null) {
            display.showTextNoSelector(currentInputData);
        }
        
        // set up KB to allow processing extractions
        kb = new DrumKB(properties);
        kb.setID("tag-request");
        kb.setParagraph(null, currentInputData);
    }

    /**
     * Handler for setting options to add to the {@code tag} message.
     * <p>
     * The {@code set-tag-options} message has two forms:
     * <dl>
     * <dt>{@literal (set-tag-options :tt-opt tt-value ...)}
     * <dd>allows direct specification of {@code tag} options. If no options are
     * specified options are reset to their default values.
     * <dt>{@literal (set-tag-options :from-file "file")}
     * <dd>read {@code tag} options from a file
     * </dl>
     * In all cases the options must be a valid sequence of keyword-argument
     * pairs in valid KQML.
     *
     * @see #tagOptions
     * @see #defaultTagOptions
     */
    private void handleSetTagOptions(KQMLPerformative msg, KQMLList content) {
        KQMLObject fileObj = content.getKeywordArg(":from-file");
        if (fileObj == null) {
            content.remove(0); // take verb out of content list
            setTagOptions(content);
        } else {
            KQMLList options = null;
            try {
                KQMLReader reader = new KQMLReader(new FileReader(fileObj.stringValue()));
                try {
                    options = reader.readList();
                    Debug.debug("options:" + options);
                    setTagOptions(options);
                } catch (IOException e) {
                    Debug.debug("Ecountered a problem while reading " + fileObj + " (options so far:" + options + ")");
                    e.printStackTrace();
                }
            } catch (FileNotFoundException e) {
                Debug.error(e);
                e.printStackTrace();
                errorReply(msg, "Sorry. Failed to set tag options (file not found).");
            }
        }
    }

    /**
     * Handler for setting and resetting folder with input term files.
     * <p>
     * To set, use: {@literal (set-terms-folder :folder "/tmp/input-terms" :extension "its")} <br>
     * To reset, use: {@literal (reset-terms-folder)}
     */
    private void handleSetTermsFolder(KQMLPerformative msg, KQMLList content) {
        KQMLObject folder = content.getKeywordArg(":folder");
        if (folder == null) {
            resetInputTerms();
            return;
        }
        try {
            File folderAsFile = new File(folder.stringValue());
            if (!folderAsFile.isDirectory()) {
                errorReply(msg, "Directory does not exist: " + folder);
                resetInputTerms();
                return;
            }
            inputTermsFolder = folderAsFile.getCanonicalPath();
            KQMLObject extension = content.getKeywordArg(":extension");
            if (extension != null) {
                inputTermsFileExtension = extension.stringValue();
            }
            Debug.info("Input terms set to: " + inputTermsFolder + File.separator + "*." + inputTermsFileExtension);
        } catch (Exception e) {
            Debug.error(e);
            e.printStackTrace();
            errorReply(msg, "Directory does not exist or cannot be accessed: " + folder);
            resetInputTerms();
        }
    }

    /**
     * Reset default values for input terms parameters.
     * 
     */
    private void resetInputTerms() {
        inputTermsFolder = null;
        inputTermsFileExtension = "its";
        Debug.info("Input terms parameters have been reset");
    }

    /**
     * Set common task parameters.
     * 
     */
    private void setCommonTaskParameters(KQMLPerformative msg, KQMLList content, boolean reply_when_done,
            boolean reply_with_ekb) {
        // task has a callback?
        if (msg.getParameter(":reply-with") != null) {
            Debug.info("New task (" + msg.getParameter(":reply-with") + ")");
            taskRequest = msg;
        } else {
            taskRequest = null;
            Debug.info("New task (no callback)");
        }
        // last task?
        KQMLObject exitWhenDoneObj = content.getKeywordArg(":exit-when-done");
        if (exitWhenDoneObj != null) {
            exitWhenDone = StringUtils.stringToBoolean(exitWhenDoneObj.toString());
        }
        // task needs ekb at the end?
        replyWithEKB = false;
        KQMLObject replyWithEKBObj = content.getKeywordArg(":reply-with-ekb");
        if (replyWithEKBObj != null) {
            replyWithEKB = StringUtils.stringToBoolean(replyWithEKBObj.toString());
        } else {
            replyWithEKB = reply_with_ekb;
        }
        // task reply at the end?
        replyWhenDone = false;
        KQMLObject replyWhenDoneObj = content.getKeywordArg(":reply-when-done");
        if (replyWhenDoneObj != null) {
            replyWhenDone = StringUtils.stringToBoolean(replyWhenDoneObj.toString());
        } else {
            replyWhenDone = reply_when_done || replyWithEKB;
        }

        // Ask EKBAgent to do inference on the resulting EKB?
        KQMLObject doInferenceObj = content.getKeywordArg(":do-inference");
        if (doInferenceObj != null) {
            ekbInferenceRequested = StringUtils.stringToBoolean(doInferenceObj.toString());
        } else {
            ekbInferenceRequested = false;
        }
    }

    /**
     * Handler for {@code run-text} requests.
     * <p>
     * Request format:
     * {@code (run-text :text "text" [:exit-when-done false] [:reply-when-done true] [:reply-with-ekb true] [:do-inference false])))}.
     * <p>
     * Throws an exception if there is a problem.
     * 
     * @param msg
     *            requester performative
     * @param content
     *            performative content
     */
    private void handleRunText(KQMLPerformative msg, KQMLList content)
            throws RuntimeException, IOException {
        // common task parameters
        setCommonTaskParameters(msg, content, true, true);

        // specific task parameters
        String text = ((KQMLString) content.getKeywordArg(":text")).stringValue();

        // write text to new file
        String folder = dataset.getFolder();
        KQMLObject replyWith = msg.getParameter(":reply-with");
        String filename = String.format("TEXT%05d%s", ++runTextCounter,
                (replyWith != null) ? "_" + replyWith.toString() : "");
        String pathname = folder + File.separator + filename;
        File f = new File(pathname);
        try {
            BufferedWriter bw = new BufferedWriter(new FileWriter(f));
            bw.write(text);
            bw.close();
            Debug.debug("Written file to: " + pathname);
        } catch (Exception e) {
            Debug.debug("Error writing file to: " + pathname);
            throw new RuntimeException("Error saving data to file.");
        }

        // set dataset
        setDatasetToFile(folder, filename);

        // init the EKB
        kb = new DrumKB(properties);
        kb.setID(filename);

        // send accepted?
        if ((taskRequest != null) && !replyWhenDone)
            try {
                sendAcceptedTask();
            } catch (Exception e) {
                e.printStackTrace();
                errorReply(taskRequest, "Error: EKB cannot be saved");
                // kb.clear();
                return;
            }

        // go!
        initiateProcessing(false);
    }

    /**
     * Handler for {@code run-file} requests.
     * <p>
     * Request format:
     * {@code (run-file :folder "folder" :file "filename" [:exit-when-done false] [:reply-when-done true] [:reply-with-ekb false] [:do-inference false])}.
     * <p>
     * Throws an exception if there is a problem.
     * 
     * @param msg
     *            requester performative
     * @param content
     *            performative content
     */
    private void handleRunFile(KQMLPerformative msg, KQMLList content)
            throws RuntimeException {
        // common task parameters
        setCommonTaskParameters(msg, content, true, false);

        // specific task parameters
        String folder = ((KQMLString) content.getKeywordArg(":folder")).stringValue();
        String filename = ((KQMLString) content.getKeywordArg(":file")).stringValue();

        // set dataset
        try {
            setDatasetToFile(folder, filename);
            Debug.debug("dataset folder: " + dataset.getFolder());
        } catch (Exception e) {
            throw new RuntimeException("Dataset not found.");
        }

        // init the EKB
        kb = new DrumKB(properties);

        // set id for this run to the basename of the file
        int pos = filename.lastIndexOf(".");
        String basename = pos > 0 ? filename.substring(0, pos) : filename;
        kb.setID(basename);

        // send accepted?
        if ((taskRequest != null) && !replyWhenDone)
            try {
                sendAcceptedTask();
            } catch (Exception e) {
                e.printStackTrace();
                errorReply(taskRequest, "Error: EKB cannot be saved");
                // kb.clear();
                return;
            }

        // go!
        initiateProcessing(false);
    }

    /**
     * Handler for {@code run-all-files} requests.
     * <p>
     * Request format:
     * {@code (run-all-files :folder "folder" :select "*.xml" [:single-ekb false] [:exit-when-done false] [:reply-when-done false] [:reply-with-ekb false] [:do-inference false])}.
     * <p>
     * Extractions will be placed in a single EKB if {@code :single-ekb} resolves to {@code true}; if this parameter is
     * missing, it defaults to {@code false}.
     * <p>
     * Throws an exception if there is a problem.
     * 
     * @param msg
     *            requester performative
     * @param content
     *            performative content
     */
    private void handleRunAllFiles(KQMLPerformative msg, KQMLList content)
            throws RuntimeException {
        // common task parameters
        setCommonTaskParameters(msg, content, false, false);

        // specific task parameters
        String folder = ((KQMLString) content.getKeywordArg(":folder")).stringValue();
        String fileType = ((KQMLString) content.getKeywordArg(":select")).stringValue();
        boolean singleEKB = false;
        KQMLObject singleEKBObject = content.getKeywordArg(":single-ekb");
        if (singleEKBObject != null) {
            singleEKB = StringUtils.stringToBoolean(singleEKBObject.toString());
        }

        // set dataset
        try {
            setDatasetWithFilePattern(folder, fileType);
        } catch (Exception e) {
            throw new RuntimeException("Dataset not found.");
        }

        if (singleEKB) {
            // init the ekb; use folder name as id
            String ekb_id = (new File(folder)).getName();
            kb = new DrumKB(properties);
            kb.setID(ekb_id);

            // send accepted?
            if ((taskRequest != null) && !replyWhenDone)
                try {
                    sendAcceptedTask();
                } catch (Exception e) {
                    e.printStackTrace();
                    errorReply(taskRequest, "Error: EKB cannot be saved");
                    // kb.clear();
                    return;
                }

            // go!
            initiateProcessing(false);
        } else {
            // the way we handle this is as a batch job of single-file run requests: we make fake run-file
            // requests and place them in the queue. N.B.: i think it's possible that external requests might be
            // interspersed with these; i'm not sure this is really likely to happen, and even if it does, it may not be
            // harmful. TODO: perhaps worth keeping an eye on this, just in case!
            while (!dataset.isSelectionEmpty()) {
                // N.B.: we use getFirstSelection() rather than popSelection() b/c we want to keep it in
                // the dataset until the job for the data item is actually created; only then we do a pop!
                String filename = dataset.getFirstSelection();
                KQMLPerformative rfRequest = new KQMLPerformative("request");
                KQMLList rfContent = new KQMLList();
                rfContent.add("run-file");
                rfContent.add(":folder");
                rfContent.add(new KQMLString(folder));
                rfContent.add(":file");
                rfContent.add(new KQMLString(filename));
                rfRequest.setParameter(":content", rfContent);
                handleTaskRequest(rfRequest, rfContent);
                dataset.popSelection();
            }
        }
    }

    /**
     * Handler for {@code run-pmcid} requests.
     * <p>
     * Request format:
     * {@code (run-pmcid :folder "folder" :pmcid "pmcid" [:save-to "ekb-path"] [:exit-when-done false] [:reply-when-done false] [:reply-with-ekb false] [:do-inference false])}
     * <p>
     * If the folder is given, the data files are assumed to be in the given folder (value of the {@code :folder}
     * parameter). All XML files (extension {@literal .xml}) are selected for processing. It is assumed that these XML
     * files contain text, optionally mixed with correctly-balanced XLM tags. The {@code :save-to} value is a path to a
     * file or a directory. If a file, the EKB will be saved to that file; if a directory, it will be saved in the
     * specified directory, with a name computed automatically. It is assumed that a file has an extension (typically,
     * {@literal .ekb}); if none is found, the path is taken to be a directory.
     * <p>
     * If the folder is not given, a call to {@code PUB-MANAGER} is made to obtain the location of the data. When the
     * reply with the location is received, the task is recreated, now with the folder being specified.
     *
     * TODO: should do a better job checking on "ekb-path".
     */
    private void handleRunPMCID(KQMLPerformative msg, KQMLList content)
            throws RuntimeException {
        // for now this is fixed
        final String pmcidFileType = ".*\\.xml";

        // specific task parameters
        KQMLObject pmcidObj = content.getKeywordArg(":pmcid");
        if (pmcidObj == null) {
            errorReply(msg, "Bad format (missing :pmcid?)");
            return;
        }
        String pmcidFolder = null;
        String pmcid = ((KQMLString) pmcidObj).stringValue();
        KQMLObject folderObj = content.getKeywordArg(":folder");
        String folder = null;
        if (folderObj == null) {
            try {
                KQMLPerformative pullMsg = KQMLPerformative
                        .fromString("(request :content (pub-pull :pmcid \"" + pmcid + "\"))");
                sendWithContinuation(pullMsg, new PubPullReplyHandler(msg, content));
                return;
            } catch (Exception e) {
                e.printStackTrace();
                errorReply(msg, "Something terrible just happened!");
                return;
            }
        } else {
            pmcidFolder = ((KQMLString) folderObj).stringValue();
        }

        KQMLObject savetoObj = content.getKeywordArg(":save-to");
        String saveto = null;
        File savetoFile = null;
        File savetoFolder = null;
        if (savetoObj != null) {
            saveto = savetoObj.stringValue();
            savetoFile = new File(saveto);
            // file or folder?
            if (savetoFile.getName().lastIndexOf('.') == -1) {
                savetoFolder = savetoFile;
                savetoFile = null;
            }
        }

        // common task parameters
        setCommonTaskParameters(msg, content, false, false);

        // init the EKB
        kb = new DrumKB(properties);
        kb.setID(pmcid);
        kb.setDocType("article");
        if (saveto != null) {
            if (savetoFolder != null) {
                Debug.debug("Setting EKB folder to: " + saveto);
                kb.setEKBFolder(saveto);
            } else if (savetoFile != null) {
                Debug.debug("Setting EKB file to: " + saveto);
                kb.setEKBFile(savetoFile);
            } else {
                Debug.debug("How did we get in this dark corner?!?");
            }
        }

        // send accepted?
        if ((taskRequest != null) && !replyWhenDone)
            try {
                sendAcceptedTask();
            } catch (Exception e) {
                e.printStackTrace();
                errorReply(taskRequest, "Error: EKB cannot be saved");
                // kb.clear();
                return;
            }

        // set dataset
        try {
            setDatasetWithFilePattern(pmcidFolder, pmcidFileType);
            Debug.debug("Number of paragraphs found: " + dataset.getSelectionSize());
        } catch (Exception e) {
            e.printStackTrace();
            errorReply(msg, "Cannot find folder for the PMCID given");
            // kb.clear();
            return;
        }

        // go!
        initiateProcessing(false);
    }

    /**
     * Handler for {@code extraction-result} messages (from Extractor).
     */
    private void handleExtraction(KQMLList content) {
        try {
            List<Extraction> newExtractions = kb.add(content);
            if ((display != null) && (newExtractions != null)) {
                for (Extraction x : newExtractions)
                    display.addExtraction(x);
            }
            Debug.debug("STATE: got extraction-result :uttnum " + newExtractions.get(0).getUttnum());
        } catch (Exception e1) {
            log("error", "Extraction interpretation failed: " + e1);
            Debug.error("STATE: got extraction-result and failed!");
        }
    }

    /**
     * Handler for {@code utterance} messages (from TextTagger).
     */
    private void handleUtterance(KQMLPerformative msg, KQMLList content) {
        KQMLObject uttnumObj = content.getKeywordArg(":uttnum");
        if (uttnumObj == null) {
            errorReply(msg, "Missing :uttnum");
            return;
        }
        // text
        KQMLObject text = content.getKeywordArg(":text");
        int uttnum = Integer.parseInt(uttnumObj.toString());
        if (text != null) {
            kb.addUtterance(uttnum, text.stringValue());
        }
        //
        if (uttnum > lastUttnumInFragment) {
            lastUttnumInFragment = uttnum;
            clausesRemaining = 1;
        } else if (uttnum == lastUttnumInFragment) {
            clausesRemaining++;
        } else {
            clausesRemaining = 0;
        }
    }

    /**
     * Handler for {@code module-status} (from TextTagger)).
     * In "live" mode we wait for this before enabling input processing.
     */
    private void handleModuleStatus(KQMLPerformative msg, KQMLList content) {
        KQMLToken sender = (KQMLToken) msg.getParameter(":sender");
        if (sender == null) {
            errorReply(msg, "No :sender... Who *are* you?");
            return;
        }
        if (!sender.toString().equalsIgnoreCase("TextTagger")) {
            errorReply(msg, "Who are *you*?");
            return;
        }
        KQMLToken status = (KQMLToken) content.get(1);
        if ((status != null) && (status.equalsIgnoreCase("ready"))) {
            isSysReady = true;
            sendGetTTParameters();
            if (mode == Mode.BATCH) {
                initiateProcessing(false);
            }
//            if (mode == Mode.CONNECTED) {
//                initiateProcessing(true);
//            }
            if (display != null)
                display.setState(Display.State.READY);
        }
    }

    /**
     * Handler for {@code start-paragraph}. This signals that the the tag request has now started cascading through the
     * system.
     * <p>
     * Note: Normally this message comes from TextTagger; however, when {@link #splitOnNewlines} is {@code true},
     * we self-generate it.
     * 
     * @see #splitOnNewlines
     */
    private void handleStartParagraph(KQMLList content) {
        paragraphId = content.getKeywordArg(":id").toString();
        if (paragraphId == null) {
            Debug.error("No paragraph ID?!?! " + content);
            return;
        }
        String pid = kb.getParagraphId();
        if (pid == null) {
            kb.setParagraphId(paragraphId);
        }
    }

    /**
     * Handler for {@code reply} messages signaling the tag request was processed.
     */
    private void handleReplyOK(KQMLList content) {
        // TODO: i should do a better job managing requests and replies. here i'm simplifying things based on a few
        // assumptions:
        // - most "ok" replies have no information other than the acknowledgement itself; TT is pretty reliable in
        // processing requests;
        // - there is one and only one request type that adds information
        KQMLList uttnums = (KQMLList) content.getKeywordArg(":uttnums");
        if (uttnums != null) { // this is the one reply we care about
            if (uttnums.isEmpty()) {
                Debug.error("STATE: No uttnums");
            } else {
                // record some info for state tracking
                lastUttnumInFragment = Integer.parseInt(uttnums.get(uttnums.size() - 1).toString());
                addUttnumsToWaitingList(uttnums);
                Debug.debug("STATE: OK - uttnums: " + uttnums);
                gotOK = true;
            }
        }
    }

    /**
     * Handler for {@code reply} messages signaling the tag request was rejected.
     */
    private void handleReplyReject(KQMLList content) {
        KQMLString reason = (KQMLString) content.getKeywordArg(":reason");
        // current task cannot be accomplished, so...
        // we send back an error, if reply was requested
        if ((taskRequest != null) && replyWhenDone) {
            KQMLPerformative rmsg = new KQMLPerformative("reply");
            KQMLList rcontent = new KQMLList();
            rcontent.add("error");
            rcontent.add(":comment");
            rcontent.add(reason);
            rmsg.setParameter(":content", rcontent);
            reply(taskRequest, rmsg);
            // kb.clear();
        }
        // and we move on
        finishCurrentDocument();
    }

    /**
     * Handler for {@code paragraph-completed} messages signaling data item was fully processed.
     * <p>
     * NB: This only says the PARSER is done; further processing may be happening in other modules! This may be removed
     * in the future.
     */
    private void handleParagraphCompleted(KQMLList content) {
        String pID = content.getKeywordArg(":id").toString();
        Debug.debug("STATE: Parsing completed: " + pID);
        checkIfDoneProcessing();
    }

    /**
     * Handler for {@code paragraph-done} messages. This indicates we've reached the end of processing for the
     * current fragment or document, depending on the document model in force.
     * <p>
     * This is only used for debugging, it doesn't affect the system state, which is tracked utterance-by-utterance.
     * 
     * @see #handleUtteranceDone(KQMLList)
     * @see #waitingList
     */
    private void handleParagraphDone(KQMLList content) {
        String pID = content.getKeywordArg(":id").toString();
        // check if we're waiting for more utterances to be processed
        if (!waitingList.isEmpty()) {
            Debug.error("STATE: Paragraph done: " + pID + ", but waiting on: " + waitingList);
            return;
        }
        Debug.warn("STATE: Paragraph done [IM is done]: " + pID);
        // checkIfDoneProcessing(); -- we handle this at utterance level
    }

    /**
     * Handler for messages from Parser about new speech acts.
     * 
     * @param content
     */
    private void handleNewSpeechAct(KQMLList content) {
        KQMLList act = (KQMLList) content.get(1);
        KQMLObject actType = act.get(0);
        KQMLObject wordsObj = act.getKeywordArg(":words"); // list or NIL

        // case #1: SA failure
        // eg: (UTT :TYPE W::UTT :ROOT NIL :TERMS NIL :UTTNUM 1 :START 0 :END 85 :WORDS NIL :TREE NIL)
        // eg: (:TREE NIL)
        if ((wordsObj == null) || wordsObj.toString().equals("NIL")) {
            Debug.debug("STATE: Parser failure"); // we won't be getting anything meaningful from the IM
            if (waitingList.isEmpty()) {
                // discard clause
                KQMLObject unObject = act.getKeywordArg(":uttnum");
                if (unObject != null) {
                    int uttnum = Integer.parseInt(unObject.toString());
                    if (uttnum == lastUttnumInFragment) {
                        clausesRemaining--;
                        Debug.debug("STATE: Assuming clause failed; " + clausesRemaining
                                + " clauses remaining");
                    }
                } else {
                    // TODO
                    Debug.warn("No :uttnum in SA!?!");
                }
            }
            // now that failed-to-parse leads to a normal flow, i think this is not necessary anymore! TODO: check
            // checkIfDoneProcessing();
            return;
        }
        // case #2: SA
        KQMLList words = (KQMLList) wordsObj;
        // case #2.1: single SA
        if (actType.toString().equalsIgnoreCase("UTT")) {
            int uttnum = Integer.parseInt(act.getKeywordArg(":uttnum").toString());
            newUttnum(uttnum, words);
        } else
        // case #2.2: compound SA (CCA)
        if (actType.toString().equalsIgnoreCase("COMPOUND-COMMUNICATION-ACT")) {
            KQMLList acts = (KQMLList) act.getKeywordArg(":acts");
            KQMLList firstAct = (KQMLList) acts.get(0);
            int uttnum = Integer.parseInt(firstAct.getKeywordArg(":uttnum").toString());
            newUttnum(uttnum, words);
        }
        Debug.warn("STATE: got new SA; waiting on: " + waitingList);
    }

    /**
     * Handler for {@code failed-to-parse} messages.
     * 
     * @param content
     */
    private void handleUtteranceFailed(KQMLList content) {
        KQMLObject uttnumObj = content.getKeywordArg(":uttnum");
        int uttnum = Integer.parseInt(uttnumObj.toString());
        Debug.warn("STATE: failed uttnum: " + uttnum);
        abandonUttnum(uttnum);
        checkIfDoneProcessing();
    }

    /**
     * Handler for messages signaling utterance was fully processed, successfully via {@code end-of-turn} or
     * unsuccessfully, via {@code interpretation-failed}.
     * <p>
     * When last utterance for the current input is done, the data item is considered
     * done as well, which may trigger special actions.
     */
    private void handleUtteranceDone(KQMLList content) {
        KQMLObject wordsObj = content.getKeywordArg(":words");

        // get uttnum
        KQMLObject uttnumObj = content.getKeywordArg(":uttnum");
        // FIXME: is is possible for it to be missing?
        int uttnum = Integer.parseInt(uttnumObj.toString());

        // case #1: IM failure
        // eg, (INTERPRETATION-FAILED :WORDS NIL ...)
        if (wordsObj instanceof KQMLToken) { // must be NIL
            Debug.error("STATE: Pathologic interpretation at uttnum=" + uttnum + ": " + wordsObj);
            abandonUttnum(uttnum);
            checkIfDoneProcessing();
            return;
        }

        KQMLList words = (KQMLList) wordsObj;

        if (waitingList.containsKey(uttnum)) {
            Debug.debug("STATE: Waiting list match for: " + uttnum);
            // in case there were pathological utterances skipped, we clear them up
            clearWaitingList(uttnum);
            // case #2: successful interpretation; we discharge the clause
            if (waitingList.get(uttnum).remove(words)) {
                doneClause(uttnum);
            } else {
                // case #3: fragment: keep waiting
                Debug.error("STATE: Unexpected fragment :uttnum " + uttnum + " :words " + words);
                return;
            }
        } else {
            // case #4: i don't think this is possible anymore (if it ever was)!
            Debug.error("STATE: Stray fragment (ignored) :uttnum " + uttnum + " :words " + words);
            return;
        }
        // FIXME: this is temporary
        // update and save the EKB
        try {
            String ekbFile = kb.saveEKB();
            log("ekb", ekbFile);
        } catch (Exception e) {
            // TODO: what do we do in this situation??? task requester expects something...
            Debug.error("Couldn't save the EKB: " + e);
        }

        //
        checkIfDoneProcessing();
    }

    /**
     * Checks whether we are at the end of processing for the current document.
     * 
     * @see #documentDone()
     */
    private void checkIfDoneProcessing() {
        // have we got the full fragment through?
        if (!gotOK) {
            Debug.debug("STATE: No OK yet");
            return;
        }
        // are we still waiting for utterances to be processed?
        if (!waitingList.isEmpty()) {
            Debug.debug("STATE: Still waiting on: " + waitingList);
            return;
        }
        // are we on the last utterance but waiting for some clauses to make it through?
        if (clausesRemaining > 0) {
            Debug.debug("STATE: " + clausesRemaining + " clauses remaining");
            return;
        }
        // are we done with all the fragments?
        if (splitParagraphsMode || splitOnNewlines) {
            if (fragmentsDone < fragments.length)
                fragmentsDone++;
            int fragsToDo = fragments.length - fragmentsDone;
            Debug.debug("STATE: Fragments done: " + fragmentsDone + " (to do: " + fragsToDo + ")");
            if (fragsToDo == 0) {
                Debug.debug("STATE: Document done [nothing left to do]; documentDone=" + documentDone);
                documentDone();
            } else {
                // send next fragment for tagging
                Debug.debug("STATE: Moving on to next fragment");
                sendTagRequestForFragment();
            }
        } else {
            Debug.debug("STATE: Document done [nothing left to do]; documentDone=" + documentDone);
            documentDone();
        }
    }

    /**
     * Marks that we ended processing on the current document.
     * 
     * @see #finishCurrentDocument()
     */
    private void documentDone() {
        // done
        documentDone = true;
        Debug.debug("STATE: Document done; finishing off");
        // if we used input terms specific to this doc, clear them up
        if (inputTermsFolder != null) {
            this.clearTerms();
        }
        finishCurrentDocument();
    }

    /**
     * Handler for requests asking for the status of this module.
     * <p>
     * Example: {@code (request :receiver DRUM :content (get-status) :reply-with TT123)}
     * 
     * @param msg
     * @param content
     */
    private void handleGetStatus(KQMLPerformative msg) {
        KQMLPerformative perf = new KQMLPerformative("reply");
        KQMLList content = new KQMLList();
        content.add("status");
        content.add(":idle");
        content.add(String.valueOf(dataset.isSelectionEmpty()));
        content.add(":secs-since-last-activity");
        content.add(String.valueOf((now() - timeOfLastSystemActivity) / 1000));
        int toDo = dataset.getSelectionSize();
        if (toDo > 0) {
            content.add(":dataset");
            String kbId = kb.getID();
            content.add(new KQMLString(String.valueOf(kbId == null ? "" : kbId)));
            content.add(":documents-to-do");
            content.add(String.valueOf(toDo));
            if (paragraphId != null) { // we check, in case we get a call before the first dataset gets going
                content.add(":current-paragraph");
                content.add(paragraphId);
            }
            if (taskRequest != null) {
                content.add(":current-task");
                content.add(taskRequest.getParameter(":reply-with"));
            }
        }
        if (splitParagraphsMode) {
            content.add(":paragraphs-to-do");
            content.add(String.valueOf(fragments.length - fragmentsDone));
        }
        if (splitOnNewlines) {
            content.add(":lines-to-do");
            content.add(String.valueOf(fragments.length - fragmentsDone));
        }
        synchronized (taskQueue) {
            if (!taskQueue.isEmpty()) {
                content.add(":scheduled-tasks");
                KQMLList tasks = new KQMLList();
                for (RunTask task : taskQueue) {
                    tasks.add(task.toString());
                }
                content.add(tasks);
            }
        }
        perf.setParameter(":content", content);
        reply(msg, perf);
    }

    /**
     * Finishes processing of the current document. If more documents remain to be
     * processed, calls {@link processSelectedDatatset} to move on to the next one.
     * 
     * @see #processSelectedDataset()
     */
    private void finishCurrentDocument() {
        if (dataset.getSelectionSize() == 0) {
            // this shouldn't happen!
            Debug.debug("no data items to flush!");
            return;
        }

        // update dataset; see if there's anything left
        String done = dataset.popSelection();
        int remaining = dataset.getSelectionSize();
        Debug.debug("STATE: Finished data item: " + done + ". " + remaining + " more to do: "
                + Arrays.toString(dataset.getSelection()));

        // update and save the EKB
        kb.setCompletionStatus(documentDone && (remaining == 0)); // no remaining text in current doc, no remaining docs
        try {
            String ekbFile = kb.saveEKB();
            log("ekb", ekbFile);
        } catch (Exception e) {
            // TODO: what do we do in this situation??? task requester expects something...
            Debug.error("Couldn't save the EKB: " + e);
        }

        // if there are more data items to be processed, keep going
        if (remaining > 0) {
            Debug.debug("STATE: Moving on to next document");
            processSelectedDataset();
        } else {
            // ... otherwise, finish task according to mode
            if (ekbInferenceRequested) {
                // we ask EKBAgent to do inference
                doEKBInference(taskRequest, replyWithEKB);
            } else {
                finishAndCleanup(taskRequest);
            }
        }
    }

    /**
     * Finish task according to mode and clean up or exit
     * 
     * @param taskRequest
     */
    private void finishAndCleanup(KQMLPerformative taskRequest) {
        if (mode != Mode.STANDALONE) {
            // if we have a callback on this document
            if (taskRequest != null) {
                if (replyWhenDone) {
                    boolean haveInferredEKB = !((inferredEKBFileName == null) && (inferredEKBAsKQMLString == null));
                    callback(taskRequest, haveInferredEKB);
                }
                Debug.warn("Task finished (" + taskRequest.getParameter(":reply-with") + ")");
            } else {
                Debug.warn("Task finished (no callback).");
            }
        }

        // quit?
        if (exitWhenDone && taskQueue.isEmpty()) {
            sendExitRequest();
        }
        if (mode == Mode.BATCH) {
            System.exit(0);
        }

        // reset display
        if (display != null) {
            display.unsetState(Display.State.RUNNING);
            display.clearSelections();
            display.enableSelector();
            display.activateExtractorPanel();
        }
    }

    /**
     * Send back reply if task wanted it.
     * 
     * @param taskRequest
     * @param haveInferredEKB
     */
    private void callback(KQMLPerformative taskRequest, boolean haveInferredEKB) {
        if (haveInferredEKB || gotOK) { // otherwise we must have rejected it already
            reply(taskRequest, makeExtractionsResultMessage());
        }
    }

    /**
     * Calls EKBAgent to do inference on the current EKB
     * 
     * @param taskRequest
     */
    private void doEKBInference(KQMLPerformative taskRequest, boolean requestEKB) {
        KQMLPerformative perf = new KQMLPerformative("request");
        KQMLList content = new KQMLList();
        content.add("do-ekb-inference");
        // set domain
        content.add(":domain");
        content.add(getProperty("ekb.reasoner")); // TODO: should throw an exception if no reasoner is set!
        // set raw ekb file
        content.add(":ekb");
        content.add(new KQMLString(kb.getEKBFile()));
        // set response type
        if (requestEKB) {
            content.add(":return-string");
            content.add("t");
        }
        perf.setParameter(":content", content);
        inferredEKBAsKQMLString = null;
        inferredEKBFileName = null;
        sendWithContinuation(perf, new DoInferenceReplyHandler(taskRequest, requestEKB));
    }

    /**
     * Sets the current dataset to a given file in a given folder. As a side effect, if a display is available, it will
     * show the selection in the display.
     *
     * @throws IOException
     *             if the folder doesn't exist
     */
    protected void setDatasetToFile(String folder, String file) throws IOException {
        dataset.setFolder(folder, true);
        // update display
        if (display != null) {
            display.showDataset(false);
        }
        dataset.select(file);
    }

    /**
     * Sets the current dataset to a set of files in a given folder. The files are indicated by a pattern. As a side
     * effect, if a display is available, it will show the selection in the display.
     *
     * @throws IOException
     *             if the folder doesn't exist
     */
    protected void setDatasetWithFilePattern(String folder, String filePattern) throws IOException {
        dataset.setFolder(folder, true);
        // update display
        if (display != null) {
            display.showDataset(false);
        }
        dataset.selectFiles(filePattern);
    }

    /**
     * Sets time of system activity.
     */
    private void setTimeOfSystemActivity() {
        timeOfLastSystemActivity = now();
    }

    /**
     * Get current time (as number of milliseconds since January 1, 1970, 00:00:00 GMT).
     * 
     * @return the number of milliseconds since January 1, 1970, 00:00:00 GMT until now
     */
    private long now() {
        return (new Date()).getTime();
    }

    /**
     * Initiates the processing of a dataset.
     * 
     */
    protected void initiateProcessing(boolean init) {
        // set display
        if (display != null) {
            display.disableSelector();
            display.setState(Display.State.RUNNING);
        }

        // send the input terms
        sendInputTerms();

        // set up the KB, if not done already
        if (init) {
            kb = new DrumKB(properties);
        }

        Debug.info("Initialization complete. Ready to go");

        // go!
        processSelectedDataset();
    }

    /**
     * Processes selected dataset. Does nothing if no dataset is selected.
     * <p>
     * If we have a display, the current data item will be shown in the Text panel.
     * <p>
     * In <em>connected</em> mode, processing a document means sending it out to the system for processing. In
     * <em>standalone</em> mode, the extraction messages are instead read from a file in the <em>cache</em> directory.
     * 
     * @see #sendTagRequest()
     * @see #processCache(String)
     * @see DrumKB#setParagraph(String, String, String)
     */
    protected void processSelectedDataset() {
        if (dataset.isSelectionEmpty()) {
            if (display != null)
                display.unsetState(Display.State.RUNNING);
            Debug.warn("Nothing to do.");
            return;
        }
        //
        documentDone = false;
        fragmentsDone = 0;
        // get data item
        String dataFolder = dataset.getFolder();
        String dataFile = dataset.getFirstSelection();
        // show in selector what item we're processing
        if (display != null) {
            display.showSelected();
        }
        Debug.debug("STATE: Working on: " + dataFile);
        Debug.debug("full list: " + Arrays.toString(dataset.getSelection()));
        currentInputFile = dataFolder + File.separator + dataFile;
        log("input", currentInputFile);
        currentInputData = dataset.getText(dataFile);
        // send start-conversation
        try {
            send(KQMLPerformative.fromString("(tell :content (start-conversation))"));
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        // if we have input terms, send them to TextTagger
        sendInputTermsForDataFile(dataFile);
        // get on with it
        if (mode == Mode.STANDALONE) {
            if (cacheDir != null)
                processCache(dataFile);
        } else {
            // normalize document
            normalizeDocument();
            // set the document in the KB
            kb.setParagraph(currentInputFile, currentInputData);
            Debug.debug("STATE: set-paragraph");
            // and push to the rest of the system
            sendTagRequest();
        }
    }

    /**
     * Processes cache file for a document.
     *
     * The cache file contains messages that would be coming in to this module
     * if the document was processed by the full system.
     * <p>
     * NOTE: It is assumed that each line in the cache file contains a full KQML message! This will fail if KQML
     * messages span multiple lines!!
     * 
     */
    protected void processCache(String fileName) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader(cacheDir + File.separator + fileName));
            String line;
            while ((line = reader.readLine()) != null) {
                // skip if line is empty
                if (line.length() == 0) {
                    continue;
                }
                KQMLPerformative perf = KQMLPerformative.fromString(line);
                String verb = perf.getVerb();
                Object content = perf.getParameter(":content");
                if (verb.equalsIgnoreCase("TELL")) {
                    receiveTell(perf, content);
                } else if (verb.equalsIgnoreCase("REQUEST")) {
                    receiveRequest(perf, content);
                } else if (verb.equalsIgnoreCase("REPLY")) {
                    receiveReply(perf, content);
                } else {
                    // ignore
                }
            }
        } catch (Exception e) {
            Debug.fatal(
                    "Problem processing cache file. Check that the file exists and each line contains a proper KQML message.");
        }
        // just in case we don't have paragraph-done in the cache file
        if (display != null)
            display.unsetState(Display.State.RUNNING);
    }

    /**
     * Processes trace file for a session.
     *
     * The trace file should contain all messages coming in to this module during the session being replayed.
     * <p>
     * Note: This is not guaranteed to work for all sessions!
     */
    protected void processTrace() {
        try {
            KQMLReader reader = new KQMLReader(new FileReader(traceFile));
            while (true) {
                try {
                    KQMLPerformative perf = reader.readPerformative();
                    String verb = perf.getVerb();
                    Object content = perf.getParameter(":content");
                    if (verb.equalsIgnoreCase("TELL")) {
                        receiveTell(perf, content);
                    } else if (verb.equalsIgnoreCase("REQUEST")) {
                        receiveRequest(perf, content);
                    } else if (verb.equalsIgnoreCase("REPLY")) {
                        receiveReply(perf, content);
                    } else {
                        // ignore
                    }
                } catch (EOFException e) {
                    break;
                } catch (KQMLExpectedListException e) {
                    e.printStackTrace();
                    Debug.fatal("Improper KQML in the trace file. Exiting.");
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            Debug.fatal("Can't process the trace file. Exiting.");
        }
        // done:
        if (display == null)
            System.exit(0);
    }

    /**
     * Loads input terms from file.
     * 
     * @param filePath
     * 
     * @see #readTermsFromFile(String)
     */
    protected void loadTermsFromFile(String filePath) {
        inputTerms = readTermsFromFile(filePath);
    }

    /**
     * Add input terms from file.
     * 
     * @param filePath
     * 
     * @see #readTermsFromFile(String)
     */
    protected void addTermsFromFile(String filePath) {
        // TODO: check for duplicates(?)
        inputTerms.addAll(readTermsFromFile(filePath));
    }

    /**
     * Clears the set of input terms.
     * 
     * @see #loadTermsFromFile(String)
     */
    protected void clearTerms() {
        inputTerms = new KQMLList();
        try {
            send(KQMLPerformative.fromString("(request :receiver TextTagger :content (clear-input-terms))"));
        } catch (Exception e) {
            Debug.error(e);
        }
    }

    /**
     * Reads input terms from file.
     * <p>
     * The file must contain each term input definition on a single line. Empty lines are ignored; lines starting with
     * {@literal #} are considered comments and are skipped.
     * <p>
     * The input term definition must conform to the requirements of {@TextTagger} -- see documentation for
     * details.
     * 
     * @param filePath
     */
    protected KQMLList readTermsFromFile(String filePath) {
        Debug.info("Reading input terms from " + filePath);
        File termsFile = new File(filePath);
        KQMLList inputTerms = new KQMLList();
        if (!termsFile.isFile()) {
            Debug.error("Cannot read input terms file (file not found: " + filePath + ")");
            return inputTerms;
        }
        try {
            BufferedReader reader = new BufferedReader(new FileReader(termsFile));
            String line;
            int i = 0;
            while ((line = reader.readLine()) != null) {
                i++;
                if (!line.matches(".*\\S.*")) {
                    continue; // skip blank lines
                }
                if (line.startsWith("#")) {
                    continue; // skip comments
                }
                try {
                    KQMLList term = KQMLList.fromString(line);
                    inputTerms.add(term);
                } catch (IOException e) {
                    Debug.fatal("Badly formed input term definition: " + line);
                }
            }
            Debug.debug("Read " + i + " lines.");
        } catch (Exception e) {
            e.printStackTrace();
        }
        Debug.info("Read " + inputTerms.size() + " input terms.");

        return inputTerms;
    }

    /**
     * Reads input terms from {@code inputTermsFolder} and sends them to {@code TextTagger}.
     * 
     * @param dataFile
     * 
     * @see #inputTermsFolder
     * @see #sendInputTerms(KQMLList)
     */
    private void sendInputTermsForDataFile(String dataFile) {
        if (inputTermsFolder == null) {
            return;
        }
        // strip extension, if any
        int extPos = dataFile.lastIndexOf('.');
        String filename = (extPos > 0) ? dataFile.substring(0, extPos) : dataFile;
        // read corresponding input terms file
        String itFile = inputTermsFolder + File.separator + filename + '.' + inputTermsFileExtension;
        loadTermsFromFile(itFile);
        // send terms over
        sendInputTerms();
    }

    /**
     * Sends input terms to {@code TextTagger}.
     * 
     * @see #inputTerms
     */
    private void sendInputTerms() {
        if (inputTerms.isEmpty()) {
            return;
        }
        KQMLPerformative msg = new KQMLPerformative("request");
        msg.setParameter(":receiver", "TextTagger");
        KQMLList content = new KQMLList();
        content.add("load-input-terms");
        content.add(":input-terms");
        content.add(inputTerms);
        msg.setParameter(":content", content);
        send(msg);
    }

    /**
     * Creates tag message(s) for the current document and sends them off.
     * If in document splitting mode, the document is first split into fragments, then
     * {@link #sendTagRequestForFragment} is called to send a tag request for the first text fragment. This initiates an
     * iterative process by which all fragments from the current document are eventually sent out for tagging.
     * 
     * @see #splitDocumentIntoFragments()
     * @see #sendTagRequestForFragment()
     */
    private void sendTagRequest() {
        currentDatasetIndex++;
        gotOK = false;
        if (splitParagraphsMode || splitOnNewlines) {
            // first, break item into paragraphs
            splitDocumentIntoFragments();
            docMap.clear();
            // and send the first fragment; the rest will be sent off in turn, after each one is finished
            sendTagRequestForFragment();
        } else {
            send(makeTagMessage(currentInputData));
            Debug.debug("STATE: tag request sent");
        }
        fragmentsDone = 0;
    }

    /**
     * Document normalization.
     * <p>
     * For the moment, this function replaces control characters that are not valid
     * XML characters with whitespace (space or newline). Replacement is done character-for-character so that character
     * positions are not affected by this transformation of the input. 
     */
    private void normalizeDocument() {
        currentInputData = currentInputData
                .replace('\u0003', '\n')    // ^C, ETX
                .replace('\u0005', '\n')    // ^E, ENQ
                .replace('\u000C', '\n')    // ^L, FORM-FEED (FF)
                .replace('\u0010', '\n')    // ^P, DLE
                .replace('\u0015', '\n')    // ^U, NAK
                .replaceAll("\\p{Zs}", " ") // all space separators
                .replaceAll("[\\p{Cc}&&[^\\p{Space}]]", " ");// all other control chars
    }

    /**
     * Breaks document into fragments (paragraphs or lines). By default, a paragraph break is defined
     * as too consecutive line terminators, potentially preceded by whitespace.
     * <p>
     * Note: All separators are skipped.
     * <p> TODO: handle unicode separators
     * 
     * @see #fragments
     */
    private void splitDocumentIntoFragments() {
        final String paragraphRegex = "(?s)\\s*+(\\S.*?)(?:(?:\\s*?\\R){2,}+|$)";
        final String lineRegex = "(?m)^\\s*+(\\S.*?)$";
        String splitRegex = (splitOnNewlines) ? lineRegex : paragraphRegex;
        Debug.debug("Splitting doc with: " + splitRegex);
        Pattern fPattern = Pattern.compile(splitRegex);
        Matcher fMatcher = fPattern.matcher(currentInputData);
        Vector<String> frags = new Vector<String>();
        Vector<Integer> offsets = new Vector<Integer>();
        while (fMatcher.find()) {
            frags.add(fMatcher.group(1));
            offsets.add(fMatcher.start(1));
            Debug.debug("frag " + frags.size() + "@"  + offsets.lastElement() + ": " + frags.lastElement());
        }
        
        if (validateParagraphs) {
            // validate fragments
            Iterator itf = frags.iterator();
            Iterator ito = offsets.iterator();
            while (itf.hasNext() && ito.hasNext()) {
                String f = (String) itf.next();
                Integer o = (Integer) ito.next();
                if (! isValidText(f)) {
                    itf.remove();
                    ito.remove();
                }
            }
            Debug.info(frags.size() + " valid fragments");
        }
        int n = frags.size();
        fragments = frags.toArray(new String[n]);
        fragmentOffsets = new int[n];
        for (int i = 0; i < n; i++) {
            fragmentOffsets[i] = offsets.get(i);
        }
    }
    
    /**
     * Checks whether the current paragraph (fragment) is text.
     */
    private boolean isValidText(String text) {
        Debug.debug("validating text: " + text);
        // tokenize
        String[] tokens = text.split("\\s+");
        int nTokens = tokens.length;
        if (nTokens == 0) {
            Debug.warn("text is empty");
            return false;
        }
        Debug.debug("tokens found: " + nTokens);
        // check text characteristics
        int nWordTokens = 0;
        int maxWordSeqLen = 0;
        int maxGapLen = 0;
        double wordDensity;
	double avgWordLen = 0.0;
        int tempWSLen = 0;
        int tempGapLen = 0;
	int tempTotalWordLen = 0;
        for (int i = 0; i<tokens.length; i++) {
            if (isWord(tokens[i])) {
                nWordTokens++;
                tempWSLen++; tempTotalWordLen += tokens[i].length();
                if (tempGapLen > maxGapLen) { maxGapLen = tempGapLen; }
                tempGapLen = 0;
            } else {
                if (tempWSLen > maxWordSeqLen) { maxWordSeqLen = tempWSLen; }
                tempWSLen = 0;
                tempGapLen++;
            }
        }
        if (tempWSLen > maxWordSeqLen) { maxWordSeqLen = tempWSLen; }
        if (tempGapLen > maxGapLen) { maxGapLen = tempGapLen; }
        wordDensity = (double) nWordTokens / nTokens;
	if (nWordTokens > 0)
	    avgWordLen = (double) tempTotalWordLen / nWordTokens;
	
        // validation heuristic: not too many nonwords, either long seq of words or short seqs of non-words; avg word length must be reasonably large
        boolean testW = nWordTokens > 5; 
	boolean testW2 = (nWordTokens * wordDensity) >= 2;
        boolean testWD = wordDensity >= 0.75;
        boolean testLWS = maxWordSeqLen >= 5;
        boolean testLGS = maxGapLen < 5;
	boolean testAWL = avgWordLen >= 3;
        boolean result =
	    (nTokens <= 10) // short sentences are ok
	    ||
	    ( (testW || testW2) && testAWL // sufficient number of actual words
	      && testWD  // not too many non-words
	      && (testLWS // there is at least one reasonably long sequence of actual words
		  || testLGS)); // or the sequences of non-words are fairly short
	
        Debug.debug("word-like tokens: " + nWordTokens + " ["+ (testW ? "OK" : "-") +"]");
        Debug.debug("avg word length: " + String.format("%.2f", avgWordLen) + " ["+ (testAWL ? "OK" : "-") +"]");
        Debug.debug("word density: " + String.format("%.2f", wordDensity) + " ["+ (testWD ? "OK" : "-") +"]");
        Debug.debug("longest word sequence: " + maxWordSeqLen + " ["+ (testLWS ? "OK" : "-") +"]");
        Debug.debug("longest non-word sequence: " + maxGapLen + " ["+ (testLGS ? "OK" : "-") +"]");
        Debug.warn("text is"+ (result ? "" : " not") + " valid w/l/d/s/g=["
		   + nWordTokens + "/"
		   + String.format("%.2f", avgWordLen) + "/"
		   + String.format("%.2f", wordDensity) + "/"
		   + maxWordSeqLen + "/" + maxGapLen + "]");
        return result;
    }

    /**
     * Checks if a token is word-like.
     * <p>
     * A word-like token is one that starts with a letter and contains any sequence of letters, marks or dashes.
     * Initial punctuation (opening quotes or brackets) and final punctuation (we allow a single punctuation character,
     * but otherwise there are no restrictions on its type) are ignored.
     * <p>
     * Of note, number-like tokens are not considered word-like by this function (numbers, currency, dates, etc.).
     * <p>
     * TODO: use a dictionary
     * 
     * @param token
     * @return
     */
    private boolean isWord(String token) {
        return token.matches("^[\\p{Ps}\\p{Pi}]?\\p{L}[\\p{L}\\p{M}\\p{Pd}\\p{Pf}']*[\\p{P}]?$");
    }

    /**
     * Sends tag request for current paragraph. Sent at the beginning of processing when {@link #splitOnNewlines} is
     * {@code true}.
     */
    private void sendTagRequestForFragment() {
        if (fragmentsDone == 0) {
            if (fragments.length == 0) {
                Debug.warn("No fragments!");
                documentDone();
                return;
            }
            // if we handle the paragraph IDs, we need to send start-paragraph
            sendStartParagraph();
        }
        if (fragmentsDone < fragments.length) {
            try {
                gotOK = false;
                // Debug.debug("Processing fragment["+fragmentsProcessed+"]:
                // /"+(fragmentOffsets[fragmentsProcessed])+"/");
                sendWithContinuation(makeTagMessage(fragments[fragmentsDone]),
                        new TagReplyHandler(fragmentsDone));
                Debug.debug("STATE: tag fragment");
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
        // these should not be needed, but, just in case...
        earlyFailures.clear();
        waitingList.clear();
    }

    /**
     * Sends start-paragraph message. Only used when {@link #splitOnNewlines} is {@code true}, at the start of
     * processing.
     * 
     */
    private void sendStartParagraph() {
        if (! splitOnNewlines) {
            return;
        }
        try {
            send(KQMLPerformative.fromString("(tell :content (start-paragraph :id paragraph" + currentDatasetIndex
                    + "))"));
            Debug.debug("STATE: start-paragraph");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    /**
     * Sends end-paragraph message. Only used when {@link #splitOnNewlines} is {@code true}, at the end of processing.
     * 
     */
    private void sendEndParagraph() {
        if (splitOnNewlines && (fragmentsDone + 1 == fragments.length)) {
            try {
                send(KQMLPerformative.fromString("(tell :content (end-paragraph :id paragraph"
                        + currentDatasetIndex + "))"));
                Debug.debug("STATE: end-paragraph");
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
    }

    /**
     * Sets up the waiting list for utterances.
     * 
     * @param uttnums
     * 
     * @see #waitingList
     */
    private void addUttnumsToWaitingList(KQMLList uttnums) {
        if (uttnums == null)
            return;
        ListIterator<KQMLObject> iterator = uttnums.listIterator();
        while (iterator.hasNext()) {
            int uttnum = Integer.parseInt(iterator.next().toString());
            if (earlyFailures.contains(uttnum)) {
                earlyFailures.remove(new Integer(uttnum));
                Debug.debug("STATE: skipped early failure uttnum=" + uttnum);
            } else {
                newUttnum(uttnum, null);
            }
        }
    }

    /**
     * Removes entry for {@code uttnum} from {@link #waitingList}.
     * 
     * @param uttnum
     * 
     * @see #waitingList
     */
    private void doneClause(int uttnum) {
        if (waitingList.get(uttnum).isEmpty()) {
            waitingList.remove(uttnum);
            Debug.debug("STATE: Utterance done :uttnum " + uttnum + " (waiting for " + lastUttnumInFragment + ")");
        }
        // if we're on the last utterance, skip clause
        if (uttnum == lastUttnumInFragment) {
            clausesRemaining--;
            Debug.debug("STATE: Utterance done " + clausesRemaining + " clauses remaining");
        }
    }

    /**
     * Adds new uttnum to {@link #waitingList}.
     *  
     * @param uttnum Utterance number
     * @param words List of words (may be {@code null}, if not known yet) 
     */
    private void newUttnum(int  uttnum, KQMLList words) {
        if (waitingList.containsKey(uttnum)) {
            if (words != null && !words.isEmpty()) {
                waitingList.get(uttnum).add(words);
            }
        } else {
            ArrayList<KQMLList> clauses = new ArrayList<KQMLList>();
            if (words != null && !words.isEmpty()) {
                clauses.add(words);
            }
            waitingList.put(uttnum, clauses);
        }
    }
    
    /**
     * Removes entry for {@code uttnum} from {@link #waitingList}.
     * 
     * @param uttnum
     * 
     * @see #waitingList
     */
    private void abandonUttnum(int uttnum) {
        // if we're on the last utterance, skip clause
        if ((uttnum == lastUttnumInFragment) && (clausesRemaining > 0)) {
            clausesRemaining--;
            Debug.error("STATE: Skipped clause; " + clausesRemaining + " clauses remaining");
            if (clausesRemaining > 0) {
                // we're still waiting for END-OF-TURN
                return;
            }
        }
        // we keep track of interpretation-failed before the full paragraph got OK-ed
        if (gotOK) {
            ArrayList<KQMLList> clause = waitingList.remove(uttnum);
            Debug.error("STATE: Abandoned: uttnum=" + uttnum + " " + clause);
        } else {
            earlyFailures.add(uttnum);
            Debug.error("STATE: Early failure: uttnum=" + uttnum);
        }
    }

    /**
     * Clear waiting list up to (but not including) {@code uttnum}.
     * 
     * @param uttnum
     * 
     * @see #waitingList
     */
    private void clearWaitingList(int uttnum) {
        for (Iterator<Entry<Integer, ArrayList<KQMLList>>> it = waitingList.entrySet().iterator(); it.hasNext();) {
            Entry<Integer, ArrayList<KQMLList>> entry = it.next();
            if (entry.getKey() < uttnum) {
                it.remove();
                Debug.debug("STATE: Removed waiting list entry: " + entry);
            }
        }
    }

    /**
     * Sets utterance offsets for the current paragraph of the current document.
     * <p>
     * Note: This depends on utterances having been added already to the {@code kb}.
     * 
     * @see DrumKB#setOffset(int, int)
     * @see #kb
     */
    protected void setUtteranceOffsetsForParagraph(int index, KQMLList uttnums) {
        if (uttnums == null)
            return;
        ListIterator<KQMLObject> iterator = uttnums.listIterator();
        while (iterator.hasNext()) {
            int uttnum = Integer.parseInt(iterator.next().toString());
            kb.setOffset(uttnum, fragmentOffsets[index]);
            docMap.put(uttnum, index);
        }
    }

    /**
     * Makes tag message.
     * 
     * @see #sendTagRequest()
     */
    private KQMLPerformative makeTagMessage(String text) {
        try {
            String message = "(tag :text "
                    + (new KQMLString(text)).toString()
                    + " :imitate-keyboard-manager T"
                    + " :next-uttnum " + (lastUttnumInFragment + 1)
                    + ")";
            KQMLList content = KQMLList.fromString(message);
            // when we break the input document ourselves, we need to send :paragraph nil
            if (splitOnNewlines) {
                content.add(":paragraph");
                content.add("nil");
            }
            // append tag options
            ListIterator<KQMLObject> iterator = tagOptions.listIterator();
            while (iterator.hasNext()) {
                content.add(iterator.next());
            }

            KQMLPerformative perf = new KQMLPerformative("request");
            perf.setParameter(":receiver", "TextTagger");
            perf.setParameter(":content", content);
            return perf;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Gets tag options as string.
     */
    protected KQMLList getTagOptions() {
        return tagOptions;
    }

    /**
     * Sets tag options.
     * 
     * @see #tagOptions
     * @see #defaultTagOptions
     */
    protected void setTagOptions(KQMLList options) {
        String oldTagOptions = tagOptions.toString();
        if (options == null) { // revert to default
            tagOptions = defaultTagOptions;
        } else { // switch to new ones, but add default ones unless changed
            int n = defaultTagOptions.size();
            for (int i = 0; i < n; i += 2) {
                String param = defaultTagOptions.get(i).toString();
                String oldValue = defaultTagOptions.get(i + 1).toString();
                KQMLObject newValueObj = options.getKeywordArg(param);
                if (newValueObj == null) {
                    options.add(param);
                    options.add(oldValue);
                }
            }
            tagOptions = options;
        }
        Debug.info("set-tag-options from: " + oldTagOptions + " to: " + tagOptions);
    }

    /**
     * Makes reply message containing extractions.
     */
    private KQMLPerformative makeExtractionsResultMessage() {
        KQMLPerformative perf = new KQMLPerformative("reply");
        KQMLList content = new KQMLList();
        content.add("result");
        content.add(":uttnums");
        content.add(kb.getUttnums());
        if (replyWithEKB) {
            content.add(":ekb");
            content.add(new KQMLString(kb.toXML()));
        } else {
            content.add(":ekb-file");
            content.add(new KQMLString(kb.getEKBFile()));
        }
        perf.setParameter(":content", content);
        if (ekbInferenceRequested) {
            if (replyWithEKB) {
                if (inferredEKBAsKQMLString != null) {
                    content.add(":inferred-ekb");
                    content.add(inferredEKBAsKQMLString);
                }
            } else {
                if (inferredEKBFileName != null) {
                    content.add(":inferred-ekb-file");
                    content.add(new KQMLString(inferredEKBFileName));
                }
            }
        }
        return perf;
    }

    /**
     * Creates log file.
     */
    private void initLog() {
        if (!logging)
            return;
        try {
            log = new Log(name + ".log");
        } catch (IOException ex) {
            error("error opening log file: " + ex);
        }
    }

    /**
     * Writes string to log file, using a specified entry tag. Defaults to writing to STDERR if log file is not defined.
     *
     * @param tag
     *            Entry tag
     * @param text
     *            String to write.
     * @see #logging
     */
    private void log(String tag, String text) {
        if (log != null) {
            log.log(tag, text);
        } else {
            System.err.println(tag + ": " + text);
        }
    }

    /**
     * Closes log file.
     */
    private void closeLog() {
        if (log != null) {
            log.close();
        }
    }

    /**
     * Delete selected dataset.
     */
    protected void deleteSelectedDataset() {
        dataset.removeSelectedFiles();
        if (display != null) {
            display.showDataset(true);
        }
    }

    /**
     * Reply with accepted message.
     * 
     * @throws RuntimeException
     */
    private void sendAcceptedTask()
            throws RuntimeException {
        // get EKB filename and return it to sender
        String ekbFilename = kb.saveEKB();
        KQMLPerformative rmsg = new KQMLPerformative("reply");
        KQMLList rcontent = new KQMLList();
        rcontent.add("accepted");
        rcontent.add(":result");
        rcontent.add(new KQMLString(ekbFilename));
        rmsg.setParameter(":content", rcontent);
        reply(taskRequest, rmsg);
    }

    /**
     * Requests that system exit.
     */
    private void sendExitRequest() {
        KQMLPerformative perf = null;
        try {
            // currently only the Parser subscribes to the exit request, and doesn't seem to act on it
            // perf = KQMLPerformative.fromString("(request :content (exit 0))");
            perf = KQMLPerformative.fromString("(request :receiver facilitator :content (exit))");
            send(perf);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    /**
     * Sends a message. Wrapper for {@code super.send()} with logging.
     */
    protected synchronized void send(KQMLPerformative msg) {
        log("sent", msg.toString());
        super.send(msg);
    }

    /**
     * Handler for replies to tag messages for fragments.
     */
    protected class TagReplyHandler implements KQMLContinuation {
        int fragmentNumber;

        public TagReplyHandler(int index) {
            fragmentNumber = index;
        }

        public void receive(KQMLPerformative replyMsg) {
            KQMLObject content = replyMsg.getParameter(":content");
            if (!(content instanceof KQMLList)) {
                error("Bad message format");
                return;
            }
            String reply = ((KQMLList) content).get(0).toString();
            if (reply.equalsIgnoreCase("ok")) {
                log("received", replyMsg.toString());
                KQMLObject uttnums = ((KQMLList) content).getKeywordArg(":uttnums");
                Debug.debug("STATE: got OK - uttnums: " + uttnums);
                if (uttnums != null) {
                    addUttnumsToWaitingList((KQMLList) uttnums);
                    setUtteranceOffsetsForParagraph(fragmentNumber, (KQMLList) uttnums);
                    Debug.debug("fragment " + fragmentNumber + " => uttnums " + uttnums);
                    gotOK = true;
                    // if we handle paragraph ids, we send out end-paragraph
                    sendEndParagraph();
                } else {
                    errorReply(replyMsg, "Missing :uttnums values");
                    // we keep going, but with unpredictable behavior
                }
            } else { // if reject, use the top-level handler
                receiveReply(replyMsg, content);
            }
        }
    }

    /**
     * Handler for replies to pub-pull messages.
     */
    protected class PubPullReplyHandler implements KQMLContinuation {
        // original message that triggered the pub-pull message
        KQMLPerformative msg;
        KQMLList cmd;

        public PubPullReplyHandler(KQMLPerformative msg, KQMLList content) {
            this.msg = msg;
            this.cmd = content;
        }

        public void receive(KQMLPerformative replyMsg) {
            log("received", replyMsg.toString());
            KQMLObject content = replyMsg.getParameter(":content");
            if (!(content instanceof KQMLList)) {
                error("Bad message format");
                return;
            }
            String reply = ((KQMLList) content).get(0).toString();
            if (reply.equalsIgnoreCase("done")) {
                KQMLObject folder = ((KQMLList) content).getKeywordArg(":output-folder");
                Debug.debug("Will process pmcid paragraphs from folder: " + folder);
                // update task params
                cmd.add(":folder");
                cmd.add(folder);
                msg.setParameter(":content", cmd);
                // and re-submit the task
                handleTaskRequest(msg, cmd);
            } else if (reply.equalsIgnoreCase("failure")) {
                KQMLObject errorMsg = ((KQMLList) content).getKeywordArg(":msg");
                if (errorMsg != null) {
                    errorReply(msg, errorMsg.stringValue());
                } else {
                    Debug.debug("Got a failure with no error message");
                    errorReply(msg, "Unknown error");
                }
            } else { // TODO: handle reject
                error("Cannot handle reply");
                return;
            }
        }
    }

    /**
     * Handler for replies to do-ekb-inference messages.
     */
    protected class DoInferenceReplyHandler implements KQMLContinuation {
        private KQMLPerformative task;
        private boolean requestEKB;

        public DoInferenceReplyHandler(KQMLPerformative task, boolean requestEKB) {
            this.task = task;
            this.requestEKB = requestEKB;
        }

        public void receive(KQMLPerformative replyMsg) {
            log("received", replyMsg.toString());
            KQMLObject content = replyMsg.getParameter(":content");
            if (!(content instanceof KQMLList)) {
                error("Bad message format");
                return;
            }
            String replyVerb = ((KQMLList) content).get(0).toString();
            if (replyVerb.equalsIgnoreCase("done")) {
                KQMLObject result = ((KQMLList) content).getKeywordArg(":result");
                Debug.debug("EKBAgent returned:" + result);
                if (requestEKB) {
                    inferredEKBAsKQMLString = (KQMLString) result;
                } else {
                    inferredEKBFileName = result.stringValue();
                }
            } else if (replyVerb.equalsIgnoreCase("failure")) {
                // we log the error, but otherwise don't do anything
                KQMLObject errorMsg = ((KQMLList) content).getKeywordArg(":msg");
                if (errorMsg != null) {
                    Debug.debug("Got a failure:" + errorMsg);
                } else {
                    Debug.debug("Got a failure with no error message");
                }
            } else { // TODO: handle reject
                error("Cannot handle reply");
            }
            // finally, try callback
            finishAndCleanup(task);
        }
    }

    /**
     * Class for representing tasks received via one of the {@code run-*} requests.
     */
    protected class RunTask {
        KQMLPerformative msg;
        KQMLList content;

        public RunTask(KQMLPerformative msg, KQMLList content) {
            this.msg = msg;
            this.content = content;
        }

        public String toString() {
            // TODO
            KQMLObject replyWith = msg.getParameter(":reply-with");
            if (replyWith == null) {
                return "ANONYMOUS";
            }
            return replyWith.toString();
        }
    }

    /**
     * Scheduler for running tasks off the task queue.
     * 
     * TODO: Documentation says ScheduledThreadPoolExecutor might be a more flexible replacement for Timer/TimerTask.
     * 
     * @author lgalescu
     * @see DrumGUI#taskQueue
     * @see DrumGUI#handleTaskRequest(KQMLPerformative, Object)
     */
    protected class TaskScheduler extends TimerTask {
        public void run() {
            synchronized (taskQueue) {
                if (taskQueue.isEmpty() || (dataset.getSelectionSize() > 0)
                        || (now() - timeOfLastSystemActivity < 2500)) // TODO: replace this hack w/ smthg proper
                /* (we need to wait for the ekb to be written, i think) */
                {
                    return;
                }

                RunTask nextTask = taskQueue.remove(0);
                Debug.debug("Removed task from queue: " + nextTask.content);
                handleTaskRequest(nextTask.msg, nextTask.content);
            }
        }
    }

}

/* 
 * Display.java
 *
 * $Id: Display.java,v 1.25 2018/11/08 21:25:41 lgalescu Exp $
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>,  3 Jul 2010
 */

package TRIPS.DrumGUI;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FileDialog;
import java.awt.Font;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JToolBar;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileFilter;

import TRIPS.KQML.KQMLList;

public class Display extends JFrame
 implements ActionListener
{
    // GUI state
    public enum State {
        // TODO: is WAITING still necessary??? ~WAITING <=> READY & DATASET
        WAITING(0), // 1.7: (0b0000000), // initial state
        READY(1), // 1.7: (0b0000001), // system (TextTagger) ready
        DATASET(2), // 1.7: (0b0000010), // have data file selected
        EDITING(4), // 1.7: (0b0000100), // user is editing text
        SETTING(8), // 1.7: (0b0001000), // user is setting system options
        TAGOPT(16), // 1.7: (0b0010000), // user is adding TextTagger options
        TERMDEF(32), // 1.7: (0b0100000), // user is modifying input terms for TextTagger
        RUNNING(64); // 1.7: (0b1000000); // system is running, not ready
        private final int value;

        private State(int value) {
            this.value = value;
        };

        private int getValue() {
            return value;
        }

        private boolean is(int value) {
            return this.value == value;
        }

        private boolean matches(int value) {
            return (this.value & value) == this.value;
        }

        private int set(int value) {
            return this.value | value;
        }

        private int unset(int value) {
            return ~this.value & value;
        }

        private int flip(int value) {
            return this.value ^ value;
        }
    }
    private int state;

    // buttons: <action, tooltip, imagePath>
    /*
     * List of menu buttons.
     * <p>
     * This array implicitly constrains the order in which buttons are displayed. Each element of the array is itself an
     * array composed of three elements: the action name, the tooltip, and the image used to decorate the button.
     */
    private static String[][] MenuButtons =
    {
            { "load", "Open folder", "images/folder.png" },
            { "view", "View", "images/file.png" },
            { "edit", "Edit", "images/edit.png" },
            { "delete", "Delete", "images/delete.png" },
            { "zoomin", "Zoom in", "images/zoomin.png" },
            { "zoomout", "Zoom out", "images/zoomout.png" },
            { "settings", "Settings", "images/gear.png" },
            { "terms", "Terms", "images/list.png" },
            { "loadterms", "Load", "images/load.png" },
            { "addterms", "Add", "images/plus.png" },
            { "clearterms", "Clear", "images/x.png" },
            { "tag", "Tag Options", "images/tag.png" },
            { "run", "Run", "images/play.png" }
    };

    // parent
    private DrumGUI parent;

    // gui objects
    protected JToolBar toolbar;
    protected SelectorPanel selector;
    protected TextPanel textPanel;
    protected ExtractorPanel extractorPanel;
    private Vector<JButton> buttons = new Vector<JButton>();
    private TagOptionsInput toi;

    private static Object[] editDialogOptions = { "Save", "Discard", "Cancel", };
    private static Object[] deleteDialogOptions = { "Delete", "Cancel", };

    // fonts
    static private int defaultFontSize = 12;
    static private int minFontSize = 8;
    static private int maxFontSize = 24;
    protected int fontSize;

    /** Constructor
     */
    public Display(DrumGUI module, String title) {
        super();
        parent = module;
        setTitle(title);
        setFontSize(defaultFontSize);

        createWidgets();
        addWidgetsToFrame();
        pack();
        setVisible(true);

        setState(State.WAITING);
    }


    /** Creates main GUI components
     */
    protected void createWidgets() {
        selector = new SelectorPanel(this, parent.dataset);
        textPanel = new TextPanel();
        extractorPanel = new ExtractorPanel(textPanel);
    }

    /**
     */
    protected void addWidgetsToFrame() {
        // size
        selector.setMinimumSize(new Dimension(150, 250));
        textPanel.setMinimumSize(new Dimension(300, 100));
        extractorPanel.setMinimumSize(new Dimension(300, 100));
        setPreferredSize(new Dimension(800, 500));
        setMinimumSize(new Dimension(500, 250));
        setResizable(true);

        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        // layout:
        /*
         * [T / [S | [D / X]]]
         * T: buttons & logo
         * T: | M - L |
         * M: menu buttons:
         * -- main menu: | folder | view | delete | edit | settings | go |
         * -- settings menu: | settings | termdef | tagopt |
         * -- termdef menu: | termdef | load | add| clear |
         * L: logo (ihmc man)
         * S: selector panel
         * D: text (document) panel
         * X: extraction panel
         * TODO: drop-down selector by type; filter, etc
         */

        // layout: we'll use BorderLayout
        Container pane = getContentPane();
        pane.setLayout(new BorderLayout(5, 5));

        // some general stuff
        TitledBorder titledBorder;
        Font titleFont = new Font("Helvetica", Font.BOLD | Font.ITALIC, fontSize + 2);

        // ((JComponent)
        // pane).setBorder(BorderFactory.createEmptyBorder(3,3,3,3));

        // toolbar
        toolbar = new JToolBar();
        toolbar.setFloatable(false);
        toolbar.setBackground(new Color(230, 230, 230));
        toolbar.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createMatteBorder(0, 0, 1, 0, new Color(190, 190, 190)),
                BorderFactory.createEmptyBorder(2, 2, 2, 2)));

        // toolbar: buttons
        addButtons(toolbar);

        // make left panel ([logo] + selector + "go" button)
        JPanel leftPanel = new JPanel();
        leftPanel.setLayout(new BoxLayout(leftPanel, BoxLayout.Y_AXIS));

        // left: list to perform selection
        titledBorder = BorderFactory.createTitledBorder("Select input file:");
        titledBorder.setTitleFont(titleFont);
        selector.setBorder(titledBorder);
        leftPanel.add(selector);

        // right-top: textPanel
        // add a border
        titledBorder = BorderFactory.createTitledBorder("Text:");
        titledBorder.setTitleFont(titleFont);
        textPanel.setBorder(BorderFactory.createCompoundBorder(titledBorder, BorderFactory.createLoweredBevelBorder()));

        // right-bottom: extractorPanel
        titledBorder = BorderFactory.createTitledBorder("Extracted data:");
        titledBorder.setTitleFont(titleFont);
        extractorPanel.setBorder(BorderFactory.createCompoundBorder(titledBorder,
                BorderFactory.createLoweredBevelBorder()));

        // split panes
        JSplitPane splitPane1 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        JSplitPane splitPane2 = new JSplitPane(JSplitPane.VERTICAL_SPLIT);

        splitPane1.setOneTouchExpandable(true);
        splitPane1.setBorder(null);

        splitPane2.setBorder(null);

        splitPane2.setLeftComponent(textPanel);
        splitPane2.setRightComponent(extractorPanel);
        splitPane1.setLeftComponent(leftPanel);
        splitPane1.setRightComponent(splitPane2);

        // finally: add top-level components to ContentPane
        add(toolbar, BorderLayout.PAGE_START);
        pane.add(splitPane1, BorderLayout.CENTER);

        // FIX ME: selector panel contents doesn't always show at startup
    }

    /** makeButtonPanel()
     */
    protected void addButtons(JToolBar toolbar) {
        JButton button;
        Icon icon;
        Color buttonBgColor = new Color(0, 0, 0);

        // make buttons
        for (int i = 0; i < MenuButtons.length; i++) {
            String[] buttonSpec = MenuButtons[i];
            button = new JButton();
            icon = createImageIcon(buttonSpec[2], 30);
            button.setIcon(icon);
            button.setToolTipText(buttonSpec[1]);
            button.setBorder(BorderFactory.createRaisedBevelBorder());
            button.setActionCommand(buttonSpec[0]);
            button.addActionListener(this);
            button.setAlignmentX(Component.CENTER_ALIGNMENT);
            toolbar.add(button);
            buttons.add(button);
        }

        // glue
        toolbar.add(Box.createHorizontalGlue());

        // logo
        icon = createImageIcon("images/ihmcLogo.png", 30);
        JLabel label = new JLabel(icon);
        label.setAlignmentX(Component.RIGHT_ALIGNMENT);
        toolbar.add(label);
    }

    private ImageIcon createImageIcon(String path, int size) {
        java.net.URL imgURL = getClass().getResource(path);
        if (imgURL != null) {
            ImageIcon imageIcon = new ImageIcon(imgURL);
            Image scaledImage = (imageIcon.getImage()).getScaledInstance(size, size, java.awt.Image.SCALE_SMOOTH);
            return new ImageIcon(scaledImage);
        } else {
            Debug.error("createImageIcon: couldn't load file: " + path);
            return null;
        }
    }

    public void actionPerformed(ActionEvent e) {
        String cmd = e.getActionCommand();
        // Debug.debug("action = " + cmd);
        if (cmd.equals("quit")) {
            // quit();
        } else if (cmd.equals("load")) {
            clearOutputPanels();
            selectDataDir();
        } else if (cmd.equals("view")) {
            showSelected();
        } else if (cmd.equals("zoomin")) {
            zoomIn();
        } else if (cmd.equals("zoomout")) {
            zoomOut();
        } else if (cmd.equals("delete")) {
            int n = JOptionPane.showOptionDialog(this, new JLabel("Will delete selected files!", JLabel.CENTER), "",
                    JOptionPane.OK_CANCEL_OPTION, JOptionPane.WARNING_MESSAGE, null, deleteDialogOptions,
                    deleteDialogOptions[1]);
            if (n == 0) { // delete
                parent.deleteSelectedDataset();
                unsetState(State.DATASET);
            } else { // cancel
            }
        } else if (cmd.equals("edit")) {
            // FIXME: if we have a data item selected, we want to edit it
            if (!State.EDITING.matches(state)) {
                setState(State.EDITING);
                showSelected();
                textPanel.setEditable(true);
            } else {
                if (textPanel.isEdited()) {
                    int n = JOptionPane.showOptionDialog(this, new JLabel("The document has unsaved changes!",
                            JLabel.CENTER), "", JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE, null,
                            editDialogOptions, editDialogOptions[2]);
                    Debug.debug("option selected: " + n);
                    if (n == 0) { // save
                        if (saveText()) {
                            textPanel.setEditable(false);
                            unsetState(State.EDITING);
                        } else {
                            // nop: resume
                        }
                    } else if (n == 1) { // discard
                        // TODO: reload
                        textPanel.discardEdits();
                        textPanel.setEditable(false);
                        unsetState(State.EDITING);
                    } else { // cancel
                    }
                } else { // cancel edit mode
                    textPanel.setEditable(false);
                    unsetState(State.EDITING);
                }
                // TODO: select edited item
            }
        } else if (cmd.equals("settings")) {
            flipState(State.SETTING);
        } else if (cmd.equals("terms")) {
            flipState(State.TERMDEF);
        } else if (cmd.equals("loadterms")) {
            String path = chooseFile();
            if (path != null) {
                Debug.info("Input terms file: " + path);
                parent.loadTermsFromFile(path);
            }
        } else if (cmd.equals("addterms")) {
            String path = chooseFile();
            if (path != null) {
                Debug.info("Input terms file: " + path);
                parent.addTermsFromFile(path);
            }
        } else if (cmd.equals("clearterms")) {
            Debug.info("Clearing input terms");
            parent.clearTerms();
        } else if (cmd.equals("tag")) {
            flipState(State.TAGOPT);
            if (State.TAGOPT.matches(state)) {
                toi = new TagOptionsInput(this);
            } else {
                toi.cancel();
                toi = null;
            }
        } else if (cmd.equals("run")) {
            clearOutputPanels();
            parent.initiateProcessing(true);
        }
    }

    private String chooseFile() {
        FileDialog dialog = new FileDialog(this, "Choose Input Terms File", FileDialog.LOAD);
        dialog.setDirectory(getDataDir());
        dialog.setFile("*.*");
        dialog.setVisible(true);

        String dir = dialog.getDirectory();
        String file = dialog.getFile();
        if (dir == null || file == null)
            return null;
        else
            return dir + file;
    }

    private void enableAllButtons(boolean enabled) {
        Iterator<JButton> iterator = buttons.iterator();
        while (iterator.hasNext()) {
            iterator.next().setEnabled(enabled);
        }
    }

    private void enableButton(String command, boolean enabled, boolean visible) {
        // Debug.debug("button [" + command + "] enabled: " + enabled);
        Iterator<JButton> iterator = buttons.iterator();
        while (iterator.hasNext()) {
            JButton button = iterator.next();
            if (button.getActionCommand().equalsIgnoreCase(command)) {
                button.setEnabled(enabled);
                button.setVisible(visible);
            }
        }
    }

    private void enableButton(String command, boolean enabled) {
        enableButton(command, enabled, enabled);
    }

    private void enableButtonOnly(String command) {
        // Debug.debug("button [" + command + "] exclusively enabled");
        Iterator<JButton> iterator = buttons.iterator();
        while (iterator.hasNext()) {
            JButton button = iterator.next();
            if (button.getActionCommand().equalsIgnoreCase(command)) {
                button.setEnabled(true);
                button.setVisible(true);
            } else {
                button.setEnabled(false);
                button.setVisible(false);
            }
        }
    }

    private void updateButtons() {
        if (State.RUNNING.matches(state)) {
            enableAllButtons(false);
            return;
        }
        if (State.EDITING.matches(state)) {
            enableButtonOnly("edit");
            return;
        } else {
            // default: available
            enableButton("edit", true);
        }
        if (State.SETTING.matches(state)) {
            if (State.TAGOPT.matches(state)) {
                enableButtonOnly("tag");
                return;
            }
            if (State.TERMDEF.matches(state)) {
                enableButtonOnly("terms");
                enableButton("loadterms", true);
                enableButton("addterms", true);
                enableButton("clearterms", true);
                return;
            } else {
                enableButton("loadterms", false); // only available in the TERMDEF menu
                enableButton("addterms", false); // only available in the TERMDEF menu
                enableButton("clearterms", false); // only available in the TERMDEF menu
            }
            enableButtonOnly("settings");
            enableButton("terms", true);
            enableButton("tag", true);
            return;
        } else {
            enableButton("settings", true); // always available
            enableButton("tag", false); // only available in the SETTING menu
            enableButton("terms", false); // only available in the SETTING menu
            enableButton("loadterms", false); // only available in the TERMDEF menu
            enableButton("addterms", false); // only available in the TERMDEF menu
            enableButton("clearterms", false); // only available in the TERMDEF menu
        }

        enableButton("load", true);
        enableButton("zoomin", true);
        enableButton("zoomout", true);
        enableButton("run", State.READY.matches(state) && State.DATASET.matches(state));
        enableButton("view", State.DATASET.matches(state));
        enableButton("delete", State.DATASET.matches(state));

        repaint(); // without this sometimes buttons fail to update -- no idea why!!!
    }

    /**
     * Flips state. Only valid for flippable states.
     */
    protected void flipState(State change) {
        // Debug.debug("cur state = " + Integer.toBinaryString(state));
        if ((change == State.READY) || (change == State.WAITING))
        { // not a flippable state
            Debug.error("Unexpected flipState request: " + change);
            return;
        }
        state = change.flip(state);
        // Debug.debug("new state = " + Integer.toBinaryString(state));
        updateButtons();
    }

    /** 
     */
    protected void setState(State change) {
        // Debug.debug("cur state = " + Integer.toBinaryString(state));
        if (change == State.WAITING) { // revert to initial state -- this excludes all others
            state = State.WAITING.getValue();
        } else { // these states may be set multiple times
            state = change.set(state);
        }
        // Debug.debug("new state = " + Integer.toBinaryString(state));
        updateButtons();
    }

    /** 
     */
    protected void unsetState(State change) {
        // Debug.debug("cur state = " + Integer.toBinaryString(state));
        state = change.unset(state);
        // Debug.debug"new state = " + Integer.toBinaryString(state));
        updateButtons();
    }

    /**
     */
    protected void clearOutputPanels() {
        Debug.debug("Clearing output panels");
        textPanel.clear();
        extractorPanel.clear();
    }

    /**
     * Shows some text in the {@link #textPanel}.
     */
    protected void textDisplay(String s) {
        textPanel.setText(s);
    }

    private void textHighlight(int start, int end) {
        textPanel.setHighlight(start, end);
    }
    private void textRemoveHighlight() {
        textPanel.removeHighlight();
    }

    protected void showTextNoSelector(String text) {
        hideSelector();
        clearOutputPanels();
        textDisplay(text);
    }

    /**
     * Sets selector panel selection mode.
     */
    protected void setSelectorMode(int mode) {
        selector.setSelectionMode(mode);
    }

    protected void enableSelector() {
        selector.setPushSelectionEnabled(true);
        Debug.debug("Selector enabled");
    }

    protected void disableSelector() {
        selector.setPushSelectionEnabled(false);
        Debug.debug("Selector disabled");
    }

    /**
     * Hides the selector panel.
     */
    private void hideSelector() {
        // nop: probably not a good idea to ever hide it, after all
    }

    /**
     * Shows the dataset in the {@link #selector} panel. If {@code update} is
     * {@code true}, it forces the {@link #selector} to first reload the
     * dataset.
     */
    protected void showDataset(boolean update) {
        if (update) {
            selector.getDataset().getFiles();
        }
        selector.refresh();
        unsetState(State.DATASET);
    }

    /**
     * Resets output panels and displays the text of first selection in
     * {@link #textPanel}.
     */
    protected void showSelected() {
        clearOutputPanels();
        // if we have multiple selections, we only show the first
        // but we must notify the user that this is what they see
        if (selector.getDataset().isSelectionEmpty()) {
            return;
        }
        String filename = selector.getDataset().getFirstSelection();
        showSelectedFile(filename);
        showSelectedText(filename);
    }

    /**
     * Shows the selected file in the {@link #selector} panel.
     */
    protected void showSelectedFile(String filename) {
        selector.showSelected(filename);
    }

    /**
     * Clears all selections in the {@link #selector} panel.
     */
    protected void clearSelections() {
        selector.clearSelections();
    }

    /**
     * Shows the contents of the selected file in the {@link #textPanel}.
     */
    protected void showSelectedText(String filename) {
        String dataString = selector.getDataset().getText(filename);
        textDisplay(dataString);
    }

    /**
     * Sets current data directory.
     */
    protected void setDataDir(String d) {
        try {
        selector.getDataset().setFolder(d, true);
        showDataset(false);
        } catch (IOException e) {
            Debug.error(e.getMessage());
        }
    }

    /**
     * Gets current data directory.
     */
    protected String getDataDir() {
        return selector.getDataset().getFolder();
    }

    /**
     * Select current data directory.
     */
    private void selectDataDir() {
        JFileChooser fc = new JFileChooser();
        String dataDir = getDataDir();
        File currDir = null;
        try {
            currDir = (new File((dataDir == null) ? "." : dataDir)).getCanonicalFile();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        fc.setCurrentDirectory(currDir);
        fc.setDialogTitle("Select data directory");
        fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        fc.setAcceptAllFileFilterUsed(false);
        fc.setApproveButtonText("Open");
        fc.setFileFilter(new FileFilter() {
            public boolean accept(File f) {
                return f.isDirectory();
            }
            public String getDescription() {
                return "Any folder";
            }
        });
        //
        if (fc.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
            File f = fc.getSelectedFile();
            setDataDir(f.getAbsolutePath());
        }
    }

    /**
     */
    protected void clearExtractorPanel() {
        extractorPanel.clear();
        extractorPanel.setTreeOperationsEnabled(false);
    }

    /**
     * Sets extractor panel display mode.
     */
    protected void setExtractorMode(String mode) {
        extractorPanel.setMode(mode);
    }

    /**
     */
    protected void addExtraction(Extraction x) {
        extractorPanel.addItem(x);
        if (x instanceof EventExtraction) {
            ArrayList<EventExtraction> subEvents = ((EventExtraction) x).getSubEvents();
            if (subEvents != null) {
                for (EventExtraction y : subEvents) {
                    extractorPanel.addItem(y);
                }
            }
        }
    }

    /**
     */
    protected void activateExtractorPanel() {
        extractorPanel.setTreeOperationsEnabled(true);
    }

    /**
     * Saves text entered in the text panel to a file.
     */
    private boolean saveText() {
        String text = textPanel.getText();
        if (text.equals("")) {
            JOptionPane.showMessageDialog(this, "Empty");
            return false;
        }
        JFileChooser fc = new JFileChooser();
        String cwd = getDataDir();
        if (cwd == null)
            cwd = ".";
        fc.setCurrentDirectory(new File(cwd));

        int returnVal = fc.showSaveDialog(this);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            File f = fc.getSelectedFile();
            try {
                BufferedWriter bw = new BufferedWriter(new FileWriter(f));
                bw.write(text);
                bw.close();
            } catch (Exception ew) {
                JOptionPane.showMessageDialog(this, ew.getMessage(), "Error Writing File", JOptionPane.ERROR_MESSAGE);
            }
            setName(f.getName());
            try {
                String path = f.getParentFile().getCanonicalPath();
                addToDataset(path, f.getName(), text);
            } catch (Exception ef) {
                return false;
            }
            return true;
        } else {
            return false;
        }
    }

    /** Adds new data item to parent's dataset.
     *
     * Side effect: If the item was saved in a folder other than the default 
     * folder (which is the parent's current data folder), the item is 
     * saved again in the default folder, so it can be made visible in the  
     * current selection panel. This behavior may be changed in the future.
     */
    private void addToDataset(String path, String name, String content) {
        try {
            File dir = new File(getDataDir());
            if (!dir.getCanonicalPath().equals(path)) {
                // create the file in the current directory as well
                File f = new File(dir, name);
                BufferedWriter bw = new BufferedWriter(new FileWriter(f));
                bw.write(content);
                bw.close();
            }
            showDataset(true); // to refresh the selector
        } catch (Exception e) {
        }
    }

    /**
     * Gets tag options from {@code parent}, converted to {@code String} and without initial and final brackets.
     */
    protected String getTagOptions() {
        String options = parent.getTagOptions().toString();
        return options.substring(1, options.length() - 1);
    }

    /**
     * Sends tag options to {@code parent}.
     */
    protected void setTagOptions(KQMLList tagOptions) {
        parent.setTagOptions(tagOptions);
    }

    private void zoomIn() {
        if (fontSize >= maxFontSize)
            return;
        fontSize++;
        textPanel.setFontSize(fontSize);
        selector.setFontSize(fontSize);
        extractorPanel.setFontSize(fontSize);
    }

    private void zoomOut() {
        if (fontSize <= minFontSize)
            return;
        fontSize--;
        textPanel.setFontSize(fontSize);
        selector.setFontSize(fontSize);
        extractorPanel.setFontSize(fontSize);
    }

    /** Sets font size */
    protected void setFontSize(int size) {
        fontSize = size;
    }
    /** Returns font size */
    protected int getFontSize() {
        return fontSize;
    }
}

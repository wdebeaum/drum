/* 
 * DataSet.java
 *
 * $Id: DataSet.java,v 1.12 2016/02/19 00:11:43 lgalescu Exp $
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>,  4 Jul 2010
 */

package TRIPS.DrumGUI;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.TreeSet;
import java.util.Vector;
import java.util.regex.Pattern;

/**
 * Class for storing and operating on a dataset.
 * <p>
 * A dataset is a collection of files in a given directory (folder). Items in the dataset (ie, files) may be selected
 * for processing. This class implements operations for:
 * <ul>
 * <li>choosing a folder
 * <li>reading files from the folder
 * <li>selecting/deselecting items
 * </ul>
 * 
 * @author lgalescu
 *
 */
public class DataSet {
    private static final String DEFAULT_FOLDER = ".";
    /** Folder */
    private String folder;
    /** Filenames in the folder */
    private Vector<String> filenames = new Vector<String>();
    /** Selections */
    private TreeSet<Integer> selected  = new TreeSet<Integer>();; 

    public DataSet() {
        this(DEFAULT_FOLDER);
    }
    public DataSet(String folder) {
        this.folder = folder;
    }

    public String toString() {
        return filenames.toString();
    }

    /*** File operations ***/

    /**
     * Sets folder. If folder is changed, the filenames are cleared.
     * 
     * @throws IOException
     *             if the folder doesn't exist
     */
    protected void setFolder(String folder)
            throws IOException
    {
        setFolder(folder, false);
    }

    /**
     * Sets folder. If getFiles is true, filenames are read from the new folder.
     * 
     * @throws IOException
     *             if the folder doesn't exist
     */
    protected void setFolder(String folder, boolean getFiles)
            throws IOException
    {
        File oldPath = (new File(this.folder)).getCanonicalFile();
        File newPath = (new File(folder)).getCanonicalFile();
        if (!newPath.isDirectory()) {
            throw new IOException("Directory does not exist: " + folder);
        }
        if (!newPath.equals(oldPath)) {
            Debug.info("Changing folder\n\tfrom: " + oldPath + "\n\tto: " + newPath);
        }
        clear();
        this.folder = folder;
        if (getFiles) {
            getFiles();
        }
    }

    /**
     * Gets folder. 
     */
    protected String getFolder() {
        return folder;
    }

    /**
     * Gets n-th filename. 
     */
    protected String getFilename(int n) {
        if (n < 0)
            return null;
        return filenames.get(n);
    }

    /**
     * Gets all filenames.
     */
    protected Vector<String> getFilenames() {
        return filenames;
    }

    /**
     * Adds new filename at the end of the filenames list.
     */
    protected void add(String filename) {
        filenames.add(filename);
    }

    /**
     * Adds filename and folder. If folder is new, the filenames are cleared first.
     * 
     * @throws IOException
     *             if the folder doesn't exist
     */
    protected void add(String folder, String filename) throws IOException {
        setFolder(folder);
        filenames.add(filename);
    }

    /**
     * Reads file containing data file names. The file is assumed to contain one
     * filename per line. Lines starting with {@literal #} are understood to be
     * comments and skipped. The folder is set to the parent folder of {@code filePath}.
     * 
     * NOTE: DataSet is cleared first (even if folder remains the same).
     */
    protected void addFromFile(String filePath) {
        File batchFile = new File(filePath);
        if (!batchFile.isFile()) {
            Debug.fatal("File not found: " + filePath);
        }
        try {
            setFolder(batchFile.getParent());
        } catch (Exception e1) {
            e1.printStackTrace();
            return;
        }
        clear();
        try {
            BufferedReader reader = new BufferedReader(new FileReader(batchFile));
            String filename;
            int i = 0;
            while ((filename = reader.readLine()) != null) {
                if (filename.startsWith("#"))
                    continue; // skip comments
                add(filename);
                i++;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        Debug.warn("Read " + filenames.size() + " filenames.");
    }

    /**
     * Reads list of files from current directory.
     */
    protected void getFiles() {
        if (folder == null) {
            Debug.error("Folder not set!");
            return;
        }
        clear();
        File dir = new File(folder);
        Debug.debug("Reading directory: " + dir);
        File[] children = dir.listFiles();
        Debug.debug("Considering " + children.length + " files.");
	Arrays.sort(children); // Mac OS happens to sort for us, Linux doesn't
        // select all normal files
        for (File child : children) {
            if (child.isFile()) {
                add(child.getName());
            }
        }
        Debug.debug("Read " + filenames.size() + " files.");
    }

    /**
     * Removes file from dataset and from the current directory.
     */
    protected void removeFile(String filename) {
        (new File(folder + File.separator + filename)).delete();
    }

    /**
     * Removes all selected files from dataset and from the current directory.
     */
    protected void removeSelectedFiles() {
        if (isSelectionEmpty()) {
            return;
        }
        for (Integer i : selected) {
            Debug.debug("Deleting " + getFilename(i));
            removeFile(getFilename(i));
        }
        getFiles();
    }

    /*** Selection operations ***/

    /**
     * Selects data item by filename. Returns index of data item.
     */
    protected int select(String filename) {
        int n = filenames.indexOf(filename);
        if (n >= 0)
            selected.add(n);
        Debug.debug("Selected index =" + n);
        return n;
    }

    /**
     * Adds n-th filename to selection.
     */
    protected void select(int n) {
        selected.add(n);
    }

    /**
     * Adds all filenames to selection.
     */
    protected void selectAll() {
        for (int i = 0; i < filenames.size(); i++) {
            selected.add(i);
        }
        Debug.debug("Selected all: " + selected);
    }

    /**
     * Select files from current directory, matching a given pattern.
     */
    protected void selectFiles(final String pattern) {
        if (filenames.isEmpty()) {
            Debug.error("No files found!");
            return;
        }
        clearSelection();
        Debug.debug("Considering " + filenames.size() + " files.");
        // select all files matching pattern
        for (int i = 0; i < filenames.size(); i++) {
            if (Pattern.matches(pattern, filenames.get(i))) {
                selected.add(i);
            }
        }
        Debug.debug("Selected files: " + selected);
    }

    /**
     * Tests if any filename is selected.
     */
    protected boolean isSelectionEmpty() {
        return selected.isEmpty();
    }

    /**
     * Returns number of items selected.
     */
    protected int getSelectionSize() {
        return selected.size();
    }

    /**
     * Gets index of first selection, if any. Returns -1 if nothing is selected.
     */
    protected int getFirstSelectionIndex() {
        return isSelectionEmpty() ? -1 : selected.first();
    }

    /**
     * Gets filename of first selection, if any. Returns {@code null} if nothing
     * is selected.
     */
    protected String getFirstSelection() {
        try {
            return getFilename(selected.first());
        } catch (Exception e) {
            Debug.error("Nothing selected!");
            return null;
        }
    }

    /**
     * Removes first selection from selected filenames.
     */
    protected String popSelection() {
        int item = getFirstSelectionIndex();
        selected.remove(item);
        Debug.debug("popped: " + item + "; remaining selections: " + selected);
        return filenames.get(item);
    }

    /**
     * Gets selections.
     */
    protected Integer[] getSelection() {
        return selected.toArray(new Integer[0]);
    }

    /**
     * Clears selections.
     */
    protected void clearSelection() {
        selected.clear();
    }

    /**
     * Clears both the filenames list and the selection list.
     */
    protected void clear() {
        filenames.clear();
        selected.clear();
    }

    /** Gets contents of data item with index n.
     * 
     * @param n Data file index.
     */
    protected String getTextAt(int n) {
        return getText(getFilename(n));
    }

    /** Gets contents of data file as a String.
     * 
     * @param filename Data file name.
     */
    protected String getText(String filename) {
        String result = "";
        // FIXME: i shouldn't replace line separators!!!
        String lineSeparator = System.getProperty("line.separator");
        try {
            String pathToFile = folder + File.separator + filename;
            BufferedReader reader = new BufferedReader(new FileReader(pathToFile));
            String line;

            // read file, line by line
            while ((line = reader.readLine()) != null) {
                result += line + lineSeparator;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return result;
    }

}

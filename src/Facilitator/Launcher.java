/*
 * Launcher.java
 *
 * George Ferguson, ferguson@cs.rochester.edu,  4 Aug 2000
 * $Id: Launcher.java,v 1.5 2010/04/22 03:30:20 lgalescu Exp $
 *
 */

package TRIPS.Facilitator;

import java.lang.reflect.Constructor;
import java.io.IOException;
import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.net.MalformedURLException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLString;
import TRIPS.KQML.KQMLToken;
import TRIPS.util.StringUtils;

/*
 * Originally this was an application that let us launch many applets in
 * one JVM. However, to solve chicken-and-egg problems stemming from the
 * need to launch the Facilitator itself, I decided to fold this
 * functionality into the Facilitator.
 *
 */
public class Launcher {
    //
    // Fields
    //
    Facilitator facilitator;
    List subprocesses = new LinkedList();
    //
    // Constructor
    //
    public Launcher(Facilitator facilitator) {
	this.facilitator = facilitator;
	// Be sure to cleanup any subprocesses
	Runtime.getRuntime().addShutdownHook(new Thread() {
		public void run() {
		    shutdown();
		}
	    });
    }
    //
    // Methods
    //
    /**
     * Called to launch a module in response to a START-MODULE message.
     */
    public void receiveRequestStartModule(KQMLList content) 
					throws LauncherException {
	Debug.debug("START-MODULE");
	if (content.getKeywordArg(":class") != null) {
	    startJavaModule(content);
	} else if (content.getKeywordArg(":exec") != null) {
	    startExecModule(content);
	} else {
	    throw new LauncherException("missing :class or :exec in START-MODULE");
	}
	Debug.debug("START-MODULE: done");
    }
    /**
     * Starts a Java module using the following parameters:
     * <PRE>
     *  :name - Name of the module
     *  :class - Class to instantiate and run (must be Runnable)
     *  :urlclasspath - Optional URL or loist of URLs from which to load class
     *  :argv - Optional list of parameters to module
     * </PRE>
     * <P>
     * If the class has a constructor that accepts an array of Strings,
     * then that constructor is used and is passed the argv array. Otherwise
     * the default constructor for the class is used.
     */
    void startJavaModule(KQMLList content) throws LauncherException {
	Debug.debug("starting Java module");
	// Get :name from msg
	String name = content.getKeywordArg(":name").toString();
	if (name == null) {
	    throw new LauncherException("missing :name in START");
	}
	Debug.debug(":name is \"" + name + "\"");
	// Get :group from msg
	String group = null;
	KQMLObject groupobj = content.getKeywordArg(":group");
	if (groupobj != null) {
	    group = groupobj.toString();
	}
	Debug.debug(":group is \"" + group + "\"");
	// Get :class from msg
	KQMLObject classarg = content.getKeywordArg(":class");
	if (classarg == null) {
	    throw new LauncherException("missing :class arg to START cmd");
	}
	String classname = kqml2string(classarg);
	Debug.debug(":class is \"" + classname + "\"");
	// Get classloader for loading class (either default or we create one)
	ClassLoader cloader;
	KQMLObject classpatharg = content.getKeywordArg(":urlclasspath");
	if (classpatharg != null) {
	    try {
		if (classpatharg instanceof KQMLList) {
		    cloader = createURLClassLoaderFromList((KQMLList)classpatharg);
		} else {
		    cloader = createURLClassLoaderFromString(kqml2string(classpatharg));
		}
	    } catch (MalformedURLException ex) {
		throw new LauncherException("malformed URL: " + ex.getMessage());
	    }
	} else {
	    cloader = ClassLoader.getSystemClassLoader();
	}
	// Get argv and convert to array of strings
	String[] argv = null;
	KQMLObject argvobj = content.getKeywordArg(":argv");
	if (argvobj == null) {
	    argv = new String[0];
	    Debug.debug(":argv not given; assuming empty");
	} else if (argvobj.getClass() != KQMLList.class) {
	    throw new LauncherException("START :argv is not a list: " + argvobj.toString());
	} else {
	    KQMLList argvl = (KQMLList)argvobj;
	    argv = createArgvFromList((KQMLList)argvobj);
	}
	// Load class
	Class clazz;
	try {
	    clazz = cloader.loadClass(classname);
	} catch (ClassNotFoundException ex) {
	    throw new LauncherException("class " + classname + " not found: " + ex);
	} catch (ExceptionInInitializerError ex) {
	    throw new LauncherException("error initializing class " + classname + ": " + ex);
	} catch (LinkageError ex) {
	    throw new LauncherException("linkage error for class " + classname + ": " + ex);
	}
	Debug.debug("loaded class: " + clazz);
	// Locate appropriate constructor
	Constructor constructor = findConstructor(clazz);
	// Create instance
	Object instance;
	try {
	    if (constructor != null) {
		Object[] initargs = new Object[1];
		initargs[0] = argv;
		instance = constructor.newInstance(initargs);
	    } else {
		Debug.warn("using default constructor for class: " + classname);
		instance = clazz.newInstance();
	    }
	} catch (Exception ex) {
	    throw new LauncherException("exception creating instance of class " + classname + ": ",ex);
	} catch (Error ex) {
	    throw new LauncherException("error creating instance of class " + classname + ": ",ex);
	}
	Debug.debug("created instance: " + instance);
	// Check that it's Runnable (don't use implementsInterface())
	if (!Runnable.class.isInstance(instance)) {
	    throw new LauncherException("instance " + instance + "is not Runnable");
	}
	// Create thread for Runnable instance
	Thread t = new Thread((Runnable)instance);
	// Start thread/instance
	Debug.debug("starting thread: " + t);
	t.start();
	Debug.debug("done");
    }
    /**
     * Called to start a module that runs outside of Java using the following
     * parameters:
     * <PRE>
     *  :name - Name of the module
     *  :exec - Pathname to executable file
     *  :argv - Optional list of parameters for process
     *  :envp - Optional list of environment for process
     *  :connect - If true, connect to Facilitator (default true)
     *  :register - If true, send register message to Facilitator (default true)
     * </PRE>
     * <P>
     * We use Runtime.exec() to launch the external process.
     * <P>
     * The process' standard error is connected to ours via a reader thread
     * that copies from the subprocess' getErrorStream() to our System.err.
     */
    void startExecModule(KQMLList content) throws LauncherException {
	Debug.debug("starting Exec module");
	// Get :name from msg
	KQMLObject nameobj = content.getKeywordArg(":name");
	if (nameobj == null) {
	    throw new LauncherException("missing :name in START");
	}
	String name = nameobj.toString();
	Debug.debug(":name is \"" + name + "\"");
	// Get :group from msg
	String group = null;
	KQMLObject groupobj = content.getKeywordArg(":group");
	if (groupobj != null) {
	    group = groupobj.toString();
	}
	Debug.debug(":group is \"" + group + "\"");
	// Get :exec from msg
	KQMLObject execarg = content.getKeywordArg(":exec");
	if (execarg == null) {
	    throw new LauncherException("missing :class arg to START cmd");
	}
	String execname = kqml2string(execarg);
	Debug.debug(":exec is \"" + execname + "\"");
	// Get argv and convert to array of strings (and add execname)
	String[] argv = null;
	KQMLObject argvobj = content.getKeywordArg(":argv");
	if (argvobj == null) {
	    Debug.debug(":argv not given; assuming just execname");
	    argv = new String[1];
	    argv[0] = execname;
	} else if (argvobj.getClass() != KQMLList.class) {
	    throw new LauncherException("START :argv is not a list: " + argvobj.toString());
	} else {
	    KQMLList argvlist = (KQMLList)argvobj;
	    argvlist.push(new KQMLToken(execname));
	    argv = createArgvFromList(argvlist);
	}
	// Ditto for envp (default is null though)
	String[] envp = null;
	KQMLObject envpobj = content.getKeywordArg(":envp");
	if (envpobj == null) {
	    Debug.debug(":envp not given; assuming null");
	} else if (envpobj.getClass() != KQMLList.class) {
	    throw new LauncherException("START :envp is not a list: " + envpobj.toString());
	} else {
	    envp = createArgvFromList((KQMLList)envpobj);
	}
	// Get :connect and :register params
	boolean connect = true;
	KQMLObject connectobj = content.getKeywordArg(":connect");
	if (connectobj != null) {
	    connect = StringUtils.stringToBoolean(connectobj.toString());
	}
	boolean register = true;
	KQMLObject registerobj = content.getKeywordArg(":register");
	if (registerobj != null) {
	    register = StringUtils.stringToBoolean(registerobj.toString());
	}
	// Launch process
	Debug.debug("creating process");
	Process proc = null;
	try {
	    proc = Runtime.getRuntime().exec(argv, envp);
	    subprocesses.add(proc);
	} catch (IOException ex) {
	    throw new LauncherException("couldn't exec process: " + ex.getMessage());
	}
	// We want stderr to go to our stderr
	new StderrReader(proc.getErrorStream()).start();
	// If we're supposed to connect, then do so now
	if (connect) {
	    ProcessClient client = new ProcessClient(facilitator, proc);
	    facilitator.add(client);
	    if (register) {
		KQMLPerformative msg = new KQMLPerformative("register");
		msg.setParameter(":name", name);
		if (group != null) {
		    msg.setParameter(":group", group);
		}
		facilitator.sendTo(msg, client);
	    }
	    client.start();
	}
	Debug.debug("done");
    }
    /**
     * Called to process a STOP message. Currently unimplemented.
     */
    public void receiveRequestStopModule(KQMLList content)
					throws LauncherException {
	Debug.warn("Facilitator: STOP not implemented yet");
	throw new LauncherException("sorry, STOP not implemented yet");
    }
    /**
     * Destroys all subprocesses started by the launcher. This would
     * normally be run from a shutdown hook.
     * @see Process.destroy()
     * @see Runtime.addShutdownHook
     */
    public void shutdown() {
	System.err.println("Facilitator: shutdown: cleaning up");
	Iterator procs = subprocesses.iterator();
	while (procs.hasNext()) {
	    Process p = (Process)procs.next();
	    System.err.println("Facilitator: shutdown: killing " + p);
	    p.destroy();
	}
	System.err.println("Facilitator: shutdown: done");
    }

    //
    // Support methods
    //
    /**
     * Returns a ClassLoader for loading classes from the given list,
     * whose elements should be KQMLString's or KQMLToken's representing
     * URLs.
     * If an an element of the list does not specify a URL protocol (i.e.,
     * does not start with ``http://'' or ``file://'' or whatever), it is
     * treated as a ``file://'' URL.
     */
    static ClassLoader createURLClassLoaderFromList(KQMLList list)
    						throws MalformedURLException {
	URL[] urls = new URL[list.size()];
	for (int i=0; i < list.size(); i++) {
	    KQMLObject urlobj = (KQMLObject)list.get(i);
	    String urlstr = kqml2string(urlobj);
	    urls[i] = string2url(urlstr);
	    Debug.debug("createURLClassLoaderFromList: url[" + i + "]=" + urls[i]);
	}
	return new URLClassLoader(urls);
    }
    /**
     * Returns a ClassLoader for loading classes from the given url.
     * If the argument contains the path separator (":", or ";" on windows), but
     * not "://", it is split into multiple paths.
     */
    static ClassLoader createURLClassLoaderFromString(String str)
    						throws MalformedURLException {
	if ((!str.contains("://")) && str.contains(File.pathSeparator)) {
	    // the string is a full classpath with separators;
	    // split it and call createURLClassLoaderFromList
	    String[] paths = str.split("\\Q" + File.pathSeparator + "\\E");
	    KQMLList kqmlPaths = new KQMLList();
	    for (String path : paths) {
	        kqmlPaths.add(path);
	    }
	    return createURLClassLoaderFromList(kqmlPaths);
	} else {
	    // the string contains a single path or URL
	    URL url = string2url(str);
	    Debug.debug("createURLClassLoaderFromString: url=" + url);
	    return new URLClassLoader(new URL[]{ url });
	}
    }
    /**
     * Returns a URL corresponding to the given string. If the string does
     * not specify a URL protocol (i.e., does not start with ``http://'' or
     * ``file://'' or whatever), it is treated as a ``file://'' URL.
     */
    static URL string2url(String str) throws MalformedURLException {
	if (!str.contains("://")) {
	    return ((new File(str)).toURI().toURL());
	} else {
	    return new URL(str);
	}
    }
    /**
     * Returns a String[] whose elements are the elements of the list L,
     * with any elements that are KQMLString's converted into their
     * stringValue(), with other elements converted directly to Strings.
     */
    static String[] createArgvFromList(KQMLList l) {
	String[] argv = new String[l.size()];
	for (int i=0; i < l.size(); i++) {
	    KQMLObject arg = l.get(i);
	    argv[i] = kqml2string(arg);
	    Debug.debug(":argv[" + i + "] is \"" + argv[i] + "\"");
	}
	return argv;
    }

    /**
     * If CLAZZ has a constructor that takes precisely an array of Strings
     * as only parameter, then returns that constructor, otherwise returns
     * null. This method is called to locate the constructor we need to
     * create instances of TRIPS.StandardTripsModule. If this fails,
     * we will end up calling the default constructor for the class and
     * passing no arguments, which might work.
     */
    static Constructor findConstructor(Class clazz) {
	Debug.debug("Launcher.findConstructor: clazz=" + clazz);
	Constructor[] constructors = clazz.getConstructors();
	for (int i=0; i < constructors.length; i++) {
	    Constructor cons = constructors[i];
	    Debug.debug("Launcher.findConstructor: cons=" + cons);
	    Class[] parameterTypes = cons.getParameterTypes();
	    // We want a constructor that will take the argv array
	    if (parameterTypes.length == 1 &&
		parameterTypes[0] == String[].class) {
		// Yes, found it
		Debug.debug("Launcher.findConstructor: returning " + cons);
		return cons;
	    }
	}
	Debug.debug("Launcher.findConstructor: returning null");
        return null;
    }
    /**
     * If OBJ is a KQMLString, then its stringValue() is returned, otherwise
     * its toString() value is returned.
     */
    static String kqml2string(KQMLObject obj) {
	if (KQMLString.class.isInstance(obj)) {
	    return ((KQMLString)obj).stringValue();
	} else {
	    return obj.toString();
	}
    }
}

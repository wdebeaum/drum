package TRIPS.util.cwc;

import javafx.application.Application;
import javafx.application.Platform;
import javafx.stage.Stage;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

/** Minimal JavaFX Application that allows us to Platform.runLater().
 * Note: like the Highlander, there can be only one JavaFX Application.
 */ 
public class JavaFXApp extends Application {
  private static final CompletableFuture<JavaFXApp> theApp = new CompletableFuture<JavaFXApp>();
  private static Stage theStage;

  /** If we haven't done so already, call Application.launch() on a new thread,
   * and wait for the application to finish starting before returning.
   */
  public static void launch() {
    Platform.setImplicitExit(false); // let us create new windows even after the last one is closed
    new Thread(new Runnable() {
      @Override public void run() {
	try {
	  Application.launch(JavaFXApp.class);
	} catch (IllegalStateException ex) {
	  System.err.println("Already launched a JavaFX Application");
	  // make theApp future complete on the existing application thread;
	  // keep trying to call runLater until it works
	  boolean notInitialized = true;
	  while (notInitialized) {
	    try {
	      Platform.runLater(()->{
		theApp.complete(null);
	      });
	      notInitialized = false;
	    } catch (IllegalStateException ex2) {
	      try {
	        Thread.sleep(1000); // wait 1 second between retries
	      } catch (InterruptedException ex3) {
		// sigh, java, I don't care if the sleep was interrupted
	      }
	    }
	  }
	}
      }
    }).start();
    // ensure finished starting before return
    try {
      theApp.get();
    // convert checked exceptions to unchecked exception
    } catch (InterruptedException e) {
      throw new RuntimeException("interrupted while starting JavaFXApp", e);
    } catch (ExecutionException e) {
      throw new RuntimeException("exception while starting JavaFXApp", e);
    }
  }

  @Override
  public void start(Stage stage) {
    theStage = stage;
    theApp.complete(this);
  }
}

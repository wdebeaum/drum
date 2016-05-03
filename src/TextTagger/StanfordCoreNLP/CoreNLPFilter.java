/** CoreNLP as a UNIX filter program
 *  @author Will de Beaumont
 */

package TRIPS.TextTagger.StanfordCoreNLP;

import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.util.StringUtils;
import java.util.Properties;
import java.io.BufferedReader;
import java.io.InputStreamReader;

public class CoreNLPFilter {
  public static void main(String[] args) throws Exception {
    Properties props = null;
    if (args.length > 0) {
      props = StringUtils.argsToProperties(args);
      if (props.containsKey("h") || props.containsKey("help")) {
	System.err.println("Command-line options are the same as for StanfordCoreNLP.");
	return;
      }
    }
    StanfordCoreNLP pipeline = new StanfordCoreNLP(props);
    BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
    String s;
    while ((s = br.readLine()) != null) {
      Annotation document = new Annotation(s);
      pipeline.annotate(document);
      pipeline.xmlPrint(document, System.out);
    }
  }
}

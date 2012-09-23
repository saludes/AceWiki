
package ch.uzh.ifi.attempto.acewiki.gfservice;

import ch.uzh.ifi.attempto.acewiki.core.WPReasoner;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;

public class WPEngine extends GFEngine {

public WPEngine() {
		super();
		reasoner = new WPReasoner();
		System.out.println("Loading WPEngine with reasoner " + reasoner);
	}

public Sentence createSentence(String serialized) {
		System.out.println("Creating WP declaration");
		return new WPDeclaration(GFGrammar.deserialize(serialized), getGFGrammar());
	}
}
package ch.uzh.ifi.attempto.acewiki.gui;

import ch.uzh.ifi.attempto.acewiki.gfservice.WPDeclaration;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import nextapp.echo.app.Column;
import jpl.Term;


public class WPSentenceComponent extends SentenceComponent {

private static final long serialVersionUID = -5656565656565656L;


public WPSentenceComponent(Sentence sentence, WikiPage hostPage) {
	super(sentence, hostPage);
}

	
public void displayCores(Column col) {
	WPDeclaration ws = (WPDeclaration)sentence;
	for (Term t: ws.getCores())
		col.add(new CoreStatementComponent(t, ws, (ArticlePage)hostPage));
}

}
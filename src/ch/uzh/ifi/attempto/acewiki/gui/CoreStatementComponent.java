// This file is part of AceWiki.
// Copyright 2008-2012, AceWiki developers.
// 
// AceWiki is free software: you can redistribute it and/or modify it under the terms of the GNU
// Lesser General Public License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
// 
// AceWiki is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
// even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public License along with AceWiki. If
// not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.acewiki.gui;

import jpl.Term;
import ch.uzh.ifi.attempto.base.*;

import ch.uzh.ifi.attempto.acewiki.core.WPReasoner;
import ch.uzh.ifi.attempto.acewiki.gfservice.*;


import java.util.regex.Pattern;
import java.util.regex.Matcher;

import nextapp.echo.app.Alignment;
import nextapp.echo.app.Column;
import nextapp.echo.app.Row;
import nextapp.echo.app.event.ActionEvent;
import nextapp.echo.app.event.ActionListener;
import nextapp.echo.app.layout.RowLayoutData;
import ch.uzh.ifi.attempto.acewiki.Task;
import ch.uzh.ifi.attempto.acewiki.Wiki;
import ch.uzh.ifi.attempto.acewiki.core.InconsistencyException;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.Question;
import ch.uzh.ifi.attempto.acewiki.core.Sentence;
import ch.uzh.ifi.attempto.echocomp.HSpace;
import ch.uzh.ifi.attempto.echocomp.MessageWindow;
import ch.uzh.ifi.attempto.echocomp.MultipleChoiceWindow;

/**
 * This class represents a sentence component consisting of a drop down menu and the sentence text.
 * 
 * @author Tobias Kuhn
 */
public class CoreStatementComponent extends Column implements ActionListener {

	private static final long serialVersionUID = -540135972060005725L;

	private static final String ACTION_EDIT = "Edit...";
	private static final String ACTION_ADD_SENTENCE = "Add Sentence...";
	private static final String ACTION_ADD_COMMENT = "Add Comment...";
	private static final String ACTION_PRUNE = "Prune";
	private static final String ACTION_DELETE = "Delete";
	private static final String ACTION_REASSERT = "Reassert";
	private static final String ACTION_RETRACT = "Retract";
	private static final String ACTION_SHOW_DETAILS = "Show Details";

	private Term mTerm;
	private Wiki wiki;
	private ArticlePage hostPage;
	private WPDeclaration parent;
	private Row sentenceRow = new Row();
	//private StatementMenu dropDown;
	private RecalcIcon recalcIcon;
	private Pattern lexPattern;

	
	/**
	 * Creates a new sentence component. The host page is the page that contains the text row
	 * (which is not necessarily the owner page of the sentence).
	 * 
	 * @param sentence The sentence to be shown.
	 * @param hostPage The host page of the text row.
	 */
	public CoreStatementComponent(Term term, WPDeclaration parent, ArticlePage page) {
		this.mTerm = term;
		this.hostPage = page;
		this.wiki = page.getWiki();
		this.recalcIcon = new RecalcIcon("The answer to this question is being updated.");
		this.parent = parent;
		lexPattern = Pattern.compile("((\\*\\()|(\\w+\\(?)|,|\\))\\s*");
		update();
	}

	private void update() {
		
		

		// boolean isRed = !sentence.isIntegrated() && !sentence.isImmutable() && !(sentence instanceof Question);

		removeAll();
		sentenceRow.removeAll();
		// sentenceRow.add(dropDown);
		sentenceRow.add(new HSpace(30));
		sentenceRow.add(new TextRow(renderTerm(mTerm).getTextContainerSet(wiki.getLanguage()), wiki, false));
		sentenceRow.add(new HSpace(5));
		sentenceRow.add(recalcIcon);
		recalcIcon.setVisible(false);
		sentenceRow.add(new HSpace(5));

		// Move to triangle to the top left of the row
		RowLayoutData rowLayoutData = new RowLayoutData();
		rowLayoutData.setAlignment(new Alignment(Alignment.LEFT, Alignment.TOP));
		//dropDown.setLayoutData(rowLayoutData);

		add(sentenceRow);

		// Question Answering:
		/* if (sentence instanceof Question && hostPage instanceof ArticlePage) {
			add(new AnswerList(wiki, (Question) sentence, recalcIcon));
		}
		*/
	}

	public void actionPerformed(ActionEvent e) {}
		

	private void log(String text) {
		/* if (text.endsWith(":")) {
			text += " " + sentence.getText(wiki.getEngine().getLanguages()[0]);
		} */
		wiki.log("page", text);
	}

	private WPDeclaration renderTerm(Term t) {
			Term ut = parent.fromCore(t);
			String lang = parent.getLanguage("Prolog");
			if (lang == null) 
				throw new IllegalArgumentException();
			WPDeclaration s = new WPDeclaration(lex(ut.toString()), lang, parent.getGFGrammar());
			if (s.getNumberOfParseTrees()==0)
				System.out.println("No parsing for " + ut.toString());
			return s;
	}

	private String lex(String s) {
		Matcher m = lexPattern.matcher(s);
		String result = "";
		while (m.find()) result +=  m.group() + " ";
		return result;
	}

	private String unlex(String s) {
		return s.replaceAll("\\s+([.?,])", "$1");
	}

	private String formatTerm(Term t) {
		String s = "";
		if (t.isVariable() && t.name().startsWith("_")) {
			int n = java.lang.Integer.parseInt(t.name().replaceAll("_(\\d+)", "$1"));
			return s + (char)(96 + n);
		} else if (t.isCompound() && t.arity() > 0) {
			String name = t.name();
			if (name.equals("-")) {
				return formatTerm(t.arg(1)) + " = " + formatTerm(t.arg(2));
			} else if (name.equals("lambda")) {
				return "forall " + formatTerm(t.arg(1)) +":" + formatTerm(t.arg(2)) + ". " + formatTerm(t.arg(3));
			} else if (name.equals("p")) {
				return "{" + formatTerm(t.arg(1)) +":" + formatTerm(t.arg(2)) + ". " + formatTerm(t.arg(3)) + "}";
			} else if (name.equals("*")) {
				return formatTerm(t.arg(1)) + "*" + formatTerm(t.arg(2));
			} else if (name.equals("+")) {
				return formatTerm(t.arg(1)) + " + " + formatTerm(t.arg(2));
			} else if (name.equals("unit") && t.arity() == 1) {
				return formatTerm(t.arg(1));
			} else {
				s = name + "(";
				int n = t.arity();
				for (int i=1; i <= n; i++) {
					s = s + formatTerm(t.arg(i));
					if (i < n) s = s +", ";
				}
				return s + ")";
			}
		} else 
			return t.toString();
	}

}

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

package ch.uzh.ifi.attempto.acewiki.gfservice;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Sets;

import ch.uzh.ifi.attempto.acewiki.core.AbstractSentence;
import ch.uzh.ifi.attempto.acewiki.core.WPReasoner;
import ch.uzh.ifi.attempto.acewiki.core.Declaration;
import ch.uzh.ifi.attempto.acewiki.core.OntologyElement;
import ch.uzh.ifi.attempto.acewiki.core.SentenceDetail;
import ch.uzh.ifi.attempto.base.TextContainer;
import ch.uzh.ifi.attempto.base.TextContainerSet;
import ch.uzh.ifi.attempto.base.TextElement;
import ch.uzh.ifi.attempto.gfservice.GfServiceException;

import jpl.*;


/**
 * This class represents a declaration statement for the GF AceWiki engine.
 * The "declaration" is a tree set that can be linearized into multiple
 * languages.
 * 
 * @author Kaarel Kaljurand
 */
public class WPDeclaration extends GFDeclaration  {

	/* final Logger logger = LoggerFactory.getLogger(GFDeclaration.class);

	private final GFGrammar mGfGrammar;

	// maps a language identifier to the set of linearizations (text containers) in this language
	private final Map<String, TextContainerSet> textContainers = new HashMap<String, TextContainerSet>();

	private ParseState mParseState; */

	private ArrayList<Term> mCoreStatements;

	public ArrayList<Term> getCores() {
		return mCoreStatements;
	}


	public String getLanguage(String suffix) {
		for (String lang: getGFGrammar().getLanguages())
			if(lang.matches("(.*)" + suffix)) return lang;
		return null;
	}

	public void addCore(Term t) {
		mCoreStatements.add(t) ;
		// System.out.println(String.format("Added '%s' as core", t));
	}

	public void removeCore(Term t) {
		mCoreStatements.remove(t);
	}

	public WPDeclaration(GFGrammar gfGrammar) {
		super(gfGrammar);
		mCoreStatements = new ArrayList<Term>();
	}

	/**
	 * Creates a new WP declaration object from a parse state.
	 * 
	 * @param parseState The parse state.
	 * @param gfGrammar The grammar object.
	 */
	public WPDeclaration(ParseState parseState, GFGrammar gfGrammar) {
		super(parseState, gfGrammar);
		mCoreStatements = new ArrayList<Term>();	
	}




	/**
	 * Creates a new GF declaration object from a text in a given language.
	 *
	 * TODO: the input text should probably be in the form of a token list
	 * 
	 * @param text The declaration text.
	 * @param language The language.
	 * @param gfGrammar The grammar object.
	 */
	public WPDeclaration(String text, String language, GFGrammar gfGrammar) {
		super(text, language, gfGrammar);
		mCoreStatements = new ArrayList<Term>();
	}

	public Term fromCore(Term t) {
		WPReasoner reasoner = (WPReasoner)getEngine().getReasoner();
		return reasoner.fromCore(t);
	}


}
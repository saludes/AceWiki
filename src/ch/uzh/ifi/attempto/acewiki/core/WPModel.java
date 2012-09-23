package ch.uzh.ifi.attempto.acewiki.core;

import java.util.ArrayList;
import jpl.Term;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class WPModel {
	private WPReasoner reasoner;
	private ArrayList<Term> mEquations;

	final Logger logger = LoggerFactory.getLogger(WPModel.class);


	public WPModel(WPReasoner r) {
		reasoner = r;
		mEquations = new ArrayList<Term>();
	}

	public void addEquation(Term eq) {
		assert (eq.isCompound() && "neqs".equals(eq.name()));
		logger.info("Added  " + reasoner.formatNumericEquation(eq));
		mEquations.add(eq);
	}

	public ArrayList<String> getFmtDefinitions() {
		ArrayList<String> fdefs = new ArrayList<String>();
		for (Term def: reasoner.getDefs()) {
			assert ("def".equals(def.name()) && def.arity()==2);
			fdefs.add(def.arg(1) + " := " + def.arg(2));
		}
		return fdefs;
	}

	
	public ArrayList<String> getFmtEquations() {
		ArrayList<String> fm = new ArrayList<String>();
		for (Term eq: mEquations)
			fm.add(reasoner.formatNumericEquation(eq));
		return fm;
	}

}


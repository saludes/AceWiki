package ch.uzh.ifi.attempto.acewiki.core;


import ch.uzh.ifi.attempto.acewiki.gfservice.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import jpl.*;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class WPReasoner extends DummyReasoner {
	// import jpl._

	private Term mAutoVarRange ;
	private ArrayList<Term> mDefs;
	private ArrayList<Term> tBox;
	

	final Logger logger = LoggerFactory.getLogger(WPReasoner.class);

	public void init(Ontology ontology) throws IllegalArgumentException { 
		/* try {
			setDirectory("/Users/saludes/Documents/projectes/MOLTO/molto-project.eu/mgl/wproblems/");
		} catch (IllegalArgumentException e) {
			logger.error("Cannot change directory");
		} */
		consult("basic");
		consult("tactics");
		consult("linear");
		//consult(getGrammarName(ontology));
		mAutoVarRange = jpl.Util.textToTerm("a-z");
		mDefs = new ArrayList<Term>();
		tBox = new ArrayList<Term>();
	}

	private String getGrammarName(Ontology ontology) {
		return ontology.getName().replaceAll("grammars__(.+)", "$1");
	}

	public String getReasonerName() {
		return "WPReasoner";
	}
	
	public String getReasonerVersion() {
		return "0.1";
	}
	
	public String getReasonerType() {
		return "word problem reasoner";
	}
	

	public ArrayList<Term> getDefs() {
		return mDefs;
	}

	public void loadSentence(Sentence s) {
		if (!(s instanceof WPDeclaration))
			System.out.println("This is a NOT a wp declaration: " + s);
		// logger.info("Loading sentence: ");
		/* debug info * for (SentenceDetail d: gd.getDetails("FarmEng")) {
			String name = d.getName();
			if ("Tree (ASCII)".equals(name)) 
				logger.info("\tTree: " + d.getRichText().replaceAll("<pre>([^<]+)</pre>", "$1"));
			else if ("Translations".equals(name))
				logger.info("\t" + name + ":  " + d.getRichText());
		} 
		logger.info(String.format("\tIt is reasonable, integrated and immutable? %b %b %b",
			gd.isReasonable(),
			gd.isIntegrated(),
			gd.isImmutable()));
		*/
		// WPDeclaration wps = new WPDeclaration(s.getParseState( 
		WPDeclaration ws = (WPDeclaration)s;
		String lang = ws.getLanguage("Prolog");
		if (lang == null) throw new IllegalArgumentException();
		Term t = jpl.Util.textToTerm(ws.getText(lang));
		logger.info("\tAs term: " + String.valueOf(t));
		logger.info("\tConverted to core:");
		
		int n = 0;
		for (Term ct: toCore(t)) {
			if ("dsubclass".equals(ct.name()) && ct.arity() == 2)
				//gd.addCore(ct);
				tBox.add(ct);
				//asserta(ct);
			else if ("istopclass".equals(ct.name()) && ct.arity() == 1)
				//gd.addCore(ct);
				tBox.add(ct);
				//asserta(ct);
			else if ("-".equals(ct.name()) &&  ct.arity() == 2)
				ws.addCore(ct);
			else
				throw new IllegalArgumentException("Not a associative list");
			n++;
			/*	debug info * logger.info(String.format("\t\t%s = %s",
						formatTerm(ct.arg(1)),
						formatTerm(ct.arg(2)))); */
		}
		ArrayList<Term> cores = ws.getCores();
		logger.info(String.format("There are %d core statements for '%s'",cores.size(),t));
	}

	public void unloadSentence(WPDeclaration s) {
		// WPDeclaration gd = (WPDeclaration)s;
		for (Term ct: s.getCores())
			System.out.println("About  to remove " + ct);
			//gd.removeCore(ct);
		
	}

	
	
	//private void log(String s) { logger.info(s); } //System.out.println(s); }

	private void setDirectory(String dir) throws IllegalArgumentException {
		String d = "D";
		String qs = String.format("working_directory(%s,%s)",d,d);
		Query owd = mkQuery(qs);
		if (owd.hasSolution()) {
			logger.info("Old directory was " + owd.oneSolution().get(d));
			qs = String.format("working_directory(_,'%s')", dir);
			Query nwd = mkQuery(qs);
			if (nwd.hasSolution())
				logger.info("New directory is " + dir);
			else
				throw new IllegalArgumentException();
		} else 
			throw new IllegalArgumentException();
	}

	private void consult(String fn) {
		Query p = mkQuery(String.format("consult('%s')", fn));
		if (!p.hasSolution()) 
			throw new IllegalArgumentException("Cannot consult " + fn);
	}

	private Term transform(String h,Variable v,Term t) {
		Term args = new Compound(";",new Term[] {t, v});
		return new Compound(h, new Term[] {args});
	}

	
	private Query mkQuery(String qs) {
		Term qt = jpl.Util.textToTerm(qs);
		return new Query(qt);
	}
	
	public Term inferType(Term t) {
		Variable v = new Variable();
		Query p = new Query(transform("type",v,t));
		if (p.hasSolution())
			return getValue(p.oneSolution(), v);
		else
			return null;
	}

	private Term getValue(Hashtable solution, Variable v) {
			return (Term)solution.get(String.valueOf(v));
	}
	

	public Term[] getCores(Hashtable solution) {
		Variable cs = new Variable("Cs");
		return jpl.Util.listToTermArray(getValue(solution, cs));
	}

	public Term[] getDefs(Hashtable solution) {
		Variable defs = new Variable("Defs");
		return jpl.Util.listToTermArray(getValue(solution, defs));
	}

	public Term[] toCore(Term term) throws IllegalArgumentException {
		Variable cs = new Variable("Cs");
		Variable defs = new Variable("Defs");
		Variable vRange = new Variable("Vs");
		for (Term t: tBox) asserta(t);
		Query q = new Query(
			new Compound("do_core",
						  new Term[] {mAutoVarRange, term, cs, defs, vRange}));
		if (q.hasSolution()) {
			Hashtable sol = q.oneSolution();
			for(Term t: jpl.Util.listToTermArray(getValue(sol, defs)))
				addDef(t);
			mAutoVarRange = getValue(sol,vRange);
			for (Term t: tBox) retracta(t);
			return jpl.Util.listToTermArray(getValue(sol,cs));
		} else
			throw new IllegalArgumentException("No solution for core: " + term.toString());
	}

	public Term fromCore(Term term) throws IllegalArgumentException {
		Variable n = new Variable("N");
		Query q = new Query(transform("unnormal", n, term));
		if (q.hasSolution())
			return getValue(q.oneSolution(),n);
		else
			throw new IllegalArgumentException("No plain format");
	}

	private void asserta(Term t) {
		Query q = new Query(
			new Compound("asserta", new Term[] {t}));
		if (!q.hasSolution())
			throw new IllegalArgumentException("Cannot assert");
		logger.info("asserta( " + t.toString() + " ).");
	}

	private void retracta(Term t) {
		Query q = new Query(
			new Compound("retract", new Term[] {t}));
		if (!q.hasSolution())
			throw new IllegalArgumentException("Cannot assert");
		logger.info("retract( " + t.toString() + " ).");
	}


	
	public String formatNumericEquation(Term equation) {
		if ("neqs".equals(equation.name())) {
			String eqS = "";
			boolean isFirst = true;
			for (Term t: jpl.Util.listToTermArray(equation.arg(1))) {
				eqS += (isFirst?"": " = ") + formatNumericTerm(t);
				isFirst = false;
			}
			return eqS;
		} else
			throw new IllegalArgumentException("Not a numeric equation");
	}


	private String formatNumericTerm(Term t) {
		if (t.isCompound() && t.arity()>0) {
			if ("+".equals(t.name())) {
				return formatNumericTerm(t.arg(1)) + " + " + formatNumericTerm(t.arg(2));
			} else if ("*".equals(t.name())) {
				return formatNumericTerm(t.arg(1)) + "*" + formatNumericTerm(t.arg(2));
			} else {
				String fmt = t.name() + "(";
				int tar = t.arity(); 
				for (int i=1; i<=tar; i++) {
					fmt += t.arg(i);
					if (i < tar) fmt += ",";
				}
				return fmt + ")";
			}
		} else
			return t.toString();
	}

	private void addDef(Term def) {
		assert (def.isCompound() && "def".equals(def.name()) && def.arity()==2);
		logger.info(def.arg(1) + " defined as " + def.arg(2));
		mDefs.add(def);
	}


	public WPModel doAuto(List<Statement> ss) {
		ArrayList<Term> terms = new ArrayList<Term>();
			for (Statement s: ss)
				if (s instanceof WPDeclaration) {
					System.out.println("The cores are " + ((WPDeclaration)s).getCores());
					terms.addAll(((WPDeclaration)s).getCores());
				}
		logger.info(String.format("Trying auto on %d core statements", terms.size()));
		for (Term t: tBox) asserta(t);
		for (Term t: terms) asserta(t);
		Variable norms = new Variable("Ns");
		Query q = new Query(
			new Compound("auto", new Term[] {norms}));
		if (!q.hasSolution()) 
			throw new IllegalArgumentException("Auto failed.");
		WPModel m = new WPModel(this);
		for(Term e: jpl.Util.listToTermArray(getValue(q.oneSolution(), norms)))
			m.addEquation(e);
		for (Term t: terms) retracta(t);
		for (Term t:tBox) retracta(t);
		return m;
	}

}
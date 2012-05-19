package com.live.rrutt.tuprolog.lib;

import com.live.rrutt.tuprolog.util.*;

import alice.tuprolog.*;
import alice.tuprolog.Number;

import java.io.*;

/**
 * @author Rick Rutt
 */
public class PrologPredicatesAndFunctors extends Library {
	
	public static boolean enableSpying = false;
	public static boolean enablePeeking = false;

	private static final long serialVersionUID = 7275020764345369935L;

	private static TextWindow textWindow = null;

	protected OutputStream outputStream = System.out;

	private void showTextWindow() {
		if (textWindow == null) {
			textWindow = new TextWindow();
		}

		textWindow.setDefaultBounds();
		textWindow.setVisible(true);
	}

	public static void disposeTextWindow() {
		if (textWindow != null) {
			textWindow.setVisible(false);
			textWindow.dispose();
			textWindow = null;
		}
	}

//	private void logMsg(String s) throws IOException {
//		outputStream.write(s.getBytes());
//		outputStream.write('\n');
//	}

	public boolean text_title_1(Struct g) throws Exception {
//		Term arg0 = g.getTerm(0);
		showTextWindow();
		String title = Utilities.stripQuotes(g.getName()); //arg0.toString());
		textWindow.setTitle(title);
		// logMsg("text_title " + title);
		return true;
	}

	public boolean text_close_0() throws Exception {
		// logMsg("text_close");
		if (textWindow != null) {
			textWindow.clear();
			textWindow.setVisible(false);
		}
		return true;
	}

	public boolean text_clear_0() throws Exception {
		showTextWindow();
		textWindow.clear();
		// logMsg("text_clear");
		return true;
	}

	public boolean text_cursor_2(Term rowTerm, Term colTerm) throws Throwable {
		int row = intValueFromTerm(rowTerm);
		int col = intValueFromTerm(colTerm);
		// logMsg("text_cursor " + row + " " + col);

		showTextWindow();
		textWindow.setCursorRowCol(row, col);
		return true;
	}
	
	private int intValueFromTerm(Term t) {
		int result = 0;
		
		Term tt = t.getTerm();		
		if (tt instanceof Int) {
			result = ((Int) tt).intValue();
		} else if (tt instanceof Number) {
			Number n = (Number) tt;
			if (n instanceof Int) {
				result = n.intValue();
			}
		}

		return result;
	}
	
	private String stringValueFromTerm(Term t) {
		String result = "";
		
		Term tt = t.getTerm();		
		if (tt instanceof Struct) {
			result = ((Struct) tt).getName();
		} else if (tt instanceof Number) {
			Number n = (Number) tt;
			if (n instanceof Int) {
				result = new java.lang.Integer(n.intValue()).toString();
			} else {
				result = n.toString();
			}
		}

		return result;
	}

	public boolean text_write_1(Term arg0) throws Exception {
		String text = stringValueFromTerm(arg0);
		showTextWindow();
		textWindow.writeText(text);
		// logMsg("text_write " + text);
		return true;
	}

	public boolean text_nl_0() throws Exception {
		showTextWindow();
		textWindow.newLine();
		// logMsg("text_nl");
		return true;
	}
	
	public boolean peek_enabled_0() throws Exception {
		return enablePeeking;
	}

	public boolean peek_write_1(Term arg0) throws Exception {
		String text = stringValueFromTerm(arg0);
		System.out.print(text);
		return true;
	}

	public boolean peek_nl_0() throws Exception {
		System.out.print("\n");
		return true;
	}
	
	public boolean spy_enabled_0() throws Exception {
		return enableSpying;
	}

	public boolean spy_write_1(Term arg0) throws Exception {
		String text = stringValueFromTerm(arg0);
		System.out.print(text);
		return true;
	}

	public boolean spy_nl_0() throws Exception {
		System.out.print("\n");
		return true;
	}

	public boolean menu_3(Term arg0, Term arg1, Term arg2) throws Exception {
		String menuCaption = stringValueFromTerm(arg0);
		Struct choiceList = (Struct)arg1;
		Term choiceTerm = arg2;
		// logMsg("menu " + menuCaption + ": " + choiceList.toString());
		MenuDialog md = new MenuDialog(new javax.swing.JFrame(), true,
				menuCaption, choiceList);
		md.setVisible(true);
		int choice = md.choice();
		return unify(choiceTerm, new alice.tuprolog.Int(choice));
	}

	public Term random_int_1(Term val0) throws Throwable {
		Term result = new Var();

		if (!(val0 instanceof Number)) {
			throw new Exception(
					"random_int requires a bound integer parameter.");
		}
		int n = ((alice.tuprolog.Number) val0).intValue();
		java.lang.Double d = new java.lang.Double(1 + (Math.random() * n));
		unify(result, new alice.tuprolog.Int(d.intValue()));

		return result;
	}

	public Term random_double_0() {
		Term result = new Var();

		unify(result, new alice.tuprolog.Double(Math.random()));
		
		return result;
	}

	public boolean str_int_2(Term arg0, Term arg1) throws Throwable {
		if (arg0 instanceof Var) {
			Term val1 = evalExpression(arg1);
			alice.tuprolog.Number n = (alice.tuprolog.Number) val1;
			String s = null;
			if (n.isInteger()) {
				s = new java.lang.Integer(n.intValue()).toString();
			} else {
				return false;
			}
			return (unify(arg0, new Struct(s)));
		} else {
			if (!arg0.isAtom()) {
				return false;
			}
			String s = ((Struct) arg0).getName();
			int n = s.length();
			if (n > 2) {
				if ((s.charAt(0) == '\'') && (s.charAt(n - 1) == '\'')) {
					s = s.substring(1, n - 1);
				}
			}
			Term term = null;
			try {
				term = new alice.tuprolog.Int(java.lang.Integer.parseInt(s));
			} catch (Exception ex) {
				// Ignore
			}
			if (term == null) {
				return false;
			}
			return (unify(arg1, term));
		}
	}

	public boolean str_char_2(Term arg0, Term arg1) {
		if ((arg0 instanceof Var) && (!((Var)arg0).isBound())) {
			String s = stringValueFromTerm(arg1);
			int n = s.length();
			if (n > 2) {
				if ((s.charAt(0) == '\'') && (s.charAt(n - 1) == '\'')) {
					s = s.substring(1, n - 1);
				}
			}
			return (unify(arg0, new Struct(s)));
		} else if ((arg1 instanceof Var) && (!((Var)arg1).isBound())) {
			String s = stringValueFromTerm(arg0);
			int n = s.length();
			if (n > 2) {
				if ((s.charAt(0) == '\'') && (s.charAt(n - 1) == '\'')) {
					s = s.substring(1, n - 1);
				}
			}
			return (unify(arg1, new Struct(s)));
		}
		return false;
	}

	public boolean ask_ok_0() {
		String msg = "Click to proceed.";
		OkDialog okd = new OkDialog(new javax.swing.JFrame(), true, msg);
		okd.setVisible(true);
		boolean ok = okd.isOk();
		return ok;
	}

	public boolean ask_ok_1(Term arg0) {
		String text = stringValueFromTerm(arg0);
		OkDialog okd = new OkDialog(new javax.swing.JFrame(), true, text);
		okd.setVisible(true);
		boolean ok = okd.isOk();
		return ok;
	}

	public boolean ask_ok_2(Term arg0, Term arg1) {
		String text0 = stringValueFromTerm(arg0);
		String text1 = stringValueFromTerm(arg1);
		String msg = Utilities.stripQuotes(text0) + Utilities.stripQuotes(text1);
		OkDialog okd = new OkDialog(new javax.swing.JFrame(), true, msg);
		okd.setVisible(true);
		boolean ok = okd.isOk();
		return ok;
	}

	public boolean ask_yes_no_0() {
		String msg = "Click to proceed.";
		YesNoDialog ynd = new YesNoDialog(new javax.swing.JFrame(), true, msg);
		ynd.setVisible(true);
		boolean ok = ynd.yes();
		return ok;
	}

	public boolean ask_yes_no_1(Term arg0) {
		String text = stringValueFromTerm(arg0);
		YesNoDialog ynd = new YesNoDialog(new javax.swing.JFrame(), true, text);
		ynd.setVisible(true);
		boolean ok = ynd.yes();
		return ok;
	}

	public boolean ask_yes_no_2(Term arg0, Term arg1) {
		String text0 = stringValueFromTerm(arg0);
		String text1 = stringValueFromTerm(arg1);
		String msg = Utilities.stripQuotes(text0) + Utilities.stripQuotes(text1);
		YesNoDialog ynd = new YesNoDialog(new javax.swing.JFrame(), true, msg);
		ynd.setVisible(true);
		boolean ok = ynd.yes();
		return ok;
	}

	public boolean break_point_0() throws Exception {
		System.out.println(" {break_point}");
		return true;
	}

	public boolean break_point_1(Term arg0) throws Exception {
		String text = stringValueFromTerm(arg0);
		System.out.println(" {break_point: " + text + "}");
		return true;
	}

}

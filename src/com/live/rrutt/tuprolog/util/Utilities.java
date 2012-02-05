package com.live.rrutt.tuprolog.util;

/**
 * @author Rick Rutt
 */
public class Utilities {

	public static String stripQuotes(String s) {
		String result = s;
		int n = s.length();
		if (n > 1) {
			if ((s.charAt(0) == '\'') && (s.charAt(n - 1) == '\'')) {
				result = s.substring(1, n - 1);
			}
		}
		return result;
	}

}
